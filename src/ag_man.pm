#!/usr/bin/perl
# This package contains a module for managing collections of data and 
# processes.  It is useful for storing data plus containers with links to 
# individual data
# 
# This is useful for rates (data) and rate libraries (containers) and 
# many other data types.
#
# All these procedures work with multiple features and must be given the 
# feature name first to using the right database tables
#
# The Data and collection management uses 3 database tables that end with:
#    _info holds info about the collections (like library info)
#       req columns: CreateDate, ModDate, Path, Folder, Info
#    _data holds the data
#       req columns: CreateDate, ModDate, DataID
#    _links store which data is in which collection
#       req columns: Path, DataID
#
# One additional table is needed that keeps track of all background processes
# 
# Unless specified otherwise, subroutines use private paths

package ag_man;
use warnings;
use strict;
use ag_error;
use ag_db;
use ag_reactions;
require Exporter;

@ag_man::ISA = qw/Exporter/;
@ag_man::EXPORT = 
    ( # Data and collection management
      'ag_man_check_permission',#done
      'ag_man_dataids_to_path', #done
      'ag_man_get_col_list',    #done
      'ag_man_get_col_info',    #done
      'ag_man_get_data',        #done
      'ag_man_col_exist',       #done
      'ag_man_data_exist',      #done
      'ag_man_modify_col',      #done
      'ag_man_add_data',        #done
      'ag_man_share_col',       #done
      'ag_man_copy_col',
      'ag_man_export_col',
      'ag_man_erase_col',       #done
      'ag_man_erase_data',      #done
      'ag_man_report_labels',   #done

      # Data and collection management using reactions
      'ag_man_get_reaction_isotopes', #done
      'ag_man_get_reaction_list',     #done
      'ag_man_get_reaction_data',     #done
      'ag_man_get_all_reaction_data', #done
      'ag_man_get_reactionid',        #done
      'ag_man_merge_col',             #done
      'ag_man_locate_reactions',      #done

      # Process management (Simple)
      'ag_man_mk_tmp_dir',      #done
      'ag_man_fg_run',          #done
      'ag_man_bg_run',          #done
      'ag_man_bg_get_update',   #done
      'ag_man_bg_abort',        #done
      'ag_man_clean',           #done

      # Process management (Full Featured)
      'ag_man_get_tmp_dir',     #done
      'bg_run',                 #done
      'bg_run_list',            #done
      'bg_run_list_update',     #done
      'bg_get_run_info',        #done
      'bg_set_run_info',        #done
      'bg_get_latest_output',   #done
      'bg_kill',                #done
      'bg_really_kill',         #done

      # Path conversion
      'get_new_shared_path',    #done
      'get_private_path',       #done
      'get_public_path',        #done
      'make_paths_private',     #done
      'make_paths_public',      #done

      # Functions for legacy specs
      'path_to_grp_name',       #done
      'path_to_lib_name',       #done
      'paths_to_grp_names',     #done
      'paths_to_lib_names',     #done
      'grp_name_to_path',       #done
      'lib_name_to_path',       #done
      'grp_names_to_paths',     #done
      'lib_names_to_paths',     #done

      # Other
      'reacID_to_zas',          #done
      'zas_to_reacID',          #done
      'zas_add_to_db',          #done
      'send_email',             #done
      'read_first_line',        #done
      'write_to_file',          #done
      'get_db_value',           #done
      'get_db_array',           #done
      'get_db_hash');           #done

sub sig_pipe {
    my $signal = shift;
    local $SIG{$signal} = 'IGNORE';
    report_error('UNEXPECTED','caught signal',$signal);
}
$SIG{TERM} = \&sig_pipe;

# returns 'none', 'r', or 'w' according to permissions for user for path
# one has read permission if one has write permission
# Note: the path should be a private path
# $required is optional required permission level, if level is not met,
# report_error is called
sub ag_man_check_permission {
    my ($feature, $in, $path, $required) = @_;
    my $ans = 'none';

    if ($path !~ m|^/([^/]+)/([^/]+)?|) {report_error('INVALID_PATH', $path)}

    if ($1 eq 'PUBLIC' or $1 eq 'SHARED') {$ans = 'r'}
    elsif ($1 eq 'USER' and $in->{USER} eq 'guest' and $in->{ACTION} eq 'BBN SIM SETUP') {$ans = 'w'}
    elsif ($1 eq 'USER' and $in->{USER} eq 'guest') {$ans = 'r'}
    elsif ($1 eq 'USER') {
	report_error('INVALID_PATH', $path) if (not defined $2);
	$ans = 'w' if ($2 eq $in->{USER});
    }
    elsif ($1 eq 'ANY') {$ans = 'w'};

    if (defined $required) {
	if ($required eq 'r' and $ans eq 'w') {} # write implies read
	elsif ($required ne $ans) {
	    report_error('PERM_DENIED',$path,"permission=".$ans,"required=".$required);
	}
    }

    return $ans;
}

# Find paths to dataids with a given permission
# $perm may be 'r' or 'w', The default is 'r'
# $ids is a reference to an array
# $paths is a reference to a hash keyed on dataid
# Options:
# 'ignore_invalid_dataids'
sub ag_man_dataids_to_path {
    my ($feature, $in, $ids, $paths, $perm, @opt) = @_;
    $perm = 'r' unless ($perm and $perm eq 'w');
    undef %{$paths};

    my $cond = "Path LIKE '/USER/".$in->{USER}."/%'";
    $cond .= " OR Path LIKE '/SHARED/%' OR Path LIKE '/PUBLIC/%'" if ($perm eq 'r');
    my $i = '';
    for (@{$ids}) {$i .= ' DataID='.$_.' OR'}
    chop $i;
    chop $i;

    my $sth = $ag_db::dbh->prepare("SELECT DataID,Path from ".$feature.'_links WHERE ('.$cond.') AND ('.$i.')')
	or report_error('UNEXPECTED',"Prepare failed when getting paths for ids", $feature, $DBI::errstr);

    $sth->execute()
	or report_error('UNEXPECTED',"Execute failed when getting paths for ids", $feature, $DBI::errstr);

    my ($rowref);
    while ($rowref = $sth->fetchrow_arrayref()) {
	$paths->{$rowref->[0]} = $rowref->[1];
    }

    unless(opt_value('ingore_invalid_dataids', @opt)) {
	for (@{$ids}) {
	    report_error('INVALID_DATAIDS', join(',',@{$ids}))
		unless ($paths->{$_});
	}
    }
}

# grp_callback arguments ($in, $group_path, $status)
# callback arguments ($in, $item_path, $status)
# status is 'b' when called before data, empty with data, and 'e' after data
# paths should end with a slash (be a path to a folder)
sub ag_man_get_col_list {
    my ($feature, $in, $grp_callback, $folder_callback, $run_callback, $paths) = @_;
    my ($p, $rowref, $i, $j, $count);
    
    my $sth = $ag_db::dbh->prepare("SELECT Path FROM $feature\_info WHERE Path REGEXP BINARY ? ORDER BY Path;")
	or report_error('UNEXPECTED',"Prepare failed when getting $feature list: $DBI::errstr");

    my $sth2 = $ag_db::dbh->prepare("SELECT DISTINCT Folder FROM $feature\_info WHERE Folder REGEXP BINARY ? ORDER BY Folder;")
	or report_error('UNEXPECTED',"Prepare failed when getting $feature folder list: $DBI::errstr");

    $ag_db::dbh->do("LOCK TABLES $feature\_info READ;")
	or report_error('UNEXPECTED',"Table lock failed for $feature\_info: $DBI::errstr");

    &$grp_callback($in,$p,'b');
    for ($j = 0; $j < scalar @{$paths}; $j++) {
	$p = @{$paths}[$j];
	report_error('UNEXPECTED',"Paths should end with a slash") if ($p !~ m|/$|);
	&$grp_callback($in,$p,$j == scalar @{$paths});

	# Get list of runs
	$count = get_db_value("SELECT COUNT(*) FROM $feature\_info WHERE Path REGEXP BINARY '^$p\[^/]+\$'", 'getting collection list count');

	$sth -> execute("^$p\[^/]+\$")
	    or report_error('UNEXPECTED',"Execute failed when getting $feature list for $p: $DBI::errstr");

	$i = 0;
	&$run_callback($in,@{$rowref}[0],'b');
	while ($rowref = $sth -> fetchrow_arrayref) {
	    &$run_callback($in,path_to_lib_name(@{$rowref}[0]),++$i == $count);
	}
	&$run_callback($in,@{$rowref}[0],'e');

	# Get list of folders
	$count = get_db_value("SELECT COUNT(DISTINCT Folder) FROM $feature\_info WHERE Folder REGEXP BINARY '^$p\[^/]+/\$'", 'getting collection folder list count');

	$sth2 -> execute("^$p\[^/]+/\$")
	    or report_error('UNEXPECTED',"Execute failed when getting $feature folder list for $p: $DBI::errstr");

	$i = 0;
	&$folder_callback($in,@{$rowref}[0],'b');
	while ($rowref = $sth2 -> fetchrow_arrayref) {
	    &$folder_callback($in,path_to_lib_name(@{$rowref}[0]),++$i == $count);
	}
	&$folder_callback($in,@{$rowref}[0],'e');
    }

    $ag_db::dbh->do("UNLOCK TABLES;")
	or report_error('UNEXPECTED',"Table unlock failed for $feature\_info: $DBI::errstr");
    &$grp_callback($in,$p,'e');
}

# callback arguments ($in, $path, \@values)
# columns is comma-separated list of column names to pass to callback
sub ag_man_get_col_info {
    my ($feature, $in, $callback, $paths, $columns) = @_;
    my ($p, $rowref);

    my $sth = $ag_db::dbh->prepare("SELECT $columns FROM $feature\_info WHERE Path = ?;")
	or report_error('UNEXPECTED',"Prepare failed when getting $feature info: $DBI::errstr");

    for $p (@{$paths}) {
	$sth -> execute($p) or
	    report_error('UNEXPECTED',"Execute failed when getting $feature info for $p: $DBI::errstr");
	if ($rowref = $sth -> fetchrow_arrayref) {
	    &$callback($in, $p, $rowref);
	} else {
	    if ($feature eq 'rlib') {report_error('INVALID_RLIB',path_to_lib_name($p),$p)}
	    else {report_error('INVALID_PATH',$p)}
	}
    }
}

# callback arguments ($in, $arrayref, $path, $status)
# status is 'b' when called before data, empty with data, and 'e' after data
# paths should end with a slash (be a path to a folder)
# returns the number of database entries found
sub ag_man_get_data {
    my ($feature, $in, $callback, $path, $columns, $conditions, $sorting) = @_;
    my ($rowref, $count, $i);
    $columns =~ s/,/,$feature\_data./g;
    $columns = "$feature\_data." . $columns;
    $conditions = '(' . $conditions . ') AND ' if ($conditions);
    $sorting = "ORDER BY $feature\_data.DataID" if (not defined $sorting);
    
    my $sth = $ag_db::dbh->prepare("SELECT $columns FROM $feature\_data, $feature\_links WHERE $conditions $feature\_data.DataID = $feature\_links.DataID AND $feature\_links.Path = ? $sorting;")
	or report_error('UNEXPECTED',"Prepare failed when getting $feature data: $DBI::errstr");

    $ag_db::dbh->do("LOCK TABLES $feature\_links READ, $feature\_data READ;")
	or report_error('UNEXPECTED',"Table lock failed while getting $feature data: $DBI::errstr");

    $count = get_db_value("SELECT COUNT(*) FROM $feature\_data, $feature\_links WHERE $conditions $feature\_data.DataID = $feature\_links.DataID AND $feature\_links.Path = '$path'");

    $sth -> execute($path) or
	report_error('UNEXPECTED',"Execute failed when getting $feature data for $path: $DBI::errstr");

    $i = 0;
    &$callback($in,$rowref,$path,'b');
    while ($rowref = $sth -> fetchrow_arrayref) {
	&$callback($in,$rowref,$path,++$i == $count);
    }
    &$callback($in,$rowref,$path,'e');

    $ag_db::dbh->do("UNLOCK TABLES;")
	or report_error('UNEXPECTED',"Table unlock failed while getting $feature data: $DBI::errstr");

    return $count;
}

# Returns reference to array of 'Y' or 'N' values
# Options:
# 'print_output' -> Print PATH=\nEXISTS=\n for each path
sub ag_man_col_exist {
    my ($feature, $in, $paths, @opt) = @_;
    my ($i, @exists);
    my $p = opt_value('print_output', @opt);

    my $sth = $ag_db::dbh->prepare("SELECT COUNT(*) FROM ".$feature.'_info WHERE Path=?')
	or report_error('UNEXPECTED',"Prepare failed when checking for existing path: $DBI::errstr");

    for ($i = 0; $i < scalar(@{$paths}); $i++) {
	$sth->execute($paths->[$i])
	    or report_error('UNEXPECTED',"Execute failed when checking for existing path: $DBI::errstr");
	$exists[$i] = $sth->fetchrow_arrayref()->[0] ? 'Y' : 'N';

	print('PATH=', get_public_path($paths->[$i], $in), "\n",
	      'EXISTS=', $exists[$i], "\n") if ($p);
    }
    return \@exists;
}

# Returns '' if no data found
# Returns reference to array of DataIDs if data found
sub ag_man_data_exist {
    my ($feature, $in, $path, $conditions, $sorting) = @_;
    my (@ids, $rowref);
    $conditions = '(' . $conditions . ') AND ' if ($conditions);
    $sorting = "ORDER BY $feature\_data.DataID" if (not defined $sorting);

    my $sth = $ag_db::dbh->prepare("SELECT $feature\_links.DataID FROM $feature\_data, $feature\_links WHERE $conditions $feature\_data.DataID = $feature\_links.DataID AND $feature\_links.Path = ? $sorting;")
	or report_error('UNEXPECTED',"Prepare failed when getting $feature dataids: $DBI::errstr");

    $sth -> execute($path) or
	report_error('UNEXPECTED',"Execute failed when getting $feature dataids for $path: $DBI::errstr");

    while ($rowref = $sth -> fetchrow_arrayref) {push @ids, $rowref->[0]}
    return \@ids if (scalar(@ids) > 0);
    return '';
}


# $columns is comma-separated list of columns to modify
# $info is reference to array of values to modify
sub ag_man_modify_col {
    my ($feature, $in, $path, $columns, $info) = @_;
    my $rows;

#    ag_man_check_permission($feature,$in,$path,'w');
#    for (@{$info}) {
#	report_error('BAD_VALUE', 'single quote in: ' . $_) if (index($_, "'") != -1);
#    }
#commented out lines so that single quote check is not made and will
# fix single quotes in java.


    report_error('BUG',"Some of these properties may not be modified: " . $columns) if ($columns =~ m/\bCreateDate\b|\bModDate\b|\bPath\b|\bFolder\b/);
    my $sep = $columns ? ',' : '';
    my $q = $columns ? "'" : '';

    $rows = get_db_value("SELECT COUNT(*) FROM $feature\_info WHERE Path='".$path."'", 'checking for existing path', 'allow_missing');

    if ($rows < 1) {
	# Add entry
	my $folder = $path;
	$folder =~ s|[^/]+$||;

	$rows = $ag_db::dbh->do("INSERT " . $feature . "\_info (CreateDate,ModDate,Path,Folder" . $sep . $columns . ") VALUES (NOW(),NOW(),'" . $path . "','" . $folder . "'" . $sep . $q . join("','",@{$info}) . $q . ")");

	report_error('INVALID_PATH', $path, "Failed to update $feature\_info for $path","columns: ".$columns,"info: ".join(',',@{$info}), "rows=" . $rows, $DBI::errstr) if (not $rows or $rows != 1);

	return "Added " . get_public_path($path, $in);
    }
    elsif ($rows == 1) {
	# Update existing entry
	report_error('PATH_EXISTS', $in->{PATH}, $in->{OVERWRITE}) if ($in->{OVERWRITE} ne 'Y');

	my @c = split(',',$columns);
	for (0 .. $#c) {$c[$_] .= "='" . @{$info}[$_] . "'"}
	$columns = join(',',@c);

	$rows = $ag_db::dbh->do("UPDATE " . $feature . "\_info SET ModDate=NOW()" . $sep . $columns . " WHERE Path='" . $path . "';");

	report_error('INVALID_PATH', $path, "Failed to update $feature\_info for $path with changes: $columns", "rows=" . $rows, $DBI::errstr) if ($rows != 1);

	return "Updated information for $path";
    }
    
    report_error('UNEXPECTED', "$path has '$rows' entries when it should have 0 or 1");
}

# callback arguments ($in, $path, \@data)
# callback returns true while @values are valid
# info_callback arguments ($in, $path, $info_columns)
# info_callback returns reference to array of values for properties in $info_columns
# columns is comma-separated list of column names to get from callback
# Options:
# 'allow_existing_path' -> Allow adding data to existing path
# 'report_data_level' -> Add individual data info to report
sub ag_man_add_data {
    my ($feature, $in, $callback, $info_callback, $path, $columns, $info_columns, $label_column, @opt) = @_;
    my $report = '';
    my $pub_path = get_public_path($path, $in);
    my $report_data_level = opt_value('report_data_level', @opt);
    my (@values, $i, $label_index);

    report_error('BUG',"Some of these properties may not be modified: " . $info_columns) if ($info_columns =~ m/\bCreateDate\b|\bModDate\b|\bPath\b|\bFolder\b/);
    report_error('BUG',"Some of these properties may not be modified: " . $columns) if ($columns =~ m/\bCreateDate\b|\bModDate\b|\bDataID\b/);

    ag_man_check_permission($feature,$in,$path,'w');
    if (not opt_value('allow_existing_path',@opt) and get_db_value("SELECT COUNT(*) FROM $feature\_info WHERE Path='".$path."'", 'checking for existing path', 'allow_missing') > 0) {
	report_error('PATH_EXISTS', $path);
    }

    if (defined $label_column) {
	# Find index in $columns that matches
	@_ = split(/,/,$columns);
	for ($i = 0; $i < scalar(@_); $i++) {
	    $label_index = $i if ($_[$i] eq $label_column);
	}
    }

    $columns =~ s/[^\w,]/_/g;
    my $tmp = $columns;
    $tmp =~ s/[^,]+/?/g;
    
    my $sth = $ag_db::dbh->prepare("INSERT " . $feature . "\_data (CreateDate,ModDate," . $columns . ") VALUES (NOW(),NOW()," . $tmp . ")")
	or report_error('UNEXPECTED',"Prepare failed when adding $feature data", $DBI::errstr);

    my $sth2 = $ag_db::dbh->prepare("INSERT " . $feature . "\_links (Path,DataID) VALUES ('" . $path . "',LAST_INSERT_ID())")
	or report_error('UNEXPECTED',"Prepare failed when adding $feature data links", $DBI::errstr);

    my $info_data = &$info_callback($in, $path, $info_columns);

    while (&$callback($in, $path, \@values)) {
	$sth -> execute(@values)
	    or report_error('UNEXPECTED',"Execute failed when adding $feature data for $path: $DBI::errstr");
	$sth2 -> execute()
	    or report_error('UNEXPECTED',"Execute failed when adding $feature data link for $path: $DBI::errstr");
	if ($report_data_level and defined($label_index)) {
	    if ($label_column eq 'ReactionID') {
		$i = get_db_array("SELECT ReactionString,DecayType FROM reaction_lookup WHERE ReactionID=".$values[$label_index],
				  'obtaining reaction string',
				  'allow_missing');
		if ($i->[0] and $i->[1]) {
		    $report .= 'Added '.$pub_path.', '.$i->[0].' ['.$i->[1]."]\n";
		}
		elsif ($i->[0]) {
		    $report .= 'Added '.$pub_path.', '.$i->[0]."\n";
		}
	    }
	    else {$report .= 'Added '.$pub_path.', '.$values[$label_index]."\n"}
	}
    }

    $i = $in->{OVERWRITE};
    $in->{OVERWRITE} = 'Y';
    $report .= ag_man_modify_col($feature, $in, $path, $info_columns, $info_data);
    $in->{OVERWRITE} = $i;
    return $report;
}

# opt='no_errors'
# returns true if $path was moved, false otherwise
sub ag_man_share_col {
    my ($feature, $in, $path, $new_path, $opt) = @_;
    $opt = '' if not defined($opt);
    my $folder = $new_path;
    $folder =~ s|[^/]+$||;

    ag_man_check_permission($feature, $in, $path, 'w');  # Need write permission for $path
    if ($new_path !~ '^/SHARED/') {
	return 0 if ($opt eq 'no_errors');
	report_error('INVALID_PATH',$new_path,'new path must begin with /SHARED/');
    }

    # Should lock tables to prevent conflicts

    my $rows = get_db_value("SELECT COUNT(*) FROM ".$feature."\_info WHERE Path='".$path."'", 'checking for existing path', 'allow_missing');
    return 0 if ($opt eq 'no_errors' and $rows < 1);
    report_error('INVALID_PATH',$path,'can not share path that does not exist')
	if ($rows < 1);

    $rows = get_db_value("SELECT COUNT(*) FROM ".$feature."\_info WHERE Path='".$new_path."'", 'checking for existing path', 'allow_missing');
    return 0 if ($opt eq 'no_errors' and $rows > 0);
    report_error('INVALID_PATH',$new_path,'can not overwrite existing shared path')
	if ($rows > 0);

    $ag_db::dbh->do("UPDATE " . $feature . "\_info SET Path='" . $new_path . "',Folder='" . $folder . "',Recipe='Added by username: " . $in->{USER} . "' WHERE Path='" . $path . "'")
	or report_error('UNEXPECTED',"Unable to share $feature collection: $path");
    $ag_db::dbh->do("UPDATE " . $feature . "\_links SET Path='" . $new_path . "' WHERE Path='" . $path . "'")
	or report_error('UNEXPECTED',"Unable to share $feature data: $path");

    return 1;
}

# opt='no_errors'
# returns true if $path was copied, false otherwise
sub ag_man_copy_col {
	
    my ($feature, $in, $path, $new_path) = @_;
    
    ag_man_check_permission($feature, $in, $path, 'w');  # Need write permission for $path
    if ($new_path !~ '^/SHARED/') {
	report_error('INVALID_PATH',$new_path,'new path must begin with /SHARED/');
    }

    # Should lock tables to prevent conflicts

	#Check that old path to copy from exists
    my $rows = get_db_value("SELECT COUNT(*) FROM ".$feature."\_info WHERE Path='".$path."'", 'checking for existing path', 'allow_missing');
    return 0 if ($rows < 1);
    report_error('INVALID_PATH',$path,'can not share path that does not exist')
	if ($rows < 1);

	#Check that there is not already a newPath in /SHARED/
    $rows = get_db_value("SELECT COUNT(*) FROM ".$feature."\_info WHERE Path='".$new_path."'", 'checking for existing path', 'allow_missing');
    return 0 if ($rows > 0);
    report_error('INVALID_PATH',$new_path,'can not overwrite existing shared path')
	if ($rows > 0);

	my %infoHash = %{get_db_hash("SELECT * FROM " . $feature . "\_info WHERE Path='" . $path . "'", 'getting hash for _info from path')};
	$infoHash{Path} = $new_path;
	$infoHash{Folder} = "/SHARED/";
	
	if(defined $infoHash{ObsPath}){
		$infoHash{ObsPath} = get_new_shared_path(get_public_path($infoHash{ObsPath}, $in));
	}
	
	if(defined $infoHash{BbnRunPath}){
		$infoHash{BbnRunPath} = get_new_shared_path(get_public_path($infoHash{BbnRunPath}, $in));
	}
	
	my $infoColumns;
	my $infoValues;

	for (keys %infoHash){	
		if(defined $infoHash{$_}){	
			$infoColumns .= ($_ . ",");
			$infoValues .= ("'" . $infoHash{$_} . "',");
		}
	}

	#remove last comma from infoColumns and infoValues
	$infoColumns =~ s/,$//;
	$infoValues =~ s/,$//;
	
	#create new row for copied library
	my $copy_to_info = $ag_db::dbh->prepare("INSERT " . $feature . "\_info (" . $infoColumns . ") VALUES (" . $infoValues . ")")
	or report_error('UNEXPECTED',"Prepare failed when adding $feature row for copy to shared action", $DBI::errstr);
	$copy_to_info->execute()
		or report_error('UNEXPECTED',"Execute failed while copying to info for $feature", $DBI::errstr);
	$copy_to_info->finish();

	#get array of DataIDs for old path
	my @oldDataIDs;
	my $get_old_data_ids = $ag_db::dbh->prepare("SELECT DataID FROM " . $feature . "\_links WHERE Path='".$path."';")
		or report_error('UNEXPECTED',"Prepare failed while getting old data ids from old path", $DBI::errstr);
	$get_old_data_ids->execute()
		or report_error('UNEXPECTED',"Execute failed while getting old data ids", $DBI::errstr);
	
	my $i;
	my @row;
    for ($i=0; @row = $get_old_data_ids -> fetchrow_array(); $i++) {
		push(@oldDataIDs, $row[0]);
    }
	$get_old_data_ids->finish();
	
	#copy old data to new data id
	foreach(@oldDataIDs){
		my %dataHash = %{get_db_hash("SELECT * FROM " . $feature . "\_data WHERE DataID='" . $_ . "'", 'getting hash for _data from DataID')};

		#take out data id from hash since data id will be auto_incremented in db
		delete $dataHash{DataID};
		
		my $dataColumns;
		my $dataValues;
	
		for (keys %dataHash){
			if(defined $dataHash{$_}){
				$dataColumns .= ($_ . ",");
				$dataValues .= ("'" . $dataHash{$_} . "',");
			}
		}
	
		#remove last comma from dataColumns and dataValues
		$dataColumns =~ s/,$//;
		$dataValues =~ s/,$//;
		
		#create new row for copied data
		my $copy_to_data = $ag_db::dbh->prepare("INSERT " . $feature . "\_data (" . $dataColumns . ") VALUES (" . $dataValues . ")")
		or report_error('UNEXPECTED',"Prepare failed when adding $feature row for copy to shared action", $DBI::errstr);
		$copy_to_data->execute()
			or report_error('UNEXPECTED',"Execute failed while copying to data for $feature", $DBI::errstr);
		$copy_to_data->finish();
		
		#get the new data id for this data
		my $newDataID = get_db_value("SELECT LAST_INSERT_ID() FROM " . $feature . "\_data", 'getting new data id');
		
		my $new_path_formatted = "'" . $new_path . "'";
		my $newDataID_formatted = "'" . $newDataID . "'";
								
		#create new row for copied data id in links
		my $copy_to_links = $ag_db::dbh->prepare("INSERT " . $feature . "\_links (Path,DataID) VALUES (" . $new_path_formatted . "," . $newDataID_formatted .")")
		or report_error('UNEXPECTED',"Prepare failed when adding $feature row for copy to shared action", $DBI::errstr);
		$copy_to_links->execute()
		or report_error('UNEXPECTED',"Execute failed while copying to info for $feature", $DBI::errstr);
		$copy_to_links->finish();

	}

    return 1;
}

# opt='check_overwrite' or 'no_error_if_missing'
# check_overwrite implies no_error_if_missing
# returns true if $path was erased, false otherwise
sub ag_man_erase_col {
    my ($feature, $in, $path, $opt) = @_;
    $opt = '' if not defined($opt);
    
    my $rows = get_db_value("SELECT COUNT(*) FROM ".$feature."\_info WHERE Path='".$path."'", 'checking for existing path', 'allow_missing');

    if ($opt eq 'check_overwrite') {
	return 0 if ($rows < 1);
	report_error('PATH_EXISTS', $in->{PATH}, $in->{OVERWRITE})
	    if ($in->{OVERWRITE} ne 'Y');
    }
    elsif ($opt eq 'no_error_if_missing') {return 0 if ($rows < 1)}
    elsif ($opt) {report_error('BUG','Unknown option to ag_man_erase_col',$opt)}

    report_error('INVALID_PATH',$path,'can not erase path that does not exist')
	if ($rows < 1);
    ag_man_check_permission($feature,$in,$path,'w');

    $ag_db::dbh->do("DELETE FROM " . $feature . "\_info WHERE Path='" . $path ."'")
	or report_error('UNEXPECTED',"Unable to delete $feature collection: $path");
    $ag_db::dbh->do("DELETE FROM " . $feature . "\_links WHERE Path='" . $path ."'")
	or report_error('UNEXPECTED',"Unable to delete $feature links: $path");
    return 1;
}

# opt='check_overwrite' or 'no_error_if_missing'
# check_overwrite implies no_error_if_missing
# returns report
# report is empty if nothing was done
sub ag_man_erase_data {
    my ($feature, $in, $path, $dataids, $label_column, @opt) = @_;
    return '' unless ($dataids);
    my $report = '';
    my ($i, $a);

    ag_man_check_permission($feature,$in,$path,'w');
    my $labels = ag_man_report_labels($feature, $in, $path, $dataids, $label_column, \$a);

    if (opt_value('no_error_if_missing', @opt)) {return 0 unless ($a);}
    if (opt_value('check_overwrite', @opt)) {
	report_error('PATH_EXISTS', $in->{PATH}, $in->{OVERWRITE})
	    if ($in->{OVERWRITE} ne 'Y');
    }

    report_error('INVALID_PATH',$path,'can not erase path that does not exist')
	unless ($a);

    my $sth = $ag_db::dbh->prepare("DELETE FROM ".$feature."\_links WHERE Path='".$path."' AND DataID=?;")
	or report_error('UNEXPECTED',"Prepare failed while erasing data: $DBI::errstr");

    for ($i = 0; $i < scalar(@{$dataids}); $i++) {
	if ($labels->[$i]) {
	    $sth->execute($dataids->[$i])
		or report_error('UNEXPECTED',"Execute failed while erasing data: $DBI::errstr");
	    $report .= 'Erased '.get_public_path($path,$in).', '. $labels->[$i] . "\n";
	}
    }
    return $report;
}

# Returns reference to array with labels corresponding to dataids
# individual labels will be undefined if dataid is invalid
sub ag_man_report_labels {
    my ($feature, $in, $path, $dataids, $label_column, $valid_count) = @_;
    my (@labels, $sth2, $rowref, $i);

    my $sth = $ag_db::dbh->prepare("SELECT ".$feature.'_data.'.$label_column." FROM ".$feature.'_data,'.$feature.'_links WHERE '.$feature."\_links.Path='".$path."' AND ".$feature.'_links.DataID='.$feature.'_data.DataID AND '.$feature.'_links.DataID=? LIMIT 1;')
	or report_error('UNEXPECTED',"Prepare failed while preparing to erase data: $DBI::errstr");
    if ($label_column eq 'ReactionID') {
	$sth2 = $ag_db::dbh->prepare("SELECT ReactionString,DecayType from reaction_lookup WHERE ReactionID=? LIMIT 1;")
	    or report_error('UNEXPECTED',"Prepare failed while obtaining reaction string: $DBI::errstr");
    }

    ${$valid_count} = 0;
    for ($i = 0; $i < scalar(@{$dataids}); $i++) {
	$sth->execute($dataids->[$i])
	    or report_error('UNEXPECTED',"Execute failed while preparing to erase data: $DBI::errstr");
	$labels[$i] = $sth->fetchrow_arrayref()->[0];
	${$valid_count}++ if ($labels[$i]);

	if ($label_column eq 'ReactionID' and $labels[$i]) {
	    $sth2->execute($labels[$i])
		or report_error('UNEXPECTED',"Execute failed while obtaining reaction string: $DBI::errstr");
	    $rowref = $sth2->fetchrow_arrayref();
	    $labels[$i] = $rowref->[0];
	    $labels[$i] .= ' ['.$rowref->[1].']' if ($rowref->[1]);
	}
    }

    return \@labels;
}

# Print PATH,ISOTOPES for each path in $paths
sub ag_man_get_reaction_isotopes {
    my ($feature, $in, $paths) = @_;
    my ($sth, $rowref, $need_tab);

    for (@{$paths}) {
	$sth = $ag_db::dbh->prepare('SELECT DISTINCT ReactantZ, ReactantA FROM reaction_lookup INNER JOIN '.$feature.'_data USING (ReactionID) INNER JOIN '.$feature.'_links USING (DataID) WHERE '.$feature."\_links.Path='".$_."' ORDER BY ReactantZ,ReactantA;")
	    or report_error('UNEXPECTED',"Prepare failed when getting isotopes",$DBI::errstr);

	$sth->execute()
	    or report_error('UNEXPECTED',"Execute failed when getting isotopes",$DBI::errstr);

	$need_tab = 0;
	print "PATH=",get_public_path($_, $in),"\nISOTOPES=";
	while ($rowref = $sth->fetchrow_arrayref()) {
	    if ($need_tab) {print "\t",$rowref->[0],',',$rowref->[1]}
	    else {print $rowref->[0],',',$rowref->[1]; $need_tab = 1;}
	}
	print "\n";
    }
}

# Print PATH,DATA_ID,ISOTOPE,REACTION_STRING,DECAY_TYPE 
# Requires $in->{ISOTOPES} and $in->{REACTION_TYPES}
sub ag_man_get_reaction_list {
    my ($feature, $in, $path) = @_;
    my ($rowref);
    my $where = '(';

    for (split(/\t/, $in->{ISOTOPES})) {
	if ($_ =~ m/^(\d+),(\d+)$/) {
	    $where .= ' (ReactantZ='.$1.' AND ReactantA='.$2.') OR';
	}
    }
    chop $where;
    chop $where;

    if ($in->{REACTION_TYPES} =~ m/\t/ or $in->{REACTION_TYPES} > 0) {
	$where .= ') AND (';
	for (split(/\t/, $in->{REACTION_TYPES})) {
	    $where .= ' ReaclibType='.$_.' OR';
	}
	chop $where;
	chop $where;
    }
    $where .= ')';

    my $sth = $ag_db::dbh->prepare('SELECT '.$feature.'_data.DataID,ReactantZ,ReactantA,ReactionString,DecayType FROM reaction_lookup INNER JOIN '.$feature.'_data USING (ReactionID) INNER JOIN '.$feature.'_links USING (DataID) WHERE '.$feature."\_links.Path='".$path."' AND ".$where.' ORDER BY ReactantZ,ReactantA,DataID;')
	or report_error('UNEXPECTED',"Prepare failed when getting reaction list",$path,$DBI::errstr);

    $sth->execute()
	or report_error('UNEXPECTED',"Execute failed when getting reaction list",$path,$DBI::errstr);

    print "PATH=",get_public_path($path, $in),"\n";
    while ($rowref = $sth->fetchrow_arrayref()) {
	print('DATA_ID=',$rowref->[0],"\n",
	      'ISOTOPE=',$rowref->[1],',',$rowref->[2],"\n",
	      'REACTION_STRING=',$rowref->[3],"\n",
	      'DECAY_TYPE=',$rowref->[4],"\n");
    }
}

# callback arguments ($in, $arrayref, $path, $dataid, $status)
# status is 'b' when called before data, empty with data, and 'e' after data
sub ag_man_get_reaction_data {
    my ($feature, $in, $callback, $dataids, $columns, $conditions, $sorting) = @_;
    my %paths; # Hash is keyed on DataID
    ag_man_dataids_to_path($feature, $in, $dataids, \%paths, 'r');

    my $id_col = scalar(@_ = split(/,/, $columns));
    $sorting = '' if (not defined $sorting);
    $conditions .= ') AND (' if ($conditions);
    for (@{$dataids}) {$conditions .= ' DataID='.$_.' OR'}
    chop $conditions;
    chop $conditions;

    my $sth = $ag_db::dbh->prepare("SELECT ".$columns.",DataID FROM ".$feature.'_data LEFT JOIN reaction_lookup USING(ReactionID) WHERE ('.$conditions.') '.$sorting)
	or report_error('UNEXPECTED',"Prepare failed when getting reaction data", $feature, $DBI::errstr);

    $sth->execute()
	or report_error('UNEXPECTED',"Execute failed when getting reaction data", $feature, $DBI::errstr);

    my ($rowref);
    &$callback($in, $rowref, '', '', 'b');
    while ($rowref = $sth->fetchrow_arrayref()) {
	&$callback($in, $rowref, $paths{$rowref->[$id_col]}, 
		   $rowref->[$id_col], '');
    }
    &$callback($in, $rowref, '', '', 'e');
}

# callback arguments ($in, $arrayref, $path, $status)
# status is 'b' when called before data, empty with data, and 'e' after data
# paths should end with a slash (be a path to a folder)
# returns the number of database entries found
sub ag_man_get_all_reaction_data {
    my ($feature, $in, $callback, $path, $columns, $conditions, $sorting) = @_;
    my ($rowref, $count, $i);
    #$columns =~ s/,/,$feature\_data./g;
    #$columns = "$feature\_data." . $columns;
    $conditions = '(' . $conditions . ') AND ' if ($conditions);
    $sorting = "ORDER BY $feature\_data.DataID" if (not defined $sorting);
    
    my $sth = $ag_db::dbh->prepare("SELECT $columns FROM reaction_lookup INNER JOIN $feature\_data USING (ReactionID) INNER JOIN $feature\_links USING (DataID) WHERE $conditions $feature\_links.Path = '$path' $sorting;")
	or report_error('UNEXPECTED',"Prepare failed when getting $feature data: $DBI::errstr");

    $count = get_db_value("SELECT COUNT(*) FROM $feature\_data, $feature\_links WHERE $conditions $feature\_data.DataID = $feature\_links.DataID AND $feature\_links.Path = '$path'");

    $sth -> execute() or
	report_error('UNEXPECTED',"Execute failed when getting $feature data for $path: $DBI::errstr");

    $i = 0;
    &$callback($in,$rowref,$path,'b');
    while ($rowref = $sth -> fetchrow_arrayref) {
	&$callback($in,$rowref,$path,++$i == $count);
    }
    &$callback($in,$rowref,$path,'e');

    return $count;
}

# Returns empty string on success or error message
# $reac_id is a reference to a scalar
sub ag_man_get_reactionid {
    my ($reac_str, $decay_type, $reac_id) = @_;
    my (@r, @p, $i);

    $decay_type = '' if ($decay_type eq 'NONE');

    $i = reactionstring_to_zas(\@r, \@p, $reac_str, 'IGNORE_NEG_Z',
			       'FORCE_DECAY_'.$decay_type);
    return $i if ($i);

    $i = zas_to_reacID(\@r, \@p, $reac_id);
    return $i;
}

# $details is reference to array of libraries / reaction number
sub ag_man_merge_col {
    my ($feature, $in, $src_paths, $dest_path, $notes, $details) = @_;
    my ($fnd, $src, @col, $rowref);
    my @pub_src_paths = @{$src_paths};
    make_paths_public(\@pub_src_paths, $in);

    get_db_array_all_rows('SHOW COLUMNS FROM '.$feature.'_data;', 'getting column list', 0, \@col);
    @col = grep(!/^DataID$/, @col);  # Remove DataID

    my $cpdata = $ag_db::dbh->prepare('INSERT INTO '.$feature.'_data ('.join(',',@col).') SELECT '.join(',',@col).' FROM '.$feature.'_data WHERE DataID=?')
	or report_error('UNEXPECTED',"Prepare failed when copying rate", $DBI::errstr);

    my $inslink = $ag_db::dbh->prepare('INSERT INTO '.$feature."\_links (Path,DataID) VALUES ('".$dest_path."',LAST_INSERT_ID())")
	or report_error('UNEXPECTED',"Prepare failed when inserting link", $DBI::errstr);

    for $src (@{$src_paths}) {
	push(@{$details}, $src) if (ref($details));
#	print $src,': ';
	$fnd = $ag_db::dbh->prepare('SELECT '.$feature.'_data.DataID,'.$feature.'_data.ReactionID FROM '.$feature.'_data INNER JOIN '.$feature.'_links USING (DataID) WHERE ReactionID NOT IN (SELECT ReactionID FROM '.$feature.'_data INNER JOIN '.$feature.'_links USING (DataID) WHERE '.$feature."\_links.Path='".$dest_path."') AND ".$feature."\_links.Path='".$src."' ORDER BY ".$feature.'_data.ReactionID')
	    or report_error('UNEXPECTED',"Prepare failed when finding ids", $DBI::errstr);

	$fnd->execute()
	    or report_error('UNEXPECTED',"Execute failed when finding ids", $DBI::errstr);

	while ($rowref = $fnd->fetchrow_arrayref()) {
	    push(@{$details}, $rowref->[1]) if (ref($details));
#	    print $rowref->[0]," ";
	    $cpdata->execute($rowref->[0])
		or report_error('UNEXPECTED',"Execute failed when copying rate", $DBI::errstr);
	    $inslink->execute()
		or report_error('UNEXPECTED',"Execute failed when inserting link", $DBI::errstr);
	}
#	print "\n";
    }
    ag_man_modify_col($feature, $in, $dest_path, 
		      'Recipe,Info', 
		      ['Merged from '.join(',',@pub_src_paths), $notes]);
}

sub ag_man_locate_reactions {
    my ($feature, $in, $callback, $paths, $reac_str, $decay, $columns) = @_;
    my ($reacid, $rowref);
    
    $rowref = ag_man_get_reactionid($reac_str, $decay, \$reacid);
    report_error('BAD_VALUE', $rowref) if ($rowref);

    my $sth = $ag_db::dbh->prepare('SELECT '.$columns.',Path,'.$feature.'_data.DataID FROM reaction_lookup INNER JOIN '.$feature.'_data USING (ReactionID) INNER JOIN '.$feature.'_links USING (DataID) WHERE reaction_lookup.ReactionID='.$reacid." AND Path IN ('".join("','",@{$paths})."');")
	or report_error('UNEXPECTED',"Prepare failed when locating reactions", $reac_str, $decay, $reacid, $DBI::errstr);

    $sth->execute()
	or report_error('UNEXPECTED',"Execute failed when locating reactions", $reac_str, $decay, $reacid, $DBI::errstr);

    &$callback($in, '', 'Unknown', 'Unknown', 'b');
    while ($rowref = $sth->fetchrow_arrayref()) {
	&$callback($in, $rowref, $rowref->[-2], $rowref->[-1], ''); 
    }
    &$callback($in, '', 'Unknown', 'Unknown', 'e');
}

########################################################
# Process management (Simple)

sub ag_man_mk_tmp_dir {
    my ($in, $feature) = @_;
    my $id_dir = ag_man_get_tmp_dir($in, '');
    my $feature_dir = ag_man_get_tmp_dir($in, $feature);

    ag_man_clean($feature_dir);
    mkdir($id_dir);  # Ingore error because dir may already exist
    mkdir($feature_dir)
	or report_error('UNEXPECTED',"Unable to make feature directory: $!");
    return $feature_dir;
}

#usage: $ret_val = ag_man_fg_run($in, 'feature', 'command', \@options);
sub ag_man_fg_run {
    my ($in, $feature, $command, $opt) = @_;
    my $dir = ag_man_get_tmp_dir($in, $feature);
    my $options = join("\t", @{$opt});

    report_error('BUG',"Command may not have single quotes",$command)
	if ($command =~ m/\'/);
    report_error('BUG',"Options may not have single quotes", $options)
	if ($options =~ m/\'/);

    $options = "'" . join("' '", @{$opt}) . "'" if (scalar @{$opt} > 0);
    return system("cd '$dir' && '$command' $options &> stdout");
}

#usage: $ret_val = ag_man_bg_run($in, 'feature', 'command', \@options);
sub ag_man_bg_run {
    my ($in, $feature, $command, $opt) = @_;
    my $dir = ag_man_get_tmp_dir($in, $feature);

    # update run list so that processes just killed won't count against user limit
    bg_run_list_update($in, "Feature='$feature'");
    return bg_run($in, $feature, $dir, 1, $command, $opt);
}

# return array index, description
# 0, RUNNING or COMPLETE
# 1, TotalRuns
# 2, CurrentRun
# 3, Y or N if text was skipped
# 4, text
sub ag_man_bg_get_update {
    my ($in, $feature) = @_;
    my @ans;

    my $rowref = bg_get_run_info($in, "Feature='$feature'", 'Pid,TotalRuns,LastReadPosition,Dir');
    report_error('UNEXPECTED',"trying to get update for $feature but it is not running.") if (not $rowref);
    
    my $ids = bg_run_list_update($in, "Pid=@{$rowref}[0]");
    $ans[0] = (scalar @{$ids} > 0) ? 'RUNNING' : 'COMPLETE';

    my $outref = bg_get_latest_output($in, @{$rowref}[3] . "/stdout", @{$rowref}[3] . "/run", @{$rowref}[2]);
    $ans[1] = @{$outref}[1];
    $ans[2] = @{$outref}[0];
    $ans[3] = @{$outref}[3];
    $ans[4] = @{$outref}[4];
    bg_set_run_info($in, @{$rowref}[0], @{$outref}[0], @{$outref}[2]);

    # Swap newlines in text with ASCII 8
    $ans[4] =~ s/\n/\x8/g;
    return \@ans;
}

sub ag_man_bg_abort {
    my ($in, $feature) = @_;
    my $ids = bg_run_list($in, "Feature='$feature'");
    #report_error('NOTHING_TO_ABORT', $feature) if (scalar @{$ids} < 1);
    bg_really_kill($in, $ids);
}

sub ag_man_clean {
     return system("/bin/rm -fr $_[0]");
}

########################################################
# Process management (Full Featured)

sub ag_man_get_tmp_dir {
    my ($in, $feature) = @_;
    return ($ag_db::dir . 'tmp/' . $in->{ID} . '/' . $feature);
}    

# usage: $pid = bg_run($in, $feature, $dir, $limit, $command, \@options);
# limit is the number of background processes allowed for this feature
sub bg_run {
    use Cwd;
    my ($in, $feature, $dir, $limit, $command, $opt) = @_;
    my $options = join("\t", @{$opt});
    my ($count);

    report_error('BUG',"Command may not have single quotes",$command)
	if ($command =~ m/\'/);
    report_error('BUG',"Options may not have single quotes", $options)
	if ($options =~ m/\'/);
    $options = "'" . join("' '", @{$opt}) . "'" if (scalar @{$opt} > 0);

    $ag_db::dbh->do("LOCK TABLES bg_processes WRITE;")
	or report_error('UNEXPECTED',"Table lock failed for bg_processes in bg_run: $DBI::errstr");

    $count = get_db_value("SELECT COUNT(*) FROM bg_processes WHERE Username='$in->{USER}' AND Feature='$feature'");
    report_error('REACHED_PROC_LIMIT',$limit,"count=$count",$feature) if ($count >= $limit and $in->{USER} ne 'guest');

    my $cwd = getcwd();
    chdir $dir or report_error('UNEXPECTED',"Can't cd to $dir: $!");

#    my $pid = system("'/bin/sleep' 3 &> /dev/null & echo \$! > 'pid'");
    my $pid = system("'$command' $options &> stdout & echo \$! > 'pid'");
    report_error('UNEXPECTED',"error in bg_run running $command in background: return value=$pid",$?) if ($pid != 0);

    chdir $cwd or report_error('UNEXPECTED',"Can't cd to $cwd: $!");

    read_first_line($in, $dir . '/pid', \$pid);
    report_error('UNEXPECTED',"error in bg_run getting pid",$pid) if ($pid <= 0);

    $command = $ag_db::dbh->quote("'$command' $options");
    # Add entry to bg_processes;
    my $sth = $ag_db::dbh->do("INSERT INTO bg_processes (Session,Pid,CurrentRun,TotalRuns,LastReadPosition,Username,Feature,Dir,Command) VALUES ('$in->{ID}',$pid,1,1,0,'$in->{USER}','$feature','$dir',$command);")
	or report_error('UNEXPECTED',"Insert failed in bg_run",$DBI::errstr);
    
    $ag_db::dbh->do("UNLOCK TABLES;")
	or report_error('UNEXPECTED',"Table unlock failed for bg_processes in bg_run: $DBI::errstr");

}

# usage: $array_ref = bg_run_list($in, $conditions);
# returns reference to array of ids
sub bg_run_list {
    my ($in, $conditions) = @_;
    my ($rowref, @runs);
    $conditions = 'AND (' . $conditions . ')' if ($conditions);

    my $sth = $ag_db::dbh->prepare("SELECT Pid FROM bg_processes WHERE Session = '$in->{ID}' AND Username = '$in->{USER}' $conditions;")
	or report_error('UNEXPECTED',"Prepare failed when getting bg run list: $DBI::errstr", $conditions);

    $sth -> execute()
	or report_error('UNEXPECTED',"Execute failed when getting bg run list: $DBI::errstr", $conditions);
    
    while ($rowref = $sth -> fetchrow_arrayref) {push @runs, @{$rowref}[0]}
    return \@runs;
}

# usage: $ret_val = bg_run_list_update($in, $conditions);
# returns reference to list of running ids
sub bg_run_list_update {
    my ($in, $conditions) = @_;
    my $pids = bg_run_list($in, $conditions);
    my ($id, $i, @running);

    my $sth = $ag_db::dbh->prepare('DELETE FROM bg_processes WHERE Pid=?')
	or report_error('UNEXPECTED',"Prepare failed when getting bg_run_list_update: $DBI::errstr");

    for $id (@{$pids}) {
	$i = kill 'SIGCONT', $id;
	if ($i) {push @running, $id}
	else {
	    $sth -> execute($id)
		or report_error('UNEXPECTED',"Unable to delete from bg_processes during update: $DBI::errstr");
	}
    }
    return \@running;
}

# returns reference to array with values
sub bg_get_run_info {
    my ($in, $conditions, $columns) = @_;
    $conditions = 'AND (' . $conditions . ')' if ($conditions);

    my $sth = $ag_db::dbh->prepare("SELECT $columns FROM bg_processes WHERE Session = '$in->{ID}' AND Username = '$in->{USER}' $conditions LIMIT 1;")
	or report_error('UNEXPECTED',"Prepare failed when getting bg run info: $DBI::errstr", $conditions);

    $sth -> execute()
	or report_error('UNEXPECTED',"Execute failed when getting bg run info: $DBI::errstr", $conditions);
    
    my $rowref = $sth -> fetchrow_arrayref;
    return if (not $rowref);
    return [@{$rowref}];
}

sub bg_set_run_info {
    my ($in, $pid, $current_run, $last_read_pos) = @_;

    $ag_db::dbh->do("UPDATE bg_processes SET CurrentRun=$current_run, LastReadPosition=$last_read_pos WHERE Pid=$pid;")
	or report_error('UNEXPECTED',"Unable to update bg_processes in bg_set_run_info: $DBI::errstr");
}

# return array index, description
# 0, CurrentRun
# 1, TotalRuns
# 2, LastReadPosition
# 3, Y or N if text was skipped
# 4, text
sub bg_get_latest_output {
    my ($in, $output_file, $run_file, $last_read_pos) = @_;
    my @ans;

    open OF, $output_file or report_error('UNEXPECTED',"Can't open $output_file",$!);
    seek(OF, 0, 2);  # jump to end of file

    open RF, $run_file or report_error('UNEXPECTED',"Can't open $run_file",$!);
    while (<RF>) {
	if ($_ =~ m/^run: (\d+) (\d+)/) {
	    $ans[0] = $1;
	    $ans[1] = $2;
	}
    }
    close RF;

    $ans[2] = tell(OF);

    # seek OF to 1K from end and read to $ans[2]
    my $offset = 0;
    my $cur = $ans[2] - 1024;
    my $tmp;
    $ans[3] = 'Y';
    if ($cur < $last_read_pos) {$cur = $last_read_pos; $ans[3] = 'N';}
    seek(OF, $cur, 0);

    $ans[4] = "";
    while ($cur+$offset < $ans[2]) {
	$tmp = read(OF, $ans[4], $ans[2]-$cur+$offset, $offset);
	report_error('UNEXPECTED',"can't read output for $output_file: $!", $ans[4], $ans[2]-$cur+$offset, $offset) if (not defined($tmp) or $tmp < 1);
	$offset += $tmp;
    }

    # if skipping text, skip till next newline
    $ans[4] =~ s|^.*\n|| if ($ans[3] eq 'Y');
    close OF;
    return \@ans;
}

# usage: $ret_val = bg_kill($in, $signal, \@ids);
# returns reference to list of ids with errors
sub bg_kill {
    my ($in, $signal, $ids) = @_;
    my ($id, $i, @problem);
    return \@problem if (scalar @{$ids} < 1);

    my $conditions = 'Pid=' . join(' OR Pid=', @{$ids});
    $ids = bg_run_list_update($in, $conditions);

    my $sth = $ag_db::dbh->prepare('DELETE FROM bg_processes WHERE Pid=?')
	or report_error('UNEXPECTED',"Prepare failed in bg_kill: $DBI::errstr");

    for $id (@{$ids}) {
	$i = kill $signal, $id;
	if ($i != 1) {push @problem, $i}
	else {
	    $sth -> execute($id)
		or report_error('UNEXPECTED',"Unable to delete from bg_processes after kill: $DBI::errstr");
	}
    }
    return \@problem;
}

# usage: $ret_val = bg_kill($in, \@ids);
sub bg_really_kill {
    my ($in, $ids) = @_;
    my ($id, $i, $problems);

    $problems = bg_kill($in, 'SIGTERM', $ids);
    return if (scalar @{$problems} < 1);
    sleep 3;
    $problems = bg_kill($in, 'SIGKILL', $problems);
}

########################################################
# Path conversion

# usage: $new_path = get_new_shared_path($path)
sub get_new_shared_path {
    my $path = shift;
    $path =~ s|^/[^/]+/|/SHARED/|;
    return $path;
}

# usage: $private_path = get_private_path($path, $in)
sub get_private_path {
    my ($path, $in) = @_;
    $path =~ s|^/USER/|/USER/$in->{USER}/|;
    return $path;
}

# usage: $public_path = get_public_path($path, $in)
sub get_public_path {
    my ($path, $in) = @_;
    $path =~ s|^/USER/$in->{USER}/|/USER/|;
    return $path;
}

# usage: make_paths_private(\@paths, $in)
sub make_paths_private {
    my $pf = shift;
    my $in = shift;
    my $n = scalar @{$pf};
    my $i;

    for ($i = 0; $i < $n; $i++) {
	@{$pf}[$i] =~ s|^/USER/|/USER/$in->{USER}/|;
    }
}

# usage: make_paths_private(\@paths, $in)
sub make_paths_public {
    my $pf = shift;
    my $in = shift;
    my $n = scalar @{$pf};
    my $i;

    for ($i = 0; $i < $n; $i++) {
	@{$pf}[$i] =~ s|^/USER/$in->{USER}/|/USER/|;
    }
}

########################################################
# Functions for legacy specs

# Returns group name from a path (to a library or group)
# Usage $name = path_to_grp_name($path);
sub path_to_grp_name {
    $_[0] =~ m|^/([^/]*)|;
    return $1;
}

# Returns library name from a path to a library
# Usage $name = path_to_lib_name($lib_path);
sub path_to_lib_name {
    my $i = shift;
    if (not $i) {return ''}
    $i =~ s|/$||;          # Remove trailing slash
    $i =~ s|^.*/||;        # Remove path from library name
    return $i;
}

# Changes paths in array to names
# Usage paths_to_grp_names(\@paths);
sub paths_to_grp_names {
    my $pf = shift;
    my $n = scalar @{$pf};
    my $i;

    for ($i = 0; $i < $n; $i++) {
	@{$pf}[$i] =~ m|^/([^/]*)|;
	@{$pf}[$i] = $1;
    }
}

# Changes paths in array to lib names
# Usage paths_to_lib_names(\@lib_paths);
sub paths_to_lib_names {
    my $pf = shift;
    my $n = scalar @{$pf};
    my $i;

    for ($i = 0; $i < $n; $i++) {
	@{$pf}[$i] =~ s|/$||;  # Remove trailing slash
	@{$pf}[$i] =~ s|^.*/||;
    }
}

# Returns group path from a name
# Usage grp_name_to_path($in, $group);
sub grp_name_to_path {
    my ($in, $g) = @_;
    return "/$g/";
}

# Returns private library path from a name or empty string
# ONLY THE FIRST MATCHING PATH IS RETURNED, 
# libraries should not have the same name
# Usage $path = lib_name_to_path($in, $lib_name, $feature);
sub lib_name_to_path {
    my ($in, $n, $feature) = @_;
    my $rowref;

    my $sth = $ag_db::dbh->prepare("SELECT Path FROM $feature\_info WHERE Path LIKE '/PUBLIC/%$n' OR Path LIKE '/SHARED/%$n' OR Path LIKE '/USER/$in->{USER}/%$n' GROUP BY Path ORDER BY Path LIMIT 1;")
	or report_error('UNEXPECTED',"Prepare failed when finding path to $n in $feature: $DBI::errstr");
    
    $sth -> execute();
    if ($rowref = $sth -> fetchrow_arrayref) {$n = @{$rowref}[0]}
    else {report_error('INVALID_RLIB',$n)}
    return $n;
}

# Changes group names in array to paths
# Usage grp_names_to_paths($in, \@groups);
sub grp_names_to_paths {
    my ($in, $gref) = @_;
    my $n = scalar @{$gref};
    my $i;

    for ($i = 0; $i < $n; $i++) {@{$gref}[$i] = "/@{$gref}[$i]/"}
}

# Changes library names in array to paths
# ONLY THE FIRST MATCHING PATH IS RETURNED FOR EACH ELEMENT IN @n
# libraries should not have the same name
# Usage lib_names_to_paths($in, \@lib_names, $feature);
sub lib_names_to_paths {
    my ($in, $lref, $feature) = @_;
    my $n = scalar @{$lref};
    my ($rowref, $i);

    for ($i = 0; $i < $n; $i++) {
	my $sth = $ag_db::dbh->prepare("SELECT Path FROM $feature\_info WHERE Path LIKE '/PUBLIC/%@{$lref}[$i]' OR Path LIKE '/SHARED/%@{$lref}[$i]' OR Path LIKE '/USER/$in->{USER}/%@{$lref}[$i]' GROUP BY Path ORDER BY Path LIMIT 1;")
	    or report_error('UNEXPECTED',"Prepare failed when finding path to @{$lref}[$i] in $feature: $DBI::errstr");
    
	$sth -> execute();
	if ($rowref = $sth -> fetchrow_arrayref) {@{$lref}[$i] = @{$rowref}[0]}
	else {report_error('INVALID_RLIB',@{$lref}[$i])}
    }
}

########################################################
# Other

# Returns empty string on success or error message
# Options:
sub reacID_to_zas {
    my ($r, $p, $id, @opt) = @_;

    my $sth = $ag_db::dbh->prepare("SELECT AllReactants,AllProducts FROM reaction_lookup WHERE ReactionID=? LIMIT 1;")
	or return 'Prepare failed while obtaining reaction id: ' . $DBI::errstr;
    $sth->execute(${$id})
	or return 'Execute failed while obtaining reaction id: ' . $DBI::errstr;
    my $rowref = $sth -> fetchrow_arrayref();
    return 'Unknown reaction id ' . ${$id} unless ($rowref);

    @{$r} = split(/,/,$rowref->[0]);
    @{$p} = split(/,/,$rowref->[1]);
    return '';
}

# Returns empty string on success or error message
# Options:
# 'DO NOT ADD' -> Do not add reaction to database if none found
# 'DO NOT CHECK' -> Do not call zas_check() before finding reacID
sub zas_to_reacID {
    my ($r, $p, $id, @opt) = @_;
    my ($i, $j);
    ${$id} = '';

    unless(opt_value('DO NOT CHECK', @opt)) {
	$i = zas_check($r, $p, @opt);
	return $i if ($i);
    }

    my $s = "AllReactants='" . join(',',@{$r}) 
	. "' AND AllProducts='" . join(',',@{$p}) . "'";

    for ($i = 0; $i < 2; $i++) {
	${$id} = get_db_value("SELECT ReactionID FROM reaction_lookup WHERE "
			      . $s, 'obtaining ReactionID', 'allow_missing');
	return '' if (${$id});
	return 'Could not find ' . zas_to_reactionstring($r, $p, @opt)
	    if (opt_value('DO NOT ADD', @opt) or $i > 0);

	$j = zas_add_to_db($r, $p, @opt);
	return $j if ($j);
    }
}

# Returns empty string on success or error message
sub zas_add_to_db {
    my ($r, $p, @opt) = @_;
    my $s = zas_to_reactionstring($r, $p, @opt, 'AVOID SPECIAL');
    my @rlookup = zas_lookup_particle($r, @opt);
    my @plookup = zas_lookup_particle($p, @opt);

    $ag_db::dbh->do("INSERT reaction_lookup (ReaclibType,ReactantZ,ReactantA,ProductZ,ProductA,ReactionString,AllReactants,AllProducts,DecayType) VALUES ("
		    . zas_reaclib_type($r, $p, @opt) . ','
		    . $rlookup[0] . ',' . $rlookup[1] . ','
		    . $plookup[0] . ',' . $plookup[1] . ",'" . $s . "','"
		    . join(',',@{$r}) . "','"
		    . join(',',@{$p}) . "','"
		    . zas_get_type($r, 'DECAY') . "');")
	or return 'Error adding ' . $s . ': ' . $DBI::errstr;
    return '';
}

sub send_email {
    my ($subject, $message, $from, $to, $cc, $bcc) = @_;
    report_error('BUG',"need subject in send_email()") if (not $subject);
    report_error('BUG',"need message in send_email()") if (not $message);
    report_error('BUG',"need from address in send_email()") if (not $from);
    report_error('BUG',"need to address in send_email()") if (not $to);
    $cc = '' if not defined($cc);
    $bcc = '' if not defined($bcc);

    open(SENDMAIL, "|/usr/sbin/sendmail -f " . $from . ' "' . $to . '" "' . $cc . '" "' . $bcc . '"')
	or report_error('UNEXPECTED',"unable to send email");

    print SENDMAIL "From: ", $from, "\n";
    print SENDMAIL "To: ", $to, "\n";
    print SENDMAIL "Cc: ", $cc, "\n";
    print SENDMAIL "Bcc: ", $bcc, "\n";
    print SENDMAIL "Subject: ", $subject, "\n";
    print SENDMAIL $message, "\n";

    close (SENDMAIL) or report_error('UNEXPECTED',"unable to send email (2)");
}

sub read_first_line {
    my ($in, $file, $str_ref) = @_;

    open F, $file
	or report_error('UNEXPECTED',"Error opening $file: $!");
    ${$str_ref} = <F>;
    chomp ${$str_ref};
    close F;
}

sub write_to_file {
    my ($in, $file, $str_ref) = @_;

    open F, "> " . $file
	or report_error('UNEXPECTED',"Error opening $file: $!");
    print F ${$str_ref};
    close F;
}

# Usage: $value = get_db_value($sql_query, $desc);
# Usage: $value = get_db_value($sql_query, $desc, 'allow_missing');
# SQL QUERY SHOULD NOT HAVE A SEMICOLON
# Returns value in first column and first row
sub get_db_value {
    my ($query, $desc, $allow_missing) = @_;
    my $sth = $ag_db::dbh->prepare($query . " LIMIT 1;")
	or report_error('UNEXPECTED',"Prepare failed while $desc: $DBI::errstr","$query LIMIT 1;");
    $sth -> execute()
	or report_error('UNEXPECTED',"Execute failed while $desc: $DBI::errstr","$query LIMIT 1;");
    my $rowref = $sth -> fetchrow_arrayref();

    report_error('UNEXPECTED',"No results while $desc in sql_query $query LIMIT 1;")
	if (not $allow_missing and not $rowref);
    return @{$rowref}[0];
}

# Usage: $row_ref = get_db_array($sql_query, $desc);
# Usage: $row_ref = get_db_array($sql_query, $desc, 'allow_missing');
# SQL QUERY SHOULD NOT HAVE A SEMICOLON
# Returns reference to array of first row
sub get_db_array {
    my ($query, $desc, $allow_missing) = @_;
    my $sth = $ag_db::dbh->prepare($query . " LIMIT 1;")
	or report_error('UNEXPECTED',"Prepare failed while $desc: $DBI::errstr","$query LIMIT 1;");
    $sth -> execute()
	or report_error('UNEXPECTED',"Execute failed while $desc: $DBI::errstr","$query LIMIT 1;");
    my $rowref = $sth -> fetchrow_arrayref();

    report_error('UNEXPECTED',"No results while $desc in sql_query $query LIMIT 1;")
	if (not $allow_missing and not $rowref);
    return $rowref;
}

# Usage: $hash_ref = get_db_hash($sql_query, $desc);
# Usage: $hash_ref = get_db_hash($sql_query, $desc, 'allow_missing');
# SQL QUERY SHOULD NOT HAVE A SEMICOLON
# Returns reference to hash of first row
sub get_db_hash {
    my ($query, $desc, $allow_missing) = @_;
    my $sth = $ag_db::dbh->prepare($query . " LIMIT 1;")
	or report_error('UNEXPECTED',"Prepare failed while $desc: $DBI::errstr","$query LIMIT 1;");
    $sth -> execute()
	or report_error('UNEXPECTED',"Execute failed while $desc: $DBI::errstr","$query LIMIT 1;");
    my $hashref = $sth -> fetchrow_hashref();

    report_error('UNEXPECTED',"No results while $desc in sql_query $query LIMIT 1;")
	if (not $allow_missing and not $hashref);
    return $hashref;
}

# Fills array with values from nth field of query
sub get_db_array_all_rows {
    my ($query, $desc, $n, $array_ref, $allow_missing) = @_;
    my ($rowref, $i);
    my $sth = $ag_db::dbh->prepare($query)
	or report_error('UNEXPECTED',"Prepare failed while $desc: $DBI::errstr",$query);
    $sth -> execute()
	or report_error('UNEXPECTED',"Execute failed while $desc: $DBI::errstr",$query);

    @{$array_ref} = ();
    for ($i=0; $rowref = $sth -> fetchrow_arrayref(); $i++) {
	push(@{$array_ref}, $rowref->[$n]);
    }

    report_error('UNEXPECTED',"No results while $desc in sql_query $query")
	unless ($allow_missing or $i>0);
}

1;
