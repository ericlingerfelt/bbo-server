#!/usr/bin/perl
    # This package contains the agm_Rate_Library module for performing CGI requests
    #
    # THIS FILE HAS BEEN AUTOMATICALLY GENERATED from an XML cgi specification
    # and should not be modified.  Instead update autogen.pm to produce the
    # desired cgi module
    #
    # AUTO GENERATION OCCURRED ON Tue Nov  7 10:18:14 2006

    package agm_Rate_Library;
    use warnings;
    use strict;
    use ag_error;
    use ag_man;
    require Exporter;

    @agm_Rate_Library::ISA = qw/Exporter/;
    @agm_Rate_Library::EXPORT = qw/
            agp_GET_RATE_LIBRARY_LIST
            agp_GET_RATE_LIBRARY_INFO
            agp_GET_RATE_LIBRARY_ISOTOPES
            agp_GET_RATE_LIST
            agp_GET_RATE_INFO
            agp_LOCATE_RATES
            agp_MODIFY_RATE
            agp_MERGE_RATE_LIBRARIES
            agp_ERASE_LIBRARY
            agp_COPY_LIBRARY_TO_SHARED/;

# Action: GET RATE LIBRARY LIST
# Input: HEADER ID ACTION USER PW PATHS
# Output: PATH RUNS FOLDERS
#   Get list of folders and libraries in list of PATHS. One set of properties is returned for every PATH in PATHS. Each line contains a property listed in the response property list. The first property in the set will always be PATH.
    sub agp_GET_RATE_LIBRARY_LIST {
        my $in = shift;		# input hash
	my @paths = split /\t/, $in->{PATHS};

	make_paths_private(\@paths, $in);
	ag_man_get_col_list('rlib', $in,
			    \&agp_GET_RATE_LIBRARY_LIST_grp_cbk,
			    \&agp_GET_RATE_LIBRARY_LIST_fol_cbk,
			    \&agp_GET_RATE_LIBRARY_LIST_run_cbk, \@paths);
    }

    sub agp_GET_RATE_LIBRARY_LIST_grp_cbk {
	print "PATH=",get_public_path($_[1], $_[0]),"\n"
	    if ($_[2] ne 'b' and $_[2] ne 'e');
    }

    sub agp_GET_RATE_LIBRARY_LIST_fol_cbk {
	if    ($_[2] eq 'b') {print "FOLDERS="}
	elsif ($_[2] eq 'e') {print "\n"}
	elsif ($_[2]) {print $_[1]}
	else  {print $_[1], "\t"}
    }

    sub agp_GET_RATE_LIBRARY_LIST_run_cbk {
	if    ($_[2] eq 'b') {print "RUNS="}
	elsif ($_[2] eq 'e') {print "\n"}
	elsif ($_[2]) {print $_[1]}
	else  {print $_[1], "\t"}
    }

# Action: GET RATE LIBRARY INFO
# Input: HEADER ID ACTION USER PW PATHS
# Output: PATH NOTES CREATION_DATE MODIFICATION_DATE RECIPE COMPLETE
#   Get information about libraries in a list of PATHS. One set of properties is returned for every PATH in PATHS. Each line contains a property listed in the response property list. The first property in the set will always be PATH.
    sub agp_GET_RATE_LIBRARY_INFO {
        my $in = shift;		# input hash
	my @paths = split /\t/, $in->{PATHS};

	make_paths_private(\@paths, $in);
	ag_man_get_col_info('rlib', $in,
			    \&agp_GET_RATE_LIBRARY_INFO_cbk, \@paths,
			    'Info,CreateDate,ModDate,Recipe,Complete')
    }

    sub agp_GET_RATE_LIBRARY_INFO_cbk {
	my $v = $_[2];
	print "PATH=",get_public_path($_[1], $_[0]),"\n",
	"NOTES=@{$v}[0]\n",
	"CREATION_DATE=@{$v}[1]\n",
	"MODIFICATION_DATE=@{$v}[2]\n",
	"RECIPE=@{$v}[3]\n",
	"COMPLETE=@{$v}[4]\n";	
    }

# Action: GET RATE LIBRARY ISOTOPES
# Input: HEADER ID ACTION USER PW PATHS
# Output: PATH ISOTOPES
#   Get list of isotopes for libraries in a list of PATHS. One set of properties is returned for every PATH in PATHS. Each line contains a property listed in the response property list. The first property in the set will always be PATH.
    sub agp_GET_RATE_LIBRARY_ISOTOPES {
        my $in = shift;         # input hash
	my @paths = split /\t/, $in->{PATHS};

	make_paths_private(\@paths, $in);
	ag_man_get_reaction_isotopes('rlib', $in, \@paths);
    }

# Action: GET RATE LIST
# Input: HEADER ID ACTION USER PW PATH ISOTOPES REACTION_TYPES
# Output: PATH DATA_ID ISOTOPE REACTION_STRING DECAY_TYPE
#   Get list of reaction rates for a library in PATH. One set of properties is returned for every rate. Each line contains a property listed in the response property list. The first property in the set will always be RATE_ID.
    sub agp_GET_RATE_LIST {
        my $in = shift;         # input hash

	ag_man_get_reaction_list('rlib', $in, 
				 get_private_path($in->{PATH}, $in));
    }

# Action: GET RATE INFO
# Input: HEADER ID ACTION USER PW DATA_IDS
# Output: DATA_ID PATH ISOTOPE REACTION_TYPE REACTION_STRING DECAY_TYPE BIBLIO_STRING CREATION_DATE MODIFICATION_DATE RATE_PARM_COUNT RATE_PARMS R_NR NOTES
#   Get information about rates for given DATA_IDS. One set of properties is returned for every DATA_ID in DATA_IDS. Each line contains a property listed in the response property list. The first property in the set will always be DATA_ID.
    sub agp_GET_RATE_INFO {
        my $in = shift;		# input hash
	my @ids = split /\t/, $in->{DATA_IDS};

	ag_man_get_reaction_data('rlib', $in,
				 \&agp_GET_RATE_INFO_cbk, \@ids,
				 'ReactantZ,ReactantA,ReaclibType,ReactionString,DecayType,Biblio,CreateDate,ModDate,ParmCount,Parameters,RNr,Info');
    }

    sub agp_GET_RATE_INFO_cbk {
	my ($in, $a, $path, $dataid, $status) = @_;

	if ($status eq 'b') {}
	elsif ($status eq 'e') {}
	else {
	    print("DATA_ID=",$dataid,"\n",
		  "PATH=",get_public_path($path, $in),"\n",
		  "ISOTOPE=",$a->[0],',',$a->[1],"\n",
		  "REACTION_TYPE=",$a->[2],"\n",
		  "REACTION_STRING=",$a->[3],"\n",
		  "DECAY_TYPE=",$a->[4],"\n",
		  "BIBLIO_STRING=",$a->[5],"\n",
		  "CREATION_DATE=",$a->[6],"\n",
		  "MODIFICATION_DATE=",$a->[7],"\n",
		  "RATE_PARM_COUNT=",$a->[8],"\n",
		  "RATE_PARMS=",$a->[9],"\n",
		  "R_NR=",$a->[10],"\n",
		  "NOTES=",$a->[11],"\n");
	}
    }

# Action: LOCATE RATES
# Input: HEADER ID ACTION USER PW PATHS REACTION_STRING DECAY_TYPE
# Output: PATH REACTION_STRING DECAY_TYPE DATA_ID
#   Look for reaction rates in PATHS with a certain REACTION_STRING and DECAY_TYPE.  Return DATA_IDs for rates located or return empty if no reaction rate exists.
    sub agp_LOCATE_RATES {
        my $in = shift;         # input hash
	my @paths = split /\t/, $in->{PATHS};
	make_paths_private(\@paths, $in);

	ag_man_locate_reactions('rlib', $in,
				\&agp_LOCATE_RATES_cbk, \@paths,
				$in->{REACTION_STRING}, $in->{DECAY_TYPE},
				'ReactionString,DecayType');
    }

    sub agp_LOCATE_RATES_cbk {
	my ($in, $a, $path, $dataid, $status) = @_;

	if ($status eq 'b') {}
	elsif ($status eq 'e') {}
	else {
		print("PATH=",get_public_path($path, $in),"\n",
		  "REACTION_STRING=",$a->[0],"\n",
		  "DECAY_TYPE=",$a->[1],"\n",
		  "DATA_ID=",$dataid,"\n");
	}
    }

# Action: MODIFY RATE
# Input: HEADER ID ACTION USER PW OVERWRITE PATH REACTION_STRING DECAY_TYPE BIBLIO_STRING RATE_PARMS R_NR NOTES
# Output: REPORT
#   Overwrite existing rate or add a new rate.  If the values for REACTION_STRING and DECAY_TYPE match an existing rate for that PATH, the existing rate will be overwritten, provided the OVERWRITE property allows it.  If REACTION_STRING and DECAY_TYPE do not match, a new rate will be added.
    sub agp_MODIFY_RATE {
        my $in = shift;		# input hash
	my $in_path = get_private_path($in->{PATH},$in);
	$in->{PARM_CNT} = scalar(@_ = split(/,/,$in->{RATE_PARMS}));

	my ($reaction_id, $i, $report);
	$i = ag_man_get_reactionid($in->{REACTION_STRING}, $in->{DECAY_TYPE},
				   \$reaction_id);
	report_error('BAD_VALUE', $i) if ($i);
	$in->{REACTION_ID} = $reaction_id;

	report_error('BAD_VALUE','Number of parameters should be 7,14,21,28,35 or 42 not '.$in->{PARM_CNT}) unless($in->{PARM_CNT} =~ m/^(7|14|21|28|35|42)$/);

	@_ = split(/,/,$in->{R_NR});
	report_error('BAD_VALUE','Number of resonant components should be number of parameters / 7 not '.scalar(@_)) unless (scalar(@_) == int($in->{PARM_CNT}/7));

	# If existing rate exists, erase it and add to report
	my $dataids = ag_man_data_exist('rlib', $in, $in_path, 'ReactionID='.$reaction_id);
	#print 'DataIDs: ',join(', ',@{$dataids}),"\n" if ($dataids);
	#exit;
	$report = ag_man_erase_data('rlib', $in, $in_path, $dataids, 'ReactionID', 'check_overwrite');
	#exit;
	$report .= ag_man_add_data('rlib', $in,
				   \&agp_MODIFY_RATE_cbk,
				   \&agp_MODIFY_RATE_info_cbk, $in_path,
				   'ReactionID,Parameters,ParmCount,Biblio,RNr,Info','',
				   'ReactionID',
				   'allow_existing_path',
				   'report_data_level');

	my $complete = 'Y';
	my $get_data_ids = $ag_db::dbh->prepare("SELECT DataID FROM rlib_links WHERE Path='".$in_path."';")
		or report_error('UNEXPECTED',"Prepare failed while getting data ids", $DBI::errstr);
	$get_data_ids->execute()
		or report_error('UNEXPECTED',"Execute failed while getting data ids", $DBI::errstr);
	
	my @row;
	my @data_ids = ();
    for ($i=0; @row = $get_data_ids -> fetchrow_array(); $i++) {
		push(@data_ids, $row[0]);
    }

	$get_data_ids->finish();
	
	if(scalar(@data_ids) != 88){
		$complete = 'N';
	}else{
		my $get_reaction_id = $ag_db::dbh->prepare("SELECT ReactionID FROM rlib_data WHERE DataID=?;")
			or report_error('UNEXPECTED',"Prepare failed while getting reaction id", $DBI::errstr);
		my @reaction_ids;
		foreach(@data_ids){
			$get_reaction_id->execute($_)
				or report_error('UNEXPECTED',"Execute failed while getting reaction id", $DBI::errstr);
			$i=0;
			@row = ();
		    for ($i=0; @row = $get_reaction_id -> fetchrow_array(); $i++) {
				push(@reaction_ids, $row[0]);
		    }
		}
		$get_reaction_id->finish();
		my @sortedReaction_ids = sort {$a <=> $b} @reaction_ids;
		my $counter = 1;
		NO_MATCH: foreach(@sortedReaction_ids){
			if($_ != $counter){
				$complete = 'N';
				last NO_MATCH;
			}
			$counter++;
		}
	}
	
	${$in}{OVERWRITE} = "Y";
	ag_man_modify_col('rlib', $in, $in_path, 'Complete', [$complete]);

	$report =~ s/\n/\x8/g;
        print "REPORT=" . $report . "\n";
    }

    sub agp_MODIFY_RATE_cbk {
	my ($in, $path, $d) = @_;
	return 0 unless ($in->{PARM_CNT});

	@{$d} = ($in->{REACTION_ID},
		 $in->{RATE_PARMS},
		 $in->{PARM_CNT},
		 $in->{BIBLIO_STRING},
		 $in->{R_NR},
		 $in->{NOTES});
	delete $in->{PARM_CNT};
	return 1;
    }

    sub agp_MODIFY_RATE_info_cbk {
	return [];
    }

# Action: MERGE RATE LIBRARIES
# Input: HEADER ID ACTION USER PW OVERWRITE PATH PATHS NOTES
# Output: REPORT
#   Merge the libraries in PATHS in increasing order into a new library in PATH.
    sub agp_MERGE_RATE_LIBRARIES {
        my $in = shift;         # input hash
	my @src_paths = split /\t/, $in->{PATHS};
	my $dest_path = get_private_path($in->{PATH}, $in);
	my $report = '';
	
	$report = 'Erased rate library in ' . $in->{PATH} . "\n"
	    if (ag_man_erase_col('rlib', $in, $dest_path, 'check_overwrite', 'no_error_if_missing'));

	$report .= 'Merged rate libraries into ' . $in->{PATH} . " with priority:\n";

	for (1..scalar(@src_paths)) {
	    $report .= "\t" . $_ . ') ' . $src_paths[$_-1] . "\n";
	}

	make_paths_private(\@src_paths, $in);
	ag_man_merge_col('rlib', $in, \@src_paths, $dest_path, $in->{NOTES});
	
	my $complete = 'Y';
	my $get_data_ids = $ag_db::dbh->prepare("SELECT DataID FROM rlib_links WHERE Path='".$dest_path."';")
		or report_error('UNEXPECTED',"Prepare failed while getting data ids", $DBI::errstr);
	$get_data_ids->execute()
		or report_error('UNEXPECTED',"Execute failed while getting data ids", $DBI::errstr);
	
	my $i;
	my @row;
	my @data_ids;
    for ($i=0; @row = $get_data_ids -> fetchrow_array(); $i++) {
		push(@data_ids, $row[0]);
    }

	$get_data_ids->finish();
	
	if(scalar(@data_ids) != 88){
		$complete = 'N';
	}else{
		my $get_reaction_id = $ag_db::dbh->prepare("SELECT ReactionID FROM rlib_data WHERE DataID=?;")
			or report_error('UNEXPECTED',"Prepare failed while getting reaction id", $DBI::errstr);
		my @reaction_ids;
		foreach(@data_ids){
			$get_reaction_id->execute($_)
				or report_error('UNEXPECTED',"Execute failed while getting reaction id", $DBI::errstr);
			$i=0;
			@row = ();
		    for ($i=0; @row = $get_reaction_id -> fetchrow_array(); $i++) {
				push(@reaction_ids, $row[0]);
		    }
		}
		$get_reaction_id->finish();
		my @sortedReaction_ids = sort {$a <=> $b} @reaction_ids;
		my $counter = 1;
		NO_MATCH: foreach(@sortedReaction_ids){
			if($_ != $counter){
				$complete = 'N';
				last NO_MATCH;
			}
			$counter++;
		}
	}
	
	${$in}{OVERWRITE} = "Y";
	ag_man_modify_col('rlib', $in, $dest_path, 'Complete', [$complete]);
	
	$report =~ s/\n/\x8/g;
        print "REPORT=" . $report . "\n";
    }

# Action: ERASE LIBRARY
# Input: HEADER ID ACTION USER PW PATH
# Output: REPORT
#   Erase rate library for a given path
    sub agp_ERASE_LIBRARY {
    	
        my $in = shift;         # input hash
	my $report = 'Erased rate library in ' . $in->{PATH} . "\n"
	    if (ag_man_erase_col('rlib', $in, get_private_path($in->{PATH}, $in)));

	$report =~ s/\n/\x8/g;
        print "REPORT=" . $report . "\n";
    }

# Action: COPY LIBRARY TO SHARED
# Input: HEADER ID ACTION USER PW PATH
# Output: REPORT
#    Copy rate library for a given path to SHARED folder
    sub agp_COPY_LIBRARY_TO_SHARED {
        my $in = shift;         # input hash
	my $priv_path = get_private_path($in->{PATH}, $in);
	my $new_path = get_new_shared_path($in->{PATH});
	my $report = 'Copied rate library in ' . $in->{PATH} . " to " . get_public_path($new_path, $in) . "\n"
	    if (ag_man_copy_col('rlib', $in, $priv_path, $new_path));
	    
	$report =~ s/\n/\x8/g;
        print "REPORT=" . $report . "\n";
    }

# Action: MOVE LIBRARY TO SHARED
# Input: HEADER ID ACTION USER PW PATH
# Output: REPORT
#   Move rate library for a given path to SHARED folder
    sub agp_MOVE_LIBRARY_TO_SHARED {
        my $in = shift;         # input hash
	my $priv_path = get_private_path($in->{PATH}, $in);
	my $new_path = get_new_shared_path($in->{PATH});
	my $report = 'Moved rate library in ' . $in->{PATH} . " to " . get_public_path($new_path, $in) . "\n"
	    if (ag_man_share_col('rlib', $in, $priv_path, $new_path));

	$report =~ s/\n/\x8/g;
        print "REPORT=" . $report . "\n";
    }

    1;
