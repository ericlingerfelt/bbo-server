#!/usr/bin/perl
    # This package contains the agm_BBN_Visualizer module for performing CGI requests
    #
    # THIS FILE HAS BEEN AUTOMATICALLY GENERATED from an XML cgi specification
    # and should not be modified.  Instead update autogen.pm to produce the
    # desired cgi module
    #
    # AUTO GENERATION OCCURRED ON Tue Nov  7 10:18:14 2006

    package agm_BBN_Visualizer;
    use warnings;
    use strict;
    use ag_error;
    use ag_man;
    require Exporter;

    @agm_BBN_Visualizer::ISA = qw/Exporter/;
    @agm_BBN_Visualizer::EXPORT = qw/
            agp_GET_BBN_RUN_LIST
            agp_GET_BBN_RUN_INFO
            agp_GET_BBN_RUN_DATA
            agp_BBN_RUN_EXIST
            agp_ERASE_BBN_RUN
            agp_COPY_BBN_RUN_TO_SHARED/;

# Action: GET BBN RUN LIST
# Input: HEADER ID ACTION USER PW PATHS
# Output: PATH RUNS FOLDERS
#   Get list of folders and simulations in list of PATHS.  One set of properties is returned for every PATH in PATHS.  Each line contains a property listed in the response property list.  The first property in the set will always be PATH.
    sub agp_GET_BBN_RUN_LIST {
        my $in = shift;		# input hash
	my @paths = split /\t/, $in->{PATHS};

	make_paths_private(\@paths, $in);
	ag_man_get_col_list('bbn_vis', $in,
			    \&agp_GET_BBN_RUN_LIST_grp_cbk,
			    \&agp_GET_BBN_RUN_LIST_fol_cbk,
			    \&agp_GET_BBN_RUN_LIST_run_cbk, \@paths);
    }

    sub agp_GET_BBN_RUN_LIST_grp_cbk {
	print "PATH=",get_public_path($_[1], $_[0]),"\n"
	    if ($_[2] ne 'b' and $_[2] ne 'e');
    }

    sub agp_GET_BBN_RUN_LIST_fol_cbk {
	if    ($_[2] eq 'b') {print "FOLDERS="}
	elsif ($_[2] eq 'e') {print "\n"}
	elsif ($_[2]) {print $_[1]}
	else  {print $_[1], "\t"}
    }

    sub agp_GET_BBN_RUN_LIST_run_cbk {
	if    ($_[2] eq 'b') {print "RUNS="}
	elsif ($_[2] eq 'e') {print "\n"}
	elsif ($_[2]) {print $_[1]}
	else  {print $_[1], "\t"}
    }

# Action: GET BBN RUN INFO
# Input: HEADER ID ACTION USER PW PATHS
# Output: PATH NOTES CREATION_DATE MODIFICATION_DATE MONTE_CARLO_LIST LOOPING_LIST LIBRARY RATE_UNCERTAINITY_PATH MONTE_CARLO_TRIALS RECIPE
#   Get list of properties set in the BBN_SIM_COMMAND for a list of PATHS.  One set of properties is returned for every PATH in PATHS.  Each line contains a property listed in the response property list.  The first property in the set will always be PATH.
    sub agp_GET_BBN_RUN_INFO {
        my $in = shift;		# input hash
	my @paths = split /\t/, $in->{PATHS};

	make_paths_private(\@paths, $in);
	ag_man_get_col_info('bbn_vis', $in,
			    \&agp_GET_BBN_RUN_INFO_cbk, \@paths,
			    'Info,CreateDate,ModDate,MonteCarloList,LoopingList,LibPath,RateUncerPath,MonteCarloTrials,Recipe')
    }

    sub agp_GET_BBN_RUN_INFO_cbk {
	my $v = $_[2];
	print "PATH=",get_public_path($_[1],$_[0]),"\n",
	"NOTES=@{$v}[0]\n",
	"CREATION_DATE=@{$v}[1]\n",
	"MODIFICATION_DATE=@{$v}[2]\n",
	"MONTE_CARLO_LIST=@{$v}[3]\n",
	"LOOPING_LIST=@{$v}[4]\n",
	"LIBRARY=@{$v}[5]\n",
	"RATE_UNCERTAINITY_PATH=@{$v}[6]\n",
	"MONTE_CARLO_TRIALS=@{$v}[7]\n",
	"RECIPE=@{$v}[8]\n";
    }

# Action: GET BBN RUN DATA
# Input: HEADER ID ACTION USER PW GET_BBN_DATA_COMMAND
# Output: PATH PARAMETERS CONSTRAINTS DATA
#   Get data from a bbn simulation.  One set of properties is returned for every table of data meeting the conditions in GET_BBN_DATA_COMMAND.  Each item in PARAMETERS corresponds with a column in DATA.  The first property returned for each table will always be PATH.
    sub agp_GET_BBN_RUN_DATA {
        my $in = shift;		# input hash

	my ($path, $quantities, $for_values) = ('', '', '');
	if (not $in->{GET_BBN_DATA_COMMAND} =~ m<^get bbn data for "([\w \.\+-/]+)" return(( quantity( final)? [\w/&]+)+)(( for [\w/]+ as [-\+]?\d*\.?\d+([Ee][-\+]?\d+)?)*)$>)
	    {report_error('BUG','need to update syntax for GET_BBN_DATA_COMMAND in agp_GET_BBN_RUN_DATA')};

#        print "PATH=/PUBLIC/No Data\n","PARAMETERS=ETA   NEUTRON_LIFETIME\n","CONSTRAINTS=with ETA as 0.1\n","DATA=1,2\t3,4\n";exit;

	$path = $1 if (defined $1);
	$quantities = $2 if (defined $2);
	$for_values = $5 if (defined $5);
	$for_values =~ s/^ for //;         # remove initial for label

	my $priv_path = get_private_path($path, $in);
	my $columns = 'Constraints' . bbn_quantity2db_column($quantities);
	my $conditions = bbn_quantity2db_column($for_values);
	
	$columns =~ s/ quantity (final )?/,/g;
	$conditions =~ s/ for / AND /g;
	$conditions =~ s/ as ([\w\+\.-]+)/='$1'/g;

	# Temporarily store $quantities in $in to pass to callback subroutine
	$in->{quantities_} = $quantities;

	my $results = ag_man_get_data('bbn_vis', $in,
				      \&agp_GET_BBN_RUN_DATA_cbk, $priv_path,
				      $columns, $conditions);

	report_error('NO_DATA',$path,$for_values,$priv_path) if ($results < 1);
	delete $in->{quantities_};
    }

    sub agp_GET_BBN_RUN_DATA_cbk {
	if    ($_[3] eq 'b') {print "PATH=",get_public_path($_[2],$_[0]),"\n";}
	elsif ($_[3] eq 'e') {}
	else  {
	    my $v = $_[1];
	    my @q = ();  # holds names of quantities
	    my @f = ();  # holds true if should only return final value
	    my $i;
	    for $i (split(/ quantity /, $_[0]->{quantities_})) {
		if ($i) {
		    $i =~ m/(final )?(.*)/;
		    push @q, $2;
		    push @f, (defined $1 ? 1 : 0);
		}
	    }

	    print "PARAMETERS=",join("\t",@q),"\n";
	    print "CONSTRAINTS=",@{$v}[0],"\n";
	    print "DATA=";
	    for $i (0 .. scalar @q - 1) {
		print "\t" if $i > 0;
		if ($q[$i] =~ m/(_min|_mid|_max)$/) {
		    my $name = bbn_quantity2db_column($q[$i],1);
		    if (@{$v}[$i+1] =~ m/(^|\t)$name=([^\t]*)/) {@{$v}[$i+1] = $2}
		    else {@{$v}[$i+1] = ''}
		}
		if (defined @{$v}[$i+1]) {
		    if ($f[$i]) {@{$v}[$i+1] =~ s/^.*,//}
		    print @{$v}[$i+1];
		}
	    }
	    print "\n";
	}
    }

# Action: BBN RUN EXIST
# Input: HEADER ID ACTION USER PW PATHS
# Output: PATH EXISTS
#   Check if path to a bbn simulation is used.  One set of properties is returned for every path in PATHS.
    sub agp_BBN_RUN_EXIST {
        my $in = shift;         # input hash
	my @paths = split /\t/, $in->{PATHS};

	make_paths_private(\@paths, $in);
	my $exists = ag_man_col_exist('bbn_vis', $in, \@paths, 'print_output');
    }
 
# Action: ERASE BBN RUN
# Input: HEADER ID ACTION USER PW PATH
# Output: REPORT
#   Erase bbn run for a given path
    sub agp_ERASE_BBN_RUN {
        my $in = shift;         # input hash
	my $report = 'Erased simulation in ' . $in->{PATH} . "\n"
	    if (ag_man_erase_col('bbn_vis', $in, get_private_path($in->{PATH}, $in)));

	$report =~ s/\n/\x8/g;
        print "REPORT=" . $report . "\n";
    }

# Action: COPY BBN RUN TO SHARED
# Status: Not supported by server
# Input: HEADER ID ACTION USER PW PATH
# Output: REPORT
#   Copy bbn run for a given path to SHARED folder
    sub agp_COPY_BBN_RUN_TO_SHARED {
        my $in = shift;         # input hash
	my $priv_path = get_private_path($in->{PATH}, $in);
	my $new_path = get_new_shared_path($in->{PATH});
	my $report = 'Copied simulation in ' . $in->{PATH} . " to " . get_public_path($new_path, $in) . "\n"
	    if (ag_man_copy_col('bbn_vis', $in, $priv_path, $new_path));

	$report =~ s/\n/\x8/g;
        print "REPORT=" . $report . "\n";
    }


# Action: MOVE BBN RUN TO SHARED
# Status: Not supported by server
# Input: HEADER ID ACTION USER PW PATH
# Output: REPORT
#   Move bbn run for a given path to SHARED folder
    sub agp_MOVE_BBN_RUN_TO_SHARED {
        my $in = shift;         # input hash
	my $priv_path = get_private_path($in->{PATH}, $in);
	my $new_path = get_new_shared_path($in->{PATH});
	my $report = 'Moved simulation in ' . $in->{PATH} . " to " . get_public_path($new_path, $in) . "\n"
	    if (ag_man_share_col('bbn_vis', $in, $priv_path, $new_path));

	$report =~ s/\n/\x8/g;
        print "REPORT=" . $report . "\n";
    }

    # Warning: this will create duplicate columns for _min,_mid,_max that should be eliminated
    sub bbn_quantity2db_column {
	my $q = shift;
	my $replace_mid = shift;
	# There are two etas, Target eta for simulation is in all CAPS
	# Final eta from simulation is all lowercase
	# Since database is case insensitive, eta is representated as eta_ in database
	$q =~ s/eta/eta_/g;
	# Only allow alphanumeric, spaces, or characters needed for exponential notation
	# Everything else is converted to an underscore in database columns
	$q =~ s/[^\w \.\+-]/_/g;
	# Anything ending in _min, _mid, _max is stored in 3 columns regardless of quantity
	$q =~ s/\w+(_min|_mid|_max)/$1/g unless (defined $replace_mid and $replace_mid);
	return $q;
    }
    1;

