#!/usr/bin/perl
    # This package contains the agm_BBN_Observations module for performing CGI requests
    #
    # THIS FILE HAS BEEN AUTOMATICALLY GENERATED from an XML cgi specification
    # and should not be modified.  Instead update autogen.pm to produce the
    # desired cgi module
    #
    # AUTO GENERATION OCCURRED ON Tue Nov  7 10:18:14 2006

    package agm_BBN_Observations;
    use warnings;
    use strict;
    use ag_error;
    use ag_man;
    require Exporter;

    @agm_BBN_Observations::ISA = qw/Exporter/;
    @agm_BBN_Observations::EXPORT = qw/
            agp_GET_OBS_LIST
            agp_GET_OBS_INFO
            agp_GET_OBS_DATA
            agp_OBS_EXIST
            agp_SAVE_OBS
            agp_ERASE_OBS
            agp_COPY_OBS_TO_SHARED/;

# Action: GET OBS LIST
# Input: HEADER ID ACTION USER PW PATHS
# Output: PATH RUNS FOLDERS
#   Get list of folders and observations in list of PATHS. One set of properties is returned for every PATH in PATHS. Each line contains a property listed in the response property list. The first property in the set will always be PATH.
    sub agp_GET_OBS_LIST {
        my $in = shift;		# input hash
	my @paths = split /\t/, $in->{PATHS};

	make_paths_private(\@paths, $in);
	ag_man_get_col_list('bbn_obs', $in,
			    \&agp_GET_OBS_LIST_grp_cbk,
			    \&agp_GET_OBS_LIST_fol_cbk,
			    \&agp_GET_OBS_LIST_run_cbk, \@paths);
    }

    sub agp_GET_OBS_LIST_grp_cbk {
	print "PATH=",get_public_path($_[1], $_[0]),"\n"
	    if ($_[2] ne 'b' and $_[2] ne 'e');
    }

    sub agp_GET_OBS_LIST_fol_cbk {
	if    ($_[2] eq 'b') {print "FOLDERS="}
	elsif ($_[2] eq 'e') {print "\n"}
	elsif ($_[2]) {print $_[1]}
	else  {print $_[1], "\t"}
    }

    sub agp_GET_OBS_LIST_run_cbk {
	if    ($_[2] eq 'b') {print "RUNS="}
	elsif ($_[2] eq 'e') {print "\n"}
	elsif ($_[2]) {print $_[1]}
	else  {print $_[1], "\t"}
    }

# Action: GET OBS INFO
# Input: HEADER ID ACTION USER PW PATHS
# Output: PATH NOTES CREATION_DATE MODIFICATION_DATE
#   Get information about constraints in a list of PATHS. One set of properties is returned for every PATH in PATHS. Each line contains a property listed in the response property list. The first property in the set will always be PATH.
    sub agp_GET_OBS_INFO {
        my $in = shift;		# input hash
	my @paths = split /\t/, $in->{PATHS};

 	make_paths_private(\@paths, $in);
	ag_man_get_col_info('bbn_obs', $in,
			    \&agp_GET_OBS_INFO_cbk, \@paths,
			    'Info,CreateDate,ModDate')
    }

    sub agp_GET_OBS_INFO_cbk {
	my $v = $_[2];
	print "PATH=",get_public_path($_[1], $_[0]),"\n",
	"NOTES=@{$v}[0]\n",
	"CREATION_DATE=@{$v}[1]\n",
	"MODIFICATION_DATE=@{$v}[2]\n";
    }

# Action: GET OBS DATA
# Input: HEADER ID ACTION USER PW PATH
# Output: PATH ISOTOPE_LABEL MIN MID MAX
#   Get abundance observation values.  One set of properties is returned for every isotope in the observation set.  The first property returned for each isotope will always be ISOTOPE.
    sub agp_GET_OBS_DATA {
        my $in = shift;		# input hash

	my $n = ag_man_get_data('bbn_obs',$in,
				\&agp_GET_OBS_DATA_cbk,
				get_private_path($in->{PATH}, $in),
				'Label,ObsMin,ObsMid,ObsMax','');
	report_error('INVALID_PATH',$in->{PATH}) if ($n < 1);
    }

    sub agp_GET_OBS_DATA_cbk {
	if    ($_[3] eq 'b') {print "PATH=",get_public_path($_[2],$_[0]),"\n"}
	elsif ($_[3] eq 'e') {}
	else  {print 
		   "ISOTOPE_LABEL=@{$_[1]}[0]\n",
		   "MIN=@{$_[1]}[1]\n",
		   "MID=@{$_[1]}[2]\n",
		   "MAX=@{$_[1]}[3]\n";
	}
    }

# Action: OBS EXIST
# Input: HEADER ID ACTION USER PW PATHS
# Output: PATH EXISTS
#   Check if path to a bbn obsveration is used.  One set of properties is returned for every path in PATHS.
    sub agp_OBS_EXIST {
        my $in = shift;		# input hash
	my @paths = split /\t/, $in->{PATHS};

	make_paths_private(\@paths, $in);
	my $exists = ag_man_col_exist('bbn_obs', $in, \@paths, 'print_output');
    }

# Action: SAVE OBS
# Input: HEADER ID ACTION USER PW PATH NOTES OVERWRITE OBSERVATIONS
# Output: REPORT
#   Save abundance observation values
    sub agp_SAVE_OBS {
        my $in = shift;		# input hash
	my $in_path = get_private_path($in->{PATH},$in);
	my $report = '';

	$report = 'Erased observation in ' . $in->{PATH} . "\n"
	    if (ag_man_erase_col('bbn_obs', $in, $in_path, 'check_overwrite'));
	$report .= ag_man_add_data('bbn_obs',$in,
				   \&agp_SAVE_OBS_cbk,
				   \&agp_SAVE_OBS_info_cbk,$in_path,
				   'Label,ObsMin,ObsMid,ObsMax',
				   'Info,Recipe','Label');

	$report =~ s/\n/\x8/g;
        print "REPORT=" . $report . "\n";
    }

    sub agp_SAVE_OBS_info_cbk {
	my @ans;
	$ans[0] = $ag_db::dbh->quote($_[0]->{NOTES});
	$ans[0] =~ s/^'(.*)'$/$1/;
	$ans[1] = 'New bbn observation';
	return \@ans;
    }

    sub agp_SAVE_OBS_cbk {
	my ($in, $path, $d) = @_;
	@{$d} = ();  # erase array
	return 0 if (not $in->{OBSERVATIONS});

	$in->{OBSERVATIONS} =~ s/^([^\t]+)\t?//;
	@{$d} = split(',',$1);
	return 1;
    }

# Action: ERASE OBS
# Input: HEADER ID ACTION USER PW PATH
# Output: REPORT
#   Erase abundance observation for a given path
    sub agp_ERASE_OBS {
        my $in = shift;		# input hash
	my $report = 'Erased observation in ' . $in->{PATH} . "\n"
	    if (ag_man_erase_col('bbn_obs', $in, get_private_path($in->{PATH}, $in)));

	$report =~ s/\n/\x8/g;
        print "REPORT=" . $report . "\n";
    }

# Action: COPY OBS TO SHARED
# Input: HEADER ID ACTION USER PW PATH
# Output: REPORT
#   Copy abundance observation for a given path to SHARED folder
    sub agp_COPY_OBS_TO_SHARED {
        my $in = shift;		# input hash
	my $priv_path = get_private_path($in->{PATH}, $in);
	my $new_path = get_new_shared_path($in->{PATH});
	my $report = 'Copied observation in ' . $in->{PATH} . " to " . get_public_path($new_path, $in) . "\n"
	    if (ag_man_copy_col('bbn_obs', $in, $priv_path, $new_path));

	$report =~ s/\n/\x8/g;
        print "REPORT=" . $report . "\n";
    }

# Action: MOVE OBS TO SHARED
# Input: HEADER ID ACTION USER PW PATH
# Output: REPORT
#   Move abundance observation for a given path to SHARED folder
    sub agp_MOVE_OBS_TO_SHARED {
        my $in = shift;		# input hash
	my $priv_path = get_private_path($in->{PATH}, $in);
	my $new_path = get_new_shared_path($in->{PATH});
	my $report = 'Moved observation in ' . $in->{PATH} . " to " . get_public_path($new_path, $in) . "\n"
	    if (ag_man_share_col('bbn_obs', $in, $priv_path, $new_path));

	$report =~ s/\n/\x8/g;
        print "REPORT=" . $report . "\n";
    }

    1;
