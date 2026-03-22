#!/usr/bin/perl
    # This package contains the agm_Rate_Uncertainties module for performing CGI requests
    #
    # THIS FILE HAS BEEN AUTOMATICALLY GENERATED from an XML cgi specification
    # and should not be modified.  Instead update autogen.pm to produce the
    # desired cgi module
    #
    # AUTO GENERATION OCCURRED ON Tue Nov  7 10:18:14 2006

    package agm_Rate_Uncertainties;
    use warnings;
    use strict;
    use ag_error;
    use ag_man;
    require Exporter;

    @agm_Rate_Uncertainties::ISA = qw/Exporter/;
    @agm_Rate_Uncertainties::EXPORT = qw/
            agp_GET_RATE_UNCERTAINTIES
            agp_GET_RATE_UNCERTAINTY_DATA
            agp_SAVE_RATE_UNCERTAINTIES
            agp_ERASE_RATE_UNCERTAINTIES
            agp_COPY_RATE_UNCERTAINTIES_TO_SHARED/;

# Action: GET RATE UNCERTAINTIES
# Input: HEADER ID ACTION USER PW PATHS
# Output: PATH RUNS FOLDERS
#   Get PATHS to uncertainty files
    sub agp_GET_RATE_UNCERTAINTIES {
        my $in = shift;		# input hash
	my @paths = split /\t/, $in->{PATHS};

	make_paths_private(\@paths, $in);
	ag_man_get_col_list('rate_uncer', $in,
			    \&agp_GET_RATE_UNCERTAINTIES_grp_cbk,
			    \&agp_GET_RATE_UNCERTAINTIES_fol_cbk,
			    \&agp_GET_RATE_UNCERTAINTIES_run_cbk, \@paths);
    }

    sub agp_GET_RATE_UNCERTAINTIES_grp_cbk {
	print "PATH=",get_public_path($_[1], $_[0]),"\n"
	    if ($_[2] ne 'b' and $_[2] ne 'e');
    }

    sub agp_GET_RATE_UNCERTAINTIES_fol_cbk {
	if    ($_[2] eq 'b') {print "FOLDERS="}
	elsif ($_[2] eq 'e') {print "\n"}
	elsif ($_[2]) {print $_[1]}
	else  {print $_[1], "\t"}
    }

    sub agp_GET_RATE_UNCERTAINTIES_run_cbk {
	if    ($_[2] eq 'b') {print "RUNS="}
	elsif ($_[2] eq 'e') {print "\n"}
	elsif ($_[2]) {print $_[1]}
	else  {print $_[1], "\t"}
    }

# Action: GET RATE UNCERTAINTY DATA
# Input: HEADER ID ACTION USER PW PATH
# Output: PATH REACTION_STRING DECAY_TYPE UNCERTAINTY
#   Get rate uncertainty data
    sub agp_GET_RATE_UNCERTAINTY_DATA {
        my $in = shift;         # input hash

	ag_man_get_all_reaction_data('rate_uncer', $in, 
				     \&agp_GET_RATE_UNCERTAINTY_DATA_cbk,
				     get_private_path($in->{PATH}, $in),
				     'Uncertainty,ReactionString,DecayType','',
				     'ORDER BY reaction_lookup.ReactionID');
    }

    sub agp_GET_RATE_UNCERTAINTY_DATA_cbk {

	if ($_[3] eq 'b') {
	    print 'PATH=',get_public_path($_[2], $_[0]),"\n";
	}
	elsif ($_[3] eq 'e') {}
	else {
	    print('REACTION_STRING=',$_[1]->[1],"\n",
		  'DECAY_TYPE=',$_[1]->[2],"\n",
		  'UNCERTAINTY=',$_[1]->[0],"\n");
	}
    }

# Action: SAVE RATE UNCERTAINTIES
# Input: HEADER ID ACTION USER PW PATH RATE_UNCERTAINTY_LIST NOTES OVERWRITE
# Output: REPORT
#   Save rate uncertainty data
    sub agp_SAVE_RATE_UNCERTAINTIES {
        my $in = shift;         # input hash
	my $path = get_private_path($in->{PATH}, $in);
	my $report = '';

	$report = 'Erased rate uncertainties in ' . $in->{PATH} . "\n"
	    if (ag_man_erase_col('rate_uncer', $in, $path, 'check_overwrite', 'no_error_if_missing'));

	$report .= ag_man_add_data('rate_uncer', $in,
				   \&agp_SAVE_RATE_UNCERTAINTIES_cbk,
				   \&agp_SAVE_RATE_UNCERTAINTIES_info_cbk,
				   $path, 'ReactionID,Uncertainty', '',
				   'ReactionID', 'report_data_level');

	$report =~ s/\n/\x8/g;
        print "REPORT=" . $report . "\n";
    }

    sub agp_SAVE_RATE_UNCERTAINTIES_cbk {
	my ($in, $path, $d) = @_;
	my @all = split(/\t/, $in->{RATE_UNCERTAINTY_LIST});
	return 0 unless (scalar(@all) > 0);

	my ($reac_str, $decay, $value) = split(/,/, shift(@all));

	my ($reaction_id, $i);
	$i = ag_man_get_reactionid($reac_str, $decay, \$reaction_id);
	report_error('BAD_VALUE', $i) if ($i);

	@{$d} = ($reaction_id, $value);

	$in->{RATE_UNCERTAINTY_LIST} = join("\t", @all);
	return 1;
    }

    sub agp_SAVE_RATE_UNCERTAINTIES_info_cbk {
	return [];
    }

# Action: ERASE RATE UNCERTAINTIES
# Status: Not supported by server
# Input: HEADER ID ACTION USER PW PATH
# Output: REPORT
#   Erase rate uncertainties for a given path
    sub agp_ERASE_RATE_UNCERTAINTIES {
        my $in = shift;		# input hash
	my $report = 'Erased rate uncertainties in ' . $in->{PATH} . "\n"
	    if (ag_man_erase_col('rate_uncer', $in, get_private_path($in->{PATH}, $in)));

	$report =~ s/\n/\x8/g;
        print "REPORT=" . $report . "\n";
   }

# Action: COPY RATE UNCERTAINTIES TO SHARED
# Status: Not supported by server
# Input: HEADER ID ACTION USER PW PATH
# Output: REPORT
#   Copy rate uncertainties for a given path to SHARED folder
    sub agp_COPY_RATE_UNCERTAINTIES_TO_SHARED {
        my $in = shift;		# input hash
	my $priv_path = get_private_path($in->{PATH}, $in);
	my $new_path = get_new_shared_path($in->{PATH});
	my $report = 'Copied rate uncertainties in ' . $in->{PATH} . " to " . get_public_path($new_path, $in) . "\n"
	    if (ag_man_copy_col('rate_uncer', $in, $priv_path, $new_path));

	$report =~ s/\n/\x8/g;
        print "REPORT=" . $report . "\n";
    }

# Action: MOVE RATE UNCERTAINTIES TO SHARED
# Status: Not supported by server
# Input: HEADER ID ACTION USER PW PATH
# Output: REPORT
#   Move rate uncertainties for a given path to SHARED folder
    sub agp_MOVE_RATE_UNCERTAINTIES_TO_SHARED {
        my $in = shift;		# input hash
	my $priv_path = get_private_path($in->{PATH}, $in);
	my $new_path = get_new_shared_path($in->{PATH});
	my $report = 'Moved rate uncertainties in ' . $in->{PATH} . " to " . get_public_path($new_path, $in) . "\n"
	    if (ag_man_share_col('rate_uncer', $in, $priv_path, $new_path));

	$report =~ s/\n/\x8/g;
        print "REPORT=" . $report . "\n";
    }

    1;
