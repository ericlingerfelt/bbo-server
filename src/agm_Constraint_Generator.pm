#!/usr/bin/perl
    # This package contains the agm_Constraint_Generator module for performing CGI requests
    #
    # THIS FILE HAS BEEN AUTOMATICALLY GENERATED from an XML cgi specification
    # and should not be modified.  Instead update autogen.pm to produce the
    # desired cgi module
    #
    # AUTO GENERATION OCCURRED ON Tue Nov  7 10:18:14 2006

    package agm_Constraint_Generator;
    use warnings;
    use strict;
    use ag_error;
    use ag_man;
    use agm_BBN_Observations;
    use agm_BBN_Visualizer;
    require Exporter;

    @agm_Constraint_Generator::ISA = qw/Exporter/;
    @agm_Constraint_Generator::EXPORT = qw/
            agp_GET_CONSTRAINT_LIST
            agp_GET_CONSTRAINT_INFO
            agp_GET_CONSTRAINT_DATA
            agp_RUN_CONSTRAINT_GENERATOR
            agp_SAVE_CONSTRAINT
            agp_ERASE_CONSTRAINT
            agp_COPY_CONSTRAINT_TO_SHARED/;

# Action: GET CONSTRAINT LIST
# Input: HEADER ID ACTION USER PW PATHS
# Output: PATH RUNS FOLDERS
#   Get list of folders and constraints in list of PATHS. One set of properties is returned for every PATH in PATHS. Each line contains a property listed in the response property list. The first property in the set will always be PATH.
    sub agp_GET_CONSTRAINT_LIST {
        my $in = shift;		# input hash
	my @paths = split /\t/, $in->{PATHS};

	make_paths_private(\@paths, $in);
	ag_man_get_col_list('constraint', $in,
			    \&agp_GET_CONSTRAINT_LIST_grp_cbk,
			    \&agp_GET_CONSTRAINT_LIST_fol_cbk,
			    \&agp_GET_CONSTRAINT_LIST_run_cbk, \@paths);
    }

    sub agp_GET_CONSTRAINT_LIST_grp_cbk {
	print "PATH=",get_public_path($_[1], $_[0]),"\n"
	    if ($_[2] ne 'b' and $_[2] ne 'e');
    }

    sub agp_GET_CONSTRAINT_LIST_fol_cbk {
	if    ($_[2] eq 'b') {print "FOLDERS="}
	elsif ($_[2] eq 'e') {print "\n"}
	elsif ($_[2]) {print $_[1]}
	else  {print $_[1], "\t"}
    }

    sub agp_GET_CONSTRAINT_LIST_run_cbk {
	if    ($_[2] eq 'b') {print "RUNS="}
	elsif ($_[2] eq 'e') {print "\n"}
	elsif ($_[2]) {print $_[1]}
	else  {print $_[1], "\t"}
    }

# Action: GET CONSTRAINT INFO
# Input: HEADER ID ACTION USER PW PATHS
# Output: PATH NOTES CREATION_DATE MODIFICATION_DATE OBS_PATH BBN_RUN_PATH
#   Get information about constraints in a list of PATHS. One set of properties is returned for every PATH in PATHS. Each line contains a property listed in the response property list. The first property in the set will always be PATH.
    sub agp_GET_CONSTRAINT_INFO {
        my $in = shift;		# input hash
	my @paths = split /\t/, $in->{PATHS};

	make_paths_private(\@paths, $in);
	ag_man_get_col_info('constraint', $in,
			    \&agp_GET_CONSTRAINT_INFO_cbk, \@paths,
			    'Info,CreateDate,ModDate,ObsPath,BbnRunPath')
    }

    sub agp_GET_CONSTRAINT_INFO_cbk {
	my $v = $_[2];
	print "PATH=",get_public_path($_[1], $_[0]),"\n",
	"NOTES=@{$v}[0]\n",
	"CREATION_DATE=@{$v}[1]\n",
	"MODIFICATION_DATE=@{$v}[2]\n",
	"OBS_PATH=",get_public_path(@{$v}[3], $_[0]),"\n",
	"BBN_RUN_PATH=",get_public_path(@{$v}[4], $_[0]),"\n";
    }

# Action: GET CONSTRAINT DATA
# Input: HEADER ID ACTION USER PW PATH
# Output: PATH ISOTOPE_LABEL MIN_POINTS MID_POINTS MAX_POINTS ETA_RANGES
#   Get eta constraint intervals.  One set of properties is returned for every isotope in the observation set.  The first property returned for each isotope will always be ISOTOPE.  The point give eta as the x-value and the observation as the y-value.
    sub agp_GET_CONSTRAINT_DATA {
        my $in = shift;		# input hash

	my $n = ag_man_get_data('constraint',$in,
				\&agp_GET_CONSTRAINT_DATA_cbk,
				get_private_path($in->{PATH}, $in),
				'Label,MinPoints,MidPoints,MaxPoints,EtaRanges','');	
	report_error('INVALID_PATH',$in->{PATH}) if ($n < 1);
    }

    sub agp_GET_CONSTRAINT_DATA_cbk {
	if    ($_[3] eq 'b') {print "PATH=",get_public_path($_[2],$_[0]),"\n"}
	elsif ($_[3] eq 'e') {}
	else  {print 
              "ISOTOPE_LABEL=@{$_[1]}[0]\n",
              "MIN_POINTS=@{$_[1]}[1]\n",
              "MID_POINTS=@{$_[1]}[2]\n",
              "MAX_POINTS=@{$_[1]}[3]\n",
              "ETA_RANGES=@{$_[1]}[4]\n";
	   }
    }

# Action: RUN CONSTRAINT GENERATOR
# Input: HEADER ID ACTION USER PW OBS_PATH BBN_RUN_PATH
# Output: RUN
    sub agp_RUN_CONSTRAINT_GENERATOR {
        my $in = shift;		# input hash
	my $run_path = get_private_path($in->{BBN_RUN_PATH}, $in);
	my $obs_path = get_private_path($in->{OBS_PATH}, $in);
	my $dir = ag_man_mk_tmp_dir($in, 'constraint');
	my $results = $run_path . "\n" . $obs_path . "\n";

	# Write options to file
	write_to_file($in, $dir . "/options", \$results);

	# Make abundance files
	$results = ag_man_get_data('bbn_vis', $in,
				   \&agp_RUN_CONSTRAINT_GENERATOR_abund_cbk, $run_path,
				   'MonteCarloTrials,eta_,D_H,3He_H,4He,7Li_H,_min,_mid,_max',
				   '','order by ETA');
	report_error('NO_DATA',$in->{BBN_RUN_PATH},$run_path) if ($results < 1);

	# Make observation file
	$results = ag_man_get_data('bbn_obs', $in,
				   \&agp_RUN_CONSTRAINT_GENERATOR_obs_cbk, $obs_path,
				   'Label,ObsMin,ObsMid,ObsMax','');
	report_error('NO_DATA',$in->{OBS_PATH},$obs_path) if ($results < 1);

	# Make empty output file so program will run
	open F, "> " . $dir . "/cosmo_out.txt";
	close F;

	ag_man_fg_run($in, 'constraint', '../../../bin/constraint', [0]) == 0
	    or report_error('UNEXPECTED',"Unable to run constraint generator");
        print "RUN=SUCCESS\n";
    }

    sub agp_RUN_CONSTRAINT_GENERATOR_abund_cbk {
	my ($in, $v, $path, $status) = @_;
	if ($status eq 'b') {
	    my $dir = ag_man_get_tmp_dir($in, 'constraint');
	    open F_D, "> " . $dir . "/abund_h2_out.txt";
	    open F_He3, "> " . $dir . "/abund_he3_out.txt";
	    open F_He4, "> " . $dir . "/abund_he4_out.txt";
	    open F_Li7, "> " . $dir . "/abund_li7_out.txt";
	}
	elsif ($status eq 'e') {close F_D; close F_He3; close F_He4; close F_Li7}
	else {
	    my ($d_uncer, $he3_uncer, $he4_uncer, $li7_uncer) = 
		(9.999999e-15,9.999999e-15,9.999999e-15,9.999999e-15);

	    if (@{$v}[0] > 0) {
                # It was a Monte Carlo simulation, data is in _min,_mid,_max columns
		# initialize and set abundance values
		@{$v}[2..5] = (9.999999e-15,9.999999e-15,9.999999e-15,9.999999e-15);
		if (@{$v}[7] =~ m/(^|\t)D_H_mid=([^\t]*)/) {@{$v}[2] = $2}
		if (@{$v}[7] =~ m/(^|\t)3He_H_mid=([^\t]*)/) {@{$v}[3] = $2}
		if (@{$v}[7] =~ m/(^|\t)4He_mid=([^\t]*)/) {@{$v}[4] = $2}
		if (@{$v}[7] =~ m/(^|\t)7Li_H_mid=([^\t]*)/) {@{$v}[5] = $2}

		# set uncertainty values
		if (@{$v}[2] != 9.999999e-15 and @{$v}[8] =~ m/(^|\t)D_H_max=([^\t]*)/) {$d_uncer = $2 - @{$v}[2]}
		if (@{$v}[3] != 9.999999e-15 and @{$v}[8] =~ m/(^|\t)3He_H_max=([^\t]*)/) {$he3_uncer = $2 - @{$v}[3]}
		if (@{$v}[4] != 9.999999e-15 and @{$v}[8] =~ m/(^|\t)4He_max=([^\t]*)/) {$he4_uncer = $2 - @{$v}[4]}
		if (@{$v}[5] != 9.999999e-15 and @{$v}[8] =~ m/(^|\t)7Li_H_max=([^\t]*)/) {$li7_uncer = $2 - @{$v}[5]}
	    }

	    printf F_D "%13.6E %13.6E %13.6E\n", @{$v}[1], @{$v}[2], $d_uncer;
	    printf F_He3 "%13.6E %13.6E %13.6E\n", @{$v}[1], @{$v}[3], $he3_uncer;
	    printf F_He4 "%13.6E %13.6E %13.6E\n", @{$v}[1], @{$v}[4], $he4_uncer;
	    printf F_Li7 "%13.6E %13.6E %13.6E\n", @{$v}[1], @{$v}[5], $li7_uncer;
	}
    }

    sub agp_RUN_CONSTRAINT_GENERATOR_obs_cbk {
	my ($in, $v, $path, $status) = @_;
	if ($status eq 'b') {
	    $in->{_d_o} = '  9.99999E-15  9.99999E-15  9.99999E-15';
	    $in->{_he3_o} = '  9.99999E-15  9.99999E-15  9.99999E-15';
	    $in->{_he4_o} = '  9.99999E-15  9.99999E-15  9.99999E-15';
	    $in->{_li7_o} = '  9.99999E-15  9.99999E-15  9.99999E-15';
	}
	elsif ($status eq 'e') {
	    open F_Obs, "> " . ag_man_get_tmp_dir($in, 'constraint') . "/abund_observ.dat";
	    printf F_Obs " %12.5E %12.5E %12.5E\n", split(',',$in->{_d_o});
	    printf F_Obs " %12.5E %12.5E %12.5E\n", split(',',$in->{_he3_o});
	    printf F_Obs " %12.5E %12.5E %12.5E\n", split(',',$in->{_he4_o});
	    printf F_Obs " %12.5E %12.5E %12.5E\n", split(',',$in->{_li7_o});
	    close F_Obs;
	    delete $in->{_d_o};
	    delete $in->{_he3_o};
	    delete $in->{_he4_o};
	    delete $in->{_li7_o};
	}
	else {
	    $in->{_d_o} = join(',',@{$v}[2,1,3]) if (@{$v}[0] eq 'D/H');
	    $in->{_he3_o} = join(',',@{$v}[2,1,3]) if (@{$v}[0] eq '3He/H');
	    $in->{_he4_o} = join(',',@{$v}[2,1,3]) if (@{$v}[0] eq '4He');
	    $in->{_li7_o} = join(',',@{$v}[2,1,3]) if (@{$v}[0] eq '7Li/H');
	}
    }

# Action: SAVE CONSTRAINT
# Input: HEADER ID ACTION USER PW PATH OVERWRITE NOTES
# Output: REPORT
    sub agp_SAVE_CONSTRAINT {
        my $in = shift;		# input hash
	my $in_path = get_private_path($in->{PATH},$in);
	my $report = '';

	$report = 'Erased constraint in ' . $in->{PATH} . "\n"
	    if (ag_man_erase_col('constraint', $in, $in_path, 'check_overwrite'));
	#print "REPORT=not finished yet.\n"; exit;
	$report .= ag_man_add_data('constraint',$in,
				  \&agp_SAVE_CONSTRAINT_cbk,
				  \&agp_SAVE_CONSTRAINT_info_cbk,$in_path,
				  'MidPoints,MinPoints,MaxPoints,EtaRanges,Label',
				  'Info,Recipe,ObsPath,BbnRunPath','Label');

	delete $in->{_array_refs};
	$report =~ s/\n/\x8/g;
        print "REPORT=" . $report . "\n";
    }

    sub agp_SAVE_CONSTRAINT_info_cbk {
	my $in = shift;
	my (@ans);
	my $dir = ag_man_get_tmp_dir($in, 'constraint');

	$ans[0] = $ag_db::dbh->quote($in->{NOTES});
	$ans[0] =~ s/^'(.*)'$/$1/;
	$ans[1] = 'new cosmological constraint';

	open F, $dir . "/options"
	    or report_error('UNEXPECTED',"Error opening options file: $!");
	$ans[3] = <F>;
	$ans[2] = <F>;
	close F;

	chomp(@ans);

	# now do setup for data callback
	open F, $dir . "/fort.20"
	    or report_error('UNEXPECTED',"Error opening eta ranges file: $!");
	my @eta_ranges = <F>;
	close F;

	my ($file_num, $range_num, @all, $curve, $all_i, @obs, @data, @array_refs);
	for $file_num (10..13) {
	    $/ = "&\n";
	    open F, $dir . "/fort." . $file_num
		or report_error('UNEXPECTED',"Error opening output file $file_num: $!");
	    @all = <F>;
	    close F;
	    chomp @all;

	    if (scalar(@all) == 15) {$range_num = 1}
	    elsif (scalar(@all) == 24) {$range_num = 2}
	    else {report_error('BUG',"non standard fort.$file_num from constraint generator")}
	    
	    report_error('BUG',"non standard fort.$file_num from constraint generator")
		if (scalar(@all) != (1+$range_num*3)*3 + 3);

	    @obs = ();
	    @data = ('', '', '', '', '');

	    # First get observation values for checking
	    for (0..2) {
		report_error('BUG',"non standard fort.$file_num from constraint generator") 
		    if ($all[$_] !~ m/ ( [\d\.E\+-]+)\n/);
		push @obs, $1;
	    }

	    # Load all min, mid, and max points into @data
	    $all_i = 4;
	    for (0..2) {                 # eta curve, 0 for mid, 1 for min, 2 for max
		for (1..$range_num) {    # number of intervals
		    for $curve (0..2) {  # observation curve, 0 for mid, 1 for min, 2 for max
			report_error('BUG',"non standard fort.$file_num from constraint generator") 
			    if ($all[$all_i] !~ m/ ( [\d\.E\+-]+)\n/ 
				or $obs[$curve] ne $1 
				or $all[$all_i] !~ m/  ([\d\.E\+-]+)  ([\d\.E\+-]+)\n/);
			$data[$curve] .= $1 . ',' . $2 . "\t" if ($1 ne '1.66000E-25');
			$all_i++;
		    }
		}
		$all_i++;
	    }
	    
	    # Strip trailing tab from @data
	    for (0..2) {$data[$_] =~ s/\t$//}

	    # Load isotope and eta ranges into @data
	    $_ = shift @eta_ranges;
	    $_ =~ m/^(\w+) / if (defined $_); # get first word

	    if ($1 eq 'h2' and $file_num = 10) {$data[4] = 'D/H'}
	    elsif ($1 eq 'he3' and $file_num = 11) {$data[4] = '3He/H'}
	    elsif ($1 eq 'he4' and $file_num = 11) {$data[4] = '4He'}
	    elsif ($1 eq 'li7' and $file_num = 11) {$data[4] = '7Li/H'}
	    else {report_error('BUG',"non standard fort.$file_num from constraint generator","isotope mismatch")}
	    
	    while (defined($eta_ranges[0]) and $eta_ranges[0] =~ m/^  \d\.\d/) {
		$_ = shift @eta_ranges;
		report_error('BUG',"non standard fort.$file_num from constraint generator")
		    if ($_ !~ m/^  ([\d\.E\+-]+) .* ([\d\.E\+-]+)$/); # get first and last number
		$data[3] .= $1 . ',' . $2 . "\t" if ($1 ne '9.99900E-25' and $1 ne '0.00000E+00');
	    }
	    $data[3] =~ s/\t$//;

	    push @array_refs, [@data];
	}

	$in->{_array_refs} = \@array_refs;
	$/ = "n";
	return \@ans;
    }

    sub agp_SAVE_CONSTRAINT_cbk {
	my ($in, $path, $d) = @_;
	my $a = $in->{_array_refs};

	return 0 if (scalar(@{$a}) < 1);
	@{$d} = @{shift @{$a}};
	return 1;
    }

# Action: ERASE CONSTRAINT
# Input: HEADER ID ACTION USER PW PATH
# Output: REPORT
#   Erase constraint for a given path
    sub agp_ERASE_CONSTRAINT {
        my $in = shift;         # input hash
	my $report = 'Erased constraint in ' . $in->{PATH} . "\n"
	    if (ag_man_erase_col('constraint', $in, get_private_path($in->{PATH}, $in)));

	$report =~ s/\n/\x8/g;
        print "REPORT=" . $report . "\n";
    }

# Action: COPY CONSTRAINT TO SHARED
# Input: HEADER ID ACTION USER PW PATH
# Output: REPORT
#   Copy constraint for a given path to SHARED folder
    sub agp_COPY_CONSTRAINT_TO_SHARED {
        my $in = shift;         # input hash
	my $priv_path = get_private_path($in->{PATH}, $in);
	my $new_path = get_new_shared_path($in->{PATH});
	my $report = 'Copied constraint in ' . $in->{PATH} . " to " . get_public_path($new_path, $in) . "\n"
	    if (ag_man_copy_col('constraint', $in, $priv_path, $new_path));

	my $obs_path = get_db_value("SELECT ObsPath from constraint_info where Path='".$priv_path."'"
									, 'getting obs path');
	
	if($obs_path =~ /USER/){							
		my $new_obs_path = get_new_shared_path(get_public_path($obs_path, $in));
		$report .= "\nCopied observation in " . get_public_path($obs_path, $in) . " to " . $new_obs_path . "\n"
	    if (ag_man_copy_col('bbn_obs', $in, $obs_path, $new_obs_path));
	}
	
	my $bbn_run_path = get_db_value("SELECT BbnRunPath from constraint_info where Path='".$priv_path."'"
									, 'getting bbn run path');

	if($bbn_run_path =~ /USER/){		
		my $new_bbn_run_path = get_new_shared_path(get_public_path($bbn_run_path, $in));
		$report .= "\nCopied simulation in " . get_public_path($bbn_run_path, $in) . " to " . $new_bbn_run_path . "\n"
	    if (ag_man_copy_col('bbn_vis', $in, $bbn_run_path, $new_bbn_run_path));
	}

	$report =~ s/\n/\x8/g;
        print "REPORT=" . $report . "\n";
    }

# Action: MOVE CONSTRAINT TO SHARED
# Input: HEADER ID ACTION USER PW PATH
# Output: REPORT
#   Move constraint for a given path to SHARED folder
    sub agp_MOVE_CONSTRAINT_TO_SHARED {
        my $in = shift;         # input hash
	my $priv_path = get_private_path($in->{PATH}, $in);
	my $new_path = get_new_shared_path($in->{PATH});
	my $report = 'Moved constraint in ' . $in->{PATH} . " to " . get_public_path($new_path, $in) . "\n"
	    if (ag_man_share_col('constraint', $in, $priv_path, $new_path));

	$report =~ s/\n/\x8/g;
        print "REPORT=" . $report . "\n";
    }
                                                                                                                      
    1;
