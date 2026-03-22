#!/usr/bin/perl
    # This package contains the agm_BBN_Simulator module for performing CGI requests
    #
    # THIS FILE HAS BEEN AUTOMATICALLY GENERATED from an XML cgi specification
    # and should not be modified.  Instead update autogen.pm to produce the
    # desired cgi module
    #
    # AUTO GENERATION OCCURRED ON Tue Nov  7 10:18:14 2006

    package agm_BBN_Simulator;
    use warnings;
    use strict;
    use ag_error;
    use ag_man;
    require Exporter;

    @agm_BBN_Simulator::ISA = qw/Exporter/;
    @agm_BBN_Simulator::EXPORT = qw/
            agp_GET_BBN_SIM_TYPES
            agp_GET_BBN_LOOPING_DEFAULTS
            agp_BBN_SIM_SETUP
            agp_RUN_BBN_SIM
            agp_BBN_SIM_UPDATE
            agp_ABORT_BBN_SIM
            agp_SAVE_BBN_SIM/;

# Action: GET BBN SIM TYPES
# Input: HEADER ID ACTION USER PW
# Output: SIMULATION_TYPE DESCRIPTION TIME_STEP_CONSTANT1 TIME_STEP_CONSTANT2 INITIAL_TIMESTEP INITIAL_TEMPERATURE FINAL_TEMPERATURE SMALLEST_ABUND_ALLOWED ACCUMULATION_INCREMENT GRAVITATIONAL_CONSTANT NEUTRON_LIFETIME NUMBER_NEUTRINO_SPECIES ETA COSMOLOGICAL_CONSTANT XI_ELECTRON XI_MUON XI_TAUON
#   Get list of available BBN simulations and all default values.  One set of properties is returned for every simulation type.  Each line contains a property listed in the response property list.  The first property in the set will always be the SIMULATION_TYPE.
    sub agp_GET_BBN_SIM_TYPES {
        my $in = shift;		# input hash

	ag_man_get_col_list('bbn_sim_types', $in,
			    \&agp_GET_BBN_SIM_TYPES_LIST_grp_cbk,
			    \&agp_GET_BBN_SIM_TYPES_LIST_fol_cbk,
			    \&agp_GET_BBN_SIM_TYPES_LIST_run_cbk,
			    ['/PUBLIC/']);
    }

    sub agp_GET_BBN_SIM_TYPES_LIST_grp_cbk {
    }

    sub agp_GET_BBN_SIM_TYPES_LIST_fol_cbk {
    }

    sub agp_GET_BBN_SIM_TYPES_LIST_run_cbk {
	if    ($_[2] eq 'b') {}
	elsif ($_[2] eq 'e') {}
	else  {
	    ag_man_get_col_info('bbn_sim_types', $_[0],
				\&agp_GET_BBN_SIM_TYPES_INFO_cbk,
				["/PUBLIC/$_[1]"],
				'SimType,Info,TIME_STEP_CONSTANT1,TIME_STEP_CONSTANT2,INITIAL_TIMESTEP,INITIAL_TEMPERATURE,FINAL_TEMPERATURE,SMALLEST_ABUND_ALLOWED,ACCUMULATION_INCREMENT,GRAVITATIONAL_CONSTANT,NEUTRON_LIFETIME,NUMBER_NEUTRINO_SPECIES,ETA,COSMOLOGICAL_CONSTANT,XI_ELECTRON,XI_MUON,XI_TAUON');
	}
    }
 
    sub agp_GET_BBN_SIM_TYPES_INFO_cbk {
	my $v = $_[2];
        print 
	    "SIMULATION_TYPE=@{$v}[0]\n",
	    "DESCRIPTION=@{$v}[1]\n",
	    "TIME_STEP_CONSTANT1=@{$v}[2]\n",
	    "TIME_STEP_CONSTANT2=@{$v}[3]\n",
	    "INITIAL_TIMESTEP=@{$v}[4]\n",
	    "INITIAL_TEMPERATURE=@{$v}[5]\n",
	    "FINAL_TEMPERATURE=@{$v}[6]\n",
	    "SMALLEST_ABUND_ALLOWED=@{$v}[7]\n",
	    "ACCUMULATION_INCREMENT=@{$v}[8]\n",
	    "GRAVITATIONAL_CONSTANT=@{$v}[9]\n",
	    "NEUTRON_LIFETIME=@{$v}[10]\n",
	    "NUMBER_NEUTRINO_SPECIES=@{$v}[11]\n",
	    "ETA=@{$v}[12]\n",
	    "COSMOLOGICAL_CONSTANT=@{$v}[13]\n",
	    "XI_ELECTRON=@{$v}[14]\n",
	    "XI_MUON=@{$v}[15]\n",
	    "XI_TAUON=@{$v}[16]\n";
    }

# Action: GET BBN LOOPING DEFAULTS
# Input: HEADER ID ACTION USER PW PARAMETERS
# Output: PARAMETER MIN MAX INCREMENT_LIN INCREMENT_LOG LOWER_BOUND UPPER_BOUND
#   Get looping defaults for bbn simulator.  One set of properties is returned for every quantity in PARAMETERS.  Each line contains a property listed in the response property list.  The first property in the set will always be the PARAMETER.
    sub agp_GET_BBN_LOOPING_DEFAULTS {
        my $in = shift;		# input hash
	my @labels = split /\t/, $in->{PARAMETERS};
	my $constraint = "Label = '" . join("' OR Label = '", @labels) . "'";

	my $n = ag_man_get_data('loop_def',$in,
				\&agp_GET_BBN_LOOPING_DATA_cbk,
				'/PUBLIC/default',
				'Label,SuggestedMin,SuggestedMax,IncrementLin,IncrementLog,LowerBound,UpperBound',
				$constraint);

	report_error('UNEXPECTED','some invalid PARAMETERS',join(',',@labels)) if ($n != scalar @labels);
    }

    sub agp_GET_BBN_LOOPING_DATA_cbk {
	if    ($_[3] eq 'b') {}
	elsif ($_[3] eq 'e') {}
	else {print 
		  "PARAMETER=@{$_[1]}[0]\n",
		  "MIN=@{$_[1]}[1]\n",
		  "MAX=@{$_[1]}[2]\n",
		  "INCREMENT_LIN=@{$_[1]}[3]\n",
		  "INCREMENT_LOG=@{$_[1]}[4]\n",
		  "LOWER_BOUND=@{$_[1]}[5]\n",
		  "UPPER_BOUND=@{$_[1]}[6]\n";
	  }
    }

# Action: BBN SIM SETUP
# Status: Uses default library
# Input: HEADER ID ACTION USER PW SIMULATION_TYPE LIBRARY
# Output: REPORT SUMMARY MIN_ISOTOPE MAX_ISOTOPE
#   Prepare library for a BBN simulation
    sub agp_BBN_SIM_SETUP {
        my $in = shift;		# input hash
	my $dir = ag_man_mk_tmp_dir($in, 'bbn_sim');
	my $lib = get_private_path($in->{LIBRARY}, $in);
	my $newlib = $lib;
	my $report = "Preparing to run BBN simulator.\n\n";

	open BSO, "> $dir/options"
	    or report_error('UNEXPECTED',"Error opening options file: $!");
	open RAH, "> $dir/rates.dat"
	    or report_error('UNEXPECTED',"Error opening rates.dat file: $!");
	open SWH, "> $dir/switch.dat"
	    or report_error('UNEXPECTED',"Error opening switch.dat file: $!");

	my $path = get_db_value("SELECT Path FROM bbn_sim_types_info WHERE SimType='$in->{SIMULATION_TYPE}'", "searching for '$in->{SIMULATION_TYPE}'");
	print BSO $path, "\n";
	
	$report .= "Creating reaction rate library from ".$lib.".\n\n";
	print BSO $lib,"\n";

	# Merge with all rates
	unless ($lib eq '/PUBLIC/BBN_ref_01') {
	    my (@details, %all, $r);
	    $newlib = get_private_path('/USER/HIDDEN/'.$in->{ID}, $in);
	    $report .= "Merging with /PUBLIC/BBN_ref_01 to complete simulation library.\n";

	    my $sth = $ag_db::dbh->prepare('SELECT ReactionString,DecayType FROM reaction_lookup WHERE ReactionID=? LIMIT 1;')
		or report_error('UNEXPECTED',"Prepare failed while looking up reaction id", $DBI::errstr);

	    ag_man_erase_col('rlib', $in, $newlib, 'no_error_if_missing');
	    ag_man_merge_col('rlib', $in, [$lib, '/PUBLIC/BBN_ref_01'],
			     $newlib, '', \@details);
	    for (@details) {
		if (index($_, '/') == 0) {$report .= "\nRates from ".$_.":\n\n"}
		elsif ($_ <= 88) {
		    $sth->execute($_)
			or report_error('UNEXPECTED',"Execute failed while looking up reaction id", $_, $DBI::errstr);
		    $r = $sth -> fetchrow_arrayref();
		    if ($r->[1]) {$r = $r->[0].' ['.$r->[1].']'}
		    else {$r = $r->[0]}

		    $report .= '   '.$_."\t".$r."\n";
		    $all{$_} = 1;
		}
	    }

	    $report .= "\nDynamically calculated rates:\n\n";
	    for (1..88) {
		unless ($all{$_}) {
		    $sth->execute($_)
			or report_error('UNEXPECTED',"Execute failed while looking up reaction id", $_, $DBI::errstr);
		    $r = $sth -> fetchrow_arrayref();
		    if ($r->[1]) {$r = $r->[0].' ['.$r->[1].']'}
		    else {$r = $r->[0]}

		    $report .= '   '.$_."\t".$r."\n";
		}
	    }
	    
	    ag_man_get_all_reaction_data('rlib', $in,
					 \&agp_BBN_SIM_SETUP_cbk, $newlib,
					 'rlib_data.ReactionID,ReactionString,DecayType,ParmCount,Parameters',
					 'rlib_data.ReactionID <= 88',
					 'ORDER BY rlib_data.ReactionID');

	    ag_man_erase_col('rlib', $in, $newlib, 'no_error_if_missing');
	}
	else {
		
		$report .= "\nDynamically calculated rates:\n\n 1	n --> 1H [bet-]";
		
	    ag_man_get_all_reaction_data('rlib', $in,
					 \&agp_BBN_SIM_SETUP_cbk, $newlib,
					 'rlib_data.ReactionID,ReactionString,DecayType,ParmCount,Parameters',
					 'rlib_data.ReactionID <= 88',
					 'ORDER BY rlib_data.ReactionID');
	}

	close BSO;
	close RAH;
	close SWH;
#	$report .= "\nCAUTION! Rates with more than 21 parameters will not be used.\nPreparation complete.\n";
	$report =~ s/\n/\x8/g;  # replace linefeeds with ASCII(8)

        print "REPORT=", $report, "\n",
              "SUMMARY=Successfully prepard for BBN simulator.\n",
              "MIN_ISOTOPE=0,1\n",
              "MAX_ISOTOPE=8,16\n";
    }

    sub agp_BBN_SIM_SETUP_cbk {
	my ($in, $d, $path, $status) = @_;

	if ($status eq 'b') {}
	elsif ($status eq 'e') {}
	elsif ($d->[3] <= 35) {    # Restrict it to 35 parameters for bbn code
	    printf(SWH '%4i%4i %s'."\n", $d->[0], 1, $d->[1]);
	    my $sets = int($d->[3] / 7);
	    my @parms = split(/,/, $d->[4]);

	    printf(RAH '%4i%4i %s'."\n", $d->[0], $sets, $d->[1]);
	    for (1..$sets) {
		printf(RAH '%13.6E%13.6E%13.6E%13.6E'."\n".'%13.6E%13.6E%13.6E'."\n", @parms[(($_-1)*7)..($_*7-1)]);
	    }
	}
    }

# Action: RUN BBN SIM
# Input: HEADER ID ACTION USER PW BBN_SIM_COMMAND
# Output: RUN
    sub agp_RUN_BBN_SIM {
        my $in = shift;		# input hash
	my $dir = ag_man_get_tmp_dir($in, 'bbn_sim');
	my ($n, $v, $sim_options);

	# Read bbnsim_path and get those options in a hash
	read_first_line($in, $dir . '/options', \$n);

	$sim_options = get_db_hash("SELECT TIME_STEP_CONSTANT1,TIME_STEP_CONSTANT2,INITIAL_TIMESTEP,INITIAL_TEMPERATURE,FINAL_TEMPERATURE,SMALLEST_ABUND_ALLOWED,ACCUMULATION_INCREMENT,GRAVITATIONAL_CONSTANT,NEUTRON_LIFETIME,NUMBER_NEUTRINO_SPECIES,ETA,COSMOLOGICAL_CONSTANT,XI_ELECTRON,XI_MUON,XI_TAUON FROM bbn_sim_types_info WHERE Path='$n'",
				    'reading BBN simulation options', '');
	# Parse BBN_SIM_COMMAND into 4 groups
	my ($mc_trials, $rate_uncer_path, $with, $vary) = ('','/PUBLIC/test','','');
	if (not $in->{BBN_SIM_COMMAND} =~ m<^run bbn simulator( Monte Carlo with (\d+) trials and uncertainties "([\w \.\+-/]+)")?(( with \w+ as [-\+]?\d*\.?\d+([Ee][-\+]?\d+)?)*)(( and vary \w+( as [-\+]?\d*\.?\d+([Ee][-\+]?\d+)?)+)*)$>)
	    {report_error('BUG','need to update syntax for BBN_SIM_COMMAND in agp_RUN_BBN_SIM')};

	$mc_trials = $2 if (defined $2);
	$rate_uncer_path = get_private_path($3,$in) if (defined $3);
	$with = $4 if (defined $4);
	$vary = $7 if (defined $7);

	report_error('BAD_VALUE',"Number of Monte Carlo trials (" . $mc_trials . ") must be at least ten.") if ($mc_trials and $mc_trials < 10);

	open BSO, "> $dir/run"
	    or report_error('UNEXPECTED',"Error opening run file: $!");
	print BSO "mc_trials=$mc_trials\nrate_uncer_path=$rate_uncer_path\nwith=$with\nvary=$vary\n";
	close BSO;

	# Update options with quantities in $with
	for $n (split(/ with /, $with)) {
	    if ($n =~ m/^(\w+) as (.*)$/) {
		report_error('UNKNOWN_QUANTITY',$1) if (not exists($sim_options->{$1}));
		$sim_options->{$1} = $2;
	    }
	}

	# Make uncer.dat
	$n = ag_man_get_data('rate_uncer', $in,
			     \&agp_RUN_BBN_SIM_rate_uncer_cbk,
			     $rate_uncer_path,
			     'Uncertainty,ReactionID','','ORDER BY ReactionID');
	report_error('INVALID_PATH',$rate_uncer_path,
		     "Expected 12 rows writing uncer.dat") if ($n != 12);

	# Make options array
	my @options;
	push(@options, '--mc_trials=' . $mc_trials) if ($mc_trials);
	push(@options, '--vary=' . $vary) if ($vary);
	while (($n, $v) = each %{$sim_options}) {push(@options, '--' . $n . '=' . $v)}

	ag_man_bg_run($in, 'bbn_sim', '../../../bin/run_bbnsim', \@options);
        print "RUN=SUCCESS\n";
    }

   sub agp_RUN_BBN_SIM_rate_uncer_cbk {
	if    ($_[3] eq 'b') {
	    open RUH, '> ' . ag_man_get_tmp_dir($_[0], 'bbn_sim') . '/uncer.dat'
		or report_error('UNEXPECTED',"Error opening uncer.dat file: $!");
	}
	elsif ($_[3] eq 'e') {close RUH}
	else  {printf(RUH "%13.6e %s\n", @{$_[1]}[0], @{$_[1]}[1]);}
    }

# Action: BBN SIM UPDATE
# Input: HEADER ID ACTION USER PW
# Output: TEXT_SKIPPED SIMULATION TEXT TOTAL_RUNS CURRENT_RUN
#   Returns the latest output from bbn simulator.  It only sends output that has not been sent before and returns as many lines of text that will fit within 1000 characters.  Newlines will be replaced with ASCII(8) so that the TEXT property will fit on one line
    sub agp_BBN_SIM_UPDATE {
        my $in = shift;		# input hash
	my $ref = ag_man_bg_get_update($in, 'bbn_sim');

        print 
	    "SIMULATION=@{$ref}[0]\n",
	    "CURRENT_RUN=@{$ref}[2]\n",
	    "TOTAL_RUNS=@{$ref}[1]\n",
	    "TEXT_SKIPPED=@{$ref}[3]\n",
	    "TEXT=@{$ref}[4]\n";
    }

# Action: ABORT BBN SIM
# Input: HEADER ID ACTION USER PW
# Output: STOP
    sub agp_ABORT_BBN_SIM {
        my $in = shift;		# input hash

	ag_man_bg_abort($in, 'bbn_sim');
        print "STOP=SUCCESS\n";
    }

# Action: SAVE BBN SIM
# Input: HEADER ID ACTION USER PW PATH OVERWRITE NOTES
# Output: REPORT
    sub agp_SAVE_BBN_SIM {
        my $in = shift;		# input hash
	my $in_path = get_private_path($in->{PATH},$in);
	my $report = '';

	ag_man_bg_abort($in, 'bbn_sim');
	$report = 'Erased simulation in ' . $in->{PATH} . "\n"
	    if (ag_man_erase_col('bbn_vis', $in, $in_path, 'check_overwrite'));
	$report .= ag_man_add_data('bbn_vis',$in,
				  \&agp_SAVE_BBN_SIM_cbk,
				  \&agp_SAVE_BBN_SIM_info_cbk,$in_path,
				  'TIME_STEP_CONSTANT1,TIME_STEP_CONSTANT2,INITIAL_TIMESTEP,INITIAL_TEMPERATURE,FINAL_TEMPERATURE,SMALLEST_ABUND_ALLOWED,ACCUMULATION_INCREMENT,GRAVITATIONAL_CONSTANT,NEUTRON_LIFETIME,NUMBER_NEUTRINO_SPECIES,ETA,COSMOLOGICAL_CONSTANT,XI_ELECTRON,XI_MUON,XI_TAUON,Constraints,eta_,D_H,3He_H,4He,7Li_H,_min,_mid,_max,MonteCarloTrials',
				  'Info,Recipe,LibPath,RateUncerPath,MonteCarloList,MonteCarloTrials,LoopingList,Changes');

	$report =~ s/\n/\x8/g;
        print "REPORT=" . $report . "\n";
    }

    sub agp_SAVE_BBN_SIM_info_cbk {
	my (@ans);
	my $dir = ag_man_get_tmp_dir($_[0], 'bbn_sim');

	$ans[0] = $ag_db::dbh->quote($_[0]->{NOTES});
	$ans[0] =~ s/^'(.*)'$/$1/;

	open F, $dir . "/options"
	    or report_error('UNEXPECTED',"Error opening options file: $!");

	$ans[1] = "BBN simulation type: " . <F>;
	$ans[2] = <F>;
	close F;

	open F, $dir . "/run";
	$ans[5] = <F>;
	$ans[5] =~ s/^.*=//;
	chomp $ans[5];  # Remove trailing linefeed to numeric comparison to get MonteCarloList
	$ans[3] = <F>;
	$ans[3] =~ s/^.*=//;
	$ans[4] = ($ans[5] and $ans[5] > 0) ? "D/H\t3He/H\t4He\t7Li/H" : '';
	$ans[7] = <F>;
	$ans[7] =~ s/^.*=//;
	$ans[6] = <F>;
	$ans[6] =~ s/^.*=( and vary )?//;
	my @t = split(' and vary ',$ans[6]);
	for (0 .. $#t) {$t[$_] =~ s/^(\w+).*$/$1/;}
	$ans[6] = join("\t",@t);
	
	chomp(@ans);
	return \@ans;
    }

    sub agp_SAVE_BBN_SIM_cbk {
	if (eof(F)) {
	    close F;
	    return 0;
	}
	my ($in, $path, $d) = @_;
	@{$d} = ();  # erase array
	@{$d}[24] = 0;

	# Ignore run
	my ($n, $_min, $_mid, $_max) = ('','','','');
	$n = <F>; # run numbers
	$n = <F>; # options
	$n =~ m/TIME_STEP_CONSTANT1=([\dEe\+\.-]+)/ and @{$d}[0] = $1;
	$n =~ m/TIME_STEP_CONSTANT2=([\dEe\+\.-]+)/ and @{$d}[1] = $1;
	$n =~ m/INITIAL_TIMESTEP=([\dEe\+\.-]+)/ and @{$d}[2] = $1;
	$n =~ m/INITIAL_TEMPERATURE=([\dEe\+\.-]+)/ and @{$d}[3] = $1;
	$n =~ m/FINAL_TEMPERATURE=([\dEe\+\.-]+)/ and @{$d}[4] = $1;
	$n =~ m/SMALLEST_ABUND_ALLOWED=([\dEe\+\.-]+)/ and @{$d}[5] = $1;
	$n =~ m/ACCUMULATION_INCREMENT=([\dEe\+\.-]+)/ and @{$d}[6] = $1;
	$n =~ m/GRAVITATIONAL_CONSTANT=([\dEe\+\.-]+)/ and @{$d}[7] = $1;
	$n =~ m/NEUTRON_LIFETIME=([\dEe\+\.-]+)/ and @{$d}[8] = $1;
	$n =~ m/NUMBER_NEUTRINO_SPECIES=([\dEe\+\.-]+)/ and @{$d}[9] = $1;
	$n =~ m/ETA=([\dEe\+\.-]+)/ and @{$d}[10] = $1;
	$n =~ m/COSMOLOGICAL_CONSTANT=([\dEe\+\.-]+)/ and @{$d}[11] = $1;
	$n =~ m/XI_ELECTRON=([\dEe\+\.-]+)/ and @{$d}[12] = $1;
	$n =~ m/XI_MUON=([\dEe\+\.-]+)/ and @{$d}[13] = $1;
	$n =~ m/XI_TAUON=([\dEe\+\.-]+)/ and @{$d}[14] = $1;
	$n =~ m/mc_trials=([\dEe\+\.-]+)/ and @{$d}[24] = $1;

	$n = <F>; # output
	$n =~ m/eta=([\dEe\+\.-]+)/ and @{$d}[16] = $1;
	$n =~ m/h2=\(([\dEe\+\.-]+),([\dEe\+\.-]+)\)/;
	if ($2 eq '9.999999E-15') {@{$d}[17] = $1}
	else {
	    $_min .= 'D_H_min=' . ($1 - $2) . "\t";
	    $_mid .= 'D_H_mid=' . $1 . "\t";
	    $_max .= 'D_H_max=' . ($1 + $2) . "\t";
	}

	$n =~ m/he3=\(([\dEe\+\.-]+),([\dEe\+\.-]+)\)/;
	if ($2 eq '9.999999E-15') {@{$d}[18] = $1}
	else {
	    $_min .= '3He_H_min=' . ($1 - $2) . "\t";
	    $_mid .= '3He_H_mid=' . $1 . "\t";
	    $_max .= '3He_H_max=' . ($1 + $2) . "\t";
	}

	$n =~ m/he4=\(([\dEe\+\.-]+),([\dEe\+\.-]+)\)/;
	if ($2 eq '9.999999E-15') {@{$d}[19] = $1}
	else {
	    $_min .= '4He_min=' . ($1 - $2) . "\t";
	    $_mid .= '4He_mid=' . $1 . "\t";
	    $_max .= '4He_max=' . ($1 + $2) . "\t";
	}

	$n =~ m/li7=\(([\dEe\+\.-]+),([\dEe\+\.-]+)\)/;
	if ($2 eq '9.999999E-15') {@{$d}[20] = $1}
	else {
	    $_min .= '7Li_H_min=' . ($1 - $2) . "\t";
	    $_mid .= '7Li_H_mid=' . $1 . "\t";
	    $_max .= '7Li_H_max=' . ($1 + $2) . "\t";
	}

	# Remove trailing tab and store in array
	$_min =~ s/\t$//;  @{$d}[21] = $_min;
	$_mid =~ s/,$//;  @{$d}[22] = $_mid;
	$_max =~ s/,$//;  @{$d}[23] = $_max;

	$n = <F>; # constraint
	$n =~ m/^.*:  (.*)$/;  @{$d}[15] = $1;

	return 1;
    }

    1;
