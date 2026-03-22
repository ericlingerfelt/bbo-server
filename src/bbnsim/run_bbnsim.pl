#!/usr/bin/perl

use warnings;
use strict;
use Getopt::Long;

our $child_pid = '';

sub sig_term {
    my $signal = shift;
    local $SIG{$signal} = 'IGNORE';
    our $child_pid;
    kill('SIGKILL', $child_pid) if $child_pid;
    exit 1;
}
$SIG{TERM} = \&sig_term;

my %options;

GetOptions( \%options,
	    'mc_trials=i',
	    'vary=s',
	    'TIME_STEP_CONSTANT1=f',
	    'TIME_STEP_CONSTANT2=f',
	    'INITIAL_TIMESTEP=f',
	    'INITIAL_TEMPERATURE=f',
	    'FINAL_TEMPERATURE=f',
	    'SMALLEST_ABUND_ALLOWED=f',
	    'ACCUMULATION_INCREMENT=f',
	    'GRAVITATIONAL_CONSTANT=f',
	    'NEUTRON_LIFETIME=f',
	    'NUMBER_NEUTRINO_SPECIES=f',
	    'ETA=f',
	    'COSMOLOGICAL_CONSTANT=f',
	    'XI_ELECTRON=f',
	    'XI_MUON=f',
	    'XI_TAUON=f');

# Make empty files so fortran code won't crash
open FH, '> bbn_output.txt' or die $!;
close FH;
open FH, '> bbn_outputa.txt' or die $!;
close FH;

my $vary = parse_vary($options{vary});
loop(\%options, $vary);

sub parse_vary {
    my ($string) = @_;
    my (@vary, $line);

    return \@vary if (not defined $string);

    for $line (split(/ and vary /, $string)) {
	if ($line =~ m/^(\w+) as (.*)$/) {
	    push(@vary, [$1, split(' as ', $2)]);
	}
    }
    return \@vary;
}

sub loop {
    use Data::Dumper;
    my ($o_ref, $v_ref) = @_;
    my (%options, @totals);
    my ($array, $name, $value);

    $| = 1;  # Always flush STDIO buffer to disk after print
    open BO, '>> run' or die $!;
    select BO;
    $| = 1;  # Always flush BO buffer to disk after print
    select STDOUT;

    # Fill @totals with the number of runs for each quantity
    my $total_runs = 1;
    for $array (@{$v_ref}) {
	push @totals, scalar(@{$array}) - 1;
	$total_runs *= $totals[-1];
    }

    print "Running bbn simulator for $total_runs run(s)\n\n";

    my ($run, @i, $constraint, $j);
    # @i is indicies to values to use in simulation
    for $run (1..$total_runs) {
	run_to_indicies($run, \@totals, \@i);
	%options = %{$o_ref};
	delete $options{vary};

	# Update %options and $constraint
	$constraint = '';
	for $j (0 .. scalar(@i)-1) {
	    $name = $v_ref->[$j]->[0];
	    $value = $v_ref->[$j]->[$i[$j]];
	    $constraint .= ' with ' . $name . ' as ' . $value;
	    $options{$name} = $value;
	}

	$name = '';
	for (keys %options) {$name .= ' ' . $_ . '=' . $options{$_}}

	print "starting run $run of $total_runs$constraint\n";
	print BO 'run: ',$run,' ',$total_runs,"\noptions: ",$name,"\n";
	run_bbnsim(\%options);
	print BO "output: ", read_bbn_output(\$value),"\n";
	$constraint =~ s/ETA as [\w\.\+-]+/eta as $value/;
	print BO 'constraint: ',$constraint,"\n";
    }
    close BO;
    print "Simulation(s) Complete!\n";
    $| = 0;  # Don't autoflush STDIO buffer to disk after print
}

sub run_to_indicies {
    my ($run, $total_ref, $i_ref) = @_;

    $run--;
    for (0 .. scalar(@{$total_ref})-1) {
	$i_ref->[$_] = ($run % $total_ref->[$_]) + 1;
	$run /= $total_ref->[$_];
    }
}

sub read_bbn_output {
    my $eta = $_[0];
    open BOUT, 'bbn_outputa.txt' or die $!;
    my @file = <BOUT>;
    close BOUT;

    for (0..3) {$file[$_] =~ s/^.*\s([\w\.\+-]+)\s+([\w\.\+-]+)\n$/($1,$2)/}
    $file[4] =~ s/^\s*(\d+)\s+([\w\.\+-]+)\s+([\w\.\+-]+)\n$/mc_runs=$1 neu_families=$2 eta=$3/;
    ${$eta} = $3;

    return "h2=$file[0] he3=$file[1] he4=$file[2] li7=$file[3] $file[4]";
}

sub run_bbnsim {
    my $o_ref = $_[0];
    our $child_pid;
    my ($mc, $mc_trials) = (0, 1);
    
    if (exists($o_ref->{mc_trials})) {
	$mc = 1;
	$mc_trials = $o_ref->{mc_trials};
    }

    # Make control file
    open FH, '> bbn_input.txt' or die $!;
    printf(FH "%6d\n%6d\n%13.6e\n%13.6e\n%13.6e\n%13.6e\n%13.6e\n%13.6e\n%13.6e\n%13.6e\n%13.6e\n%13.6e\n%13.6e\n%13.6e\n%13.6e\n%13.6e\n%13.6e\n",
	   $mc, $mc_trials,
	   $o_ref->{ETA},
	   $o_ref->{GRAVITATIONAL_CONSTANT},
	   $o_ref->{NEUTRON_LIFETIME},
	   $o_ref->{NUMBER_NEUTRINO_SPECIES},
	   $o_ref->{XI_ELECTRON},
	   $o_ref->{XI_MUON},
	   $o_ref->{XI_TAUON},
	   $o_ref->{COSMOLOGICAL_CONSTANT},
	   $o_ref->{TIME_STEP_CONSTANT1},
	   $o_ref->{TIME_STEP_CONSTANT2},
	   $o_ref->{INITIAL_TEMPERATURE},
	   $o_ref->{FINAL_TEMPERATURE},
	   $o_ref->{SMALLEST_ABUND_ALLOWED},
	   $o_ref->{ACCUMULATION_INCREMENT},
	   $o_ref->{INITIAL_TIMESTEP},
	   );
    close FH;

    #sleep 1; return;
    $| = 1;

    if ($child_pid = fork()) {
	# Parent running
	wait();  # Note only one child at a time
    }
    else {
	die "Can't run bbnsim\n" unless defined $child_pid;
	# Child running
	open STDIN, '/dev/null' or die "Can't read /dev/null: $!";
	exec '../../../bin/bbnsim';
	die "Can't run bbnsim!!!!!\n";
    }

    return $?;
}
