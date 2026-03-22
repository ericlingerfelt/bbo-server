#!/usr/bin/perl
# This package contains a module for using reaction strings.  It uses the
# reaction_lookup database table for storing reactions and contains some
# subroutines for using and updating this information.
#
# Subroutines that use arrays of reactants or products are in this format
# $particle[0] is z of 1st particle
# $particle[1] is a of 1st particle
# $particle[2] is z of 2nd particle
# $particle[3] is a of 2nd particle
# ...
# $particle[last] is decay type
#
# The last argument, decay type, is optional but should be present any time
# there is a different reaction with the same reaction string.
# Common values for decay type are: ec, bet+, bet-
#
# Decay type may also be a symbol with a minus or asterisk followed by a digit.
# For example, meta-stable Al26 would have a type of 'Al-6' or 'Al*6'
#
# A z of -1 indicates invalid_particle
# If z is negative, it represents the particle in @negative_symbols
# For example, -2 is an electron, -3 is nu
# If z is negative, a is ignored but still must be present

package ag_reactions;
use warnings;
use strict;
require Exporter;

@ag_reactions::ISA = qw/Exporter/;
@ag_reactions::EXPORT =
    ('zas_to_reactionstring',  # input two zas arrays (reactants and products)
     #'zas_to_reacID',  # Moved to ag_man.pm
     #'zas_add_to_db',  # Moved to ag_man.pm
     'zas_check',
     'zas_reaclib_type',

     'zas_get_type',           # input one zas array (reactants or products)
     'zas_lookup_particle',
     'zas_count',
     'zas_sum',
     'zas_sort',
     'zas_sort_indicies',
     'zas_find_next',
     'zas_in_middle',
     'zas_equal',

     'reactionstring_to_zas',
     #'reacID_to_zas',  # Moved to ag_man.pm

     'za_to_label',

     'label_to_za',

     'opt_value',
     'opt_start');

my @weights = qw/
    1  1.00794  4.002602  6.941  9.012182
    10.811  12.0107  14.0067  15.9994  18.9984032
    20.1797  22.98976928  24.3050  26.9815386  28.0855
    30.973762  32.065  35.453  39.948  39.0983
    40.078  44.955912  47.867  50.9415  51.9961
    54.938045  55.845  58.933195  58.6934  63.546
    65.409  69.723  72.64  74.92160  78.96
    79.904  83.798  85.4678  87.62  88.90585
    91.224  92.90638  95.94  98  101.07
    102.90550  106.42  107.8682  112.411  114.818
    118.710  121.760  127.60  126.90447  131.293
    132.9054519  137.327  138.90547  140.116  140.90765
    144.242  145  150.36  151.964  157.25
    158.92535  162.500  164.93032  167.259  168.93421
    173.04  174.967  178.49  180.94788  183.84
    186.207  190.23  192.217  195.084  196.966569
    200.59  204.3833  207.2  208.98040  209
    210  222  223  226  227
    232.03806  231.03588  238.02891  237  244
    243  247  247  251  252
    257  258  259  262  261
    262  266  264  277  268
    281  272  285  284  289  288  291  294/;

my @symbols = qw/
    n   H   He  Li  Be  B   C   N   O   F
    Ne  Na  Mg  Al  Si  P   S   Cl  Ar  K
    Ca  Sc  Ti  V   Cr  Mn  Fe  Co  Ni  Cu
    Zn  Ga  Ge  As  Se  Br  Kr  Rb  Sr  Y
    Zr  Nb  Mo  Tc  Ru  Rh  Pd  Ag  Cd  In
    Sn  Sb  Te  I   Xe  Cs  Ba  La  Ce  Pr
    Nd  Pm  Sm  Eu  Gd  Tb  Dy  Ho  Er  Tm
    Yb  Lu  Hf  Ta  W   Re  Os  Ir  Pt  Au
    Hg  Tl  Pb  Bi  Po  At  Rn  Fr  Ra  Ac
    Th  Pa  U   Np  Pu  Am  Cm  Bk  Cf  Es
    Fm  Md  No  Lr  Rf  Db  Sg  Bh  Hs  Mt
    Ds  Rg  Uub Uut Uuq Uup Uuh Uus Uuo    /;

# First row MUST BEGIN WITH ? -1 -1
my @special = qw/
    ?   -1  -1
    n   0   1
    p   1   1
    d   1   2
    t   1   3
    a   2   4
    nu  -2  -2
    e   -3  -3
    g   -4  -4
    /;

# Options:
# 'USE SHORT ARROW' -> Reactants and products are separated with '->'
# 'USE LONG ARROW'  -> Reactants and products are separated with '-->' [DEFAULT]
# 'USE ()'          -> Reactants and products are separated with '(,)'
# all options for za_to_label()
# all options for zas_find_next()
sub zas_to_reactionstring {
    my ($r, $p, @opt) = @_;
    my (@rlabels, @plabels, $ans, $i);

    for ($i = 0; $i < scalar(@{$r})-1; $i += 2) {
	push @rlabels, za_to_label([$r->[$i], $r->[$i+1], zas_get_type($r, 'ISOTOPE_'.$i.'.0')], @opt);
    }
    for ($i = 0; $i < scalar(@{$p})-1; $i += 2) {
	push @plabels, za_to_label([$p->[$i], $p->[$i+1], zas_get_type($p, 'ISOTOPE_'.$i.'.0')], @opt);
    }

    if (opt_value('USE SHORT ARROW', @opt)) {
	$ans = join(' + ',@rlabels).' -> '.join(' + ',@plabels);
    }
    elsif (opt_value('USE ()', @opt)) {
	$ans = pop(@rlabels).'('.join(' ',@rlabels).',';
	my $l = pop @plabels;
	$ans .= join(' ',@plabels).')'.$l;
    }
    else {
	$ans = join(' + ',@rlabels).' --> '.join(' + ',@plabels);
    }

    return $ans;
}

# Returns empty string on success or error message
sub zas_check {
    my ($r, $p, @opt) = @_;
    my ($z_sum_r, $a_sum_r) = zas_sum($r, @opt);
    my ($z_sum_p, $a_sum_p) = zas_sum($p, @opt);
    my $rtype = zas_get_type($r, 'DECAY');

    if (not $rtype) {
	unless ($z_sum_r == $z_sum_p and $a_sum_r == $a_sum_p) {
	    return 'Reaction is not balanced';
	}
    }
    elsif ($rtype eq 'bet-') {
	unless ($z_sum_r == $z_sum_p-1 and $a_sum_r == $a_sum_p) {
	    return 'Beta-minus decay is not balanced';
	}
    }
    elsif ($rtype eq 'bet+') {
	unless ($z_sum_r == $z_sum_p+1 and $a_sum_r == $a_sum_p) {
	    return 'Beta-plus decay is not balanced';
	}
    }
    elsif ($rtype eq 'ec') {
	unless ($z_sum_r == $z_sum_p+1 and $a_sum_r == $a_sum_p) {
	    return 'Electron capture is not balanced';
	}
    }
    else {return 'Unknown reaction type ' . $rtype}

    return '';
}

# Returns 1-8 or zero if not a reaclib type reaction
# Options:
# all options for zas_count()
sub zas_reaclib_type {
    my ($r, $p, @opt) = @_;
    my ($r_cnt, $p_cnt) = (zas_count($r, @opt), zas_count($p, @opt));

    if ($r_cnt == 1) {
	return 1 if ($p_cnt == 1);
	return 2 if ($p_cnt == 2);
	return 3 if ($p_cnt == 3);
    }
    elsif ($r_cnt == 2) {
	return 4 if ($p_cnt == 1);
	return 5 if ($p_cnt == 2);
	return 6 if ($p_cnt == 3);
	return 7 if ($p_cnt == 4);
    }
    elsif ($r_cnt == 3) {
	return 8 if ($p_cnt == 1 or $p_cnt == 2);
    }
    return 0;
}

# Options, one of the following:
# 'DECAY' -> Return decay type if a decay
# 'ISOTOPE_n' -> Return isotope specific type for nth isotope
# 'ALL' -> Return all type info as space separated list
sub zas_get_type {
    my ($p, @opt) = @_;
    return '' if (scalar(@{$p}) % 2 != 1);
    return $p->[-1] if (opt_value('ALL', @opt));
    my @t = split(/ /,$p->[-1]);
    my ($i);

    if (opt_value('DECAY', @opt)) {
	for (@t) {return $_ if ($_ =~ m/ec|bet\+|bet-/)}
    }
    elsif ($i = opt_start('ISOTOPE_', @opt)) {
	my ($z, $a) = ($p->[$i], $p->[$i+1]);
	#print 'z=',$z,' a=',$a,"\n";
	my @za;
	for (@t) {
	    @za = label_to_za($_);
	    #print $_,' ',join(',',@za),"\n";
	    return $_ if ($za[0] > 0 and $za[0] == $z and $za[1] == $a);
	}
    }
    return '';
}

# Return za array of lookup particle
# Usage: ($z, $a) = zas_lookup_particle($p);
sub zas_lookup_particle {
    my ($p, @opt) = @_;
    my ($i, $l);
    undef $l;
    $i = zas_find_next($p, $l, 'SMALLEST'); # Find heaviest particle
    return (-1, -1) unless ($i or $i == 0);
    return ($p->[$i], $p->[$i+1]);
}

# Returns number of particles in za
# Options:
# 'IGNORE_NEG_Z' -> Ignore special symbols with a negative z (g, e, nu, ...)
sub zas_count {
    my ($p, @opt) = @_;
    
    if (opt_value('IGNORE_NEG_Z', @opt)) {
	my ($i, $s) = (0, 0);
	for (; $i < scalar(@{$p})-1; $i += 2) {
	    $s++ if ($p->[$i] >= 0);
	}
	return $s;
    }

    return int(scalar(@{$p})/2);
}

sub zas_sum {
    my ($p, @opt) = @_;
    my ($z_sum, $a_sum) = (0,0);

    for (my $t = 0; $t < scalar(@{$p})-1; $t += 2) {
	$z_sum += $p->[$t] if ($p->[$t] > 0);
	$a_sum += $p->[$t+1] if ($p->[$t+1] > 0);
    }
    return ($z_sum, $a_sum);
}

# Returns sorted array instead of returning indicies
# Options:
# all options for zas_sort_indicies()
sub zas_sort {
    my ($p, @opt) = @_;
    my @ans;
    my $typeinfo = zas_get_type($p, 'ALL');

    for (zas_sort_indicies($p, @opt)) {
	push @ans, $p->[$_];
	push @ans, $p->[$_+1];
    }
    push(@ans, $typeinfo) if ($typeinfo);
    return @ans;
}

# Options:
# all options for zas_find_next()
sub zas_sort_indicies {
    my ($p, @opt) = @_;
    my (@ans, $i);

    do {
	$i = zas_find_next($p, $i, @opt);
	push @ans, $i if (defined($i));
    } while (defined($i));
    return @ans;
}

# Options:
# 'BIGGEST'  -> Return index to lightest particle heavier than $l [DEFAULT]
# 'SMALLEST' -> Return index to heaviest particle lighter than $l
# $l may be undefined
# Return value is undefined if none match
sub zas_find_next {
    my ($p, $l, @opt) = @_;
    my ($i, $ans);

    if (opt_value('SMALLEST', @opt)) {
	for ($i = 0; $i < scalar(@{$p})-1; $i += 2) {
	    $ans = $i if (zas_in_middle($p, $ans, $i, $l)
			  or (zas_equal($p, $l, $i) and $i > $l));
	}
    }
    else {
	for ($i = 0; $i < scalar(@{$p})-1; $i += 2) {
	    $ans = $i if (zas_in_middle($p, $l, $i, $ans));
	    return $i if (zas_equal($p, $l, $i) and $i > $l);
	}
    }
    return $ans;
}

# $a, $b, $c are indicies to particles in $p
# Returns true if $a < $b < $c, sorting first by Z then A
# If a variable is undefined, assume condition is met
# Return false if $b is undefined or $a, $b, or $c is invalid
sub zas_in_middle {
    my ($p, $a, $b, $c) = @_;

    return 0 unless (defined($b) and $b >= 0 and $b < scalar(@{$p}));
    if (defined($a)) {
	if ($a < 0 or $a > scalar(@{$p}) or $p->[$a] > $p->[$b] or 
	    ($p->[$a] == $p->[$b] and $p->[$a+1] >= $p->[$b+1])) {return 0}
    }
    if (defined($c)) {
	if ($c < 0 or $c > scalar(@{$p}) or $p->[$c] < $p->[$b] or 
	    ($p->[$c] == $p->[$b] and $p->[$c+1] <= $p->[$b+1])) {return 0}
    }
    return 1;
}

# $a and $b are indicies to particles in $p
# Return true if $a particle is the same as $b particle, 0 otherwise
sub zas_equal {
    my ($p, $a, $b) = @_;

    return 0 unless (defined($a) and defined($b)
		     and $a >= 0 and $b >= 0
		     and $a < scalar(@{$p}) and $b < scalar(@{$p}));
    return 1 if ($p->[$a] == $p->[$b] and $p->[$a+1] == $p->[$b+1]);
    return 0;
}

# Returns empty string on success or error message
# Options:
# 'IGNORE_NEG_Z' -> Ignore special symbols with a negative z (g, e, nu, ...)
# 'FORCE_DECAY_' -> Use this decay.  Example: 'FORCE_DECAY_bet+'
sub reactionstring_to_zas {
    my ($r, $p, $str, @opt) = @_;
    my ($rt, $pt) = ('', '');
    my @a;
    @{$r} = ();
    @{$p} = ();

    if ($str =~ m/^.*\(.*,.*\).*$/) { # 8Be(n,g)9Be
	# Convert into space separated list
	$str =~ s/\(|\)/ /g;
	$str =~ s/[[:space:]]+/+/g;
	$str =~ s/\+?,\+?/->/g;            # Convert into arrow type
	#print "new str=$str\n";
    }

    if ($str =~ m/^(.*?)-+>(.*)$/) { # with arrow: 8Be + n --> 9Be
	for (split(/[\+ \t]+/, $1)) {
	    if (not $_) {}
	    else {
		@a = label_to_za($_, @opt);
		return "Unable to intrepret '" . $_ . "'" if ($a[0] == -1);
		unless (opt_value('IGNORE_NEG_Z', @opt) and $a[0] < 0) {
		    push(@{$r}, $a[0], $a[1]);
		    $rt .= $a[2] . ' ' if ($a[2]);
		}
	    }
	}
	for (split(/[\+ \t]+/, $2)) {
	    if (not $_) {}
	    else {
		@a = label_to_za($_, @opt);
		return "Unable to intrepret '" . $_ . "'" if ($a[0] == -1);
		unless (opt_value('IGNORE_NEG_Z', @opt) and $a[0] < 0) {
		    push(@{$p}, $a[0], $a[1]);
		    $pt .= $a[2] . ' ' if ($a[2]);
		}
	    }
	}
    }
    else {
	return "Unable to match reaction string: " . $str;
    }

    my ($z_sum_r, $a_sum_r) = zas_sum($r, @opt);
    my ($z_sum_p, $a_sum_p) = zas_sum($p, @opt);
    my $force_decay = opt_start('FORCE_DECAY_', @opt);
    if ($force_decay) {$rt .= $force_decay.' '; $pt .= $force_decay.' '}
    elsif (opt_value('FORCE_DECAY_', @opt)) {} # Force no decay
    elsif ($a_sum_r != $a_sum_p) {} # Unknown reaction
    elsif ($z_sum_r == $z_sum_p) {} # Normal reaction
    elsif ($z_sum_r == $z_sum_p-1) {$rt .= 'bet- '; $pt .= 'bet- '}
    elsif ($z_sum_r == $z_sum_p+1) {$rt .= 'bet+ '; $pt .= 'bet+ '}

    chop($rt);
    chop($pt);
    push(@{$r}, $rt) if ($rt);
    push(@{$p}, $pt) if ($pt);
    @{$r} = zas_sort($r, @opt);
    @{$p} = zas_sort($p, @opt);
    return '';
}

# Options:
# 'SYMBOL FIRST' -> Return C12 instead of 12C
# 'AVOID SPECIAL' -> Use symbol in @symbols instead of @special if possible
# 'USE ONLY NUMBERS' -> C12 becomes 6,12
sub za_to_label {
    my ($p, @opt) = @_;
    my ($z, $a, $t) = @{$p};
    my ($i, $ans) = ('', '');

    return $z.','.$a if (opt_value('USE ONLY NUMBERS', @opt));

    if ($z < 1 or not opt_value('AVOID SPECIAL', @opt)) {
	for ($i = 0; $i < scalar(@special); $i += 3) {
	    return $special[$i] 
		if ($z == $special[$i+1] and $a == $special[$i+2]);
	}
    }

    return $z.','.$a if ($z < 1 or $z > $#symbols);
    $z = $symbols[$z];

    $/ = ' ';
    if (defined($t)) {
	if ($t =~ m/^$z((\-|\*)\d*)$/g) {$a = $1}
	elsif ($t =~ m/^((\-|\*)\d*)$z$/g) {$a = $1}
    }
    $/ = "\n";
    
    if (opt_value('SYMBOL FIRST', @opt)) {return $z . $a}
    else {return $a . $z}
}

sub label_to_za {
    my ($label, @opt) = @_;
    my ($i, $z, $a) = (-1, -1, -1);

    for ($i = 0; $i < scalar(@special); $i += 3) {
	return ($special[$i+1], $special[$i+2]) if ($label eq $special[$i]);
    }

    return ($1, $2) if ($label =~ m/^(\d+),(\d+)$/);

    return (-1, -1) unless($label =~ m/^(.*?)([A-Za-z]+)(.*?)$/);
    #print $1,', ',$2,', ',$3,"\n";
    return (-1, -1) if ($1 and $3);
    return (-1, -1) unless ($1 or $3);
    $a = $1 if ($1);
    $a = $3 if ($3);

    for ($i = 1; $z == -1 and $i < scalar(@symbols); $i++) {
	$z = $i if ($2 eq $symbols[$i]);
    }
    return (-1, -1) if ($z == -1);

    if ($a =~ m/^\d+$/ and $a >= $z) {   # only digits
	return ($z, $a);
    }
    elsif ($a =~ m/^(\-|\*)(\d*)$/) { # - or * with digits
	return ($z, substr($weights[$z],0,1) . $2, $symbols[$z].$a);
    }

    return (-1, -1);
}

sub opt_value {
    my ($n, @o) = @_;
    for (@o) {return $_ if ($_ eq $n)}
    return '';
}

sub opt_start {
    my ($n, @o) = @_;
    for (@o) {return $1 if ($_ =~ m/^$n(.*)$/)}
    return '';
}

1;
