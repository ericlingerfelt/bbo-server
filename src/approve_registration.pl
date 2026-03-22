#!/usr/bin/perl

use warnings;
use strict;
use ag_error;
use ag_db;
use ag_man;
use agm_Administration;
use DBI;
use Data::Dumper;

$ag_db::dbh = DBI->connect("dbi:mysql:$ag_db::username",
                           $ag_db::username, $ag_db::pw,
                           { PrintError => 0, AutoCommit => 0 })
    or report_error('BUG','DBI->connect failed',$DBI::errstr);

my $in = {};
$in->{Changes} = [];  # holds changes to $in

# Print pending registrations
ag_man_get_col_list('registrations', $in, \&do_nothing, \&do_nothing,
		    \&print_pending, ['/ANY/']);

print "Type in username to approve: ";
my $username = <STDIN>;
chomp($username);
$in->{USER} = $username;

# Load registration info into $in
ag_man_get_col_info('registrations', $in, \&load_pending, ['/ANY/'.$username],
		    'Password,Email,FirstName,LastName,Institution,Address,Country,ResearchDesc,PwHint,Info,WhereHeardOfSuite');

default_account_info($in, $username);
pick_field($in, 'Username');
pick_field($in, 'Alias');
pick_field($in, 'Country');
pick_field($in, 'Institution');
#print Dumper($in),"\n\n";
approve_registration($in, $username, @{$in->{Changes}});
print "Account created\n";

sub do_nothing {}

sub print_pending {
    if    ($_[2] eq 'b') {print "Usernames of pending registrations:\n"}
    elsif ($_[2] eq 'e') {print "\n"}
    else  {
	print "\t", $_[1], "\n";
    }
}

sub pick_field {
    my ($in, $f) = @_;
    my ($sth, $new_f, $rowref);
    print "\n", $f, " list:\n";

    while (1) {
	$sth = $ag_db::dbh->prepare("SELECT DISTINCT $f FROM users_info ORDER BY $f") or last;
	$sth->execute() or last;
	while ($rowref = $sth->fetchrow_arrayref()) {print "\t",$rowref->[0],"\n"}
	last;
    }

    print "\nUser supplied '",$in->{$f},"'\nType in ",$f," from list: ";
    $new_f = <STDIN>;
    chomp($new_f);
    if ($new_f) {
	$in->{$f} = $new_f;
	push(@{$in->{Changes}}, $f, $new_f);
    }
}
