#!/usr/bin/perl
    # This package contains a module for reporting errors to the user
    #
    # THIS FILE HAS BEEN AUTOMATICALLY GENERATED from an XML cgi specification
    # and should not be modified.  Instead update autogen.pm to produce the
    # desired cgi module
    #
    # AUTO GENERATION OCCURRED ON Wed Dec  6 12:04:07 2006

    package ag_error;
    use warnings;
    use strict;
    require Exporter;

    @ag_error::ISA = qw/Exporter/;
    @ag_error::EXPORT = qw/report_error/;

# first item is short string with type of error
# all other items are debugging info that will not be sent to user
sub report_error {
    my $e = shift;
    my $m; # message corresponding to what is in $e
    my $d = '  Email coordinator@bigbangonline.org for more information.';
    
    if (not $e) {$e = 'BUG';}

    if ($e eq 'BUG') {
	$m = 'You found a bug in our software. It has been logged for developers to fix.  We apologize and will fix this promptly.';
    }
    elsif ($e eq 'INVALID_USER') {
	$m = "The username or password is incorrect.";
    }
    elsif ($e eq 'INVALID_SESSION') {
	$m = "Your session was idle too long and has been closed.  Please login again.";
    }
    elsif ($e eq 'BAD_CGI_VALUE') {
	my $a = shift;
	$m = "The following value is not in the correct format.  Please remove uncommon characters and try again.  Value=$a";
    }
    elsif ($e eq 'BAD_HEADER') {
	$m = 'This software has been updated.  Please close this application and open it again to receive the updates';
    }
    elsif ($e eq 'UNEXPECTED') {
	$m = 'An unexpected problem occurred on the server and your request could not be completed. It has been logged for developers to fix.  We apologize and will fix this promptly.';
    }
    elsif ($e eq 'INVALID_RLIB') {
	my $a = shift;
	$m = "Rate library '$a' was not found.";
    }
    elsif ($e eq 'INVALID_PATH') {
	my $a = shift;
	$m = "Invalid path '$a'";
    }
    elsif ($e eq 'UNKNOWN_QUANTITY') {
	my $a = shift;
	$m = "Unknown quantity '$a'";
    }
    elsif ($e eq 'NOTHING_TO_ABORT') {
	$m = 'Program is not running and can not be aborted';
    }
    elsif ($e eq 'REACHED_PROC_LIMIT') {
	$m = 'This feature is running more than one program.  Please wait for one of these to complete before continuing.';
    }
    elsif ($e eq 'REACHED_SESSION_LIMIT') {
	$m = 'This suite has more than one session open.  Please close one before logging in again.  If the suite crashed before it was able to logout, wait for idle sessions to be closed (about 4 hours) or send us an email.';
    }
    elsif ($e eq 'PATH_EXISTS') {
	my $a = shift;
	$m = "The path '$a' is in use.  Please select another one and try again.";
    }
    elsif ($e eq 'PERM_DENIED') {
	my $a = shift;
	$m = "The path '$a' may not be used for this operation.  Please select another path and try again.";
    }
    elsif ($e eq 'NO_DATA') {
	my ($path, $other) = (shift, shift);
	$m = "No data was found";
	$m .= " for path '" . $path . "'" if (defined $path and $path);
	$m .= " for " . $other if (defined $other and $other);
    }
    elsif ($e eq 'BAD_VALUE') {
	my $a = shift;
	$m = "Invalid value";
	$m .= ": " . $a if (defined $a and $a);
    }
    elsif ($e eq 'INVALID_DATAIDS') {
	$m = 'Some of the data requested is not available.  This may be because it was erased or overwritten in a different session or there may be a bug in this program.  This message has been sent to developers to check for bugs.';
    }
    else {
	unshift(@_, "Unknown error: $e");
	$m = 'An unexpected problem occurred on the server and your request could not be completed. It has been logged for developers to fix.  We apologize and will fix this promptly.';
    }

    #print "\nERROR=$m\n";
    #print "\nERROR=", join("\t", ($m, $d, @_)), "\n";
    print "\nERROR=", join("\t", ($m, $d)), "\n";

    $ag_db::dbh->disconnect();
    exit 1;
}
