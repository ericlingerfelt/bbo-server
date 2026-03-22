#!/usr/bin/perl
    # This package contains the agm_Administration module for performing CGI requests
    #
    # THIS FILE HAS BEEN AUTOMATICALLY GENERATED from an XML cgi specification
    # and should not be modified.  Instead update autogen.pm to produce the
    # desired cgi module
    #
    # AUTO GENERATION OCCURRED ON Tue Nov  7 10:18:14 2006

    package agm_Administration;
    use warnings;
    use strict;
    use ag_error;
    use ag_man;
    require Exporter;

    @agm_Administration::ISA = qw/Exporter/;
    @agm_Administration::EXPORT = qw/
            agp_GET_ID
            agp_GET_TIMEOUT
            agp_LOGOUT
            agp_REGISTER
            authencate_user
            check_session_id
            approve_registration
            load_pending
            default_account_info/;

# Returns empty string if valid username and password
# Otherwise returns a string with error message
   sub authencate_user {
        my $in = shift;		# input hash
	my $pw = get_db_value("SELECT Password from accounts where Username='$in->{USER}'", 'authencating user', 'allow missing');
	if ($pw and $pw eq $in->{PW}) { return ''; }
	else { return 'Invalid username or password';}
   }

# Returns empty string if valid username and password
# Otherwise returns a string with error message
   sub check_session_id {
        my $in = shift;		# input hash
	my $ip = $ENV{'REMOTE_ADDR'};
	$ip = '' unless($ip);
	$in->{IP} = $ip;
	my $pw = get_db_value("SELECT Password from sessions where Session='$in->{ID}'", 'checking session id', 'allow missing');
	if ($pw and $pw eq $in->{PW}) { 
	    # Update LastRequest to now()
	    $ag_db::dbh->do("UPDATE sessions SET LastRequest=NOW(),IP='" . $ip . "' WHERE Session='$in->{ID}';")
		or report_error('UNEXPECTED',"Failed to update LastRequest when checking session id: $DBI::errstr");
	    return ''; 
	}
	else { return 'Invalid session id';}
   }

# Action: GET ID
# Input: HEADER ACTION USER PW
# Output: ID
#   Get session ID from server
    sub agp_GET_ID {
        my $in = shift;		# input hash
	my $try_limit = 25;     # number of times to find a unique ID
	my $try = 0;
	my $id;

	# seed random number generator
	srand((time() ^ (time() % $])) ^ exp(length($0)) ** $$);

	my $login_limit = get_db_value("SELECT MaxLogins FROM accounts WHERE Username='$in->{USER}'");

	$ag_db::dbh->do('LOCK TABLES sessions WRITE;')
	    or report_error('UNEXPECTED',"Table lock failed: $DBI::errstr");

	$try = get_db_value("SELECT COUNT(*) FROM sessions WHERE Username='$in->{USER}'");
	report_error('REACHED_SESSION_LIMIT',$try,$login_limit)
	    if ($try >= $login_limit);

	while (1) {
	    $id = make_id();
	    last if ($ag_db::dbh->do("INSERT INTO sessions (Session,Username,Password,IP,LastRequest) VALUES ('$id','$in->{USER}','$in->{PW}','',NOW())"));

	    report_error('UNEXPECTED','Failed to find unique ID') 
		if ($try++ >= $try_limit);
	}

	$ag_db::dbh->do('UNLOCK TABLES;')
	    or report_error('UNEXPECTED',"Table unlock failed: $DBI::errstr");

        print "ID=$id\n";
    }

# Returns 20 character string
    sub make_id {
	my ($id, $c, $i);
	$id = '';
	
 	for ($i = 0; $i < 20; $i++) {
	    $c = int(61 * rand());
	    # Get random number with 62 possible values
	    # (26 Uppercase, 26 lowercase, 10 numeral symbols)
	    
	    # Convert integer to ASCII value
	    if ($c < 10) {
		$c += 48; # A number from 0 to 9
	    }
	    elsif ($c < 36) {
		$c += 55; # An uppercase letter
	    }
	    else {
		$c += 61; # A lowercase letter
	    }
	    $id .= chr($c);
	}
	return $id;
    }

# Action: GET TIMEOUT
# Input: HEADER ID ACTION USER PW
# Output: TIMEOUT
#   Users are automatically logged out if a CGI request is not called within a certain timeframe.  This request returns the length of the timeframe in minutes.
    sub agp_GET_TIMEOUT {
        my $in = shift;		# input hash
	my $timeout = get_db_value("SELECT Timeout from accounts where Username='$in->{USER}'", 'getting timeout');
        print "TIMEOUT=$timeout\n";
    }

# Action: LOGOUT
# Input: HEADER ID ACTION USER PW
# Output: LOGOUT
#   Terminates user's session
    sub agp_LOGOUT {
        my $in = shift;		# input hash

#	$ag_db::dbh->do('LOCK TABLES sessions WRITE;')
#	    or report_error('UNEXPECTED',"Table lock failed: $DBI::errstr");

	$ag_db::dbh->do("DELETE FROM sessions WHERE Session='$in->{ID}';")
	    or report_error('UNEXPECTED',"LOGOUT db call: $DBI::errstr");

#	$ag_db::dbh->do('UNLOCK TABLES;')
#	    or report_error('UNEXPECTED',"Table unlock failed: $DBI::errstr");

	# Close all running programs
	bg_really_kill($in, bg_run_list($in,''));

	# Delete all temp files
	ag_man_clean($ag_db::dir . 'tmp/' . $in->{ID});

        print "LOGOUT=SUCCESS\n";
    }

# Action: REGISTER
# Input: HEADER ID ACTION USER PW LAST_NAME FIRST_NAME EMAIL INSTITUTION ADDRESS COUNTRY RESEARCH_TYPE DESIRED_USERNAME DESIRED_PASSWORD PASSWORD_HINT NOTES HEAR_OF_SUITE
# Output: REGISTER
#   Submit a user registration for approval for an account.  The NOTES field allows additional information (supervisor / research mentor) to be submitted
    sub agp_REGISTER {
	use Digest::SHA1  qw(sha1_hex);
	use URI::Escape;
        my $in = shift;		# input hash

	report_error('BAD_VALUE', 'desired username is unavailable.  Please select a different username and try again') 
	    if (check_username($in->{DESIRED_USERNAME}));

	report_error('BAD_VALUE', 'desired password must have at least two digits, two letters, and two symbols.  In addition, the password must never have been used before.') 
	    if (check_password($in->{DESIRED_PASSWORD}));

	# Insert registration into database
	my $message = ag_man_modify_col('registrations', $in, 
					'/ANY/' . $in->{DESIRED_USERNAME},
					'Password,Email,FirstName,LastName,Institution,Address,Country,ResearchDesc,PwHint,Info,WhereHeardOfSuite,Visibility',
					[sha1_hex($in->{DESIRED_PASSWORD}),
					 $in->{EMAIL},
					 $in->{FIRST_NAME},
					 $in->{LAST_NAME},
					 $in->{INSTITUTION},
					 $in->{ADDRESS},
					 $in->{COUNTRY},
					 $in->{RESEARCH_TYPE},
					 $in->{PASSWORD_HINT},
					 $in->{NOTES},
					 $in->{HEAR_OF_SUITE},
					 0]);
	report_error('BUG',"Unable to add new registration to database", $message) if ($message !~ m|^Added /ANY/$in->{DESIRED_USERNAME}$|);

	# Email coordinator
	send_email('bigbangonline.org registration',
		   "Dear coordinator\@bigbangonline.org,\n\n" . 
		   "'" . $in->{FIRST_NAME} . ' ' . $in->{LAST_NAME} . "' just registered for an account at bigbangonline.org\n\n" . 
		   "First name: \t" . $in->{FIRST_NAME} . "\n" .
		   "Last name: \t" . $in->{LAST_NAME} . "\n" .
		   "Desired username: \t" . $in->{DESIRED_USERNAME} . "\n" .
		   "Email address: \t" . $in->{EMAIL} . "\n\n" .
		   "Institution: \t" . $in->{INSTITUTION} . "\n" .
		   "Address: \t" . $in->{ADDRESS} . "\n" .
		   "Country: \t" . $in->{COUNTRY} . "\n\n" .
		   "Research Type: \t" . $in->{RESEARCH_TYPE} . "\n" .
		   "Additional Information: \t" . $in->{NOTES} . "\n" .
		   "Where heard of suite: \t" . $in->{HEAR_OF_SUITE} . "\n",
		   'coordinator@bigbangonline.org',
		   'coordinator@bigbangonline.org');
#		   'jpscott@phy.ornl.gov');

	# Email user of success
	send_email('bigbangonline.org registration',
		   'Dear ' . $in->{FIRST_NAME} . ' ' . $in->{LAST_NAME} . ",\n\n" .
		   "Thank you for registering for a user account for the big bang online infrastructure.  You will receive an email once your account has been activated.  This should happen within the next work day.\n\n" . 
		   "Please email coordinator\@bigbangonline.org if you have any questions, comments, or concerns.\n\n" . 
		   "Thank you,\n\n" . 
		   "coordinator\@bigbangonline.org\n",
		   'coordinator@bigbangonline.org',
		   $in->{FIRST_NAME} . ' ' . $in->{LAST_NAME} . ' <' . $in->{EMAIL} . '>');
	
        print "REGISTER=SUCCESS\n";
    }

# Return zero for good password
    sub check_password {
	my $pw = shift;

	my $i = $pw;
	$i =~ s/[^A-Za-z]//g;
	my $num_letters = length($i);
	
	$i = $pw;
	$i =~ s/\D//g;
	my $num_digits = length($i);
	my $num_symbols = length($pw) - $num_letters - $num_digits;

	return -1 if ($num_letters < 2 or $num_digits < 2 or $num_symbols < 2);
	return -1 if (get_db_value("SELECT COUNT(*) FROM users_info WHERE Password=SHA1('" . $pw . "')") > 0);
	return -1 if (get_db_value("SELECT COUNT(*) FROM registrations_info WHERE Password=SHA1('" . $pw . "')") > 0);
	return 0;
    }

# Return zero for unused username
    sub check_username {
	my $username = shift;

	return -1 if (get_db_value("SELECT COUNT(*) FROM users_info WHERE Path='/ANY/" . $username . "'") > 0);
	return -1 if (get_db_value("SELECT COUNT(*) FROM registrations_info WHERE Path='/ANY/" . $username . "'") > 0);
	return 0;
    }

    sub approve_registration {
	use Data::Dumper;
	my ($in, $username, @changes) = @_;
	my $d = {};
	my ($m);

	ag_man_get_col_info('registrations', $d, \&load_pending, 
			    ['/ANY/'.$username],
			    'Password,Email,FirstName,LastName,Institution,Address,Country,ResearchDesc,PwHint,Info,WhereHeardOfSuite');

	default_account_info($d, $username);
	while (@changes) {
	    $m = shift(@changes);
	    $d->{$m} = shift(@changes);
	}

	$m = ag_man_modify_col('users', $in, 
			       '/ANY/' . $username,
			       'Password,Email,FirstName,LastName,Institution,Address,Country,ResearchDesc,PwHint,Info,WhereHeardOfSuite',
			       [$d->{Password},
				$d->{Email},
				$d->{FirstName},
				$d->{LastName},
				$d->{Institution},
				$d->{Address},
				$d->{Country},
				$d->{ResearchDesc},
				$d->{PwHint},
				$d->{Info},
				$d->{WhereHeardOfSuite}]);
	report_error('BUG',"Unable to add registration to database", $m) if ($m !~ m|^Added /ANY/$username$|);

	$ag_db::dbh->do("INSERT INTO accounts (Username,Alias,Password,Email,CreateDate,PwChangeDate,MaxLogins,Permissions,Status,Timeout) VALUES ('"
			.$d->{Username}."','"
			.$d->{Alias}."','"
			.$d->{Password}."','"
			.$d->{Email}
			."',DATE(NOW()),ADDDATE(DATE(NOW()),INTERVAL 1 YEAR),"
			.$d->{MaxLogins}.","
			.$d->{Permissions}.","
			.$d->{Status}.","
			.$d->{Timeout}.");")
	    or report_error('UNEXPECTED','Unable to add account',$DBI::errstr);

	$ag_db::dbh->do("DELETE FROM registrations_info WHERE Path='/ANY/".$username."'")
	    or report_error('UNEXPECTED','Unable to delete registration',$DBI::errstr);

	# Email user account 
	send_email('bigbangonline.org account',
		   'Dear '.$d->{Alias}.",\n\n"
		   ."Thank you for registering as a user of our Big Bang Online Software Suite.  Your username below and the password you registered with will enable you to save your work on our server, share files with others, upload your own files for processing, and access advanced options in our codes.  \n\n"
		   ."Please do not share your username and password with others.  Doing so puts the work you save on our server at risk of modification or deletion by others. If your colleagues would like access, have them contact us and we will be happy to accommodate them.\n\n"
		   ."Our software suite is in development and is updated frequently. Please report any problems and questions to\n\n"
		   ."coordinator\@bigbangonline.org\n\n"
		   ."We are also very interested in getting user feedback -- including how you are using our suite in your research, and any new features that you would like to see added to our suite to make it more helpful for your research.\n\n"
		   ."Finally, use of this suite constitutes consent to security monitoring and testing. All activity is logged with your host name and IP address.\n\n"
		   ."username: ".$d->{Username}
		   ."\n\nThank you very much,\n\n"
		   ."coordinator\@bigbangonline.org\n",
		   'coordinator@bigbangonline.org',
		   $d->{Alias}.' <'.$d->{Email}.'>');

	# Send stats
	$m = get_db_array('SELECT COUNT(*), COUNT(DISTINCT Institution),COUNT(DISTINCT Country) from users_info', 'getting stats');

	send_email('bigbangonline account stats',
		   $m->[0].' users from '
		   .$m->[1].' different organizations in '
		   .$m->[2]. " countries have accounts with bigbangonline.org\n\nJason\n",
		   'coordinator@bigbangonline.org',
		   'coordinator@bigbangonline.org');
    }

    sub load_pending {
	my ($in, $p, $d) = @_;
	$in->{Password} = $d->[0];
	$in->{Email} = $d->[1];
	$in->{FirstName} = $d->[2];
	$in->{LastName} = $d->[3];
	$in->{Institution} = $d->[4];
	$in->{Address} = $d->[5];
	$in->{Country} = $d->[6];
	$in->{ResearchDesc} = $d->[7];
	$in->{PwHint} = $d->[8];
	$in->{Info} = $d->[9];
	$in->{WhereHeardOfSuite} = $d->[10];
    }

    # Apply users_info data to account data
    sub default_account_info {
	my ($d, $username) = @_;

	$d->{Username} = $username;
	$d->{Alias} = $d->{FirstName} . ' ' . $d->{LastName};
	$d->{MaxLogins} = 8;
	$d->{Permissions} = 0;
	$d->{Status} = 0;
	$d->{Timeout} = 180;
    }

    1;
