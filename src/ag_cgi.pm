#!/usr/bin/perl
    # This package contains a module for parsing CGI data into data structures
    #
    # THIS FILE HAS BEEN AUTOMATICALLY GENERATED from an XML cgi specification
    # and should not be modified.  Instead update autogen.pm to produce the
    # desired cgi module
    #
    # AUTO GENERATION OCCURRED ON Wed Dec  6 12:04:08 2006

    package ag_cgi;
    use warnings;
    use strict;
    use CGI qw(:standard);
    use ag_error;
    require Exporter;

    @ag_cgi::ISA = qw/Exporter/;
    @ag_cgi::EXPORT = qw/read_cgi/;

sub read_cgi {
    print header('text/plain'); # tell webserver what script is returning
    my %in_props = (
        'ABORT BBN SIM' => 	['HEADER','ID','ACTION','USER','PW',],
        'BBN RUN EXIST' => 	['HEADER','ID','ACTION','USER','PW','PATHS',],
        'BBN SIM SETUP' => 	['HEADER','ID','ACTION','USER','PW','SIMULATION_TYPE','LIBRARY',],
        'BBN SIM UPDATE' => 	['HEADER','ID','ACTION','USER','PW',],
        'COPY BBN RUN TO SHARED' => 	['HEADER','ID','ACTION','USER','PW','PATH',],
        'COPY CONSTRAINT TO SHARED' => 	['HEADER','ID','ACTION','USER','PW','PATH',],
        'COPY LIBRARY TO SHARED' => 	['HEADER','ID','ACTION','USER','PW','PATH',],
        'COPY OBS TO SHARED' => 	['HEADER','ID','ACTION','USER','PW','PATH',],
        'COPY RATE UNCERTAINTIES TO SHARED' => 	['HEADER','ID','ACTION','USER','PW','PATH',],
        'ERASE BBN RUN' => 	['HEADER','ID','ACTION','USER','PW','PATH',],
        'ERASE CONSTRAINT' => 	['HEADER','ID','ACTION','USER','PW','PATH',],
        'ERASE LIBRARY' => 	['HEADER','ID','ACTION','USER','PW','PATH',],
        'ERASE OBS' => 	['HEADER','ID','ACTION','USER','PW','PATH',],
        'ERASE RATE UNCERTAINTIES' => 	['HEADER','ID','ACTION','USER','PW','PATH',],
        'GET BBN LOOPING DEFAULTS' => 	['HEADER','ID','ACTION','USER','PW','PARAMETERS',],
        'GET BBN RUN DATA' => 	['HEADER','ID','ACTION','USER','PW','GET_BBN_DATA_COMMAND',],
        'GET BBN RUN INFO' => 	['HEADER','ID','ACTION','USER','PW','PATHS',],
        'GET BBN RUN LIST' => 	['HEADER','ID','ACTION','USER','PW','PATHS',],
        'GET BBN SIM TYPES' => 	['HEADER','ID','ACTION','USER','PW',],
        'GET CONSTRAINT DATA' => 	['HEADER','ID','ACTION','USER','PW','PATH',],
        'GET CONSTRAINT INFO' => 	['HEADER','ID','ACTION','USER','PW','PATHS',],
        'GET CONSTRAINT LIST' => 	['HEADER','ID','ACTION','USER','PW','PATHS',],
        'GET ID' => 	['HEADER','ACTION','USER','PW',],
        'GET OBS DATA' => 	['HEADER','ID','ACTION','USER','PW','PATH',],
        'GET OBS INFO' => 	['HEADER','ID','ACTION','USER','PW','PATHS',],
        'GET OBS LIST' => 	['HEADER','ID','ACTION','USER','PW','PATHS',],
        'GET RATE INFO' => 	['HEADER','ID','ACTION','USER','PW','DATA_IDS',],
        'GET RATE LIBRARY INFO' => 	['HEADER','ID','ACTION','USER','PW','PATHS',],
        'GET RATE LIBRARY ISOTOPES' => 	['HEADER','ID','ACTION','USER','PW','PATHS',],
        'GET RATE LIBRARY LIST' => 	['HEADER','ID','ACTION','USER','PW','PATHS',],
        'GET RATE LIST' => 	['HEADER','ID','ACTION','USER','PW','PATH','ISOTOPES','REACTION_TYPES',],
        'GET RATE UNCERTAINTIES' => 	['HEADER','ID','ACTION','USER','PW','PATHS',],
        'GET RATE UNCERTAINTY DATA' => 	['HEADER','ID','ACTION','USER','PW','PATH',],
        'GET TIMEOUT' => 	['HEADER','ID','ACTION','USER','PW',],
        'LOCATE RATES' => 	['HEADER','ID','ACTION','USER','PW','PATHS','REACTION_STRING','DECAY_TYPE',],
        'LOGOUT' => 	['HEADER','ID','ACTION','USER','PW',],
        'MERGE RATE LIBRARIES' => 	['HEADER','ID','ACTION','USER','PW','OVERWRITE','PATH','PATHS','NOTES',],
        'MODIFY RATE' => 	['HEADER','ID','ACTION','USER','PW','OVERWRITE','PATH','REACTION_STRING','DECAY_TYPE','BIBLIO_STRING','RATE_PARMS','R_NR','NOTES',],
        'OBS EXIST' => 	['HEADER','ID','ACTION','USER','PW','PATHS',],
        'REGISTER' => 	['HEADER','ID','ACTION','USER','PW','LAST_NAME','FIRST_NAME','EMAIL','INSTITUTION','ADDRESS','COUNTRY','RESEARCH_TYPE','DESIRED_USERNAME','DESIRED_PASSWORD','PASSWORD_HINT','NOTES','HEAR_OF_SUITE',],
        'RUN BBN SIM' => 	['HEADER','ID','ACTION','USER','PW','BBN_SIM_COMMAND',],
        'RUN CONSTRAINT GENERATOR' => 	['HEADER','ID','ACTION','USER','PW','OBS_PATH','BBN_RUN_PATH',],
        'SAVE BBN SIM' => 	['HEADER','ID','ACTION','USER','PW','PATH','OVERWRITE','NOTES',],
        'SAVE CONSTRAINT' => 	['HEADER','ID','ACTION','USER','PW','PATH','OVERWRITE','NOTES',],
        'SAVE OBS' => 	['HEADER','ID','ACTION','USER','PW','PATH','NOTES','OVERWRITE','OBSERVATIONS',],
        'SAVE RATE UNCERTAINTIES' => 	['HEADER','ID','ACTION','USER','PW','PATH','RATE_UNCERTAINTY_LIST','NOTES','OVERWRITE',],
    );

    my %prop_fmt = (
        'ACCUMULATION_INCREMENT' => 	'[-\+]?\d*\.?\d+([Ee][-\+]?\d+)?',
        'ACTION' => 	'[\w ]+',
        'ADDRESS' => 	'[\w -\.\+\10]+',
        'BBN_RUN_PATH' => 	'[\w \.\+-/]+',
        'BBN_SIM_COMMAND' => 	'run bbn simulator( Monte Carlo with \d+ trials and uncertainties "[\w \.\+-/]+")?( with \w+ as [-\+]?\d*\.?\d+([Ee][-\+]?\d+)?)*( and vary \w+( as [-\+]?\d*\.?\d+([Ee][-\+]?\d+)?)+)*',
        'BIBLIO_STRING' => 	'[\w\+-]{1,4}',
        'COMPLETE' => 	'Y|N',
        'CONSTRAINTS' => 	'(with \w+ as [-\+]?\d*\.?\d+([Ee][-\+]?\d+)?)*',
        'COSMOLOGICAL_CONSTANT' => 	'[-\+]?\d*\.?\d+([Ee][-\+]?\d+)?',
        'COUNTRY' => 	'[A-Z a-z]+',
        'CREATION_DATE' => 	'\d{4}/\d{2}\d{2} \d{2}:\d{2}:\d{2}',
        'CURRENT_RUN' => 	'\d+',
        'DATA' => 	'((,*[-\+]?\d*\.?\d+([Ee][-\+]?\d+)?)+\t?)+',
        'DATA_ID' => 	'-?\d+',
        'DATA_IDS' => 	'[\d(\t)?]+',
        'DECAY_TYPE' => 	'NONE|ec|bet\+|bet-',
        'DESCRIPTION' => 	'[\w -\.\+\10]*',
        'DESIRED_PASSWORD' => 	'[\40-\176]{6,12}',
        'DESIRED_USERNAME' => 	'[\w -]+',
        'EMAIL' => 	'[\w\.-]+@[\w\.-]+\.[\w-]+',
        'ETA' => 	'[-\+]?\d*\.?\d+([Ee][-\+]?\d+)?',
        'ETA_RANGES' => 	'([-\+]?\d*\.?\d+([Ee][-\+]?\d+)?,[-\+]?\d*\.?\d+([Ee][-\+]?\d+)?(\t)?)*',
        'EXISTS' => 	'Y|N',
        'FINAL_TEMPERATURE' => 	'[-\+]?\d*\.?\d+([Ee][-\+]?\d+)?',
        'FIRST_NAME' => 	'[\w \.-]+',
        'FOLDERS' => 	'[\w \.\+-/\t]*',
        'GET_BBN_DATA_COMMAND' => 	'get bbn data for "[\w \.\+-/]+" return( quantity( final)? [\w/&]+)+( for [\w/&]+ as [-\+]?\d*\.?\d+([Ee][-\+]?\d+)?)*',
        'GRAVITATIONAL_CONSTANT' => 	'[-\+]?\d*\.?\d+([Ee][-\+]?\d+)?',
        'HEADER' => 	'\d+\.\d+',
        'HEAR_OF_SUITE' => 	'[\w -\.\+\10]+',
        'ID' => 	'\w{20}',
        'INCREMENT_LIN' => 	'[-\+]?\d*\.?\d+([Ee][-\+]?\d+)?',
        'INCREMENT_LOG' => 	'[-\+]?\d*\.?\d+([Ee][-\+]?\d+)?',
        'INITIAL_TEMPERATURE' => 	'[-\+]?\d*\.?\d+([Ee][-\+]?\d+)?',
        'INITIAL_TIMESTEP' => 	'[-\+]?\d*\.?\d+([Ee][-\+]?\d+)?',
        'INSTITUTION' => 	'[\w -\.\+\10]+',
        'ISOTOPE' => 	'\d+,\d+',
        'ISOTOPES' => 	'(\d+,\d+(\t)?)+',
        'ISOTOPE_LABEL' => 	'[\w \+-/]+',
        'LAST_NAME' => 	'[\w \.-]+',
        'LIBRARY' => 	'[\w \.\+-/]+',
        'LIBRARY_NOTES' => 	'[[:print:]\b]*',
        'LIBRARY_RECIPE' => 	'[\w -\.\+\10]*',
        'LOGOUT' => 	'SUCCESS',
        'LOOPING_LIST' => 	'[\w \+-/\t]*',
        'LOWER_BOUND' => 	'[-\+]?\d*\.?\d+([Ee][-\+]?\d+)?',
        'MAX' => 	'[-\+]?\d*\.?\d+([Ee][-\+]?\d+)?',
        'MAX_ISOTOPE' => 	'\d+,\d+',
        'MAX_POINTS' => 	'([-\+]?\d*\.?\d+([Ee][-\+]?\d+)?,[-\+]?\d*\.?\d+([Ee][-\+]?\d+)?(\t)?)*',
        'MID' => 	'[-\+]?\d*\.?\d+([Ee][-\+]?\d+)?',
        'MID_POINTS' => 	'([-\+]?\d*\.?\d+([Ee][-\+]?\d+)?,[-\+]?\d*\.?\d+([Ee][-\+]?\d+)?(\t)?)*',
        'MIN' => 	'[-\+]?\d*\.?\d+([Ee][-\+]?\d+)?',
        'MIN_ISOTOPE' => 	'\d+,\d+',
        'MIN_POINTS' => 	'([-\+]?\d*\.?\d+([Ee][-\+]?\d+)?,[-\+]?\d*\.?\d+([Ee][-\+]?\d+)?(\t)?)*',
        'MODIFICATION_DATE' => 	'\d{4}/\d{2}\d{2} \d{2}:\d{2}:\d{2}',
        'MONTE_CARLO_LIST' => 	'[\w \+-/\t]*',
        'MONTE_CARLO_TRIALS' => 	'\d+',
        'NEUTRON_LIFETIME' => 	'[-\+]?\d*\.?\d+([Ee][-\+]?\d+)?',
        'NOTES' => 	'[[:print:]\b]*',
        'NUMBER_NEUTRINO_SPECIES' => 	'[-\+]?\d*\.?\d+([Ee][-\+]?\d+)?',
        'OBSERVATIONS' => 	'([\w \+-/\t]+,[-\+]?\d*\.?\d+([Ee][-\+]?\d+)?,[-\+]?\d*\.?\d+([Ee][-\+]?\d+)?,[-\+]?\d*\.?\d+([Ee][-\+]?\d+)?\t?)+',
        'OBS_PATH' => 	'[\w \.\+-/]+',
        'OVERWRITE' => 	'Y|N',
        'PARAMETER' => 	'[\w \+-/]+',
        'PARAMETERS' => 	'[\w \+-/\t]+',
        'PASSWORD_HINT' => 	'[\w -\.\+\10]+',
        'PATH' => 	'[\w \.\+-/]+',
        'PATHS' => 	'[\w \.\+-/\t]+',
        'PW' => 	'[A-Fa-f0-9]{40}',
        'RATE_PARMS' => 	'([-\+]?\d*\.?\d+([Ee][-\+]?\d+)?(,)?)+',
        'RATE_PARM_COUNT' => 	'7|14|21|28|35|42|49',
        'RATE_UNCERTAINTY_LIST' => 	'([\w >\*\+-]+,[\w\+-]+,[-\+]?\d*\.?\d+([Ee][-\+]?\d+)?(\t)?)+',
        'RATE_UNCERTAINTY_PATH' => 	'[\w \.\+-/]+',
        'REACTION_STRING' => 	'[\w >\*\+-]+',
        'REACTION_TYPE' => 	'[1-8]',
        'REACTION_TYPES' => 	'[1-8\t]*|-\d+',
        'RECIPE' => 	'[\w \.\+-/\t\10]+',
        'REGISTER' => 	'SUCCESS',
        'REPORT' => 	'[\w -\.\+\10]*',
        'RESEARCH_TYPE' => 	'[\w -\.\+\10]+',
        'RUN' => 	'SUCCESS',
        'RUNS' => 	'[\w \.\+-/\t]*',
        'R_NR' => 	'((r|nr),?)+',
        'SIMULATION' => 	'RUNNING|COMPLETE',
        'SIMULATION_TYPE' => 	'[\w -\.\+]+',
        'SMALLEST_ABUND_ALLOWED' => 	'[-\+]?\d*\.?\d+([Ee][-\+]?\d+)?',
        'STOP' => 	'SUCCESS',
        'SUMMARY' => 	'[\w -\.\+]*',
        'TEXT' => 	'[\w -\.\+\10]*',
        'TEXT_SKIPPED' => 	'Y|N',
        'TIMEOUT' => 	'\d+',
        'TIME_STEP_CONSTANT1' => 	'[-\+]?\d*\.?\d+([Ee][-\+]?\d+)?',
        'TIME_STEP_CONSTANT2' => 	'[-\+]?\d*\.?\d+([Ee][-\+]?\d+)?',
        'TOTAL_RUNS' => 	'\d+',
        'UNCERTAINTY' => 	'[-\+]?\d*\.?\d+([Ee][-\+]?\d+)?',
        'UPPER_BOUND' => 	'[-\+]?\d*\.?\d+([Ee][-\+]?\d+)?',
        'USER' => 	'[\w -]+',
        'XI_ELECTRON' => 	'[-\+]?\d*\.?\d+([Ee][-\+]?\d+)?',
        'XI_MUON' => 	'[-\+]?\d*\.?\d+([Ee][-\+]?\d+)?',
        'XI_TAUON' => 	'[-\+]?\d*\.?\d+([Ee][-\+]?\d+)?',
    );

    my %h;  # reference to this hash will be returned
    my $ACTION = param('ACTION');
    my ($prop_name, $value);

    if (not $ACTION) {	
	report_error('BUG', "Missing CGI property: ACTION"); 
    }
    elsif (exists($in_props{$ACTION})) {

	# loop through all required properties for this action
	for $prop_name (@{ $in_props{$ACTION} }) {
	    $value = param($prop_name);
	    report_error('BUG', "Missing value for CGI property: $prop_name") if (not $value);
	    report_error('BAD_CGI_VALUE', $value, 
			 "Invalid CGI property value: $prop_name=$value", 
			 "Valid format is ^($prop_fmt{$prop_name})\$") 
		if (not $value =~ m/^($prop_fmt{$prop_name})$/);
	    
	    $h{$prop_name} = $value;
	}
    }
    else {
	report_error('BUG', "Unknown CGI property: ACTION=$ACTION");
    }
    return \%h;
}

    1;
