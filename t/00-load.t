#!perl -T
use 5.10;
use strict;
use warnings FATAL => 'all';
use Test::More;

plan tests => 1;

BEGIN {
    use_ok( 'SQL::Parser::Neo' ) || print "Bail out!\n";
}

diag( "Testing SQL::Parser::Neo $SQL::Parser::Neo::VERSION, Perl $], $^X" );
