#!/usr/bin/perl
use sanity;
use Devel::SimpleTrace;
use SQL::Translator::Parser::_Empty::Pg;

my $parser = SQL::Translator::Parser::_Empty::Pg->new();
$parser->YYSlurpFile('t/parse.sql');
my $tree = $parser->YYParse();
use Data::Dump;
dd $tree;
