#!/usr/bin/perl
use sanity;
use Devel::SimpleTrace;
use SQL::Translator::Parser::Pg;

my $parser = SQL::Translator::Parser::Pg->new();
foreach my $filename (glob 't/sql/*.sql') {
   $parser->YYSlurpFile($filename);
   my $tree = $parser->YYParse();

   use Data::Dump;
   dd $tree;
}
