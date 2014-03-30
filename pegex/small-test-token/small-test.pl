use Pegex::Parser;
use Pegex::Grammar;
use Pegex::Compiler;
use Pegex::Input;

use Devel::SimpleTrace;
use Time::HiRes 'time';
use Data::Dump;

require "AST.pm";
require "ParserToken.pm";
require "../Parser.pm";

my $start_time = time;
my $grtree_lexer  = Pegex::Compiler->new->compile(
   Pegex::Input->new(file => 'Pg-mini-lexer.pgx')->open->read
)->tree;
my $grtree_parser = Pegex::Compiler->new->compile(
   Pegex::Input->new(file => 'Pg-mini-parser.pgx')->open->read
)->tree;
printf "[%.3f] Compiled\n", time-$start_time;

my $lexer = SQL::Transform::Parser::Pg::Pegex::Parser->new(
   grammar  => Pegex::Grammar->new(tree => $grtree_lexer),
   receiver => SQL::Transform::Parser::Pg::AST->new,
   wrap     => 0,
   wrap_detail => 1,
   throw_on_error => 1,
);
#my $input = Pegex::Input->new(file => 'test.sql');
#$input->open;
#printf "[%.3f] Opened\n", time-$start_time;
#my $tree = $lexer->parse($input);
#printf "[%.3f] Parsed\n", time-$start_time;
#exit;

my $parser = SQL::Transform::Parser::Pg::Pegex::ParserToken->new(
   grammar  => Pegex::Grammar->new(tree => $grtree_parser),
   receiver => Pegex::Receiver->new,
   lexer    => $lexer,
   wrap     => 0,
   throw_on_error => 1,
);
#$parser->debug(1);
printf "[%.3f] Parser Gen\n", time-$start_time;

my $input = Pegex::Input->new(file => 'test.sql');
$input->open;
printf "[%.3f] Opened\n", time-$start_time;
my $tree = $parser->parse($input);
printf "[%.3f] Parsed\n", time-$start_time;

#dd $tree;
exit;
