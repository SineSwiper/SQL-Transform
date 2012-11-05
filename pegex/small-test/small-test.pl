use Pegex::Parser;
use Pegex::Grammar;
use Pegex::Compiler;
use Pegex::Input;

use Devel::SimpleTrace;
use Time::HiRes 'time';
use Data::Dump;

require "../AST.pm";
require "../Parser.pm";

my $start_time = time;
my $grtree = Pegex::Compiler->new->compile(
   Pegex::Input->new(file => 'Pg-mini.pgx')->open->read
)->tree;
printf "[%.3f] Compiled\n", time-$start_time;

my $parser = SQL::Transform::Parser::Pg::Pegex::Parser->new(
   grammar  => Pegex::Grammar->new(tree => $grtree),
   receiver => SQL::Transform::Parser::Pg::AST->new,
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
