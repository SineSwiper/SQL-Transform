use Pegex::Parser;
use Pegex::Grammar;
use Pegex::Compiler;
use Pegex::Input;

use Devel::SimpleTrace;
use Time::HiRes 'time';
use Data::Dump;

require "./eyapp2pegex.pm";

my $start_time = time;
my $grammar = Pegex::Input->new(file => 'eyapp2pegex.pgx');
my $grtree = Pegex::Compiler->new->compile($grammar)->tree;
printf "[%.3f] Compiled\n", time-$start_time;

my $parser = Pegex::Parser->new(
   grammar  => Pegex::Grammar->new(tree => $grtree),
   receiver => 'Transform::Parser::Input::Eyapp::AST',
   wrap     => 0,
);
#$parser->debug(1);
printf "[%.3f] Parser Gen\n", time-$start_time;

my $input = Pegex::Input->new(file => '../lib/SQL/Translator/Parser/Pg.eyp');
$input->open;
printf "[%.3f] Opened\n", time-$start_time;
my $tree = $parser->parse($input);
printf "[%.3f] Parsed\n", time-$start_time;

#dd $tree;
#exit;

my $tfout = Transform::Parser::Output::Pegex->new(
   tree      => $tree,
   out_class => 'SQL::Transform::Parser::Pg',
   pgx       => 'Pg.pgx',
   ast       => 'Pg.pm',
);
$tfout->parse;
printf "[%.3f] Re-parsed\n", time-$start_time;
