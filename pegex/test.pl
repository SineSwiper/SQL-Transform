use Data::Dump;
use Pegex;
use Pegex::Input;
#use Devel::SimpleTrace;

require "./eyapp2pegex.pm";

my $parser = pegex(
   Pegex::Input->new(file => 'eyapp2pegex.pgx')->open, {
      receiver => Transform::Parser::Input::Eyapp::AST->new,
      wrap     => 0,
   },
);
#$parser->debug(1);

my $input = Pegex::Input->new(file => '../lib/SQL/Translator/Parser/Pg.eyp');
$input->open;
my $tree = $parser->parse($input);

#dd $tree;
#exit;

my $tfout = Transform::Parser::Output::Pegex->new(
   tree      => $tree,
   out_class => 'SQL::Transform::Parser::Pg',
   pgx       => 'Pg.pgx',
   ast       => 'Pg.pm',
);
$tfout->parse;
