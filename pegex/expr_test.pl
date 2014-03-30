use Pegex::Parser;
use Pegex::Grammar;
use Pegex::Compiler;
use Pegex::Input;

#use Devel::SimpleTrace;
use Data::Dump;

my $grtree = Pegex::Compiler->new->compile(
   Pegex::Input->new(string => '
exprs: expr+ %% /<EOL>+ ~/

expr  : +add | +mult | num
expr_2:        +mult | num

mult: num    /~ <STAR>   ~/ expr
add:  expr_2 /~ <PLUS>   ~/ expr
num:         /~ (<DIGIT>+) ~/

ws: /[^\S\r\n]/
   ')
)->tree;

my $parser = Pegex::Parser->new(
   grammar  => Pegex::Grammar->new(tree => $grtree),
   wrap     => 0,
);
$parser->debug(1);

my $input = Pegex::Input->new(string => 
'2
2 + 4
2 * 4
2 + 4 * 5
2 * 4 + 5
2 * 4 * 5
2 + 4 + 5
');
$input->open;
my $tree = $parser->parse($input);

dd $tree;
