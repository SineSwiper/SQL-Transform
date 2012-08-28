package SQL::Convertor::Parser::Pg::AST;
### FIXME: Split into AST::Lexer and AST::Parser

use base 'Pegex::Receiver';

use sanity qw(sanity -warnings/all/FATAL warnings/all);  # keep the syntax checking for now
use Config;
use Math::BigInt;
use Math::BigFloat;

# configure some basic big number stuff
Math::BigInt  ->config({
   upgrade    => 'Math::BigFloat',
   round_mode => 'common',
});
Math::BigFloat->config({
   round_mode => 'common',
});

my $perl_safe_digits    = int( log(2)/log(10) * $Config{ivsize} * 8 );
my $identifier_max_size = 64;

### FIXME: AST needs --> got_IDENT, got_Op, got_L_XUIFULL, L_XUSFULL, got_L_XDOLQFULL, got_ws
### FIXME: Also need some subs for the numbers

sub got_IDENT {
   my ($self, $match) = @_;

   # Unlike PostgreSQL, we can do a proper Unicode lowercase,
   # so downcase_truncate_identifier turns into a 'lc' + truncation.
   return [ lc substr($match->[0], 0, $identifier_max_size) ];
}

sub got_Op {
   my ($self, $match) = @_;
   my $text   = $match->[0];
   my $nchars = length $text;
   my $pos    = $self->parser->position - $nchars;
   
   #* For SQL compatibility, '+' and '-' cannot be the
   #* last char of a multi-char operator unless the operator
   #* contains chars that are not in SQL operators.
   #* The idea is to lex '=-' as two operators, but not
   #* to forbid operator names like '?-' that could not be
   #* sequences of SQL operators.
   my $non_math = $self->parser->grammar->tree->{L_NON_MATH}->{'.rgx'};
   while ($nchars > 1 && $text[$nchars-1] =~ /[\+\-]/) {
      my $ic;

      for ($ic = $nchars-2; $ic >= 0; $ic--) {
         last if ($text[$ic] =~ $non_math);
      }
      last if ($ic >= 0);  #* found a char that makes it OK
      $nchars--;           #* else remove the +/-, and check again
   }

   if ($nchars < length $text) {
      #* Strip the unwanted chars from the token
      $self->parser->position($pos + $nchars);
      $text = substr($text, 0, $nchars);
      my $char = substr($text, 0, 1);

      #* If what we have left is only one char, and it's
      #* one of the characters matching "self", then
      #* return it as a character token the same way
      #* that the "self" rule would have.
      return ($char, $char)
         if ($nchars == 1 && $char =~ /${self}/);
   }

   #* Complain if operator is too long.  Unlike the case
   #* for identifiers, we make this an error not a notice-
   #* and-truncate, because the odds are we are looking at
   #* a syntactic mistake anyway.
   $p->YYError("Operator too long")
      if ($nchars >= $identifier_max_size);

   #* Convert "!=" operator to "<>" for compatibility
   return ('Op', ($text eq "!=") ? '<>' : $text);
}
}
