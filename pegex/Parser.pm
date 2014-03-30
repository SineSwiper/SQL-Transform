package SQL::Transform::Parser::Pg::Pegex::Parser;

use Pegex::Base;
extends 'Pegex::Parser';

use sanity qw(sanity -warnings/all/FATAL warnings/all);  # keep the syntax checking for now

my $identifier_max_size = 64;

sub match_rule_Op {
   my $self  = $_[0];
   my $re_op = $self->{grammar}{tree}{L_Op}{'.rgx'};
   use Data::Dump;
   dd $self->grammar->tree;
  
   $self->{buffer} =~ /$re_op/g or return 0;
   my $text = $1;
   my $slen = length $text;
   my $pos  = $self->position - $slen;
   
   #* For SQL compatibility, '+' and '-' cannot be the
   #* last char of a multi-char operator unless the operator
   #* contains chars that are not in SQL operators.
   #* The idea is to lex '=-' as two operators, but not
   #* to forbid operator names like '?-' that could not be
   #* sequences of SQL operators.
   $text =~ s/[\+\-]+$// unless ($text =~ /[\~\!\@\#\^\&\|\`\?]/);
   my $flen = length $text; 

   if ($slen < $flen) {
      #* Strip the unwanted chars from the token
      $self->position($pos + $flen);

      #* If what we have left is only one char, and it's
      #* one of the characters matching "self", then
      # reject this rule, and let one of the Self rules
      # match.
      return 0 if ($flen == 1 && $text =~ /[,\(\)\[\].;\:\+\-\*\/\%\^\<\>\=]/);
   }
   else {
      # Whitespace processing (only valid if we don't have any +/- in front)
      my $ws = $self->grammar->tree->{ws1}->{'.rgx'};
      $self->{buffer} =~ /$ws/g;
   }

   #* Complain if operator is too long.  Unlike the case
   #* for identifiers, we make this an error not a notice-
   #* and-truncate, because the odds are we are looking at
   #* a syntactic mistake anyway.
   $self->throw_error("Operator too long")
      if ($flen >= $identifier_max_size);

   #* Convert "!=" operator to "<>" for compatibility
   return [ ($text eq "!=") ? '<>' : $text ];
}
