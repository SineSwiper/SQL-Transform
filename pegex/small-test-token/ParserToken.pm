package SQL::Transform::Parser::Pg::Pegex::ParserToken;

use Pegex::Base;
extends 'Pegex::Parser::Tokenized';

use sanity qw(sanity -warnings/all/FATAL warnings/all);  # keep the syntax checking for now

sub match_rule_unreserved_keyword     { shift->_keyword_match('unreserved')     }
sub match_rule_col_name_keyword       { shift->_keyword_match('col_name')       }
sub match_rule_type_func_name_keyword { shift->_keyword_match('type_func_name') }
sub match_rule_reserved_keyword       { shift->_keyword_match('reserved')       }
sub match_rule_any_keyword { 
   my $self  = $_[0];
   my $pos   = $self->{position};
   my $token = $self->{buffer}[$pos];
   return unless ($token->{keyword_type});
   return [];
}

sub _keyword_match { 
   my $self  = $_[0];
   my $pos   = $self->{position};
   my $token = $self->{buffer}[$pos];
   return unless ($token->{keyword_type} && $token->{keyword_type} eq $_[1]);
   return [];
}
