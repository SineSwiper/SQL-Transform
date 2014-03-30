package SQL::Transform::Parser::Pg::AST;
### FIXME: Split into AST::Lexer and AST::Parser

use base 'Pegex::Receiver';

use sanity qw(sanity -warnings/all/FATAL warnings/all);  # keep the syntax checking for now
use Config;
use Math::BigInt;
use Math::BigFloat;

use Data::Dump;

# configure some basic big number stuff
Math::BigInt  ->config({
   upgrade    => 'Math::BigFloat',
   round_mode => 'common',
});
Math::BigFloat->config({
   round_mode => 'common',
});

my $perl_safe_digits    = int( log(2)/log(10) * $Config{ivsize} * 8 );  ### FIXME: Use better measurements from Types::Numbers
my $identifier_max_size = 64;

# blank out any whitespace captures
sub got_ws  { return; }
sub got_ws1 { return; }
sub got_ws2 { return; }

# quote parsing
sub got_L_XQFULL {  # standard quoted strings
   my ($self, $match) = @_;
   $match =~ s/'\s+'//g;  # concat
   $match =~ s/''/'/g;    # escaped quote
   return $match;
}
sub got_L_XEFULL {  # extended quoted strings (support backslash escape sequences)
   my ($self, $match) = @_;
   $match =~ s/(?<!\\)'\s+'//g;   # concat
   $match =~ s/(?<!\\)''/'/g;     # escaped quote
   $match =~ s/(\\.)/eval '"'.$1.'"'/ge;  # escaped char
   return $match;
}
sub got_L_XDOLQFULL { $_[1]->[1] }                  # $foo$ quoted strings (only need param #2)
sub got_L_XUIFULL   { $_[0]->udeescape(@{$_[1]}) }  # quoted identifier with Unicode escapes
sub got_L_XUSFULL   { $_[0]->udeescape(@{$_[1]}) }  # quoted string with Unicode escapes

# numbers
sub got_ICONST { $_[0]->process_number_literal($_[1], 0) }
sub got_FCONST { $_[0]->process_number_literal($_[1], 1) }

sub got_IDENT {
   # Unlike PostgreSQL, we can do a proper Unicode lowercase,
   # so downcase_truncate_identifier turns into a 'lc' + truncation.
   lc substr($_[1]->[0], 0, $identifier_max_size);
}

#* static char *
#* litbuf_udeescape(unsigned char escape, core_yyscan_t yyscanner)

sub udeescape {
   my ($self, $new, $escape) = @_;
   if ($escape =~ /^[\da-f\+\'\"\s]$/i) {
      $self->parser->throw_error("Invalid Unicode escape character");
      return undef;
   }

   $new =~ s/(?<!\Q${escape}\E)\Q${escape}\E(?:u([0-9A-Fa-f]{4})|U([0-9A-Fa-f]{8}))/chr hex $+/ge;

   return $new;
}

#* static int
#* process_integer_literal(const char *token, YYSTYPE *lval)

### This works a little differently, but achieves the same effect ###
sub process_number_literal {
   my ($self, $num, $is_float) = @_;

   # if this number is bigger than a bread box, use BigInt/Float
   my $digits = $num;
   $digits =~ s/^\D|[\.e].*$//gi;

   if ($perl_safe_digits >= length($digits)) {
      $num = int $num unless $is_float;
   }
   else {
      $num = $is_float ?
         Math::BigFloat->new($num) :
         Math::BigInt->new($num);
   }

   return $num;
}
