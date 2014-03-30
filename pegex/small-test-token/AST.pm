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

my $perl_safe_digits    = int( log(2)/log(10) * $Config{ivsize} * 8 );
my $identifier_max_size = 64;

#* Keyword category lists.  Generally, every keyword present in
#* the Postgres grammar should appear in exactly one of these lists.
#*
#* Put a new keyword into the first list that it can go into without causing
#* shift or reduce conflicts.  The earlier lists define "less reserved"
#* categories of keywords.

my %KEYWORD_TYPE;

#* "Unreserved" keywords --- available for use as any kind of name.
%KEYWORD_TYPE = (
   %KEYWORD_TYPE,
   (map { $_ => 'unreserved' } split(/\s*\|\s*/,
      'ABORT|ABSOLUTE|ACCESS|ACTION|ADD|ADMIN|AFTER|AGGREGATE|ALSO|ALTER|ALWAYS|ASSERTION|ASSIGNMENT|AT|ATTRIBUTE|
      BACKWARD|BEFORE|BEGIN|BY|
      CACHE|CALLED|CASCADE|CASCADED|CATALOG|CHAIN|CHARACTERISTICS|CHECKPOINT|CLASS|CLOSE|CLUSTER|COMMENT|COMMENTS|COMMIT|COMMITTED|CONFIGURATION|CONNECTION|CONSTRAINTS|CONTENT|CONTINUE|CONVERSION|COPY|COST|CSV|CURRENT|CURSOR|CYCLE|
      DATA|DATABASE|DAY|DEALLOCATE|DECLARE|DEFAULTS|DEFERRED|DEFINER|DELETE|DELIMITER|DELIMITERS|DICTIONARY|DISABLE|DISCARD|DOCUMENT|DOMAIN|DOUBLE|DROP|
      EACH|ENABLE|ENCODING|ENCRYPTED|ENUM|ESCAPE|EXCLUDE|EXCLUDING|EXCLUSIVE|EXECUTE|EXPLAIN|EXTENSION|EXTERNAL|
      FAMILY|FIRST|FOLLOWING|FORCE|FORWARD|FUNCTION|FUNCTIONS|
      GLOBAL|GRANTED|
      HANDLER|HEADER|HOLD|HOUR|
      IDENTITY|IF|IMMEDIATE|IMMUTABLE|IMPLICIT|INCLUDING|INCREMENT|INDEX|INDEXES|INHERIT|INHERITS|INLINE|INPUT|INSENSITIVE|INSERT|INSTEAD|INVOKER|ISOLATION|
      KEY|
      LABEL|LANGUAGE|LARGE|LAST|LC_COLLATE|LC_CTYPE|LEAKPROOF|LEVEL|LISTEN|LOAD|LOCAL|LOCATION|LOCK|
      MAPPING|MATCH|MAXVALUE|MINUTE|MINVALUE|MODE|MONTH|MOVE|
      NAME|NAMES|NEXT|NO|NOTHING|NOTIFY|NOWAIT|NULLS|
      OBJECT|OF|OFF|OIDS|OPERATOR|OPTION|OPTIONS|OWNED|OWNER|
      PARSER|PARTIAL|PARTITION|PASSING|PASSWORD|PLANS|PRECEDING|PREPARE|PREPARED|PRESERVE|PRIOR|PRIVILEGES|PROCEDURAL|PROCEDURE|
      QUOTE|
      RANGE|READ|REASSIGN|RECHECK|RECURSIVE|REF|REINDEX|RELATIVE|RELEASE|RENAME|REPEATABLE|REPLACE|REPLICA|RESET|RESTART|RESTRICT|RETURNS|REVOKE|ROLE|ROLLBACK|ROWS|RULE|
      SAVEPOINT|SCHEMA|SCROLL|SEARCH|SECOND|SECURITY|SEQUENCE|SEQUENCES|SERIALIZABLE|SERVER|SESSION|SET|SHARE|SHOW|SIMPLE|SNAPSHOT|STABLE|STANDALONE|START|STATEMENT|STATISTICS|STDIN|STDOUT|STORAGE|STRICT|STRIP|SYSID|SYSTEM|
      TABLES|TABLESPACE|TEMP|TEMPLATE|TEMPORARY|TEXT|TRANSACTION|TRIGGER|TRUNCATE|TRUSTED|TYPE|TYPES|
      UNBOUNDED|UNCOMMITTED|UNENCRYPTED|UNKNOWN|UNLISTEN|UNLOGGED|UNTIL|UPDATE|
      VACUUM|VALID|VALIDATE|VALIDATOR|VALUE|VARYING|VERSION|VIEW|VOLATILE|
      WHITESPACE|WITHOUT|WORK|WRAPPER|WRITE|
      XML|
      YEAR|YES|
      ZONE'
   ))
);

#* Column identifier --- keywords that can be column, table, etc names.
#*
#* Many of these keywords will in fact be recognized as type or function
#* names too; but they have special productions for the purpose, and so
#* can't be treated as "generic" type or function names.
#*
#* The type names appearing here are not usable as function names
#* because they can be followed by '(' in typename productions, which
#* looks too much like a function call for an LR(1) parser.
%KEYWORD_TYPE = (
   %KEYWORD_TYPE,
   (map { $_ => 'col_name' } split(/\s*\|\s*/,
      'BETWEEN|BIGINT|BIT|BOOLEAN|
      CHAR|CHARACTER|COALESCE|
      DEC|DECIMAL|
      EXISTS|EXTRACT|
      FLOAT|
      GREATEST|
      INOUT|INT|INTEGER|INTERVAL|
      LEAST|
      NATIONAL|NCHAR|NONE|NULLIF|NUMERIC|
      OUT|OVERLAY|
      POSITION|PRECISION|
      REAL|ROW|
      SETOF|SMALLINT|SUBSTRING|
      TIME|TIMESTAMP|TREAT|TRIM|
      VALUES|VARCHAR|
      XMLATTRIBUTES|XMLCONCAT|XMLELEMENT|XMLEXISTS|XMLFOREST|XMLPARSE|XMLPI|XMLROOT|XMLSERIALIZE'
   ))
);

#* Type/function identifier --- keywords that can be type or function names.
#*
#* Most of these are keywords that are used as operators in expressions;
#* in general such keywords can't be column names because they would be
#* ambiguous with variables, but they are unambiguous as function identifiers.
#*
#* Do not include POSITION, SUBSTRING, etc here since they have explicit
#* productions in a_expr to support the goofy SQL9x argument syntax.
#* - thomas 2000-11-28
%KEYWORD_TYPE = (
   %KEYWORD_TYPE,
   (map { $_ => 'type_func_name' } split(/\s*\|\s*/,
      'AUTHORIZATION|
      BINARY|
      COLLATION|CONCURRENTLY|CROSS|CURRENT_SCHEMA|
      FREEZE|FULL|
      ILIKE|INNER|IS|ISNULL|
      JOIN|
      LEFT|LIKE|
      NATURAL|NOTNULL|
      OUTER|OVER|OVERLAPS|
      RIGHT|
      SIMILAR|
      VERBOSE'
   ))
);

#* Reserved keyword --- these keywords are usable only as a ColLabel.
#*
#* Keywords appear here if they could not be distinguished from variable,
#* type, or function names in some contexts.  Don't put things here unless
#* forced to.
%KEYWORD_TYPE = (
   %KEYWORD_TYPE,
   (map { $_ => 'reserved' } split(/\s*\|\s*/,
      'ALL|ANALYSE|ANALYZE|AND|ANY|ARRAY|AS|ASC|ASYMMETRIC|
      BOTH|
      CASE|CAST|CHECK|COLLATE|COLUMN|CONSTRAINT|CREATE|CURRENT_CATALOG|CURRENT_DATE|CURRENT_ROLE|CURRENT_TIME|CURRENT_TIMESTAMP|CURRENT_USER|
      DEFAULT|DEFERRABLE|DESC|DISTINCT|DO|
      ELSE|END|EXCEPT|
      FALSE|FETCH|FOR|FOREIGN|FROM|
      GRANT|GROUP|
      HAVING|
      IN|INITIALLY|INTERSECT|INTO|
      LATERAL|LEADING|LIMIT|LOCALTIME|LOCALTIMESTAMP|
      NOT|NULL|
      OFFSET|ON|ONLY|OR|ORDER|
      PLACING|PRIMARY|
      REFERENCES|RETURNING|
      SELECT|SESSION_USER|SOME|SYMMETRIC|
      TABLE|THEN|TO|TRAILING|TRUE|
      UNION|UNIQUE|USER|USING|
      VARIADIC|
      WHEN|WHERE|WINDOW|WITH'
   ))
);

# blank out any whitespace captures
sub got_ws  { return; }
sub got_ws1 { return; }
sub got_ws2 { return; }

# quote parsing
sub got_L_XQFULL {  # standard quoted strings
   my ($self, $val) = @_;
   $val =~ s/'\s+'//g;  # concat
   $val =~ s/''/'/g;    # escaped quote
   return $val;
}
sub got_L_XEFULL {  # extended quoted strings (support backslash escape sequences)
   my ($self, $val) = @_;
   $val =~ s/(?<!\\)'\s+'//g;   # concat
   $val =~ s/(?<!\\)''/'/g;     # escaped quote
   $val =~ s/(\\.)/eval '"'.$1.'"'/ge;  # escaped char
   return $val;
}
sub got_L_XDOLQFULL { $_[1]->[1] }                  # $foo$ quoted strings (only need param #2)
sub got_L_XUIFULL   { $_[0]->udeescape(@{$_[1]}) }  # quoted identifier with Unicode escapes
sub got_L_XUSFULL   { $_[0]->udeescape(@{$_[1]}) }  # quoted string with Unicode escapes

### Constants (with wrapping) ###

# numbers
sub got_ICONST { $_[0]->process_number_literal($_[1], 0) }
sub got_FCONST { $_[0]->process_number_literal($_[1], 1) }

# operators

### Self (single-character) tokens ###

# These are normally defined as 'X' in a traditional parser, but no such syntax
# exists in Pegex.  We can't just use atoms or /X/ syntax, because we have to 
# include token separation.  Thus, all of the tokens are defined here as P_*
# tokens, and use a naming scheme similar to the Pegex atoms.

# NOTE: We have to double-check that it doesn't end up as an Op before the
# assignment.  Self does take priority, but the Op rule will already invalidate
# itself if it realizes that it's going to be a single-character L_SELF.

my %SELF_TOKENS = (
   ',' => 'COMMA',
   ';' => 'SEMI',
   ':' => 'COLON',
   '.' => 'DOT',
   '+' => 'PLUS',
   '-' => 'MINUS',
   '/' => 'SLASH',
   '*' => 'STAR',
   '^' => 'CARET',
   '=' => 'EQUAL',
   '%' => 'PERCENT',
   '(' => 'LPAREN',
   ')' => 'RPAREN',
   '[' => 'LBRACKET',
   ']' => 'RBRACKET',
   '<' => 'LT',
   '>' => 'GT',
);

sub got_SELF {
   my ($self, $val) = @_;
   return {
      ref   => 'P_'.$SELF_TOKENS{$val},
      match => $val,
      pos   => $self->{parser}{position},
   };
}

# identifiers
sub got_IDENT {
   # Unlike PostgreSQL, we can do a proper Unicode lowercase,
   # so downcase_truncate_identifier turns into a 'lc' + truncation.
   return {
      ref   => 'IDENT',
      match => lc substr($_[1], 0, $identifier_max_size),
      pos   => $_[0]->{parser}{position},
   };
}

sub got_KEYWORD {
   my ($self, $val) = @_;
   my $ref = uc $val;
   $ref = 'ANALYZE' if ($ref eq 'ANALYSE');
   ### FIXME: Support L_XNFULL|NCHAR ###
   
   return {
      ref   => $ref,
      match => lc $val,
      pos   => $self->{parser}{position},
      
      # special keyword type
      keyword_type => $KEYWORD_TYPE{$ref},
   };
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

   return {
      ref   => $is_float ? 'FCONST' : 'ICONST',
      match => $num,
      pos   => $self->{parser}{position},
   };
}
