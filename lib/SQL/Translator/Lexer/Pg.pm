package SQL::Translator::Lexer::Pg;

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

# ERRCODEs
### FIXME: Convert these to use DBI::Const stuff... ###
use constant {
   WARNING => 19,
   ERROR   => 20,

   ERRCODE_SYNTAX_ERROR            => '42000',
   ERRCODE_FEATURE_NOT_SUPPORTED   => '0A000',
   ERRCODE_INVALID_ESCAPE_SEQUENCE => '22025',
   ERRCODE_NONSTANDARD_USE_OF_ESCAPE_CHARACTER => '22906',
};

# Based and converted from PostgreSQL's src/backend/parser/scan.l and
# src/interfaces/ecpg/preproc/parser.c

# Some comments retained from those sources, marked as #*.

#*-------------------------------------------------------------------------
#*
#* parser.c
#*      Main entry point/driver for PostgreSQL grammar
#*
#* Note that the grammar is not allowed to perform any table access
#* (since we need to be able to do basic parsing even while inside an
#* aborted transaction).  Therefore, the data structures returned by
#* the grammar are "raw" parsetrees that still need to be analyzed by
#* analyze.c and related files.
#*
#*
#* Portions Copyright (c) 1996-2012, PostgreSQL Global Development Group
#* Portions Copyright (c) 1994, Regents of the University of California
#*
#* IDENTIFICATION
#*    src/interfaces/ecpg/preproc/parser.c
#*
#*-------------------------------------------------------------------------

#* Intermediate filter between parser and base lexer (base_yylex in scan.l).
#*
#* The filter is needed because in some cases the standard SQL grammar
#* requires more than one token lookahead.  We reduce these cases to one-token
#* lookahead by combining tokens here, in order to keep the grammar LALR(1).
#*
#* Using a filter is simpler than trying to recognize multiword tokens
#* directly in scan.l, because we'd have to allow for comments between the
#* words.  Furthermore it's not clear how to do it without re-introducing
#* scanner backtrack, which would cost more performance than this filter
#* layer does.

#* int
#* filtered_base_yylex(void)
sub Filtered_Lexer {
   my ($p) = shift;
   my $cur_token;
   my $s = $p->YYInput;
   
   #* Get next token --- we might already have it
   if ($p->{have_lookahead}) {
      $cur_token = $p->{lookahead_token};
      pos($$s)   = $p->{lookahead_pos};
      $p->{have_lookahead} = 0;
   }
   else {
      $cur_token = [ $p->Lexer ];
   }

   #* Do we need to look ahead for a possible multiword token?
   if    ($cur_token->[0] eq 'NULLS') {
      #* NULLS FIRST and NULLS LAST must be reduced to one token
      my $cur_pos    = pos($$s);
      my $next_token = [ $p->Lexer ];
      for ($next_token->[0]) {
         when ('FIRST') { $cur_token = [ 'NULLS_FIRST', 'NULLS_FIRST' ]; }
         when ('LAST')  { $cur_token = [ 'NULLS_LAST',  'NULLS_LAST'  ]; }
         default {
            #* save the lookahead token for next time
            $p->{lookahead_token} = $next_token;
            $p->{lookahead_pos}   = pos($$s);
            $p->{have_lookahead}  = 1;
            #* and back up the output info to cur_token
            pos($$s) = $cur_pos;
         }
      }
   }
   elsif ($cur_token->[0] eq 'WITH') {
      #* WITH TIME must be reduced to one token
      my $cur_pos    = pos($$s);
      my $next_token = [ $p->Lexer ];
      for ($next_token->[0]) {
         when ('TIME') { $cur_token = [ 'WITH_TIME', 'WITH_TIME' ]; }
         default {
            #* save the lookahead token for next time
            $p->{lookahead_token} = $next_token;
            $p->{lookahead_pos}   = pos($$s);
            $p->{have_lookahead}  = 1;
            #* and back up the output info to cur_token
            pos($$s) = $cur_pos;
         }
      }
   }

   ### Debug output
   no warnings 'uninitialized';
   printf "---ENDTOKEN: %s, %s\n", @$cur_token;      
   
   return @$cur_token;
}



#*-------------------------------------------------------------------------
#*
#* scan.l
#*     lexical scanner for PostgreSQL
#*
#* NOTE NOTE NOTE:
#*
#* The rules in this file must be kept in sync with psql's lexer!!!
#*
#* The rules are designed so that the scanner never has to backtrack,
#* in the sense that there is always a rule that can match the input
#* consumed so far (the rule action may internally throw back some input
#* with yyless(), however).  As explained in the flex manual, this makes
#* for a useful speed increase --- about a third faster than a plain -CF
#* lexer, in simple testing.  The extra complexity is mostly in the rules
#* for handling float numbers and continued string literals.  If you change
#* the lexical rules, verify that you haven't broken the no-backtrack
#* property by running flex with the "-b" option and checking that the
#* resulting "lex.backup" file says that no backing up is needed.
#*
#*
#* Portions Copyright (c) 1996-2011, PostgreSQL Global Development Group
#* Portions Copyright (c) 1994, Regents of the University of California
#*
#* IDENTIFICATION
#*     src/backend/parser/scan.l
#*
#*-------------------------------------------------------------------------

#* GUC variables.  This is a DIRECT violation of the warning given at the
#* head of gram.y, ie flex/bison code must not depend on any GUC variables;
#* as such, changing their values can induce very unintuitive behavior.
#* But we shall have to live with it as a short-term thing until the switch
#* to SQL-standard string syntax is complete.

my $backslash_quote = 'BACKSLASH_QUOTE_SAFE_ENCODING';
my $escape_string_warning = 1;
my $standard_conforming_strings = 1;

#* OK, here is a short description of lex/flex rules behavior.
#* The longest pattern which matches an input string is always chosen.
#* For equal-length patterns, the first occurring in the rules list is chosen.
#* INITIAL is the starting state, to which all non-conditional rules apply.
#* Exclusive states change parsing rules while the state is active.  When in
#* an exclusive state, only those rules defined for that state apply.
#*
#* We use exclusive states for quoted strings, extended comments,
#* and to eliminate parsing troubles for numeric strings.
#* Exclusive states:
#*  <xb> bit string literal
#*  <xc> extended C-style comments
#*  <xd> delimited identifiers (double-quoted identifiers)
#*  <xh> hexadecimal numeric string
#*  <xq> standard quoted strings
#*  <xe> extended quoted strings (support backslash escape sequences)
#*  <xdolq> $foo$ quoted strings
#*  <xui> quoted identifier with Unicode escapes
#*  <xus> quoted string with Unicode escapes
#*  <xeu> Unicode surrogate pair in extended quoted string

#* In order to make the world safe for Windows and Mac clients as well as
#* Unix ones, we accept either \n or \r as a newline.  A DOS-style \r\n
#* sequence will be seen as two successive newlines, but that doesn't cause
#* any problems.  Comments that start with -- and extend to the next
#* newline are treated as equivalent to a single whitespace character.
#*
#* NOTE a fine point: if there is no newline following --, we will absorb
#* everything to the end of the input as a comment.  This is correct.  Older
#* versions of Postgres failed to recognize -- as a comment if the input
#* did not end with a newline.
#*
#* XXX perhaps \f (formfeed) should be treated as a newline as well?
#*
#* XXX if you change the set of whitespace characters, fix scanner_isspace()
#* to agree, and see also the plpgsql lexer.

# For the purposes of sanity, we will be using a bunch of scalars, instead
# of something like "use constant" or other things, since we can use the
# ${blah} syntax, don't have to jump in and out of hash definitions (because
# we need to use a previous definition), and scalars are less "messy" than
# dealing with ".sub." jumps.

# Oddly enough, according to Benchmark tests, strings are 60-70% faster than
# compiled qr() Regexps.  This may be due to the fact that qr() Regexps don't
# support the g/c flags embedded into the RE, and thus need to have to
# re-string the RE, anyway.

my $space       = '[ \t\n\r\f]';
my $horiz_space = '[ \t\f]';
my $newline     = '[\n\r]';
my $non_newline = '[^\n\r]';

my $comment     = "--${non_newline}*";

my $whitespace  = "(?:${space}+|${comment})";

#* SQL requires at least one newline in the whitespace separating
#* string literals that are to be concatenated.  Silly, but who are we
#* to argue?  Note that {whitespace_with_newline} should not have * after
#* it, whereas {whitespace} should generally have a * after it...

my $special_whitespace      = "(?:${space}+|${comment}${newline})";
my $horiz_whitespace        = "(?:${horiz_space}|${comment})";
my $whitespace_with_newline = "${horiz_whitespace}*${newline}${special_whitespace}*";

#* To ensure that {quotecontinue} can be scanned without having to back up
#* if the full pattern isn't matched, we include trailing whitespace in
#* {quotestop}.  This matches all cases where {quotecontinue} fails to match,
#* except for {quote} followed by whitespace and just one "-" (not two,
#* which would start a {comment}).  To cover that we have {quotefail}.
#* The actions for {quotestop} and {quotefail} must throw back characters
#* beyond the quote proper.

### TODO: Optimize this to Perl RE standards, rather than flex work-arounds ###
### (IE: Zero-width assersions would be nice...) ###

my $quote         = "'";
my $quotestop     = "${quote}${whitespace}*";
my $quotecontinue = "${quote}${whitespace_with_newline}${quote}";
my $quotefail     = "${quote}${whitespace}*-";

# This is often combined, so just put it into one RE
my $quotestopfail = "${quotestop}-?";

### NOTE ###
# SQL and Unicode have a bit of a shakey co-existence.  SQL was designed
# with English phrases in mind, along with English digits and identifiers.
# Thus, Unicode isn't allowed everywhere, so that proper detection between
# ASCII and Unicode can be achieved.

# However, Perl can already properly detect Unicode naturally.  So, instead
# of allowing strictest ANSI/ISO SQL in this case, we'll allow identifiers
# to be in UTF-8 without the need for the U& notation.  After all, the
# entire SQL statement could have been written in UTF-8 and we don't want to
# flat out reject the whole thing based on that.

### Bit string ###
#* It is tempting to scan the string for only those characters
#* which are allowed. However, this leads to silently swallowed
#* characters if illegal characters are included in the string.
#* For example, if xbinside is [01] then B'ABCD' is interpreted
#* as a zero-length string, and the ABCD' is lost!
#* Better to pass the string forward and let the input routines
#* validate the contents.

my $xbstart  = "[bB]${quote}";
my $xbinside = q/[^']*/;

### Hexadecimal number ###
my $xhstart  = "[xX]${quote}";
my $xhinside = q/[^']*/;

### National character ###
my $xnstart  = "[nN]${quote}";

### Quoted string that allows backslash escapes ###
my $xestart       = "[eE]${quote}";
my $xeinside      = '[^\\'."'".']+';
my $xeescape      = '\\[^0-7]';
my $xeoctesc      = '\\[0-7]{1,3}';
my $xehexesc      = '\\x[0-9A-Fa-f]{1,2}';
my $xeunicode     = '\\(?:u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})';
my $xeunicodefail = '\\(?:u[0-9A-Fa-f]{0,3}|U[0-9A-Fa-f]{0,7})';

### Extended quote ###
#* xqdouble implements embedded quote, ''''

my $xqstart  = $quote;
my $xqdouble = $quote.$quote;
my $xqinside = q/[^']+/;

### $foo$ style quotes ("dollar quoting") ###
#* The quoted string starts with $foo$ where "foo" is an optional string
#* in the form of an identifier, except that it may not contain "$",
#* and extends to the first occurrence of an identical string.
#* There is *no* processing of the quoted text.
#*
#* {dolqfailed} is an error rule to avoid scanner backup when {dolqdelim}
#* fails to match its trailing "$".

my $dolq_start = '[\p{Alphabetic}\x80-\xFF_]';
my $dolq_cont  = '[\p{Alphabetic}\x80-\xFF_\p{Number}]';
my $dolqdelim  = '\$'."(${dolq_start}${dolq_cont}*)?".'\$';
my $dolqfailed = '\$'."${dolq_start}${dolq_cont}*";
my $dolqinside = '[^\$]+';

### Double quote ###
#* Allows embedded spaces and other special characters into identifiers.

my $dquote   = '"';
my $xdstart  = $dquote;
my $xdstop   = $dquote;
my $xddouble = $dquote.$dquote;
my $xdinside = q/[^"]+/;

### Unicode escapes ###

# See, in the "primitive" world of Perl, we have these things called "modifiers" for our REs :)
#* uescape         [uU][eE][sS][cC][aA][pP][eE]{whitespace}*{quote}[^']{quote}
my $uescape     = "(?i:UESCAPE${whitespace}*${quote}[^']${quote})";

#* error rule to avoid backup
#* uescapefail     ("-"|[uU][eE][sS][cC][aA][pP][eE]{whitespace}*"-"|[uU][eE][sS][cC][aA][pP][eE]{whitespace}*{quote}[^']|[uU][eE][sS][cC][aA][pP][eE]{whitespace}*{quote}|[uU][eE][sS][cC][aA][pP][eE]{whitespace}*|[uU][eE][sS][cC][aA][pP]|[uU][eE][sS][cC][aA]|[uU][eE][sS][cC]|[uU][eE][sS]|[uU][eE]|[uU])
my $uescapefail = "(?i:-|UE?S?C?A?P?E?${whitespace}*${quote}?[^']?)";

### Quoted identifier with Unicode escapes ###
my $xuistart = "[uU]\&${dquote}";
my $xuistop1 = "${dquote}${whitespace}*${uescapefail}?";
my $xuistop2 = "${dquote}${whitespace}*${uescape}";

### Quoted string with Unicode escapes ###
my $xusstart = "[uU]\&${quote}";
my $xusstop1 = "${quote}${whitespace}*${uescapefail}?";
my $xusstop2 = "${quote}${whitespace}*${uescape}";

#* error rule to avoid backup #
my $xufailed = '[uU]\&';

### C-style comments ###

#* The "extended comment" syntax closely resembles allowable operator syntax.
#* The tricky part here is to get lex to recognize a string starting with
#* slash-star as a comment, when interpreting it as an operator would produce
#* a longer match --- remember lex will prefer a longer match!  Also, if we
#* have something like plus-slash-star, lex will think this is a 3-character
#* operator whereas we want to see it as a + operator and a comment start.
#* The solution is two-fold:
#* 1. append {op_chars}* to xcstart so that it matches as much text as
#*    {operator} would. Then the tie-breaker (first matching rule of same
#*    length) ensures xcstart wins.  We put back the extra stuff with yyless()
#*    in case it contains a star-slash that should terminate the comment.
#* 2. In the operator rule, check for slash-star within the operator, and
#*    if found throw it back with yyless().  This handles the plus-slash-star
#*    problem.
#* Dash-dash comments have similar interactions with the operator rule.

my $op_chars = '[\~\!\@\#\^\&\|\`\?\+\-\*\/\%\<\>\=]';
my $xcstart  = '\/\*'.$op_chars.'*';
my $xcstop   = '\*+\/';
my $xcinside = '[^*/]+';

my $ident_start = '[\p{Alphabetic}\x80-\xFF_]';
my $ident_cont  = '[\w\x80-\xFF\$]';  # \w naturally "just works"

my $identifier  = "${ident_start}${ident_cont}*";

my $typecast     = '::';
my $dot_dot      = '\.\.';
my $colon_equals = ':=';

#* "self" is the set of chars that should be returned as single-character
#* tokens.  "op_chars" is the set of chars that can make up "Op" tokens,
#* which can be one or more characters long (but if a single-char token
#* appears in the "self" set, it is not to be returned as an Op).  Note
#* that the sets overlap, but each has some chars that are not in the other.
#*
#* If you change either set, adjust the character lists appearing in the
#* rule for "operator"!

my $self     = '[,\(\)\[\].;\:\+\-\*\/\%\^\<\>\=]';
#my $op_chars = '[\~\!\@\#\^\&\|\`\?\+\-\*\/\%\<\>\=]';
my $operator = "${op_chars}+";

# used in $operator checks
my $non_math = '[\~\!\@\#\^\&\|\`\?]';

#* we no longer allow unary minus in numbers.
#* instead we pass it separately to parser. there it gets
#* coerced via doNegate() -- Leon aug 20 1999
#*
#* {decimalfail} is used because we would like "1..10" to lex as 1, dot_dot, 10.
#*
#* {realfail1} and {realfail2} are added to prevent the need for scanner
#* backup when the {real} rule fails to match completely.

# We aren't allowing non-English here, else all coder's brains would spontaneously explode
# at the prospect of making a Tibetian digit work with functions like int()...
my $digit       = '\d';
my $integer     = "${digit}+";
my $decimal     = "(?:(?:${digit}*".'\.'."${digit}+)|(?:${digit}+".'\.'."${digit}*))";
my $decimalfail = "${digit}+\.\.";
my $real        = "(?:${integer}|${decimal})[Ee][-+]?${digit}+";
my $realfail1   = "(?:${integer}|${decimal})[Ee]";
my $realfail2   = "(?:${integer}|${decimal})[Ee][-+]";

my $param       = '\$'.$integer;

# (I'm not even going to bother...)
#* other         .

#* Dollar quoted strings are totally opaque, and no escaping is done on them.
#* Other quoted strings must allow some special characters such as single-quote
#*  and newline.
#* Embedded single-quotes are implemented both in the SQL standard
#*  style of two adjacent single quotes "''" and in the Postgres/Java style
#*  of escaped-quote "\'".
#* Other embedded escaped characters are matched explicitly and the leading
#*  backslash is dropped from the string.
#* Note that xcstart must appear before operator, as explained above!
#*  Also whitespace (comment) must appear before operator.

#* %%

# In order to get maximum RE preformance, the \G needs to be embedded into the
# string.  We waited until now to do it, since the vars were being used as "sub-REs"
# in other variables.  Also, we made sure that any | cases were put into (?:), so
# that \G doesn't get excluded in certain cases.

### TODO: Getting rid of this $scalar business and just putting it into the RE
### itself is still 20% faster...

my $xeunicode_raw = $xeunicode;

# $whitespace = '\G'.$whitespace;
eval "\$$_ = '\\G'.\$$_;" for (qw/
   whitespace quotestop quotefail quotestopfail quotecontinue
   xcstart xcstop xcinside op_chars
   xbstart xbinside
   xhstart xhinside
   xnstart
   xqstart
   xestart xeinside xeunicode xeunicodefail xeescape xeoctesc xehexesc
   xusstart xusstop1 xusstop2 xufailed
   xqdouble xqinside
   xdstart xdstop xddouble xdinside
   xuistart xuistop1 xuistop2

   dolqdelim dolqfailed dolqinside
   typecast dot_dot colon_equals identifier
   self operator param
   integer decimal decimalfail real realfail1 realfail2
/);

my $keywords = &_keywords;

sub Lexer {
   my ($p) = shift;
   $p->{startstate} ||= 'INITIAL';
   
   # Like flex/bison's yylloc, we rely heavily on pos, and use the
   # traditional \G matches that e/yapp typically does.  The pos
   # command is capable of storing its position even on string
   # references, but since we're already using $_ (and prolonged usage
   # is just asking for trouble), we need to copy it to $s.

   # Fortunately, pos($$s) will ref-match with pos(${$p->YYInput}), so
   # there are no "pos transfers" required.  Hence, we keep the ref.
   my $s = $p->YYInput;

   # Start condition checks:
   #    * Using label jumps to save on unneeded recursive calls.
   #    * Unless the rest of the section hits only ignores and EOI blocks, or
   #      finishes with a return, most RE checks should end with a redo/goto.
   #    * Some of the blocks may have been re-ordered to group them together,
   #      but the effect is the same.
   #    * Since we actually aren't following flex's "longest match" rule, some
   #      if-blocks need to be re-ordered.
   #    * Ignore blocks need special +| REs, so that it will ignore multiple instances
   #      without problems.
   START: for ($p->{startstate}) {
      ### Debug output
      no warnings 'uninitialized';
      my $i = index($$s, "\n", pos($$s));
      $i = $i == -1 ? 0 : $i - pos($$s);
      $i = 50 if ($i > 50 || $i <= 0);
      printf "---STARTSTATE: %s @ %u ('%s')\n", $p->{startstate}, pos($$s), substr($$s, pos($$s), $i);      
      
      when ('INITIAL') {
         # Since INITIAL is such a large block and the lack of "longest match", we
         # need to be careful about how these if-blocks are ordered.  In most cases,
         # it's easy to figure out which RE will match as the longest, so we can find
         # the ones that conflict and sort those on length.

         # A few pointers on that front:
         #    * Though scan.l does the same thing, the failed REs should always be AFTER
         #      the starter REs, as they are generally the same thing except with less
         #      characters.
         #    * The biggest dual conflict offenders seem to be the less-specific REs
         #      after $xcstart above.

         ;;; $$s =~ /${whitespace}+/gc;  #* ignore
         
         if ($$s =~ /${xcstart}/gc) {
            say '---GOTMATCH: xcstart';
            $p->{xcdepth} = 0;
            $p->YYLess(2);  #* Put back any characters past slash-star; see above
            $p->{startstate} = 'xc'; redo START;
         }
         if ($$s =~ /${xbstart}/gc) {
            #* Binary bit type.
            #* At some point we should simply pass the string
            #* forward to the parser and label it there.
            #* In the meantime, place a leading "b" on the string
            #* to mark it for the input routine as a binary string.

            say '---GOTMATCH: xbstart';
            $p->{literalbuf} = 'b';
            $p->{startstate} = 'xb'; redo START;
         }
         if ($$s =~ /${xhstart}/gc) {
            #* Hexadecimal bit type.
            #* At some point we should simply pass the string
            #* forward to the parser and label it there.
            #* In the meantime, place a leading "x" on the string
            #* to mark it for the input routine as a hex string.

            say '---GOTMATCH: xhstart';
            $p->{literalbuf} = 'x';
            $p->{startstate} = 'xh'; redo START;
         }
         if ($$s =~ /${xnstart}/gc) {
            #* National character.
            #* We will pass this along as a normal character string,
            #* but preceded with an internally-generated "NCHAR".

            ### FIXME: We should emulate Pg's KeywordLookup, but for now,
            ### we're just going to blindly assume that the keyword exists.

            say '---GOTMATCH: xnstart';
            $p->YYLess(1);  #* eat only 'n' this time
            return('NCHAR', 'NCHAR');
         }
         if ($$s =~ /${xqstart}/gc) {
            say '---GOTMATCH: xqstart';
            $p->{warn_on_first_escape} = 1;
            $p->{literalbuf} = '';
            $p->{startstate} = $standard_conforming_strings ? 'xq' : 'xe';
            redo START;
         }
         if ($$s =~ /${xestart}/gc) {
            say '---GOTMATCH: xestart';
            $p->{warn_on_first_escape} = 0;
            $p->{literalbuf} = '';
            $p->{startstate} = 'xe'; redo START;
         }
         if ($$s =~ /${xusstart}/gc) {
            say '---GOTMATCH: xusstart';
            $p->{literalbuf} = '';

            unless ($standard_conforming_strings) {
               $p->ereport(ERROR,
                  ERRCODE_FEATURE_NOT_SUPPORTED,
                  "Unsafe use of string constant with Unicode escapes, near char position ".$-[0].".",
                  "String constants with Unicode escapes cannot be used when standard_conforming_strings is off.",
                  $p->YYLLoc()
               );
            }

            $p->{startstate} = 'xus'; redo START;
         }
         if ($$s =~ /${dolqdelim}/gc) {
            say '---GOTMATCH: dolqdelim';
            $p->{dolqstart} = $p->YYText;
            $p->{literalbuf} = '';
            $p->{startstate} = 'xdolq'; redo START;
         }
         if ($$s =~ /${dolqfailed}/gc) {
            say '---GOTMATCH: dolqfailed';
            #* throw back all but the initial "$"
            $p->YYLess(1);
            #* and treat it as {other}
            return ($p->YYText, $p->YYText);
         }
         if ($$s =~ /${xdstart}/gc) {
            say '---GOTMATCH: xdstart';
            $p->{literalbuf} = '';
            $p->{startstate} = 'xd'; redo START;
         }
         if ($$s =~ /${xuistart}/gc) {
            say '---GOTMATCH: xuistart';
            $p->{literalbuf} = '';
            $p->{startstate} = 'xui'; redo START;
         }
         if ($$s =~ /${xufailed}/gc) {
            say '---GOTMATCH: xufailed';
            #* throw back all but the initial u/U
            $p->YYLess(1);
            #* and treat it as {identifier}

            # Unlike PostgreSQL, we can do a proper Unicode lowercase,
            # so downcase_truncate_identifier turns into a 'lc' + truncation.
            return ('IDENT', lc substr($p->YYText, 0, $identifier_max_size));
         }

         # Okay, this is where the ordering gets hairy:
         #    * TYPECAST, DOT_DOT, and COLON_EQUALS are -not- contained in the char sets for
         #      $op_chars, so we're safe there, but they are in $self, so they should be first.
         #    * IDENT and PARAM don't conflict with anyone else, nor do any of the numerals.
         #    * As per the Pg comments in the "self" section, $self should win out for single
         #      characters.  But, Op should take priority for more than one character, as per
         #      flex's LM rule.  Thus, we need to get creative with those two...
         #    * All of the numbers need another priority check, so that integers don't win out
         #      over reals.
         #    * All priorities checks need their 'winner' comparisons done FIRST, so that it
         #      short-circuits and prevents the REs from screwing up the position.

         if ($$s =~ /${typecast}/gc) {
            say '---GOTMATCH: typecast';
            return ('TYPECAST', $p->YYText);
         }
         if ($$s =~ /${dot_dot}/gc) {
            say '---GOTMATCH: dot_dot';
            return ('DOT_DOT', $p->YYText);
         }
         if ($$s =~ /${colon_equals}/gc) {
            say '---GOTMATCH: colon_equals';
            return ('COLON_EQUALS', $p->YYText);
         }

         # priority check
         my $self_op_winner = $p->YYLexPrioritize({
            self     => $self,
            operator => $operator,
         }, [qw(self operator)]);
         say "---GOTMATCH: $self_op_winner" if $self_op_winner;
         
         if ($self_op_winner eq 'self'     && $$s =~ /${self}/gc) {
            return ($p->YYText, $p->YYText);
         }
         if ($self_op_winner eq 'operator' && $$s =~ /${operator}/gc) {
            #* Check for embedded slash-star or dash-dash; those
            #* are comment starts, so operator must stop there.
            #* Note that slash-star or dash-dash at the first
            #* character will match a prior rule, not this one.
            my $text = $p->YYText;
            my @text = split //, $text;  # C's array-based notation comes in handy here...
            my $slashstar = index($text, '/*') + 1;
            my $dashdash  = index($text, '--') + 1;
            my $nchars    = length $text;

            if ($slashstar && $dashdash) {
               #* if both appear, take the first one
               $slashstar = $dashdash if ($slashstar > $dashdash);
            }
            elsif (!$slashstar) {
               $slashstar = $dashdash;
            }
            $nchars = $slashstar if ($slashstar);


            #* For SQL compatibility, '+' and '-' cannot be the
            #* last char of a multi-char operator unless the operator
            #* contains chars that are not in SQL operators.
            #* The idea is to lex '=-' as two operators, but not
            #* to forbid operator names like '?-' that could not be
            #* sequences of SQL operators.
            while ($nchars > 1 && $text[$nchars-1] =~ /[\+\-]/) {
               my $ic;

               for ($ic = $nchars-2; $ic >= 0; $ic--) {
                  last if ($text[$ic] =~ /${non_math}/);
               }
               last if ($ic >= 0);  #* found a char that makes it OK
               $nchars--;           #* else remove the +/-, and check again
            }

            if ($nchars < length $text) {
               #* Strip the unwanted chars from the token
               $p->YYLess($nchars);
               $text = $p->YYText;
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
         if ($$s =~ /${param}/gc) {
            say '---GOTMATCH: param';
            return ('PARAM', '$'.int( substr($p->YYText, 1) ));
         }
         
         # numeral priority check
         my $numeral_winner = $p->YYLexPrioritize({
            integer     => $integer,     
            decimal     => $decimal,
            decimalfail => $decimalfail,
            real        => $real,
            realfail1   => $realfail1,
            realfail2   => $realfail2,
         }, [qw(integer decimal decimalfail real realfail1 realfail2)]);
         say "---GOTMATCH: $numeral_winner" if $numeral_winner;

         if ($numeral_winner eq 'real'        && $$s =~ /${real}/gc) {
            return $p->process_number_literal( $p->YYText );
         }
         if ($numeral_winner eq 'realfail1'   && $$s =~ /${realfail1}/gc) {
            #* throw back the [Ee], and treat as {decimal}.  Note
            #* that it is possible the input is actually {integer},
            #* but since this case will almost certainly lead to a
            #* syntax error anyway, we don't bother to distinguish.
            pos($$s) = pos($$s) - 1;
            return $p->process_number_literal( $p->YYText );
         }
         if ($numeral_winner eq 'realfail2'   && $$s =~ /${realfail2}/gc) {
            #* throw back the [Ee][+-], and proceed as above
            pos($$s) = pos($$s) - 2;
            return $p->process_number_literal( $p->YYText );
         }
         if ($numeral_winner eq 'decimal'     && $$s =~ /${decimal}/gc) {
            return $p->process_number_literal( $p->YYText );
         }
         if ($numeral_winner eq 'decimalfail' && $$s =~ /${decimalfail}/gc) {
            #* throw back the .., and treat as integer
            pos($$s) = pos($$s) - 2;
            return $p->process_number_literal( $p->YYText );
         }
         if ($numeral_winner eq 'integer'     && $$s =~ /${integer}/gc) {
            return $p->process_number_literal( $p->YYText );
         }
         if ($$s =~ /${identifier}/gc) {
            say '---GOTMATCH: identifier';
            my $ident = $p->YYText;
            return (uc $ident, $ident) if ($ident =~ $keywords);

            # Unlike PostgreSQL, we can do a proper Unicode lowercase,
            # so downcase_truncate_identifier turns into a 'lc' + truncation.
            return ('IDENT', lc substr($ident, 0, $identifier_max_size));
         }

         # The default {other}
         if ($$s =~ /\G(.)/gc) {
            say '---GOTMATCH: other';
            return ($1, $1);
         }
         # End of line: INITIAL
      }

      ### THE REST ###

      when ('xc') {
         XC:
         printf "---STARTSTATE: %s @ %u ('%s')\n", $p->{startstate}, pos($$s), substr($$s, pos($$s), 50);
         if ($$s =~ /${xcstart}/gc) {
            printf "---xcstart: %s @ %u ('%s')\n", $p->{startstate}, pos($$s), substr($$s, pos($$s), 50);
            $p->{xcdepth}++;
            $p->YYLess(2);  #* Put back any characters past slash-star; see above
            # no goto: YYLess toss-back makes it an infinite loop, since we aren't using "longest match"
         }
         if ($$s =~ /${xcstop}/gc) {
            printf "---xcstop: %s @ %u ('%s')\n", $p->{startstate}, pos($$s), substr($$s, pos($$s), 50);
            ($p->{xcdepth} <= 0) ?
               do { $p->{startstate} = 'INITIAL'; redo START; } :
               $p->{xcdepth}--;
         }
         
         # goto ignores, since ordering is important here...
         goto XC if ($$s =~ /${xcinside}/gc);  #* ignore
         goto XC if ($$s =~ /${op_chars}/gc);  #* ignore
         goto XC if ($$s =~ /\G\*+/gc);        #* ignore

         if ($p->YYEndOfInput) {
            $p->YYError("Unterminated /* comment");
         }
         
         goto XC;
         # End of line: xc
      }
      when ('xb') {
         XB:
         if ($$s =~ /${quotestopfail}/gc) {
            $p->YYLess(1);
            $p->{startstate} = 'INITIAL';

            return ('BCONST', $p->{literalbuf});
         }
         if ($$s =~ /${xbinside}/gc) {
            $p->{literalbuf} .= $p->YYText;
         }
         ;;; $$s =~ /${quotecontinue}+/gc;  #* ignore
         if ($p->YYEndOfInput) {
            $p->YYError("Unterminated bit string literal");
         }
         goto XB;
         # End of line: xb
      }
      # Moved to get it out of that complex mess below...
      when ('xdolq') {
         XDOLQ: 
         if ($$s =~ /${dolqdelim}/gc) {
            if ($p->YYText eq $p->{dolqstart}) {
               $p->{dolqstart} = '';
               $p->{startstate} = 'INITIAL';
               return ('SCONST', $p->{literalbuf});
            }
            else {
               #* When we fail to match $...$ to dolqstart, transfer
               #* the $... part to the output, but put back the final
               #* $ for rescanning.  Consider $delim$...$junk$delim$
               pos($$s) = pos($$s) - 1;
               $p->{literalbuf} .= $p->YYText;
               goto XDOLQ;
            }
         }
         if ($$s =~ /${dolqinside}/gc) {
            $p->{literalbuf} .= $p->YYText;
            goto XDOLQ;
         }
         if ($$s =~ /${dolqfailed}/gc) {
            $p->{literalbuf} .= $p->YYText;
            goto XDOLQ;
         }
         if ($$s =~ /\G(.)/gc) {
            #* This is only needed for $ inside the quoted text
            $p->{literalbuf} .= $1;
         }
         if ($p->YYEndOfInput) {
            $p->YYError("Unterminated dollar-quoted string");
         }
         goto XDOLQ;
         # End of line: xdolq
      }

      # The start conditions start to get complicated at this point.
      # Hence, this is why we are constantly adding "continue;", as the
      # blocks are no longer 1:1 exclusive matches.  Final blocks for
      # certain start conditions are marked, explaining why there is no
      # "continue;" for those.

      # (We're still going to put INITIAL blocks at the top, though.)

      # Moved since this is the less-complex set
      XDXUI:
      when ('xd') {
         if ($$s =~ /${xdstop}/gc) {
            $p->{startstate} = 'INITIAL';

            $p->YYError("Zero-length delimited identifier")
               unless ($p->{literalbuf});
            my $ident = $p->{literalbuf};
            $ident = substr($ident, 0, $identifier_max_size);
            return ('IDENT', $ident);
         }
         continue;
      }
      when ('xui') {
         if ($$s =~ /${xuistop1}/gc) {
            $p->{startstate} = 'INITIAL';

            $p->YYError("Zero-length delimited identifier")
               unless ($p->{literalbuf});
            my $ident = $p->udeescape('\\');
            $ident = substr($ident, 0, $identifier_max_size);
            $p->YYLess(1);  #* throw back all but the quote
            return ('IDENT', $ident);
         }
         if ($$s =~ /${xuistop2}/gc) {
            $p->{startstate} = 'INITIAL';

            $p->YYError("Zero-length delimited identifier")
               unless ($p->{literalbuf});
            my $ident = $p->udeescape( substr($p->YYText, $p->YYLeng - 2, 1) );
            $ident = substr($ident, 0, $identifier_max_size);
            return ('IDENT', $ident);
         }
         continue;
      }
      when ([qw(xd xui)]) {
         if ($$s =~ /${xddouble}/gc) {
            $p->{literalbuf} .= '"';
            goto XDXUI;
         }
         if ($$s =~ /${xdinside}/gc) {
            $p->{literalbuf} .= $p->YYText;
            goto XDXUI;
         }
         if ($p->YYEndOfInput) {
            $p->YYError("Unterminated quoted identifier");
         }
         goto XDXUI;
         # End of line: xd xui
      }
      
      XCOMPLEX:
      when ([qw(xq xe)]) {
         if ($$s =~ /${quotestopfail}/gc) {
            $p->YYLess(1);
            $p->{startstate} = 'INITIAL';

            ### NOTE: pg_verifymbstr validates database encoding.  Since we don't have a
            ### database nor a "default encoding", this check is purposely unimplemented.

            # Also, we remove sets for saw_non_ascii, since this is the only place it's used.

            #* check that the data remains valid if it might have been
            #* made invalid by unescaping any chars.
            #if ($p->{saw_non_ascii}) {
               #* pg_verifymbstr(yyextra->literalbuf,
               #*                yyextra->literallen,
               #*                false);
            #}

            return ('SCONST', $p->{literalbuf});
         }
         continue;
      }
      when ('xus') {
         if ($$s =~ /${xusstop1}/gc) {
            $p->YYLess(1);  #* throw back all but the quote
            $p->{startstate} = 'INITIAL';
            $p->{literalbuf} = $p->udeescape('\\');
            return ('SCONST', $p->{literalbuf});
         }
         if ($$s =~ /${xusstop2}/gc) {
            $p->{startstate} = 'INITIAL';
            $p->{literalbuf} = $p->udeescape( substr($$s, pos($$s) - 2, 1) );
            return ('SCONST', $p->{literalbuf});
         }
         continue;
      }
      when ([qw(xq xe xus)]) {
         if ($$s =~ /${xqdouble}/gc) {
            $p->{literalbuf} .= "'";
            goto XCOMPLEX;
         }
         continue;
      }
      when ([qw(xq xus)]) {
         if ($$s =~ /${xqinside}/gc) {
            $p->{literalbuf} .= $p->YYText;
            goto XCOMPLEX;
         }
         continue;
      }
      when ('xe') {
         if ($$s =~ /${xeinside}/gc) {
            $p->{literalbuf} .= $p->YYText;
            goto XCOMPLEX;
         }
         if ($$s =~ /${xeunicode}/gc) {
            my $ord = hex substr($p->YYText, 2);

            $p->check_escape_warning;

            if ($p->is_utf16_surrogate_first($ord)) {
               $p->{utf16_first_part} = $ord;
               $p->{startstate} = $_ = 'xeu';  # redo--; $_ abuse++ : xeu is right next door...
            }
            elsif ($p->is_utf16_surrogate_second($ord)) {
               $p->YYError("Invalid Unicode surrogate pair");
            }
            else {
               $p->{literalbuf} .= chr $ord;
            }
         }
         continue;
      }
      when ('xeu') {
         if ($$s =~ /${xeunicode}/gc) {
            my $ord = hex substr($p->YYText, 2);

            unless ($p->is_utf16_surrogate_second($ord)) {
               $p->YYError("Invalid Unicode surrogate pair");
            }

            my $cp = $p->surrogate_pair_to_codepoint($p->{utf16_first_part}, $ord);

            $p->{literalbuf} .= chr $cp;
            $p->{startstate} = $_ = 'xe'; goto XCOMPLEX;   # xe is in this neighborhood; might as well shorten the jump...
         }
         continue;
      }
      when ([qw(xe xeu)]) {
         if ($$s =~ /${xeunicodefail}/gc) {
            $p->ereport(ERROR,
               ERRCODE_INVALID_ESCAPE_SEQUENCE,
               "Invalid Unicode escape",
               "Unicode escapes must be \\uXXXX or \\UXXXXXXXX.",
               $p->YYLLoc()
            );
         }
         continue;
      }
      # This block was moved because flex naturally makes it last based on the
      # "longest match" rule.  We have no such rule, so we re-order it.
      when ('xeu') {
         # The three rules say '.', '\n', and '<<EOF>>', but it's really just an "else".
         $p->YYError("Invalid Unicode surrogate pair");
         # End of line: xeu
      }
      when ('xe') {
         if ($$s =~ /${xeescape}/gc) {
            my $c = substr($p->YYText, 1, 1);
            if ($c eq '\'') {
               if ($backslash_quote eq 'BACKSLASH_QUOTE_OFF' || $backslash_quote eq 'BACKSLASH_QUOTE_SAFE_ENCODING') {
                  $p->ereport(ERROR,
                     ERRCODE_NONSTANDARD_USE_OF_ESCAPE_CHARACTER,
                     "Unsafe use of \\' in a string literal",
                     "Use '' to write quotes in strings. \\' is insecure in client-only encodings.",
                     $p->YYLLoc()
                  );
               }
            }
            $p->check_string_escape_warning($c);
            $p->{literalbuf} .= eval "\"\\$c\"";
            goto XCOMPLEX;
         }
         if ($$s =~ /${xeoctesc}/gc) {
            my $ord = oct substr($p->YYText, 1);

            $p->check_escape_warning;
            $p->{literalbuf} .= chr $ord;
            goto XCOMPLEX;
         }
         if ($$s =~ /${xehexesc}/gc) {
            my $ord = hex substr($p->YYText, 2);

            $p->check_escape_warning;
            $p->{literalbuf} .= chr $ord;
            goto XCOMPLEX;
         }
         continue;
      }
      when ([qw(xq xe xus)]) {
         ;;; $$s =~ /${quotecontinue}+/gc;  #* ignore
         continue;
      }
      when ('xe') {
         if ($$s =~ /\G(.)/gc) {
            #* This is only needed for \ just before EOF
            $p->{literalbuf} .= $1;
            goto XCOMPLEX;
         }
         continue;
      }
      when ([qw(xq xe xus)]) {
         if ($p->YYEndOfInput) {
            $p->YYError("Unterminated quoted string");
         }
         goto XCOMPLEX;
         # End of line: xq xe xus
      }
      default {
         die "Unknown lexer start condition '".$p->{startstate}."'";
      }
   }

   print "---EOB\n";
   
   # End of blocks
   if ($p->YYEndOfInput) {
      return('', undef);
   }
   goto START;
}

### Here is where we put in the keywords.  This is basically a dupe of the list in the
### parser file...

sub _keywords {
   my $kw = join '|', qw(
      abort absolute access action add admin after aggregate all also alter always analyse analyze and any array as asc assertion assignment asymmetric at attribute authorization
      backward before begin between bigint binary bit boolean both by
      cache called cascade cascaded case cast catalog chain char character characteristics check checkpoint class close cluster coalesce collate collation column comment comments commit committed concurrently configuration connection constraint constraints content continue conversion copy cost create cross csv current current_catalog current_date current_role current_schema current_time current_timestamp current_user cursor cycle
      data database day deallocate dec decimal declare default defaults deferrable deferred definer delete delimiter delimiters desc dictionary disable discard distinct do document domain double drop
      each else enable encoding encrypted end enum escape event except exclude excluding exclusive execute exists explain extension external extract
      false family fetch first float following for force foreign forward freeze from full function functions
      global grant granted greatest group
      handler having header hold hour
      identity if ilike immediate immutable implicit in including increment index indexes inherit inherits initially inline inner inout input insensitive insert instead int integer intersect interval into invoker is isnull isolation
      join
      key
      label language large last lateral lc_collate lc_ctype leading leakproof least left level like limit listen load local localtime localtimestamp location lock
      mapping match maxvalue minute minvalue mode month move
      name names national natural nchar next no none not nothing notify notnull nowait null nullif nulls numeric
      object of off offset oids on only operator option options or order out outer over overlaps overlay owned owner
      parser partial partition passing password placing plans position preceding precision prepare prepared preserve primary prior privileges procedural procedure
      quote
      range read real reassign recheck recursive ref references reindex relative release rename repeatable replace replica reset restart restrict returning returns revoke right role rollback row rows rule
      savepoint schema scroll search second security select sequence sequences serializable server session session_user set setof share show similar simple smallint snapshot some stable standalone start statement statistics stdin stdout storage strict strip substring symmetric sysid system
      table tables tablespace temp template temporary text then time timestamp to trailing transaction treat trigger trim true truncate trusted type types
      unbounded uncommitted unencrypted union unique unknown unlisten unlogged until update user using
      vacuum valid validate validator value values varchar variadic varying verbose version view volatile
      when where whitespace window with without work wrapper write
      xml xmlattributes xmlconcat xmlelement xmlexists xmlforest xmlparse xmlpi xmlroot xmlserialize
      year yes
      zone
   );
   return qr/^(?:$kw)$/i;
}

#* %%

#* #define startlit()  ( yyextra->literallen = 0 )

### Perl: $p->{literalbuf} = '';  (or just clobber it with the new value)

#* static void
#* addlit(char *ytext, int yleng, core_yyscan_t yyscanner)

### Perl: $p->{literalbuf} .= $ytext;

#* static void
#* addlitchar(unsigned char ychar, core_yyscan_t yyscanner)

### Perl: $p->{literalbuf} .= $ychar;

#* static char *
#* litbufdup(core_yyscan_t yyscanner)

### Perl: $p->{literalbuf}

#* static int
#* process_integer_literal(const char *token, YYSTYPE *lval)

### This works a little differently, but achieves the same effect ###
sub process_number_literal {
   my ($p, $num) = @_;

   # if this number is bigger than a bread box, use BigInt/Float
   my $digits = $num;
   $digits =~ s/^\D|[\.e].*$//gi;
   my $is_float = ($num =~ /[\.e]/i);

   if ($perl_safe_digits >= length($digits)) {
      $num = int $num unless $is_float;
   }
   else {
      $num = $is_float ?
         Math::BigFloat->new($num) :
         Math::BigInt->new($num);
   }

   return (($is_float ? 'F' : 'I').'CONST', $num);
}

#* static unsigned int
#* hexval(unsigned char c)

### Perl: hex $c;

#* static void
#* check_unicode_value(pg_wchar c, char *loc, core_yyscan_t yyscanner)

### Perl: NOOP, since we'll assume that the "database encoding" is UTF-8

#* static bool
#* is_utf16_surrogate_first(pg_wchar c)
sub is_utf16_surrogate_first {
   my ($p, $c) = @_;
   return ($c >= 0xD800 && $c <= 0xDBFF);
}

#* static bool
#* is_utf16_surrogate_second(pg_wchar c)
sub is_utf16_surrogate_second {
   my ($p, $c) = @_;
   return ($c >= 0xDC00 && $c <= 0xDFFF);
}

#* static pg_wchar
#* surrogate_pair_to_codepoint(pg_wchar first, pg_wchar second)
sub surrogate_pair_to_codepoint {
   my ($p, $first, $second) = @_;
   return (($first & 0x3FF) << 10) + 0x10000 + ($second & 0x3FF);
}

#* static void
#* addunicode(pg_wchar c, core_yyscan_t yyscanner)

### Perl: $p->{literalbuf} .= chr $c;  (Perl is naturally Unicode compatible)

#* static char *
#* litbuf_udeescape(unsigned char escape, core_yyscan_t yyscanner)

sub udeescape {
   my ($p, $escape) = @_;
   my $new = $p->{literalbuf};

   if ($escape =~ /^[\da-f\+\'\"\s]$/i) {
      $p->YYError("Invalid Unicode escape character");
      return undef;
   }

   # Pffft... all of this C code for what boils down to a single line of Perl.
   # All praise regular expressions!
   $new =~ s/(?<!\Q${escape}\E)\Q${escape}\E(?:u([0-9A-Fa-f]{4})|U([0-9A-Fa-f]{8}))/chr hex $+/ge;

   return $new;
}

#* static unsigned char
#* unescape_single_char(unsigned char c, core_yyscan_t yyscanner)

### Perl: eval "\"\\$c\""

#* static void
#* check_string_escape_warning(unsigned char ychar, core_yyscan_t yyscanner)
sub check_string_escape_warning {
   my ($p, $c) = @_;
   if ($c eq "'") {
      if ($p->{warn_on_first_escape} && $escape_string_warning) {
         $p->ereport(WARNING,
            ERRCODE_NONSTANDARD_USE_OF_ESCAPE_CHARACTER,
            "Nonstandard use of \\' in a string literal",
            "Use '' to write quotes in strings, or use the escape string syntax (E'...').",
            $p->YYLLoc()
         );
      }
      $p->{warn_on_first_escape} = 0;   #* warn only once per string
   }
   elsif ($c eq "\\") {
      if ($p->{warn_on_first_escape} && $escape_string_warning) {
         $p->ereport(WARNING,
            ERRCODE_NONSTANDARD_USE_OF_ESCAPE_CHARACTER,
            "Nonstandard use of \\\\ in a string literal",
            "Use the escape string syntax for backslashes, e.g., E'\\\\'.",
            $p->YYLLoc()
         );
      }
      $p->{warn_on_first_escape} = 0;   #* warn only once per string
   }
   else {
      $p->check_escape_warning;
   }
}

#* static void
#* check_escape_warning(core_yyscan_t yyscanner)
sub check_escape_warning {
   my ($p) = @_;
   if ($p->{warn_on_first_escape} && $escape_string_warning) {
      $p->ereport(WARNING,
         ERRCODE_NONSTANDARD_USE_OF_ESCAPE_CHARACTER,
         "Nonstandard use of escape in a string literal",
         "Use the escape string syntax for escapes, e.g., E'\\r\\n'.",
         $p->YYLLoc()
      );
   }
   $p->{warn_on_first_escape} = 0;   #* warn only once per string
}


# Emulation of yyless(X) is done by using $-[0] (aka @LAST_MATCH_START).
sub YYLess {
   my ($p, $x) = @_;
   return (pos(${$p->input}) = $-[0] + $x);
}

# @LMS is also useful for emulating yytext.
sub YYText {
   my ($p) = @_;
   return substr(${$p->input}, $-[0], $p->YYLeng);
}

sub YYLeng {
   my ($p) = @_;
   # use pos instead of $+[0], since the pos might have changed from a previous YYLess command
   return pos(${$p->input}) - $-[0];
}

sub YYLexPrioritize {
   my ($p, $re_hash) = (shift, shift);
   my $order_array = ref $_[0] ? shift : [ @_ ];

   # Reverse the array to an index hash
   my $h = 0;
   my %order_hash = map { $_ => $h++; } @$order_array;

   my $s = $p->input;

   # Run through the REs in order, capture the matches and char lengths
   my @results;
   my $orig_pos = pos($$s);
   foreach my $rule (@$order_array) {
      my $re = $re_hash->{$rule};
      if (${$p->input} =~ /${re}/gc) {
         push @results, [ $rule => $p->YYText ];
         pos($$s) = $orig_pos;  # reset back to orig pos
      }
   }
   return undef unless scalar @results;

   # Who wins?  Sort by the standard lex prioritization: 1. char length of result, 2. rule order
   @results = sort { length $b->[1] <=> length $a->[1] || $order_hash{ $a->[0] } <=> $order_hash{ $b->[0] } } @results;

   # return rule name only; still need to run the RE again in case of other RE-based YY variables
   return $results[0]->[0];
}

# Bison's yylloc is a YYLTYPE of:
#    first_line
#    first_column
#    last_line
#    last_column
#
# We can't exactly provide that, but we can provide more
# than enough to get by with an error message.

### FIXME: This is probably too much, and might hurt speed. ###
sub YYLLoc {
   my ($p, $val, $i) = @_;
   return {
      val        => $val // $p->YYText,
      char_pos   => $-[0],
      token      => $p->YYCurtok,
      token_val  => $p->YYCurval,
      #first_line => $p->YYFirstline,  ### XXX: This looks broken in Eyapp... ###
      left_side  => $p->YYLhs,
      right_side => [ $p->YYRightside($i) ],
      rule_name  => $p->YYName,
   };
}

# Eyapp's version of YYError seems to ignore that all-important aspect of actually dying...
sub YYError {
   my ($p, $msg) = @_;
   $p->ereport(ERROR, ERRCODE_SYNTAX_ERROR, $msg, undef, $p->YYLLoc);
}

# Define our own version of "ereport", which mimics a lot of
# the variables of DBI->set_err.
### FIXME: $msg will also return sprintf vars! ###
sub ereport {
   my ($p, $err, $state, $msg, $hint, $lloc) = @_;

   ($hint, $lloc) = (undef, $hint) if (ref $hint);  # assume this is a location hash
   $hint ||= 'Obvious.';

   if (ref $lloc && %$lloc) {
      ### FIXME: Improve code when we finalize our parser method... ###
      my $s = $lloc->{val};
      $lloc->{first_line_str} = substr($s, 0, index($s, "\n"));      
   }

   my $full_msg = '';
   $full_msg .= sprintf("ERROR:   %s\n", $msg);
   $full_msg .= sprintf("ACTION:  %s\n", $hint);
   $full_msg .= sprintf("LINE %u: %s\n", $lloc->{first_line}, $lloc->{first_line_str}) if (ref $lloc && %$lloc);
   ### FIXME: Add "     ^" to show problem

   ### FIXME: Temporary until we get an idea of what gets reported in YYLLoc ###
   use Data::Dumper;
   delete $lloc->{val};
   $full_msg .= 'LLOC:    '.Data::Dumper->new([$lloc], [qw(LLOC)])->Maxdepth(3)->Indent(1)->Sortkeys(1)->Dump();

   die $full_msg;
}

42;
