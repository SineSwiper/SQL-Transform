use v5.10;
use Benchmark qw/cmpthese/;

$| = 1;

my $space       = qr([\s\t\n\r\f]);
my $horiz_space = qr([\s\t\f]);
my $newline     = qr([\n\r]);
my $non_newline = qr([^\n\r]);

my $comment     = qr(--${non_newline}*);

my $string = "This is a longer string with a bunch of\nwhitespace here and there\n  -- This is a comment\nBack to regular stuff.\n\n  \n \n\t\nHere.\n";
$string .= $string for (1..5);

my $whitespace1 = qr/${space}+|${comment}/;
my $whitespace2 = qr/[ \t\n\r\f]+|--[^\n\r]*/;
my $whitespace3 = qr/\G(?-xism:[ \t\n\r\f]+|--[^\n\r]*)/;
my $whitespace4 = "(?-xism:${space}+|${comment})";
my $whitespace5 = '(?-xism:[ \t\n\r\f]+|--[^\n\r]*)';
my $whitespace6 = '\G(?-xism:[ \t\n\r\f]+|--[^\n\r]*)';

print "
   multi_qr   => $whitespace1
   dual_qr    => $whitespace2
   single_qr  => $whitespace3
   multi_str  => $whitespace4
   dual_str   => $whitespace5
   single_str => $whitespace6
";

my $subs = {
   multi_qr => sub {
      my $count = 0;
      pos($string) = 0;
      LOOP: {
         $count++, redo LOOP if $string =~ /\G$whitespace1/gc;
         redo LOOP           if $string =~ /\G\S+/gc;
      }
      return $count;
   },
   dual_qr => sub {
      my $count = 0;
      pos($string) = 0;
      LOOP: {
         $count++, redo LOOP if $string =~ /\G$whitespace2/gc;
         redo LOOP           if $string =~ /\G\S+/gc;
      }
      return $count;
   },
   single_qr => sub {
      my $count = 0;
      pos($string) = 0;
      LOOP: {
         $count++, redo LOOP if $string =~ /$whitespace3/gc;
         redo LOOP           if $string =~ /\G\S+/gc;
      }
      return $count;
   },
   multi_str => sub {
      my $count = 0;
      pos($string) = 0;
      LOOP: {
         $count++, redo LOOP if $string =~ /\G$whitespace4/gc;
         redo LOOP           if $string =~ /\G\S+/gc;
      }
      return $count;
   },
   dual_str => sub {
      my $count = 0;
      pos($string) = 0;
      LOOP: {
         $count++, redo LOOP if $string =~ /\G$whitespace5/gc;
         redo LOOP           if $string =~ /\G\S+/gc;
      }
      return $count;
   },
   single_str => sub {
      my $count = 0;
      pos($string) = 0;
      LOOP: {
         $count++, redo LOOP if $string =~ /$whitespace6/gc;
         redo LOOP           if $string =~ /\G\S+/gc;
      }
      return $count;
   },
   single_real => sub {
      my $count = 0;
      pos($string) = 0;
      LOOP: {
         $count++, redo LOOP if $string =~ /\G(?-xism:[ \t\n\r\f]+|--[^\n\r]*)/gc;
         redo LOOP           if $string =~ /\G\S+/gc;
      }
      return $count;
   },
   single_real_nomod => sub {
      my $count = 0;
      pos($string) = 0;
      LOOP: {
         $count++, redo LOOP if $string =~ /\G(?:[ \t\n\r\f]+|--[^\n\r]*)/gc;
         redo LOOP           if $string =~ /\G\S+/gc;
      }
      return $count;
   },
   single_real_noqm => sub {
      my $count = 0;
      pos($string) = 0;
      LOOP: {
         $count++, redo LOOP if $string =~ /\G([ \t\n\r\f]+|--[^\n\r]*)/gc;
         redo LOOP           if $string =~ /\G\S+/gc;
      }
      return $count;
   },
};

foreach my $name (keys %$subs) {
   print "$name => ";
   say $subs->{$name}->();
}
say "Testing...";

cmpthese(1000, $subs);

# Now testing trimming the string
my $uescapefail = "(?i:-|UESCAPE${whitespace}*-|UESCAPE${whitespace}*${quote}[^']|UESCAPE${whitespace}*${quote}|UESCAPE${whitespace}*|UE?S?C?A?P?)";

$subs = {
   trimmed => sub {
      my ($count, $pos) = (0, 0);
      my $s = $string;
      LOOP: {
         $s = substr($s, pos($s)||0);
         $count++, redo LOOP if $s =~ /$whitespace6/gc;
         
         for (1.100) { $s =~ /\G$uescapefail/gc; }
         
         redo LOOP           if $s =~ /\G\S+/gc;
      }
      return $count;
   },
   no_trim => sub {
      my ($count, $pos) = (0, 0);
      my $s = $string;
      LOOP: {
         $pos = pos($s);
         $s = $string;
         pos($s) = $pos;
         
         $count++, redo LOOP if $s =~ /$whitespace6/gc;
         
         for (1.100) { $s =~ /\G$uescapefail/gc; }
         
         redo LOOP           if $s =~ /\G\S+/gc;
      }
      return $count;
   },
};

foreach my $name (keys %$subs) {
   print "$name => ";
   say $subs->{$name}->();
}
say "Testing...";

cmpthese(1000, $subs);
