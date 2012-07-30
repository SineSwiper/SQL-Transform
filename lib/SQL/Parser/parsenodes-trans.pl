use v5.10;

s/\/\*/\#\*/g and $in_comment = 1;
$in_comment and s/^(\s+)\*/substr($1, 1).'#*'/e;
s/\*\///g     and $in_comment = 0;

/^\s*\#\*?\s*$/ and $_ = '';

if ($in_struct) {
   s/^[ ]{4}\#\*/   #*/;
   s/^[ ]{32}\#\*/(' ' x 49).'#*'/e;

   /^\{|^\s+NodeTag\s+type\;/ and $_ = '';
   s/(?:const|struct) //g;
   /^\s+(\w+)\s+\*?(\w+)\;\s*(\#\* .+)?$/ and do {
      my ($t, $n, $c) = (ucfirst($1), $2, $3);
      state $type_trans = {qw(
         List     ArrayRef
         Oid      UInt
         Bits8    UInt8
         Bits16   UInt16
         Bits32   UInt32
         Node     Any
         Char     Str
      )};
      $t = 'HashRef' if $n eq 'location';
      $t =~ s/^uint/UInt/i;
      $t = $type_trans->{$t} || $t;
      $c = '  '.$c if $c;
      
      $_ = sprintf("   %-20s => [ %-15s ],%s\n", $n, $t, $c);
   };
   s/^\} (\w+)\;/\}\;/ and $in_struct = 0;
}
else {
   s/^typedef struct (\w+)/typedef_struct '$1' \{/ and $in_struct = 1;
   $in_struct and s/Stmt'/'/;
}

if ($in_enum) {
   /^\{/ and $_ = '';
   s/^\s+(\w+(?:\s*=\s*[^,\r\n\#]+[^,\s\#])?)(,)?/   \'$1\'$2/;
   s/^\} (\w+)\;/\] 0\;/ and $in_enum = 0;
}
else {
   s/^typedef enum (\w+)/typedef_enum '$1' \[/ and $in_enum = 1;
}
