package Dummy::Class;

use Moo;
use MooX::Types::MooseLike::Base qw(Str Bool ArrayRef);
use Devel::SimpleTrace;

has type => (
   is  => 'rwp',
   isa => Str,
   required => 1,
);
has names => (
   is  => 'rwp',
   isa => ArrayRef,
);
has typeOid => (
   is  => 'rwp',
   isa => Str,
);
has setof => (
   is  => 'rwp',
   isa => Bool,
);
has pct_type => (
   is  => 'rwp',
   isa => Bool,
);
has typmods => (
   is  => 'rwp',
   isa => ArrayRef,
);

package main;

use v5.10;
use Benchmark qw/cmpthese/;

$| = 1;

my $subs = {
   moo_obj => sub {
      return Dummy::Class->new(
         type     => 'asd'.rand(5000),
         names    => [1, 2, 3, 4],
         typeOid  => '.1.2.3.4',
         setof    => int(rand(2)),
         pct_type => int(rand(2)),
         typmods  => ['asd', 'foo', 'bar'],
      );
   },
   basic_obj => sub {
      return {
         CLASSTYPE => 'CLASS',
         type     => 'asd'.rand(5000),
         names    => [1, 2, 3, 4],
         typeOid  => '.1.2.3.4',
         setof    => int(rand(2)),
         pct_type => int(rand(2)),
         typmods  => ['asd', 'foo', 'bar'],
      };
   },
};

foreach my $name (keys %$subs) {
   print "$name => ";
   say $subs->{$name}->();
}
say "Testing...";

cmpthese(100000, $subs);

use v5.10;
use Benchmark qw/cmpthese/;
use SQL::Statement;

my $parser = SQL::Parser->new();
$parser->{RaiseError}=1;
$parser->{PrintError}=0;

my $sql = [
   "SELECT * FROM asd",
   "SELECT b.a, c.*, NVL(f, 2) AS fall, NVL(e,2) FROM asd b JOIN foo c WHERE 1=2 AND c=? AND c.e=? ORDER BY f DESC LIMIT 5,2",
   "SELECT pname, sname, rname
             FROM Prof p
             JOIN Subject s
               ON p.pid = s.pid",
   q{SELECT timestamper, applname, appluniq, version, nodename
     FROM APPL, PREC, NODE
     WHERE appl_type LIKE '%DB'
       AND APPL.id=PREC.appl_id
       AND PREC.node_id=NODE.id},
   "SELECT me.dbi_hostid, me.docsIfCmtsCmMac, me.docsIfCmtsCmPtr FROM docsIfCmtsMacToCmTable me  JOIN docsIfCmtsCmStatusTable status ON status.dbi_hostid = me.dbi_hostid AND status.docsIfCmtsCmStatusIndex = me.docsIfCmtsCmPtr",
];

cmpthese(2000, {
   SQL0 => sub {
      return SQL::Statement->new($sql->[0], $parser);
   },
   SQL1 => sub {
      return SQL::Statement->new($sql->[1], $parser);
   },
   SQL2 => sub {
      return SQL::Statement->new($sql->[2], $parser);
   },
   SQL3 => sub {
      return SQL::Statement->new($sql->[3], $parser);
   },
   SQL4 => sub {
      return SQL::Statement->new($sql->[4], $parser);
   },
});

