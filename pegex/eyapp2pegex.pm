package Transform::Parser::Input::Eyapp::AST;

use sanity;
use Moo;

extends 'Pegex::Receiver';

use List::AllUtils qw/first/;
 
sub got_eyapp {           $_[0]->aoh2hash($_[1]);   }
sub got_body  { { body => $_[0]->flatten ($_[1]) }; }
sub got_rule  {           $_[0]->flatten ($_[1]);   }

sub got_head  { 
   my ($self, $match) = @_;
   $match = first { ref $_ eq 'HASH' && $_->{code} } @{ $self->flatten($match) };
   return { head => $match->{code} };
}
sub got_rules {
   my ($self, $match) = @_;   
   return $self->clean_hash( $self->aoh2hash($match) );
}
sub got_percode {
   my ($self, $match) = @_;
   $match =~ s/\A\{\s+|\%\}\Z//g;
   $match =~ s/\s+\Z//;
   return { code => $match };
}

# combine AoH to single hash
sub aoh2hash {
   my ($self, $arr) = @_;
   return { map { %$_ } @$arr };
}

# delete empty keys
sub clean_hash {
   my ($self, $hash) = @_;
   foreach my $k (keys %$hash) {
      my $v = $hash->{$k};
      delete $hash->{$k} if (
         !defined $v or
         ref $v eq 'ARRAY' && not scalar @{ $_[0]->flatten   ($v) } or
         ref $v eq 'HASH'  && not scalar %{ $_[0]->clean_hash($v) }
      );
   }
   return $hash;
}

1;

package Transform::Parser::Output::Pegex;

use sanity qw(sanity autodie/io);
use Moo;
use MooX::Types::MooseLike::Base qw/HashRef Bool Str/;
use Storable qw/dclone/;
use List::AllUtils qw/first/;

no warnings 'uninitialized';

has tree => (
   is       => 'ro',
   isa      => HashRef,
   required => 1,
);
has out_class => (
   is       => 'ro',
   isa      => Str,
   required => 1,   
);
has pgx => (
   is       => 'ro',
   isa      => Str,
   required => 1,   
);
has ast => (
   is       => 'ro',
   isa      => Str,
   required => 1,   
);

has comments => (
   is      => 'ro',
   isa     => Bool,
   default => sub { 1 },
);

sub parse {
   my $self = $_[0];
   my $tree = $self->tree;  # protect the original
   
   open(my $pgx, '>', $self->pgx);
   open(my $ast, '>', $self->ast);
   
   # ::AST header
   $ast->print('package '.$self->out_class."::AST;\n\n");
   
   $ast->print("use Moo;\n\n");
   
   $ast->print("extends 'Pegex::Receiver';\n\n");

   $ast->print("## %% ##\n\n");
   
   $ast->say($tree->{head});

   $ast->print("## %% ##\n\n");
   
   # PGX header
   $pgx->print('%grammar '.$self->out_class."\n\n");

   # Pre-parse analysis of rules
   my $body = $tree->{body};
   my $rule_loc = {};
   for (my $i = 0; $i < @$body; $i++) {
      my $rule = $body->[$i];
      my ($lhs, $rhs) = @$rule{qw(lhs rhs)};
      $rule_loc->{$lhs} = {
         rule => $rule,
         loc  => $i,
      };
      
      ### Find patterns that can be optimized
      
      # 1. Optional rules (and has_code)
      for (my $j = 0; $j < @$rhs; $j++) {
         my $choice = $rhs->[$j];
         my $code = first { $_->{code} } @$choice;
         $rule->{has_code} = 1 if $code;
         
         unless (first { !$_->{comment} && !$_->{code} } @$choice) {
            $rule->{is_optional} = 1;
            $rule->{empty_rule} = splice @$rhs, $j--, 1;
         }
      }
      
      # 2. Split recursion
      if (@$rhs <= 2) {
         for (my $j = 0; $j < @$rhs; $j++) {
            my $choice = $rhs->[$j];
            my @tls = grep { $_->{token} || $_->{literal} } @$choice;
            next unless (@tls >= 2);
            my $fc = shift @tls;
            my $lc = pop   @tls;
            
            my $el;
            $el = $lc if ($fc->{token} eq $lhs);  # left side recursion
            $el = $fc if ($lc->{token} eq $lhs);  # right side recursion
            next unless $el;
            $el = $self->_rhs_convert([ $el ], $lhs);
            
            # figure out number of elements needed
            my $qual = $rule->{is_optional} ? '*' : '2+';
            if ($qual eq '2+') {
               for (my $k = 0; $k < @$rhs; $k++) {
                  my $kchoice = $rhs->[$k];
                  my @ktls = grep { $_->{token} || $_->{literal} } @$kchoice;
                  next unless (@ktls == 1);
                  my $kel = $self->_rhs_convert(\@ktls, $lhs);
                  next unless ($kel eq $el);
                  
                  $qual = '+';
                  splice @$rhs, $k--, 1;
               }
            }
            
            my @ccs = grep { !$_->{token} && !$_->{literal} } @$choice;
            my $re = @tls ? '/ ~ '.$self->_rhs_convert(\@tls, $lhs).' ~ /' : '~';
            @$choice = (
               { pegex => ($qual eq '2+' && $el !~ /\</ ? "<$el>$qual" : "$el$qual")." % $re" },
               @ccs
            );
            delete $rule->{is_optional};
         }
      }
      
      # 3. Other recursion
      for (my $j = 0; $j < @$rhs; $j++) {
         my $choice = $rhs->[$j];
         if (first { $_->{token} eq $lhs } @$choice) {
            push @{$rule->{comments}}, "/*## WARNING: Recursion; needs refactoring! ### */";
            last;
         }
      }
   }
   
   # Parse rules
   ### FIXME: Make use of empty_rule ###
   foreach my $rule (@$body) {
      $pgx->say( join("\n\n", map { $self->_comment_convert($_) } @{$rule->{comments}}) )
         if ( $self->comments && $rule->{comments} );
      
      my $lhs = $rule->{lhs};
      my $rhs = $rule->{rhs};
      my ($choices, $subrules) = ([], []);

      foreach my $choice (@$rhs) {
         my $has_code = first { $_->{code} } @$choice;
         
         # anything with code will need a separate rule for reference
         if ($has_code && @$rhs > 1) {
            push @$subrules, $choice;
            push @$choices, $lhs.'_'.scalar(@$subrules);
         }
         else {
            my $new_choice = $self->_rhs_code_separate($choice, $lhs, $ast);
            push @$choices, $self->_rhs_convert($new_choice, $lhs);
            
         }
      }
      
      my $o = $rule->{is_optional};
      my $new_lhs = $lhs.(@$subrules ? '  ' : '').(@$subrules >= 10 ? ' ' : '');
      
      if (@$choices == 1) {
         if ($o && $choices->[0] =~ /\s+/)
              { $pgx->say( "$new_lhs: ( ".$choices->[0].' )?' ); }
         else { $pgx->say( "$new_lhs: ".$choices->[0].($o ? '?' : '') ); }
      }
      else {
         $pgx->print( "$new_lhs:".($o ? ' (' : '')."\n     " );
         $pgx->say( join "\n   | ",  @$choices );
         $pgx->say($o ? ')?' : ';');
      }
      
      for (my $sr = 0; $sr < @$subrules; $sr++) {
         my $subrule = $subrules->[$sr];
         my $sublhs  = $lhs.'_'.($sr+1).($sr < 9 && @$subrules >= 10 ? ' ' : '');
         my $choice = $self->_rhs_code_separate($subrule, $sublhs, $ast);
         $pgx->say( "$sublhs: ".$self->_rhs_convert($choice, $lhs) );
      }
      $pgx->say();
      $ast->say() if $rule->{any_code};
   }
   
   $ast->print("## %% ##\n\n");
   
   $ast->say($tree->{tail});
   
   $pgx->close;
   $ast->close;
}

sub _comment_convert {
   my ($self, $comment) = @_;
   
   $comment =~ s/\A\/\*/\#/;         # comment starter
   $comment =~ s/\*\/\Z//;           # comment ender
   $comment =~ s/^(?:\s*\*)?/\#/gm;  # beginning of lines
   $comment =~ s/\A\#\#/\#/;         # remove dupe # from previous match
   
   # remove dangling #
   $comment =~ s/\A\#\s*\n//;
   $comment =~ s/^\#\s*\Z//m;
   $comment =~ s/\s+\Z//;
   
   # fix * box art
   $comment =~ s/(\*+)$/'#' x length($1)/gem;
   $comment =~ s/(?<=\#{5})\Z/\#/;
   
   return $comment;
}

sub _code_convert {
   my ($self, $code) = @_;
   
   my ($sp) = ($code =~ /\n(\s+)\}\z/);
   $code =~ s/^$sp//gm if $sp;
   
   return $code;
}

# reverse the atoms
use Pegex::Grammar::Atoms;
my $atoms = Pegex::Grammar::Atoms->atoms;
my $literals = { map {
   my $k = $atoms->{$_}; $k =~ s/\\(\W)/$1/g;
   $k => '<C_'.$_.'>';
} keys %$atoms };

sub _rhs_convert {
   my ($self, $choice, $lhs) = @_;
   
   my @items;
   my ($has_recursion, $had_comment) = (0, 0);
   foreach my $item (@$choice) {
      my ($type, $val) = %$item;
      
      if ($had_comment) {
         my $comment = pop @items;
         $comment .= "\n   ";
         push @items, $comment;
         $had_comment = 0;
      }
      
      for ($type) {
         when ('token')   { push @items, $val; $has_recursion = 1 if $val eq $lhs; }
         when ('pegex')   { push @items, $val; }
         when ('literal') { push @items, " ~ ".($literals->{$val} || die "$lhs: $val?  No such literal!")." ~ "; }
         when ('comment') { push @items, "\n".$self->_comment_convert($val); $had_comment = 1; }
         default          { die "$lhs: $type?  What is this?  I don't even..."; }
      }
   }
   
   return join ' ', @items;
}

sub _rhs_code_separate {
   my ($self, $choice, $lhs, $ast) = @_;
   $choice = dclone $choice;

   my $code    = first { $_->{code} } @$choice;
   $choice     = [ grep { !$_->{code} } @$choice ];
   my $comment = pop @$choice if ($choice->[$#$choice]->{comment} && $code);
   
   if ($code) {
      $ast->say( $self->_comment_convert($comment->{comment}) ) if $comment;
      $ast->say( "### $lhs: ".$self->_rhs_convert($choice, $lhs) );
      $ast->say( "sub got_$lhs ".$self->_code_convert($code->{code}) );
   }
   
   return $choice;
}

1;
