package SQL::Converter::SPIF::_classes;

# Based and converted from PostgreSQL's src/include/nodes/*nodes.h

# Most comments retained from those sources, marked as #*.

  ###########################################################################
### Instead of C++'s typedef structs, we will use packages with Moo support 
### to fill in the properties.  Here are the rules:
###
### * Until we get a better idea of what we are dealing with, no 'required'
###   properties.
### * (is => 'rw'), since SQL::Translator works the same way.  However,
###   'location' is 'rwp', since there's really no reason to change that one.
### * Remove the first Nodetag->type property, since that's redundant with
###   the class name.
### 
  ###########################################################################

# Okay, this looks really weird, but it makes translating these typedefs
# to Moo classes a LOT easier...

use Moo;
use MooX::Types::MooseLike::Base  qw(Any Str Bool ArrayRef HashRef Object Value Maybe);
use MooX::Types::CLike            qw(:c :stdint);
use Sub::Quote 'quote_sub';
use Scalar::Util 'blessed';
use Import::Into;
use Devel::SimpleTrace;

BEGIN { require parent; }

sub typedef_struct ($$) {
   my ($subpackage, $type_hash) = @_;
   my $class_type = $subpackage;
   printf "typedef_struct %s\n", $class_type;
   $class_type =~ s/:://g;

   # New class type definition
   no warnings 'uninitialized';
   my $full_package = "SQL::Converter::SPIF::$subpackage";
   MooX::Types::MooseLike::register_types([{
      name       => $class_type,
      test       => sub { !defined($_[0]) or ( blessed($_[0]) and $_[0]->isa($full_package) ); },
      message    => sub { "Object is not a $full_package class!" },
   }], __PACKAGE__)
      unless (eval '$'.__PACKAGE__.'::{$class_type}');
   
   # package scoping
   Moo->import::into($full_package);
   
   # special subclassing for Expr
   eval "package $full_package; extends 'SQL::Converter::PIL::Expr';" if (delete $type_hash->{xpr});
   
   # small sub to remove any undef values from new()
   quote_sub $full_package.'::BUILDARGS' => q{
      my $class = shift;
      my $args  = $class->next::method(@_);  # let Moo's default BUILDARGS handle hashification
      
      foreach my $k (keys %$args) {
         delete $args->{$k} unless defined $args->{$k};
         delete $args->{$k} if (ref $args->{$k} eq 'ARRAY' and !@{ $args->{$k} });
         delete $args->{$k} if (ref $args->{$k} eq 'HASH'  and !%{ $args->{$k} });
      }
      use Data::Dump;
   }."my \$class_type = '$class_type';\n".q{
      dd { $class_type, $args };
      return $args;
   };
   
   foreach my $name (sort keys %$type_hash) {
      my ($isa, $default, $required, $weak) = @{$type_hash->{$name}};
      my @has_params;
      
      push @has_params, is => ($name eq 'location') ? 'rwp' : 'rw';
      
      # Because we're passing types hashrefs instead of named subs, we
      # can get away with not using/exporting the MooX::Types stuff.
      push @has_params, isa      => Maybe[$isa] if $isa;
      push @has_params, default  => $default if $default;
      push @has_params, required => 1        if $required;
      push @has_params, weak_ref => 1        if $weak;
      
      eval "package $full_package;".' has $name => @has_params;';
   };
   #use Data::Dump; eval 'dd \%'.$full_package.'::;';
}

### In order to add some level of readability into the result trees, we 
### try to use real strings as much as possible for enums, ie: the parser
### isn't using them for bitwise math.

### Of course, if it can't be found in either the lexer/parser, we just
### remove it.

use enum::hash 'enum';

BEGIN {
   # Basic starter list
   my $CONSTANT_LIST = {
      NULL  => undef,
      NIL   => undef,
      TRUE  => 1,
      FALSE => 0,
      
      InvalidOid         => 0,
      DEFAULT_INDEX_TYPE => 'btree',
   };

   sub typedef_enum ($$;$) {
      my ($name, $enum_array, $is_bitmask) = @_;
      printf "typedef_enum %s\n", $name;

      # New class type definition
      if ($name) {
         MooX::Types::MooseLike::register_types([{
            name       => $name,
            subtype_of => $is_bitmask ? 'Int' : 'Str',
            from       => 'MooX::Types::MooseLike::Base',
            test       => sub { 1 },
            message    => sub { 1 },
         }], __PACKAGE__);
      }

      # real strings
      unless ($is_bitmask) {
         my @names = map { (split /\s+/, $_, 2)[0] } @$enum_array;
         
         # find LCP
         my $prefix = longest_common_prefix(@names);
         
         foreach my $n (@names) {
            my $val = $n;
            $val =~ s/^\Q$prefix\E//;
            printf "   %s = %s\n", $n, $val;
            $CONSTANT_LIST->{$n} = $val;
         }
      }
      else {
         unshift @$enum_array, '~';  # tell enum::hash to use a bitmask
         my %enum = enum @$enum_array;
         $CONSTANT_LIST->{$_} = $enum{$_} for (keys %enum);
         printf "   %s = %s\n", $_, $enum{$_} for (keys %enum);
      }
   }

   sub longest_common_prefix {
      my $prefix = shift;
      for (@_) {
         chop $prefix until (/^\Q$prefix\E/);
      }
      return $prefix;
   }

   # 'our' just acts really weird here, so we need this
   sub CONSTANTS {
      return $CONSTANT_LIST;
   }
}

#############################################################################

### Each thing is a BEGIN block so that definitions are executed as 
### they are parsed.  This makes it possible to refer to the struct types after
### they are built.

### (We take some liberties and bundle some of the blocks, though...)

BEGIN {
   ### This is a variation of the INTERVAL_MASK(XXXX) thing found in gram.y
   ### This is somewhat defined in datetime.h, but we define it differently.
   typedef_enum '', [
      'INTERVAL_MASK_SECOND=1',
      'INTERVAL_MASK_MINUTE',    
      'INTERVAL_MASK_HOUR',      
      'INTERVAL_MASK_DAY',
      'INTERVAL_MASK_MONTH',
      'INTERVAL_MASK_YEAR',
      'INTERVAL_FULL_RANGE=0x7FFF',
   ], 1;
   
   ### Defined in pg_class.h ###
   typedef_enum '', [
      'RELPERSISTENCE_PERMANENT = "p"',     #* regular table
      'RELPERSISTENCE_UNLOGGED  = "u"',     #* unlogged permanent table
      'RELPERSISTENCE_TEMP      = "t"',     #* temporary table
   ], 1;
   
   ### This is directly in gram.y for some reason... ###
   #* ConstraintAttributeSpec yields an integer bitmask of these flags:
   typedef_enum '', [
      'CAS_NOT_DEFERRABLE=1',
      'CAS_DEFERRABLE',
      'CAS_INITIALLY_IMMEDIATE',
      'CAS_INITIALLY_DEFERRED',
      'CAS_NOT_VALID',
      'CAS_NO_INHERIT',
   ], 1;

}

#*-------------------------------------------------------------------------
#* nodes.h
#*    Definitions for tagged nodes.
#* Portions Copyright (c) 1996-2012, PostgreSQL Global Development Group
#* Portions Copyright (c) 1994, Regents of the University of California
#* src/include/nodes/nodes.h
#*-------------------------------------------------------------------------

### We don't use much from this file...

BEGIN {
   #* CmdType -
   #*     enums for type of operation represented by a Query or PlannedStmt
   #* This is needed in both parsenodes.h and plannodes.h, so put it here...
   typedef_enum 'CmdType', [
      'CMD_UNKNOWN',
      'CMD_SELECT',      #* select stmt 
      'CMD_UPDATE',      #* update stmt 
      'CMD_INSERT',      #* insert stmt 
      'CMD_DELETE',
      'CMD_UTILITY',     #* cmds like create, destroy, copy, vacuum,
                         #* etc. 
      'CMD_NOTHING'      #* dummy command for instead nothing rules
                         #* with qual 
   ], 0;


   #* JoinType -
   #*     enums for types of relation joins
   #* JoinType determines the exact semantics of joining two relations using
   #* a matching qualification.  For example, it tells what to do with a tuple
   #* that has no match in the other relation.
   #* This is needed in both parsenodes.h and plannodes.h, so put it here...
   typedef_enum 'JoinType', [
      #* The canonical kinds of joins according to the SQL JOIN syntax. Only
      #* these codes can appear in parser output (e.g., JoinExpr nodes).
      'JOIN_INNER',               #* matching tuple pairs only 
      'JOIN_LEFT',                #* pairs + unmatched LHS tuples 
      'JOIN_FULL',                #* pairs + unmatched LHS + unmatched RHS 
      'JOIN_RIGHT',               #* pairs + unmatched RHS tuples 

      #* Semijoins and anti-semijoins (as defined in relational theory) do not
      #* appear in the SQL JOIN syntax, but there are standard idioms for
      #* representing them (e.g., using EXISTS).   The planner recognizes these
      #* cases and converts them to joins.  So the planner and executor must
      #* support these codes.  NOTE: in JOIN_SEMI output, it is unspecified
      #* which matching RHS row is joined to.  In JOIN_ANTI output, the row is
      #* guaranteed to be null-extended.
      'JOIN_SEMI',                #* 1 copy of each LHS row that has match(es) 
      'JOIN_ANTI',                #* 1 copy of each LHS row that has no match 

      #* These codes are used internally in the planner, but are not supported
      #* by the executor (nor, indeed, by most of the planner).
      'JOIN_UNIQUE_OUTER',        #* LHS path must be made unique 
      'JOIN_UNIQUE_INNER'         #* RHS path must be made unique 

      #* We might need additional join types someday.
   ], 0;
}

#endif   #* NODES_H

#*-------------------------------------------------------------------------
#* primnodes.h
#*    Definitions for "primitive" node types, those that are used in more
#*    than one of the parse/plan/execute stages of the query pipeline.
#*    Currently, these are mostly nodes for executable expressions
#*    and join trees.
#* Portions Copyright (c) 1996-2012, PostgreSQL Global Development Group
#* Portions Copyright (c) 1994, Regents of the University of California
#* src/include/nodes/primnodes.h
#*-------------------------------------------------------------------------

BEGIN {
   #* Alias -
   #*    specifies an alias for a range variable; the alias might also
   #*    specify renaming of columns within the table.
   #* Note: colnames is a list of Value nodes (always strings).  In Alias structs
   #* associated with RTEs, there may be entries corresponding to dropped
   #* columns; these are normally empty strings ("").  See parsenodes.h for info.
   typedef_struct 'Alias', {
      aliasname            => [ Str             ],  #* aliased rel name (never qualified) 
      colnames             => [ ArrayRef        ],  #* optional list of column aliases 
   };

   typedef_enum 'InhOption', [
      'INH_NO',                     #* Do NOT scan child tables 
      'INH_YES',                    #* DO scan child tables 
      'INH_DEFAULT'                 #* Use current SQL_inheritance option 
   ], 0;

   #* What to do at commit time for temporary relations 
   typedef_enum 'OnCommitAction', [
      'ONCOMMIT_NOOP',              #* No ON COMMIT clause (do nothing) 
      'ONCOMMIT_PRESERVE_ROWS',     #* ON COMMIT PRESERVE ROWS (do nothing) 
      'ONCOMMIT_DELETE_ROWS',       #* ON COMMIT DELETE ROWS 
      'ONCOMMIT_DROP'               #* ON COMMIT DROP 
   ], 0;
}
BEGIN {
   #* RangeVar - range variable, used in FROM clauses
   #* Also used to represent table names in utility statements; there, the alias
   #* field is not used, and inhOpt shows whether to apply the operation
   #* recursively to child tables.  In some contexts it is also useful to carry
   #* a TEMP table indication here.
   typedef_struct 'RangeVar', {
      catalogname          => [ Str             ],  #* the catalog (database) name, or NULL 
      schemaname           => [ Str             ],  #* the schema name, or NULL 
      relname              => [ Str             ],  #* the relation/sequence name 
      inhOpt               => [ InhOption       ],  #* expand rel by inheritance? recursively act
                                                    #* on children? 
      relpersistence       => [ Str             ],  #* see RELPERSISTENCE_* in pg_class.h 
      alias                => [ Alias           ],  #* table alias & optional column aliases 
      location             => [ HashRef         ],  #* token location, or -1 if unknown 
   };
}
BEGIN {
   #* IntoClause - target information for SELECT INTO and CREATE TABLE AS
   typedef_struct 'IntoClause', {
      rel                  => [ RangeVar        ],  #* target relation name 
      colNames             => [ ArrayRef        ],  #* column names to assign, or NIL 
      options              => [ ArrayRef        ],  #* options from WITH clause 
      onCommit             => [ OnCommitAction  ],  #* what do we do at COMMIT? 
      tableSpaceName       => [ Str             ],  #* table space to use, or NULL 
      skipData             => [ Bool            ],  #* true for WITH NO DATA 
   };

   #* ----------------------------------------------------------------
   #*                  node types for executable expressions
   #* ----------------------------------------------------------------

   #* Expr - generic superclass for executable-expression nodes
   #* All node types that are used in executable expression trees should derive
   #* from Expr (that is, have Expr as their first field).  Since Expr only
   #* contains NodeTag, this is a formality, but it is an easy form of
   #* documentation.  See also the ExprState node types in execnodes.h.
   typedef_struct 'Expr', {
   };
   ### The subclassing is handled in typedef_struct ###
}
BEGIN {
   ### RIP: Var          ###
   ### RIP: Const        ###
   ### RIP: Param        ###
   ### RIP: Aggref       ###
   ### RIP: WindowFunc   ###
   ### RIP: FuncExpr     ###
   ### RIP: OpExpr       ###
   ### RIP: DistinctExpr ###
   ### RIP: NullIfExpr   ###
   ### RIP: ScalarArrayOpExpr ###
   ### RIP: BoolExpr     ###

   #* CoercionContext - distinguishes the allowed set of type casts
   #* NB: ordering of the alternatives is significant; later (larger) values
   #* allow more casts than earlier ones.
   typedef_enum 'CoercionContext', [
      'COERCION_IMPLICIT',          #* coercion in context of expression 
      'COERCION_ASSIGNMENT',        #* coercion in context of assignment 
      'COERCION_EXPLICIT'           #* explicit cast operation 
   ], 0;

   #* CoercionForm - information showing how to display a function-call node
   typedef_enum 'CoercionForm', [
      'COERCE_EXPLICIT_CALL',       #* display as a function call 
      'COERCE_EXPLICIT_CAST',       #* display as an explicit cast 
      'COERCE_IMPLICIT_CAST',       #* implicit cast, so hide it 
      'COERCE_DONTCARE'             #* special case for planner 
   ], 0;
}
BEGIN {
   #* NamedArgExpr - a named argument of a function
   #* This node type can only appear in the args list of a FuncCall or FuncExpr
   #* node.  We support pure positional call notation (no named arguments),
   #* named notation (all arguments are named), and mixed notation (unnamed
   #* arguments followed by named ones).
   #* Parse analysis sets argnumber to the positional index of the argument,
   #* but doesn't rearrange the argument list.
   #* The planner will convert argument lists to pure positional notation
   #* during expression preprocessing, so execution never sees a NamedArgExpr.
   typedef_struct 'NamedArgExpr', {
      xpr                  => [ Expr            ],
      arg                  => [ Expr            ],  #* the argument expression 
      name                 => [ Str             ],  #* the name 
      argnumber            => [ Int             ],  #* argument's number in positional notation 
      location             => [ HashRef         ],  #* argument name location, or -1 if unknown 
   };

   #* SubLink
   #* A SubLink represents a subselect appearing in an expression, and in some
   #* cases also the combining operator(s) just above it.  The subLinkType
   #* indicates the form of the expression represented:
   #*  EXISTS_SUBLINK      EXISTS(SELECT ...)
   #*  ALL_SUBLINK         (lefthand) op ALL (SELECT ...)
   #*  ANY_SUBLINK         (lefthand) op ANY (SELECT ...)
   #*  ROWCOMPARE_SUBLINK  (lefthand) op (SELECT ...)
   #*  EXPR_SUBLINK        (SELECT with single targetlist item ...)
   #*  ARRAY_SUBLINK       ARRAY(SELECT with single targetlist item ...)
   #*  CTE_SUBLINK         WITH query (never actually part of an expression)
   #* For ALL, ANY, and ROWCOMPARE, the lefthand is a list of expressions of the
   #* same length as the subselect's targetlist.  ROWCOMPARE will *always* have
   #* a list with more than one entry; if the subselect has just one target
   #* then the parser will create an EXPR_SUBLINK instead (and any operator
   #* above the subselect will be represented separately).  Note that both
   #* ROWCOMPARE and EXPR require the subselect to deliver only one row.
   #* ALL, ANY, and ROWCOMPARE require the combining operators to deliver boolean
   #* results.  ALL and ANY combine the per-row results using AND and OR
   #* semantics respectively.
   #* ARRAY requires just one target column, and creates an array of the target
   #* column's type using any number of rows resulting from the subselect.
   #* SubLink is classed as an Expr node, but it is not actually executable;
   #* it must be replaced in the expression tree by a SubPlan node during
   #* planning.
   #* NOTE: in the raw output of gram.y, testexpr contains just the raw form
   #* of the lefthand expression (if any), and operName is the String name of
   #* the combining operator.  Also, subselect is a raw parsetree.  During parse
   #* analysis, the parser transforms testexpr into a complete boolean expression
   #* that compares the lefthand value(s) to PARAM_SUBLINK nodes representing the
   #* output columns of the subselect.  And subselect is transformed to a Query.
   #* This is the representation seen in saved rules and in the rewriter.
   #* In EXISTS, EXPR, and ARRAY SubLinks, testexpr and operName are unused and
   #* are always null.
   #* The CTE_SUBLINK case never occurs in actual SubLink nodes, but it is used
   #* in SubPlans generated for WITH subqueries.
   typedef_enum 'SubLinkType', [
      'EXISTS_SUBLINK',
      'ALL_SUBLINK',
      'ANY_SUBLINK',
      'ROWCOMPARE_SUBLINK',
      'EXPR_SUBLINK',
      'ARRAY_SUBLINK',
      'CTE_SUBLINK'                 #* for SubPlans only 
   ], 0;
}
BEGIN {
   typedef_struct 'SubLink', {
      xpr                  => [ Expr            ],
      subLinkType          => [ SubLinkType     ],  #* see above 
      testexpr             => [ Any             ],  #* outer-query test for ALL/ANY/ROWCOMPARE 
      operName             => [ ArrayRef        ],  #* originally specified operator name 
      subselect            => [ Any             ],  #* subselect as Query* or parsetree 
      location             => [ HashRef         ],  #* token location, or -1 if unknown 
   };

   ### RIP: SubPlan ###
   ### RIP: AlternativeSubPlan ###
   ### RIP: FieldSelect ###
   ### RIP: FieldStore ###
   ### RIP: RelabelType ###
   ### RIP: CoerceViaIO ###
   ### RIP: ArrayCoerceExpr ###
   ### RIP: ConvertRowtypeExpr ###
   ### RIP: CollateExpr ###

   #*----------
   #* CaseExpr - a CASE expression
   #* We support two distinct forms of CASE expression:
   #*      CASE WHEN boolexpr THEN expr [ WHEN boolexpr THEN expr ... ]
   #*      CASE testexpr WHEN compexpr THEN expr [ WHEN compexpr THEN expr ... ]
   #* These are distinguishable by the "arg" field being NULL in the first case
   #* and the testexpr in the second case.
   #* In the raw grammar output for the second form, the condition expressions
   #* of the WHEN clauses are just the comparison values.  Parse analysis
   #* converts these to valid boolean expressions of the form
   #*      CaseTestExpr '=' compexpr
   #* where the CaseTestExpr node is a placeholder that emits the correct
   #* value at runtime.  This structure is used so that the testexpr need be
   #* evaluated only once.  Note that after parse analysis, the condition
   #* expressions always yield boolean.
   #* Note: we can test whether a CaseExpr has been through parse analysis
   #* yet by checking whether casetype is InvalidOid or not.
   #*----------
   typedef_struct 'CaseExpr', {
      xpr                  => [ Expr            ],
      casetype             => [ UInt            ],  #* type of expression result 
      casecollid           => [ UInt            ],  #* OID of collation, or InvalidOid if none 
      arg                  => [ Expr            ],  #* implicit equality comparison argument 
      args                 => [ ArrayRef        ],  #* the arguments (list of WHEN clauses) 
      defresult            => [ Expr            ],  #* the default result (ELSE clause) 
      location             => [ HashRef         ],  #* token location, or -1 if unknown 
   };

   #* CaseWhen - one arm of a CASE expression
   typedef_struct 'CaseWhen', {
      xpr                  => [ Expr            ],
      expr                 => [ Expr            ],  #* condition expression 
      result               => [ Expr            ],  #* substitution result 
      location             => [ HashRef         ],  #* token location, or -1 if unknown 
   };

   ### RIP: CaseTestExpr ###
   ### RIP: ArrayExpr ###

   #* RowExpr - a ROW() expression
   #* Note: the list of fields must have a one-for-one correspondence with
   #* physical fields of the associated rowtype, although it is okay for it
   #* to be shorter than the rowtype.  That is, the N'th list element must
   #* match up with the N'th physical field.  When the N'th physical field
   #* is a dropped column (attisdropped) then the N'th list element can just
   #* be a NULL constant.  (This case can only occur for named composite types,
   #* not RECORD types, since those are built from the RowExpr itself rather
   #* than vice versa.)  It is important not to assume that length(args) is
   #* the same as the number of columns logically present in the rowtype.
   #* colnames provides field names in cases where the names can't easily be
   #* obtained otherwise.  Names *must* be provided if row_typeid is RECORDOID.
   #* If row_typeid identifies a known composite type, colnames can be NIL to
   #* indicate the type's cataloged field names apply.  Note that colnames can
   #* be non-NIL even for a composite type, and typically is when the RowExpr
   #* was created by expanding a whole-row Var.  This is so that we can retain
   #* the column alias names of the RTE that the Var referenced (which would
   #* otherwise be very difficult to extract from the parsetree).  Like the
   #* args list, colnames is one-for-one with physical fields of the rowtype.
   typedef_struct 'RowExpr', {
      xpr                  => [ Expr            ],
      args                 => [ ArrayRef        ],  #* the fields 
      row_typeid           => [ UInt            ],  #* RECORDOID or a composite type's ID 

      #* Note: we deliberately do NOT store a typmod.  Although a typmod will be
      #* associated with specific RECORD types at runtime, it will differ for
      #* different backends, and so cannot safely be stored in stored
      #* parsetrees.  We must assume typmod -1 for a RowExpr node.
      #* We don't need to store a collation either.  The result type is
      #* necessarily composite, and composite types never have a collation.
      row_format           => [ CoercionForm    ],  #* how to display this node 
      colnames             => [ ArrayRef        ],  #* list of String, or NIL 
      location             => [ HashRef         ],  #* token location, or -1 if unknown 
   };

   ### RIP: RowCompareExpr ###
   
   #* CoalesceExpr - a COALESCE expression
   typedef_struct 'CoalesceExpr', {
      xpr                  => [ Expr            ],
      coalescetype         => [ UInt            ],  #* type of expression result 
      coalescecollid       => [ UInt            ],  #* OID of collation, or InvalidOid if none 
      args                 => [ ArrayRef        ],  #* the arguments 
      location             => [ HashRef         ],  #* token location, or -1 if unknown 
   };

   #* MinMaxExpr - a GREATEST or LEAST function
   typedef_enum 'MinMaxOp', [
      'IS_GREATEST',
      'IS_LEAST'
   ], 0;
}
BEGIN {
   typedef_struct 'MinMaxExpr', {
      xpr                  => [ Expr            ],
      minmaxtype           => [ UInt            ],  #* common type of arguments and result 
      minmaxcollid         => [ UInt            ],  #* OID of collation of result 
      inputcollid          => [ UInt            ],  #* OID of collation that function should use 
      op                   => [ MinMaxOp        ],  #* function to execute 
      args                 => [ ArrayRef        ],  #* the arguments 
      location             => [ HashRef         ],  #* token location, or -1 if unknown 
   };

   #* XmlExpr - various SQL/XML functions requiring special grammar productions
   #* 'name' carries the "NAME foo" argument (already XML-escaped).
   #* 'named_args' and 'arg_names' represent an xml_attribute list.
   #* 'args' carries all other arguments.
   #* Note: result type/typmod/collation are not stored, but can be deduced
   #* from the XmlExprOp.  The type/typmod fields are just used for display
   #* purposes, and are NOT the true result type of the node.
   typedef_enum 'XmlExprOp', [
      'IS_XMLCONCAT',               #* XMLCONCAT(args) 
      'IS_XMLELEMENT',              #* XMLELEMENT(name, xml_attributes, args) 
      'IS_XMLFOREST',               #* XMLFOREST(xml_attributes) 
      'IS_XMLPARSE',                #* XMLPARSE(text, is_doc, preserve_ws) 
      'IS_XMLPI',                   #* XMLPI(name [, args]) 
      'IS_XMLROOT',                 #* XMLROOT(xml, version, standalone) 
      'IS_XMLSERIALIZE',            #* XMLSERIALIZE(is_document, xmlval) 
      'IS_DOCUMENT'                 #* xmlval IS DOCUMENT 
   ], 0;

   typedef_enum 'XmlOptionType', [
      'XMLOPTION_DOCUMENT',
      'XMLOPTION_CONTENT'
   ], 0;
}
BEGIN {
   typedef_struct 'XMLExpr', {
      xpr                  => [ Expr            ],
      op                   => [ XmlExprOp       ],  #* xml function ID 
      name                 => [ Str             ],  #* name in xml(NAME foo ...) syntaxes 
      named_args           => [ ArrayRef        ],  #* non-XML expressions for xml_attributes 
      arg_names            => [ ArrayRef        ],  #* parallel list of Value strings 
      args                 => [ ArrayRef        ],  #* list of expressions 
      xmloption            => [ XmlOptionType   ],  #* DOCUMENT or CONTENT 
      type                 => [ UInt            ],  #* target type/typmod for XMLSERIALIZE 
      typmod               => [ Int32           ],
      location             => [ HashRef         ],  #* token location, or -1 if unknown 
   };

   #* ----------------
   #* NullTest
   #* NullTest represents the operation of testing a value for NULLness.
   #* The appropriate test is performed and returned as a boolean Datum.
   #* NOTE: the semantics of this for rowtype inputs are noticeably different
   #* from the scalar case.  We provide an "argisrow" flag to reflect that.
   #* ----------------

   typedef_enum 'NullTestType', [
      'IS_NULL',
      'IS_NOT_NULL'
   ], 0;
}
BEGIN {
   typedef_struct 'NullTest', {
      xpr                  => [ Expr            ],
      arg                  => [ Expr            ],  #* input expression 
      nulltesttype         => [ NullTestType    ],  #* IS NULL, IS NOT NULL 
      argisrow             => [ Bool            ],  #* T if input is of a composite type 
   };

   #* BooleanTest
   #* BooleanTest represents the operation of determining whether a boolean
   #* is TRUE, FALSE, or UNKNOWN (ie, NULL).  All six meaningful combinations
   #* are supported.  Note that a NULL input does *not* cause a NULL result.
   #* The appropriate test is performed and returned as a boolean Datum.

   typedef_enum 'BoolTestType', [
      'IS_TRUE', 'IS_NOT_TRUE', 'IS_FALSE', 'IS_NOT_FALSE', 'IS_UNKNOWN', 'IS_NOT_UNKNOWN'
   ], 0;
}
BEGIN {
   typedef_struct 'BooleanTest', {
      xpr                  => [ Expr            ],
      arg                  => [ Expr            ],  #* input expression 
      booltesttype         => [ BoolTestType    ],  #* test type 
   };

   ### RIP: CoerceToDomain ###
   ### RIP: CoerceToDomainValue ###

   #* Placeholder node for a DEFAULT marker in an INSERT or UPDATE command.
   #* This is not an executable expression: it must be replaced by the actual
   #* column default expression during rewriting.  But it is convenient to
   #* treat it as an expression node during parsing and rewriting.
   typedef_struct 'SetToDefault', {
      xpr                  => [ Expr            ],
      typeId               => [ UInt            ],  #* type for substituted value 
      typeMod              => [ Int32           ],  #* typemod for substituted value 
      collation            => [ UInt            ],  #* collation for the substituted value 
      location             => [ HashRef         ],  #* token location, or -1 if unknown 
   };

   #* Node representing [WHERE] CURRENT OF cursor_name
   #* CURRENT OF is a bit like a Var, in that it carries the rangetable index
   #* of the target relation being constrained; this aids placing the expression
   #* correctly during planning.  We can assume however that its "levelsup" is
   #* always zero, due to the syntactic constraints on where it can appear.
   #* The referenced cursor can be represented either as a hardwired string
   #* or as a reference to a run-time parameter of type REFCURSOR.  The latter
   #* case is for the convenience of plpgsql.
   typedef_struct 'CurrentOfExpr', {
      xpr                  => [ Expr            ],
      cvarno               => [ Int16           ],  #* RT index of target relation 
      cursor_name          => [ Str             ],  #* name of referenced cursor, or NULL 
      cursor_param         => [ Int             ],  #* refcursor parameter number, or 0 
   };

   ### RIP: TargetEntry ###
   ### RIP: RangeTblRef ###

   #*----------
   #* JoinExpr - for SQL JOIN expressions
   #* isNatural, usingClause, and quals are interdependent.  The user can write
   #* only one of NATURAL, USING(), or ON() (this is enforced by the grammar).
   #* If he writes NATURAL then parse analysis generates the equivalent USING()
   #* list, and from that fills in "quals" with the right equality comparisons.
   #* If he writes USING() then "quals" is filled with equality comparisons.
   #* If he writes ON() then only "quals" is set.  Note that NATURAL/USING
   #* are not equivalent to ON() since they also affect the output column list.
   #* alias is an Alias node representing the AS alias-clause attached to the
   #* join expression, or NULL if no clause.  NB: presence or absence of the
   #* alias has a critical impact on semantics, because a join with an alias
   #* restricts visibility of the tables/columns inside it.
   #* During parse analysis, an RTE is created for the Join, and its index
   #* is filled into rtindex.  This RTE is present mainly so that Vars can
   #* be created that refer to the outputs of the join.  The planner sometimes
   #* generates JoinExprs internally; these can have rtindex = 0 if there are
   #* no join alias variables referencing such joins.
   #*----------
   typedef_struct 'JoinExpr', {
      jointype             => [ JoinType        ],  #* type of join 
      isNatural            => [ Bool            ],  #* Natural join? Will need to shape table 
      larg                 => [ Any             ],  #* left subtree 
      rarg                 => [ Any             ],  #* right subtree 
      usingClause          => [ ArrayRef        ],  #* USING clause, if any (list of String) 
      quals                => [ Any             ],  #* qualifiers on join, if any 
      alias                => [ Alias           ],  #* user-written alias clause, if any 
      rtindex              => [ Int             ],  #* RT index assigned for join, or 0 
   };

   #*----------
   #* FromExpr - represents a FROM ... WHERE ... construct
   #* This is both more flexible than a JoinExpr (it can have any number of
   #* children, including zero) and less so --- we don't need to deal with
   #* aliases and so on.  The output column set is implicitly just the union
   #* of the outputs of the children.
   #*----------
   typedef_struct 'FromExpr', {
      fromlist             => [ ArrayRef        ],  #* List of join subtrees 
      quals                => [ Any             ],  #* qualifiers on join, if any 
   };
}

#endif   #* PRIMNODES_H 

#*-------------------------------------------------------------------------
#*
#* parsenodes.h
#*    definitions for parse tree nodes
#*
#* Many of the node types used in parsetrees include a "location" field.
#* This is a byte (not character) offset in the original source text, to be
#* used for positioning an error cursor when there is an error related to
#* the node.  Access to the original source text is needed to make use of
#* the location.
#*
#*
#* Portions Copyright (c) 1996-2012, PostgreSQL Global Development Group
#* Portions Copyright (c) 1994, Regents of the University of California
#*
#* src/include/nodes/parsenodes.h
#*
#*-------------------------------------------------------------------------

BEGIN {

   #* Sort ordering options for ORDER BY and CREATE INDEX 
   typedef_enum 'SortByDir', [
      'SORTBY_DEFAULT',
      'SORTBY_ASC',
      'SORTBY_DESC',
      'SORTBY_USING'                #* not allowed in CREATE INDEX ... 
   ], 0;

   typedef_enum 'SortByNulls', [
      'SORTBY_NULLS_DEFAULT',
      'SORTBY_NULLS_FIRST',
      'SORTBY_NULLS_LAST'
   ], 0;


   #****************************************************************************
   #*  Supporting data structures for Parse Trees
   #*  Most of these node types appear in raw parsetrees output by the grammar,
   #*  and get transformed to something else by the analyzer.  A few of them
   #*  are used as-is in transformed querytrees.
   #***************************************************************************

   #* TypeName - specifies a type in definitions
   #* For TypeName structures generated internally, it is often easier to
   #* specify the type by OID than by name.  If "names" is NIL then the
   #* actual type OID is given by typeOid, otherwise typeOid is unused.
   #* Similarly, if "typmods" is NIL then the actual typmod is expected to
   #* be prespecified in typemod, otherwise typemod is unused.
   #* If pct_type is TRUE, then names is actually a field name and we look up
   #* the type of that field.  Otherwise (the normal case), names is a type
   #* name possibly qualified with schema and database name.
   typedef_struct 'TypeName', {
      names                => [ ArrayRef        ],  #* qualified name (list of Value strings) 
      typeOid              => [ UInt            ],  #* type identified by OID 
      setof                => [ Bool            ],  #* is a set? 
      pct_type             => [ Bool            ],  #* %TYPE specified? 
      typmods              => [ ArrayRef        ],  #* type modifier expression(s) 
      typemod              => [ Int32           ],  #* prespecified type modifier 
      arrayBounds          => [ ArrayRef        ],  #* array bounds 
      location             => [ HashRef         ],  #* token location, or -1 if unknown 
   };

   #* ColumnRef - specifies a reference to a column, or possibly a whole tuple
   #* The "fields" list must be nonempty.  It can contain string Value nodes
   #* (representing names) and A_Star nodes (representing occurrence of a '*').
   #* Currently, A_Star must appear only as the last list element --- the grammar
   #* is responsible for enforcing this!
   #* Note: any array subscripting or selection of fields from composite columns
   #* is represented by an A_Indirection node above the ColumnRef.  However,
   #* for simplicity in the normal case, initial field selection from a table
   #* name is represented within ColumnRef and not by adding A_Indirection.
   typedef_struct 'Column::Reference', {
      fields               => [ ArrayRef        ],  #* field names (Value strings) or A_Star 
      location             => [ HashRef         ],  #* token location, or -1 if unknown 
   };

   #* ParamRef - specifies a $n parameter reference
   typedef_struct 'ParamRef', {
      number               => [ Int             ],  #* the number of the parameter 
      location             => [ HashRef         ],  #* token location, or -1 if unknown 
   };

   #* WindowDef - raw representation of WINDOW and OVER clauses
   #* For entries in a WINDOW list, "name" is the window name being defined.
   #* For OVER clauses, we use "name" for the "OVER window" syntax, or "refname"
   #* for the "OVER (window)" syntax, which is subtly different --- the latter
   #* implies overriding the window frame clause.
   typedef_struct 'WindowDef', {
      name                 => [ Str             ],  #* window's own name 
      refname              => [ Str             ],  #* referenced window name, if any 
      partitionClause      => [ ArrayRef        ],  #* PARTITION BY expression list 
      orderClause          => [ ArrayRef        ],  #* ORDER BY (list of SortBy) 
      frameOptions         => [ Int             ],  #* frame_clause options, see below 
      startOffset          => [ Any             ],  #* expression for starting bound, if any 
      endOffset            => [ Any             ],  #* expression for ending bound, if any 
      location             => [ HashRef         ],  #* parse location, or -1 if none/unknown 
   };
   
   #* A_Expr - infix, prefix, and postfix expressions
   typedef_enum 'A_Expr_Kind', [
      'AEXPR_OP',                   #* normal operator 
      'AEXPR_AND',                  #* booleans - name field is unused 
      'AEXPR_OR',
      'AEXPR_NOT',
      'AEXPR_OP_ANY',               #* scalar op ANY (array) 
      'AEXPR_OP_ALL',               #* scalar op ALL (array) 
      'AEXPR_DISTINCT',             #* IS DISTINCT FROM - name must be "=" 
      'AEXPR_NULLIF',               #* NULLIF - name must be "=" 
      'AEXPR_OF',                   #* IS [NOT] OF - name must be "=" or "<>" 
      'AEXPR_IN'                    #* [NOT] IN - name must be "=" or "<>" 
   ], 0;
}
BEGIN {
   typedef_struct 'A_Expr', {
      xpr                  => [ Expr            ],
      kind                 => [ A_Expr_Kind     ],  #* see above 
      name                 => [ ArrayRef        ],  #* possibly-qualified name of operator 
      lexpr                => [ Any             ],  #* left argument, or NULL if none 
      rexpr                => [ Any             ],  #* right argument, or NULL if none 
      location             => [ HashRef         ],  #* token location, or -1 if unknown 
   };

   #* A_Const - a literal constant
   
   ### Normally, val would equal a 'Value'.  In Pg, a 'Value' is an object that functions much
   ### like a Perl SCALAR, in that it can store something of one type and be able to convert
   ### at will.  Perl already does this naturally, but knowing the original type of a constant
   ### is still useful information.  Thus, we add in a 'type' attr here, instead of enforcing
   ### types on every single SCALAR-type found from the parser.
   
   ### Oh, and MooX::Types::MooseLike just so happens to have a nice 'Value' type that fits 
   ### perfectly here...
   typedef_struct 'A_Const', {
      xpr                  => [ Expr            ],
      type                 => [ Str             ],  ### (see above...)
      val                  => [ Value           ],  #* value (includes type info, see value.h) 
      location             => [ HashRef         ],  #* token location, or -1 if unknown 
   };

   #* TypeCast - a CAST expression
   typedef_struct 'TypeCast', {
      xpr                  => [ Expr            ],
      arg                  => [ Any             ],  #* the expression being casted 
      typeName             => [ TypeName        ],  #* the target type 
      location             => [ HashRef         ],  #* token location, or -1 if unknown 
   };

   #* CollateClause - a COLLATE expression
   typedef_struct 'CollateClause', {
      xpr                  => [ Expr            ],
      arg                  => [ Any             ],  #* input expression 
      collname             => [ ArrayRef        ],  #* possibly-qualified collation name 
      location             => [ HashRef         ],  #* token location, or -1 if unknown 
   };

   #* FuncCall - a function or aggregate invocation
   #* agg_order (if not NIL) indicates we saw 'foo(... ORDER BY ...)'.
   #* agg_star indicates we saw a 'foo(*)' construct, while agg_distinct
   #* indicates we saw 'foo(DISTINCT ...)'.  In any of these cases, the
   #* construct *must* be an aggregate call.  Otherwise, it might be either an
   #* aggregate or some other kind of function.  However, if OVER is present
   #* it had better be an aggregate or window function.
   typedef_struct 'Function::Call', {
      funcname             => [ ArrayRef        ],  #* qualified name of function 
      args                 => [ ArrayRef        ],  #* the arguments (list of exprs) 
      agg_order            => [ ArrayRef        ],  #* ORDER BY (list of SortBy) 
      agg_star             => [ Bool            ],  #* argument was really '*' 
      agg_distinct         => [ Bool            ],  #* arguments were labeled DISTINCT 
      func_variadic        => [ Bool            ],  #* last argument was labeled VARIADIC 
      over                 => [ WindowDef       ],  #* OVER clause, if any 
      location             => [ HashRef         ],  #* token location, or -1 if unknown 
   };

   #* A_Star - '*' representing all columns of a table or compound field
   #* This can appear within ColumnRef.fields, A_Indirection.indirection, and
   #* ResTarget.indirection lists.
   typedef_struct 'A_Star', {
   };

   #* A_Indices - array subscript or slice bounds ([lidx:uidx] or [uidx])
   typedef_struct 'A_Indices', {
      lidx                 => [ Any             ],  #* NULL if it's a single subscript 
      uidx                 => [ Any             ],
   };

   #* A_Indirection - select a field and/or array element from an expression
   #* The indirection list can contain A_Indices nodes (representing
   #* subscripting), string Value nodes (representing field selection --- the
   #* string value is the name of the field to select), and A_Star nodes
   #* (representing selection of all fields of a composite type).
   #* For example, a complex selection operation like
   #*              (foo).field1[42][7].field2
   #* would be represented with a single A_Indirection node having a 4-element
   #* indirection list.
   #* Currently, A_Star must appear only as the last list element --- the grammar
   #* is responsible for enforcing this!
   typedef_struct 'A_Indirection', {
      arg                  => [ Any             ],  #* the thing being selected from 
      indirection          => [ ArrayRef        ],  #* subscripts and/or field names and/or * 
   };

   #* A_ArrayExpr - an ARRAY[] construct
   typedef_struct 'A_ArrayExpr', {
      xpr                  => [ Expr            ],
      elements             => [ ArrayRef        ],  #* array element expressions 
      location             => [ HashRef         ],  #* token location, or -1 if unknown 
   };

   #* ResTarget -
   #*    result target (used in target list of pre-transformed parse trees)
   #* In a SELECT target list, 'name' is the column label from an
   #* 'AS ColumnLabel' clause, or NULL if there was none, and 'val' is the
   #* value expression itself.  The 'indirection' field is not used.
   #* INSERT uses ResTarget in its target-column-names list.  Here, 'name' is
   #* the name of the destination column, 'indirection' stores any subscripts
   #* attached to the destination, and 'val' is not used.
   #* In an UPDATE target list, 'name' is the name of the destination column,
   #* 'indirection' stores any subscripts attached to the destination, and
   #* 'val' is the expression to assign.
   #* See A_Indirection for more info about what can appear in 'indirection'.
   typedef_struct 'ResultTarget', {
      name                 => [ Str             ],  #* column name or NULL 
      indirection          => [ ArrayRef        ],  #* subscripts, field names, and '*', or NIL 
      val                  => [ Any             ],  #* the value expression to compute or assign 
      location             => [ HashRef         ],  #* token location, or -1 if unknown 
   };
}
BEGIN {
   #* SortBy - for ORDER BY clause
   typedef_struct 'SortBy', {
      node                 => [ Any             ],  #* expression to sort on 
      sortby_dir           => [ SortByDir       ],  #* ASC/DESC/USING/default 
      sortby_nulls         => [ SortByNulls     ],  #* NULLS FIRST/LAST 
      useOp                => [ ArrayRef        ],  #* name of op to use, if SORTBY_USING 
      location             => [ HashRef         ],  #* operator location, or -1 if none/unknown 
   };

   ### (Moved WindowDef, since it gets used earlier than its definition...)

   #* frameOptions is an OR of these bits.  The NONDEFAULT and BETWEEN bits are
   #* used so that ruleutils.c can tell which properties were specified and
   #* which were defaulted; the correct behavioral bits must be set either way.
   #* The START_foo and END_foo options must come in pairs of adjacent bits for
   #* the convenience of gram.y, even though some of them are useless/invalid.
   #* We will need more bits (and fields) to cover the full SQL:2008 option set.
   typedef_enum '', [
      'FRAMEOPTION_NONDEFAULT=1',                #* any specified? 
      'FRAMEOPTION_RANGE',                       #* RANGE behavior 
      'FRAMEOPTION_ROWS',                        #* ROWS behavior 
      'FRAMEOPTION_BETWEEN',                     #* BETWEEN given? 
      'FRAMEOPTION_START_UNBOUNDED_PRECEDING',   #* start is U. P. 
      'FRAMEOPTION_END_UNBOUNDED_PRECEDING',     #* (disallowed) 
      'FRAMEOPTION_START_UNBOUNDED_FOLLOWING',   #* (disallowed) 
      'FRAMEOPTION_END_UNBOUNDED_FOLLOWING',     #* end is U. F. 
      'FRAMEOPTION_START_CURRENT_ROW',           #* start is C. R. 
      'FRAMEOPTION_END_CURRENT_ROW',             #* end is C. R. 
      'FRAMEOPTION_START_VALUE_PRECEDING',       #* start is V. P. 
      'FRAMEOPTION_END_VALUE_PRECEDING',         #* end is V. P. 
      'FRAMEOPTION_START_VALUE_FOLLOWING',       #* start is V. F. 
      'FRAMEOPTION_END_VALUE_FOLLOWING',         #* end is V. F. 
      'FRAMEOPTION_START_VALUE = (FRAMEOPTION_START_VALUE_PRECEDING | FRAMEOPTION_START_VALUE_FOLLOWING)',
      'FRAMEOPTION_END_VALUE   = (FRAMEOPTION_END_VALUE_PRECEDING   | FRAMEOPTION_END_VALUE_FOLLOWING)',
      'FRAMEOPTION_DEFAULTS    = (FRAMEOPTION_RANGE | FRAMEOPTION_START_UNBOUNDED_PRECEDING | FRAMEOPTION_END_CURRENT_ROW)',
   ], 1;

   #* RangeSubselect - subquery appearing in a FROM clause
   typedef_struct 'Range::SubSelect', {
      subquery             => [ Any             ],  #* the untransformed sub-select clause 
      alias                => [ Alias           ],  #* table alias & optional column aliases 
   };

   #* RangeFunction - function call appearing in a FROM clause
   typedef_struct 'Range::Function', {
      funccallnode         => [ Any             ],  #* untransformed function call tree 
      alias                => [ Alias           ],  #* table alias & optional column aliases 
      coldeflist           => [ ArrayRef        ],  #* list of ColumnDef nodes to describe result
                                                    #* of function returning RECORD 
   };

   #* ColumnDef - column definition (used in various creates)
   #* If the column has a default value, we may have the value expression
   #* in either "raw" form (an untransformed parse tree) or "cooked" form
   #* (a post-parse-analysis, executable expression tree), depending on
   #* how this ColumnDef node was created (by parsing, or by inheritance
   #* from an existing relation).  We should never have both in the same node!
   #* Similarly, we may have a COLLATE specification in either raw form
   #* (represented as a CollateClause with arg==NULL) or cooked form
   #* (the collation's OID).
   #* The constraints list may contain a CONSTR_DEFAULT item in a raw
   #* parsetree produced by gram.y, but transformCreateStmt will remove
   #* the item and set raw_default instead.  CONSTR_DEFAULT items
   #* should not appear in any subsequent processing.
   typedef_struct 'Column::Definition', {
      colname              => [ Str             ],  #* name of column 
      typeName             => [ TypeName        ],  #* type of column 
      inhcount             => [ Int             ],  #* number of times column is inherited 
      is_local             => [ Bool            ],  #* column has local (non-inherited) def'n 
      is_not_null          => [ Bool            ],  #* NOT NULL constraint specified? 
      is_from_type         => [ Bool            ],  #* column definition came from table type 
      storage              => [ Str             ],  #* attstorage setting, or 0 for default 
      raw_default          => [ Any             ],  #* default value (untransformed parse tree) 
      cooked_default       => [ Any             ],  #* default value (transformed expr tree) 
      collClause           => [ CollateClause   ],  #* untransformed COLLATE spec, if any 
      collOid              => [ UInt            ],  #* collation OID (InvalidOid if not set) 
      constraints          => [ ArrayRef        ],  #* other constraints on column 
      fdwoptions           => [ ArrayRef        ],  #* per-column FDW options 
   };

   #* TableLikeClause - CREATE TABLE ( ... LIKE ... ) clause
   typedef_struct 'TableLikeClause', {
      relation             => [ RangeVar        ],
      options              => [ UInt32          ],  #* OR of TableLikeOption flags 
   };

   typedef_enum 'TableLikeOption', [
      'CREATE_TABLE_LIKE_DEFAULTS    = 1 << 0',
      'CREATE_TABLE_LIKE_CONSTRAINTS = 1 << 1',
      'CREATE_TABLE_LIKE_INDEXES     = 1 << 2',
      'CREATE_TABLE_LIKE_STORAGE     = 1 << 3',
      'CREATE_TABLE_LIKE_COMMENTS    = 1 << 4',
      'CREATE_TABLE_LIKE_ALL         = 0x7FFFFFFF'
   ], 1;

   #* IndexElem - index parameters (used in CREATE INDEX)
   #* For a plain index attribute, 'name' is the name of the table column to
   #* index, and 'expr' is NULL.  For an index expression, 'name' is NULL and
   #* 'expr' is the expression tree.
   typedef_struct 'IndexElem', {
      name                 => [ Str             ],  #* name of attribute to index, or NULL 
      expr                 => [ Any             ],  #* expression to index, or NULL 
      indexcolname         => [ Str             ],  #* name for index column; NULL = default 
      collation            => [ ArrayRef        ],  #* name of collation; NIL = default 
      opclass              => [ ArrayRef        ],  #* name of desired opclass; NIL = default 
      ordering             => [ SortByDir       ],  #* ASC/DESC/default 
      nulls_ordering       => [ SortByNulls     ],  #* FIRST/LAST/default 
   };

   #* DefElem - a generic "name = value" option definition
   #* In some contexts the name can be qualified.  Also, certain SQL commands
   #* allow a SET/ADD/DROP action to be attached to option settings, so it's
   #* convenient to carry a field for that too.  (Note: currently, it is our
   #* practice that the grammar allows namespace and action only in statements
   #* where they are relevant; C code can just ignore those fields in other
   #* statements.)
   typedef_enum 'DefElemAction', [
      'DEFELEM_UNSPEC',             #* no action given 
      'DEFELEM_SET',
      'DEFELEM_ADD',
      'DEFELEM_DROP'
   ], 0;
}
BEGIN {
   typedef_struct 'DefElem', {
      defnamespace         => [ Str             ],  #* NULL if unqualified name 
      defname              => [ Str             ],
      arg                  => [ Any             ],  #* a (Value *) or a (TypeName *) 
      defaction            => [ DefElemAction   ],  #* unspecified action, or SET/ADD/DROP 
   };

   #* LockingClause - raw representation of FOR UPDATE/SHARE options
   #* Note: lockedRels == NIL means "all relations in query".  Otherwise it
   #* is a list of RangeVar nodes.  (We use RangeVar mainly because it carries
   #* a location field --- currently, parse analysis insists on unqualified
   #* names in LockingClause.)
   typedef_struct 'LockingClause', {
      lockedRels           => [ ArrayRef        ],  #* FOR UPDATE or FOR SHARE relations 
      forUpdate            => [ Bool            ],  #* true = FOR UPDATE, false = FOR SHARE 
      noWait               => [ Bool            ],  #* NOWAIT option 
   };

   #* XMLSERIALIZE (in raw parse tree only)
   typedef_struct 'XMLSerialize', {
      xmloption            => [ XmlOptionType   ],  #* DOCUMENT or CONTENT 
      expr                 => [ Any             ],
      typeName             => [ TypeName        ],
      location             => [ HashRef         ],  #* token location, or -1 if unknown 
   };

   #* WithClause -
   #*     representation of WITH clause
   #* Note: WithClause does not propagate into the Query representation;
   #* but CommonTableExpr does.
   typedef_struct 'WithClause', {
      ctes                 => [ ArrayRef        ],  #* list of CommonTableExprs 
      recursive            => [ Bool            ],  #* true = WITH RECURSIVE 
      location             => [ HashRef         ],  #* token location, or -1 if unknown 
   };

   #* CommonTableExpr -
   #*     representation of WITH list element
   #* We don't currently support the SEARCH or CYCLE clause.
   typedef_struct 'CommonTableExpr', {
      ctename              => [ Str             ],  #* query name (never qualified) 
      aliascolnames        => [ ArrayRef        ],  #* optional list of column names 
      #* SelectStmt/InsertStmt/etc before parse analysis, Query afterwards: 
      ctequery             => [ Any             ],  #* the CTE's subquery 
      location             => [ HashRef         ],  #* token location, or -1 if unknown 
      #* These fields are set during parse analysis: 
      cterecursive         => [ Bool            ],  #* is this CTE actually recursive? 
      cterefcount          => [ Int             ],  #* number of RTEs referencing this CTE
                                                    #* (excluding internal self-references) 
      ctecolnames          => [ ArrayRef        ],  #* list of output column names 
      ctecoltypes          => [ ArrayRef        ],  #* OID list of output column type OIDs 
      ctecoltypmods        => [ ArrayRef        ],  #* integer list of output column typmods 
      ctecolcollations     => [ ArrayRef        ],  #* OID list of column collation OIDs 
   };
}
BEGIN {
   #*****************************************************************************
   #*      Optimizable Statements
   #****************************************************************************

   #* ----------------------
   #*      Insert Statement
   #* The source expression is represented by SelectStmt for both the
   #* SELECT and VALUES cases.  If selectStmt is NULL, then the query
   #* is INSERT ... DEFAULT VALUES.
   #* ----------------------
   typedef_struct 'Statement::Insert', {
      relation             => [ RangeVar        ],  #* relation to insert into 
      cols                 => [ ArrayRef        ],  #* optional: names of the target columns 
      selectStmt           => [ Any             ],  #* the source SELECT/VALUES, or NULL 
      returningList        => [ ArrayRef        ],  #* list of expressions to return 
      withClause           => [ WithClause      ],  #* WITH clause 
   };

   #* ----------------------
   #*      Delete Statement
   #* ----------------------
   typedef_struct 'Statement::Delete', {
      relation             => [ RangeVar        ],  #* relation to delete from 
      usingClause          => [ ArrayRef        ],  #* optional using clause for more tables 
      whereClause          => [ Any             ],  #* qualifications 
      returningList        => [ ArrayRef        ],  #* list of expressions to return 
      withClause           => [ WithClause      ],  #* WITH clause 
   };

   #* ----------------------
   #*      Update Statement
   #* ----------------------
   typedef_struct 'Statement::Update', {
      relation             => [ RangeVar        ],  #* relation to update 
      targetList           => [ ArrayRef        ],  #* the target list (of ResTarget) 
      whereClause          => [ Any             ],  #* qualifications 
      fromClause           => [ ArrayRef        ],  #* optional from clause for more tables 
      returningList        => [ ArrayRef        ],  #* list of expressions to return 
      withClause           => [ WithClause      ],  #* WITH clause 
   };

   #* ----------------------
   #*      Select Statement
   #* A "simple" SELECT is represented in the output of gram.y by a single
   #* SelectStmt node; so is a VALUES construct.  A query containing set
   #* operators (UNION, INTERSECT, EXCEPT) is represented by a tree of SelectStmt
   #* nodes, in which the leaf nodes are component SELECTs and the internal nodes
   #* represent UNION, INTERSECT, or EXCEPT operators.  Using the same node
   #* type for both leaf and internal nodes allows gram.y to stick ORDER BY,
   #* LIMIT, etc, clause values into a SELECT statement without worrying
   #* whether it is a simple or compound SELECT.
   #* ----------------------
   typedef_enum 'SetOperation', [
      'SETOP_NONE',
      'SETOP_UNION',
      'SETOP_INTERSECT',
      'SETOP_EXCEPT'
   ], 0;
}
BEGIN {
   ### Yo dawg!  I heard you like using your SelectStmt before using your SelectStmt... ###
   typedef_struct 'Statement::Select', {};
}
BEGIN {
   typedef_struct 'Statement::Select', {

      #* These fields are used only in "leaf" SelectStmts.
      distinctClause       => [ ArrayRef        ],  #* NULL, list of DISTINCT ON exprs, or
                                                    #* lcons(NIL,NIL) for all (SELECT DISTINCT) 
      intoClause           => [ IntoClause      ],  #* target for SELECT INTO 
      targetList           => [ ArrayRef        ],  #* the target list (of ResTarget) 
      fromClause           => [ ArrayRef        ],  #* the FROM clause 
      whereClause          => [ Any             ],  #* WHERE qualification 
      groupClause          => [ ArrayRef        ],  #* GROUP BY clauses 
      havingClause         => [ Any             ],  #* HAVING conditional-expression 
      windowClause         => [ ArrayRef        ],  #* WINDOW window_name AS (...), ... 
      withClause           => [ WithClause      ],  #* WITH clause 

      #* In a "leaf" node representing a VALUES list, the above fields are all
      #* null, and instead this field is set.  Note that the elements of the
      #* sublists are just expressions, without ResTarget decoration. Also note
      #* that a list element can be DEFAULT (represented as a SetToDefault
      #* node), regardless of the context of the VALUES list. It's up to parse
      #* analysis to reject that where not valid.
      valuesLists          => [ ArrayRef        ],  #* untransformed list of expression lists 

      #* These fields are used in both "leaf" SelectStmts and upper-level
      #* SelectStmts.
      sortClause           => [ ArrayRef        ],  #* sort clause (a list of SortBy's) 
      limitOffset          => [ Any             ],  #* # of result tuples to skip 
      limitCount           => [ Any             ],  #* # of result tuples to return 
      lockingClause        => [ ArrayRef        ],  #* FOR UPDATE (list of LockingClause's) 

      #* These fields are used only in upper-level SelectStmts.
      op                   => [ SetOperation    ],  #* type of set op 
      all                  => [ Bool            ],  #* ALL specified? 
      larg                 => [ StatementSelect ],  #* left child 
      rarg                 => [ StatementSelect ],  #* right child 
      #* Eventually add fields for CORRESPONDING spec here 
   };


   #* ----------------------
   #*      Set Operation node for post-analysis query trees
   #* After parse analysis, a SELECT with set operations is represented by a
   #* top-level Query node containing the leaf SELECTs as subqueries in its
   #* range table.  Its setOperations field shows the tree of set operations,
   #* with leaf SelectStmt nodes replaced by RangeTblRef nodes, and internal
   #* nodes replaced by SetOperationStmt nodes.  Information about the output
   #* column types is added, too.  (Note that the child nodes do not necessarily
   #* produce these types directly, but we've checked that their output types
   #* can be coerced to the output column type.)  Also, if it's not UNION ALL,
   #* information about the types' sort/group semantics is provided in the form
   #* of a SortGroupClause list (same representation as, eg, DISTINCT).
   #* The resolved common column collations are provided too; but note that if
   #* it's not UNION ALL, it's okay for a column to not have a common collation,
   #* so a member of the colCollations list could be InvalidOid even though the
   #* column has a collatable type.
   #* ----------------------
   typedef_struct 'Statement::SetOperation', {
      op                   => [ SetOperation    ],  #* type of set op 
      all                  => [ Bool            ],  #* ALL specified? 
      larg                 => [ Any             ],  #* left child 
      rarg                 => [ Any             ],  #* right child 
      #* Eventually add fields for CORRESPONDING spec here 

      #* Fields derived during parse analysis: 
      colTypes             => [ ArrayRef        ],  #* OID list of output column type OIDs 
      colTypmods           => [ ArrayRef        ],  #* integer list of output column typmods 
      colCollations        => [ ArrayRef        ],  #* OID list of output column collation OIDs 
      groupClauses         => [ ArrayRef        ],  #* a list of SortGroupClause's 
      #* groupClauses is NIL if UNION ALL, but must be set otherwise 
   };


   #*****************************************************************************
   #*      Other Statements (no optimizations required)
   #*      These are not touched by parser/analyze.c except to put them into
   #*      the utilityStmt field of a Query.  This is eventually passed to
   #*      ProcessUtility (by-passing rewriting and planning).  Some of the
   #*      statements do need attention from parse analysis, and this is
   #*      done by routines in parser/parse_utilcmd.c after ProcessUtility
   #*      receives the command for execution.
   #****************************************************************************

   #* When a command can act on several kinds of objects with only one
   #* parse structure required, use these constants to designate the
   #* object type.  Note that commands typically don't support all the types.

   typedef_enum 'ObjectType', [
      'OBJECT_AGGREGATE',
      'OBJECT_ATTRIBUTE',           #* type's attribute, when distinct from column 
      'OBJECT_CAST',
      'OBJECT_COLUMN',
      'OBJECT_CONSTRAINT',
      'OBJECT_COLLATION',
      'OBJECT_CONVERSION',
      'OBJECT_DATABASE',
      'OBJECT_DOMAIN',
      'OBJECT_EXTENSION',
      'OBJECT_FDW',
      'OBJECT_FOREIGN_SERVER',
      'OBJECT_FOREIGN_TABLE',
      'OBJECT_FUNCTION',
      'OBJECT_INDEX',
      'OBJECT_LANGUAGE',
      'OBJECT_LARGEOBJECT',
      'OBJECT_OPCLASS',
      'OBJECT_OPERATOR',
      'OBJECT_OPFAMILY',
      'OBJECT_ROLE',
      'OBJECT_RULE',
      'OBJECT_SCHEMA',
      'OBJECT_SEQUENCE',
      'OBJECT_TABLE',
      'OBJECT_TABLESPACE',
      'OBJECT_TRIGGER',
      'OBJECT_TSCONFIGURATION',
      'OBJECT_TSDICTIONARY',
      'OBJECT_TSPARSER',
      'OBJECT_TSTEMPLATE',
      'OBJECT_TYPE',
      'OBJECT_VIEW'
   ], 0;

   #* ----------------------
   #*      Create Schema Statement
   #* NOTE: the schemaElts list contains raw parsetrees for component statements
   #* of the schema, such as CREATE TABLE, GRANT, etc.  These are analyzed and
   #* executed after the schema itself is created.
   #* ----------------------
   typedef_struct 'Statement::CreateSchema', {
      schemaname           => [ Str             ],  #* the name of the schema to create 
      authid               => [ Str             ],  #* the owner of the created schema 
      schemaElts           => [ ArrayRef        ],  #* schema components (list of parsenodes) 
   };

   typedef_enum 'DropBehavior', [
      'DROP_RESTRICT',              #* drop fails if any dependent objects 
      'DROP_CASCADE'                #* remove dependent objects too 
   ], 0;
}
BEGIN {
   #* ----------------------
   #*  Alter Table
   #* ----------------------
   typedef_struct 'Statement::AlterTable', {
      relation             => [ RangeVar        ],  #* table to work on 
      cmds                 => [ ArrayRef        ],  #* list of subcommands 
      relkind              => [ ObjectType      ],  #* type of object 
      missing_ok           => [ Bool            ],  #* skip error if table missing 
   };

   typedef_enum 'AlterTableType', [
      'AT_AddColumn',               #* add column 
      'AT_AddColumnRecurse',        #* internal to commands/tablecmds.c 
      'AT_AddColumnToView',         #* implicitly via CREATE OR REPLACE VIEW 
      'AT_ColumnDefault',           #* alter column default 
      'AT_DropNotNull',             #* alter column drop not null 
      'AT_SetNotNull',              #* alter column set not null 
      'AT_SetStatistics',           #* alter column set statistics 
      'AT_SetOptions',              #* alter column set ( options ) 
      'AT_ResetOptions',            #* alter column reset ( options ) 
      'AT_SetStorage',              #* alter column set storage 
      'AT_DropColumn',              #* drop column 
      'AT_DropColumnRecurse',       #* internal to commands/tablecmds.c 
      'AT_AddIndex',                #* add index 
      'AT_ReAddIndex',              #* internal to commands/tablecmds.c 
      'AT_AddConstraint',           #* add constraint 
      'AT_AddConstraintRecurse',    #* internal to commands/tablecmds.c 
      'AT_ValidateConstraint',      #* validate constraint 
      'AT_ValidateConstraintRecurse',       #* internal to commands/tablecmds.c 
      'AT_ProcessedConstraint',     #* pre-processed add constraint (local in
                                    #* parser/parse_utilcmd.c) 
      'AT_AddIndexConstraint',      #* add constraint using existing index 
      'AT_DropConstraint',          #* drop constraint 
      'AT_DropConstraintRecurse',   #* internal to commands/tablecmds.c 
      'AT_AlterColumnType',         #* alter column type 
      'AT_AlterColumnGenericOptions',       #* alter column OPTIONS (...) 
      'AT_ChangeOwner',             #* change owner 
      'AT_ClusterOn',               #* CLUSTER ON 
      'AT_DropCluster',             #* SET WITHOUT CLUSTER 
      'AT_AddOids',                 #* SET WITH OIDS 
      'AT_AddOidsRecurse',          #* internal to commands/tablecmds.c 
      'AT_DropOids',                #* SET WITHOUT OIDS 
      'AT_SetTableSpace',           #* SET TABLESPACE 
      'AT_SetRelOptions',           #* SET (...) -- AM specific parameters 
      'AT_ResetRelOptions',         #* RESET (...) -- AM specific parameters 
      'AT_ReplaceRelOptions',       #* replace reloption list in its entirety 
      'AT_EnableTrig',              #* ENABLE TRIGGER name 
      'AT_EnableAlwaysTrig',        #* ENABLE ALWAYS TRIGGER name 
      'AT_EnableReplicaTrig',       #* ENABLE REPLICA TRIGGER name 
      'AT_DisableTrig',             #* DISABLE TRIGGER name 
      'AT_EnableTrigAll',           #* ENABLE TRIGGER ALL 
      'AT_DisableTrigAll',          #* DISABLE TRIGGER ALL 
      'AT_EnableTrigUser',          #* ENABLE TRIGGER USER 
      'AT_DisableTrigUser',         #* DISABLE TRIGGER USER 
      'AT_EnableRule',              #* ENABLE RULE name 
      'AT_EnableAlwaysRule',        #* ENABLE ALWAYS RULE name 
      'AT_EnableReplicaRule',       #* ENABLE REPLICA RULE name 
      'AT_DisableRule',             #* DISABLE RULE name 
      'AT_AddInherit',              #* INHERIT parent 
      'AT_DropInherit',             #* NO INHERIT parent 
      'AT_AddOf',                   #* OF <type_name> 
      'AT_DropOf',                  #* NOT OF 
      'AT_GenericOptions'           #* OPTIONS (...) 
   ], 0;
}
BEGIN {
   typedef_struct 'AlterTable::Command', {    #* one subcommand of an ALTER TABLE 
      subtype              => [ AlterTableType  ],  #* Type of table alteration to apply 
      name                 => [ Str             ],  #* column, constraint, or trigger to act on,
                                                    #* or new owner or tablespace 
      def                  => [ Any             ],  #* definition of new column, index,
                                                    #* constraint, or parent table 
      behavior             => [ DropBehavior    ],  #* RESTRICT or CASCADE for DROP cases 
      missing_ok           => [ Bool            ],  #* skip error if missing? 
   };


   #* ----------------------
   #*  Alter Domain
   #* The fields are used in different ways by the different variants of
   #* this command.
   #* ----------------------
   typedef_struct 'Statement::AlterDomain', {
      subtype              => [ Char            ],  #*------------
                                                    #*  T = alter column default
                                                    #*  N = alter column drop not null
                                                    #*  O = alter column set not null
                                                    #*  C = add constraint
                                                    #*  X = drop constraint
                                                    #*------------
      typeName             => [ ArrayRef        ],  #* domain to work on 
      name                 => [ Str             ],  #* column or constraint name to act on 
      def                  => [ Any             ],  #* definition of default or constraint 
      behavior             => [ DropBehavior    ],  #* RESTRICT or CASCADE for DROP cases 
      missing_ok           => [ Bool            ],  #* skip error if missing? 
   };


   #* ----------------------
   #*      Grant|Revoke Statement
   #* ----------------------
   typedef_enum 'GrantTargetType', [
      'ACL_TARGET_OBJECT',          #* grant on specific named object(s) 
      'ACL_TARGET_ALL_IN_SCHEMA',   #* grant on all objects in given schema(s) 
      'ACL_TARGET_DEFAULTS'         #* ALTER DEFAULT PRIVILEGES 
   ], 0;

   typedef_enum 'GrantObjectType', [
      'ACL_OBJECT_COLUMN',          #* column 
      'ACL_OBJECT_RELATION',        #* table, view 
      'ACL_OBJECT_SEQUENCE',        #* sequence 
      'ACL_OBJECT_DATABASE',        #* database 
      'ACL_OBJECT_DOMAIN',          #* domain 
      'ACL_OBJECT_FDW',             #* foreign-data wrapper 
      'ACL_OBJECT_FOREIGN_SERVER',  #* foreign server 
      'ACL_OBJECT_FUNCTION',        #* function 
      'ACL_OBJECT_LANGUAGE',        #* procedural language 
      'ACL_OBJECT_LARGEOBJECT',     #* largeobject 
      'ACL_OBJECT_NAMESPACE',       #* namespace 
      'ACL_OBJECT_TABLESPACE',      #* tablespace 
      'ACL_OBJECT_TYPE'             #* type 
   ], 0;
}
BEGIN {
   typedef_struct 'Statement::Grant', {
      is_grant             => [ Bool            ],  #* true = GRANT, false = REVOKE 
      targtype             => [ GrantTargetType ],  #* type of the grant target 
      objtype              => [ GrantObjectType ],  #* kind of object being operated on 
      objects              => [ ArrayRef        ],  #* list of RangeVar nodes, Function nodes,
                                                    #* or plain names (as Value strings) 
      privileges           => [ ArrayRef        ],  #* list of AccessPriv nodes 
      #* privileges == NIL denotes ALL PRIVILEGES 
      grantees             => [ ArrayRef        ],  #* list of PrivGrantee nodes 
      grant_option         => [ Bool            ],  #* grant or revoke grant option 
      behavior             => [ DropBehavior    ],  #* drop behavior (for REVOKE) 
   };

   typedef_struct 'PrivGrantee', {
      rolname              => [ Str             ],  #* if NULL then PUBLIC 
   };

   #* Note: FuncWithArgs carries only the types of the input parameters of the
   #* function.  So it is sufficient to identify an existing function, but it
   #* is not enough info to define a function nor to call it.
   typedef_struct 'FuncWithArgs', {
      funcname             => [ ArrayRef        ],  #* qualified name of function 
      funcargs             => [ ArrayRef        ],  #* list of Typename nodes 
   };

   #* An access privilege, with optional list of column names
   #* priv_name == NULL denotes ALL PRIVILEGES (only used with a column list)
   #* cols == NIL denotes "all columns"
   #* Note that simple "ALL PRIVILEGES" is represented as a NIL list, not
   #* an AccessPriv with both fields null.
   typedef_struct 'AccessPriv', {
      priv_name            => [ Str             ],  #* string name of privilege 
      cols                 => [ ArrayRef        ],  #* list of Value strings 
   };

   #* ----------------------
   #*      Grant/Revoke Role Statement
   #* Note: because of the parsing ambiguity with the GRANT <privileges>
   #* statement, granted_roles is a list of AccessPriv; the execution code
   #* should complain if any column lists appear.  grantee_roles is a list
   #* of role names, as Value strings.
   #* ----------------------
   typedef_struct 'Statement::GrantRole', {
      granted_roles        => [ ArrayRef        ],  #* list of roles to be granted/revoked 
      grantee_roles        => [ ArrayRef        ],  #* list of member roles to add/delete 
      is_grant             => [ Bool            ],  #* true = GRANT, false = REVOKE 
      admin_opt            => [ Bool            ],  #* with admin option 
      grantor              => [ Str             ],  #* set grantor to other than current role 
      behavior             => [ DropBehavior    ],  #* drop behavior (for REVOKE) 
   };
}
BEGIN {
   #* ----------------------
   #*  Alter Default Privileges Statement
   #* ----------------------
   typedef_struct 'AlterDefaultPrivileges', {
      options              => [ ArrayRef        ],  #* list of DefElem 
      action               => [ StatementGrant  ],  #* GRANT/REVOKE action (with objects=NIL) 
   };

   #* ----------------------
   #*      Copy Statement
   #* We support "COPY relation FROM file", "COPY relation TO file", and
   #* "COPY (query) TO file".  In any given CopyStmt, exactly one of "relation"
   #* and "query" must be non-NULL.
   #* ----------------------
   typedef_struct 'Statement::Copy', {
      relation             => [ RangeVar        ],  #* the relation to copy 
      query                => [ Any             ],  #* the SELECT query to copy 
      attlist              => [ ArrayRef        ],  #* List of column names (as Strings), or NIL
                                                    #* for all columns 
      is_from              => [ Bool            ],  #* TO or FROM 
      filename             => [ Str             ],  #* filename, or NULL for STDIN/STDOUT 
      options              => [ ArrayRef        ],  #* List of DefElem nodes 
   };

   #* ----------------------
   #* SET Statement (includes RESET)
   #* "SET var TO DEFAULT" and "RESET var" are semantically equivalent, but we
   #* preserve the distinction in VariableSetKind for CreateCommandTag().
   #* ----------------------
   typedef_enum 'VariableSetKind', [
      'VAR_SET_VALUE',              #* SET var = value 
      'VAR_SET_DEFAULT',            #* SET var TO DEFAULT 
      'VAR_SET_CURRENT',            #* SET var FROM CURRENT 
      'VAR_SET_MULTI',              #* special case for SET TRANSACTION ... 
      'VAR_RESET',                  #* RESET var 
      'VAR_RESET_ALL'               #* RESET ALL 
   ], 0;
}
BEGIN {
   typedef_struct 'Statement::VariableSet', {
      kind                 => [ VariableSetKind ],
      name                 => [ Str             ],  #* variable to be set 
      args                 => [ ArrayRef        ],  #* List of A_Const nodes 
      is_local             => [ Bool            ],  #* SET LOCAL? 
   };

   #* ----------------------
   #* Show Statement
   #* ----------------------
   typedef_struct 'Statement::VariableShow', {
      name                 => [ Str             ],
   };

   #* ----------------------
   #*      Create Table Statement
   #* NOTE: in the raw gram.y output, ColumnDef and Constraint nodes are
   #* intermixed in tableElts, and constraints is NIL.  After parse analysis,
   #* tableElts contains just ColumnDefs, and constraints contains just
   #* Constraint nodes (in fact, only CONSTR_CHECK nodes, in the present
   #* implementation).
   #* ----------------------

   typedef_struct 'Statement::Create', {
      relation             => [ RangeVar        ],  #* relation to create 
      tableElts            => [ ArrayRef        ],  #* column definitions (list of ColumnDef) 
      inhRelations         => [ ArrayRef        ],  #* relations to inherit from (list of
                                                    #* inhRelation) 
      ofTypename           => [ TypeName        ],  #* OF typename 
      constraints          => [ ArrayRef        ],  #* constraints (list of Constraint nodes) 
      options              => [ ArrayRef        ],  #* options from WITH clause 
      oncommit             => [ OnCommitAction  ],  #* what do we do at COMMIT? 
      tablespacename       => [ Str             ],  #* table space to use, or NULL 
      if_not_exists        => [ Bool            ],  #* just do nothing if it already exists? 
   };

   #* ----------
   #* Definitions for constraints in CreateStmt
   #* Note that column defaults are treated as a type of constraint,
   #* even though that's a bit odd semantically.
   #* For constraints that use expressions (CONSTR_CHECK, CONSTR_DEFAULT)
   #* we may have the expression in either "raw" form (an untransformed
   #* parse tree) or "cooked" form (the nodeToString representation of
   #* an executable expression tree), depending on how this Constraint
   #* node was created (by parsing, or by inheritance from an existing
   #* relation).  We should never have both in the same node!
   #* FKCONSTR_ACTION_xxx values are stored into pg_constraint.confupdtype
   #* and pg_constraint.confdeltype columns; FKCONSTR_MATCH_xxx values are
   #* stored into pg_constraint.confmatchtype.  Changing the code values may
   #* require an initdb!
   #* If skip_validation is true then we skip checking that the existing rows
   #* in the table satisfy the constraint, and just install the catalog entries
   #* for the constraint.  A new FK constraint is marked as valid iff
   #* initially_valid is true.  (Usually skip_validation and initially_valid
   #* are inverses, but we can set both true if the table is known empty.)
   #* Constraint attributes (DEFERRABLE etc) are initially represented as
   #* separate Constraint nodes for simplicity of parsing.  parse_utilcmd.c makes
   #* a pass through the constraints list to insert the info into the appropriate
   #* Constraint node.
   #* ----------

   typedef_enum 'ConstrType', [      #* types of constraints 
      'CONSTR_NULL',                #* not SQL92, but a lot of people expect it 
      'CONSTR_NOTNULL',
      'CONSTR_DEFAULT',
      'CONSTR_CHECK',
      'CONSTR_PRIMARY',
      'CONSTR_UNIQUE',
      'CONSTR_EXCLUSION',
      'CONSTR_FOREIGN',
      'CONSTR_ATTR_DEFERRABLE',     #* attributes for previous constraint node 
      'CONSTR_ATTR_NOT_DEFERRABLE',
      'CONSTR_ATTR_DEFERRED',
      'CONSTR_ATTR_IMMEDIATE'
   ], 0;

   typedef_enum '', [
      #* Foreign key action codes
      'FKCONSTR_ACTION_NOACTION   = a',
      'FKCONSTR_ACTION_RESTRICT   = r',
      'FKCONSTR_ACTION_CASCADE    = c',
      'FKCONSTR_ACTION_SETNULL    = n',
      'FKCONSTR_ACTION_SETDEFAULT = d',

      #* Foreign key matchtype codes 
      'FKCONSTR_MATCH_FULL        = f',
      'FKCONSTR_MATCH_PARTIAL     = p',
      'FKCONSTR_MATCH_SIMPLE      = s',
   ], 1;
}
BEGIN {
   typedef_struct 'Constraint', {
      contype              => [ ConstrType      ],  #* see above 

      #* Fields used for most/all constraint types: 
      conname              => [ Str             ],  #* Constraint name, or NULL if unnamed 
      deferrable           => [ Bool            ],  #* DEFERRABLE? 
      initdeferred         => [ Bool            ],  #* INITIALLY DEFERRED? 
      location             => [ HashRef         ],  #* token location, or -1 if unknown 

      #* Fields used for constraints with expressions (CHECK and DEFAULT): 
      is_no_inherit        => [ Bool            ],  #* is constraint non-inheritable? 
      raw_expr             => [ Any             ],  #* expr, as untransformed parse tree 
      cooked_expr          => [ Str             ],  #* expr, as nodeToString representation 

      #* Fields used for unique constraints (UNIQUE and PRIMARY KEY): 
      keys                 => [ ArrayRef        ],  #* String nodes naming referenced column(s) 

      #* Fields used for EXCLUSION constraints: 
      exclusions           => [ ArrayRef        ],  #* list of (IndexElem, operator name) pairs 

      #* Fields used for index constraints (UNIQUE, PRIMARY KEY, EXCLUSION): 
      options              => [ ArrayRef        ],  #* options from WITH clause 
      indexname            => [ Str             ],  #* existing index to use; otherwise NULL 
      indexspace           => [ Str             ],  #* index tablespace; NULL for default 
      #* These could be, but currently are not, used for UNIQUE/PKEY: 
      access_method        => [ Str             ],  #* index access method; NULL for default 
      where_clause         => [ Any             ],  #* partial index predicate 

      #* Fields used for FOREIGN KEY constraints: 
      pktable              => [ RangeVar        ],  #* Primary key table 
      fk_attrs             => [ ArrayRef        ],  #* Attributes of foreign key 
      pk_attrs             => [ ArrayRef        ],  #* Corresponding attrs in PK table 
      fk_matchtype         => [ Str             ],  #* FULL, PARTIAL, SIMPLE 
      fk_upd_action        => [ Str             ],  #* ON UPDATE action 
      fk_del_action        => [ Str             ],  #* ON DELETE action 
      old_conpfeqop        => [ ArrayRef        ],  #* pg_constraint.conpfeqop of my former self 

      #* Fields used for constraints that allow a NOT VALID specification 
      skip_validation      => [ Bool            ],  #* skip validation of existing rows? 
      initially_valid      => [ Bool            ],  #* mark the new constraint as valid? 
   };

   #* ----------------------
   #*      Create/Drop Table Space Statements
   #* ----------------------

   typedef_struct 'Statement::CreateTableSpace', {
      tablespacename       => [ Str             ],
      owner                => [ Str             ],
      location             => [ HashRef         ],
   };

   typedef_struct 'Statement::DropTableSpace', {
      tablespacename       => [ Str             ],
      missing_ok           => [ Bool            ],  #* skip error if missing? 
   };

   typedef_struct 'AlterTableSpaceOptions', {
      tablespacename       => [ Str             ],
      options              => [ ArrayRef        ],
      isReset              => [ Bool            ],
   };

   #* ----------------------
   #*      Create/Alter Extension Statements
   #* ----------------------

   typedef_struct 'Statement::CreateExtension', {
      extname              => [ Str             ],
      if_not_exists        => [ Bool            ],  #* just do nothing if it already exists? 
      options              => [ ArrayRef        ],  #* List of DefElem nodes 
   };

   #* Only used for ALTER EXTENSION UPDATE; later might need an action field 
   typedef_struct 'Statement::AlterExtension', {
      extname              => [ Str             ],
      options              => [ ArrayRef        ],  #* List of DefElem nodes 
   };

   typedef_struct 'AlterExtensionContents', {
      extname              => [ Str             ],  #* Extension's name 
      action               => [ Int             ],  #* +1 = add object, -1 = drop object 
      objtype              => [ ObjectType      ],  #* Object's type 
      objname              => [ ArrayRef        ],  #* Qualified name of the object 
      objargs              => [ ArrayRef        ],  #* Arguments if needed (eg, for functions) 
   };

   #* ----------------------
   #*      Create/Alter FOREIGN DATA WRAPPER Statements
   #* ----------------------

   typedef_struct 'Statement::CreateFdw', {
      fdwname              => [ Str             ],  #* foreign-data wrapper name 
      func_options         => [ ArrayRef        ],  #* HANDLER/VALIDATOR options 
      options              => [ ArrayRef        ],  #* generic options to FDW 
   };

   typedef_struct 'Statement::AlterFdw', {
      fdwname              => [ Str             ],  #* foreign-data wrapper name 
      func_options         => [ ArrayRef        ],  #* HANDLER/VALIDATOR options 
      options              => [ ArrayRef        ],  #* generic options to FDW 
   };

   #* ----------------------
   #*      Create/Alter FOREIGN SERVER Statements
   #* ----------------------

   typedef_struct 'Statement::CreateForeignServer', {
      servername           => [ Str             ],  #* server name 
      servertype           => [ Str             ],  #* optional server type 
      version              => [ Str             ],  #* optional server version 
      fdwname              => [ Str             ],  #* FDW name 
      options              => [ ArrayRef        ],  #* generic options to server 
   };

   typedef_struct 'Statement::AlterForeignServer', {
      servername           => [ Str             ],  #* server name 
      version              => [ Str             ],  #* optional server version 
      options              => [ ArrayRef        ],  #* generic options to server 
      has_version          => [ Bool            ],  #* version specified 
   };

   #* ----------------------
   #*      Create FOREIGN TABLE Statements
   #* ----------------------

   typedef_struct 'Statement::CreateForeignTable', {
      base                 => [ StatementCreate ],
      servername           => [ Str             ],
      options              => [ ArrayRef        ],
   };

   #* ----------------------
   #*      Create/Drop USER MAPPING Statements
   #* ----------------------

   typedef_struct 'Statement::CreateUserMapping', {
      username             => [ Str             ],  #* username or PUBLIC/CURRENT_USER 
      servername           => [ Str             ],  #* server name 
      options              => [ ArrayRef        ],  #* generic options to server 
   };

   typedef_struct 'Statement::AlterUserMapping', {
      username             => [ Str             ],  #* username or PUBLIC/CURRENT_USER 
      servername           => [ Str             ],  #* server name 
      options              => [ ArrayRef        ],  #* generic options to server 
   };

   typedef_struct 'Statement::DropUserMapping', {
      username             => [ Str             ],  #* username or PUBLIC/CURRENT_USER 
      servername           => [ Str             ],  #* server name 
      missing_ok           => [ Bool            ],  #* ignore missing mappings 
   };

   #* ----------------------
   #*      Create TRIGGER Statement
   #* ----------------------

   #* Bits within tgtype
   typedef_enum '', [
      'TRIGGER_TYPE_ROW         = (1 << 0)',
      'TRIGGER_TYPE_BEFORE      = (1 << 1)',
      'TRIGGER_TYPE_INSERT      = (1 << 2)',
      'TRIGGER_TYPE_DELETE      = (1 << 3)',
      'TRIGGER_TYPE_UPDATE      = (1 << 4)',
      'TRIGGER_TYPE_TRUNCATE    = (1 << 5)',
      'TRIGGER_TYPE_INSTEAD     = (1 << 6)',
      'TRIGGER_TYPE_LEVEL_MASK  = (TRIGGER_TYPE_ROW)',
      'TRIGGER_TYPE_STATEMENT   = 0',
      #* Note bits within TRIGGER_TYPE_TIMING_MASK aren't adjacent
      'TRIGGER_TYPE_TIMING_MASK = (TRIGGER_TYPE_BEFORE | TRIGGER_TYPE_INSTEAD)',
      'TRIGGER_TYPE_AFTER       = 0',
      'TRIGGER_TYPE_EVENT_MASK  = (TRIGGER_TYPE_INSERT | TRIGGER_TYPE_DELETE | TRIGGER_TYPE_UPDATE | TRIGGER_TYPE_TRUNCATE)',
   ], 1;
      
   typedef_struct 'Statement::CreateTrig', {
      trigname             => [ Str             ],  #* TRIGGER's name 
      relation             => [ RangeVar        ],  #* relation trigger is on 
      funcname             => [ ArrayRef        ],  #* qual. name of function to call 
      args                 => [ ArrayRef        ],  #* list of (T_String) Values or NIL 
      row                  => [ Bool            ],  #* ROW/STATEMENT 
      #* timing uses the TRIGGER_TYPE bits defined in catalog/pg_trigger.h 
      timing               => [ Int16           ],  #* BEFORE, AFTER, or INSTEAD 
      #* events uses the TRIGGER_TYPE bits defined in catalog/pg_trigger.h 
      events               => [ Int16           ],  #* "OR" of INSERT/UPDATE/DELETE/TRUNCATE 
      columns              => [ ArrayRef        ],  #* column names, or NIL for all columns 
      whenClause           => [ Any             ],  #* qual expression, or NULL if none 
      isconstraint         => [ Bool            ],  #* This is a constraint trigger 
      #* The remaining fields are only used for constraint triggers 
      deferrable           => [ Bool            ],  #* [NOT] DEFERRABLE 
      initdeferred         => [ Bool            ],  #* INITIALLY {DEFERRED|IMMEDIATE} 
      constrrel            => [ RangeVar        ],  #* opposite relation, if RI trigger 
   };

   #* ----------------------
   #*      Create PROCEDURAL LANGUAGE Statements
   #* ----------------------
   typedef_struct 'Statement::CreatePLang', {
      replace              => [ Bool            ],  #* T => replace if already exists 
      plname               => [ Str             ],  #* PL name 
      plhandler            => [ ArrayRef        ],  #* PL call handler function (qual. name) 
      plinline             => [ ArrayRef        ],  #* optional inline function (qual. name) 
      plvalidator          => [ ArrayRef        ],  #* optional validator function (qual. name) 
      pltrusted            => [ Bool            ],  #* PL is trusted 
   };

   #* ----------------------
   #*  Create/Alter/Drop Role Statements
   #* Note: these node types are also used for the backwards-compatible
   #* Create/Alter/Drop User/Group statements.  In the ALTER and DROP cases
   #* there's really no need to distinguish what the original spelling was,
   #* but for CREATE we mark the type because the defaults vary.
   #* ----------------------
   typedef_enum 'RoleStmtType', [
      'ROLESTMT_ROLE',
      'ROLESTMT_USER',
      'ROLESTMT_GROUP'
   ], 0;
}
BEGIN {
   typedef_struct 'Statement::CreateRole', {
      stmt_type            => [ RoleStmtType    ],  #* ROLE/USER/GROUP 
      role                 => [ Str             ],  #* role name 
      options              => [ ArrayRef        ],  #* List of DefElem nodes 
   };

   typedef_struct 'Statement::AlterRole', {
      role                 => [ Str             ],  #* role name 
      options              => [ ArrayRef        ],  #* List of DefElem nodes 
      action               => [ Int             ],  #* +1 = add members, -1 = drop members 
   };

   typedef_struct 'Statement::AlterRoleSet', {
      role                 => [ Str             ],  #* role name 
      database             => [ Str             ],  #* database name, or NULL 
      setstmt              => [ StatementVariableSet ],  #* SET or RESET subcommand 
   };

   typedef_struct 'Statement::DropRole', {
      roles                => [ ArrayRef        ],  #* List of roles to remove 
      missing_ok           => [ Bool            ],  #* skip error if a role is missing? 
   };

   #* ----------------------
   #*      {Create|Alter} SEQUENCE Statement
   #* ----------------------

   typedef_struct 'Statement::CreateSeq', {
      sequence             => [ RangeVar        ],  #* the sequence to create 
      options              => [ ArrayRef        ],
      ownerId              => [ UInt            ],  #* ID of owner, or InvalidOid for default 
   };

   typedef_struct 'Statement::AlterSeq', {
      sequence             => [ RangeVar        ],  #* the sequence to alter 
      options              => [ ArrayRef        ],
      missing_ok           => [ Bool            ],  #* skip error if a role is missing? 
   };

   #* ----------------------
   #*      Create {Aggregate|Operator|Type} Statement
   #* ----------------------
   typedef_struct 'Statement::Define', {
      kind                 => [ ObjectType      ],  #* aggregate, operator, type 
      oldstyle             => [ Bool            ],  #* hack to signal old CREATE AGG syntax 
      defnames             => [ ArrayRef        ],  #* qualified name (list of Value strings) 
      args                 => [ ArrayRef        ],  #* a list of TypeName (if needed) 
      definition           => [ ArrayRef        ],  #* a list of DefElem 
   };

   #* ----------------------
   #*      Create Domain Statement
   #* ----------------------
   typedef_struct 'Statement::CreateDomain', {
      domainname           => [ ArrayRef        ],  #* qualified name (list of Value strings) 
      typeName             => [ TypeName        ],  #* the base type 
      collClause           => [ CollateClause   ],  #* untransformed COLLATE spec, if any 
      constraints          => [ ArrayRef        ],  #* constraints (list of Constraint nodes) 
   };

   #* ----------------------
   #*      Create Operator Class Statement
   #* ----------------------
   typedef_struct 'Statement::CreateOpClass', {
      opclassname          => [ ArrayRef        ],  #* qualified name (list of Value strings) 
      opfamilyname         => [ ArrayRef        ],  #* qualified name (ditto); NIL if omitted 
      amname               => [ Str             ],  #* name of index AM opclass is for 
      datatype             => [ TypeName        ],  #* datatype of indexed column 
      items                => [ ArrayRef        ],  #* List of CreateOpClassItem nodes 
      isDefault            => [ Bool            ],  #* Should be marked as default for type? 
   };

   typedef_enum '', [
      'OPCLASS_ITEM_OPERATOR    = OPERATOR',
      'OPCLASS_ITEM_FUNCTION    = FUNCTION',
      'OPCLASS_ITEM_STORAGETYPE = STORAGETYPE',
   ], 1;

   typedef_struct 'CreateOpClass::Item', {
      itemtype             => [ Str             ],  #* see codes above 
      #* fields used for an operator or function item: 
      name                 => [ ArrayRef        ],  #* operator or function name 
      args                 => [ ArrayRef        ],  #* argument types 
      number               => [ Int             ],  #* strategy num or support proc num 
      order_family         => [ ArrayRef        ],  #* only used for ordering operators 
      class_args           => [ ArrayRef        ],  #* only used for functions 
      #* fields used for a storagetype item: 
      storedtype           => [ TypeName        ],  #* datatype stored in index 
   };

   #* ----------------------
   #*      Create Operator Family Statement
   #* ----------------------
   typedef_struct 'Statement::CreateOpFamily', {
      opfamilyname         => [ ArrayRef        ],  #* qualified name (list of Value strings) 
      amname               => [ Str             ],  #* name of index AM opfamily is for 
   };

   #* ----------------------
   #*      Alter Operator Family Statement
   #* ----------------------
   typedef_struct 'Statement::AlterOpFamily', {
      opfamilyname         => [ ArrayRef        ],  #* qualified name (list of Value strings) 
      amname               => [ Str             ],  #* name of index AM opfamily is for 
      isDrop               => [ Bool            ],  #* ADD or DROP the items? 
      items                => [ ArrayRef        ],  #* List of CreateOpClassItem nodes 
   };

   #* ----------------------
   #*      Drop Table|Sequence|View|Index|Type|Domain|Conversion|Schema Statement
   #* ----------------------

   typedef_struct 'Statement::Drop', {
      objects              => [ ArrayRef        ],  #* list of sublists of names (as Values) 
      arguments            => [ ArrayRef        ],  #* list of sublists of arguments (as Values) 
      removeType           => [ ObjectType      ],  #* object type 
      behavior             => [ DropBehavior    ],  #* RESTRICT or CASCADE behavior 
      missing_ok           => [ Bool            ],  #* skip error if object is missing? 
      concurrent           => [ Bool            ],  #* drop index concurrently? 
   };

   #* ----------------------
   #*              Truncate Table Statement
   #* ----------------------
   typedef_struct 'Statement::Truncate', {
      relations            => [ ArrayRef        ],  #* relations (RangeVars) to be truncated 
      restart_seqs         => [ Bool            ],  #* restart owned sequences? 
      behavior             => [ DropBehavior    ],  #* RESTRICT or CASCADE behavior 
   };

   #* ----------------------
   #*              Comment On Statement
   #* ----------------------
   typedef_struct 'Statement::Comment', {
      objtype              => [ ObjectType      ],  #* Object's type 
      objname              => [ ArrayRef        ],  #* Qualified name of the object 
      objargs              => [ ArrayRef        ],  #* Arguments if needed (eg, for functions) 
      comment              => [ Str             ],  #* Comment to insert, or NULL to remove 
   };

   #* ----------------------
   #*              SECURITY LABEL Statement
   #* ----------------------
   typedef_struct 'Statement::SecLabel', {
      objtype              => [ ObjectType      ],  #* Object's type 
      objname              => [ ArrayRef        ],  #* Qualified name of the object 
      objargs              => [ ArrayRef        ],  #* Arguments if needed (eg, for functions) 
      provider             => [ Str             ],  #* Label provider (or NULL) 
      label                => [ Str             ],  #* New security label to be assigned 
   };

   #* ----------------------
   #*      Declare Cursor Statement
   #* Note: the "query" field of DeclareCursorStmt is only used in the raw grammar
   #* output.  After parse analysis it's set to null, and the Query points to the
   #* DeclareCursorStmt, not vice versa.
   #* ----------------------
   typedef_enum '', [
      'CURSOR_OPT_BINARY=1',      #* BINARY 
      'CURSOR_OPT_SCROLL',        #* SCROLL explicitly given 
      'CURSOR_OPT_NO_SCROLL',     #* NO SCROLL explicitly given 
      'CURSOR_OPT_INSENSITIVE',   #* INSENSITIVE 
      'CURSOR_OPT_HOLD',          #* WITH HOLD 
      #* these planner-control flags do not correspond to any SQL grammar: 
      'CURSOR_OPT_FAST_PLAN',     #* prefer fast-start plan 
      'CURSOR_OPT_GENERIC_PLAN',  #* force use of generic plan 
      'CURSOR_OPT_CUSTOM_PLAN',   #* force use of custom plan 
   ], 1;

   typedef_struct 'Statement::DeclareCursor', {
      portalname           => [ Str             ],  #* name of the portal (cursor) 
      options              => [ Int             ],  #* bitmask of options (see above) 
      query                => [ Any             ],  #* the raw SELECT query 
   };

   #* ----------------------
   #*      Close Portal Statement
   #* ----------------------
   typedef_struct 'Statement::ClosePortal', {
      portalname           => [ Str             ],  #* name of the portal (cursor) 
      #* NULL means CLOSE ALL 
   };

   #* ----------------------
   #*      Fetch Statement (also Move)
   #* ----------------------
   typedef_enum 'FetchDirection', [
      #* for these, howMany is how many rows to fetch; FETCH_ALL means ALL 
      'FETCH_FORWARD',
      'FETCH_BACKWARD',
      #* for these, howMany indicates a position; only one row is fetched 
      'FETCH_ABSOLUTE',
      'FETCH_RELATIVE',
      'FETCH_ALL',
   ], 0;
}
BEGIN {
   typedef_struct 'Statement::Fetch', {
      direction            => [ FetchDirection  ],  #* see above 
      howMany              => [ Long            ],  #* number of rows, or position argument 
      portalname           => [ Str             ],  #* name of portal (cursor) 
      ismove               => [ Bool            ],  #* TRUE if MOVE 
   };

   #* ----------------------
   #*      Create Index Statement
   #* This represents creation of an index and/or an associated constraint.
   #* If indexOid isn't InvalidOid, we are not creating an index, just a
   #* UNIQUE/PKEY constraint using an existing index.  isconstraint must always
   #* be true in this case, and the fields describing the index properties are
   #* empty.
   #* ----------------------
   typedef_struct 'Statement::Index', {
      idxname              => [ Str             ],  #* name of new index, or NULL for default 
      relation             => [ RangeVar        ],  #* relation to build index on 
      accessMethod         => [ Str             ],  #* name of access method (eg. btree) 
      tableSpace           => [ Str             ],  #* tablespace, or NULL for default 
      indexParams          => [ ArrayRef        ],  #* a list of IndexElem 
      options              => [ ArrayRef        ],  #* options from WITH clause 
      whereClause          => [ Any             ],  #* qualification (partial-index predicate) 
      excludeOpNames       => [ ArrayRef        ],  #* exclusion operator names, or NIL if none 
      indexOid             => [ UInt            ],  #* OID of an existing index, if any 
      oldNode              => [ UInt            ],  #* relfilenode of my former self 
      unique               => [ Bool            ],  #* is index unique? 
      primary              => [ Bool            ],  #* is index on primary key? 
      isconstraint         => [ Bool            ],  #* is it from a CONSTRAINT clause? 
      deferrable           => [ Bool            ],  #* is the constraint DEFERRABLE? 
      initdeferred         => [ Bool            ],  #* is the constraint INITIALLY DEFERRED? 
      concurrent           => [ Bool            ],  #* should this be a concurrent index build? 
   };

   #* ----------------------
   #*      Create Function Statement
   #* ----------------------
   typedef_struct 'Statement::CreateFunction', {
      replace              => [ Bool            ],  #* T => replace if already exists 
      funcname             => [ ArrayRef        ],  #* qualified name of function to create 
      parameters           => [ ArrayRef        ],  #* a list of FunctionParameter 
      returnType           => [ TypeName        ],  #* the return type 
      options              => [ ArrayRef        ],  #* a list of DefElem 
      withClause           => [ ArrayRef        ],  #* a list of DefElem 
   };

   typedef_enum 'FunctionParameterMode', [
      'FUNC_PARAM_IN',        #* input only 
      'FUNC_PARAM_OUT',       #* output only 
      'FUNC_PARAM_INOUT',     #* both 
      'FUNC_PARAM_VARIADIC',  #* variadic (always input) 
      'FUNC_PARAM_TABLE'      #* table function output column 
   ], 0;
}
BEGIN {
   typedef_struct 'Function::Parameter', {
      name                 => [ Str             ],  #* parameter name, or NULL if not given 
      argType              => [ TypeName        ],  #* TypeName for parameter type 
      mode                 => [ FunctionParameterMode ],  #* IN/OUT/etc 
      defexpr              => [ Any             ],  #* raw default expr, or NULL if not given 
   };

   typedef_struct 'AlterFunction', {
      func                 => [ Function        ],  #* name and args of function 
      actions              => [ ArrayRef        ],  #* list of DefElem 
   };

   #* ----------------------
   #*      DO Statement
   #* DoStmt is the raw parser output, InlineCodeBlock is the execution-time API
   #* ----------------------
   typedef_struct 'Statement::Do', {
      args                 => [ ArrayRef        ],  #* List of DefElem nodes 
   };

   ### RIP: InlineCodeBlock ###

   #* ----------------------
   #*      Alter Object Rename Statement
   #* ----------------------
   typedef_struct 'Statement::Rename', {
      renameType           => [ ObjectType      ],  #* OBJECT_TABLE, OBJECT_COLUMN, etc 
      relationType         => [ ObjectType      ],  #* if column name, associated relation type 
      relation             => [ RangeVar        ],  #* in case it's a table 
      object               => [ ArrayRef        ],  #* in case it's some other object 
      objarg               => [ ArrayRef        ],  #* argument types, if applicable 
      subname              => [ Str             ],  #* name of contained object (column, rule,
                                                    #* trigger, etc) 
      newname              => [ Str             ],  #* the new name 
      behavior             => [ DropBehavior    ],  #* RESTRICT or CASCADE behavior 
      missing_ok           => [ Bool            ],  #* skip error if missing? 
   };

   #* ----------------------
   #*      ALTER object SET SCHEMA Statement
   #* ----------------------
   typedef_struct 'Statement::AlterObjectSchema', {
      objectType           => [ ObjectType      ],  #* OBJECT_TABLE, OBJECT_TYPE, etc 
      relation             => [ RangeVar        ],  #* in case it's a table 
      object               => [ ArrayRef        ],  #* in case it's some other object 
      objarg               => [ ArrayRef        ],  #* argument types, if applicable 
      addname              => [ Str             ],  #* additional name if needed 
      newschema            => [ Str             ],  #* the new schema 
      missing_ok           => [ Bool            ],  #* skip error if missing? 
   };

   #* ----------------------
   #*      Alter Object Owner Statement
   #* ----------------------
   typedef_struct 'Statement::AlterOwner', {
      objectType           => [ ObjectType      ],  #* OBJECT_TABLE, OBJECT_TYPE, etc 
      relation             => [ RangeVar        ],  #* in case it's a table 
      object               => [ ArrayRef        ],  #* in case it's some other object 
      objarg               => [ ArrayRef        ],  #* argument types, if applicable 
      addname              => [ Str             ],  #* additional name if needed 
      newowner             => [ Str             ],  #* the new owner 
   };


   #* ----------------------
   #*      Create Rule Statement
   #* ----------------------
   typedef_struct 'Statement::Rule', {
      relation             => [ RangeVar        ],  #* relation the rule is for 
      rulename             => [ Str             ],  #* name of the rule 
      whereClause          => [ Any             ],  #* qualifications 
      event                => [ CmdType         ],  #* SELECT, INSERT, etc 
      instead              => [ Bool            ],  #* is a 'do instead'? 
      actions              => [ ArrayRef        ],  #* the action statements 
      replace              => [ Bool            ],  #* OR REPLACE 
   };

   #* ----------------------
   #*      Notify Statement
   #* ----------------------
   typedef_struct 'Statement::Notify', {
      conditionname        => [ Str             ],  #* condition name to notify 
      payload              => [ Str             ],  #* the payload string, or NULL if none 
   };

   #* ----------------------
   #*      Listen Statement
   #* ----------------------
   typedef_struct 'Statement::Listen', {
      conditionname        => [ Str             ],  #* condition name to listen on 
   };

   #* ----------------------
   #*      Unlisten Statement
   #* ----------------------
   typedef_struct 'Statement::Unlisten', {
      conditionname        => [ Str             ],  #* name to unlisten on, or NULL for all 
   };

   #* ----------------------
   #*      {Begin|Commit|Rollback} Transaction Statement
   #* ----------------------
   typedef_enum 'TransactionStmtKind', [
      'TRANS_STMT_BEGIN',
      'TRANS_STMT_START',           #* semantically identical to BEGIN 
      'TRANS_STMT_COMMIT',
      'TRANS_STMT_ROLLBACK',
      'TRANS_STMT_SAVEPOINT',
      'TRANS_STMT_RELEASE',
      'TRANS_STMT_ROLLBACK_TO',
      'TRANS_STMT_PREPARE',
      'TRANS_STMT_COMMIT_PREPARED',
      'TRANS_STMT_ROLLBACK_PREPARED'
   ], 0;
}
BEGIN {
   typedef_struct 'Statement::Transaction', {
      kind                 => [ TransactionStmtKind ],  #* see above 
      options              => [ ArrayRef        ],  #* for BEGIN/START and savepoint commands 
      gid                  => [ Str             ],  #* for two-phase-commit related commands 
   };

   #* ----------------------
   #*      Create Type Statement, composite types
   #* ----------------------
   typedef_struct 'Statement::CompositeType', {
      typevar              => [ RangeVar        ],  #* the composite type to be created 
      coldeflist           => [ ArrayRef        ],  #* list of ColumnDef nodes 
   };

   #* ----------------------
   #*      Create Type Statement, enum types
   #* ----------------------
   typedef_struct 'Statement::CreateEnum', {
      typeName             => [ ArrayRef        ],  #* qualified name (list of Value strings) 
      vals                 => [ ArrayRef        ],  #* enum values (list of Value strings) 
   };

   #* ----------------------
   #*      Create Type Statement, range types
   #* ----------------------
   typedef_struct 'Statement::CreateRange', {
      typeName             => [ ArrayRef        ],  #* qualified name (list of Value strings) 
      params               => [ ArrayRef        ],  #* range parameters (list of DefElem) 
   };

   #* ----------------------
   #*      Alter Type Statement, enum types
   #* ----------------------
   typedef_struct 'Statement::AlterEnum', {
      typeName             => [ ArrayRef        ],  #* qualified name (list of Value strings) 
      newVal               => [ Str             ],  #* new enum value's name 
      newValNeighbor       => [ Str             ],  #* neighboring enum value, if specified 
      newValIsAfter        => [ Bool            ],  #* place new enum value after neighbor? 
   };

   #* ----------------------
   #*      Create View Statement
   #* ----------------------
   typedef_struct 'Statement::View', {
      view                 => [ RangeVar        ],  #* the view to be created 
      aliases              => [ ArrayRef        ],  #* target column names 
      query                => [ Any             ],  #* the SELECT query 
      replace              => [ Bool            ],  #* replace an existing view? 
      options              => [ ArrayRef        ],  #* options from WITH clause 
   };

   #* ----------------------
   #*      Load Statement
   #* ----------------------
   typedef_struct 'Statement::Load', {
      filename             => [ Str             ],  #* file to load 
   };

   #* ----------------------
   #*      Createdb Statement
   #* ----------------------
   typedef_struct 'Statement::Createdb', {
      dbname               => [ Str             ],  #* name of database to create 
      options              => [ ArrayRef        ],  #* List of DefElem nodes 
   };

   #* ----------------------
   #*  Alter Database
   #* ----------------------
   typedef_struct 'Statement::AlterDatabase', {
      dbname               => [ Str             ],  #* name of database to alter 
      options              => [ ArrayRef        ],  #* List of DefElem nodes 
   };

   typedef_struct 'AlterDatabaseSet', {
      dbname               => [ Str             ],  #* database name 
      setstmt              => [ StatementVariableSet ],  #* SET or RESET subcommand 
   };

   #* ----------------------
   #*      Dropdb Statement
   #* ----------------------
   typedef_struct 'Statement::Dropdb', {
      dbname               => [ Str             ],  #* database to drop 
      missing_ok           => [ Bool            ],  #* skip error if db is missing? 
   };

   #* ----------------------
   #*      Cluster Statement (support pbrown's cluster index implementation)
   #* ----------------------
   typedef_struct 'Statement::Cluster', {
      relation             => [ RangeVar        ],  #* relation being indexed, or NULL if all 
      indexname            => [ Str             ],  #* original index defined 
      verbose              => [ Bool            ],  #* print progress info 
   };

   #* ----------------------
   #*      Vacuum and Analyze Statements
   #* Even though these are nominally two statements, it's convenient to use
   #* just one node type for both.  Note that at least one of VACOPT_VACUUM
   #* and VACOPT_ANALYZE must be set in options.  VACOPT_FREEZE is an internal
   #* convenience for the grammar and is not examined at runtime --- the
   #* freeze_min_age and freeze_table_age fields are what matter.
   #* ----------------------
   typedef_enum 'VacuumOption', [
      'VACOPT_VACUUM  = 1 << 0',    #* do VACUUM 
      'VACOPT_ANALYZE = 1 << 1',    #* do ANALYZE 
      'VACOPT_VERBOSE = 1 << 2',    #* print progress info 
      'VACOPT_FREEZE  = 1 << 3',    #* FREEZE option 
      'VACOPT_FULL    = 1 << 4',    #* FULL (non-concurrent) vacuum 
      'VACOPT_NOWAIT  = 1 << 5'     #* don't wait to get lock (autovacuum only) 
   ], 1;

   typedef_struct 'Statement::Vacuum', {
      options              => [ Int             ],  #* OR of VacuumOption flags 
      freeze_min_age       => [ Int             ],  #* min freeze age, or -1 to use default 
      freeze_table_age     => [ Int             ],  #* age at which to scan whole table 
      relation             => [ RangeVar        ],  #* single table to process, or NULL 
      va_cols              => [ ArrayRef        ],  #* list of column names, or NIL for all 
   };

   #* ----------------------
   #*      Explain Statement
   #* The "query" field is either a raw parse tree (SelectStmt, InsertStmt, etc)
   #* or a Query node if parse analysis has been done.  Note that rewriting and
   #* planning of the query are always postponed until execution of EXPLAIN.
   #* ----------------------
   typedef_struct 'Statement::Explain', {
      query                => [ Any             ],  #* the query (see comments above) 
      options              => [ ArrayRef        ],  #* list of DefElem nodes 
   };

   #* ----------------------
   #*      CREATE TABLE AS Statement (a/k/a SELECT INTO)
   #* A query written as CREATE TABLE AS will produce this node type natively.
   #* A query written as SELECT ... INTO will be transformed to this form during
   #* parse analysis.
   #* The "query" field is handled similarly to EXPLAIN, though note that it
   #* can be a SELECT or an EXECUTE, but not other DML statements.
   #* ----------------------
   typedef_struct 'Statement::CreateTableAs', {
      query                => [ Any             ],  #* the query (see comments above) 
      into                 => [ IntoClause      ],  #* destination table 
      is_select_into       => [ Bool            ],  #* it was written as SELECT INTO 
   };

   #* ----------------------
   #* Checkpoint Statement
   #* ----------------------
   typedef_struct 'Statement::CheckPoint', {
   };

   #* ----------------------
   #* Discard Statement
   #* ----------------------

   typedef_enum 'DiscardMode', [
      'DISCARD_ALL',
      'DISCARD_PLANS',
      'DISCARD_TEMP'
   ], 0;
}
BEGIN {
   typedef_struct 'Statement::Discard', {
      target               => [ DiscardMode     ],
   };

   #* ----------------------
   #*      LOCK Statement
   #* ----------------------
   typedef_struct 'Statement::Lock', {
      relations            => [ ArrayRef        ],  #* relations to lock 
      mode                 => [ Str             ],  #* lock mode 
      nowait               => [ Bool            ],  #* no wait mode 
   };

   #* ----------------------
   #*      SET CONSTRAINTS Statement
   #* ----------------------
   typedef_struct 'Statement::ConstraintsSet', {
      constraints          => [ ArrayRef        ],  #* List of names as RangeVars 
      deferred             => [ Bool            ],
   };

   #* ----------------------
   #*      REINDEX Statement
   #* ----------------------
   typedef_struct 'Statement::Reindex', {
      kind                 => [ ObjectType      ],  #* OBJECT_INDEX, OBJECT_TABLE, OBJECT_DATABASE 
      relation             => [ RangeVar        ],  #* Table or index to reindex 
      name                 => [ Str             ],  #* name of database to reindex 
      do_system            => [ Bool            ],  #* include system tables in database case 
      do_user              => [ Bool            ],  #* include user tables in database case 
   };

   #* ----------------------
   #*      CREATE CONVERSION Statement
   #* ----------------------
   typedef_struct 'Statement::CreateConversion', {
      conversion_name      => [ ArrayRef        ],  #* Name of the conversion 
      for_encoding_name    => [ Str             ],  #* source encoding name 
      to_encoding_name     => [ Str             ],  #* destination encoding name 
      func_name            => [ ArrayRef        ],  #* qualified conversion function name 
      def                  => [ Bool            ],  #* is this a default conversion? 
   };

   #* ----------------------
   #*  CREATE CAST Statement
   #* ----------------------
   typedef_struct 'Statement::CreateCast', {
      sourcetype           => [ TypeName        ],
      targettype           => [ TypeName        ],
      func                 => [ Function        ],
      context              => [ CoercionContext ],
      inout                => [ Bool            ],
   };

   #* ----------------------
   #*      PREPARE Statement
   #* ----------------------
   typedef_struct 'Statement::Prepare', {
      name                 => [ Str             ],  #* Name of plan, arbitrary 
      argtypes             => [ ArrayRef        ],  #* Types of parameters (List of TypeName) 
      query                => [ Any             ],  #* The query itself (as a raw parsetree) 
   };


   #* ----------------------
   #*      EXECUTE Statement
   #* ----------------------
   typedef_struct 'Statement::Execute', {
      name                 => [ Str             ],  #* The name of the plan to execute 
      params               => [ ArrayRef        ],  #* Values to assign to parameters 
   };

   #* ----------------------
   #*      DEALLOCATE Statement
   #* ----------------------
   typedef_struct 'Statement::Deallocate', {
      name                 => [ Str             ],  #* The name of the plan to remove 
      #* NULL means DEALLOCATE ALL 
   };

   #*      DROP OWNED statement
   typedef_struct 'Statement::DropOwned', {
      roles                => [ ArrayRef        ],
      behavior             => [ DropBehavior    ],
   };

   #*      REASSIGN OWNED statement
   typedef_struct 'Statement::ReassignOwned', {
      roles                => [ ArrayRef        ],
      newrole              => [ Str             ],
   };

   #* TS Dictionary stmts: DefineStmt, RenameStmt and DropStmt are default
   typedef_struct 'Statement::AlterTSDictionary', {
      dictname             => [ ArrayRef        ],  #* qualified name (list of Value strings) 
      options              => [ ArrayRef        ],  #* List of DefElem nodes 
   };

   #* TS Configuration stmts: DefineStmt, RenameStmt and DropStmt are default
   typedef_struct 'Statement::AlterTSConfiguration', {
      cfgname              => [ ArrayRef        ],  #* qualified name (list of Value strings) 

      #* dicts will be non-NIL if ADD/ALTER MAPPING was specified. If dicts is
      #* NIL, but tokentype isn't, DROP MAPPING was specified.
      tokentype            => [ ArrayRef        ],  #* list of Value strings 
      dicts                => [ ArrayRef        ],  #* list of list of Value strings 
      override             => [ Bool            ],  #* if true - remove old variant 
      replace              => [ Bool            ],  #* if true - replace dictionary by another 
      missing_ok           => [ Bool            ],  #* for DROP - skip error if missing? 
   };
}

#endif   #* PARSENODES_H    

# use Data::Dump 'dd';
# use Package::Subroutine::Namespace; 
# my @childs = sort Package::Subroutine::Namespace->list_childs('SQL::Converter::PIL');
# dd(\@childs, scalar @childs);

42;