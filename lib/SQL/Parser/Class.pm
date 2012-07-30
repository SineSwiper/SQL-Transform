package SQL::Parser::Classes::Neo;

# Based and converted from PostgreSQL's src/include/nodes/*nodes.h

# Most comments retained from that source, marked as #*.

  ###########################################################################
### Instead of C++'s typedef structs, we will use packages with Moo support 
### to fill in the properties.  Here are the rules:
###
### * Until we get a better idea of what we are dealing with, no 'required'
###   properties.
### * (is => 'rwp'), since these structs might need to be modified by later
###   by the parser, but should otherwise be untouched afterwords.
### * Remove the first Nodetag->type property, since that's redundant with
###   the class name.
### 
  ###########################################################################

# Okay, this looks really weird, but it makes translating these typedefs
# to Moo classes a LOT easier...

use Moo;
use MooX::Types::MooseLike::Base  qw(Str Bool ArrayRef HashRef Object);
use MooX::Types::CLike            qw(:c :stdint);
use Scalar::Util 'blessed';

sub typedef_struct ($$) {
   my ($subpackage, $type_hash) = @_;
   my $class_type = $subpackage;
   $subpackage =~ s/Stmt$//;

   # New class type definition
   MooX::Types::MooseLike::register_types([{
      name       => $class_type,
      subtype_of => 'Object',
      from       => 'MooX::Types::MooseLike::Base',
      test       => sub { blessed($_[0]) and $_[0]->isa("SQL::Statement::$subpackage") },
      message    => sub { "Object is not a SQL::Statement::$subpackage class!" },
   }], __PACKAGE__);
   
   # package scoping
   package SQL::Statement::$subpackage;
   
   use Moo;
   
   foreach my $name (sort keys %$type_hash) {
      my ($isa, $default, $required, $weak) = @{$type_hash->{$name}};
      my @has_params;
      
      push @has_params, is => 'rwp';
      
      # Because we're passing types hashrefs instead of named subs, we
      # can get away with not using/exporting the MooX::Types stuff.
      push @has_params, isa      => $isa      if $isa;
      push @has_params, default  => $default  if $default;
      push @has_params, required => $required if $required;
      push @has_params, weak_ref => 1         if $weak;
      
      has $name => @has_params;
   };
}

### In order to add some level of readability into the result trees, we 
### try to use real strings as much as possible for enums, ie: the parser
### isn't using them for bitwise math.

### Of course, if it can't be found in either the lexer/parser, we just
### remove it.

use enum::hash 'enum';

my $constant_list = {};
sub typedef_enum ($$;$) {
   my ($name, $enum_array, $is_bitmask) = @_;

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
         $constant_list->{$n} = $val;
      }
   }
   else {
      unshift @$enum_array, '~';  # tell enum::hash to use a bitmask
      my %enum = enum @$enum_array;
      $constant_list->{$_} = $enum{$_} for (keys %enum);
   }
}

sub longest_common_prefix {
   my $prefix = shift;
   for (@_) {
      chop $prefix until (/^\Q$prefix\E/);
   }
   return $prefix;
}

#############################################################################

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

#* Alias -
#*    specifies an alias for a range variable; the alias might also
#*    specify renaming of columns within the table.
#* Note: colnames is a list of Value nodes (always strings).  In Alias structs
#* associated with RTEs, there may be entries corresponding to dropped
#* columns; these are normally empty strings ("").  See parsenodes.h for info.
typedef_struct 'Alias' {
   aliasname            => [ Str             ],  #* aliased rel name (never qualified) 
   colnames             => [ ArrayRef        ],  #* optional list of column aliases 
};

typedef_enum 'InhOption' [
   'INH_NO',                     #* Do NOT scan child tables 
   'INH_YES',                    #* DO scan child tables 
   'INH_DEFAULT'                 #* Use current SQL_inheritance option 
] 0;

#* What to do at commit time for temporary relations 
typedef_enum 'OnCommitAction' [
   'ONCOMMIT_NOOP',              #* No ON COMMIT clause (do nothing) 
   'ONCOMMIT_PRESERVE_ROWS',     #* ON COMMIT PRESERVE ROWS (do nothing) 
   'ONCOMMIT_DELETE_ROWS',       #* ON COMMIT DELETE ROWS 
   'ONCOMMIT_DROP'               #* ON COMMIT DROP 
] 0;

#* RangeVar - range variable, used in FROM clauses
#* Also used to represent table names in utility statements; there, the alias
#* field is not used, and inhOpt shows whether to apply the operation
#* recursively to child tables.  In some contexts it is also useful to carry
#* a TEMP table indication here.
typedef_struct 'RangeVar' {
   catalogname          => [ Str             ],  #* the catalog (database) name, or NULL 
   schemaname           => [ Str             ],  #* the schema name, or NULL 
   relname              => [ Str             ],  #* the relation/sequence name 
   inhOpt               => [ InhOption       ],  #* expand rel by inheritance? recursively act
                                                 #* on children? 
   relpersistence       => [ Str             ],  #* see RELPERSISTENCE_* in pg_class.h 
   alias                => [ Alias           ],  #* table alias & optional column aliases 
   location             => [ HashRef         ],  #* token location, or -1 if unknown 
};

#* IntoClause - target information for SELECT INTO and CREATE TABLE AS
typedef_struct 'IntoClause' {
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
typedef_struct 'Expr' {
};

#* Var - expression node representing a variable (ie, a table column)
#* Note: during parsing/planning, varnoold/varoattno are always just copies
#* of varno/varattno.  At the tail end of planning, Var nodes appearing in
#* upper-level plan nodes are reassigned to point to the outputs of their
#* subplans; for example, in a join node varno becomes INNER_VAR or OUTER_VAR
#* and varattno becomes the index of the proper element of that subplan's
#* target list.  But varnoold/varoattno continue to hold the original values.
#* The code doesn't really need varnoold/varoattno, but they are very useful
#* for debugging and interpreting completed plans, so we keep them around.
#define    INNER_VAR        65000       #* reference to inner subplan 
#define    OUTER_VAR        65001       #* reference to outer subplan 
#define    INDEX_VAR        65002       #* reference to index column 

#define IS_SPECIAL_VARNO(varno)     ((varno) >= INNER_VAR)

#* Symbols for the indexes of the special RTE entries in rules 
#define    PRS2_OLD_VARNO           1
#define    PRS2_NEW_VARNO           2

typedef_struct 'Var' {
   xpr                  => [ Expr            ],
   varno                => [ Index           ],  #* index of this var's relation in the range
                                                 #* table, or INNER_VAR/OUTER_VAR/INDEX_VAR 
   varattno             => [ AttrNumber      ],  #* attribute number of this var, or zero for
                                                 #* all 
   vartype              => [ UInt            ],  #* pg_type OID for the type of this var 
   vartypmod            => [ Int32           ],  #* pg_attribute typmod value 
   varcollid            => [ UInt            ],  #* OID of collation, or InvalidOid if none 
   varlevelsup          => [ Index           ],  #* for subquery variables referencing outer
                                                 #* relations; 0 in a normal var, >0 means N
                                                 #* levels up 
   varnoold             => [ Index           ],  #* original value of varno, for debugging 
   varoattno            => [ AttrNumber      ],  #* original value of varattno 
   location             => [ HashRef         ],  #* token location, or -1 if unknown 
};

#* Const
typedef_struct 'Const' {
   xpr                  => [ Expr            ],
   consttype            => [ UInt            ],  #* pg_type OID of the constant's datatype 
   consttypmod          => [ Int32           ],  #* typmod value, if any 
   constcollid          => [ UInt            ],  #* OID of collation, or InvalidOid if none 
   constlen             => [ Int             ],  #* typlen of the constant's datatype 
   constvalue           => [ Datum           ],  #* the constant's value 
   constisnull          => [ Bool            ],  #* whether the constant is null (if true,
                                                 #* constvalue is undefined) 
   constbyval           => [ Bool            ],  #* whether this datatype is passed by value.
                                                 #* If true, then all the information is stored
                                                 #* in the Datum. If false, then the Datum
                                                 #* contains a pointer to the information. 
   location             => [ HashRef         ],  #* token location, or -1 if unknown 
};

#* ----------------
#* Param
#*      paramkind - specifies the kind of parameter. The possible values
#*      for this field are:
#*      PARAM_EXTERN:  The parameter value is supplied from outside the plan.
#*              Such parameters are numbered from 1 to n.
#*      PARAM_EXEC:  The parameter is an internal executor parameter, used
#*              for passing values into and out of sub-queries or from
#*              nestloop joins to their inner scans.
#*              For historical reasons, such parameters are numbered from 0.
#*              These numbers are independent of PARAM_EXTERN numbers.
#*      PARAM_SUBLINK:  The parameter represents an output column of a SubLink
#*              node's sub-select.  The column number is contained in the
#*              `paramid' field.  (This type of Param is converted to
#*              PARAM_EXEC during planning.)
#* Note: currently, paramtypmod is valid for PARAM_SUBLINK Params, and for
#* PARAM_EXEC Params generated from them; it is always -1 for PARAM_EXTERN
#* params, since the APIs that supply values for such parameters don't carry
#* any typmod info.
#* ----------------
typedef_enum 'ParamKind' [
   'PARAM_EXTERN',
   'PARAM_EXEC',
   'PARAM_SUBLINK'
] 0;

typedef_struct 'Param' {
   xpr                  => [ Expr            ],
   paramkind            => [ ParamKind       ],  #* kind of parameter. See above 
   paramid              => [ Int             ],  #* numeric ID for parameter 
   paramtype            => [ UInt            ],  #* pg_type OID of parameter's datatype 
   paramtypmod          => [ Int32           ],  #* typmod value, if known 
   paramcollid          => [ UInt            ],  #* OID of collation, or InvalidOid if none 
   location             => [ HashRef         ],  #* token location, or -1 if unknown 
};

#* Aggref
#* The aggregate's args list is a targetlist, ie, a list of TargetEntry nodes
#* (before Postgres 9.0 it was just bare expressions).  The non-resjunk TLEs
#* represent the aggregate's regular arguments (if any) and resjunk TLEs can
#* be added at the end to represent ORDER BY expressions that are not also
#* arguments.  As in a top-level Query, the TLEs can be marked with
#* ressortgroupref indexes to let them be referenced by SortGroupClause
#* entries in the aggorder and/or aggdistinct lists.  This represents ORDER BY
#* and DISTINCT operations to be applied to the aggregate input rows before
#* they are passed to the transition function.  The grammar only allows a
#* simple "DISTINCT" specifier for the arguments, but we use the full
#* query-level representation to allow more code sharing.
typedef_struct 'Aggref' {
   xpr                  => [ Expr            ],
   aggfnoid             => [ UInt            ],  #* pg_proc Oid of the aggregate 
   aggtype              => [ UInt            ],  #* type Oid of result of the aggregate 
   aggcollid            => [ UInt            ],  #* OID of collation of result 
   inputcollid          => [ UInt            ],  #* OID of collation that function should use 
   args                 => [ ArrayRef        ],  #* arguments and sort expressions 
   aggorder             => [ ArrayRef        ],  #* ORDER BY (list of SortGroupClause) 
   aggdistinct          => [ ArrayRef        ],  #* DISTINCT (list of SortGroupClause) 
   aggstar              => [ Bool            ],  #* TRUE if argument list was really '*' 
   agglevelsup          => [ Index           ],  #* > 0 if agg belongs to outer query 
   location             => [ HashRef         ],  #* token location, or -1 if unknown 
};

#* WindowFunc
typedef_struct 'WindowFunc' {
   xpr                  => [ Expr            ],
   winfnoid             => [ UInt            ],  #* pg_proc Oid of the function 
   wintype              => [ UInt            ],  #* type Oid of result of the window function 
   wincollid            => [ UInt            ],  #* OID of collation of result 
   inputcollid          => [ UInt            ],  #* OID of collation that function should use 
   args                 => [ ArrayRef        ],  #* arguments to the window function 
   winref               => [ Index           ],  #* index of associated WindowClause 
   winstar              => [ Bool            ],  #* TRUE if argument list was really '*' 
   winagg               => [ Bool            ],  #* is function a simple aggregate? 
   location             => [ HashRef         ],  #* token location, or -1 if unknown 
};

#* ----------------
#*  ArrayRef: describes an array subscripting operation
#* An ArrayRef can describe fetching a single element from an array,
#* fetching a subarray (array slice), storing a single element into
#* an array, or storing a slice.  The "store" cases work with an
#* initial array value and a source value that is inserted into the
#* appropriate part of the array; the result of the operation is an
#* entire new modified array value.
#* If reflowerindexpr = NIL, then we are fetching or storing a single array
#* element at the subscripts given by refupperindexpr.  Otherwise we are
#* fetching or storing an array slice, that is a rectangular subarray
#* with lower and upper bounds given by the index expressions.
#* reflowerindexpr must be the same length as refupperindexpr when it
#* is not NIL.
#* Note: the result datatype is the element type when fetching a single
#* element; but it is the array type when doing subarray fetch or either
#* type of store.
#* ----------------
typedef_struct 'ArrayRef' {
   xpr                  => [ Expr            ],
   refarraytype         => [ UInt            ],  #* type of the array proper 
   refelemtype          => [ UInt            ],  #* type of the array elements 
   reftypmod            => [ Int32           ],  #* typmod of the array (and elements too) 
   refcollid            => [ UInt            ],  #* OID of collation, or InvalidOid if none 
   refupperindexpr      => [ ArrayRef        ],  #* expressions that evaluate to upper array
                                                 #* indexes 
   reflowerindexpr      => [ ArrayRef        ],  #* expressions that evaluate to lower array
                                                 #* indexes 
   refexpr              => [ Expr            ],  #* the expression that evaluates to an array
                                                 #* value 
   refassgnexpr         => [ Expr            ],  #* expression for the source value, or NULL if
                                                 #* fetch 
};

#* CoercionContext - distinguishes the allowed set of type casts
#* NB: ordering of the alternatives is significant; later (larger) values
#* allow more casts than earlier ones.
typedef_enum 'CoercionContext' [
   'COERCION_IMPLICIT',          #* coercion in context of expression 
   'COERCION_ASSIGNMENT',        #* coercion in context of assignment 
   'COERCION_EXPLICIT'           #* explicit cast operation 
] 0;

#* CoercionForm - information showing how to display a function-call node
typedef_enum 'CoercionForm' [
   'COERCE_EXPLICIT_CALL',       #* display as a function call 
   'COERCE_EXPLICIT_CAST',       #* display as an explicit cast 
   'COERCE_IMPLICIT_CAST',       #* implicit cast, so hide it 
   'COERCE_DONTCARE'             #* special case for planner 
] 0;

#* FuncExpr - expression node for a function call
typedef_struct 'FuncExpr' {
   xpr                  => [ Expr            ],
   funcid               => [ UInt            ],  #* PG_PROC OID of the function 
   funcresulttype       => [ UInt            ],  #* PG_TYPE OID of result value 
   funcretset           => [ Bool            ],  #* true if function returns set 
   funcformat           => [ CoercionForm    ],  #* how to display this function call 
   funccollid           => [ UInt            ],  #* OID of collation of result 
   inputcollid          => [ UInt            ],  #* OID of collation that function should use 
   args                 => [ ArrayRef        ],  #* arguments to the function 
   location             => [ HashRef         ],  #* token location, or -1 if unknown 
};

#* NamedArgExpr - a named argument of a function
#* This node type can only appear in the args list of a FuncCall or FuncExpr
#* node.  We support pure positional call notation (no named arguments),
#* named notation (all arguments are named), and mixed notation (unnamed
#* arguments followed by named ones).
#* Parse analysis sets argnumber to the positional index of the argument,
#* but doesn't rearrange the argument list.
#* The planner will convert argument lists to pure positional notation
#* during expression preprocessing, so execution never sees a NamedArgExpr.
typedef_struct 'NamedArgExpr' {
   xpr                  => [ Expr            ],
   arg                  => [ Expr            ],  #* the argument expression 
   name                 => [ Str             ],  #* the name 
   argnumber            => [ Int             ],  #* argument's number in positional notation 
   location             => [ HashRef         ],  #* argument name location, or -1 if unknown 
};

#* OpExpr - expression node for an operator invocation
#* Semantically, this is essentially the same as a function call.
#* Note that opfuncid is not necessarily filled in immediately on creation
#* of the node.  The planner makes sure it is valid before passing the node
#* tree to the executor, but during parsing/planning opfuncid can be 0.
typedef_struct 'OpExpr' {
   xpr                  => [ Expr            ],
   opno                 => [ UInt            ],  #* PG_OPERATOR OID of the operator 
   opfuncid             => [ UInt            ],  #* PG_PROC OID of underlying function 
   opresulttype         => [ UInt            ],  #* PG_TYPE OID of result value 
   opretset             => [ Bool            ],  #* true if operator returns set 
   opcollid             => [ UInt            ],  #* OID of collation of result 
   inputcollid          => [ UInt            ],  #* OID of collation that operator should use 
   args                 => [ ArrayRef        ],  #* arguments to the operator (1 or 2) 
   location             => [ HashRef         ],  #* token location, or -1 if unknown 
};

#* DistinctExpr - expression node for "x IS DISTINCT FROM y"
#* Except for the nodetag, this is represented identically to an OpExpr
#* referencing the "=" operator for x and y.
#* We use "=", not the more obvious "<>", because more datatypes have "="
#* than "<>".  This means the executor must invert the operator result.
#* Note that the operator function won't be called at all if either input
#* is NULL, since then the result can be determined directly.
typedef OpExpr DistinctExpr;

#* NullIfExpr - a NULLIF expression
#* Like DistinctExpr, this is represented the same as an OpExpr referencing
#* the "=" operator for x and y.
typedef OpExpr NullIfExpr;

#* ScalarArrayOpExpr - expression node for "scalar op ANY/ALL (array)"
#* The operator must yield boolean.  It is applied to the left operand
#* and each element of the righthand array, and the results are combined
#* with OR or AND (for ANY or ALL respectively).  The node representation
#* is almost the same as for the underlying operator, but we need a useOr
#* flag to remember whether it's ANY or ALL, and we don't have to store
#* the result type (or the collation) because it must be boolean.
typedef_struct 'ScalarArrayOpExpr' {
   xpr                  => [ Expr            ],
   opno                 => [ UInt            ],  #* PG_OPERATOR OID of the operator 
   opfuncid             => [ UInt            ],  #* PG_PROC OID of underlying function 
   useOr                => [ Bool            ],  #* true for ANY, false for ALL 
   inputcollid          => [ UInt            ],  #* OID of collation that operator should use 
   args                 => [ ArrayRef        ],  #* the scalar and array operands 
   location             => [ HashRef         ],  #* token location, or -1 if unknown 
};

#* BoolExpr - expression node for the basic Boolean operators AND, OR, NOT
#* Notice the arguments are given as a List.  For NOT, of course the list
#* must always have exactly one element.  For AND and OR, the executor can
#* handle any number of arguments.  The parser generally treats AND and OR
#* as binary and so it typically only produces two-element lists, but the
#* optimizer will flatten trees of AND and OR nodes to produce longer lists
#* when possible.  There are also a few special cases where more arguments
#* can appear before optimization.
typedef_enum 'BoolExprType' [
   'AND_EXPR', OR_EXPR, NOT_EXPR
] 0;

typedef_struct 'BoolExpr' {
   xpr                  => [ Expr            ],
   boolop               => [ BoolExprType    ],
   args                 => [ ArrayRef        ],  #* arguments to this expression 
   location             => [ HashRef         ],  #* token location, or -1 if unknown 
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
typedef_enum 'SubLinkType' [
   'EXISTS_SUBLINK',
   'ALL_SUBLINK',
   'ANY_SUBLINK',
   'ROWCOMPARE_SUBLINK',
   'EXPR_SUBLINK',
   'ARRAY_SUBLINK',
   'CTE_SUBLINK'                 #* for SubPlans only 
] 0;


typedef_struct 'SubLink' {
   xpr                  => [ Expr            ],
   subLinkType          => [ SubLinkType     ],  #* see above 
   testexpr             => [ Any             ],  #* outer-query test for ALL/ANY/ROWCOMPARE 
   operName             => [ ArrayRef        ],  #* originally specified operator name 
   subselect            => [ Any             ],  #* subselect as Query* or parsetree 
   location             => [ HashRef         ],  #* token location, or -1 if unknown 
};

#* SubPlan - executable expression node for a subplan (sub-SELECT)
#* The planner replaces SubLink nodes in expression trees with SubPlan
#* nodes after it has finished planning the subquery.  SubPlan references
#* a sub-plantree stored in the subplans list of the toplevel PlannedStmt.
#* (We avoid a direct link to make it easier to copy expression trees
#* without causing multiple processing of the subplan.)
#* In an ordinary subplan, testexpr points to an executable expression
#* (OpExpr, an AND/OR tree of OpExprs, or RowCompareExpr) for the combining
#* operator(s); the left-hand arguments are the original lefthand expressions,
#* and the right-hand arguments are PARAM_EXEC Param nodes representing the
#* outputs of the sub-select.  (NOTE: runtime coercion functions may be
#* inserted as well.)  This is just the same expression tree as testexpr in
#* the original SubLink node, but the PARAM_SUBLINK nodes are replaced by
#* suitably numbered PARAM_EXEC nodes.
#* If the sub-select becomes an initplan rather than a subplan, the executable
#* expression is part of the outer plan's expression tree (and the SubPlan
#* node itself is not, but rather is found in the outer plan's initPlan
#* list).  In this case testexpr is NULL to avoid duplication.
#* The planner also derives lists of the values that need to be passed into
#* and out of the subplan.  Input values are represented as a list "args" of
#* expressions to be evaluated in the outer-query context (currently these
#* args are always just Vars, but in principle they could be any expression).
#* The values are assigned to the global PARAM_EXEC params indexed by parParam
#* (the parParam and args lists must have the same ordering).  setParam is a
#* list of the PARAM_EXEC params that are computed by the sub-select, if it
#* is an initplan; they are listed in order by sub-select output column
#* position.  (parParam and setParam are integer Lists, not Bitmapsets,
#* because their ordering is significant.)
#* Also, the planner computes startup and per-call costs for use of the
#* SubPlan.  Note that these include the cost of the subquery proper,
#* evaluation of the testexpr if any, and any hashtable management overhead.
typedef_struct 'SubPlan' {
   xpr                  => [ Expr            ],
   #* Fields copied from original SubLink: 
   subLinkType          => [ SubLinkType     ],  #* see above 
   #* The combining operators, transformed to an executable expression: 
   testexpr             => [ Any             ],  #* OpExpr or RowCompareExpr expression tree 
   paramIds             => [ ArrayRef        ],  #* IDs of Params embedded in the above 
   #* Identification of the Plan tree to use: 
   plan_id              => [ Int             ],  #* Index (from 1) in PlannedStmt.subplans 
   #* Identification of the SubPlan for EXPLAIN and debugging purposes: 
   plan_name            => [ Str             ],  #* A name assigned during planning 
   #* Extra data useful for determining subplan's output type: 
   firstColType         => [ UInt            ],  #* Type of first column of subplan result 
   firstColTypmod       => [ Int32           ],  #* Typmod of first column of subplan result 
   firstColCollation    => [ UInt            ],  #* Collation of first column of
                                        #* subplan result 
   #* Information about execution strategy: 
   useHashTable         => [ Bool            ],  #* TRUE to store subselect output in a hash
                                                 #* table (implies we are doing "IN") 
   unknownEqFalse       => [ Bool            ],  #* TRUE if it's okay to return FALSE when the
                                                 #* spec result is UNKNOWN; this allows much
                                                 #* simpler handling of null values 
   #* Information for passing params into and out of the subselect: 
   #* setParam and parParam are lists of integers (param IDs) 
   setParam             => [ ArrayRef        ],  #* initplan subqueries have to set these
                                                 #* Params for parent plan 
   parParam             => [ ArrayRef        ],  #* indices of input Params from parent plan 
   args                 => [ ArrayRef        ],  #* exprs to pass as parParam values 
   #* Estimated execution costs: 
   startup_cost         => [ Cost            ],  #* one-time setup cost 
   per_call_cost        => [ Cost            ],  #* cost for each subplan evaluation 
};

#* AlternativeSubPlan - expression node for a choice among SubPlans
#* The subplans are given as a List so that the node definition need not
#* change if there's ever more than two alternatives.  For the moment,
#* though, there are always exactly two; and the first one is the fast-start
#* plan.
typedef_struct 'AlternativeSubPlan' {
   xpr                  => [ Expr            ],
   subplans             => [ ArrayRef        ],  #* SubPlan(s) with equivalent results 
};

#* ----------------
#* FieldSelect
#* FieldSelect represents the operation of extracting one field from a tuple
#* value.  At runtime, the input expression is expected to yield a rowtype
#* Datum.  The specified field number is extracted and returned as a Datum.
#* ----------------

typedef_struct 'FieldSelect' {
   xpr                  => [ Expr            ],
   arg                  => [ Expr            ],  #* input expression 
   fieldnum             => [ AttrNumber      ],  #* attribute number of field to extract 
   resulttype           => [ UInt            ],  #* type of the field (result type of this
                                                 #* node) 
   resulttypmod         => [ Int32           ],  #* output typmod (usually -1) 
   resultcollid         => [ UInt            ],  #* OID of collation of the field 
};

#* ----------------
#* FieldStore
#* FieldStore represents the operation of modifying one field in a tuple
#* value, yielding a new tuple value (the input is not touched!).  Like
#* the assign case of ArrayRef, this is used to implement UPDATE of a
#* portion of a column.
#* A single FieldStore can actually represent updates of several different
#* fields.  The parser only generates FieldStores with single-element lists,
#* but the planner will collapse multiple updates of the same base column
#* into one FieldStore.
#* ----------------

typedef_struct 'FieldStore' {
   xpr                  => [ Expr            ],
   arg                  => [ Expr            ],  #* input tuple value 
   newvals              => [ ArrayRef        ],  #* new value(s) for field(s) 
   fieldnums            => [ ArrayRef        ],  #* integer list of field attnums 
   resulttype           => [ UInt            ],  #* type of result (same as type of arg) 
   #* Like RowExpr, we deliberately omit a typmod and collation here 
};

#* ----------------
#* RelabelType
#* RelabelType represents a "dummy" type coercion between two binary-
#* compatible datatypes, such as reinterpreting the result of an OID
#* expression as an int4.  It is a no-op at runtime; we only need it
#* to provide a place to store the correct type to be attributed to
#* the expression result during type resolution.  (We can't get away
#* with just overwriting the type field of the input expression node,
#* so we need a separate node to show the coercion's result type.)
#* ----------------

typedef_struct 'RelabelType' {
   xpr                  => [ Expr            ],
   arg                  => [ Expr            ],  #* input expression 
   resulttype           => [ UInt            ],  #* output type of coercion expression 
   resulttypmod         => [ Int32           ],  #* output typmod (usually -1) 
   resultcollid         => [ UInt            ],  #* OID of collation, or InvalidOid if none 
   relabelformat        => [ CoercionForm    ],  #* how to display this node 
   location             => [ HashRef         ],  #* token location, or -1 if unknown 
};

#* ----------------
#* CoerceViaIO
#* CoerceViaIO represents a type coercion between two types whose textual
#* representations are compatible, implemented by invoking the source type's
#* typoutput function then the destination type's typinput function.
#* ----------------

typedef_struct 'CoerceViaIO' {
   xpr                  => [ Expr            ],
   arg                  => [ Expr            ],  #* input expression 
   resulttype           => [ UInt            ],  #* output type of coercion 
   #* output typmod is not stored, but is presumed -1 
   resultcollid         => [ UInt            ],  #* OID of collation, or InvalidOid if none 
   coerceformat         => [ CoercionForm    ],  #* how to display this node 
   location             => [ HashRef         ],  #* token location, or -1 if unknown 
};

#* ----------------
#* ArrayCoerceExpr
#* ArrayCoerceExpr represents a type coercion from one array type to another,
#* which is implemented by applying the indicated element-type coercion
#* function to each element of the source array.  If elemfuncid is InvalidOid
#* then the element types are binary-compatible, but the coercion still
#* requires some effort (we have to fix the element type ID stored in the
#* array header).
#* ----------------

typedef_struct 'ArrayCoerceExpr' {
   xpr                  => [ Expr            ],
   arg                  => [ Expr            ],  #* input expression (yields an array) 
   elemfuncid           => [ UInt            ],  #* OID of element coercion function, or 0 
   resulttype           => [ UInt            ],  #* output type of coercion (an array type) 
   resulttypmod         => [ Int32           ],  #* output typmod (also element typmod) 
   resultcollid         => [ UInt            ],  #* OID of collation, or InvalidOid if none 
   isExplicit           => [ Bool            ],  #* conversion semantics flag to pass to func 
   coerceformat         => [ CoercionForm    ],  #* how to display this node 
   location             => [ HashRef         ],  #* token location, or -1 if unknown 
};

#* ----------------
#* ConvertRowtypeExpr
#* ConvertRowtypeExpr represents a type coercion from one composite type
#* to another, where the source type is guaranteed to contain all the columns
#* needed for the destination type plus possibly others; the columns need not
#* be in the same positions, but are matched up by name.  This is primarily
#* used to convert a whole-row value of an inheritance child table into a
#* valid whole-row value of its parent table's rowtype.
#* ----------------

typedef_struct 'ConvertRowtypeExpr' {
   xpr                  => [ Expr            ],
   arg                  => [ Expr            ],  #* input expression 
   resulttype           => [ UInt            ],  #* output type (always a composite type) 
   #* Like RowExpr, we deliberately omit a typmod and collation here 
   convertformat        => [ CoercionForm    ],  #* how to display this node 
   location             => [ HashRef         ],  #* token location, or -1 if unknown 
};

#*----------
#* CollateExpr - COLLATE
#* The planner replaces CollateExpr with RelabelType during expression
#* preprocessing, so execution never sees a CollateExpr.
#*----------
typedef_struct 'CollateExpr' {
   xpr                  => [ Expr            ],
   arg                  => [ Expr            ],  #* input expression 
   collOid              => [ UInt            ],  #* collation's OID 
   location             => [ HashRef         ],  #* token location, or -1 if unknown 
};

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
typedef_struct 'CaseExpr' {
   xpr                  => [ Expr            ],
   casetype             => [ UInt            ],  #* type of expression result 
   casecollid           => [ UInt            ],  #* OID of collation, or InvalidOid if none 
   arg                  => [ Expr            ],  #* implicit equality comparison argument 
   args                 => [ ArrayRef        ],  #* the arguments (list of WHEN clauses) 
   defresult            => [ Expr            ],  #* the default result (ELSE clause) 
   location             => [ HashRef         ],  #* token location, or -1 if unknown 
};

#* CaseWhen - one arm of a CASE expression
typedef_struct 'CaseWhen' {
   xpr                  => [ Expr            ],
   expr                 => [ Expr            ],  #* condition expression 
   result               => [ Expr            ],  #* substitution result 
   location             => [ HashRef         ],  #* token location, or -1 if unknown 
};

#* Placeholder node for the test value to be processed by a CASE expression.
#* This is effectively like a Param, but can be implemented more simply
#* since we need only one replacement value at a time.
#* We also use this in nested UPDATE expressions.
#* See transformAssignmentIndirection().
typedef_struct 'CaseTestExpr' {
   xpr                  => [ Expr            ],
   typeId               => [ UInt            ],  #* type for substituted value 
   typeMod              => [ Int32           ],  #* typemod for substituted value 
   collation            => [ UInt            ],  #* collation for the substituted value 
};

#* ArrayExpr - an ARRAY[] expression
#* Note: if multidims is false, the constituent expressions all yield the
#* scalar type identified by element_typeid.  If multidims is true, the
#* constituent expressions all yield arrays of element_typeid (ie, the same
#* type as array_typeid); at runtime we must check for compatible subscripts.
typedef_struct 'ArrayExpr' {
   xpr                  => [ Expr            ],
   array_typeid         => [ UInt            ],  #* type of expression result 
   array_collid         => [ UInt            ],  #* OID of collation, or InvalidOid if none 
   element_typeid       => [ UInt            ],  #* common type of array elements 
   elements             => [ ArrayRef        ],  #* the array elements or sub-arrays 
   multidims            => [ Bool            ],  #* true if elements are sub-arrays 
   location             => [ HashRef         ],  #* token location, or -1 if unknown 
};

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
typedef_struct 'RowExpr' {
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

#* RowCompareExpr - row-wise comparison, such as (a, b) <= (1, 2)
#* We support row comparison for any operator that can be determined to
#* act like =, <>, <, <=, >, or >= (we determine this by looking for the
#* operator in btree opfamilies).  Note that the same operator name might
#* map to a different operator for each pair of row elements, since the
#* element datatypes can vary.
#* A RowCompareExpr node is only generated for the < <= > >= cases;
#* the = and <> cases are translated to simple AND or OR combinations
#* of the pairwise comparisons.  However, we include = and <> in the
#* RowCompareType enum for the convenience of parser logic.
typedef_enum 'RowCompareType' [
    #* Values of this enum are chosen to match btree strategy numbers 
   'ROWCOMPARE_LT = 1',          #* BTLessStrategyNumber 
   'ROWCOMPARE_LE = 2',          #* BTLessEqualStrategyNumber 
   'ROWCOMPARE_EQ = 3',          #* BTEqualStrategyNumber 
   'ROWCOMPARE_GE = 4',          #* BTGreaterEqualStrategyNumber 
   'ROWCOMPARE_GT = 5',          #* BTGreaterStrategyNumber 
   'ROWCOMPARE_NE = 6'           #* no such btree strategy 
] 0;

typedef_struct 'RowCompareExpr' {
   xpr                  => [ Expr            ],
   rctype               => [ RowCompareType  ],  #* LT LE GE or GT, never EQ or NE 
   opnos                => [ ArrayRef        ],  #* OID list of pairwise comparison ops 
   opfamilies           => [ ArrayRef        ],  #* OID list of containing operator families 
   inputcollids         => [ ArrayRef        ],  #* OID list of collations for comparisons 
   largs                => [ ArrayRef        ],  #* the left-hand input arguments 
   rargs                => [ ArrayRef        ],  #* the right-hand input arguments 
};

#* CoalesceExpr - a COALESCE expression
typedef_struct 'CoalesceExpr' {
   xpr                  => [ Expr            ],
   coalescetype         => [ UInt            ],  #* type of expression result 
   coalescecollid       => [ UInt            ],  #* OID of collation, or InvalidOid if none 
   args                 => [ ArrayRef        ],  #* the arguments 
   location             => [ HashRef         ],  #* token location, or -1 if unknown 
};

#* MinMaxExpr - a GREATEST or LEAST function
typedef_enum 'MinMaxOp' [
   'IS_GREATEST',
   'IS_LEAST'
] 0;

typedef_struct 'MinMaxExpr' {
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
typedef_enum 'XmlExprOp' [
   'IS_XMLCONCAT',               #* XMLCONCAT(args) 
   'IS_XMLELEMENT',              #* XMLELEMENT(name, xml_attributes, args) 
   'IS_XMLFOREST',               #* XMLFOREST(xml_attributes) 
   'IS_XMLPARSE',                #* XMLPARSE(text, is_doc, preserve_ws) 
   'IS_XMLPI',                   #* XMLPI(name [, args]) 
   'IS_XMLROOT',                 #* XMLROOT(xml, version, standalone) 
   'IS_XMLSERIALIZE',            #* XMLSERIALIZE(is_document, xmlval) 
   'IS_DOCUMENT'                 #* xmlval IS DOCUMENT 
] 0;

typedef enum 'XmlOptionType' [
   'XMLOPTION_DOCUMENT',
   'XMLOPTION_CONTENT'
] 0;

typedef_struct 'XmlExpr' {
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

typedef_enum 'NullTestType' [
   'IS_NULL',
   'IS_NOT_NULL'
] 0;

typedef_struct 'NullTest' {
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

typedef_enum 'BoolTestType' [
   'IS_TRUE', IS_NOT_TRUE, IS_FALSE, IS_NOT_FALSE, IS_UNKNOWN, IS_NOT_UNKNOWN
] 0;

typedef_struct 'BooleanTest' {
   xpr                  => [ Expr            ],
   arg                  => [ Expr            ],  #* input expression 
   booltesttype         => [ BoolTestType    ],  #* test type 
};

#* CoerceToDomain
#* CoerceToDomain represents the operation of coercing a value to a domain
#* type.  At runtime (and not before) the precise set of constraints to be
#* checked will be determined.  If the value passes, it is returned as the
#* result; if not, an error is raised.  Note that this is equivalent to
#* RelabelType in the scenario where no constraints are applied.
typedef_struct 'CoerceToDomain' {
   xpr                  => [ Expr            ],
   arg                  => [ Expr            ],  #* input expression 
   resulttype           => [ UInt            ],  #* domain type ID (result type) 
   resulttypmod         => [ Int32           ],  #* output typmod (currently always -1) 
   resultcollid         => [ UInt            ],  #* OID of collation, or InvalidOid if none 
   coercionformat       => [ CoercionForm    ],  #* how to display this node 
   location             => [ HashRef         ],  #* token location, or -1 if unknown 
};

#* Placeholder node for the value to be processed by a domain's check
#* constraint.  This is effectively like a Param, but can be implemented more
#* simply since we need only one replacement value at a time.
#* Note: the typeId/typeMod/collation will be set from the domain's base type,
#* not the domain itself.  This is because we shouldn't consider the value
#* to be a member of the domain if we haven't yet checked its constraints.
typedef_struct 'CoerceToDomainValue' {
   xpr                  => [ Expr            ],
   typeId               => [ UInt            ],  #* type for substituted value 
   typeMod              => [ Int32           ],  #* typemod for substituted value 
   collation            => [ UInt            ],  #* collation for the substituted value 
   location             => [ HashRef         ],  #* token location, or -1 if unknown 
};

#* Placeholder node for a DEFAULT marker in an INSERT or UPDATE command.
#* This is not an executable expression: it must be replaced by the actual
#* column default expression during rewriting.  But it is convenient to
#* treat it as an expression node during parsing and rewriting.
typedef_struct 'SetToDefault' {
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
typedef_struct 'CurrentOfExpr' {
   xpr                  => [ Expr            ],
   cvarno               => [ Index           ],  #* RT index of target relation 
   cursor_name          => [ Str             ],  #* name of referenced cursor, or NULL 
   cursor_param         => [ Int             ],  #* refcursor parameter number, or 0 
};

#*--------------------
#* TargetEntry -
#*     a target entry (used in query target lists)
#* Strictly speaking, a TargetEntry isn't an expression node (since it can't
#* be evaluated by ExecEvalExpr).  But we treat it as one anyway, since in
#* very many places it's convenient to process a whole query targetlist as a
#* single expression tree.
#* In a SELECT's targetlist, resno should always be equal to the item's
#* ordinal position (counting from 1).  However, in an INSERT or UPDATE
#* targetlist, resno represents the attribute number of the destination
#* column for the item; so there may be missing or out-of-order resnos.
#* It is even legal to have duplicated resnos; consider
#*      UPDATE table SET arraycol[1] = ..., arraycol[2] = ..., ...
#* The two meanings come together in the executor, because the planner
#* transforms INSERT/UPDATE tlists into a normalized form with exactly
#* one entry for each column of the destination table.  Before that's
#* happened, however, it is risky to assume that resno == position.
#* Generally get_tle_by_resno() should be used rather than list_nth()
#* to fetch tlist entries by resno, and only in SELECT should you assume
#* that resno is a unique identifier.
#* resname is required to represent the correct column name in non-resjunk
#* entries of top-level SELECT targetlists, since it will be used as the
#* column title sent to the frontend.  In most other contexts it is only
#* a debugging aid, and may be wrong or even NULL.  (In particular, it may
#* be wrong in a tlist from a stored rule, if the referenced column has been
#* renamed by ALTER TABLE since the rule was made.  Also, the planner tends
#* to store NULL rather than look up a valid name for tlist entries in
#* non-toplevel plan nodes.)  In resjunk entries, resname should be either
#* a specific system-generated name (such as "ctid") or NULL; anything else
#* risks confusing ExecGetJunkAttribute!
#* ressortgroupref is used in the representation of ORDER BY, GROUP BY, and
#* DISTINCT items.  Targetlist entries with ressortgroupref=0 are not
#* sort/group items.  If ressortgroupref>0, then this item is an ORDER BY,
#* GROUP BY, and/or DISTINCT target value.  No two entries in a targetlist
#* may have the same nonzero ressortgroupref --- but there is no particular
#* meaning to the nonzero values, except as tags.  (For example, one must
#* not assume that lower ressortgroupref means a more significant sort key.)
#* The order of the associated SortGroupClause lists determine the semantics.
#* resorigtbl/resorigcol identify the source of the column, if it is a
#* simple reference to a column of a base table (or view).  If it is not
#* a simple reference, these fields are zeroes.
#* If resjunk is true then the column is a working column (such as a sort key)
#* that should be removed from the final output of the query.  Resjunk columns
#* must have resnos that cannot duplicate any regular column's resno.  Also
#* note that there are places that assume resjunk columns come after non-junk
#* columns.
#*--------------------
typedef_struct 'TargetEntry' {
   xpr                  => [ Expr            ],
   expr                 => [ Expr            ],  #* expression to evaluate 
   resno                => [ AttrNumber      ],  #* attribute number (see notes above) 
   resname              => [ Str             ],  #* name of the column (could be NULL) 
   ressortgroupref      => [ Index           ],  #* nonzero if referenced by a sort/group
                                                 #* clause 
   resorigtbl           => [ UInt            ],  #* OID of column's source table 
   resorigcol           => [ AttrNumber      ],  #* column's number in source table 
   resjunk              => [ Bool            ],  #* set to true to eliminate the attribute from
                                                 #* final target list 
};


#* ----------------------------------------------------------------
#*                  node types for join trees
#* The leaves of a join tree structure are RangeTblRef nodes.  Above
#* these, JoinExpr nodes can appear to denote a specific kind of join
#* or qualified join.  Also, FromExpr nodes can appear to denote an
#* ordinary cross-product join ("FROM foo, bar, baz WHERE ...").
#* FromExpr is like a JoinExpr of jointype JOIN_INNER, except that it
#* may have any number of child nodes, not just two.
#* NOTE: the top level of a Query's jointree is always a FromExpr.
#* Even if the jointree contains no rels, there will be a FromExpr.
#* NOTE: the qualification expressions present in JoinExpr nodes are
#* *in addition to* the query's main WHERE clause, which appears as the
#* qual of the top-level FromExpr.  The reason for associating quals with
#* specific nodes in the jointree is that the position of a qual is critical
#* when outer joins are present.  (If we enforce a qual too soon or too late,
#* that may cause the outer join to produce the wrong set of NULL-extended
#* rows.)  If all joins are inner joins then all the qual positions are
#* semantically interchangeable.
#* NOTE: in the raw output of gram.y, a join tree contains RangeVar,
#* RangeSubselect, and RangeFunction nodes, which are all replaced by
#* RangeTblRef nodes during the parse analysis phase.  Also, the top-level
#* FromExpr is added during parse analysis; the grammar regards FROM and
#* WHERE as separate.
#* ----------------------------------------------------------------

#* RangeTblRef - reference to an entry in the query's rangetable
#* We could use direct pointers to the RT entries and skip having these
#* nodes, but multiple pointers to the same node in a querytree cause
#* lots of headaches, so it seems better to store an index into the RT.
typedef_struct 'RangeTblRef' {
   rtindex              => [ Int             ],
};

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
typedef_struct 'JoinExpr' {
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
typedef_struct 'FromExpr' {
   fromlist             => [ ArrayRef        ],  #* List of join subtrees 
   quals                => [ Any             ],  #* qualifiers on join, if any 
};

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

#* Sort ordering options for ORDER BY and CREATE INDEX 
typedef_enum 'SortByDir' [
   'SORTBY_DEFAULT',
   'SORTBY_ASC',
   'SORTBY_DESC',
   'SORTBY_USING'                #* not allowed in CREATE INDEX ... 
] 0;

typedef_enum 'SortByNulls' [
   'SORTBY_NULLS_DEFAULT',
   'SORTBY_NULLS_FIRST',
   'SORTBY_NULLS_LAST'
] 0;


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
typedef_struct 'TypeName' {
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
typedef_struct 'ColumnRef' {
   fields               => [ ArrayRef        ],  #* field names (Value strings) or A_Star 
   location             => [ HashRef         ],  #* token location, or -1 if unknown 
};

#* ParamRef - specifies a $n parameter reference
typedef_struct 'ParamRef' {
   number               => [ Int             ],  #* the number of the parameter 
   location             => [ HashRef         ],  #* token location, or -1 if unknown 
};

#* A_Expr - infix, prefix, and postfix expressions
typedef_enum 'A_Expr_Kind' [
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
] 0;

typedef_struct 'A_Expr' {
   kind                 => [ A_Expr_Kind     ],  #* see above 
   name                 => [ ArrayRef        ],  #* possibly-qualified name of operator 
   lexpr                => [ Any             ],  #* left argument, or NULL if none 
   rexpr                => [ Any             ],  #* right argument, or NULL if none 
   location             => [ HashRef         ],  #* token location, or -1 if unknown 
};

#* A_Const - a literal constant
typedef_struct 'A_Const' {
   val                  => [ Value           ],  #* value (includes type info, see value.h) 
   location             => [ HashRef         ],  #* token location, or -1 if unknown 
};

#* TypeCast - a CAST expression
typedef_struct 'TypeCast' {
   arg                  => [ Any             ],  #* the expression being casted 
   typeName             => [ TypeName        ],  #* the target type 
   location             => [ HashRef         ],  #* token location, or -1 if unknown 
};

#* CollateClause - a COLLATE expression
typedef_struct 'CollateClause' {
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
typedef_struct 'FuncCall' {
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
typedef_struct 'A_Star' {
};

#* A_Indices - array subscript or slice bounds ([lidx:uidx] or [uidx])
typedef_struct 'A_Indices' {
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
typedef_struct 'A_Indirection' {
   arg                  => [ Any             ],  #* the thing being selected from 
   indirection          => [ ArrayRef        ],  #* subscripts and/or field names and/or * 
};

#* A_ArrayExpr - an ARRAY[] construct
typedef_struct 'A_ArrayExpr' {
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
typedef_struct 'ResTarget' {
   name                 => [ Str             ],  #* column name or NULL 
   indirection          => [ ArrayRef        ],  #* subscripts, field names, and '*', or NIL 
   val                  => [ Any             ],  #* the value expression to compute or assign 
   location             => [ HashRef         ],  #* token location, or -1 if unknown 
};

#* SortBy - for ORDER BY clause
typedef_struct 'SortBy' {
   node                 => [ Any             ],  #* expression to sort on 
   sortby_dir           => [ SortByDir       ],  #* ASC/DESC/USING/default 
   sortby_nulls         => [ SortByNulls     ],  #* NULLS FIRST/LAST 
   useOp                => [ ArrayRef        ],  #* name of op to use, if SORTBY_USING 
   location             => [ HashRef         ],  #* operator location, or -1 if none/unknown 
};

#* WindowDef - raw representation of WINDOW and OVER clauses
#* For entries in a WINDOW list, "name" is the window name being defined.
#* For OVER clauses, we use "name" for the "OVER window" syntax, or "refname"
#* for the "OVER (window)" syntax, which is subtly different --- the latter
#* implies overriding the window frame clause.
typedef_struct 'WindowDef' {
   name                 => [ Str             ],  #* window's own name 
   refname              => [ Str             ],  #* referenced window name, if any 
   partitionClause      => [ ArrayRef        ],  #* PARTITION BY expression list 
   orderClause          => [ ArrayRef        ],  #* ORDER BY (list of SortBy) 
   frameOptions         => [ Int             ],  #* frame_clause options, see below 
   startOffset          => [ Any             ],  #* expression for starting bound, if any 
   endOffset            => [ Any             ],  #* expression for ending bound, if any 
   location             => [ HashRef         ],  #* parse location, or -1 if none/unknown 
};

#* frameOptions is an OR of these bits.  The NONDEFAULT and BETWEEN bits are
#* used so that ruleutils.c can tell which properties were specified and
#* which were defaulted; the correct behavioral bits must be set either way.
#* The START_foo and END_foo options must come in pairs of adjacent bits for
#* the convenience of gram.y, even though some of them are useless/invalid.
#* We will need more bits (and fields) to cover the full SQL:2008 option set.
typedef_enum '' [
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
] 1;

#* RangeSubselect - subquery appearing in a FROM clause
typedef_struct 'RangeSubselect' {
   subquery             => [ Any             ],  #* the untransformed sub-select clause 
   alias                => [ Alias           ],  #* table alias & optional column aliases 
};

#* RangeFunction - function call appearing in a FROM clause
typedef_struct 'RangeFunction' {
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
typedef_struct 'ColumnDef' {
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
typedef_struct 'TableLikeClause' {
   relation             => [ RangeVar        ],
   options              => [ UInt32          ],  #* OR of TableLikeOption flags 
};

typedef_enum 'TableLikeOption' [
   'CREATE_TABLE_LIKE_DEFAULTS    = 1 << 0',
   'CREATE_TABLE_LIKE_CONSTRAINTS = 1 << 1',
   'CREATE_TABLE_LIKE_INDEXES     = 1 << 2',
   'CREATE_TABLE_LIKE_STORAGE     = 1 << 3',
   'CREATE_TABLE_LIKE_COMMENTS    = 1 << 4',
   'CREATE_TABLE_LIKE_ALL         = 0x7FFFFFFF'
] 1;

#* IndexElem - index parameters (used in CREATE INDEX)
#* For a plain index attribute, 'name' is the name of the table column to
#* index, and 'expr' is NULL.  For an index expression, 'name' is NULL and
#* 'expr' is the expression tree.
typedef_struct 'IndexElem' {
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
typedef_enum 'DefElemAction' [
   'DEFELEM_UNSPEC',             #* no action given 
   'DEFELEM_SET',
   'DEFELEM_ADD',
   'DEFELEM_DROP'
] 0;

typedef_struct 'DefElem' {
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
typedef_struct 'LockingClause' {
   lockedRels           => [ ArrayRef        ],  #* FOR UPDATE or FOR SHARE relations 
   forUpdate            => [ Bool            ],  #* true = FOR UPDATE, false = FOR SHARE 
   noWait               => [ Bool            ],  #* NOWAIT option 
};

#* XMLSERIALIZE (in raw parse tree only)
typedef_struct 'XmlSerialize' {
   xmloption            => [ XmlOptionType   ],  #* DOCUMENT or CONTENT 
   expr                 => [ Any             ],
   typeName             => [ TypeName        ],
   location             => [ HashRef         ],  #* token location, or -1 if unknown 
};

#* WithClause -
#*     representation of WITH clause
#* Note: WithClause does not propagate into the Query representation;
#* but CommonTableExpr does.
typedef_struct 'WithClause' {
   ctes                 => [ ArrayRef        ],  #* list of CommonTableExprs 
   recursive            => [ Bool            ],  #* true = WITH RECURSIVE 
   location             => [ HashRef         ],  #* token location, or -1 if unknown 
};

#* CommonTableExpr -
#*     representation of WITH list element
#* We don't currently support the SEARCH or CYCLE clause.
typedef_struct 'CommonTableExpr' {
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

#*****************************************************************************
#*      Optimizable Statements
#****************************************************************************

#* ----------------------
#*      Insert Statement
#* The source expression is represented by SelectStmt for both the
#* SELECT and VALUES cases.  If selectStmt is NULL, then the query
#* is INSERT ... DEFAULT VALUES.
#* ----------------------
typedef_struct 'Insert' {
   relation             => [ RangeVar        ],  #* relation to insert into 
   cols                 => [ ArrayRef        ],  #* optional: names of the target columns 
   selectStmt           => [ Any             ],  #* the source SELECT/VALUES, or NULL 
   returningList        => [ ArrayRef        ],  #* list of expressions to return 
   withClause           => [ WithClause      ],  #* WITH clause 
};

#* ----------------------
#*      Delete Statement
#* ----------------------
typedef_struct 'Delete' {
   relation             => [ RangeVar        ],  #* relation to delete from 
   usingClause          => [ ArrayRef        ],  #* optional using clause for more tables 
   whereClause          => [ Any             ],  #* qualifications 
   returningList        => [ ArrayRef        ],  #* list of expressions to return 
   withClause           => [ WithClause      ],  #* WITH clause 
};

#* ----------------------
#*      Update Statement
#* ----------------------
typedef_struct 'Update' {
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
typedef_enum 'SetOperation' [
   'SETOP_NONE',
   'SETOP_UNION',
   'SETOP_INTERSECT',
   'SETOP_EXCEPT'
] 0;

typedef_struct 'Select' {

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
   larg                 => [ SelectStmt      ],  #* left child 
   rarg                 => [ SelectStmt      ],  #* right child 
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
typedef_struct 'SetOperation' {
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

typedef_enum 'ObjectType' [
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
] 0;

#* ----------------------
#*      Create Schema Statement
#* NOTE: the schemaElts list contains raw parsetrees for component statements
#* of the schema, such as CREATE TABLE, GRANT, etc.  These are analyzed and
#* executed after the schema itself is created.
#* ----------------------
typedef_struct 'CreateSchema' {
   schemaname           => [ Str             ],  #* the name of the schema to create 
   authid               => [ Str             ],  #* the owner of the created schema 
   schemaElts           => [ ArrayRef        ],  #* schema components (list of parsenodes) 
};

typedef_enum 'DropBehavior' [
   'DROP_RESTRICT',              #* drop fails if any dependent objects 
   'DROP_CASCADE'                #* remove dependent objects too 
] 0;

#* ----------------------
#*  Alter Table
#* ----------------------
typedef_struct 'AlterTable' {
   relation             => [ RangeVar        ],  #* table to work on 
   cmds                 => [ ArrayRef        ],  #* list of subcommands 
   relkind              => [ ObjectType      ],  #* type of object 
   missing_ok           => [ Bool            ],  #* skip error if table missing 
};

typedef_enum 'AlterTableType' [
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
] 0;

typedef_struct 'AlterTableCmd' {    #* one subcommand of an ALTER TABLE 
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
typedef_struct 'AlterDomain' {
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
typedef_enum 'GrantTargetType' [
   'ACL_TARGET_OBJECT',          #* grant on specific named object(s) 
   'ACL_TARGET_ALL_IN_SCHEMA',   #* grant on all objects in given schema(s) 
   'ACL_TARGET_DEFAULTS'         #* ALTER DEFAULT PRIVILEGES 
] 0;

typedef_enum 'GrantObjectType' [
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
] 0;

typedef_struct 'Grant' {
   is_grant             => [ Bool            ],  #* true = GRANT, false = REVOKE 
   targtype             => [ GrantTargetType ],  #* type of the grant target 
   objtype              => [ GrantObjectType ],  #* kind of object being operated on 
   objects              => [ ArrayRef        ],  #* list of RangeVar nodes, FuncWithArgs nodes,
                                                 #* or plain names (as Value strings) 
   privileges           => [ ArrayRef        ],  #* list of AccessPriv nodes 
   #* privileges == NIL denotes ALL PRIVILEGES 
   grantees             => [ ArrayRef        ],  #* list of PrivGrantee nodes 
   grant_option         => [ Bool            ],  #* grant or revoke grant option 
   behavior             => [ DropBehavior    ],  #* drop behavior (for REVOKE) 
};

typedef_struct 'PrivGrantee' {
   rolname              => [ Str             ],  #* if NULL then PUBLIC 
};

#* Note: FuncWithArgs carries only the types of the input parameters of the
#* function.  So it is sufficient to identify an existing function, but it
#* is not enough info to define a function nor to call it.
typedef_struct 'FuncWithArgs' {
   funcname             => [ ArrayRef        ],  #* qualified name of function 
   funcargs             => [ ArrayRef        ],  #* list of Typename nodes 
};

#* An access privilege, with optional list of column names
#* priv_name == NULL denotes ALL PRIVILEGES (only used with a column list)
#* cols == NIL denotes "all columns"
#* Note that simple "ALL PRIVILEGES" is represented as a NIL list, not
#* an AccessPriv with both fields null.
typedef_struct 'AccessPriv' {
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
typedef_struct 'GrantRole' {
   granted_roles        => [ ArrayRef        ],  #* list of roles to be granted/revoked 
   grantee_roles        => [ ArrayRef        ],  #* list of member roles to add/delete 
   is_grant             => [ Bool            ],  #* true = GRANT, false = REVOKE 
   admin_opt            => [ Bool            ],  #* with admin option 
   grantor              => [ Str             ],  #* set grantor to other than current role 
   behavior             => [ DropBehavior    ],  #* drop behavior (for REVOKE) 
};

#* ----------------------
#*  Alter Default Privileges Statement
#* ----------------------
typedef_struct 'AlterDefaultPrivileges' {
   options              => [ ArrayRef        ],  #* list of DefElem 
   action               => [ GrantStmt       ],  #* GRANT/REVOKE action (with objects=NIL) 
};

#* ----------------------
#*      Copy Statement
#* We support "COPY relation FROM file", "COPY relation TO file", and
#* "COPY (query) TO file".  In any given CopyStmt, exactly one of "relation"
#* and "query" must be non-NULL.
#* ----------------------
typedef_struct 'Copy' {
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
typedef enum 'VariableSetKind' [
   'VAR_SET_VALUE',              #* SET var = value 
   'VAR_SET_DEFAULT',            #* SET var TO DEFAULT 
   'VAR_SET_CURRENT',            #* SET var FROM CURRENT 
   'VAR_SET_MULTI',              #* special case for SET TRANSACTION ... 
   'VAR_RESET',                  #* RESET var 
   'VAR_RESET_ALL'               #* RESET ALL 
] 0;

typedef_struct 'VariableSet' {
   kind                 => [ VariableSetKind ],
   name                 => [ Str             ],  #* variable to be set 
   args                 => [ ArrayRef        ],  #* List of A_Const nodes 
   is_local             => [ Bool            ],  #* SET LOCAL? 
};

#* ----------------------
#* Show Statement
#* ----------------------
typedef_struct 'VariableShow' {
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

typedef_struct 'Create' {
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

typedef_enum 'ConstrType' [      #* types of constraints 
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
] 0;

use constant { 
   #* Foreign key action codes
   FKCONSTR_ACTION_NOACTION   => 'a',
   FKCONSTR_ACTION_RESTRICT   => 'r',
   FKCONSTR_ACTION_CASCADE    => 'c',
   FKCONSTR_ACTION_SETNULL    => 'n',
   FKCONSTR_ACTION_SETDEFAULT => 'd',

   #* Foreign key matchtype codes 
   FKCONSTR_MATCH_FULL        => 'f',
   FKCONSTR_MATCH_PARTIAL     => 'p',
   FKCONSTR_MATCH_SIMPLE      => 's',
};

typedef_struct 'Constraint' {
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

typedef_struct 'CreateTableSpace' {
   tablespacename       => [ Str             ],
   owner                => [ Str             ],
   location             => [ HashRef         ],
};

typedef_struct 'DropTableSpace' {
   tablespacename       => [ Str             ],
   missing_ok           => [ Bool            ],  #* skip error if missing? 
};

typedef_struct 'AlterTableSpaceOptions' {
   tablespacename       => [ Str             ],
   options              => [ ArrayRef        ],
   isReset              => [ Bool            ],
};

#* ----------------------
#*      Create/Alter Extension Statements
#* ----------------------

typedef_struct 'CreateExtension' {
   extname              => [ Str             ],
   if_not_exists        => [ Bool            ],  #* just do nothing if it already exists? 
   options              => [ ArrayRef        ],  #* List of DefElem nodes 
};

#* Only used for ALTER EXTENSION UPDATE; later might need an action field 
typedef_struct 'AlterExtension' {
   extname              => [ Str             ],
   options              => [ ArrayRef        ],  #* List of DefElem nodes 
};

typedef_struct 'AlterExtensionContents' {
   extname              => [ Str             ],  #* Extension's name 
   action               => [ Int             ],  #* +1 = add object, -1 = drop object 
   objtype              => [ ObjectType      ],  #* Object's type 
   objname              => [ ArrayRef        ],  #* Qualified name of the object 
   objargs              => [ ArrayRef        ],  #* Arguments if needed (eg, for functions) 
};

#* ----------------------
#*      Create/Alter FOREIGN DATA WRAPPER Statements
#* ----------------------

typedef_struct 'CreateFdw' {
   fdwname              => [ Str             ],  #* foreign-data wrapper name 
   func_options         => [ ArrayRef        ],  #* HANDLER/VALIDATOR options 
   options              => [ ArrayRef        ],  #* generic options to FDW 
};

typedef_struct 'AlterFdw' {
   fdwname              => [ Str             ],  #* foreign-data wrapper name 
   func_options         => [ ArrayRef        ],  #* HANDLER/VALIDATOR options 
   options              => [ ArrayRef        ],  #* generic options to FDW 
};

#* ----------------------
#*      Create/Alter FOREIGN SERVER Statements
#* ----------------------

typedef_struct 'CreateForeignServer' {
   servername           => [ Str             ],  #* server name 
   servertype           => [ Str             ],  #* optional server type 
   version              => [ Str             ],  #* optional server version 
   fdwname              => [ Str             ],  #* FDW name 
   options              => [ ArrayRef        ],  #* generic options to server 
};

typedef_struct 'AlterForeignServer' {
   servername           => [ Str             ],  #* server name 
   version              => [ Str             ],  #* optional server version 
   options              => [ ArrayRef        ],  #* generic options to server 
   has_version          => [ Bool            ],  #* version specified 
};

#* ----------------------
#*      Create FOREIGN TABLE Statements
#* ----------------------

typedef_struct 'CreateForeignTable' {
   base                 => [ CreateStmt      ],
   servername           => [ Str             ],
   options              => [ ArrayRef        ],
};

#* ----------------------
#*      Create/Drop USER MAPPING Statements
#* ----------------------

typedef_struct 'CreateUserMapping' {
   username             => [ Str             ],  #* username or PUBLIC/CURRENT_USER 
   servername           => [ Str             ],  #* server name 
   options              => [ ArrayRef        ],  #* generic options to server 
};

typedef_struct 'AlterUserMapping' {
   username             => [ Str             ],  #* username or PUBLIC/CURRENT_USER 
   servername           => [ Str             ],  #* server name 
   options              => [ ArrayRef        ],  #* generic options to server 
};

typedef_struct 'DropUserMapping' {
   username             => [ Str             ],  #* username or PUBLIC/CURRENT_USER 
   servername           => [ Str             ],  #* server name 
   missing_ok           => [ Bool            ],  #* ignore missing mappings 
};

#* ----------------------
#*      Create TRIGGER Statement
#* ----------------------
typedef_struct 'CreateTrig' {
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
typedef_struct 'CreatePLang' {
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
typedef_enum 'RoleStmtType' [
   'ROLESTMT_ROLE',
   'ROLESTMT_USER',
   'ROLESTMT_GROUP'
] 0;

typedef_struct 'CreateRole' {
   stmt_type            => [ RoleStmtType    ],  #* ROLE/USER/GROUP 
   role                 => [ Str             ],  #* role name 
   options              => [ ArrayRef        ],  #* List of DefElem nodes 
};

typedef_struct 'AlterRole' {
   role                 => [ Str             ],  #* role name 
   options              => [ ArrayRef        ],  #* List of DefElem nodes 
   action               => [ Int             ],  #* +1 = add members, -1 = drop members 
};

typedef_struct 'AlterRoleSet' {
   role                 => [ Str             ],  #* role name 
   database             => [ Str             ],  #* database name, or NULL 
   setstmt              => [ VariableSetStmt ],  #* SET or RESET subcommand 
};

typedef_struct 'DropRole' {
   roles                => [ ArrayRef        ],  #* List of roles to remove 
   missing_ok           => [ Bool            ],  #* skip error if a role is missing? 
};

#* ----------------------
#*      {Create|Alter} SEQUENCE Statement
#* ----------------------

typedef_struct 'CreateSeq' {
   sequence             => [ RangeVar        ],  #* the sequence to create 
   options              => [ ArrayRef        ],
   ownerId              => [ UInt            ],  #* ID of owner, or InvalidOid for default 
};

typedef_struct 'AlterSeq' {
   sequence             => [ RangeVar        ],  #* the sequence to alter 
   options              => [ ArrayRef        ],
   missing_ok           => [ Bool            ],  #* skip error if a role is missing? 
};

#* ----------------------
#*      Create {Aggregate|Operator|Type} Statement
#* ----------------------
typedef_struct 'Define' {
   kind                 => [ ObjectType      ],  #* aggregate, operator, type 
   oldstyle             => [ Bool            ],  #* hack to signal old CREATE AGG syntax 
   defnames             => [ ArrayRef        ],  #* qualified name (list of Value strings) 
   args                 => [ ArrayRef        ],  #* a list of TypeName (if needed) 
   definition           => [ ArrayRef        ],  #* a list of DefElem 
};

#* ----------------------
#*      Create Domain Statement
#* ----------------------
typedef_struct 'CreateDomain' {
   domainname           => [ ArrayRef        ],  #* qualified name (list of Value strings) 
   typeName             => [ TypeName        ],  #* the base type 
   collClause           => [ CollateClause   ],  #* untransformed COLLATE spec, if any 
   constraints          => [ ArrayRef        ],  #* constraints (list of Constraint nodes) 
};

#* ----------------------
#*      Create Operator Class Statement
#* ----------------------
typedef_struct 'CreateOpClass' {
   opclassname          => [ ArrayRef        ],  #* qualified name (list of Value strings) 
   opfamilyname         => [ ArrayRef        ],  #* qualified name (ditto); NIL if omitted 
   amname               => [ Str             ],  #* name of index AM opclass is for 
   datatype             => [ TypeName        ],  #* datatype of indexed column 
   items                => [ ArrayRef        ],  #* List of CreateOpClassItem nodes 
   isDefault            => [ Bool            ],  #* Should be marked as default for type? 
};

use constant {
   OPCLASS_ITEM_OPERATOR    => 'OPERATOR',
   OPCLASS_ITEM_FUNCTION    => 'FUNCTION',
   OPCLASS_ITEM_STORAGETYPE => 'STORAGETYPE',
};

typedef_struct 'CreateOpClassItem' {
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
typedef_struct 'CreateOpFamily' {
   opfamilyname         => [ ArrayRef        ],  #* qualified name (list of Value strings) 
   amname               => [ Str             ],  #* name of index AM opfamily is for 
};

#* ----------------------
#*      Alter Operator Family Statement
#* ----------------------
typedef_struct 'AlterOpFamily' {
   opfamilyname         => [ ArrayRef        ],  #* qualified name (list of Value strings) 
   amname               => [ Str             ],  #* name of index AM opfamily is for 
   isDrop               => [ Bool            ],  #* ADD or DROP the items? 
   items                => [ ArrayRef        ],  #* List of CreateOpClassItem nodes 
};

#* ----------------------
#*      Drop Table|Sequence|View|Index|Type|Domain|Conversion|Schema Statement
#* ----------------------

typedef_struct 'Drop' {
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
typedef_struct 'Truncate' {
   relations            => [ ArrayRef        ],  #* relations (RangeVars) to be truncated 
   restart_seqs         => [ Bool            ],  #* restart owned sequences? 
   behavior             => [ DropBehavior    ],  #* RESTRICT or CASCADE behavior 
};

#* ----------------------
#*              Comment On Statement
#* ----------------------
typedef_struct 'Comment' {
   objtype              => [ ObjectType      ],  #* Object's type 
   objname              => [ ArrayRef        ],  #* Qualified name of the object 
   objargs              => [ ArrayRef        ],  #* Arguments if needed (eg, for functions) 
   comment              => [ Str             ],  #* Comment to insert, or NULL to remove 
};

#* ----------------------
#*              SECURITY LABEL Statement
#* ----------------------
typedef_struct 'SecLabel' {
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
typedef_enum '' [
   'CURSOR_OPT_BINARY=1',      #* BINARY 
   'CURSOR_OPT_SCROLL',        #* SCROLL explicitly given 
   'CURSOR_OPT_NO_SCROLL',     #* NO SCROLL explicitly given 
   'CURSOR_OPT_INSENSITIVE',   #* INSENSITIVE 
   'CURSOR_OPT_HOLD',          #* WITH HOLD 
   #* these planner-control flags do not correspond to any SQL grammar: 
   'CURSOR_OPT_FAST_PLAN',     #* prefer fast-start plan 
   'CURSOR_OPT_GENERIC_PLAN',  #* force use of generic plan 
   'CURSOR_OPT_CUSTOM_PLAN',   #* force use of custom plan 
] 1;

typedef_struct 'DeclareCursor' {
   portalname           => [ Str             ],  #* name of the portal (cursor) 
   options              => [ Int             ],  #* bitmask of options (see above) 
   query                => [ Any             ],  #* the raw SELECT query 
};

#* ----------------------
#*      Close Portal Statement
#* ----------------------
typedef_struct 'ClosePortal' {
   portalname           => [ Str             ],  #* name of the portal (cursor) 
   #* NULL means CLOSE ALL 
};

#* ----------------------
#*      Fetch Statement (also Move)
#* ----------------------
typedef_enum 'FetchDirection' [
   #* for these, howMany is how many rows to fetch; FETCH_ALL means ALL 
   'FETCH_FORWARD',
   'FETCH_BACKWARD',
   #* for these, howMany indicates a position; only one row is fetched 
   'FETCH_ABSOLUTE',
   'FETCH_RELATIVE'
] 0;

typedef_struct 'Fetch' {
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
typedef_struct 'Index' {
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
typedef_struct 'CreateFunction' {
   replace              => [ Bool            ],  #* T => replace if already exists 
   funcname             => [ ArrayRef        ],  #* qualified name of function to create 
   parameters           => [ ArrayRef        ],  #* a list of FunctionParameter 
   returnType           => [ TypeName        ],  #* the return type 
   options              => [ ArrayRef        ],  #* a list of DefElem 
   withClause           => [ ArrayRef        ],  #* a list of DefElem 
};

typedef_enum 'FunctionParameterMode' [
   'FUNC_PARAM_IN',        #* input only 
   'FUNC_PARAM_OUT',       #* output only 
   'FUNC_PARAM_INOUT',     #* both 
   'FUNC_PARAM_VARIADIC',  #* variadic (always input) 
   'FUNC_PARAM_TABLE'      #* table function output column 
] 0;

typedef_struct 'FunctionParameter' {
   name                 => [ Str             ],  #* parameter name, or NULL if not given 
   argType              => [ TypeName        ],  #* TypeName for parameter type 
   mode                 => [ FunctionParameterMode ],  #* IN/OUT/etc 
   defexpr              => [ Any             ],  #* raw default expr, or NULL if not given 
};

typedef_struct 'AlterFunction' {
   func                 => [ FuncWithArgs    ],  #* name and args of function 
   actions              => [ ArrayRef        ],  #* list of DefElem 
};

#* ----------------------
#*      DO Statement
#* DoStmt is the raw parser output, InlineCodeBlock is the execution-time API
#* ----------------------
typedef_struct 'Do' {
   args                 => [ ArrayRef        ],  #* List of DefElem nodes 
};

typedef_struct 'InlineCodeBlock' {
   source_text          => [ Str             ],  #* source text of anonymous code block 
   langOid              => [ UInt            ],  #* OID of selected language 
   langIsTrusted        => [ Bool            ],  #* trusted property of the language 
};

#* ----------------------
#*      Alter Object Rename Statement
#* ----------------------
typedef_struct 'Rename' {
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
typedef_struct 'AlterObjectSchema' {
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
typedef_struct 'AlterOwner' {
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
typedef_struct 'Rule' {
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
typedef_struct 'Notify' {
   conditionname        => [ Str             ],  #* condition name to notify 
   payload              => [ Str             ],  #* the payload string, or NULL if none 
};

#* ----------------------
#*      Listen Statement
#* ----------------------
typedef_struct 'Listen' {
   conditionname        => [ Str             ],  #* condition name to listen on 
};

#* ----------------------
#*      Unlisten Statement
#* ----------------------
typedef_struct 'Unlisten' {
   conditionname        => [ Str             ],  #* name to unlisten on, or NULL for all 
};

#* ----------------------
#*      {Begin|Commit|Rollback} Transaction Statement
#* ----------------------
typedef_enum 'TransactionStmtKind' [
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
] 0;

typedef_struct 'Transaction' {
   kind                 => [ TransactionStmtKind ],  #* see above 
   options              => [ ArrayRef        ],  #* for BEGIN/START and savepoint commands 
   gid                  => [ Str             ],  #* for two-phase-commit related commands 
};

#* ----------------------
#*      Create Type Statement, composite types
#* ----------------------
typedef_struct 'CompositeType' {
   typevar              => [ RangeVar        ],  #* the composite type to be created 
   coldeflist           => [ ArrayRef        ],  #* list of ColumnDef nodes 
};

#* ----------------------
#*      Create Type Statement, enum types
#* ----------------------
typedef_struct 'CreateEnum' {
   typeName             => [ ArrayRef        ],  #* qualified name (list of Value strings) 
   vals                 => [ ArrayRef        ],  #* enum values (list of Value strings) 
};

#* ----------------------
#*      Create Type Statement, range types
#* ----------------------
typedef_struct 'CreateRange' {
   typeName             => [ ArrayRef        ],  #* qualified name (list of Value strings) 
   params               => [ ArrayRef        ],  #* range parameters (list of DefElem) 
};

#* ----------------------
#*      Alter Type Statement, enum types
#* ----------------------
typedef_struct 'AlterEnum' {
   typeName             => [ ArrayRef        ],  #* qualified name (list of Value strings) 
   newVal               => [ Str             ],  #* new enum value's name 
   newValNeighbor       => [ Str             ],  #* neighboring enum value, if specified 
   newValIsAfter        => [ Bool            ],  #* place new enum value after neighbor? 
};

#* ----------------------
#*      Create View Statement
#* ----------------------
typedef_struct 'View' {
   view                 => [ RangeVar        ],  #* the view to be created 
   aliases              => [ ArrayRef        ],  #* target column names 
   query                => [ Any             ],  #* the SELECT query 
   replace              => [ Bool            ],  #* replace an existing view? 
   options              => [ ArrayRef        ],  #* options from WITH clause 
};

#* ----------------------
#*      Load Statement
#* ----------------------
typedef_struct 'Load' {
   filename             => [ Str             ],  #* file to load 
};

#* ----------------------
#*      Createdb Statement
#* ----------------------
typedef_struct 'Createdb' {
   dbname               => [ Str             ],  #* name of database to create 
   options              => [ ArrayRef        ],  #* List of DefElem nodes 
};

#* ----------------------
#*  Alter Database
#* ----------------------
typedef_struct 'AlterDatabase' {
   dbname               => [ Str             ],  #* name of database to alter 
   options              => [ ArrayRef        ],  #* List of DefElem nodes 
};

typedef_struct 'AlterDatabaseSet' {
   dbname               => [ Str             ],  #* database name 
   setstmt              => [ VariableSetStmt ],  #* SET or RESET subcommand 
};

#* ----------------------
#*      Dropdb Statement
#* ----------------------
typedef_struct 'Dropdb' {
   dbname               => [ Str             ],  #* database to drop 
   missing_ok           => [ Bool            ],  #* skip error if db is missing? 
};

#* ----------------------
#*      Cluster Statement (support pbrown's cluster index implementation)
#* ----------------------
typedef_struct 'Cluster' {
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
typedef_enum 'VacuumOption' [
   'VACOPT_VACUUM = 1 << 0',     #* do VACUUM 
   'VACOPT_ANALYZE = 1 << 1',    #* do ANALYZE 
   'VACOPT_VERBOSE = 1 << 2',    #* print progress info 
   'VACOPT_FREEZE = 1 << 3',     #* FREEZE option 
   'VACOPT_FULL = 1 << 4',       #* FULL (non-concurrent) vacuum 
   'VACOPT_NOWAIT = 1 << 5'      #* don't wait to get lock (autovacuum only) 
] 0;

typedef_struct 'Vacuum' {
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
typedef_struct 'Explain' {
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
typedef_struct 'CreateTableAs' {
   query                => [ Any             ],  #* the query (see comments above) 
   into                 => [ IntoClause      ],  #* destination table 
   is_select_into       => [ Bool            ],  #* it was written as SELECT INTO 
};

#* ----------------------
#* Checkpoint Statement
#* ----------------------
typedef_struct 'CheckPoint' {
};

#* ----------------------
#* Discard Statement
#* ----------------------

typedef_enum 'DiscardMode' [
   'DISCARD_ALL',
   'DISCARD_PLANS',
   'DISCARD_TEMP'
] 0;

typedef_struct 'Discard' {
   target               => [ DiscardMode     ],
};

#* ----------------------
#*      LOCK Statement
#* ----------------------
typedef_struct 'Lock' {
   relations            => [ ArrayRef        ],  #* relations to lock 
   mode                 => [ Int             ],  #* lock mode 
   nowait               => [ Bool            ],  #* no wait mode 
};

#* ----------------------
#*      SET CONSTRAINTS Statement
#* ----------------------
typedef_struct 'ConstraintsSet' {
   constraints          => [ ArrayRef        ],  #* List of names as RangeVars 
   deferred             => [ Bool            ],
};

#* ----------------------
#*      REINDEX Statement
#* ----------------------
typedef_struct 'Reindex' {
   kind                 => [ ObjectType      ],  #* OBJECT_INDEX, OBJECT_TABLE, OBJECT_DATABASE 
   relation             => [ RangeVar        ],  #* Table or index to reindex 
   name                 => [ Str             ],  #* name of database to reindex 
   do_system            => [ Bool            ],  #* include system tables in database case 
   do_user              => [ Bool            ],  #* include user tables in database case 
};

#* ----------------------
#*      CREATE CONVERSION Statement
#* ----------------------
typedef_struct 'CreateConversion' {
   conversion_name      => [ ArrayRef        ],  #* Name of the conversion 
   for_encoding_name    => [ Str             ],  #* source encoding name 
   to_encoding_name     => [ Str             ],  #* destination encoding name 
   func_name            => [ ArrayRef        ],  #* qualified conversion function name 
   def                  => [ Bool            ],  #* is this a default conversion? 
};

#* ----------------------
#*  CREATE CAST Statement
#* ----------------------
typedef_struct 'CreateCast' {
   sourcetype           => [ TypeName        ],
   targettype           => [ TypeName        ],
   func                 => [ FuncWithArgs    ],
   context              => [ CoercionContext ],
   inout                => [ Bool            ],
};

#* ----------------------
#*      PREPARE Statement
#* ----------------------
typedef_struct 'Prepare' {
   name                 => [ Str             ],  #* Name of plan, arbitrary 
   argtypes             => [ ArrayRef        ],  #* Types of parameters (List of TypeName) 
   query                => [ Any             ],  #* The query itself (as a raw parsetree) 
};


#* ----------------------
#*      EXECUTE Statement
#* ----------------------

typedef_struct 'Execute' {
   name                 => [ Str             ],  #* The name of the plan to execute 
   params               => [ ArrayRef        ],  #* Values to assign to parameters 
};


#* ----------------------
#*      DEALLOCATE Statement
#* ----------------------
typedef_struct 'Deallocate' {
   name                 => [ Str             ],  #* The name of the plan to remove 
   #* NULL means DEALLOCATE ALL 
};

#*      DROP OWNED statement
typedef_struct 'DropOwned' {
   roles                => [ ArrayRef        ],
   behavior             => [ DropBehavior    ],
};

#*      REASSIGN OWNED statement
typedef_struct 'ReassignOwned' {
   roles                => [ ArrayRef        ],
   newrole              => [ Str             ],
};

#* TS Dictionary stmts: DefineStmt, RenameStmt and DropStmt are default
typedef_struct 'AlterTSDictionary' {
   dictname             => [ ArrayRef        ],  #* qualified name (list of Value strings) 
   options              => [ ArrayRef        ],  #* List of DefElem nodes 
};

#* TS Configuration stmts: DefineStmt, RenameStmt and DropStmt are default
typedef_struct 'AlterTSConfiguration' {
   cfgname              => [ ArrayRef        ],  #* qualified name (list of Value strings) 

   #* dicts will be non-NIL if ADD/ALTER MAPPING was specified. If dicts is
   #* NIL, but tokentype isn't, DROP MAPPING was specified.
   tokentype            => [ ArrayRef        ],  #* list of Value strings 
   dicts                => [ ArrayRef        ],  #* list of list of Value strings 
   override             => [ Bool            ],  #* if true - remove old variant 
   replace              => [ Bool            ],  #* if true - replace dictionary by another 
   missing_ok           => [ Bool            ],  #* for DROP - skip error if missing? 
};

#endif   #* PARSENODES_H 

### Finally, let's define our big constant list ###
use constant $constant_list;
