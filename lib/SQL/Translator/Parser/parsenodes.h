#*-------------------------------------------------------------------------
#* parsenodes.h
#*    definitions for parse tree nodes
#* Many of the node types used in parsetrees include a "location" field.
#* This is a byte (not character) offset in the original source text, to be
#* used for positioning an error cursor when there is an error related to
#* the node.  Access to the original source text is needed to make use of
#* the location.
#* Portions Copyright (c) 1996-2012, PostgreSQL Global Development Group
#* Portions Copyright (c) 1994, Regents of the University of California
#* src/include/nodes/parsenodes.h
#*-------------------------------------------------------------------------
#ifndef PARSENODES_H
#define PARSENODES_H

#include "nodes/bitmapset.h"
#include "nodes/primnodes.h"
#include "nodes/value.h"

#* Possible sources of a Query 
typedef_enum 'QuerySource' [
   'QSRC_ORIGINAL',              #* original parsetree (explicit query) 
   'QSRC_PARSER',                #* added by parse analysis (now unused) 
   'QSRC_INSTEAD_RULE',          #* added by unconditional INSTEAD rule 
   'QSRC_QUAL_INSTEAD_RULE',     #* added by conditional INSTEAD rule 
   'QSRC_NON_INSTEAD_RULE'       #* added by non-INSTEAD rule 
] 0;

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

#* Grantable rights are encoded so that we can OR them together in a bitmask.
#* The present representation of AclItem limits us to 16 distinct rights,
#* even though AclMode is defined as uint32.  See utils/acl.h.
#* Caution: changing these codes breaks stored ACLs, hence forces initdb.
typedef uint32 AclMode;         #* a bitmask of privilege bits 

#define ACL_INSERT      (1<<0)  #* for relations 
#define ACL_SELECT      (1<<1)
#define ACL_UPDATE      (1<<2)
#define ACL_DELETE      (1<<3)
#define ACL_TRUNCATE    (1<<4)
#define ACL_REFERENCES  (1<<5)
#define ACL_TRIGGER     (1<<6)
#define ACL_EXECUTE     (1<<7)  #* for functions 
#define ACL_USAGE       (1<<8)  #* for languages, namespaces, FDWs, and
                                #* servers 
#define ACL_CREATE      (1<<9)  #* for namespaces and databases 
#define ACL_CREATE_TEMP (1<<10) #* for databases 
#define ACL_CONNECT     (1<<11) #* for databases 
#define N_ACL_RIGHTS    12      #* 1 plus the last 1<<x 
#define ACL_NO_RIGHTS   0
#* Currently, SELECT ... FOR UPDATE/FOR SHARE requires UPDATE privileges 
#define ACL_SELECT_FOR_UPDATE   ACL_UPDATE


#*****************************************************************************
#*  Query Tree
#****************************************************************************

#* Query -
#*    Parse analysis turns all statements into a Query tree
#*    for further processing by the rewriter and planner.
#*    Utility statements (i.e. non-optimizable statements) have the
#*    utilityStmt field set, and the Query itself is mostly dummy.
#*    DECLARE CURSOR is a special case: it is represented like a SELECT,
#*    but the original DeclareCursorStmt is stored in utilityStmt.
#*    Planning converts a Query tree into a Plan tree headed by a PlannedStmt
#*    node --- the Query structure is not used by the executor.
typedef_struct 'Query' {

   commandType          => [ CmdType         ],  #* select|insert|update|delete|utility 

   querySource          => [ QuerySource     ],  #* where did I come from? 

   queryId              => [ UInt32          ],  #* query identifier (can be set by plugins) 

   canSetTag            => [ Bool            ],  #* do I set the command result tag? 

   utilityStmt          => [ Any             ],  #* non-null if this is DECLARE CURSOR or a
                                                 #* non-optimizable statement 

   resultRelation       => [ Int             ],  #* rtable index of target relation for
                                                 #* INSERT/UPDATE/DELETE; 0 for SELECT 

   hasAggs              => [ Bool            ],  #* has aggregates in tlist or havingQual 
   hasWindowFuncs       => [ Bool            ],  #* has window functions in tlist 
   hasSubLinks          => [ Bool            ],  #* has subquery SubLink 
   hasDistinctOn        => [ Bool            ],  #* distinctClause is from DISTINCT ON 
   hasRecursive         => [ Bool            ],  #* WITH RECURSIVE was specified 
   hasModifyingCTE      => [ Bool            ],  #* has INSERT/UPDATE/DELETE in WITH 
   hasForUpdate         => [ Bool            ],  #* FOR UPDATE or FOR SHARE was specified 

   cteList              => [ ArrayRef        ],  #* WITH list (of CommonTableExpr's) 

   rtable               => [ ArrayRef        ],  #* list of range table entries 
   jointree             => [ FromExpr        ],  #* table join tree (FROM and WHERE clauses) 

   targetList           => [ ArrayRef        ],  #* target list (of TargetEntry) 

   returningList        => [ ArrayRef        ],  #* return-values list (of TargetEntry) 

   groupClause          => [ ArrayRef        ],  #* a list of SortGroupClause's 

   havingQual           => [ Any             ],  #* qualifications applied to groups 

   windowClause         => [ ArrayRef        ],  #* a list of WindowClause's 

   distinctClause       => [ ArrayRef        ],  #* a list of SortGroupClause's 

   sortClause           => [ ArrayRef        ],  #* a list of SortGroupClause's 

   limitOffset          => [ Any             ],  #* # of result tuples to skip (int8 expr) 
   limitCount           => [ Any             ],  #* # of result tuples to return (int8 expr) 

   rowMarks             => [ ArrayRef        ],  #* a list of RowMarkClause's 

   setOperations        => [ Any             ],  #* set-operation tree if this is top level of
                                                 #* a UNION/INTERSECT/EXCEPT query 

   constraintDeps       => [ ArrayRef        ],  #* a list of pg_constraint OIDs that the query
                                                 #* depends on to be semantically valid 
};


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
#define FRAMEOPTION_NONDEFAULT                  0x00001 #* any specified? 
#define FRAMEOPTION_RANGE                       0x00002 #* RANGE behavior 
#define FRAMEOPTION_ROWS                        0x00004 #* ROWS behavior 
#define FRAMEOPTION_BETWEEN                     0x00008 #* BETWEEN given? 
#define FRAMEOPTION_START_UNBOUNDED_PRECEDING   0x00010 #* start is U. P. 
#define FRAMEOPTION_END_UNBOUNDED_PRECEDING     0x00020 #* (disallowed) 
#define FRAMEOPTION_START_UNBOUNDED_FOLLOWING   0x00040 #* (disallowed) 
#define FRAMEOPTION_END_UNBOUNDED_FOLLOWING     0x00080 #* end is U. F. 
#define FRAMEOPTION_START_CURRENT_ROW           0x00100 #* start is C. R. 
#define FRAMEOPTION_END_CURRENT_ROW             0x00200 #* end is C. R. 
#define FRAMEOPTION_START_VALUE_PRECEDING       0x00400 #* start is V. P. 
#define FRAMEOPTION_END_VALUE_PRECEDING         0x00800 #* end is V. P. 
#define FRAMEOPTION_START_VALUE_FOLLOWING       0x01000 #* start is V. F. 
#define FRAMEOPTION_END_VALUE_FOLLOWING         0x02000 #* end is V. F. 

#define FRAMEOPTION_START_VALUE \
    (FRAMEOPTION_START_VALUE_PRECEDING | FRAMEOPTION_START_VALUE_FOLLOWING)
#define FRAMEOPTION_END_VALUE \
    (FRAMEOPTION_END_VALUE_PRECEDING | FRAMEOPTION_END_VALUE_FOLLOWING)

#define FRAMEOPTION_DEFAULTS \
    (FRAMEOPTION_RANGE | FRAMEOPTION_START_UNBOUNDED_PRECEDING | \
     FRAMEOPTION_END_CURRENT_ROW)

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
   'CREATE_TABLE_LIKE_DEFAULTS = 1 << 0',
   'CREATE_TABLE_LIKE_CONSTRAINTS = 1 << 1',
   'CREATE_TABLE_LIKE_INDEXES = 1 << 2',
   'CREATE_TABLE_LIKE_STORAGE = 1 << 3',
   'CREATE_TABLE_LIKE_COMMENTS = 1 << 4',
   'CREATE_TABLE_LIKE_ALL = 0x7FFFFFFF'
] 0;

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


#****************************************************************************
#*  Nodes for a Query tree
#***************************************************************************

#*--------------------
#* RangeTblEntry -
#*    A range table is a List of RangeTblEntry nodes.
#*    A range table entry may represent a plain relation, a sub-select in
#*    FROM, or the result of a JOIN clause.  (Only explicit JOIN syntax
#*    produces an RTE, not the implicit join resulting from multiple FROM
#*    items.  This is because we only need the RTE to deal with SQL features
#*    like outer joins and join-output-column aliasing.)  Other special
#*    RTE types also exist, as indicated by RTEKind.
#*    Note that we consider RTE_RELATION to cover anything that has a pg_class
#*    entry.  relkind distinguishes the sub-cases.
#*    alias is an Alias node representing the AS alias-clause attached to the
#*    FROM expression, or NULL if no clause.
#*    eref is the table reference name and column reference names (either
#*    real or aliases).  Note that system columns (OID etc) are not included
#*    in the column list.
#*    eref->aliasname is required to be present, and should generally be used
#*    to identify the RTE for error messages etc.
#*    In RELATION RTEs, the colnames in both alias and eref are indexed by
#*    physical attribute number; this means there must be colname entries for
#*    dropped columns.  When building an RTE we insert empty strings ("") for
#*    dropped columns.  Note however that a stored rule may have nonempty
#*    colnames for columns dropped since the rule was created (and for that
#*    matter the colnames might be out of date due to column renamings).
#*    The same comments apply to FUNCTION RTEs when the function's return type
#*    is a named composite type.
#*    In JOIN RTEs, the colnames in both alias and eref are one-to-one with
#*    joinaliasvars entries.  A JOIN RTE will omit columns of its inputs when
#*    those columns are known to be dropped at parse time.  Again, however,
#*    a stored rule might contain entries for columns dropped since the rule
#*    was created.  (This is only possible for columns not actually referenced
#*    in the rule.)  When loading a stored rule, we replace the joinaliasvars
#*    items for any such columns with NULL Consts.  (We can't simply delete
#*    them from the joinaliasvars list, because that would affect the attnums
#*    of Vars referencing the rest of the list.)
#*    inh is TRUE for relation references that should be expanded to include
#*    inheritance children, if the rel has any.  This *must* be FALSE for
#*    RTEs other than RTE_RELATION entries.
#*    inFromCl marks those range variables that are listed in the FROM clause.
#*    It's false for RTEs that are added to a query behind the scenes, such
#*    as the NEW and OLD variables for a rule, or the subqueries of a UNION.
#*    This flag is not used anymore during parsing, since the parser now uses
#*    a separate "namespace" data structure to control visibility, but it is
#*    needed by ruleutils.c to determine whether RTEs should be shown in
#*    decompiled queries.
#*    requiredPerms and checkAsUser specify run-time access permissions
#*    checks to be performed at query startup.  The user must have *all*
#*    of the permissions that are OR'd together in requiredPerms (zero
#*    indicates no permissions checking).  If checkAsUser is not zero,
#*    then do the permissions checks using the access rights of that user,
#*    not the current effective user ID.  (This allows rules to act as
#*    setuid gateways.)  Permissions checks only apply to RELATION RTEs.
#*    For SELECT/INSERT/UPDATE permissions, if the user doesn't have
#*    table-wide permissions then it is sufficient to have the permissions
#*    on all columns identified in selectedCols (for SELECT) and/or
#*    modifiedCols (for INSERT/UPDATE; we can tell which from the query type).
#*    selectedCols and modifiedCols are bitmapsets, which cannot have negative
#*    integer members, so we subtract FirstLowInvalidHeapAttributeNumber from
#*    column numbers before storing them in these fields.  A whole-row Var
#*    reference is represented by setting the bit for InvalidAttrNumber.
#*--------------------
typedef_enum 'RTEKind' [
   'RTE_RELATION',               #* ordinary relation reference 
   'RTE_SUBQUERY',               #* subquery in FROM 
   'RTE_JOIN',                   #* join 
   'RTE_FUNCTION',               #* function in FROM 
   'RTE_VALUES',                 #* VALUES (<exprlist>), (<exprlist>), ... 
   'RTE_CTE'                     #* common table expr (WITH list element) 
] 0;

typedef_struct 'RangeTblEntry' {

   rtekind              => [ RTEKind         ],  #* see above 

   #* XXX the fields applicable to only some rte kinds should be merged into
   #* a union.  I didn't do this yet because the diffs would impact a lot of
   #* code that is being actively worked on.  FIXME someday.

   #* Fields valid for a plain relation RTE (else zero):
   relid                => [ UInt            ],  #* OID of the relation 
   relkind              => [ Str             ],  #* relation kind (see pg_class.relkind) 

   #* Fields valid for a subquery RTE (else NULL):
   subquery             => [ Query           ],  #* the sub-query 
   security_barrier     => [ Bool            ],  #* subquery from security_barrier view 

   #* Fields valid for a join RTE (else NULL/zero):
   #* joinaliasvars is a list of Vars or COALESCE expressions corresponding
   #* to the columns of the join result.  An alias Var referencing column K
   #* of the join result can be replaced by the K'th element of joinaliasvars
   #* --- but to simplify the task of reverse-listing aliases correctly, we
   #* do not do that until planning time.  In a Query loaded from a stored
   #* rule, it is also possible for joinaliasvars items to be NULL Consts,
   #* denoting columns dropped since the rule was made.
   jointype             => [ JoinType        ],  #* type of join 
   joinaliasvars        => [ ArrayRef        ],  #* list of alias-var expansions 

   #* Fields valid for a function RTE (else NULL):
   #* If the function returns RECORD, funccoltypes lists the column types
   #* declared in the RTE's column type specification, funccoltypmods lists
   #* their declared typmods, funccolcollations their collations.  Otherwise,
   #* those fields are NIL.
   funcexpr             => [ Any             ],  #* expression tree for func call 
   funccoltypes         => [ ArrayRef        ],  #* OID list of column type OIDs 
   funccoltypmods       => [ ArrayRef        ],  #* integer list of column typmods 
   funccolcollations    => [ ArrayRef        ],  #* OID list of column collation OIDs 

   #* Fields valid for a values RTE (else NIL):
   values_lists         => [ ArrayRef        ],  #* list of expression lists 
   values_collations    => [ ArrayRef        ],  #* OID list of column collation OIDs 

   #* Fields valid for a CTE RTE (else NULL/zero):
   ctename              => [ Str             ],  #* name of the WITH list item 
   ctelevelsup          => [ Index           ],  #* number of query levels up 
   self_reference       => [ Bool            ],  #* is this a recursive self-reference? 
   ctecoltypes          => [ ArrayRef        ],  #* OID list of column type OIDs 
   ctecoltypmods        => [ ArrayRef        ],  #* integer list of column typmods 
   ctecolcollations     => [ ArrayRef        ],  #* OID list of column collation OIDs 

   #* Fields valid in all RTEs:
   alias                => [ Alias           ],  #* user-written alias clause, if any 
   eref                 => [ Alias           ],  #* expanded reference names 
   inh                  => [ Bool            ],  #* inheritance requested? 
   inFromCl             => [ Bool            ],  #* present in FROM clause? 
   requiredPerms        => [ AclMode         ],  #* bitmask of required access permissions 
   checkAsUser          => [ UInt            ],  #* if valid, check access as this role 
   selectedCols         => [ Bitmapset       ],  #* columns needing SELECT permission 
   modifiedCols         => [ Bitmapset       ],  #* columns needing INSERT/UPDATE permission 
};

#* SortGroupClause -
#*      representation of ORDER BY, GROUP BY, PARTITION BY,
#*      DISTINCT, DISTINCT ON items
#* You might think that ORDER BY is only interested in defining ordering,
#* and GROUP/DISTINCT are only interested in defining equality.  However,
#* one way to implement grouping is to sort and then apply a "uniq"-like
#* filter.  So it's also interesting to keep track of possible sort operators
#* for GROUP/DISTINCT, and in particular to try to sort for the grouping
#* in a way that will also yield a requested ORDER BY ordering.  So we need
#* to be able to compare ORDER BY and GROUP/DISTINCT lists, which motivates
#* the decision to give them the same representation.
#* tleSortGroupRef must match ressortgroupref of exactly one entry of the
#*      query's targetlist; that is the expression to be sorted or grouped by.
#* eqop is the OID of the equality operator.
#* sortop is the OID of the ordering operator (a "<" or ">" operator),
#*      or InvalidOid if not available.
#* nulls_first means about what you'd expect.  If sortop is InvalidOid
#*      then nulls_first is meaningless and should be set to false.
#* hashable is TRUE if eqop is hashable (note this condition also depends
#*      on the datatype of the input expression).
#* In an ORDER BY item, all fields must be valid.  (The eqop isn't essential
#* here, but it's cheap to get it along with the sortop, and requiring it
#* to be valid eases comparisons to grouping items.)  Note that this isn't
#* actually enough information to determine an ordering: if the sortop is
#* collation-sensitive, a collation OID is needed too.  We don't store the
#* collation in SortGroupClause because it's not available at the time the
#* parser builds the SortGroupClause; instead, consult the exposed collation
#* of the referenced targetlist expression to find out what it is.
#* In a grouping item, eqop must be valid.  If the eqop is a btree equality
#* operator, then sortop should be set to a compatible ordering operator.
#* We prefer to set eqop/sortop/nulls_first to match any ORDER BY item that
#* the query presents for the same tlist item.  If there is none, we just
#* use the default ordering op for the datatype.
#* If the tlist item's type has a hash opclass but no btree opclass, then
#* we will set eqop to the hash equality operator, sortop to InvalidOid,
#* and nulls_first to false.  A grouping item of this kind can only be
#* implemented by hashing, and of course it'll never match an ORDER BY item.
#* The hashable flag is provided since we generally have the requisite
#* information readily available when the SortGroupClause is constructed,
#* and it's relatively expensive to get it again later.  Note there is no
#* need for a "sortable" flag since OidIsValid(sortop) serves the purpose.
#* A query might have both ORDER BY and DISTINCT (or DISTINCT ON) clauses.
#* In SELECT DISTINCT, the distinctClause list is as long or longer than the
#* sortClause list, while in SELECT DISTINCT ON it's typically shorter.
#* The two lists must match up to the end of the shorter one --- the parser
#* rearranges the distinctClause if necessary to make this true.  (This
#* restriction ensures that only one sort step is needed to both satisfy the
#* ORDER BY and set up for the Unique step.  This is semantically necessary
#* for DISTINCT ON, and presents no real drawback for DISTINCT.)
typedef_struct 'SortGroupClause' {
   tleSortGroupRef      => [ Index           ],  #* reference into targetlist 
   eqop                 => [ UInt            ],  #* the equality operator ('=' op) 
   sortop               => [ UInt            ],  #* the ordering operator ('<' op), or 0 
   nulls_first          => [ Bool            ],  #* do NULLs come before normal values? 
   hashable             => [ Bool            ],  #* can eqop be implemented by hashing? 
};

#* WindowClause -
#*      transformed representation of WINDOW and OVER clauses
#* A parsed Query's windowClause list contains these structs.  "name" is set
#* if the clause originally came from WINDOW, and is NULL if it originally
#* was an OVER clause (but note that we collapse out duplicate OVERs).
#* partitionClause and orderClause are lists of SortGroupClause structs.
#* winref is an ID number referenced by WindowFunc nodes; it must be unique
#* among the members of a Query's windowClause list.
#* When refname isn't null, the partitionClause is always copied from there;
#* the orderClause might or might not be copied (see copiedOrder); the framing
#* options are never copied, per spec.
typedef_struct 'WindowClause' {
   name                 => [ Str             ],  #* window name (NULL in an OVER clause) 
   refname              => [ Str             ],  #* referenced window name, if any 
   partitionClause      => [ ArrayRef        ],  #* PARTITION BY list 
   orderClause          => [ ArrayRef        ],  #* ORDER BY list 
   frameOptions         => [ Int             ],  #* frame_clause options, see WindowDef 
   startOffset          => [ Any             ],  #* expression for starting bound, if any 
   endOffset            => [ Any             ],  #* expression for ending bound, if any 
   winref               => [ Index           ],  #* ID referenced by window functions 
   copiedOrder          => [ Bool            ],  #* did we copy orderClause from refname? 
};

#* RowMarkClause -
#*     parser output representation of FOR UPDATE/SHARE clauses
#* Query.rowMarks contains a separate RowMarkClause node for each relation
#* identified as a FOR UPDATE/SHARE target.  If FOR UPDATE/SHARE is applied
#* to a subquery, we generate RowMarkClauses for all normal and subquery rels
#* in the subquery, but they are marked pushedDown = true to distinguish them
#* from clauses that were explicitly written at this query level.  Also,
#* Query.hasForUpdate tells whether there were explicit FOR UPDATE/SHARE
#* clauses in the current query level.
typedef_struct 'RowMarkClause' {
   rti                  => [ Index           ],  #* range table index of target relation 
   forUpdate            => [ Bool            ],  #* true = FOR UPDATE, false = FOR SHARE 
   noWait               => [ Bool            ],  #* NOWAIT option 
   pushedDown           => [ Bool            ],  #* pushed down from higher query level? 
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

#* Convenience macro to get the output tlist of a CTE's query 
#define GetCTETargetList(cte) \
    (AssertMacro(IsA((cte)->ctequery, Query)), \
     ((Query *) (cte)->ctequery)->commandType == CMD_SELECT ? \
     ((Query *) (cte)->ctequery)->targetList : \
     ((Query *) (cte)->ctequery)->returningList)


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
   'SETOP_NONE = 0',
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
    char        subtype;        #*------------
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
typedef enum
{
    VAR_SET_VALUE,              #* SET var = value 
    VAR_SET_DEFAULT,            #* SET var TO DEFAULT 
    VAR_SET_CURRENT,            #* SET var FROM CURRENT 
    VAR_SET_MULTI,              #* special case for SET TRANSACTION ... 
    VAR_RESET,                  #* RESET var 
    VAR_RESET_ALL               #* RESET ALL 
} VariableSetKind;

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

typedef_enum 'ConstrType' [         #* types of constraints 
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

#* Foreign key action codes 
#define FKCONSTR_ACTION_NOACTION    'a'
#define FKCONSTR_ACTION_RESTRICT    'r'
#define FKCONSTR_ACTION_CASCADE     'c'
#define FKCONSTR_ACTION_SETNULL     'n'
#define FKCONSTR_ACTION_SETDEFAULT  'd'

#* Foreign key matchtype codes 
#define FKCONSTR_MATCH_FULL         'f'
#define FKCONSTR_MATCH_PARTIAL      'p'
#define FKCONSTR_MATCH_SIMPLE       's'

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

#define OPCLASS_ITEM_OPERATOR       1
#define OPCLASS_ITEM_FUNCTION       2
#define OPCLASS_ITEM_STORAGETYPE    3

typedef_struct 'CreateOpClassItem' {
   itemtype             => [ Int             ],  #* see codes above 
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
#define CURSOR_OPT_BINARY       0x0001  #* BINARY 
#define CURSOR_OPT_SCROLL       0x0002  #* SCROLL explicitly given 
#define CURSOR_OPT_NO_SCROLL    0x0004  #* NO SCROLL explicitly given 
#define CURSOR_OPT_INSENSITIVE  0x0008  #* INSENSITIVE 
#define CURSOR_OPT_HOLD         0x0010  #* WITH HOLD 
#* these planner-control flags do not correspond to any SQL grammar: 
#define CURSOR_OPT_FAST_PLAN    0x0020  #* prefer fast-start plan 
#define CURSOR_OPT_GENERIC_PLAN 0x0040  #* force use of generic plan 
#define CURSOR_OPT_CUSTOM_PLAN  0x0080  #* force use of custom plan 

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

#define FETCH_ALL   LONG_MAX

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
    #* the assigned enum values appear in pg_proc, don't change 'em! 
   'FUNC_PARAM_IN = 'i'',        #* input only 
   'FUNC_PARAM_OUT = 'o'',       #* output only 
   'FUNC_PARAM_INOUT = 'b'',     #* both 
   'FUNC_PARAM_VARIADIC = 'v'',  #* variadic (always input) 
   'FUNC_PARAM_TABLE = 't''      #* table function output column 
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
