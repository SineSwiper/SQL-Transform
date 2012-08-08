
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
   typedef_struct 'Aggref', {
      xpr                  => [ Expr            ],
      aggfnoid             => [ UInt            ],  #* pg_proc Oid of the aggregate 
      aggtype              => [ UInt            ],  #* type Oid of result of the aggregate 
      aggcollid            => [ UInt            ],  #* OID of collation of result 
      inputcollid          => [ UInt            ],  #* OID of collation that function should use 
      args                 => [ ArrayRef        ],  #* arguments and sort expressions 
      aggorder             => [ ArrayRef        ],  #* ORDER BY (list of SortGroupClause) 
      aggdistinct          => [ ArrayRef        ],  #* DISTINCT (list of SortGroupClause) 
      aggstar              => [ Bool            ],  #* TRUE if argument list was really '*' 
      agglevelsup          => [ Int16           ],  #* > 0 if agg belongs to outer query 
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
   typedef_struct 'SubPlan', {
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
      startup_cost         => [ Double          ],  #* one-time setup cost 
      per_call_cost        => [ Double          ],  #* cost for each subplan evaluation 
   };

   #* AlternativeSubPlan - expression node for a choice among SubPlans
   #* The subplans are given as a List so that the node definition need not
   #* change if there's ever more than two alternatives.  For the moment,
   #* though, there are always exactly two; and the first one is the fast-start
   #* plan.
   typedef_struct 'AlternativeSubPlan', {
      xpr                  => [ Expr            ],
      subplans             => [ ArrayRef        ],  #* SubPlan(s) with equivalent results 
   };

   #* ----------------
   #* FieldSelect
   #* FieldSelect represents the operation of extracting one field from a tuple
   #* value.  At runtime, the input expression is expected to yield a rowtype
   #* Datum.  The specified field number is extracted and returned as a Datum.
   #* ----------------

   typedef_struct 'FieldSelect', {
      xpr                  => [ Expr            ],
      arg                  => [ Expr            ],  #* input expression 
      fieldnum             => [ Int16           ],  #* attribute number of field to extract 
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

   typedef_struct 'FieldStore', {
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

   typedef_struct 'RelabelType', {
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

   typedef_struct 'CoerceViaIO', {
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

   typedef_struct 'ArrayCoerceExpr', {
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

   typedef_struct 'ConvertRowtypeExpr', {
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
   typedef_struct 'CollateExpr', {
      xpr                  => [ Expr            ],
      arg                  => [ Expr            ],  #* input expression 
      collOid              => [ UInt            ],  #* collation's OID 
      location             => [ HashRef         ],  #* token location, or -1 if unknown 
   };

   #* Placeholder node for the test value to be processed by a CASE expression.
   #* This is effectively like a Param, but can be implemented more simply
   #* since we need only one replacement value at a time.
   #* We also use this in nested UPDATE expressions.
   #* See transformAssignmentIndirection().
   typedef_struct 'CaseTestExpr', {
      xpr                  => [ Expr            ],
      typeId               => [ UInt            ],  #* type for substituted value 
      typeMod              => [ Int32           ],  #* typemod for substituted value 
      collation            => [ UInt            ],  #* collation for the substituted value 
   };

   #* CoerceToDomain
   #* CoerceToDomain represents the operation of coercing a value to a domain
   #* type.  At runtime (and not before) the precise set of constraints to be
   #* checked will be determined.  If the value passes, it is returned as the
   #* result; if not, an error is raised.  Note that this is equivalent to
   #* RelabelType in the scenario where no constraints are applied.
   typedef_struct 'CoerceToDomain', {
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
   typedef_struct 'CoerceToDomainValue', {
      xpr                  => [ Expr            ],
      typeId               => [ UInt            ],  #* type for substituted value 
      typeMod              => [ Int32           ],  #* typemod for substituted value 
      collation            => [ UInt            ],  #* collation for the substituted value 
      location             => [ HashRef         ],  #* token location, or -1 if unknown 
   };
   