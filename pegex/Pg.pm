package SQL::Transform::Parser::Pg::AST;

use Moo;

extends 'Pegex::Receiver';

## %% ##

use sanity qw(sanity -warnings/all/FATAL warnings/all);  # keep the syntax checking for now
use parent 'SQL::Translator::Lexer::Pg';
use SQL::Translator::Statement::_classes;
use constant SQL::Translator::Statement::_classes::CONSTANTS;

# ERRCODEs
### FIXME: Convert these to use DBI::Const stuff... ###
use constant {
   WARNING => 19,
   ERROR   => 20,

   ERRCODE_SYNTAX_ERROR            => '42000',
   ERRCODE_FEATURE_NOT_SUPPORTED   => '0A000',
   ERRCODE_INVALID_ESCAPE_SEQUENCE => '22025',
   ERRCODE_INVALID_PARAMETER_VALUE => '22023',
   ERRCODE_WINDOWING_ERROR         => '42P20',
   ERRCODE_NONSTANDARD_USE_OF_ESCAPE_CHARACTER => '22906',
};
## %% ##

### stmtblock: stmtmulti
sub got_stmtblock { $_[0]->{parsetree} = $_[1]; }

### stmtmulti_1: stmtmulti <SEMI> stmt
sub got_stmtmulti_1 { defined $_[3] ? $_[0]->lappend($_[1], $_[3]) : $_[1] }
### stmtmulti_2: stmt
sub got_stmtmulti_2 { defined $_[1] ? $_[0]->lappend($_[1])        : undef }

### CreateRoleStmt: CREATE ROLE RoleId opt_with OptRoleList
sub got_CreateRoleStmt {
   return SQL::Translator::Statement::CreateRole->new(
      stmt_type => ROLESTMT_ROLE,
      role      => $_[3],
      options   => $_[5],
   );
}

### opt_with_1: WITH
sub got_opt_with_1 {}

### OptRoleList_1: OptRoleList CreateOptRoleElem
sub got_OptRoleList_1 { $_[0]->lappend($_[1], $_[2]) }

### AlterOptRoleList_1: AlterOptRoleList AlterOptRoleElem
sub got_AlterOptRoleList_1 { $_[0]->lappend($_[1], $_[2]) }

### AlterOptRoleElem_1: PASSWORD Sconst
sub got_AlterOptRoleElem_1 { $_[0]->makeDefElem("password",            $_[2] ) }
### AlterOptRoleElem_2: PASSWORD NULL
sub got_AlterOptRoleElem_2 { $_[0]->makeDefElem("password",            'NULL') }
### AlterOptRoleElem_3: ENCRYPTED PASSWORD Sconst
sub got_AlterOptRoleElem_3 { $_[0]->makeDefElem("encryptedPassword",   $_[3] ) }
### AlterOptRoleElem_4: UNENCRYPTED PASSWORD Sconst
sub got_AlterOptRoleElem_4 { $_[0]->makeDefElem("unencryptedPassword", $_[3] ) }
### AlterOptRoleElem_5: INHERIT
sub got_AlterOptRoleElem_5 { $_[0]->makeDefElem("inherit",             'TRUE') }
### AlterOptRoleElem_6: CONNECTION LIMIT SignedIconst
sub got_AlterOptRoleElem_6 { $_[0]->makeDefElem("connectionlimit",     $_[3] ) }
# Supported but not documented for roles, for use by ALTER GROUP.
### AlterOptRoleElem_7: VALID UNTIL Sconst
sub got_AlterOptRoleElem_7 { $_[0]->makeDefElem("validUntil",          $_[3] ) }
### AlterOptRoleElem_8: USER name_list
sub got_AlterOptRoleElem_8 { $_[0]->makeDefElem("rolemembers",         $_[2] ) }
# We handle identifiers that aren't parser keywords with
# the following special-case codes, to avoid bloating the
# size of the main parser.
### AlterOptRoleElem_9: IDENT
sub got_AlterOptRoleElem_9 {
   for ($_[1]) {
      when (/^(?:super|create)user$/)   { $_[0]->makeDefElem("superuser",     TRUE ) }
      when (/^no(?:super|create)user$/) { $_[0]->makeDefElem("superuser",     FALSE) }
      when ("createrole")               { $_[0]->makeDefElem("createrole",    TRUE ) }
      when ("nocreaterole")             { $_[0]->makeDefElem("createrole",    FALSE) }
      when ("replication")              { $_[0]->makeDefElem("isreplication", TRUE ) }
      when ("noreplication")            { $_[0]->makeDefElem("isreplication", FALSE) }
      when ("createdb")                 { $_[0]->makeDefElem("createdb",      TRUE ) }
      when ("nocreatedb")               { $_[0]->makeDefElem("createdb",      FALSE) }
      when ("login")                    { $_[0]->makeDefElem("canlogin",      TRUE ) }
      when ("nologin")                  { $_[0]->makeDefElem("canlogin",      FALSE) }
      #* Note that INHERIT is a keyword, so it's handled by main parser, but
      #* NOINHERIT is handled here.
      when ("noinherit")                { $_[0]->makeDefElem("inherit",       FALSE) }
      default {
         $_[0]->ereport(ERROR,
            ERRCODE_SYNTAX_ERROR,
            sprintf("unrecognized role option \"%s\"", $_[1]),
            $_[0]->YYLLoc($_[1], 1));
      }
   }
}

# The following are not supported by ALTER ROLE/USER/GROUP
### CreateOptRoleElem_1: AlterOptRoleElem
sub got_CreateOptRoleElem_1 { $_[1] }
### CreateOptRoleElem_2: SYSID Iconst
sub got_CreateOptRoleElem_2 { $_[0]->makeDefElem("sysid",        $_[2]) }
### CreateOptRoleElem_3: ADMIN name_list
sub got_CreateOptRoleElem_3 { $_[0]->makeDefElem("adminmembers", $_[2]) }
### CreateOptRoleElem_4: ROLE name_list
sub got_CreateOptRoleElem_4 { $_[0]->makeDefElem("rolemembers",  $_[2]) }
### CreateOptRoleElem_5: IN ROLE name_list
sub got_CreateOptRoleElem_5 { $_[0]->makeDefElem("addroleto",    $_[3]) }
### CreateOptRoleElem_6: IN GROUP name_list
sub got_CreateOptRoleElem_6 { $_[0]->makeDefElem("addroleto",    $_[3]) }

### CreateUserStmt: CREATE USER RoleId opt_with OptRoleList
sub got_CreateUserStmt {
   return SQL::Translator::Statement::CreateRole->new(
      stmt_type => ROLESTMT_USER,
      role      => $_[3],
      options   => $_[5],
   );
}

### AlterRoleStmt: ALTER ROLE RoleId opt_with AlterOptRoleList
sub got_AlterRoleStmt {
  return SQL::Translator::Statement::AlterRole->new(
     role     => $_[3],
     action   => +1,   #* add, if there are members
     options  => $_[5],
  );
}

### opt_in_database_1: IN DATABASE database_name
sub got_opt_in_database_1 { $_[3] }

### AlterRoleSetStmt: ALTER ROLE RoleId opt_in_database SetResetClause
sub got_AlterRoleSetStmt {
   return SQL::Translator::Statement::AlterRoleSet->new(
      role     => $_[3],
      database => $_[4],
      setstmt  => $_[5],
   );
}

### AlterUserStmt: ALTER USER RoleId opt_with AlterOptRoleList
sub got_AlterUserStmt {
  return SQL::Translator::Statement::AlterRole->new(
     role     => $_[3],
     action   => +1,   #* add, if there are members
     options  => $_[5],
  );
}

### AlterUserSetStmt: ALTER USER RoleId SetResetClause
sub got_AlterUserSetStmt {
   return SQL::Translator::Statement::AlterRoleSet->new(
      role     => $_[3],
      database => NULL,
      setstmt  => $_[4],
   );
}

### DropRoleStmt_1: DROP ROLE name_list
sub got_DropRoleStmt_1 {
   return SQL::Translator::Statement::DropRole->new(
      missing_ok => FALSE,
      roles      => $_[3],
   );
}
### DropRoleStmt_2: DROP ROLE IF EXISTS name_list
sub got_DropRoleStmt_2 {
   return SQL::Translator::Statement::DropRole->new(
      missing_ok => TRUE,
      roles      => $_[5],
   );
}

### DropUserStmt_1: DROP USER name_list
sub got_DropUserStmt_1 {
   return SQL::Translator::Statement::DropRole->new(
      missing_ok => FALSE,
      roles      => $_[3],
   );
}
### DropUserStmt_2: DROP USER IF EXISTS name_list
sub got_DropUserStmt_2 {
   return SQL::Translator::Statement::DropRole->new(
      roles      => $_[5],
      missing_ok => TRUE,
   );
}

### CreateGroupStmt: CREATE GROUP RoleId opt_with OptRoleList
sub got_CreateGroupStmt {
   return SQL::Translator::Statement::CreateRole->new(
      stmt_type => ROLESTMT_GROUP,
      role      => $_[3],
      options   => $_[5],
   );
}

### AlterGroupStmt: ALTER GROUP RoleId add_drop USER name_list
sub got_AlterGroupStmt {
   return SQL::Translator::Statement::AlterRole->new(
      role    => $_[3],
      action  => $_[4],
      options => $_[0]->lappend( $_[0]->makeDefElem("rolemembers", $_[6]) ),
   );
}

### add_drop_1: ADD
sub got_add_drop_1 { +1 }
### add_drop_2: DROP
sub got_add_drop_2 { -1 }

### DropGroupStmt_1: DROP GROUP name_list
sub got_DropGroupStmt_1 {
   return SQL::Translator::Statement::DropRole->new(
      missing_ok => FALSE,
      roles      => $_[3],
   );
}
### DropGroupStmt_2: DROP GROUP IF EXISTS name_list
sub got_DropGroupStmt_2 {
   return SQL::Translator::Statement::DropRole->new(
      missing_ok => TRUE,
      roles      => $_[5],
   );
}

### CreateSchemaStmt_1: CREATE SCHEMA OptSchemaName AUTHORIZATION RoleId OptSchemaEltList
sub got_CreateSchemaStmt_1 {
   return SQL::Translator::Statement::CreateSchema->new(
      #* One can omit the schema name or the authorization id.
      (defined $_[3]) ? (
         schemaname => $_[3],
      ) : (
         schemaname => $_[5],
         authid     => $_[5],
         schemaElts => $_[6],
      ),
   );
}
### CreateSchemaStmt_2: CREATE SCHEMA ColId OptSchemaEltList
sub got_CreateSchemaStmt_2 {
   return SQL::Translator::Statement::CreateSchema->new(
      #* ...but not both
      schemaname => $_[3],
      authid     => NULL,
      schemaElts => $_[4],
   );
}

### OptSchemaName_1: ColId
sub got_OptSchemaName_1 { $_[1] }

### OptSchemaEltList_1: OptSchemaEltList schema_stmt
sub got_OptSchemaEltList_1 { $_[0]->lappend($_[1], $_[2]) }

### VariableSetStmt_1: SET set_rest
sub got_VariableSetStmt_1 { $_[2]->is_local(0); $_[2] }
### VariableSetStmt_2: SET LOCAL set_rest
sub got_VariableSetStmt_2 { $_[3]->is_local(1); $_[3] }
### VariableSetStmt_3: SET SESSION set_rest
sub got_VariableSetStmt_3 { $_[3]->is_local(0); $_[3] }

### set_rest_1: TRANSACTION transaction_mode_list
sub got_set_rest_1 {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_SET_MULTI,
      name => 'TRANSACTION',
      args => $_[2],
   );
}
### set_rest_2: SESSION CHARACTERISTICS AS TRANSACTION transaction_mode_list
sub got_set_rest_2 {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_SET_MULTI,
      name => 'SESSION CHARACTERISTICS',
      args => $_[5],
   );
}

### set_rest_more_1 : 
# Generic SET syntaxes:
    var_name TO var_list
sub got_set_rest_more_1  {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_SET_VALUE,
      name => $_[1],
      args => $_[3],
   );
}
### set_rest_more_2 : var_name <EQUAL> var_list
sub got_set_rest_more_2  {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_SET_VALUE,
      name => $_[1],
      args => $_[3],
   );
}
### set_rest_more_3 : var_name TO DEFAULT
sub got_set_rest_more_3  {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_SET_DEFAULT,
      name => $_[1],
   );
}
### set_rest_more_4 : var_name <EQUAL> DEFAULT
sub got_set_rest_more_4  {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_SET_DEFAULT,
      name => $_[1],
   );
}
# Special syntaxes mandated by SQL standard:
### set_rest_more_5 : var_name FROM CURRENT
sub got_set_rest_more_5  {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_SET_CURRENT,
      name => $_[1],
   );
}
### set_rest_more_6 : TIME ZONE zone_value
sub got_set_rest_more_6  {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_SET_VALUE,
      name => 'timezone',
      (defined $_[3]) ?
         (args => $_[0]->lappend($_[3])) :
         (kind => VAR_SET_DEFAULT)
   );
}
### set_rest_more_7 : CATALOG Sconst
sub got_set_rest_more_7  {
   $_[0]->ereport(ERROR,
         ERRCODE_FEATURE_NOT_SUPPORTED,
          "current database cannot be changed",
          $_[0]->YYLLoc($_[2], 2));
   return NULL; #*not reached
}
### set_rest_more_8 : SCHEMA Sconst
sub got_set_rest_more_8  {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_SET_VALUE,
      name => 'search_path',
      args => $_[0]->lappend($_[0]->makeStringConst($_[2], $_[0]->YYLLoc($_[2], 2))),
   );
}
### set_rest_more_9 : NAMES opt_encoding
sub got_set_rest_more_9  {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_SET_VALUE,
      name => 'client_encoding',
      (defined $_[2]) ?
         (args => $_[0]->lappend($_[0]->makeStringConst($_[2], $_[0]->YYLLoc($_[2], 2)))) :
         (kind => VAR_SET_DEFAULT),
   );
}
### set_rest_more_10: ROLE ColId_or_Sconst
sub got_set_rest_more_10 {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_SET_VALUE,
      name => 'role',
      args => $_[0]->lappend($_[0]->makeStringConst($_[2], $_[0]->YYLLoc($_[2], 2))),
   );
}
### set_rest_more_11: SESSION AUTHORIZATION ColId_or_Sconst
sub got_set_rest_more_11 {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_SET_VALUE,
      name => 'session_authorization',
      args => $_[0]->lappend($_[0]->makeStringConst($_[3], $_[0]->YYLLoc($_[3], 3))),
   );
}
### set_rest_more_12: SESSION AUTHORIZATION DEFAULT
sub got_set_rest_more_12 {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_SET_DEFAULT,
      name => 'session_authorization',
   );
}
# Special syntaxes invented by PostgreSQL:
### set_rest_more_13: XML OPTION document_or_content
sub got_set_rest_more_13 {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_SET_VALUE,
      name => 'xmloption',
      args => $_[0]->lappend($_[0]->makeStringConst($_[3], $_[0]->YYLLoc($_[3], 3))),
   );
}
### set_rest_more_14: TRANSACTION SNAPSHOT Sconst
sub got_set_rest_more_14 {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_SET_MULTI,
      name => 'transaction_snapshot',
      args => [ $_[0]->makeStringConst($_[3], $_[0]->YYLLoc($_[3], 3)) ],
   );
}

### var_name_1: ColId
sub got_var_name_1 { $_[1] }
### var_name_2: var_name <DOT> ColId
sub got_var_name_2 { sprintf("%s.%s", $_[1], $_[3]) }

### var_list_1: var_value
sub got_var_list_1 { $_[0]->lappend($_[1]) }
### var_list_2: var_list <COMMA> var_value
sub got_var_list_2 { $_[0]->lappend($_[1], $_[3]) }

### var_value_1: opt_boolean_or_string
sub got_var_value_1 { $_[0]->makeStringConst($_[1], $_[0]->YYLLoc($_[1], 1)) }
### var_value_2: NumericOnly
sub got_var_value_2 { $_[0]->makeAConst     ($_[1], $_[0]->YYLLoc($_[1], 1)) }

### iso_level_1: READ UNCOMMITTED
sub got_iso_level_1 { "read uncommitted" }
### iso_level_2: READ COMMITTED
sub got_iso_level_2 { "read committed"   }
### iso_level_3: REPEATABLE READ
sub got_iso_level_3 { "repeatable read"  }
### iso_level_4: SERIALIZABLE
sub got_iso_level_4 { "serializable"     }

### opt_boolean_or_string_1: TRUE
sub got_opt_boolean_or_string_1 { "TRUE" }
### opt_boolean_or_string_2: FALSE
sub got_opt_boolean_or_string_2 { "FALSE" }
# OFF is also accepted as a boolean value, but is handled
# by the ColId rule below. The action for booleans and strings
# is the same, so we don't need to distinguish them here.
### opt_boolean_or_string_3: ON
sub got_opt_boolean_or_string_3 { "on" }
### opt_boolean_or_string_4: ColId_or_Sconst
sub got_opt_boolean_or_string_4 { $_[1] }

### zone_value_1: Sconst
sub got_zone_value_1 { $_[0]->makeStringConst($_[1], $_[0]->YYLLoc($_[1], 1)) }
### zone_value_2: IDENT
sub got_zone_value_2 { $_[0]->makeStringConst($_[1], $_[0]->YYLLoc($_[1], 1)) }
### zone_value_3: ConstInterval Sconst opt_interval
sub got_zone_value_3 {
   my $t = $_[1];
   if (defined $_[3]) {
      my $n = $_[3]->[0];
      if (($n->val & ~(INTERVAL_MASK_HOUR | INTERVAL_MASK_MINUTE)) != 0) {
         $_[0]->ereport(ERROR,
               ERRCODE_SYNTAX_ERROR,
                "Time zone interval must be HOUR or HOUR TO MINUTE",
                $_[0]->YYLLoc($_[3], 3));
      }
   }
   $t->typmods($_[3]);
   return $_[0]->makeStringConstCast($_[2], $_[0]->YYLLoc($_[2], 2), $t);
}
### zone_value_4: ConstInterval <LPAREN> Iconst <RPAREN> Sconst opt_interval
sub got_zone_value_4 {
   my $t = $_[1];
   if (defined $_[6]) {
      my $n = $_[6]->[0];
      if (($n->val & ~(INTERVAL_MASK_HOUR | INTERVAL_MASK_MINUTE)) != 0) {
         $_[0]->ereport(ERROR,
               ERRCODE_SYNTAX_ERROR,
                "Time zone interval must be HOUR or HOUR TO MINUTE",
                $_[0]->YYLLoc($_[6], 6));
      }
      if (@{$_[6]} != 1) {
         $_[0]->ereport(ERROR,
               ERRCODE_SYNTAX_ERROR,
                "interval precision specified twice",
                $_[0]->YYLLoc($_[1], 1));
      }
      $t->typmods( $_[0]->lappend($_[6],                                        $_[0]->makeIntConst($_[3], $_[0]->YYLLoc($_[3], 3))) );
   }
   else {
      $t->typmods( $_[0]->lappend($_[0]->makeIntConst(INTERVAL_FULL_RANGE, {}), $_[0]->makeIntConst($_[3], $_[0]->YYLLoc($_[3], 3))) );
   }
   return $_[0]->makeStringConstCast($_[5], $_[0]->YYLLoc($_[5], 5), $t);
}
### zone_value_5: NumericOnly
sub got_zone_value_5 { $_[0]->makeAConst($_[1], $_[0]->YYLLoc($_[1], 1)) }
### zone_value_6: DEFAULT
sub got_zone_value_6 { NULL }
### zone_value_7: LOCAL
sub got_zone_value_7 { NULL }

### opt_encoding_1: Sconst
sub got_opt_encoding_1 { $_[1] }
### opt_encoding_2: DEFAULT
sub got_opt_encoding_2 { NULL  }

### ColId_or_Sconst_1: ColId
sub got_ColId_or_Sconst_1 { $_[1] }
### ColId_or_Sconst_2: Sconst
sub got_ColId_or_Sconst_2 { $_[1] }

### VariableResetStmt_1: RESET var_name
sub got_VariableResetStmt_1 {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_RESET,
      name => $_[2],
   );
}
### VariableResetStmt_2: RESET TIME ZONE
sub got_VariableResetStmt_2 {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_RESET,
      name => 'timezone',
   );
}
### VariableResetStmt_3: RESET TRANSACTION ISOLATION LEVEL
sub got_VariableResetStmt_3 {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_RESET,
      name => 'transaction_isolation',
   );
}
### VariableResetStmt_4: RESET SESSION AUTHORIZATION
sub got_VariableResetStmt_4 {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_RESET,
      name => 'session_authorization',
   );
}
### VariableResetStmt_5: RESET ALL
sub got_VariableResetStmt_5 {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_RESET_ALL,
   );
}

### SetResetClause_1: SET set_rest
sub got_SetResetClause_1 { $_[2] }
### SetResetClause_2: VariableResetStmt
sub got_SetResetClause_2 { $_[1] }

### FunctionSetResetClause_1: SET set_rest_more
sub got_FunctionSetResetClause_1 { $_[2] }
### FunctionSetResetClause_2: VariableResetStmt
sub got_FunctionSetResetClause_2 { $_[1] }

### VariableShowStmt_1: SHOW var_name
sub got_VariableShowStmt_1 {
   return SQL::Translator::Statement::VariableShow->new(
      name => $_[2],
   );
}
### VariableShowStmt_2: SHOW TIME ZONE
sub got_VariableShowStmt_2 {
   return SQL::Translator::Statement::VariableShow->new(
      name => 'timezone',
   );
}
### VariableShowStmt_3: SHOW TRANSACTION ISOLATION LEVEL
sub got_VariableShowStmt_3 {
   return SQL::Translator::Statement::VariableShow->new(
      name => 'transaction_isolation',
   );
}
### VariableShowStmt_4: SHOW SESSION AUTHORIZATION
sub got_VariableShowStmt_4 {
   return SQL::Translator::Statement::VariableShow->new(
      name => 'session_authorization',
   );
}
### VariableShowStmt_5: SHOW ALL
sub got_VariableShowStmt_5 {
   return SQL::Translator::Statement::VariableShow->new(
      name => 'all',
   );
}

### ConstraintsSetStmt: SET CONSTRAINTS constraints_set_list constraints_set_mode
sub got_ConstraintsSetStmt {
   return SQL::Translator::Statement::ConstraintsSet->new(
      constraints => $_[3],
      deferred    => $_[4],
   );
}

### constraints_set_list_1: ALL
sub got_constraints_set_list_1 { NIL   }
### constraints_set_list_2: qualified_name_list
sub got_constraints_set_list_2 { $_[1] }

### constraints_set_mode_1: DEFERRED
sub got_constraints_set_mode_1 { TRUE  }
### constraints_set_mode_2: IMMEDIATE
sub got_constraints_set_mode_2 { FALSE }

### CheckPointStmt: CHECKPOINT
sub got_CheckPointStmt { SQL::Translator::Statement::CheckPoint->new() }

### DiscardStmt_1: DISCARD ALL
sub got_DiscardStmt_1 {
   return SQL::Translator::Statement::Discard->new(
      target => DISCARD_ALL,
   );
}
### DiscardStmt_2: DISCARD TEMP
sub got_DiscardStmt_2 {
   return SQL::Translator::Statement::Discard->new(
      target => DISCARD_TEMP,
   );
}
### DiscardStmt_3: DISCARD TEMPORARY
sub got_DiscardStmt_3 {
   return SQL::Translator::Statement::Discard->new(
      target => DISCARD_TEMP,
   );
}
### DiscardStmt_4: DISCARD PLANS
sub got_DiscardStmt_4 {
   return SQL::Translator::Statement::Discard->new(
      target => DISCARD_PLANS,
   );
}

### AlterTableStmt_1: ALTER TABLE relation_expr alter_table_cmds
sub got_AlterTableStmt_1 {
   return SQL::Translator::Statement::AlterTable->new(
      relation   => $_[3],
      cmds       => $_[4],
      relkind    => OBJECT_TABLE,
      missing_ok => FALSE,
   );
}
### AlterTableStmt_2: ALTER TABLE IF EXISTS relation_expr alter_table_cmds
sub got_AlterTableStmt_2 {
   return SQL::Translator::Statement::AlterTable->new(
      relation   => $_[5],
      cmds       => $_[6],
      relkind    => OBJECT_TABLE,
      missing_ok => TRUE,
   );
}
### AlterTableStmt_3: ALTER INDEX qualified_name alter_table_cmds
sub got_AlterTableStmt_3 {
   return SQL::Translator::Statement::AlterTable->new(
      relation   => $_[3],
      cmds       => $_[4],
      relkind    => OBJECT_INDEX,
      missing_ok => FALSE,
   );
}
### AlterTableStmt_4: ALTER INDEX IF EXISTS qualified_name alter_table_cmds
sub got_AlterTableStmt_4 {
   return SQL::Translator::Statement::AlterTable->new(
      relation   => $_[5],
      cmds       => $_[6],
      relkind    => OBJECT_INDEX,
      missing_ok => TRUE,
   );
}
### AlterTableStmt_5: ALTER SEQUENCE qualified_name alter_table_cmds
sub got_AlterTableStmt_5 {
   return SQL::Translator::Statement::AlterTable->new(
      relation   => $_[3],
      cmds       => $_[4],
      relkind    => OBJECT_SEQUENCE,
      missing_ok => FALSE,
   );
}
### AlterTableStmt_6: ALTER SEQUENCE IF EXISTS qualified_name alter_table_cmds
sub got_AlterTableStmt_6 {
   return SQL::Translator::Statement::AlterTable->new(
      relation   => $_[5],
      cmds       => $_[6],
      relkind    => OBJECT_SEQUENCE,
      missing_ok => TRUE,
   );
}
### AlterTableStmt_7: ALTER VIEW qualified_name alter_table_cmds
sub got_AlterTableStmt_7 {
   return SQL::Translator::Statement::AlterTable->new(
      relation   => $_[3],
      cmds       => $_[4],
      relkind    => OBJECT_VIEW,
      missing_ok => FALSE,
   );
}
### AlterTableStmt_8: ALTER VIEW IF EXISTS qualified_name alter_table_cmds
sub got_AlterTableStmt_8 {
   return SQL::Translator::Statement::AlterTable->new(
      relation   => $_[5],
      cmds       => $_[6],
      relkind    => OBJECT_VIEW,
      missing_ok => TRUE,
   );
}

### alter_table_cmds_1: alter_table_cmd
sub got_alter_table_cmds_1 { $_[0]->lappend($_[1]);       }
### alter_table_cmds_2: alter_table_cmds <COMMA> alter_table_cmd
sub got_alter_table_cmds_2 { $_[0]->lappend($_[1], $_[3]) }

# ALTER TABLE <name> ADD COLUMN <coldef>
### alter_table_cmd_1 : 
# ALTER TABLE <name> ADD <coldef>
    ADD columnDef
sub got_alter_table_cmd_1  {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_AddColumn,
      def        => $_[2],
   );
}
# ALTER TABLE <name> ALTER [COLUMN] <colname> {SET DEFAULT <expr>|DROP DEFAULT}
### alter_table_cmd_2 : ADD COLUMN columnDef
sub got_alter_table_cmd_2  {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_AddColumn,
      def        => $_[3],
   );
}
# ALTER TABLE <name> ALTER [COLUMN] <colname> DROP NOT NULL
### alter_table_cmd_3 : ALTER opt_column ColId alter_column_default
sub got_alter_table_cmd_3  {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_ColumnDefault,
      name       => $_[3],
      def        => $_[4],
   );
}
# ALTER TABLE <name> ALTER [COLUMN] <colname> SET NOT NULL
### alter_table_cmd_4 : ALTER opt_column ColId DROP NOT NULL
sub got_alter_table_cmd_4  {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_DropNotNull,
      name       => $_[3],
   );
}
# ALTER TABLE <name> ALTER [COLUMN] <colname> SET STATISTICS <SignedIconst>
### alter_table_cmd_5 : ALTER opt_column ColId SET NOT NULL
sub got_alter_table_cmd_5  {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_SetNotNull,
      name       => $_[3],
   );
}
# ALTER TABLE <name> ALTER [COLUMN] <colname> SET ( column_parameter = value [, ... ] )
### alter_table_cmd_6 : ALTER opt_column ColId SET STATISTICS SignedIconst
sub got_alter_table_cmd_6  {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_SetStatistics,
      name       => $_[3],
      def        => $_[6]+0,
   );
}
# ALTER TABLE <name> ALTER [COLUMN] <colname> SET ( column_parameter = value [, ... ] )
### alter_table_cmd_7 : ALTER opt_column ColId SET reloptions
sub got_alter_table_cmd_7  {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_SetOptions,
      name       => $_[3],
      def        => $_[5],
   );
}
# ALTER TABLE <name> ALTER [COLUMN] <colname> SET STORAGE <storagemode>
### alter_table_cmd_8 : ALTER opt_column ColId RESET reloptions
sub got_alter_table_cmd_8  {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_ResetOptions,
      name       => $_[3],
      def        => $_[5],
   );
}
# ALTER TABLE <name> DROP [COLUMN] IF EXISTS <colname> [RESTRICT|CASCADE]
### alter_table_cmd_9 : ALTER opt_column ColId SET STORAGE ColId
sub got_alter_table_cmd_9  {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_SetStorage,
      name       => $_[3],
      def        => $_[6],
   );
}
# ALTER TABLE <name> DROP [COLUMN] <colname> [RESTRICT|CASCADE]
### alter_table_cmd_10: DROP opt_column IF EXISTS ColId opt_drop_behavior
sub got_alter_table_cmd_10 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_DropColumn,
      name       => $_[5],
      behavior   => $_[6],
      missing_ok => TRUE,
   );
}
# ALTER TABLE <name> ALTER [COLUMN] <colname> [SET DATA] TYPE <typename>
#      [ USING <expression> ]
### alter_table_cmd_11: DROP opt_column ColId opt_drop_behavior
sub got_alter_table_cmd_11 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_DropColumn,
      name       => $_[3],
      behavior   => $_[4],
      missing_ok => FALSE,
   );
}
# ALTER FOREIGN TABLE <name> ALTER [COLUMN] <colname> OPTIONS
### alter_table_cmd_12: ALTER opt_column ColId opt_set_data TYPE Typename opt_collate_clause alter_using
sub got_alter_table_cmd_12 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype => AT_AlterColumnType,
      name    => $_[3],
      def     => SQL::Translator::Statement::Column::Definition->new(
         #* We only use these three fields of the ColumnDef node
         typeName    => $_[6],
         collClause  => $_[7],
         raw_default => $_[8],
      ),
   );
}
# ALTER TABLE <name> ADD CONSTRAINT ...
### alter_table_cmd_13: ALTER opt_column ColId alter_generic_options
sub got_alter_table_cmd_13 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype => AT_AlterColumnGenericOptions,
      name    => $_[3],
      def     => $_[4],
   );
}
# ALTER TABLE <name> VALIDATE CONSTRAINT ...
### alter_table_cmd_14: ADD TableConstraint
sub got_alter_table_cmd_14 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_AddConstraint,
      def        => $_[2],
   );
}
# ALTER TABLE <name> DROP CONSTRAINT IF EXISTS <name> [RESTRICT|CASCADE]
### alter_table_cmd_15: VALIDATE CONSTRAINT name
sub got_alter_table_cmd_15 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_ValidateConstraint,
      name       => $_[3],
   );
}
# ALTER TABLE <name> DROP CONSTRAINT <name> [RESTRICT|CASCADE]
### alter_table_cmd_16: DROP CONSTRAINT IF EXISTS name opt_drop_behavior
sub got_alter_table_cmd_16 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_DropConstraint,
      name       => $_[5],
      behavior   => $_[6],
      missing_ok => TRUE,
   );
}
# ALTER TABLE <name> SET WITH OIDS
### alter_table_cmd_17: DROP CONSTRAINT name opt_drop_behavior
sub got_alter_table_cmd_17 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_DropConstraint,
      name       => $_[3],
      behavior   => $_[4],
      missing_ok => FALSE,
   );
}
# ALTER TABLE <name> SET WITHOUT OIDS
### alter_table_cmd_18: SET WITH OIDS
sub got_alter_table_cmd_18 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_AddOids,
   );
}
# ALTER TABLE <name> CLUSTER ON <indexname>
### alter_table_cmd_19: SET WITHOUT OIDS
sub got_alter_table_cmd_19 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_DropOids,
   );
}
# ALTER TABLE <name> SET WITHOUT CLUSTER
### alter_table_cmd_20: CLUSTER ON name
sub got_alter_table_cmd_20 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_ClusterOn,
      name       => $_[3],
   );
}
# ALTER TABLE <name> ENABLE TRIGGER <trig>
### alter_table_cmd_21: SET WITHOUT CLUSTER
sub got_alter_table_cmd_21 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_DropCluster,
      name       => NULL,
   );
}
# ALTER TABLE <name> ENABLE ALWAYS TRIGGER <trig>
### alter_table_cmd_22: ENABLE TRIGGER name
sub got_alter_table_cmd_22 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_EnableTrig,
      name       => $_[3],
   );
}
# ALTER TABLE <name> ENABLE REPLICA TRIGGER <trig>
### alter_table_cmd_23: ENABLE ALWAYS TRIGGER name
sub got_alter_table_cmd_23 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_EnableAlwaysTrig,
      name       => $_[4],
   );
}
# ALTER TABLE <name> ENABLE TRIGGER ALL
### alter_table_cmd_24: ENABLE REPLICA TRIGGER name
sub got_alter_table_cmd_24 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_EnableReplicaTrig,
      name       => $_[4],
   );
}
# ALTER TABLE <name> ENABLE TRIGGER USER
### alter_table_cmd_25: ENABLE TRIGGER ALL
sub got_alter_table_cmd_25 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_EnableTrigAll,
   );
}
# ALTER TABLE <name> DISABLE TRIGGER <trig>
### alter_table_cmd_26: ENABLE TRIGGER USER
sub got_alter_table_cmd_26 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_EnableTrigUser,
   );
}
# ALTER TABLE <name> DISABLE TRIGGER ALL
### alter_table_cmd_27: DISABLE TRIGGER name
sub got_alter_table_cmd_27 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_DisableTrig,
      name       => $_[3],
   );
}
# ALTER TABLE <name> DISABLE TRIGGER USER
### alter_table_cmd_28: DISABLE TRIGGER ALL
sub got_alter_table_cmd_28 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_DisableTrigAll,
   );
}
# ALTER TABLE <name> ENABLE RULE <rule>
### alter_table_cmd_29: DISABLE TRIGGER USER
sub got_alter_table_cmd_29 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_DisableTrigUser,
   );
}
# ALTER TABLE <name> ENABLE ALWAYS RULE <rule>
### alter_table_cmd_30: ENABLE RULE name
sub got_alter_table_cmd_30 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_EnableRule,
      name       => $_[3],
   );
}
# ALTER TABLE <name> ENABLE REPLICA RULE <rule>
### alter_table_cmd_31: ENABLE ALWAYS RULE name
sub got_alter_table_cmd_31 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_EnableAlwaysRule,
      name       => $_[4],
   );
}
# ALTER TABLE <name> DISABLE RULE <rule>
### alter_table_cmd_32: ENABLE REPLICA RULE name
sub got_alter_table_cmd_32 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_EnableReplicaRule,
      name       => $_[4],
   );
}
# ALTER TABLE <name> INHERIT <parent>
### alter_table_cmd_33: DISABLE RULE name
sub got_alter_table_cmd_33 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_DisableRule,
      name       => $_[3],
   );
}
# ALTER TABLE <name> NO INHERIT <parent>
### alter_table_cmd_34: INHERIT qualified_name
sub got_alter_table_cmd_34 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_AddInherit,
      def        =>  $_[2],
   );
}
# ALTER TABLE <name> OF <type_name>
### alter_table_cmd_35: NO INHERIT qualified_name
sub got_alter_table_cmd_35 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_DropInherit,
      def        =>  $_[3],
   );
}
# ALTER TABLE <name> NOT OF
### alter_table_cmd_36: OF any_name
sub got_alter_table_cmd_36 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_AddOf,
      def        => $_[0]->makeTypeNameFromNameList($_[2]),
   );
}
# ALTER TABLE <name> OWNER TO RoleId
### alter_table_cmd_37: NOT OF
sub got_alter_table_cmd_37 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_DropOf,
   );
}
# ALTER TABLE <name> SET TABLESPACE <tablespacename>
### alter_table_cmd_38: OWNER TO RoleId
sub got_alter_table_cmd_38 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_ChangeOwner,
      name       => $_[3],
   );
}
# ALTER TABLE <name> SET (...)
### alter_table_cmd_39: SET TABLESPACE name
sub got_alter_table_cmd_39 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_SetTableSpace,
      name       => $_[3],
   );
}
# ALTER TABLE <name> RESET (...)
### alter_table_cmd_40: SET reloptions
sub got_alter_table_cmd_40 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_SetRelOptions,
      def        => $_[2],
   );
}
### alter_table_cmd_41: RESET reloptions
sub got_alter_table_cmd_41 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_ResetRelOptions,
      def        => $_[2],
   );
}
### alter_table_cmd_42: alter_generic_options
sub got_alter_table_cmd_42 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_GenericOptions,
      def        => $_[1],
   );
}

### alter_column_default_1: SET DEFAULT a_expr
sub got_alter_column_default_1 { $_[3] }
### alter_column_default_2: DROP DEFAULT
sub got_alter_column_default_2 { NULL  }

### opt_drop_behavior_1: CASCADE
sub got_opt_drop_behavior_1 { DROP_CASCADE  }
### opt_drop_behavior_2: RESTRICT
sub got_opt_drop_behavior_2 { DROP_RESTRICT }

### opt_collate_clause_1: COLLATE any_name
sub got_opt_collate_clause_1 {
   return SQL::Translator::Statement::CollateClause->new(
      arg      => NULL,
      collname => $_[2],
      location => $_[0]->YYLLoc($_[1], 1),
   );
}

### alter_using_1: USING a_expr
sub got_alter_using_1 { $_[2] }

### reloptions: <LPAREN> reloption_list <RPAREN>
sub got_reloptions { $_[2] }

### opt_reloptions_1: WITH reloptions
sub got_opt_reloptions_1 { $_[2] }

### reloption_list_1: reloption_elem
sub got_reloption_list_1 { $_[0]->lappend($_[1]) }
### reloption_list_2: reloption_list <COMMA> reloption_elem
sub got_reloption_list_2 { $_[0]->lappend($_[1], $_[3]) }

### reloption_elem_1: ColLabel <EQUAL> def_arg
sub got_reloption_elem_1 { $_[0]->makeDefElem        ($_[1], $_[3])                        }
### reloption_elem_2: ColLabel
sub got_reloption_elem_2 { $_[0]->makeDefElem        ($_[1], NULL)                         }
### reloption_elem_3: ColLabel <DOT> ColLabel <EQUAL> def_arg
sub got_reloption_elem_3 { $_[0]->makeDefElemExtended($_[1], $_[3], $_[5], DEFELEM_UNSPEC) }
### reloption_elem_4: ColLabel <DOT> ColLabel
sub got_reloption_elem_4 { $_[0]->makeDefElemExtended($_[1], $_[3], NULL,  DEFELEM_UNSPEC) }

### AlterCompositeTypeStmt: ALTER TYPE any_name alter_type_cmds
sub got_AlterCompositeTypeStmt {
   return SQL::Translator::Statement::AlterTable->new(
      #* can't use qualified_name, sigh
      relation => $_[0]->makeRangeVarFromAnyName($_[3], $_[0]->YYLLoc($_[3], 3)),
      cmds     => $_[4],
      relkind  => OBJECT_TYPE,
   );
}

### alter_type_cmds_1: alter_type_cmd
sub got_alter_type_cmds_1 { $_[0]->lappend($_[1])        }
### alter_type_cmds_2: alter_type_cmds <COMMA> alter_type_cmd
sub got_alter_type_cmds_2 { $_[0]->lappend($_[1], $_[3]) }

# ALTER TYPE <name> DROP ATTRIBUTE IF EXISTS <attname> [RESTRICT|CASCADE]
### alter_type_cmd_1: 
# ALTER TYPE <name> ADD ATTRIBUTE <coldef> [RESTRICT|CASCADE]
    ADD ATTRIBUTE TableFuncElement opt_drop_behavior
sub got_alter_type_cmd_1 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_AddColumn,
      def        => $_[3],
      behavior   => $_[4],
   );
}
# ALTER TYPE <name> DROP ATTRIBUTE <attname> [RESTRICT|CASCADE]
### alter_type_cmd_2: DROP ATTRIBUTE IF EXISTS ColId opt_drop_behavior
sub got_alter_type_cmd_2 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_DropColumn,
      name       => $_[5],
      behavior   => $_[6],
      missing_ok => TRUE,
   );
}
# ALTER TYPE <name> ALTER ATTRIBUTE <attname> [SET DATA] TYPE <typename> [RESTRICT|CASCADE]
### alter_type_cmd_3: DROP ATTRIBUTE ColId opt_drop_behavior
sub got_alter_type_cmd_3 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_DropColumn,
      name       => $_[3],
      behavior   => $_[4],
      missing_ok => FALSE,
   );
}
### alter_type_cmd_4: ALTER ATTRIBUTE ColId opt_set_data TYPE Typename opt_collate_clause opt_drop_behavior
sub got_alter_type_cmd_4 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_AlterColumnType,
      name       => $_[3],
      def        => SQL::Translator::Statement::Column::Definition->new(
         #* We only use these three fields of the ColumnDef node
         typeName    => $_[6],
         collClause  => $_[7],
         raw_default => undef,
      ),
      behavior   => $_[8],
   );
}

### ClosePortalStmt_1: CLOSE cursor_name
sub got_ClosePortalStmt_1 {
   return SQL::Translator::Statement::ClosePortal->new(
      portalname => $_[2],
   );
}
### ClosePortalStmt_2: CLOSE ALL
sub got_ClosePortalStmt_2 {
   return SQL::Translator::Statement::ClosePortal->new(
      portalname => NULL,
   );
}

### CopyStmt_1: COPY opt_binary qualified_name opt_column_list opt_oids copy_from copy_file_name copy_delimiter opt_with copy_options
sub got_CopyStmt_1 {
   return SQL::Translator::Statement::Copy->new(
      relation => $_[3],
      query    => NULL,
      attlist  => $_[4],
      is_from  => $_[6],
      filename => $_[7],

      #* Concatenate user-supplied flags
      options  => $_[0]->lappend(@_[2,5,8,10]),
   );
}
### CopyStmt_2: COPY select_with_parens TO copy_file_name opt_with copy_options
sub got_CopyStmt_2 {
   return SQL::Translator::Statement::Copy->new(
      relation => NULL,
      query    => $_[2],
      attlist  => NIL,
      is_from  => FALSE,
      filename => $_[4],
      options  => $_[6],
   );
}

### copy_from_1: FROM
sub got_copy_from_1 { TRUE  }
### copy_from_2: TO
sub got_copy_from_2 { FALSE }

### copy_file_name_1: Sconst
sub got_copy_file_name_1 { $_[1] }
### copy_file_name_2: STDIN
sub got_copy_file_name_2 { NULL  }
### copy_file_name_3: STDOUT
sub got_copy_file_name_3 { NULL  }

### copy_options_1: copy_opt_list
sub got_copy_options_1 { $_[1] }
### copy_options_2: <LPAREN> copy_generic_opt_list <RPAREN>
sub got_copy_options_2 { $_[2] }

### copy_opt_list_1: copy_opt_list copy_opt_item
sub got_copy_opt_list_1 { $_[0]->lappend($_[1], $_[2]) }

### copy_opt_item_1 : BINARY
sub got_copy_opt_item_1  { $_[0]->makeDefElem("format",         "binary") }
### copy_opt_item_2 : OIDS
sub got_copy_opt_item_2  { $_[0]->makeDefElem("oids",           TRUE);     }
### copy_opt_item_3 : DELIMITER opt_as Sconst
sub got_copy_opt_item_3  { $_[0]->makeDefElem("delimiter",      $_[3]);    }
### copy_opt_item_4 : NULL opt_as Sconst
sub got_copy_opt_item_4  { $_[0]->makeDefElem("null",           $_[3]);    }
### copy_opt_item_5 : CSV
sub got_copy_opt_item_5  { $_[0]->makeDefElem("format",         "csv");    }
### copy_opt_item_6 : HEADER
sub got_copy_opt_item_6  { $_[0]->makeDefElem("header",         TRUE);     }
### copy_opt_item_7 : QUOTE opt_as Sconst
sub got_copy_opt_item_7  { $_[0]->makeDefElem("quote",          $_[3]);    }
### copy_opt_item_8 : ESCAPE opt_as Sconst
sub got_copy_opt_item_8  { $_[0]->makeDefElem("escape",         $_[3]);    }
### copy_opt_item_9 : FORCE QUOTE columnList
sub got_copy_opt_item_9  { $_[0]->makeDefElem("force_quote",    $_[3]);    }
### copy_opt_item_10: FORCE QUOTE <STAR>
sub got_copy_opt_item_10 { $_[0]->makeDefElem("force_quote",    SQL::Translator::Statement::A_Star->new()) }
### copy_opt_item_11: FORCE NOT NULL columnList
sub got_copy_opt_item_11 { $_[0]->makeDefElem("force_not_null", $_[4]);    }
### copy_opt_item_12: ENCODING Sconst
sub got_copy_opt_item_12 { $_[0]->makeDefElem("encoding",       $_[2]);    }

### opt_binary_1: BINARY
sub got_opt_binary_1 { $_[0]->makeDefElem("format",    "binary") }

### opt_oids_1: WITH OIDS
sub got_opt_oids_1 { $_[0]->makeDefElem("oids",      TRUE);     }

### copy_delimiter_1: opt_using DELIMITERS Sconst
sub got_copy_delimiter_1 { $_[0]->makeDefElem("delimiter", $_[3]);    }

### opt_using_1: USING
sub got_opt_using_1 {}

### copy_generic_opt_list_1: copy_generic_opt_elem
sub got_copy_generic_opt_list_1 { $_[0]->lappend($_[1]);            }
### copy_generic_opt_list_2: copy_generic_opt_list <COMMA> copy_generic_opt_elem
sub got_copy_generic_opt_list_2 { $_[0]->lappend($_[1], $_[3]);     }

### copy_generic_opt_elem: ColLabel copy_generic_opt_arg
sub got_copy_generic_opt_elem { $_[0]->makeDefElem($_[1], $_[2]) }

### copy_generic_opt_arg_1: opt_boolean_or_string
sub got_copy_generic_opt_arg_1 { $_[1] }
### copy_generic_opt_arg_2: NumericOnly
sub got_copy_generic_opt_arg_2 { $_[1] }
### copy_generic_opt_arg_3: <STAR>
sub got_copy_generic_opt_arg_3 { SQL::Translator::Statement::A_Star->new() }
### copy_generic_opt_arg_4: <LPAREN> copy_generic_opt_arg_list <RPAREN>
sub got_copy_generic_opt_arg_4 { $_[2] }

### copy_generic_opt_arg_list_1: copy_generic_opt_arg_list_item
sub got_copy_generic_opt_arg_list_1 { $_[0]->lappend($_[1]);        }
### copy_generic_opt_arg_list_2: copy_generic_opt_arg_list <COMMA> copy_generic_opt_arg_list_item
sub got_copy_generic_opt_arg_list_2 { $_[0]->lappend($_[1], $_[3]) }

### copy_generic_opt_arg_list_item: opt_boolean_or_string
sub got_copy_generic_opt_arg_list_item { $_[1] }

### CreateStmt_1: CREATE OptTemp TABLE qualified_name <LPAREN> OptTableElementList <RPAREN> OptInherit OptWith OnCommitOption OptTableSpace
sub got_CreateStmt_1 {
   $_[4]->relpersistence($_[2]);
   return SQL::Translator::Statement::Create->new(
      relation       => $_[4],
      tableElts      => $_[6],
      inhRelations   => $_[8],
      constraints    => NIL,
      options        => $_[9],
      oncommit       => $_[10],
      tablespacename => $_[11],
      if_not_exists  => FALSE,
   );
}
### CreateStmt_2: CREATE OptTemp TABLE IF NOT EXISTS qualified_name <LPAREN> OptTableElementList <RPAREN> OptInherit OptWith OnCommitOption OptTableSpace
sub got_CreateStmt_2 {
   $_[7]->relpersistence($_[2]);
   return SQL::Translator::Statement::Create->new(
      relation       => $_[7],
      tableElts      => $_[9],
      inhRelations   => $_[11],
      constraints    => NIL,
      options        => $_[12],
      oncommit       => $_[13],
      tablespacename => $_[14],
      if_not_exists  => TRUE,
   );
}
### CreateStmt_3: CREATE OptTemp TABLE qualified_name OF any_name OptTypedTableElementList OptWith OnCommitOption OptTableSpace
sub got_CreateStmt_3 {
   my $ofTypename = $_[0]->makeTypeNameFromNameList($_[6]);
   $ofTypename->_set_location( $_[0]->YYLLoc($_[6], 6) );

   $_[4]->relpersistence($_[2]);
   return SQL::Translator::Statement::Create->new(
      relation       => $_[4],
      tableElts      => $_[7],
      ofTypename     => $ofTypename,
      constraints    => NIL,
      options        => $_[8],
      oncommit       => $_[9],
      tablespacename => $_[10],
      if_not_exists  => FALSE,
   );
}
### CreateStmt_4: CREATE OptTemp TABLE IF NOT EXISTS qualified_name OF any_name OptTypedTableElementList OptWith OnCommitOption OptTableSpace
sub got_CreateStmt_4 {
   my $ofTypename = $_[0]->makeTypeNameFromNameList($_[9]);
   $ofTypename->_set_location( $_[0]->YYLLoc($_[9], 9) );

   $_[7]->relpersistence($_[2]);
   return SQL::Translator::Statement::Create->new(
      relation       => $_[7],
      tableElts      => $_[10],
      ofTypename     => $ofTypename,
      constraints    => NIL,
      options        => $_[11],
      oncommit       => $_[12],
      tablespacename => $_[13],
      if_not_exists  => TRUE,
   );
}

### OptTemp_1: TEMPORARY
sub got_OptTemp_1 { RELPERSISTENCE_TEMP      }
### OptTemp_2: TEMP
sub got_OptTemp_2 { RELPERSISTENCE_TEMP      }
### OptTemp_3: LOCAL TEMPORARY
sub got_OptTemp_3 { RELPERSISTENCE_TEMP      }
### OptTemp_4: LOCAL TEMP
sub got_OptTemp_4 { RELPERSISTENCE_TEMP      }
### OptTemp_5: GLOBAL TEMPORARY
sub got_OptTemp_5 { RELPERSISTENCE_TEMP      }
### OptTemp_6: GLOBAL TEMP
sub got_OptTemp_6 { RELPERSISTENCE_TEMP      }
### OptTemp_7: UNLOGGED
sub got_OptTemp_7 { RELPERSISTENCE_UNLOGGED  }

### OptTableElementList_1: TableElementList
sub got_OptTableElementList_1 { $_[1] }

### OptTypedTableElementList_1: <LPAREN> TypedTableElementList <RPAREN>
sub got_OptTypedTableElementList_1 { $_[2] }

### TableElementList_1: TableElement
sub got_TableElementList_1 { $_[0]->lappend($_[1]       ) }
### TableElementList_2: TableElementList <COMMA> TableElement
sub got_TableElementList_2 { $_[0]->lappend($_[1], $_[3]) }

### TypedTableElementList_1: TypedTableElement
sub got_TypedTableElementList_1 { $_[0]->lappend($_[1]       ) }
### TypedTableElementList_2: TypedTableElementList <COMMA> TypedTableElement
sub got_TypedTableElementList_2 { $_[0]->lappend($_[1], $_[3]) }

### TableElement_1: columnDef
sub got_TableElement_1 { $_[1] }
### TableElement_2: TableLikeClause
sub got_TableElement_2 { $_[1] }
### TableElement_3: TableConstraint
sub got_TableElement_3 { $_[1] }

### TypedTableElement_1: columnOptions
sub got_TypedTableElement_1 { $_[1] }
### TypedTableElement_2: TableConstraint
sub got_TypedTableElement_2 { $_[1] }

### columnDef: ColId Typename create_generic_options ColQualList
sub got_columnDef {
   my $n = SQL::Translator::Statement::Column::Definition->new(
      colname        => $_[1],
      typeName       => $_[2],
      fdwoptions     => $_[3],
      inhcount       => 0,
      is_local       => TRUE,
      is_not_null    => FALSE,
      is_from_type   => FALSE,
      storage        => 0,
      raw_default    => NULL,
      cooked_default => NULL,
      collOid        => InvalidOid,
   );
   $_[0]->SplitColQualList($_[4], $n);
   return $n;
}

### columnOptions: ColId WITH OPTIONS ColQualList
sub got_columnOptions {
   my $n = SQL::Translator::Statement::Column::Definition->new(
      colname        => $_[1],
      typeName       => NULL,
      inhcount       => 0,
      is_local       => TRUE,
      is_not_null    => FALSE,
      is_from_type   => FALSE,
      storage        => 0,
      raw_default    => NULL,
      cooked_default => NULL,
      collOid        => InvalidOid,
   );
   $_[0]->SplitColQualList($_[4], $n);
   return $n;
}

### ColQualList_1: ColQualList ColConstraint
sub got_ColQualList_1 { $_[0]->lappend($_[1], $_[2]) }

### ColConstraint_1: CONSTRAINT name ColConstraintElem
sub got_ColConstraint_1 {
   $_[3]->conname($_[2]);
   $_[3]->_set_location($_[0]->YYLLoc($_[1], 1));
   return $_[3];
}
### ColConstraint_2: ColConstraintElem
sub got_ColConstraint_2 { $_[1] }
# Note: the CollateClause is momentarily included in
# the list built by ColQualList, but we split it out
# again in SplitColQualList.
### ColConstraint_3: ConstraintAttr
sub got_ColConstraint_3 { $_[1] }
### ColConstraint_4: COLLATE any_name
sub got_ColConstraint_4 {
   return SQL::Translator::Statement::CollateClause->new(
      arg      => NULL,
      collname => $_[2],
      location => $_[0]->YYLLoc($_[1], 1),
   );
}

### ColConstraintElem_1: NOT NULL
sub got_ColConstraintElem_1 {
   return SQL::Translator::Statement::Constraint->new(
      contype         => CONSTR_NOTNULL,
      location        => $_[0]->YYLLoc($_[1], 1),
   );
}
### ColConstraintElem_2: NULL
sub got_ColConstraintElem_2 {
   return SQL::Translator::Statement::Constraint->new(
      contype         => CONSTR_NULL,
      location        => $_[0]->YYLLoc($_[1], 1),
   );
}
### ColConstraintElem_3: UNIQUE opt_definition OptConsTableSpace
sub got_ColConstraintElem_3 {
   return SQL::Translator::Statement::Constraint->new(
      contype         => CONSTR_UNIQUE,
      location        => $_[0]->YYLLoc($_[1], 1),
      keys            => NULL,
      options         => $_[2],
      indexname       => NULL,
      indexspace      => $_[3],
   );
}
### ColConstraintElem_4: PRIMARY KEY opt_definition OptConsTableSpace
sub got_ColConstraintElem_4 {
   return SQL::Translator::Statement::Constraint->new(
      contype         => CONSTR_PRIMARY,
      location        => $_[0]->YYLLoc($_[1], 1),
      keys            => NULL,
      options         => $_[3],
      indexname       => NULL,
      indexspace      => $_[4],
   );
}
### ColConstraintElem_5: CHECK <LPAREN> a_expr <RPAREN> opt_no_inherit
sub got_ColConstraintElem_5 {
   return SQL::Translator::Statement::Constraint->new(
      contype         => CONSTR_CHECK,
      location        => $_[0]->YYLLoc($_[1], 1),
      is_no_inherit   => $_[5],
      raw_expr        => $_[3],
      cooked_expr     => NULL,
   );
}
### ColConstraintElem_6: DEFAULT b_expr
sub got_ColConstraintElem_6 {
   return SQL::Translator::Statement::Constraint->new(
      contype         => CONSTR_DEFAULT,
      location        => $_[0]->YYLLoc($_[1], 1),
      raw_expr        => $_[2],
      cooked_expr     => NULL,
   );
}
### ColConstraintElem_7: REFERENCES qualified_name opt_column_list key_match key_actions
sub got_ColConstraintElem_7 {
   return SQL::Translator::Statement::Constraint->new(
      contype         => CONSTR_FOREIGN,
      location        => $_[0]->YYLLoc($_[1], 1),
      pktable         => $_[2],
      fk_attrs        => NIL,
      pk_attrs        => $_[3],
      fk_matchtype    => $_[4],
      fk_upd_action   => ($_[5] >> 8),
      fk_del_action   => ($_[5] & 0xFF),
      skip_validation => FALSE,
      initially_valid => TRUE,
   );
}

### ConstraintAttr_1: DEFERRABLE
sub got_ConstraintAttr_1 {
   return SQL::Translator::Statement::Constraint->new(
      contype  => CONSTR_ATTR_DEFERRABLE,
      location => $_[0]->YYLLoc($_[1], 1),
   );
}
### ConstraintAttr_2: NOT DEFERRABLE
sub got_ConstraintAttr_2 {
   return SQL::Translator::Statement::Constraint->new(
      contype  => CONSTR_ATTR_NOT_DEFERRABLE,
      location => $_[0]->YYLLoc($_[1], 1),
   );
}
### ConstraintAttr_3: INITIALLY DEFERRED
sub got_ConstraintAttr_3 {
   return SQL::Translator::Statement::Constraint->new(
      contype  => CONSTR_ATTR_DEFERRED,
      location => $_[0]->YYLLoc($_[1], 1),
   );
}
### ConstraintAttr_4: INITIALLY IMMEDIATE
sub got_ConstraintAttr_4 {
   return SQL::Translator::Statement::Constraint->new(
      contype  => CONSTR_ATTR_IMMEDIATE,
      location => $_[0]->YYLLoc($_[1], 1),
   );
}

### TableLikeClause: LIKE qualified_name TableLikeOptionList
sub got_TableLikeClause {
   return SQL::Translator::Statement::TableLikeClause->new(
      relation => $_[2],
      options  => $_[3],
   );
}

### TableLikeOptionList_1: TableLikeOptionList INCLUDING TableLikeOption
sub got_TableLikeOptionList_1 { $_[1] |  $_[3] }
### TableLikeOptionList_2: TableLikeOptionList EXCLUDING TableLikeOption
sub got_TableLikeOptionList_2 { $_[1] & ~$_[3] }

### TableLikeOption_1: DEFAULTS
sub got_TableLikeOption_1 { CREATE_TABLE_LIKE_DEFAULTS    }
### TableLikeOption_2: CONSTRAINTS
sub got_TableLikeOption_2 { CREATE_TABLE_LIKE_CONSTRAINTS }
### TableLikeOption_3: INDEXES
sub got_TableLikeOption_3 { CREATE_TABLE_LIKE_INDEXES     }
### TableLikeOption_4: STORAGE
sub got_TableLikeOption_4 { CREATE_TABLE_LIKE_STORAGE     }
### TableLikeOption_5: COMMENTS
sub got_TableLikeOption_5 { CREATE_TABLE_LIKE_COMMENTS    }
### TableLikeOption_6: ALL
sub got_TableLikeOption_6 { CREATE_TABLE_LIKE_ALL         }

### TableConstraint_1: CONSTRAINT name ConstraintElem
sub got_TableConstraint_1 {
   $_[3]->conname($_[2]);
   $_[3]->_set_location($_[0]->YYLLoc($_[1], 1));
   return $_[3];
}
### TableConstraint_2: ConstraintElem
sub got_TableConstraint_2 { $_[1] }

### ConstraintElem_1: CHECK <LPAREN> a_expr <RPAREN> ConstraintAttributeSpec
sub got_ConstraintElem_1 {
   my $n = SQL::Translator::Statement::Constraint->new(
      contype       => CONSTR_CHECK,
      location      => $_[0]->YYLLoc($_[1], 1),
      raw_expr      => $_[3],
      cooked_expr   => NULL,
   );
   $_[0]->processCASbits($_[5], $_[0]->YYLLoc($_[5], 5), "CHECK", $n, 0,0,0,1);
   $n->initially_valid( !$n->skip_validation );
   return $n;
}
### ConstraintElem_2: UNIQUE <LPAREN> columnList <RPAREN> opt_definition OptConsTableSpace ConstraintAttributeSpec
sub got_ConstraintElem_2 {
   my $n = SQL::Translator::Statement::Constraint->new(
      contype       => CONSTR_UNIQUE,
      location      => $_[0]->YYLLoc($_[1], 1),
      keys          => $_[3],
      options       => $_[5],
      indexname     => NULL,
      indexspace    => $_[6],
   );
   $_[0]->processCASbits($_[7], $_[0]->YYLLoc($_[7], 7), "UNIQUE", $n, 1,1,0,0);
   return $n;
}
### ConstraintElem_3: UNIQUE ExistingIndex ConstraintAttributeSpec
sub got_ConstraintElem_3 {
   my $n = SQL::Translator::Statement::Constraint->new(
      contype       => CONSTR_UNIQUE,
      location      => $_[0]->YYLLoc($_[1], 1),
      keys          => NIL,
      options       => NIL,
      indexname     => $_[2],
      indexspace    => NULL,
   );
   $_[0]->processCASbits($_[3], $_[0]->YYLLoc($_[3], 3), "UNIQUE", $n, 1,1,0,0);
   return $n;
}
### ConstraintElem_4: PRIMARY KEY <LPAREN> columnList <RPAREN> opt_definition OptConsTableSpace ConstraintAttributeSpec
sub got_ConstraintElem_4 {
   my $n = SQL::Translator::Statement::Constraint->new(
      contype       => CONSTR_PRIMARY,
      location      => $_[0]->YYLLoc($_[1], 1),
      keys          => $_[4],
      options       => $_[6],
      indexname     => NULL,
      indexspace    => $_[7],
   );
   $_[0]->processCASbits($_[8], $_[0]->YYLLoc($_[8], 8), "PRIMARY KEY", $n, 1,1,0,0);
   return $n;
}
### ConstraintElem_5: PRIMARY KEY ExistingIndex ConstraintAttributeSpec
sub got_ConstraintElem_5 {
   my $n = SQL::Translator::Statement::Constraint->new(
      contype       => CONSTR_PRIMARY,
      location      => $_[0]->YYLLoc($_[1], 1),
      keys          => NIL,
      options       => NIL,
      indexname     => $_[3],
      indexspace    => NULL,
   );
   $_[0]->processCASbits($_[4], $_[0]->YYLLoc($_[4], 4), "PRIMARY KEY", $n, 1,1,0,0);
   return $n;
}
### ConstraintElem_6: EXCLUDE access_method_clause <LPAREN> ExclusionConstraintList <RPAREN> opt_definition OptConsTableSpace ExclusionWhereClause ConstraintAttributeSpec
sub got_ConstraintElem_6 {
   my $n = SQL::Translator::Statement::Constraint->new(
      contype       => CONSTR_EXCLUSION,
      location      => $_[0]->YYLLoc($_[1], 1),
      access_method => $_[2],
      exclusions    => $_[4],
      options       => $_[6],
      indexname     => NULL,
      indexspace    => $_[7],
      where_clause  => $_[8],
   );
   $_[0]->processCASbits($_[9], $_[0]->YYLLoc($_[9], 9), "EXCLUDE", $n, 1,1,0,0);
   return $n;
}
### ConstraintElem_7: FOREIGN KEY <LPAREN> columnList <RPAREN> REFERENCES qualified_name opt_column_list key_match key_actions ConstraintAttributeSpec
sub got_ConstraintElem_7 {
   my $n = SQL::Translator::Statement::Constraint->new(
      contype       => CONSTR_FOREIGN,
      location      => $_[0]->YYLLoc($_[1], 1),
      pktable       => $_[7],
      fk_attrs      => $_[4],
      pk_attrs      => $_[8],
      fk_matchtype  => $_[9],
      fk_upd_action => ($_[10] >> 8),
      fk_del_action => ($_[10] & 0xFF),
   );
   $_[0]->processCASbits($_[11], $_[0]->YYLLoc($_[1], 1), "FOREIGN KEY", $n, 1,1,1,0);
   $n->initially_valid( !$n->skip_validation );
   return $n;
}

### opt_no_inherit_1: NO INHERIT
sub got_opt_no_inherit_1 { TRUE  }

### opt_column_list_1: <LPAREN> columnList <RPAREN>
sub got_opt_column_list_1 { $_[2] }

### columnList_1: columnElem
sub got_columnList_1 { $_[0]->lappend($_[1])        }
### columnList_2: columnList <COMMA> columnElem
sub got_columnList_2 { $_[0]->lappend($_[1], $_[3]) }

### columnElem: ColId
sub got_columnElem { $_[1] }

### key_match_1: MATCH FULL
sub got_key_match_1 { FKCONSTR_MATCH_FULL        }
### key_match_2: MATCH PARTIAL
sub got_key_match_2 { FKCONSTR_MATCH_PARTIAL     }
### key_match_3: MATCH SIMPLE
sub got_key_match_3 { FKCONSTR_MATCH_SIMPLE      }

### ExclusionConstraintList_1: ExclusionConstraintElem
sub got_ExclusionConstraintList_1 { $_[0]->lappend($_[1])        }
### ExclusionConstraintList_2: ExclusionConstraintList <COMMA> ExclusionConstraintElem
sub got_ExclusionConstraintList_2 { $_[0]->lappend($_[1], $_[3]) }

# allow OPERATOR() decoration for the benefit of ruleutils.c
### ExclusionConstraintElem_1: index_elem WITH any_operator
sub got_ExclusionConstraintElem_1 { $_[0]->lappend($_[1], $_[3]) }
### ExclusionConstraintElem_2: index_elem WITH OPERATOR <LPAREN> any_operator <RPAREN>
sub got_ExclusionConstraintElem_2 { $_[0]->lappend($_[1], $_[5]) }

### ExclusionWhereClause_1: WHERE <LPAREN> a_expr <RPAREN>
sub got_ExclusionWhereClause_1 { $_[3] }

### key_actions_1: key_update
sub got_key_actions_1 { ($_[1] << 8) | (FKCONSTR_ACTION_NOACTION & 0xFF)                    }
### key_actions_2: key_delete
sub got_key_actions_2 { (FKCONSTR_ACTION_NOACTION << 8) | ($_[1] & 0xFF)                    }
### key_actions_3: key_update key_delete
sub got_key_actions_3 { ($_[1] << 8) | ($_[2] & 0xFF)                                       }
### key_actions_4: key_delete key_update
sub got_key_actions_4 { ($_[2] << 8) | ($_[1] & 0xFF)                                       }

### key_update: ON UPDATE key_action
sub got_key_update { $_[3] }

### key_delete: ON DELETE key_action
sub got_key_delete { $_[3] }

### key_action_1: NO ACTION
sub got_key_action_1 { FKCONSTR_ACTION_NOACTION   }
### key_action_2: RESTRICT
sub got_key_action_2 { FKCONSTR_ACTION_RESTRICT   }
### key_action_3: CASCADE
sub got_key_action_3 { FKCONSTR_ACTION_CASCADE    }
### key_action_4: SET NULL
sub got_key_action_4 { FKCONSTR_ACTION_SETNULL    }
### key_action_5: SET DEFAULT
sub got_key_action_5 { FKCONSTR_ACTION_SETDEFAULT }

### OptInherit_1: INHERITS <LPAREN> qualified_name_list <RPAREN>
sub got_OptInherit_1 { $_[3] }

### OptWith_1: WITH reloptions
sub got_OptWith_1 { $_[2] }
### OptWith_2: WITH OIDS
sub got_OptWith_2 { $_[0]->lappend(defWithOids(TRUE))  }
### OptWith_3: WITHOUT OIDS
sub got_OptWith_3 { $_[0]->lappend(defWithOids(FALSE)) }

### OnCommitOption_1: ON COMMIT DROP
sub got_OnCommitOption_1 { ONCOMMIT_DROP          }
### OnCommitOption_2: ON COMMIT DELETE ROWS
sub got_OnCommitOption_2 { ONCOMMIT_DELETE_ROWS   }
### OnCommitOption_3: ON COMMIT PRESERVE ROWS
sub got_OnCommitOption_3 { ONCOMMIT_PRESERVE_ROWS }

### OptTableSpace_1: TABLESPACE name
sub got_OptTableSpace_1 { $_[2] }

### OptConsTableSpace_1: USING INDEX TABLESPACE name
sub got_OptConsTableSpace_1 { $_[4] }

### ExistingIndex: USING INDEX index_name
sub got_ExistingIndex { $_[3] }

### CreateAsStmt: CREATE OptTemp TABLE create_as_target AS SelectStmt opt_with_data
sub got_CreateAsStmt {
   my $ctas = SQL::Translator::Statement::CreateTableAs->new(
      query          => $_[6],
      into           => $_[4],
      is_select_into => FALSE,
   );
   #* cram additional flags into the IntoClause
   $_[4]->rel->relpersistence($_[2]);
   $_[4]->skipData(!$_[7]);
   return $ctas;
}

### create_as_target: qualified_name opt_column_list OptWith OnCommitOption OptTableSpace
sub got_create_as_target {
   return SQL::Translator::Statement::IntoClause->new(
      rel            => $_[1],
      colNames       => $_[2],
      options        => $_[3],
      onCommit       => $_[4],
      tableSpaceName => $_[5],
      skipData       => FALSE,  #* might get changed later
   );
}

### opt_with_data_1: WITH DATA
sub got_opt_with_data_1 { TRUE  }
### opt_with_data_2: WITH NO DATA
sub got_opt_with_data_2 { FALSE }

### CreateSeqStmt: CREATE OptTemp SEQUENCE qualified_name OptSeqOptList
sub got_CreateSeqStmt {
   $_[4]->relpersistence($_[2]);
   return SQL::Translator::Statement::CreateSeq->new(
      sequence => $_[4],
      options  => $_[5],
      ownerId  => InvalidOid,
   );
}

### AlterSeqStmt: ALTER SEQUENCE qualified_name SeqOptList
sub got_AlterSeqStmt {
   return SQL::Translator::Statement::AlterSeq->new(
      sequence => $_[3],
      options  => $_[4],
   );
}

### OptSeqOptList_1: SeqOptList
sub got_OptSeqOptList_1 { $_[1] }

### SeqOptList_1: SeqOptElem
sub got_SeqOptList_1 { $_[0]->lappend($_[1]) }
### SeqOptList_2: SeqOptList SeqOptElem
sub got_SeqOptList_2 { $_[0]->lappend($_[1], $_[2]) }

### SeqOptElem_1 : CACHE NumericOnly
sub got_SeqOptElem_1  { $_[0]->makeDefElem("cache",     $_[2]) }
### SeqOptElem_2 : CYCLE
sub got_SeqOptElem_2  { $_[0]->makeDefElem("cycle",     TRUE)  }
### SeqOptElem_3 : NO CYCLE
sub got_SeqOptElem_3  { $_[0]->makeDefElem("cycle",     FALSE) }
### SeqOptElem_4 : INCREMENT opt_by NumericOnly
sub got_SeqOptElem_4  { $_[0]->makeDefElem("increment", $_[3]) }
### SeqOptElem_5 : MAXVALUE NumericOnly
sub got_SeqOptElem_5  { $_[0]->makeDefElem("maxvalue",  $_[2]) }
### SeqOptElem_6 : MINVALUE NumericOnly
sub got_SeqOptElem_6  { $_[0]->makeDefElem("minvalue",  $_[2]) }
### SeqOptElem_7 : NO MAXVALUE
sub got_SeqOptElem_7  { $_[0]->makeDefElem("maxvalue",  NULL)  }
### SeqOptElem_8 : NO MINVALUE
sub got_SeqOptElem_8  { $_[0]->makeDefElem("minvalue",  NULL)  }
### SeqOptElem_9 : OWNED BY any_name
sub got_SeqOptElem_9  { $_[0]->makeDefElem("owned_by",  $_[3]) }
### SeqOptElem_10: START opt_with NumericOnly
sub got_SeqOptElem_10 { $_[0]->makeDefElem("start",     $_[3]) }
### SeqOptElem_11: RESTART
sub got_SeqOptElem_11 { $_[0]->makeDefElem("restart",   NULL)  }
### SeqOptElem_12: RESTART opt_with NumericOnly
sub got_SeqOptElem_12 { $_[0]->makeDefElem("restart",   $_[3]) }

### opt_by_1: BY
sub got_opt_by_1 {}

### NumericOnly_1: FCONST
sub got_NumericOnly_1 { $_[1]+0 }
### NumericOnly_2: <DASH> FCONST
sub got_NumericOnly_2 { -$_[2]  }
### NumericOnly_3: SignedIconst
sub got_NumericOnly_3 { $_[1]+0 }

### NumericOnly_list_1: NumericOnly
sub got_NumericOnly_list_1 { $_[0]->lappend($_[1])        }
### NumericOnly_list_2: NumericOnly_list <COMMA> NumericOnly
sub got_NumericOnly_list_2 { $_[0]->lappend($_[1], $_[3]) }

### CreatePLangStmt_1: CREATE opt_or_replace opt_trusted opt_procedural LANGUAGE ColId_or_Sconst
sub got_CreatePLangStmt_1 {
   return SQL::Translator::Statement::CreatePLang->new(
      replace     => $_[2],
      plname      => $_[6],
      #* parameters are all to be supplied by system
      plhandler   => NIL,
      plinline    => NIL,
      plvalidator => NIL,
      pltrusted   => FALSE,
   );
}
### CreatePLangStmt_2: CREATE opt_or_replace opt_trusted opt_procedural LANGUAGE ColId_or_Sconst HANDLER handler_name opt_inline_handler opt_validator
sub got_CreatePLangStmt_2 {
   return SQL::Translator::Statement::CreatePLang->new(
      replace     => $_[2],
      plname      => $_[6],
      plhandler   => $_[8],
      plinline    => $_[9],
      plvalidator => $_[10],
      pltrusted   => $_[3],
   );
}

### opt_trusted_1: TRUSTED
sub got_opt_trusted_1 { TRUE  }

### handler_name_1: name
sub got_handler_name_1 { $_[0]->lappend($_[1])        }
### handler_name_2: name attrs
sub got_handler_name_2 { $_[0]->lcons  ($_[1], $_[2]) }

### opt_inline_handler_1: INLINE handler_name
sub got_opt_inline_handler_1 { $_[2] }

### validator_clause_1: VALIDATOR handler_name
sub got_validator_clause_1 { $_[2] }
### validator_clause_2: NO VALIDATOR
sub got_validator_clause_2 { NIL   }

### opt_validator_1: validator_clause
sub got_opt_validator_1 { $_[1] }

### DropPLangStmt_1: DROP opt_procedural LANGUAGE ColId_or_Sconst opt_drop_behavior
sub got_DropPLangStmt_1 {
   return SQL::Translator::Statement::Drop->new(
      removeType => OBJECT_LANGUAGE,
      objects    => [ [ $_[4] ] ],
      arguments  => NIL,
      behavior   => $_[5],
      missing_ok => FALSE,
      concurrent => FALSE,
   );
}
### DropPLangStmt_2: DROP opt_procedural LANGUAGE IF EXISTS ColId_or_Sconst opt_drop_behavior
sub got_DropPLangStmt_2 {
   return SQL::Translator::Statement::Drop->new(
      removeType => OBJECT_LANGUAGE,
      objects    => [ [ $_[6] ] ],
      arguments  => NIL,
      behavior   => $_[7],
      missing_ok => TRUE,
      concurrent => FALSE,
   );
}

### opt_procedural_1: PROCEDURAL
sub got_opt_procedural_1 {}

### CreateTableSpaceStmt: CREATE TABLESPACE name OptTableSpaceOwner LOCATION Sconst
sub got_CreateTableSpaceStmt {
   return SQL::Translator::Statement::CreateTableSpace->new(
      tablespacename => $_[3],
      owner          => $_[4],
      location       => $_[6],
   );
}

### OptTableSpaceOwner_1: OWNER name
sub got_OptTableSpaceOwner_1 { $_[2] }

### DropTableSpaceStmt_1: DROP TABLESPACE name
sub got_DropTableSpaceStmt_1 {
   return SQL::Translator::Statement::DropTableSpace->new(
      tablespacename => $_[3],
      missing_ok     => FALSE,
   );
}
### DropTableSpaceStmt_2: DROP TABLESPACE IF EXISTS name
sub got_DropTableSpaceStmt_2 {
   return SQL::Translator::Statement::DropTableSpace->new(
      tablespacename => $_[5],
      missing_ok     => TRUE,
   );
}

### CreateExtensionStmt_1: CREATE EXTENSION name opt_with create_extension_opt_list
sub got_CreateExtensionStmt_1 {
   return SQL::Translator::Statement::CreateExtension->new(
      extname       => $_[3],
      if_not_exists => FALSE,
      options       => $_[5],
   );
}
### CreateExtensionStmt_2: CREATE EXTENSION IF NOT EXISTS name opt_with create_extension_opt_list
sub got_CreateExtensionStmt_2 {
   return SQL::Translator::Statement::CreateExtension->new(
      extname       => $_[6],
      if_not_exists => TRUE,
      options       => $_[8],
   );
}

### create_extension_opt_list_1: create_extension_opt_list create_extension_opt_item
sub got_create_extension_opt_list_1 { $_[0]->lappend($_[1], $_[2]) }

### create_extension_opt_item_1: SCHEMA name
sub got_create_extension_opt_item_1 { $_[0]->makeDefElem("schema",      $_[2]) }
### create_extension_opt_item_2: VERSION ColId_or_Sconst
sub got_create_extension_opt_item_2 { $_[0]->makeDefElem("new_version", $_[2]) }
### create_extension_opt_item_3: FROM ColId_or_Sconst
sub got_create_extension_opt_item_3 { $_[0]->makeDefElem("old_version", $_[2]) }

### AlterExtensionStmt: ALTER EXTENSION name UPDATE alter_extension_opt_list
sub got_AlterExtensionStmt {
   return SQL::Translator::Statement::AlterExtension->new(
      extname => $_[3],
      options => $_[5],
   );
}

### alter_extension_opt_list_1: alter_extension_opt_list alter_extension_opt_item
sub got_alter_extension_opt_list_1 { $_[0]->lappend($_[1], $_[2]) }

### alter_extension_opt_item: TO ColId_or_Sconst
sub got_alter_extension_opt_item { $_[0]->makeDefElem("new_version", $_[2]) }

### AlterExtensionContentsStmt_1 : ALTER EXTENSION name add_drop AGGREGATE func_name aggr_args
sub got_AlterExtensionContentsStmt_1  {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_AGGREGATE,
      objname => $_[6],
      objargs => $_[7],
   );
}
### AlterExtensionContentsStmt_2 : ALTER EXTENSION name add_drop CAST <LPAREN> Typename AS Typename <RPAREN>
sub got_AlterExtensionContentsStmt_2  {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_CAST,
      objname => $_[0]->lappend($_[7]),
      objargs => $_[0]->lappend($_[9]),
   );
}
### AlterExtensionContentsStmt_3 : ALTER EXTENSION name add_drop COLLATION any_name
sub got_AlterExtensionContentsStmt_3  {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_COLLATION,
      objname => $_[6],
   );
}
### AlterExtensionContentsStmt_4 : ALTER EXTENSION name add_drop CONVERSION any_name
sub got_AlterExtensionContentsStmt_4  {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_CONVERSION,
      objname => $_[6],
   );
}
### AlterExtensionContentsStmt_5 : ALTER EXTENSION name add_drop DOMAIN any_name
sub got_AlterExtensionContentsStmt_5  {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_DOMAIN,
      objname => $_[6],
   );
}
### AlterExtensionContentsStmt_6 : ALTER EXTENSION name add_drop FUNCTION function_with_argtypes
sub got_AlterExtensionContentsStmt_6  {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_FUNCTION,
      objname => $_[6]->funcname,
      objargs => $_[6]->funcargs,
   );
}
### AlterExtensionContentsStmt_7 : ALTER EXTENSION name add_drop opt_procedural LANGUAGE name
sub got_AlterExtensionContentsStmt_7  {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_LANGUAGE,
      objname => $_[0]->lappend($_[7]),
   );
}
### AlterExtensionContentsStmt_8 : ALTER EXTENSION name add_drop OPERATOR any_operator oper_argtypes
sub got_AlterExtensionContentsStmt_8  {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_OPERATOR,
      objname => $_[6],
      objargs => $_[7],
   );
}
### AlterExtensionContentsStmt_9 : ALTER EXTENSION name add_drop OPERATOR CLASS any_name USING access_method
sub got_AlterExtensionContentsStmt_9  {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_OPCLASS,
      objname => $_[7],
      objargs => $_[0]->lappend($_[9]),
   );
}
### AlterExtensionContentsStmt_10: ALTER EXTENSION name add_drop OPERATOR FAMILY any_name USING access_method
sub got_AlterExtensionContentsStmt_10 {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_OPFAMILY,
      objname => $_[7],
      objargs => $_[0]->lappend($_[9]),
   );
}
### AlterExtensionContentsStmt_11: ALTER EXTENSION name add_drop SCHEMA name
sub got_AlterExtensionContentsStmt_11 {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_SCHEMA,
      objname => $_[0]->lappend($_[6]),
   );
}
### AlterExtensionContentsStmt_12: ALTER EXTENSION name add_drop TABLE any_name
sub got_AlterExtensionContentsStmt_12 {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_TABLE,
      objname => $_[6],
   );
}
### AlterExtensionContentsStmt_13: ALTER EXTENSION name add_drop TEXT SEARCH PARSER any_name
sub got_AlterExtensionContentsStmt_13 {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_TSPARSER,
      objname => $_[8],
   );
}
### AlterExtensionContentsStmt_14: ALTER EXTENSION name add_drop TEXT SEARCH DICTIONARY any_name
sub got_AlterExtensionContentsStmt_14 {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_TSDICTIONARY,
      objname => $_[8],
   );
}
### AlterExtensionContentsStmt_15: ALTER EXTENSION name add_drop TEXT SEARCH TEMPLATE any_name
sub got_AlterExtensionContentsStmt_15 {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_TSTEMPLATE,
      objname => $_[8],
   );
}
### AlterExtensionContentsStmt_16: ALTER EXTENSION name add_drop TEXT SEARCH CONFIGURATION any_name
sub got_AlterExtensionContentsStmt_16 {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_TSCONFIGURATION,
      objname => $_[8],
   );
}
### AlterExtensionContentsStmt_17: ALTER EXTENSION name add_drop SEQUENCE any_name
sub got_AlterExtensionContentsStmt_17 {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_SEQUENCE,
      objname => $_[6],
   );
}
### AlterExtensionContentsStmt_18: ALTER EXTENSION name add_drop VIEW any_name
sub got_AlterExtensionContentsStmt_18 {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_VIEW,
      objname => $_[6],
   );
}
### AlterExtensionContentsStmt_19: ALTER EXTENSION name add_drop FOREIGN TABLE any_name
sub got_AlterExtensionContentsStmt_19 {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_FOREIGN_TABLE,
      objname => $_[7],
   );
}
### AlterExtensionContentsStmt_20: ALTER EXTENSION name add_drop FOREIGN DATA WRAPPER name
sub got_AlterExtensionContentsStmt_20 {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_FDW,
      objname => $_[0]->lappend($_[8]),
   );
}
### AlterExtensionContentsStmt_21: ALTER EXTENSION name add_drop SERVER name
sub got_AlterExtensionContentsStmt_21 {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_FOREIGN_SERVER,
      objname => $_[0]->lappend($_[6]),
   );
}
### AlterExtensionContentsStmt_22: ALTER EXTENSION name add_drop TYPE any_name
sub got_AlterExtensionContentsStmt_22 {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_TYPE,
      objname => $_[6],
   );
}

### CreateFdwStmt: CREATE FOREIGN DATA WRAPPER name opt_fdw_options create_generic_options
sub got_CreateFdwStmt {
   return SQL::Translator::Statement::CreateFdw->new(
      fdwname      => $_[5],
      func_options => $_[6],
      options      => $_[7],
   );
}

### fdw_option_1: HANDLER handler_name
sub got_fdw_option_1 { $_[0]->makeDefElem("handler",   $_[2]) }
### fdw_option_2: NO HANDLER
sub got_fdw_option_2 { $_[0]->makeDefElem("handler",   NULL)  }
### fdw_option_3: VALIDATOR handler_name
sub got_fdw_option_3 { $_[0]->makeDefElem("validator", $_[2]) }
### fdw_option_4: NO VALIDATOR
sub got_fdw_option_4 { $_[0]->makeDefElem("validator", NULL)  }

### fdw_options_1: fdw_option
sub got_fdw_options_1 { $_[0]->lappend($_[1])        }
### fdw_options_2: fdw_options fdw_option
sub got_fdw_options_2 { $_[0]->lappend($_[1], $_[2]) }

### opt_fdw_options_1: fdw_options
sub got_opt_fdw_options_1 { $_[1] }

### DropFdwStmt_1: DROP FOREIGN DATA WRAPPER name opt_drop_behavior
sub got_DropFdwStmt_1 {
   return SQL::Translator::Statement::Drop->new(
      removeType => OBJECT_FDW,
      objects    => [ [ $_[5] ] ],
      arguments  => NIL,
      missing_ok => FALSE,
      behavior   => $_[6],
      concurrent => FALSE,
   );
}
### DropFdwStmt_2: DROP FOREIGN DATA WRAPPER IF EXISTS name opt_drop_behavior
sub got_DropFdwStmt_2 {
   return SQL::Translator::Statement::Drop->new(
      removeType => OBJECT_FDW,
      objects    => [ [ $_[7] ] ],
      arguments  => NIL,
      missing_ok => TRUE,
      behavior   => $_[8],
      concurrent => FALSE,
   );
}

### AlterFdwStmt_1: ALTER FOREIGN DATA WRAPPER name opt_fdw_options alter_generic_options
sub got_AlterFdwStmt_1 {
   return SQL::Translator::Statement::AlterFdw->new(
      fdwname      => $_[5],
      func_options => $_[6],
      options      => $_[7],
   );
}
### AlterFdwStmt_2: ALTER FOREIGN DATA WRAPPER name fdw_options
sub got_AlterFdwStmt_2 {
   return SQL::Translator::Statement::AlterFdw->new(
      fdwname      => $_[5],
      func_options => $_[6],
      options      => NIL,
   );
}

### create_generic_options_1: OPTIONS <LPAREN> generic_option_list <RPAREN>
sub got_create_generic_options_1 { $_[3] }

### generic_option_list_1: generic_option_elem
sub got_generic_option_list_1 { $_[0]->lappend($_[1]) }
### generic_option_list_2: generic_option_list <COMMA> generic_option_elem
sub got_generic_option_list_2 { $_[0]->lappend($_[1], $_[3]) }

### alter_generic_options: OPTIONS <LPAREN> alter_generic_option_list <RPAREN>
sub got_alter_generic_options { $_[3] }

### alter_generic_option_list_1: alter_generic_option_elem
sub got_alter_generic_option_list_1 { $_[0]->lappend($_[1]) }
### alter_generic_option_list_2: alter_generic_option_list <COMMA> alter_generic_option_elem
sub got_alter_generic_option_list_2 { $_[0]->lappend($_[1], $_[3]) }

### alter_generic_option_elem_1: generic_option_elem
sub got_alter_generic_option_elem_1 { $_[1] }
### alter_generic_option_elem_2: SET generic_option_elem
sub got_alter_generic_option_elem_2 { $_[2]->defaction(DEFELEM_SET); $_[2]; }
### alter_generic_option_elem_3: ADD generic_option_elem
sub got_alter_generic_option_elem_3 { $_[2]->defaction(DEFELEM_ADD); $_[2]; }
### alter_generic_option_elem_4: DROP generic_option_name
sub got_alter_generic_option_elem_4 { $_[0]->makeDefElemExtended(NULL, $_[2], NULL, DEFELEM_DROP) }

### generic_option_elem: generic_option_name generic_option_arg
sub got_generic_option_elem { $_[0]->makeDefElem($_[1], $_[2]) }

### generic_option_name: ColLabel
sub got_generic_option_name { $_[1] }

### generic_option_arg: Sconst
sub got_generic_option_arg { $_[1] }

### CreateForeignServerStmt: CREATE SERVER name opt_type opt_foreign_server_version FOREIGN DATA WRAPPER name create_generic_options
sub got_CreateForeignServerStmt {
   return SQL::Translator::Statement::CreateForeignServer->new(
      servername => $_[3],
      servertype => $_[4],
      version    => $_[5],
      fdwname    => $_[9],
      options    => $_[10],
   );
}

### opt_type_1: TYPE Sconst
sub got_opt_type_1 { $_[2] }

### foreign_server_version_1: VERSION Sconst
sub got_foreign_server_version_1 { $_[2] }
### foreign_server_version_2: VERSION NULL
sub got_foreign_server_version_2 { NULL  }

### opt_foreign_server_version_1: foreign_server_version
sub got_opt_foreign_server_version_1 { $_[1] }

### DropForeignServerStmt_1: DROP SERVER name opt_drop_behavior
sub got_DropForeignServerStmt_1 {
   return SQL::Translator::Statement::Drop->new(
      removeType => OBJECT_FOREIGN_SERVER,
      objects    => [ [ $_[3] ] ],
      arguments  => NIL,
      missing_ok => FALSE,
      behavior   => $_[4],
      concurrent => FALSE,
   );
}
### DropForeignServerStmt_2: DROP SERVER IF EXISTS name opt_drop_behavior
sub got_DropForeignServerStmt_2 {
   return SQL::Translator::Statement::Drop->new(
      removeType => OBJECT_FOREIGN_SERVER,
      objects    => [ [ $_[5] ] ],
      arguments  => NIL,
      missing_ok => TRUE,
      behavior   => $_[6],
      concurrent => FALSE,
   );
}

### AlterForeignServerStmt_1: ALTER SERVER name foreign_server_version alter_generic_options
sub got_AlterForeignServerStmt_1 {
   return SQL::Translator::Statement::AlterForeignServer->new(
      servername  => $_[3],
      version     => $_[4],
      options     => $_[5],
      has_version => TRUE,
   );
}
### AlterForeignServerStmt_2: ALTER SERVER name foreign_server_version
sub got_AlterForeignServerStmt_2 {
   return SQL::Translator::Statement::AlterForeignServer->new(
      servername  => $_[3],
      version     => $_[4],
      has_version => TRUE,
   );
}
### AlterForeignServerStmt_3: ALTER SERVER name alter_generic_options
sub got_AlterForeignServerStmt_3 {
   return SQL::Translator::Statement::AlterForeignServer->new(
      servername => $_[3],
      options    => $_[4],
   );
}

### CreateForeignTableStmt_1: CREATE FOREIGN TABLE qualified_name OptForeignTableElementList SERVER name create_generic_options
sub got_CreateForeignTableStmt_1 {
   $_[4]->relpersistence(RELPERSISTENCE_PERMANENT);
   return SQL::Translator::Statement::CreateForeignTable->new(
      base_relation      => $_[4],
      base_tableElts     => $_[5],
      base_inhRelations  => NIL,
      base_if_not_exists => FALSE,
      #* FDW-specific data
      servername         => $_[7],
      options            => $_[8],
   );
}
### CreateForeignTableStmt_2: CREATE FOREIGN TABLE IF NOT EXISTS qualified_name OptForeignTableElementList SERVER name create_generic_options
sub got_CreateForeignTableStmt_2 {
   $_[7]->relpersistence(RELPERSISTENCE_PERMANENT);
   return SQL::Translator::Statement::CreateForeignTable->new(
      base_relation      => $_[7],
      base_tableElts     => $_[8],
      base_inhRelations  => NIL,
      base_if_not_exists => TRUE,
      #* FDW-specific data
      servername         => $_[10],
      options            => $_[11],
   );
}

### OptForeignTableElementList_1: <LPAREN> ForeignTableElementList <RPAREN>
sub got_OptForeignTableElementList_1 { $_[2] }
### OptForeignTableElementList_2: <LPAREN> <RPAREN>
sub got_OptForeignTableElementList_2 { NIL   }

### ForeignTableElementList_1: ForeignTableElement
sub got_ForeignTableElementList_1 { $_[0]->lappend($_[1]) }
### ForeignTableElementList_2: ForeignTableElementList <COMMA> ForeignTableElement
sub got_ForeignTableElementList_2 { $_[0]->lappend($_[1], $_[3]) }

### ForeignTableElement: columnDef
sub got_ForeignTableElement { $_[1] }

### AlterForeignTableStmt: ALTER FOREIGN TABLE relation_expr alter_table_cmds
sub got_AlterForeignTableStmt {
   return SQL::Translator::Statement::AlterTable->new(
      relation => $_[4],
      cmds     => $_[5],
      relkind  => OBJECT_FOREIGN_TABLE,
   );
}

### CreateUserMappingStmt: CREATE USER MAPPING FOR auth_ident SERVER name create_generic_options
sub got_CreateUserMappingStmt {
   return SQL::Translator::Statement::CreateUserMapping->new(
      username   => $_[5],
      servername => $_[7],
      options    => $_[8],
   );
}

### auth_ident_1: CURRENT_USER
sub got_auth_ident_1 { "current_user" }
### auth_ident_2: USER
sub got_auth_ident_2 { "current_user" }
### auth_ident_3: RoleId
sub got_auth_ident_3 { ($_[1] eq "public") ? NULL : $_[1] }

### DropUserMappingStmt_1: DROP USER MAPPING FOR auth_ident SERVER name
sub got_DropUserMappingStmt_1 {
   return SQL::Translator::Statement::DropUserMapping->new(
      username   => $_[5],
      servername => $_[7],
      missing_ok => FALSE,
   );
}
### DropUserMappingStmt_2: DROP USER MAPPING IF EXISTS FOR auth_ident SERVER name
sub got_DropUserMappingStmt_2 {
   return SQL::Translator::Statement::DropUserMapping->new(
      username   => $_[7],
      servername => $_[9],
      missing_ok => TRUE,
   );
}

### AlterUserMappingStmt: ALTER USER MAPPING FOR auth_ident SERVER name alter_generic_options
sub got_AlterUserMappingStmt {
   return SQL::Translator::Statement::AlterUserMapping->new(
      username   => $_[5],
      servername => $_[7],
      options    => $_[8],
   );
}

### CreateTrigStmt_1: CREATE TRIGGER name TriggerActionTime TriggerEvents ON qualified_name TriggerForSpec TriggerWhen EXECUTE PROCEDURE func_name <LPAREN> TriggerFuncArgs <RPAREN>
sub got_CreateTrigStmt_1 {
   return SQL::Translator::Statement::CreateTrig->new(
      trigname      => $_[3],
      relation      => $_[7],
      funcname      => $_[12],
      args          => $_[14],
      row           => $_[8],
      timing        => $_[4],
      events        => int($_[5]->[0]),
      columns       => $_[5]->[1],
      whenClause    => $_[9],
      isconstraint  => FALSE,
      deferrable    => FALSE,
      initdeferred  => FALSE,
      constrrel     => NULL,
   );
}
### CreateTrigStmt_2: CREATE CONSTRAINT TRIGGER name AFTER TriggerEvents ON qualified_name OptConstrFromTable ConstraintAttributeSpec FOR EACH ROW TriggerWhen EXECUTE PROCEDURE func_name <LPAREN> TriggerFuncArgs <RPAREN>
sub got_CreateTrigStmt_2 {
   my $n = SQL::Translator::Statement::CreateTrig->new(
      trigname      => $_[4],
      relation      => $_[8],
      funcname      => $_[17],
      args          => $_[19],
      row           => TRUE,
      timing        => TRIGGER_TYPE_AFTER,
      events        => int($_[6]->[0]),
      columns       => $_[6]->[1],
      whenClause    => $_[14],
      isconstraint  => TRUE,
      constrrel     => $_[9],
   );
   $_[0]->processCASbits($_[10], $_[0]->YYLLoc($_[10], 10), "TRIGGER", $n, 1,1,0,0);
   return $n;
}

### TriggerActionTime_1: BEFORE
sub got_TriggerActionTime_1 { TRIGGER_TYPE_BEFORE }
### TriggerActionTime_2: AFTER
sub got_TriggerActionTime_2 { TRIGGER_TYPE_AFTER }
### TriggerActionTime_3: INSTEAD OF
sub got_TriggerActionTime_3 { TRIGGER_TYPE_INSTEAD }

### TriggerEvents_1: TriggerOneEvent
sub got_TriggerEvents_1 { $_[1] }
### TriggerEvents_2: TriggerEvents OR TriggerOneEvent
sub got_TriggerEvents_2 {
   my $events1 = $_[1]->[0];
   my $events2 = $_[3]->[0];
   my @columns1 = @{$_[1]->[1]};
   my @columns2 = @{$_[3]->[1]};

   $_[0]->YYError("duplicate trigger events specified")
      if ($events1 & $events2);
   #* concat'ing the columns lists loses information about
   #* which columns went with which event, but so long as
   #* only UPDATE carries columns and we disallow multiple
   #* UPDATE items, it doesn't matter.  Command execution
   #* should just ignore the columns for non-UPDATE events.
   return [ ($events1 | $events2), @columns1, @columns2 ];
}

### TriggerOneEvent_1: INSERT
sub got_TriggerOneEvent_1 { $_[0]->lappend(TRIGGER_TYPE_INSERT,   NIL)   }
### TriggerOneEvent_2: DELETE
sub got_TriggerOneEvent_2 { $_[0]->lappend(TRIGGER_TYPE_DELETE,   NIL)   }
### TriggerOneEvent_3: UPDATE
sub got_TriggerOneEvent_3 { $_[0]->lappend(TRIGGER_TYPE_UPDATE,   NIL)   }
### TriggerOneEvent_4: UPDATE OF columnList
sub got_TriggerOneEvent_4 { $_[0]->lappend(TRIGGER_TYPE_UPDATE,   $_[3]) }
### TriggerOneEvent_5: TRUNCATE
sub got_TriggerOneEvent_5 { $_[0]->lappend(TRIGGER_TYPE_TRUNCATE, NIL)   }

### TriggerForSpec_1: FOR TriggerForOptEach TriggerForType
sub got_TriggerForSpec_1 { $_[3] }

### TriggerForOptEach_1: EACH
sub got_TriggerForOptEach_1 {}

### TriggerForType_1: ROW
sub got_TriggerForType_1 { TRUE  }
### TriggerForType_2: STATEMENT
sub got_TriggerForType_2 { FALSE }

### TriggerWhen_1: WHEN <LPAREN> a_expr <RPAREN>
sub got_TriggerWhen_1 { $_[3] }

### TriggerFuncArgs_1: TriggerFuncArg
sub got_TriggerFuncArgs_1 { $_[0]->lappend($_[1])        }
### TriggerFuncArgs_2: TriggerFuncArgs <COMMA> TriggerFuncArg
sub got_TriggerFuncArgs_2 { $_[0]->lappend($_[1], $_[3]) }

### TriggerFuncArg_1: Iconst
sub got_TriggerFuncArg_1 { $_[1] }
### TriggerFuncArg_2: FCONST
sub got_TriggerFuncArg_2 { $_[1] }
### TriggerFuncArg_3: Sconst
sub got_TriggerFuncArg_3 { $_[1] }
### TriggerFuncArg_4: ColLabel
sub got_TriggerFuncArg_4 { $_[1] }

### OptConstrFromTable_1: FROM qualified_name
sub got_OptConstrFromTable_1 { $_[2] }

### ConstraintAttributeSpec_1: ConstraintAttributeSpec ConstraintAttributeElem
sub got_ConstraintAttributeSpec_1 {
   #* We must complain about conflicting options.
   #* We could, but choose not to, complain about redundant
   #* options (ie, where $_[2]'s bit is already set in $_[1]).
   my $newspec = $_[1] | $_[2];

   #* special message for this case
   (($newspec & (CAS_NOT_DEFERRABLE | CAS_INITIALLY_DEFERRED)) == (CAS_NOT_DEFERRABLE | CAS_INITIALLY_DEFERRED))
      and $_[0]->ereport(ERROR,
            ERRCODE_SYNTAX_ERROR,
             "Constraint declared INITIALLY DEFERRED must be DEFERRABLE",
             $_[0]->YYLLoc($_[2], 2));
   #* generic message for other conflicts
   (($newspec & (CAS_NOT_DEFERRABLE | CAS_DEFERRABLE)) == (CAS_NOT_DEFERRABLE | CAS_DEFERRABLE) ||
      ($newspec & (CAS_INITIALLY_IMMEDIATE | CAS_INITIALLY_DEFERRED)) == (CAS_INITIALLY_IMMEDIATE | CAS_INITIALLY_DEFERRED))
      and $_[0]->ereport(ERROR,
            ERRCODE_SYNTAX_ERROR,
             "Conflicting constraint properties",
             $_[0]->YYLLoc($_[2], 2));
   return $newspec;
}

### ConstraintAttributeElem_1: NOT DEFERRABLE
sub got_ConstraintAttributeElem_1 { CAS_NOT_DEFERRABLE      }
### ConstraintAttributeElem_2: DEFERRABLE
sub got_ConstraintAttributeElem_2 { CAS_DEFERRABLE          }
### ConstraintAttributeElem_3: INITIALLY IMMEDIATE
sub got_ConstraintAttributeElem_3 { CAS_INITIALLY_IMMEDIATE }
### ConstraintAttributeElem_4: INITIALLY DEFERRED
sub got_ConstraintAttributeElem_4 { CAS_INITIALLY_DEFERRED  }
### ConstraintAttributeElem_5: NOT VALID
sub got_ConstraintAttributeElem_5 { CAS_NOT_VALID           }
### ConstraintAttributeElem_6: NO INHERIT
sub got_ConstraintAttributeElem_6 { CAS_NO_INHERIT          }

### DropTrigStmt_1: DROP TRIGGER name ON qualified_name opt_drop_behavior
sub got_DropTrigStmt_1 {
   return SQL::Translator::Statement::Drop->new(
      removeType => OBJECT_TRIGGER,
      objects    => [ $_[0]->lappend($_[5], $_[3]) ],
      arguments  => NIL,
      missing_ok => FALSE,
      behavior   => $_[6],
      concurrent => FALSE,
   );
}
### DropTrigStmt_2: DROP TRIGGER IF EXISTS name ON qualified_name opt_drop_behavior
sub got_DropTrigStmt_2 {
   return SQL::Translator::Statement::Drop->new(
      removeType => OBJECT_TRIGGER,
      objects    => [ $_[0]->lappend($_[7], $_[5]) ],
      arguments  => NIL,
      missing_ok => TRUE,
      behavior   => $_[8],
      concurrent => FALSE,
   );
}

### CreateAssertStmt: CREATE ASSERTION name CHECK <LPAREN> a_expr <RPAREN> ConstraintAttributeSpec
sub got_CreateAssertStmt {
   my $n = SQL::Translator::Statement::CreateTrig->new(
      trigname     => $_[3],
      args         => [ $_[6] ],
      isconstraint => TRUE,
   );
   $_[0]->processCASbits($_[8], $_[0]->YYLLoc($_[8], 8), "ASSERTION", $n, 1,1,0,0);
   return $n;
}

### DropAssertStmt: DROP ASSERTION name opt_drop_behavior
sub got_DropAssertStmt {
   return SQL::Translator::Statement::Drop->new(
      objects    => [ [ $_[3] ] ],
      arguments  => NIL,
      behavior   => $_[4],
      removeType => OBJECT_TRIGGER,  #* XXX
   );
}

### DefineStmt_1 : CREATE AGGREGATE func_name aggr_args definition
sub got_DefineStmt_1  {
   return SQL::Translator::Statement::Define->new(
      kind       => OBJECT_AGGREGATE,
      oldstyle   => FALSE,
      defnames   => $_[3],
      args       => $_[4],
      definition => $_[5],
   );
}
# old-style (pre-8.2) syntax for CREATE AGGREGATE
### DefineStmt_2 : CREATE AGGREGATE func_name old_aggr_definition
sub got_DefineStmt_2  {
   return SQL::Translator::Statement::Define->new(
      kind       => OBJECT_AGGREGATE,
      oldstyle   => TRUE,
      defnames   => $_[3],
      args       => NIL,
      definition => $_[4],
   );
}
### DefineStmt_3 : CREATE OPERATOR any_operator definition
sub got_DefineStmt_3  {
   return SQL::Translator::Statement::Define->new(
      kind       => OBJECT_OPERATOR,
      oldstyle   => FALSE,
      defnames   => $_[3],
      args       => NIL,
      definition => $_[4],
   );
}
### DefineStmt_4 : CREATE TYPE any_name definition
sub got_DefineStmt_4  {
   return SQL::Translator::Statement::Define->new(
      kind       => OBJECT_TYPE,
      oldstyle   => FALSE,
      defnames   => $_[3],
      args       => NIL,
      definition => $_[4],
   );
}
# Shell type (identified by lack of definition)
### DefineStmt_5 : CREATE TYPE any_name
sub got_DefineStmt_5  {
   return SQL::Translator::Statement::Define->new(
      kind       => OBJECT_TYPE,
      oldstyle   => FALSE,
      defnames   => $_[3],
      args       => NIL,
      definition => NIL,
   );
}
### DefineStmt_6 : CREATE TYPE any_name AS <LPAREN> OptTableFuncElementList <RPAREN>
sub got_DefineStmt_6  {
   return SQL::Translator::Statement::CompositeType->new(
      #* can't use qualified_name, sigh
      typevar    => $_[0]->makeRangeVarFromAnyName($_[3], $_[0]->YYLLoc($_[3], 3)),
      coldeflist => $_[6],
   );
}
### DefineStmt_7 : CREATE TYPE any_name AS ENUM <LPAREN> opt_enum_val_list <RPAREN>
sub got_DefineStmt_7  {
   return SQL::Translator::Statement::CreateEnum->new(
      typeName => $_[3],
      vals     => $_[7],
   );
}
### DefineStmt_8 : CREATE TEXT SEARCH PARSER any_name definition
sub got_DefineStmt_8  {
   return SQL::Translator::Statement::Define->new(
      kind       => OBJECT_TSPARSER,
      args       => NIL,
      defnames   => $_[5],
      definition => $_[6],
   );
}
### DefineStmt_9 : CREATE TEXT SEARCH DICTIONARY any_name definition
sub got_DefineStmt_9  {
   return SQL::Translator::Statement::Define->new(
      kind       => OBJECT_TSDICTIONARY,
      args       => NIL,
      defnames   => $_[5],
      definition => $_[6],
   );
}
### DefineStmt_10: CREATE TEXT SEARCH TEMPLATE any_name definition
sub got_DefineStmt_10 {
   return SQL::Translator::Statement::Define->new(
      kind       => OBJECT_TSTEMPLATE,
      args       => NIL,
      defnames   => $_[5],
      definition => $_[6],
   );
}
### DefineStmt_11: CREATE TEXT SEARCH CONFIGURATION any_name definition
sub got_DefineStmt_11 {
   return SQL::Translator::Statement::Define->new(
      kind       => OBJECT_TSCONFIGURATION,
      args       => NIL,
      defnames   => $_[5],
      definition => $_[6],
   );
}
### DefineStmt_12: CREATE COLLATION any_name definition
sub got_DefineStmt_12 {
   return SQL::Translator::Statement::Define->new(
      kind       => OBJECT_COLLATION,
      args       => NIL,
      defnames   => $_[3],
      definition => $_[4],
   );
}
### DefineStmt_13: CREATE COLLATION any_name FROM any_name
sub got_DefineStmt_13 {
   return SQL::Translator::Statement::Define->new(
      kind       => OBJECT_COLLATION,
      args       => NIL,
      defnames   => $_[3],
      definition => $_[0]->lappend($_[0]->makeDefElem("from",  $_[5])),
   );
}

### definition: <LPAREN> def_list <RPAREN>
sub got_definition { $_[2] }

### def_list_1: def_elem
sub got_def_list_1 { $_[0]->lappend($_[1])        }
### def_list_2: def_list <COMMA> def_elem
sub got_def_list_2 { $_[0]->lappend($_[1], $_[3]) }

### def_elem_1: ColLabel <EQUAL> def_arg
sub got_def_elem_1 { $_[0]->makeDefElem($_[1],  $_[3]) }
### def_elem_2: ColLabel
sub got_def_elem_2 { $_[0]->makeDefElem($_[1], NULL)   }

### def_arg_1: func_type
sub got_def_arg_1 { $_[1] }
### def_arg_2: reserved_keyword
sub got_def_arg_2 { $_[1] }
### def_arg_3: qual_all_Op
sub got_def_arg_3 { $_[1] }
### def_arg_4: NumericOnly
sub got_def_arg_4 { $_[1] }
### def_arg_5: Sconst
sub got_def_arg_5 { $_[1] }

### aggr_args_1: <LPAREN> type_list <RPAREN>
sub got_aggr_args_1 { $_[2] }
### aggr_args_2: <LPAREN> <STAR> <RPAREN>
sub got_aggr_args_2 { NIL   }

### old_aggr_definition: <LPAREN> old_aggr_list <RPAREN>
sub got_old_aggr_definition { $_[2] }

### old_aggr_list_1: old_aggr_elem
sub got_old_aggr_list_1 { $_[0]->lappend($_[1])        }
### old_aggr_list_2: old_aggr_list <COMMA> old_aggr_elem
sub got_old_aggr_list_2 { $_[0]->lappend($_[1], $_[3]) }

### old_aggr_elem: IDENT <EQUAL> def_arg
sub got_old_aggr_elem { $_[0]->makeDefElem($_[1], $_[3]) }

### opt_enum_val_list_1: enum_val_list
sub got_opt_enum_val_list_1 { $_[1] }

### enum_val_list_1: Sconst
sub got_enum_val_list_1 { $_[0]->lappend($_[1])        }
### enum_val_list_2: enum_val_list <COMMA> Sconst
sub got_enum_val_list_2 { $_[0]->lappend($_[1], $_[3]) }

### AlterEnumStmt_1: ALTER TYPE any_name ADD VALUE Sconst
sub got_AlterEnumStmt_1 {
   return SQL::Translator::Statement::AlterEnum->new(
      typeName       => $_[3],
      newVal         => $_[6],
      newValNeighbor => NULL,
      newValIsAfter  => TRUE,
   );
}
### AlterEnumStmt_2: ALTER TYPE any_name ADD VALUE Sconst BEFORE Sconst
sub got_AlterEnumStmt_2 {
   return SQL::Translator::Statement::AlterEnum->new(
      typeName       => $_[3],
      newVal         => $_[6],
      newValNeighbor => $_[8],
      newValIsAfter  => FALSE,
   );
}
### AlterEnumStmt_3: ALTER TYPE any_name ADD VALUE Sconst AFTER Sconst
sub got_AlterEnumStmt_3 {
   return SQL::Translator::Statement::AlterEnum->new(
      typeName       => $_[3],
      newVal         => $_[6],
      newValNeighbor => $_[8],
      newValIsAfter  => TRUE,
   );
}

### CreateOpClassStmt: CREATE OPERATOR CLASS any_name opt_default FOR TYPE Typename USING access_method opt_opfamily AS opclass_item_list
sub got_CreateOpClassStmt {
   return SQL::Translator::Statement::CreateOpClass->new(
      opclassname  => $_[4],
      isDefault    => $_[5],
      datatype     => $_[8],
      amname       => $_[10],
      opfamilyname => $_[11],
      items        => $_[13],
   );
}

### opclass_item_list_1: opclass_item
sub got_opclass_item_list_1 { $_[0]->lappend($_[1])        }
### opclass_item_list_2: opclass_item_list <COMMA> opclass_item
sub got_opclass_item_list_2 { $_[0]->lappend($_[1], $_[3]) }

### opclass_item_1: OPERATOR Iconst any_operator opclass_purpose opt_recheck
sub got_opclass_item_1 {
   return SQL::Translator::Statement::CreateOpClass::Item->new(
      itemtype     => OPCLASS_ITEM_OPERATOR,
      name         => $_[3],
      args         => NIL,
      number       => $_[2],
      order_family => $_[4],
   );
}
### opclass_item_2: OPERATOR Iconst any_operator oper_argtypes opclass_purpose opt_recheck
sub got_opclass_item_2 {
   return SQL::Translator::Statement::CreateOpClass::Item->new(
      itemtype     => OPCLASS_ITEM_OPERATOR,
      name         => $_[3],
      args         => $_[4],
      number       => $_[2],
      order_family => $_[5],
   );
}
### opclass_item_3: FUNCTION Iconst func_name func_args
sub got_opclass_item_3 {
   return SQL::Translator::Statement::CreateOpClass::Item->new(
      itemtype     => OPCLASS_ITEM_FUNCTION,
      name         => $_[3],
      args         => $_[0]->extractArgTypes($_[4]),
      number       => $_[2],
   );
}
### opclass_item_4: FUNCTION Iconst <LPAREN> type_list <RPAREN> func_name func_args
sub got_opclass_item_4 {
   return SQL::Translator::Statement::CreateOpClass::Item->new(
      itemtype     => OPCLASS_ITEM_FUNCTION,
      name         => $_[6],
      args         => $_[0]->extractArgTypes($_[7]),
      number       => $_[2],
      class_args   => $_[4],
   );
}
### opclass_item_5: STORAGE Typename
sub got_opclass_item_5 {
   return SQL::Translator::Statement::CreateOpClass::Item->new(
      itemtype     => OPCLASS_ITEM_STORAGETYPE,
      storedtype   => $_[2],
   );
}

### opt_default_1: DEFAULT
sub got_opt_default_1 { TRUE  }

### opt_opfamily_1: FAMILY any_name
sub got_opt_opfamily_1 { $_[2] }

### opclass_purpose_1: FOR SEARCH
sub got_opclass_purpose_1 { NULL  }
### opclass_purpose_2: FOR ORDER BY any_name
sub got_opclass_purpose_2 { $_[4] }

### opt_recheck_1: RECHECK
sub got_opt_recheck_1 { TRUE  }

### CreateOpFamilyStmt: CREATE OPERATOR FAMILY any_name USING access_method
sub got_CreateOpFamilyStmt {
   return SQL::Translator::Statement::CreateOpFamily->new(
      opfamilyname => $_[4],
      amname       => $_[6],
   );
}

### AlterOpFamilyStmt_1: ALTER OPERATOR FAMILY any_name USING access_method ADD opclass_item_list
sub got_AlterOpFamilyStmt_1 {
   return SQL::Translator::Statement::AlterOpFamily->new(
      opfamilyname => $_[4],
      amname       => $_[6],
      isDrop       => FALSE,
      items        => $_[8],
   );
}
### AlterOpFamilyStmt_2: ALTER OPERATOR FAMILY any_name USING access_method DROP opclass_drop_list
sub got_AlterOpFamilyStmt_2 {
   return SQL::Translator::Statement::AlterOpFamily->new(
      opfamilyname => $_[4],
      amname       => $_[6],
      isDrop       => TRUE,
      items        => $_[8],
   );
}

### opclass_drop_list_1: opclass_drop
sub got_opclass_drop_list_1 { $_[0]->lappend($_[1]) }
### opclass_drop_list_2: opclass_drop_list <COMMA> opclass_drop
sub got_opclass_drop_list_2 { $_[0]->lappend($_[1], $_[3]) }

### opclass_drop_1: OPERATOR Iconst <LPAREN> type_list <RPAREN>
sub got_opclass_drop_1 {
   return SQL::Translator::Statement::CreateOpClass::Item->new(
      itemtype => OPCLASS_ITEM_OPERATOR,
      number   => $_[2],
      args     => $_[4],
   );
}
### opclass_drop_2: FUNCTION Iconst <LPAREN> type_list <RPAREN>
sub got_opclass_drop_2 {
   return SQL::Translator::Statement::CreateOpClass::Item->new(
      itemtype => OPCLASS_ITEM_FUNCTION,
      number   => $_[2],
      args     => $_[4],
   );
}

### DropOpClassStmt_1: DROP OPERATOR CLASS any_name USING access_method opt_drop_behavior
sub got_DropOpClassStmt_1 {
   return SQL::Translator::Statement::Drop->new(
      objects    => [ $_[4] ],
      arguments  => [ [ $_[6] ] ],
      removeType => OBJECT_OPCLASS,
      behavior   => $_[7],
      missing_ok => FALSE,
      concurrent => FALSE,
   );
}
### DropOpClassStmt_2: DROP OPERATOR CLASS IF EXISTS any_name USING access_method opt_drop_behavior
sub got_DropOpClassStmt_2 {
   return SQL::Translator::Statement::Drop->new(
      objects    => [ $_[6] ],
      arguments  => [ [ $_[8] ] ],
      removeType => OBJECT_OPCLASS,
      behavior   => $_[9],
      missing_ok => TRUE,
      concurrent => FALSE,
   );
}

### DropOpFamilyStmt_1: DROP OPERATOR FAMILY any_name USING access_method opt_drop_behavior
sub got_DropOpFamilyStmt_1 {
   return SQL::Translator::Statement::Drop->new(
      objects    => [ $_[4] ],
      arguments  => [ [ $_[6] ] ],
      removeType => OBJECT_OPFAMILY,
      behavior   => $_[7],
      missing_ok => FALSE,
      concurrent => FALSE,
   );
}
### DropOpFamilyStmt_2: DROP OPERATOR FAMILY IF EXISTS any_name USING access_method opt_drop_behavior
sub got_DropOpFamilyStmt_2 {
   return SQL::Translator::Statement::Drop->new(
      objects    => [ $_[6] ],
      arguments  => [ [ $_[8] ] ],
      removeType => OBJECT_OPFAMILY,
      behavior   => $_[9],
      missing_ok => TRUE,
      concurrent => FALSE,
   );
}

### DropOwnedStmt: DROP OWNED BY name_list opt_drop_behavior
sub got_DropOwnedStmt {
   return SQL::Translator::Statement::DropOwned->new(
      roles    => $_[4],
      behavior => $_[5],
   );
}

### ReassignOwnedStmt: REASSIGN OWNED BY name_list TO name
sub got_ReassignOwnedStmt {
   return SQL::Translator::Statement::ReassignOwned->new(
      roles    => $_[4],
      newrole  => $_[6],
   );
}

### DropStmt_1: DROP drop_type IF EXISTS any_name_list opt_drop_behavior
sub got_DropStmt_1 {
   return SQL::Translator::Statement::Drop->new(
      removeType => $_[2],
      missing_ok => TRUE,
      objects    => $_[5],
      arguments  => NIL,
      behavior   => $_[6],
      concurrent => FALSE,
   );
}
### DropStmt_2: DROP drop_type any_name_list opt_drop_behavior
sub got_DropStmt_2 {
   return SQL::Translator::Statement::Drop->new(
      removeType => $_[2],
      missing_ok => FALSE,
      objects    => $_[3],
      arguments  => NIL,
      behavior   => $_[4],
      concurrent => FALSE,
   );
}
### DropStmt_3: DROP INDEX CONCURRENTLY any_name_list opt_drop_behavior
sub got_DropStmt_3 {
   return SQL::Translator::Statement::Drop->new(
      removeType => OBJECT_INDEX,
      missing_ok => FALSE,
      objects    => $_[4],
      arguments  => NIL,
      behavior   => $_[5],
      concurrent => TRUE,
   );
}
### DropStmt_4: DROP INDEX CONCURRENTLY IF EXISTS any_name_list opt_drop_behavior
sub got_DropStmt_4 {
   return SQL::Translator::Statement::Drop->new(
      removeType => OBJECT_INDEX,
      missing_ok => TRUE,
      objects    => $_[6],
      arguments  => NIL,
      behavior   => $_[7],
      concurrent => TRUE,
   );
}

### drop_type_1 : TABLE
sub got_drop_type_1  { OBJECT_TABLE           }
### drop_type_2 : SEQUENCE
sub got_drop_type_2  { OBJECT_SEQUENCE        }
### drop_type_3 : VIEW
sub got_drop_type_3  { OBJECT_VIEW            }
### drop_type_4 : INDEX
sub got_drop_type_4  { OBJECT_INDEX           }
### drop_type_5 : FOREIGN TABLE
sub got_drop_type_5  { OBJECT_FOREIGN_TABLE   }
### drop_type_6 : EVENT_TRIGGER
sub got_drop_type_6  { OBJECT_EVENT_TRIGGER   }
### drop_type_7 : TYPE
sub got_drop_type_7  { OBJECT_TYPE            }
### drop_type_8 : DOMAIN
sub got_drop_type_8  { OBJECT_DOMAIN          }
### drop_type_9 : COLLATION
sub got_drop_type_9  { OBJECT_COLLATION       }
### drop_type_10: CONVERSION
sub got_drop_type_10 { OBJECT_CONVERSION      }
### drop_type_11: SCHEMA
sub got_drop_type_11 { OBJECT_SCHEMA          }
### drop_type_12: EXTENSION
sub got_drop_type_12 { OBJECT_EXTENSION       }
### drop_type_13: TEXT SEARCH PARSER
sub got_drop_type_13 { OBJECT_TSPARSER        }
### drop_type_14: TEXT SEARCH DICTIONARY
sub got_drop_type_14 { OBJECT_TSDICTIONARY    }
### drop_type_15: TEXT SEARCH TEMPLATE
sub got_drop_type_15 { OBJECT_TSTEMPLATE      }
### drop_type_16: TEXT SEARCH CONFIGURATION
sub got_drop_type_16 { OBJECT_TSCONFIGURATION }

### any_name_list_1: any_name
sub got_any_name_list_1 { $_[0]->lappend($_[1])        }
### any_name_list_2: any_name_list <COMMA> any_name
sub got_any_name_list_2 { $_[0]->lappend($_[1], $_[3]) }

### any_name_1: ColId
sub got_any_name_1 { $_[0]->lappend($_[1])      }
### any_name_2: ColId attrs
sub got_any_name_2 { $_[0]->lcons($_[1], $_[2]) }

### attrs_1: <DOT> attr_name
sub got_attrs_1 { $_[0]->lappend($_[2])        }
### attrs_2: attrs <DOT> attr_name
sub got_attrs_2 { $_[0]->lappend($_[1], $_[3]) }

### TruncateStmt: TRUNCATE opt_table relation_expr_list opt_restart_seqs opt_drop_behavior
sub got_TruncateStmt {
   return SQL::Translator::Statement::Truncate->new(
      relations    => $_[3],
      restart_seqs => $_[4],
      behavior     => $_[5],
   );
}

### opt_restart_seqs_1: CONTINUE IDENTITY
sub got_opt_restart_seqs_1 { FALSE }
### opt_restart_seqs_2: RESTART IDENTITY
sub got_opt_restart_seqs_2 { TRUE  }

### CommentStmt_1 : COMMENT ON comment_type any_name IS comment_text
sub got_CommentStmt_1  {
   return SQL::Translator::Statement::Comment->new(
      objtype => $_[3],
      objname => $_[4],
      objargs => NIL,
      comment => $_[6],
   );
}
### CommentStmt_2 : COMMENT ON AGGREGATE func_name aggr_args IS comment_text
sub got_CommentStmt_2  {
   return SQL::Translator::Statement::Comment->new(
      objtype => OBJECT_AGGREGATE,
      objname => $_[4],
      objargs => $_[5],
      comment => $_[7],
   );
}
### CommentStmt_3 : COMMENT ON FUNCTION func_name func_args IS comment_text
sub got_CommentStmt_3  {
   return SQL::Translator::Statement::Comment->new(
      objtype => OBJECT_FUNCTION,
      objname => $_[4],
      objargs => $_[0]->extractArgTypes($_[5]),
      comment => $_[7],
   );
}
### CommentStmt_4 : COMMENT ON OPERATOR any_operator oper_argtypes IS comment_text
sub got_CommentStmt_4  {
   return SQL::Translator::Statement::Comment->new(
      objtype => OBJECT_OPERATOR,
      objname => $_[4],
      objargs => $_[5],
      comment => $_[7],
   );
}
### CommentStmt_5 : COMMENT ON CONSTRAINT name ON any_name IS comment_text
sub got_CommentStmt_5  {
   return SQL::Translator::Statement::Comment->new(
      objtype => OBJECT_CONSTRAINT,
      objname => $_[0]->lappend($_[6], $_[4]),
      objargs => NIL,
      comment => $_[8],
   );
}
### CommentStmt_6 : COMMENT ON RULE name ON any_name IS comment_text
sub got_CommentStmt_6  {
   return SQL::Translator::Statement::Comment->new(
      objtype => OBJECT_RULE,
      objname => $_[0]->lappend($_[6], $_[4]),
      objargs => NIL,
      comment => $_[8],
   );
}
# Obsolete syntax supported for awhile for compatibility
### CommentStmt_7 : COMMENT ON RULE name IS comment_text
sub got_CommentStmt_7  {
   return SQL::Translator::Statement::Comment->new(
      objtype => OBJECT_RULE,
      objname => $_[0]->lappend($_[4]),
      objargs => NIL,
      comment => $_[6],
   );
}
### CommentStmt_8 : COMMENT ON TRIGGER name ON any_name IS comment_text
sub got_CommentStmt_8  {
   return SQL::Translator::Statement::Comment->new(
      objtype => OBJECT_TRIGGER,
      objname => $_[0]->lappend($_[6], $_[4]),
      objargs => NIL,
      comment => $_[8],
   );
}
### CommentStmt_9 : COMMENT ON OPERATOR CLASS any_name USING access_method IS comment_text
sub got_CommentStmt_9  {
   return SQL::Translator::Statement::Comment->new(
      objtype => OBJECT_OPCLASS,
      objname => $_[5],
      objargs => $_[0]->lappend($_[7]),
      comment => $_[9],
   );
}
### CommentStmt_10: COMMENT ON OPERATOR FAMILY any_name USING access_method IS comment_text
sub got_CommentStmt_10 {
   return SQL::Translator::Statement::Comment->new(
      objtype => OBJECT_OPFAMILY,
      objname => $_[5],
      objargs => $_[0]->lappend($_[7]),
      comment => $_[9],
   );
}
### CommentStmt_11: COMMENT ON LARGE OBJECT NumericOnly IS comment_text
sub got_CommentStmt_11 {
   return SQL::Translator::Statement::Comment->new(
      objtype => OBJECT_LARGEOBJECT,
      objname => $_[0]->lappend($_[5]),
      objargs => NIL,
      comment => $_[7],
   );
}
### CommentStmt_12: COMMENT ON CAST <LPAREN> Typename AS Typename <RPAREN> IS comment_text
sub got_CommentStmt_12 {
   return SQL::Translator::Statement::Comment->new(
      objtype => OBJECT_CAST,
      objname => $_[0]->lappend($_[5]),
      objargs => $_[0]->lappend($_[7]),
      comment => $_[10],
   );
}
### CommentStmt_13: COMMENT ON opt_procedural LANGUAGE any_name IS comment_text
sub got_CommentStmt_13 {
   return SQL::Translator::Statement::Comment->new(
      objtype => OBJECT_LANGUAGE,
      objname => $_[5],
      objargs => NIL,
      comment => $_[7],
   );
}
### CommentStmt_14: COMMENT ON TEXT SEARCH PARSER any_name IS comment_text
sub got_CommentStmt_14 {
   return SQL::Translator::Statement::Comment->new(
      objtype => OBJECT_TSPARSER,
      objname => $_[6],
      comment => $_[8],
   );
}
### CommentStmt_15: COMMENT ON TEXT SEARCH DICTIONARY any_name IS comment_text
sub got_CommentStmt_15 {
   return SQL::Translator::Statement::Comment->new(
      objtype => OBJECT_TSDICTIONARY,
      objname => $_[6],
      comment => $_[8],
   );
}
### CommentStmt_16: COMMENT ON TEXT SEARCH TEMPLATE any_name IS comment_text
sub got_CommentStmt_16 {
   return SQL::Translator::Statement::Comment->new(
      objtype => OBJECT_TSTEMPLATE,
      objname => $_[6],
      comment => $_[8],
   );
}
### CommentStmt_17: COMMENT ON TEXT SEARCH CONFIGURATION any_name IS comment_text
sub got_CommentStmt_17 {
   return SQL::Translator::Statement::Comment->new(
      objtype => OBJECT_TSCONFIGURATION,
      objname => $_[6],
      comment => $_[8],
   );
}

### comment_type_1 : COLUMN
sub got_comment_type_1  { OBJECT_COLUMN         }
### comment_type_2 : DATABASE
sub got_comment_type_2  { OBJECT_DATABASE       }
### comment_type_3 : SCHEMA
sub got_comment_type_3  { OBJECT_SCHEMA         }
### comment_type_4 : INDEX
sub got_comment_type_4  { OBJECT_INDEX          }
### comment_type_5 : SEQUENCE
sub got_comment_type_5  { OBJECT_SEQUENCE       }
### comment_type_6 : TABLE
sub got_comment_type_6  { OBJECT_TABLE          }
### comment_type_7 : DOMAIN
sub got_comment_type_7  { OBJECT_DOMAIN         }
### comment_type_8 : TYPE
sub got_comment_type_8  { OBJECT_TYPE           }
### comment_type_9 : VIEW
sub got_comment_type_9  { OBJECT_VIEW           }
### comment_type_10: COLLATION
sub got_comment_type_10 { OBJECT_COLLATION      }
### comment_type_11: CONVERSION
sub got_comment_type_11 { OBJECT_CONVERSION     }
### comment_type_12: TABLESPACE
sub got_comment_type_12 { OBJECT_TABLESPACE     }
### comment_type_13: EXTENSION
sub got_comment_type_13 { OBJECT_EXTENSION      }
### comment_type_14: ROLE
sub got_comment_type_14 { OBJECT_ROLE           }
### comment_type_15: FOREIGN TABLE
sub got_comment_type_15 { OBJECT_FOREIGN_TABLE  }
### comment_type_16: SERVER
sub got_comment_type_16 { OBJECT_FOREIGN_SERVER }
### comment_type_17: FOREIGN DATA WRAPPER
sub got_comment_type_17 { OBJECT_FDW            }
### comment_type_18: EVENT TRIGGER
sub got_comment_type_18 { OBJECT_EVENT_TRIGGER  }

### comment_text_1: Sconst
sub got_comment_text_1 { $_[1] }
### comment_text_2: NULL
sub got_comment_text_2 { NULL  }

### SecLabelStmt_1: SECURITY LABEL opt_provider ON security_label_type any_name IS security_label
sub got_SecLabelStmt_1 {
   return SQL::Translator::Statement::SecLabel->new(
      provider => $_[3],
      objtype  => $_[5],
      objname  => $_[6],
      objargs  => NIL,
      label    => $_[8],
   );
}
### SecLabelStmt_2: SECURITY LABEL opt_provider ON AGGREGATE func_name aggr_args IS security_label
sub got_SecLabelStmt_2 {
   return SQL::Translator::Statement::SecLabel->new(
      provider => $_[3],
      objtype  => OBJECT_AGGREGATE,
      objname  => $_[6],
      objargs  => $_[7],
      label    => $_[9],
   );
}
### SecLabelStmt_3: SECURITY LABEL opt_provider ON FUNCTION func_name func_args IS security_label
sub got_SecLabelStmt_3 {
   return SQL::Translator::Statement::SecLabel->new(
      provider => $_[3],
      objtype  => OBJECT_FUNCTION,
      objname  => $_[6],
      objargs  => $_[0]->extractArgTypes($_[7]),
      label    => $_[9],
   );
}
### SecLabelStmt_4: SECURITY LABEL opt_provider ON LARGE OBJECT NumericOnly IS security_label
sub got_SecLabelStmt_4 {
   return SQL::Translator::Statement::SecLabel->new(
      provider => $_[3],
      objtype  => OBJECT_LARGEOBJECT,
      objname  => $_[0]->lappend($_[7]),
      objargs  => NIL,
      label    => $_[9],
   );
}
### SecLabelStmt_5: SECURITY LABEL opt_provider ON opt_procedural LANGUAGE any_name IS security_label
sub got_SecLabelStmt_5 {
   return SQL::Translator::Statement::SecLabel->new(
      provider => $_[3],
      objtype  => OBJECT_LANGUAGE,
      objname  => $_[7],
      objargs  => NIL,
      label    => $_[9],
   );
}

### opt_provider_1: FOR ColId_or_Sconst
sub got_opt_provider_1 { $_[2] }

### security_label_type_1 : COLUMN
sub got_security_label_type_1  { OBJECT_COLUMN        }
### security_label_type_2 : DATABASE
sub got_security_label_type_2  { OBJECT_DATABASE      }
### security_label_type_3 : EVENT TRIGGER
sub got_security_label_type_3  { OBJECT_EVENT_TRIGGER }
### security_label_type_4 : SCHEMA
sub got_security_label_type_4  { OBJECT_SCHEMA        }
### security_label_type_5 : FOREIGN TABLE
sub got_security_label_type_5  { OBJECT_FOREIGN_TABLE }
### security_label_type_6 : SCHEMA
sub got_security_label_type_6  { OBJECT_SCHEMA        }
### security_label_type_7 : SEQUENCE
sub got_security_label_type_7  { OBJECT_SEQUENCE      }
### security_label_type_8 : TABLE
sub got_security_label_type_8  { OBJECT_TABLE         }
### security_label_type_9 : DOMAIN
sub got_security_label_type_9  { OBJECT_TYPE          }
### security_label_type_10: ROLE
sub got_security_label_type_10 { OBJECT_ROLE          }
### security_label_type_11: TABLESPACE
sub got_security_label_type_11 { OBJECT_TABLESPACE    }
### security_label_type_12: TYPE
sub got_security_label_type_12 { OBJECT_TYPE          }
### security_label_type_13: VIEW
sub got_security_label_type_13 { OBJECT_VIEW          }

### security_label_1: Sconst
sub got_security_label_1 { $_[1] }
### security_label_2: NULL
sub got_security_label_2 { NULL  }

### FetchStmt_1: FETCH fetch_args
sub got_FetchStmt_1 {
   $_[2]->ismove(FALSE);
   return $_[2];
}
### FetchStmt_2: MOVE fetch_args
sub got_FetchStmt_2 {
   $_[2]->ismove(TRUE);
   return $_[2];
}

### fetch_args_1 : cursor_name
sub got_fetch_args_1  {
   return SQL::Translator::Statement::Fetch->new(
      portalname => $_[1],
      direction  => FETCH_FORWARD,
      howMany    => 1,
   );
}
### fetch_args_2 : from_in cursor_name
sub got_fetch_args_2  {
   return SQL::Translator::Statement::Fetch->new(
      portalname => $_[2],
      direction  => FETCH_FORWARD,
      howMany    => 1,
   );
}
### fetch_args_3 : NEXT opt_from_in cursor_name
sub got_fetch_args_3  {
   return SQL::Translator::Statement::Fetch->new(
      portalname => $_[3],
      direction  => FETCH_FORWARD,
      howMany    => 1,
   );
}
### fetch_args_4 : PRIOR opt_from_in cursor_name
sub got_fetch_args_4  {
   return SQL::Translator::Statement::Fetch->new(
      portalname => $_[3],
      direction  => FETCH_BACKWARD,
      howMany    => 1,
   );
}
### fetch_args_5 : FIRST opt_from_in cursor_name
sub got_fetch_args_5  {
   return SQL::Translator::Statement::Fetch->new(
      portalname => $_[3],
      direction  => FETCH_ABSOLUTE,
      howMany    => 1,
   );
}
### fetch_args_6 : LAST opt_from_in cursor_name
sub got_fetch_args_6  {
   return SQL::Translator::Statement::Fetch->new(
      portalname => $_[3],
      direction  => FETCH_ABSOLUTE,
      howMany    => -1,
   );
}
### fetch_args_7 : ABSOLUTE SignedIconst opt_from_in cursor_name
sub got_fetch_args_7  {
   return SQL::Translator::Statement::Fetch->new(
      portalname => $_[4],
      direction  => FETCH_ABSOLUTE,
      howMany    => $_[2],
   );
}
### fetch_args_8 : RELATIVE SignedIconst opt_from_in cursor_name
sub got_fetch_args_8  {
   return SQL::Translator::Statement::Fetch->new(
      portalname => $_[4],
      direction  => FETCH_RELATIVE,
      howMany    => $_[2],
   );
}
### fetch_args_9 : SignedIconst opt_from_in cursor_name
sub got_fetch_args_9  {
   return SQL::Translator::Statement::Fetch->new(
      portalname => $_[3],
      direction  => FETCH_FORWARD,
      howMany    => $_[1],
   );
}
### fetch_args_10: ALL opt_from_in cursor_name
sub got_fetch_args_10 {
   return SQL::Translator::Statement::Fetch->new(
      portalname => $_[3],
      direction  => FETCH_FORWARD,
      howMany    => FETCH_ALL,
   );
}
### fetch_args_11: FORWARD opt_from_in cursor_name
sub got_fetch_args_11 {
   return SQL::Translator::Statement::Fetch->new(
      portalname => $_[3],
      direction  => FETCH_FORWARD,
      howMany    => 1,
   );
}
### fetch_args_12: FORWARD SignedIconst opt_from_in cursor_name
sub got_fetch_args_12 {
   return SQL::Translator::Statement::Fetch->new(
      portalname => $_[4],
      direction  => FETCH_FORWARD,
      howMany    => $_[2],
   );
}
### fetch_args_13: FORWARD ALL opt_from_in cursor_name
sub got_fetch_args_13 {
   return SQL::Translator::Statement::Fetch->new(
      portalname => $_[4],
      direction  => FETCH_FORWARD,
      howMany    => FETCH_ALL,
   );
}
### fetch_args_14: BACKWARD opt_from_in cursor_name
sub got_fetch_args_14 {
   return SQL::Translator::Statement::Fetch->new(
      portalname => $_[3],
      direction  => FETCH_BACKWARD,
      howMany    => 1,
   );
}
### fetch_args_15: BACKWARD SignedIconst opt_from_in cursor_name
sub got_fetch_args_15 {
   return SQL::Translator::Statement::Fetch->new(
      portalname => $_[4],
      direction  => FETCH_BACKWARD,
      howMany    => $_[2],
   );
}
### fetch_args_16: BACKWARD ALL opt_from_in cursor_name
sub got_fetch_args_16 {
   return SQL::Translator::Statement::Fetch->new(
      portalname => $_[4],
      direction  => FETCH_BACKWARD,
      howMany    => FETCH_ALL,
   );
}

### from_in_1: FROM
sub got_from_in_1 {}
### from_in_2: IN
sub got_from_in_2 {}

### opt_from_in_1: from_in
sub got_opt_from_in_1 {}

### GrantStmt: GRANT privileges ON privilege_target TO grantee_list opt_grant_grant_option
sub got_GrantStmt {
   return SQL::Translator::Statement::Grant->new(
      is_grant     => TRUE,
      privileges   => $_[2],
      targtype     => $_[4]->targtype,
      objtype      => $_[4]->objtype,
      objects      => $_[4]->objs,
      grantees     => $_[6],
      grant_option => $_[7],
   );
}

### RevokeStmt_1: REVOKE privileges ON privilege_target FROM grantee_list opt_drop_behavior
sub got_RevokeStmt_1 {
   return SQL::Translator::Statement::Grant->new(
      is_grant     => FALSE,
      grant_option => FALSE,
      privileges   => $_[2],
      targtype     => $_[4]->targtype,
      objtype      => $_[4]->objtype,
      objects      => $_[4]->objs,
      grantees     => $_[6],
      behavior     => $_[7],
   );
}
### RevokeStmt_2: REVOKE GRANT OPTION FOR privileges ON privilege_target FROM grantee_list opt_drop_behavior
sub got_RevokeStmt_2 {
   return SQL::Translator::Statement::Grant->new(
      is_grant     => FALSE,
      grant_option => TRUE,
      privileges   => $_[5],
      targtype     => $_[7]->targtype,
      objtype      => $_[7]->objtype,
      objects      => $_[7]->objs,
      grantees     => $_[9],
      behavior     => $_[10],
   );
}

### privileges_1: privilege_list
sub got_privileges_1 { $_[1] }
### privileges_2: ALL
sub got_privileges_2 { NULL }
### privileges_3: ALL PRIVILEGES
sub got_privileges_3 { NULL }
### privileges_4: ALL <LPAREN> columnList <RPAREN>
sub got_privileges_4 {
   return [ SQL::Translator::Statement::AccessPriv->new(
      priv_name => NULL,
      cols      => $_[3],
   ) ];
}
### privileges_5: ALL PRIVILEGES <LPAREN> columnList <RPAREN>
sub got_privileges_5 {
   return [ SQL::Translator::Statement::AccessPriv->new(
      priv_name => NULL,
      cols      => $_[4],
   ) ];
}

### privilege_list_1: privilege
sub got_privilege_list_1 { $_[0]->lappend($_[1]) }
### privilege_list_2: privilege_list <COMMA> privilege
sub got_privilege_list_2 { $_[0]->lappend($_[1], $_[3]) }

### privilege_1: SELECT opt_column_list
sub got_privilege_1 {
   return SQL::Translator::Statement::AccessPriv->new(
      priv_name => $_[1],
      cols      => $_[2],
   );
}
### privilege_2: REFERENCES opt_column_list
sub got_privilege_2 {
   return SQL::Translator::Statement::AccessPriv->new(
      priv_name => $_[1],
      cols      => $_[2],
   );
}
### privilege_3: CREATE opt_column_list
sub got_privilege_3 {
   return SQL::Translator::Statement::AccessPriv->new(
      priv_name => $_[1],
      cols      => $_[2],
   );
}
### privilege_4: ColId opt_column_list
sub got_privilege_4 {
   return SQL::Translator::Statement::AccessPriv->new(
      priv_name => $_[1],
      cols      => $_[2],
   );
}

### privilege_target_1 : qualified_name_list
sub got_privilege_target_1  {
   return SQL::Translator::Statement::PrivTarget->new(
      targtype => ACL_TARGET_OBJECT,
      objtype  => ACL_OBJECT_RELATION,
      objs     => $_[1],
   );
}
### privilege_target_2 : TABLE qualified_name_list
sub got_privilege_target_2  {
   return SQL::Translator::Statement::PrivTarget->new(
      targtype => ACL_TARGET_OBJECT,
      objtype  => ACL_OBJECT_RELATION,
      objs     => $_[2],
   );
}
### privilege_target_3 : SEQUENCE qualified_name_list
sub got_privilege_target_3  {
   return SQL::Translator::Statement::PrivTarget->new(
      targtype => ACL_TARGET_OBJECT,
      objtype  => ACL_OBJECT_SEQUENCE,
      objs     => $_[2],
   );
}
### privilege_target_4 : FOREIGN DATA WRAPPER name_list
sub got_privilege_target_4  {
   return SQL::Translator::Statement::PrivTarget->new(
      targtype => ACL_TARGET_OBJECT,
      objtype  => ACL_OBJECT_FDW,
      objs     => $_[4],
   );
}
### privilege_target_5 : FOREIGN SERVER name_list
sub got_privilege_target_5  {
   return SQL::Translator::Statement::PrivTarget->new(
      targtype => ACL_TARGET_OBJECT,
      objtype  => ACL_OBJECT_FOREIGN_SERVER,
      objs     => $_[3],
   );
}
### privilege_target_6 : FUNCTION function_with_argtypes_list
sub got_privilege_target_6  {
   return SQL::Translator::Statement::PrivTarget->new(
      targtype => ACL_TARGET_OBJECT,
      objtype  => ACL_OBJECT_FUNCTION,
      objs     => $_[2],
   );
}
### privilege_target_7 : DATABASE name_list
sub got_privilege_target_7  {
   return SQL::Translator::Statement::PrivTarget->new(
      targtype => ACL_TARGET_OBJECT,
      objtype  => ACL_OBJECT_DATABASE,
      objs     => $_[2],
   );
}
### privilege_target_8 : LANGUAGE name_list
sub got_privilege_target_8  {
   return SQL::Translator::Statement::PrivTarget->new(
      targtype => ACL_TARGET_OBJECT,
      objtype  => ACL_OBJECT_LANGUAGE,
      objs     => $_[2],
   );
}
### privilege_target_9 : LARGE OBJECT NumericOnly_list
sub got_privilege_target_9  {
   return SQL::Translator::Statement::PrivTarget->new(
      targtype => ACL_TARGET_OBJECT,
      objtype  => ACL_OBJECT_LARGEOBJECT,
      objs     => $_[3],
   );
}
### privilege_target_10: SCHEMA name_list
sub got_privilege_target_10 {
   return SQL::Translator::Statement::PrivTarget->new(
      targtype => ACL_TARGET_OBJECT,
      objtype  => ACL_OBJECT_NAMESPACE,
      objs     => $_[2],
   );
}
### privilege_target_11: TABLESPACE name_list
sub got_privilege_target_11 {
   return SQL::Translator::Statement::PrivTarget->new(
      targtype => ACL_TARGET_OBJECT,
      objtype  => ACL_OBJECT_TABLESPACE,
      objs     => $_[2],
   );
}
### privilege_target_12: ALL TABLES IN SCHEMA name_list
sub got_privilege_target_12 {
   return SQL::Translator::Statement::PrivTarget->new(
      targtype => ACL_TARGET_ALL_IN_SCHEMA,
      objtype  => ACL_OBJECT_RELATION,
      objs     => $_[5],
   );
}
### privilege_target_13: ALL SEQUENCES IN SCHEMA name_list
sub got_privilege_target_13 {
   return SQL::Translator::Statement::PrivTarget->new(
      targtype => ACL_TARGET_ALL_IN_SCHEMA,
      objtype  => ACL_OBJECT_SEQUENCE,
      objs     => $_[5],
   );
}
### privilege_target_14: ALL FUNCTIONS IN SCHEMA name_list
sub got_privilege_target_14 {
   return SQL::Translator::Statement::PrivTarget->new(
      targtype => ACL_TARGET_ALL_IN_SCHEMA,
      objtype  => ACL_OBJECT_FUNCTION,
      objs     => $_[5],
   );
}

### grantee_list_1: grantee
sub got_grantee_list_1 { $_[0]->lappend($_[1]) }
### grantee_list_2: grantee_list <COMMA> grantee
sub got_grantee_list_2 { $_[0]->lappend($_[1], $_[3]) }

### grantee_1: RoleId
sub got_grantee_1 {
   return SQL::Translator::Statement::PrivGrantee->new(
      #* This hack lets us avoid reserving PUBLIC as a keyword
      rolname => ($_[1] eq "public") ? NULL : $_[1],
   );
}
### grantee_2: GROUP RoleId
sub got_grantee_2 {
   return SQL::Translator::Statement::PrivGrantee->new(
      #* Treat GROUP PUBLIC as a synonym for PUBLIC
      rolname => ($_[2] eq "public") ? NULL : $_[2],
   );
}

### opt_grant_grant_option_1: WITH GRANT OPTION
sub got_opt_grant_grant_option_1 { TRUE  }

### function_with_argtypes_list_1: function_with_argtypes
sub got_function_with_argtypes_list_1 { $_[0]->lappend($_[1])        }
### function_with_argtypes_list_2: function_with_argtypes_list <COMMA> function_with_argtypes
sub got_function_with_argtypes_list_2 { $_[0]->lappend($_[1], $_[3]) }

### function_with_argtypes: func_name func_args
sub got_function_with_argtypes {
   return SQL::Translator::Statement::FuncWithArgs->new(
      funcname => $_[1],
      funcargs => $_[0]->extractArgTypes($_[2]),
   );
}

### GrantRoleStmt: GRANT privilege_list TO name_list opt_grant_admin_option opt_granted_by
sub got_GrantRoleStmt {
   return SQL::Translator::Statement::GrantRole->new(
      is_grant      => TRUE,
      granted_roles => $_[2],
      grantee_roles => $_[4],
      admin_opt     => $_[5],
      grantor       => $_[6],
   );
}

### RevokeRoleStmt_1: REVOKE privilege_list FROM name_list opt_granted_by opt_drop_behavior
sub got_RevokeRoleStmt_1 {
   return SQL::Translator::Statement::GrantRole->new(
      is_grant      => FALSE,
      admin_opt     => FALSE,
      granted_roles => $_[2],
      grantee_roles => $_[4],
      behavior      => $_[6],
   );
}
### RevokeRoleStmt_2: REVOKE ADMIN OPTION FOR privilege_list FROM name_list opt_granted_by opt_drop_behavior
sub got_RevokeRoleStmt_2 {
   return SQL::Translator::Statement::GrantRole->new(
      is_grant      => FALSE,
      admin_opt     => TRUE,
      granted_roles => $_[5],
      grantee_roles => $_[7],
      behavior      => $_[9],
   );
}

### opt_grant_admin_option_1: WITH ADMIN OPTION
sub got_opt_grant_admin_option_1 { TRUE  }

### opt_granted_by_1: GRANTED BY RoleId
sub got_opt_granted_by_1 { $_[3] }

### AlterDefaultPrivilegesStmt: ALTER DEFAULT PRIVILEGES DefACLOptionList DefACLAction
sub got_AlterDefaultPrivilegesStmt {
   return SQL::Translator::Statement::AlterDefaultPrivileges->new(
      options => $_[4],
      action  => $_[5],
   );
}

### DefACLOptionList_1: DefACLOptionList DefACLOption
sub got_DefACLOptionList_1 { $_[0]->lappend($_[1], $_[2]) }

### DefACLOption_1: IN SCHEMA name_list
sub got_DefACLOption_1 { $_[0]->makeDefElem("schemas", $_[3]) }
### DefACLOption_2: FOR ROLE name_list
sub got_DefACLOption_2 { $_[0]->makeDefElem("roles",   $_[3]) }
### DefACLOption_3: FOR USER name_list
sub got_DefACLOption_3 { $_[0]->makeDefElem("roles",   $_[3]) }

### DefACLAction_1: GRANT privileges ON defacl_privilege_target TO grantee_list opt_grant_grant_option
sub got_DefACLAction_1 {
   return SQL::Translator::Statement::Grant->new(
      is_grant     => TRUE,
      privileges   => $_[2],
      targtype     => 'TARGET_DEFAULTS',
      objtype      => $_[4],
      objects      => NIL,
      grantees     => $_[6],
      grant_option => $_[7],
   );
}
### DefACLAction_2: REVOKE privileges ON defacl_privilege_target FROM grantee_list opt_drop_behavior
sub got_DefACLAction_2 {
   return SQL::Translator::Statement::Grant->new(
      is_grant     => FALSE,
      grant_option => FALSE,
      privileges   => $_[2],
      targtype     => 'TARGET_DEFAULTS',
      objtype      => $_[4],
      objects      => NIL,
      grantees     => $_[6],
      behavior     => $_[7],
   );
}
### DefACLAction_3: REVOKE GRANT OPTION FOR privileges ON defacl_privilege_target FROM grantee_list opt_drop_behavior
sub got_DefACLAction_3 {
   return SQL::Translator::Statement::Grant->new(
      is_grant     => FALSE,
      grant_option => TRUE,
      privileges   => $_[5],
      targtype     => 'TARGET_DEFAULTS',
      objtype      => $_[7],
      objects      => NIL,
      grantees     => $_[9],
      behavior     => $_[10],
   );
}

### defacl_privilege_target_1: TABLES
sub got_defacl_privilege_target_1 { ACL_OBJECT_RELATION }
### defacl_privilege_target_2: FUNCTIONS
sub got_defacl_privilege_target_2 { ACL_OBJECT_FUNCTION }
### defacl_privilege_target_3: SEQUENCES
sub got_defacl_privilege_target_3 { ACL_OBJECT_SEQUENCE }
### defacl_privilege_target_4: TYPES
sub got_defacl_privilege_target_4 { ACL_OBJECT_TYPE     }

### IndexStmt: CREATE opt_unique INDEX opt_concurrently opt_index_name ON qualified_name access_method_clause <LPAREN> index_params <RPAREN> opt_reloptions OptTableSpace where_clause
sub got_IndexStmt {
   return SQL::Translator::Statement::Index->new(
      unique       => $_[2],
      concurrent   => $_[4],
      idxname      => $_[5],
      relation     => $_[7],
      accessMethod => $_[8],
      indexParams  => $_[10],
      options      => $_[12],
      tableSpace   => $_[13],
      whereClause  => $_[14],

      excludeOpNames => NIL,
      idxcomment     => NULL,
      indexOid       => InvalidOid,
      oldNode        => InvalidOid,
      primary        => FALSE,
      isconstraint   => FALSE,
      deferrable     => FALSE,
      initdeferred   => FALSE,
   );
}

### opt_unique_1: UNIQUE
sub got_opt_unique_1 { TRUE  }

### opt_concurrently_1: CONCURRENTLY
sub got_opt_concurrently_1 { TRUE  }

### opt_index_name_1: index_name
sub got_opt_index_name_1 { $_[1] }

### access_method_clause_1: USING access_method
sub got_access_method_clause_1 { $_[2] }

### index_params_1: index_elem
sub got_index_params_1 { $_[0]->lappend($_[1])        }
### index_params_2: index_params <COMMA> index_elem
sub got_index_params_2 { $_[0]->lappend($_[1], $_[3]) }

### index_elem_1: ColId opt_collate opt_class opt_asc_desc opt_nulls_order
sub got_index_elem_1 {
   return SQL::Translator::Statement::IndexElem->new(
      name           => $_[1],
      expr           => NULL,
      indexcolname   => NULL,
      collation      => $_[2],
      opclass        => $_[3],
      ordering       => $_[4],
      nulls_ordering => $_[5],
   );
}
### index_elem_2: func_expr opt_collate opt_class opt_asc_desc opt_nulls_order
sub got_index_elem_2 {
   return SQL::Translator::Statement::IndexElem->new(
      name           => NULL,
      expr           => $_[1],
      indexcolname   => NULL,
      collation      => $_[2],
      opclass        => $_[3],
      ordering       => $_[4],
      nulls_ordering => $_[5],
   );
}
### index_elem_3: <LPAREN> a_expr <RPAREN> opt_collate opt_class opt_asc_desc opt_nulls_order
sub got_index_elem_3 {
   return SQL::Translator::Statement::IndexElem->new(
      name           => NULL,
      expr           => $_[2],
      indexcolname   => NULL,
      collation      => $_[4],
      opclass        => $_[5],
      ordering       => $_[6],
      nulls_ordering => $_[7],
   );
}

### opt_collate_1: COLLATE any_name
sub got_opt_collate_1 { $_[2] }

### opt_class_1: any_name
sub got_opt_class_1 { $_[1] }
### opt_class_2: USING any_name
sub got_opt_class_2 { $_[2] }

### opt_asc_desc_1: ASC
sub got_opt_asc_desc_1 { SORTBY_ASC     }
### opt_asc_desc_2: DESC
sub got_opt_asc_desc_2 { SORTBY_DESC    }

### opt_nulls_order_1: NULLS_FIRST
sub got_opt_nulls_order_1 { SORTBY_NULLS_FIRST   }
### opt_nulls_order_2: NULLS_LAST
sub got_opt_nulls_order_2 { SORTBY_NULLS_LAST    }

### CreateFunctionStmt_1: CREATE opt_or_replace FUNCTION func_name func_args_with_defaults RETURNS func_return createfunc_opt_list opt_definition
sub got_CreateFunctionStmt_1 {
   return SQL::Translator::Statement::CreateFunction->new(
      replace    => $_[2],
      funcname   => $_[4],
      parameters => $_[5],
      returnType => $_[7],
      options    => $_[8],
      withClause => $_[9],
   );
}
### CreateFunctionStmt_2: CREATE opt_or_replace FUNCTION func_name func_args_with_defaults RETURNS TABLE <LPAREN> table_func_column_list <RPAREN> createfunc_opt_list opt_definition
sub got_CreateFunctionStmt_2 {
   my $n = SQL::Translator::Statement::CreateFunction->new(
      replace    => $_[2],
      funcname   => $_[4],
      parameters => $_[0]->mergeTableFuncParameters($_[5], $_[9]),
      returnType => $_[0]->TableFuncTypeName($_[9]),
      options    => $_[11],
      withClause => $_[12],
   );
   $n->returnType->_set_location( $_[0]->YYLLoc($_[7], 7) );
   return $n;
}
### CreateFunctionStmt_3: CREATE opt_or_replace FUNCTION func_name func_args_with_defaults createfunc_opt_list opt_definition
sub got_CreateFunctionStmt_3 {
   return SQL::Translator::Statement::CreateFunction->new(
      replace    => $_[2],
      funcname   => $_[4],
      parameters => $_[5],
      returnType => NULL,
      options    => $_[6],
      withClause => $_[7],
   );
}

### opt_or_replace_1: OR REPLACE
sub got_opt_or_replace_1 { TRUE  }

### func_args_1: <LPAREN> func_args_list <RPAREN>
sub got_func_args_1 { $_[2] }
### func_args_2: <LPAREN> <RPAREN>
sub got_func_args_2 { NULL  }

### func_args_list_1: func_arg
sub got_func_args_list_1 { $_[0]->lappend($_[1])        }
### func_args_list_2: func_args_list <COMMA> func_arg
sub got_func_args_list_2 { $_[0]->lappend($_[1], $_[3]) }

### func_args_with_defaults_1: <LPAREN> func_args_with_defaults_list <RPAREN>
sub got_func_args_with_defaults_1 { $_[2] }
### func_args_with_defaults_2: <LPAREN> <RPAREN>
sub got_func_args_with_defaults_2 { NULL  }

### func_args_with_defaults_list_1: func_arg_with_default
sub got_func_args_with_defaults_list_1 { $_[0]->lappend($_[1])        }
### func_args_with_defaults_list_2: func_args_with_defaults_list <COMMA> func_arg_with_default
sub got_func_args_with_defaults_list_2 { $_[0]->lappend($_[1], $_[3]) }

### func_arg_1: arg_class param_name func_type
sub got_func_arg_1 {
   return SQL::Translator::Statement::Function::Parameter->new(
      name    => $_[2],
      argType => $_[3],
      mode    => $_[1],
      defexpr => NULL,
   );
}
### func_arg_2: param_name arg_class func_type
sub got_func_arg_2 {
   return SQL::Translator::Statement::Function::Parameter->new(
      name    => $_[1],
      argType => $_[3],
      mode    => $_[2],
      defexpr => NULL,
   );
}
### func_arg_3: param_name func_type
sub got_func_arg_3 {
   return SQL::Translator::Statement::Function::Parameter->new(
      name    => $_[1],
      argType => $_[2],
      mode    => 'IN',
      defexpr => NULL,
   );
}
### func_arg_4: arg_class func_type
sub got_func_arg_4 {
   return SQL::Translator::Statement::Function::Parameter->new(
      name    => NULL,
      argType => $_[2],
      mode    => $_[1],
      defexpr => NULL,
   );
}
### func_arg_5: func_type
sub got_func_arg_5 {
   return SQL::Translator::Statement::Function::Parameter->new(
      name    => NULL,
      argType => $_[1],
      mode    => 'IN',
      defexpr => NULL,
   );
}

### arg_class_1: IN
sub got_arg_class_1 { FUNC_PARAM_IN       }
### arg_class_2: OUT
sub got_arg_class_2 { FUNC_PARAM_OUT      }
### arg_class_3: INOUT
sub got_arg_class_3 { FUNC_PARAM_INOUT    }
### arg_class_4: IN OUT
sub got_arg_class_4 { FUNC_PARAM_INOUT    }
### arg_class_5: VARIADIC
sub got_arg_class_5 { FUNC_PARAM_VARIADIC }

# We can catch over-specified results here if we want to,
# but for now better to silently swallow typmod, etc.
# - thomas 2000-03-22
### func_return: func_type
sub got_func_return { $_[1] }

### func_type_1: Typename
sub got_func_type_1 { $_[1] }
### func_type_2: type_function_name attrs <PERCENT> TYPE
sub got_func_type_2 {
   my $n = $_[0]->makeTypeNameFromNameList($_[0]->lcons($_[1], $_[2]));
   $n->pct_type(TRUE);
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}
### func_type_3: SETOF type_function_name attrs <PERCENT> TYPE
sub got_func_type_3 {
   my $n = $_[0]->makeTypeNameFromNameList($_[0]->lcons($_[2], $_[3]));
   $n->pct_type(TRUE);
   $n->setof   (TRUE);
   $n->_set_location( $_[0]->YYLLoc($_[2], 2) );
   return $n;
}

### func_arg_with_default_1: func_arg
sub got_func_arg_with_default_1 { $_[1] }
### func_arg_with_default_2: func_arg DEFAULT a_expr
sub got_func_arg_with_default_2 {
   $_[1]->defexpr($_[3]);
   return $_[1];
}
### func_arg_with_default_3: func_arg <EQUAL> a_expr
sub got_func_arg_with_default_3 {
   $_[1]->defexpr($_[3]);
   return $_[1];
}

### createfunc_opt_list_1: 
# Must be at least one to prevent conflict
    createfunc_opt_item
sub got_createfunc_opt_list_1 { $_[0]->lappend($_[1]) }
### createfunc_opt_list_2: createfunc_opt_list createfunc_opt_item
sub got_createfunc_opt_list_2 { $_[0]->lappend($_[1], $_[2]) }

### common_func_opt_item_1 : CALLED ON NULL INPUT
sub got_common_func_opt_item_1  { $_[0]->makeDefElem("strict",     FALSE      ) }
### common_func_opt_item_2 : RETURNS NULL ON NULL INPUT
sub got_common_func_opt_item_2  { $_[0]->makeDefElem("strict",     TRUE       ) }
### common_func_opt_item_3 : STRICT
sub got_common_func_opt_item_3  { $_[0]->makeDefElem("strict",     TRUE       ) }
### common_func_opt_item_4 : IMMUTABLE
sub got_common_func_opt_item_4  { $_[0]->makeDefElem("volatility", "immutable") }
### common_func_opt_item_5 : STABLE
sub got_common_func_opt_item_5  { $_[0]->makeDefElem("volatility", "stable"   ) }
### common_func_opt_item_6 : VOLATILE
sub got_common_func_opt_item_6  { $_[0]->makeDefElem("volatility", "volatile" ) }
### common_func_opt_item_7 : EXTERNAL SECURITY DEFINER
sub got_common_func_opt_item_7  { $_[0]->makeDefElem("security",   TRUE       ) }
### common_func_opt_item_8 : EXTERNAL SECURITY INVOKER
sub got_common_func_opt_item_8  { $_[0]->makeDefElem("security",   FALSE      ) }
### common_func_opt_item_9 : SECURITY DEFINER
sub got_common_func_opt_item_9  { $_[0]->makeDefElem("security",   TRUE       ) }
### common_func_opt_item_10: SECURITY INVOKER
sub got_common_func_opt_item_10 { $_[0]->makeDefElem("security",   FALSE      ) }
### common_func_opt_item_11: COST NumericOnly
sub got_common_func_opt_item_11 { $_[0]->makeDefElem("cost",       $_[2]      ) }
### common_func_opt_item_12: ROWS NumericOnly
sub got_common_func_opt_item_12 { $_[0]->makeDefElem("rows",       $_[2]      ) }
# we abuse the normal content of a DefElem here
### common_func_opt_item_13: FunctionSetResetClause
sub got_common_func_opt_item_13 { $_[0]->makeDefElem("set",        $_[1]      ) }

### createfunc_opt_item_1: AS func_as
sub got_createfunc_opt_item_1 { $_[0]->makeDefElem("as",       $_[2]) }
### createfunc_opt_item_2: LANGUAGE ColId_or_Sconst
sub got_createfunc_opt_item_2 { $_[0]->makeDefElem("language", $_[2]) }
### createfunc_opt_item_3: WINDOW
sub got_createfunc_opt_item_3 { $_[0]->makeDefElem("window",    TRUE) }
### createfunc_opt_item_4: common_func_opt_item
sub got_createfunc_opt_item_4 { $_[1] }

### func_as_1: Sconst
sub got_func_as_1 { $_[0]->lappend($_[1])        }
### func_as_2: Sconst <COMMA> Sconst
sub got_func_as_2 { $_[0]->lappend($_[1], $_[3]) }

### opt_definition_1: WITH definition
sub got_opt_definition_1 { $_[2] }

### table_func_column: param_name func_type
sub got_table_func_column {
   return SQL::Translator::Statement::Function::Parameter->new(
      name    => $_[1],
      argType => $_[2],
      mode    => FUNC_PARAM_TABLE,
      defexpr => NULL,
   );
}

### table_func_column_list_1: table_func_column
sub got_table_func_column_list_1 { $_[0]->lappend($_[1])        }
### table_func_column_list_2: table_func_column_list <COMMA> table_func_column
sub got_table_func_column_list_2 { $_[0]->lappend($_[1], $_[3]) }

### AlterFunctionStmt: ALTER FUNCTION function_with_argtypes alterfunc_opt_list opt_restrict
sub got_AlterFunctionStmt {
   return SQL::Translator::Statement::AlterFunction->new(
      func    => $_[3],
      actions => $_[4],
   );
}

### alterfunc_opt_list_1: 
# At least one option must be specified
    common_func_opt_item
sub got_alterfunc_opt_list_1 { $_[0]->lappend($_[1])        }
### alterfunc_opt_list_2: alterfunc_opt_list common_func_opt_item
sub got_alterfunc_opt_list_2 { $_[0]->lappend($_[1], $_[2]) }

### RemoveFuncStmt_1: DROP FUNCTION func_name func_args opt_drop_behavior
sub got_RemoveFuncStmt_1 {
   return SQL::Translator::Statement::Drop->new(
      removeType => OBJECT_FUNCTION,
      objects    => [ $_[3] ],
      arguments  => [ $_[0]->extractArgTypes($_[4]) ],
      behavior   => $_[5],
      missing_ok => FALSE,
      concurrent => FALSE,
   );
}
### RemoveFuncStmt_2: DROP FUNCTION IF EXISTS func_name func_args opt_drop_behavior
sub got_RemoveFuncStmt_2 {
   return SQL::Translator::Statement::Drop->new(
      removeType => OBJECT_FUNCTION,
      objects    => [ $_[5] ],
      arguments  => [ $_[0]->extractArgTypes($_[6]) ],
      behavior   => $_[7],
      missing_ok => TRUE,
      concurrent => FALSE,
   );
}

### RemoveAggrStmt_1: DROP AGGREGATE func_name aggr_args opt_drop_behavior
sub got_RemoveAggrStmt_1 {
   return SQL::Translator::Statement::Drop->new(
      removeType => OBJECT_AGGREGATE,
      objects    => [ $_[3] ],
      arguments  => [ $_[4] ],
      behavior   => $_[5],
      missing_ok => FALSE,
      concurrent => FALSE,
   );
}
### RemoveAggrStmt_2: DROP AGGREGATE IF EXISTS func_name aggr_args opt_drop_behavior
sub got_RemoveAggrStmt_2 {
   return SQL::Translator::Statement::Drop->new(
      removeType => OBJECT_AGGREGATE,
      objects    => [ $_[5] ],
      arguments  => [ $_[6] ],
      behavior   => $_[7],
      missing_ok => TRUE,
      concurrent => FALSE,
   );
}

### RemoveOperStmt_1: DROP OPERATOR any_operator oper_argtypes opt_drop_behavior
sub got_RemoveOperStmt_1 {
   return SQL::Translator::Statement::Drop->new(
      removeType => OBJECT_OPERATOR,
      objects    => [ $_[3] ],
      arguments  => [ $_[4] ],
      behavior   => $_[5],
      missing_ok => FALSE,
      concurrent => FALSE,
   );
}
### RemoveOperStmt_2: DROP OPERATOR IF EXISTS any_operator oper_argtypes opt_drop_behavior
sub got_RemoveOperStmt_2 {
   return SQL::Translator::Statement::Drop->new(
      removeType => OBJECT_OPERATOR,
      objects    => [ $_[5] ],
      arguments  => [ $_[6] ],
      behavior   => $_[7],
      missing_ok => TRUE,
      concurrent => FALSE,
   );
}

### oper_argtypes_1: <LPAREN> Typename <RPAREN>
sub got_oper_argtypes_1 {
   $_[0]->ereport(ERROR,
         ERRCODE_SYNTAX_ERROR,
         "missing argument",
         "Use NONE to denote the missing argument of a unary operator.",
         $_[0]->YYLLoc($_[3], 3));
}
### oper_argtypes_2: <LPAREN> Typename <COMMA> Typename <RPAREN>
sub got_oper_argtypes_2 { $_[0]->lappend($_[2], $_[4]) }
# left unary
### oper_argtypes_3: <LPAREN> NONE <COMMA> Typename <RPAREN>
sub got_oper_argtypes_3 { $_[0]->lappend(NULL,  $_[4]) }
# right unary
### oper_argtypes_4: <LPAREN> Typename <COMMA> NONE <RPAREN>
sub got_oper_argtypes_4 { $_[0]->lappend($_[2], NULL ) }

### any_operator_1: all_Op
sub got_any_operator_1 { $_[0]->lappend($_[1])      }
### any_operator_2: ColId <DOT> any_operator
sub got_any_operator_2 { $_[0]->lcons($_[1], $_[3]) }

### DoStmt: DO dostmt_opt_list
sub got_DoStmt {
   return SQL::Translator::Statement::Do->new(
      args => $_[2],
   );
}

### dostmt_opt_list_1: dostmt_opt_item
sub got_dostmt_opt_list_1 { $_[0]->lappend($_[1]) }
### dostmt_opt_list_2: dostmt_opt_list dostmt_opt_item
sub got_dostmt_opt_list_2 { $_[0]->lappend($_[1], $_[2]) }

### dostmt_opt_item_1: Sconst
sub got_dostmt_opt_item_1 { $_[0]->makeDefElem("as",       $_[1]) }
### dostmt_opt_item_2: LANGUAGE ColId_or_Sconst
sub got_dostmt_opt_item_2 { $_[0]->makeDefElem("language", $_[2]) }

### CreateCastStmt_1: CREATE CAST <LPAREN> Typename AS Typename <RPAREN> WITH FUNCTION function_with_argtypes cast_context
sub got_CreateCastStmt_1 {
   return SQL::Translator::Statement::CreateCast->new(
      sourcetype => $_[4],
      targettype => $_[6],
      func       => $_[10],
      context    => $_[11],
      inout      => FALSE,
   );
}
### CreateCastStmt_2: CREATE CAST <LPAREN> Typename AS Typename <RPAREN> WITHOUT FUNCTION cast_context
sub got_CreateCastStmt_2 {
   return SQL::Translator::Statement::CreateCast->new(
      sourcetype => $_[4],
      targettype => $_[6],
      func       => NULL,
      context    => $_[10],
      inout      => FALSE,
   );
}
### CreateCastStmt_3: CREATE CAST <LPAREN> Typename AS Typename <RPAREN> WITH INOUT cast_context
sub got_CreateCastStmt_3 {
   return SQL::Translator::Statement::CreateCast->new(
      sourcetype => $_[4],
      targettype => $_[6],
      func       => NULL,
      context    => $_[10],
      inout      => TRUE,
   );
}

### cast_context_1: AS IMPLICIT
sub got_cast_context_1 { COERCION_IMPLICIT   }
### cast_context_2: AS ASSIGNMENT
sub got_cast_context_2 { COERCION_ASSIGNMENT }

### DropCastStmt: DROP CAST opt_if_exists <LPAREN> Typename AS Typename <RPAREN> opt_drop_behavior
sub got_DropCastStmt {
   return SQL::Translator::Statement::Drop->new(
      removeType => OBJECT_CAST,
      objects    => [ [ $_[5] ] ],
      arguments  => [ [ $_[7] ] ],
      behavior   => $_[9],
      missing_ok => $_[3],
      concurrent => FALSE,
   );
}

### opt_if_exists_1: IF EXISTS
sub got_opt_if_exists_1 { TRUE  }

### ReindexStmt_1: REINDEX reindex_type qualified_name opt_force
sub got_ReindexStmt_1 {
   return SQL::Translator::Statement::Reindex->new(
      kind      => $_[2],
      relation  => $_[3],
      name      => NULL,
   );
}
### ReindexStmt_2: REINDEX SYSTEM name opt_force
sub got_ReindexStmt_2 {
   return SQL::Translator::Statement::Reindex->new(
      kind      => OBJECT_DATABASE,
      name      => $_[3],
      relation  => NULL,
      do_system => TRUE,
      do_user   => FALSE,
   );
}
### ReindexStmt_3: REINDEX DATABASE name opt_force
sub got_ReindexStmt_3 {
   return SQL::Translator::Statement::Reindex->new(
      kind      => OBJECT_DATABASE,
      name      => $_[3],
      relation  => NULL,
      do_system => TRUE,
      do_user   => TRUE,
   );
}

### reindex_type_1: INDEX
sub got_reindex_type_1 { 'INDEX' }
### reindex_type_2: TABLE
sub got_reindex_type_2 { 'TABLE' }

### opt_force_1: FORCE
sub got_opt_force_1 { TRUE  }

### RenameStmt_1 : ALTER AGGREGATE func_name aggr_args RENAME TO name
sub got_RenameStmt_1  {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_AGGREGATE,
      object              => $_[3],
      objarg              => $_[4],
      newname             => $_[7],
      missing_ok          => FALSE,
   );
}
### RenameStmt_2 : ALTER COLLATION any_name RENAME TO name
sub got_RenameStmt_2  {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_COLLATION,
      object              => $_[3],
      newname             => $_[6],
      missing_ok          => FALSE,
   );
}
### RenameStmt_3 : ALTER CONVERSION any_name RENAME TO name
sub got_RenameStmt_3  {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_CONVERSION,
      object              => $_[3],
      newname             => $_[6],
      missing_ok          => FALSE,
   );
}
### RenameStmt_4 : ALTER DATABASE database_name RENAME TO database_name
sub got_RenameStmt_4  {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_DATABASE,
      subname             => $_[3],
      newname             => $_[6],
      missing_ok          => FALSE,
   );
}
### RenameStmt_5 : ALTER FUNCTION function_with_argtypes RENAME TO name
sub got_RenameStmt_5  {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_FUNCTION,
      object              => $_[3]->funcname,
      objarg              => $_[3]->funcargs,
      newname             => $_[6],
      missing_ok          => FALSE,
   );
}
### RenameStmt_6 : ALTER GROUP RoleId RENAME TO RoleId
sub got_RenameStmt_6  {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_ROLE,
      subname             => $_[3],
      newname             => $_[6],
      missing_ok          => FALSE,
   );
}
### RenameStmt_7 : ALTER opt_procedural LANGUAGE name RENAME TO name
sub got_RenameStmt_7  {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_LANGUAGE,
      subname             => $_[4],
      newname             => $_[7],
      missing_ok          => FALSE,
   );
}
### RenameStmt_8 : ALTER OPERATOR CLASS any_name USING access_method RENAME TO name
sub got_RenameStmt_8  {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_OPCLASS,
      object              => $_[4],
      subname             => $_[6],
      newname             => $_[9],
      missing_ok          => FALSE,
   );
}
### RenameStmt_9 : ALTER OPERATOR FAMILY any_name USING access_method RENAME TO name
sub got_RenameStmt_9  {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_OPFAMILY,
      object              => $_[4],
      subname             => $_[6],
      newname             => $_[9],
      missing_ok          => FALSE,
   );
}
### RenameStmt_10: ALTER SCHEMA name RENAME TO name
sub got_RenameStmt_10 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_SCHEMA,
      subname             => $_[3],
      newname             => $_[6],
      missing_ok          => FALSE,
   );
}
### RenameStmt_11: ALTER TABLE relation_expr RENAME TO name
sub got_RenameStmt_11 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_TABLE,
      relation            => $_[3],
      subname             => NULL,
      newname             => $_[6],
      missing_ok          => FALSE,
   );
}
### RenameStmt_12: ALTER SEQUENCE qualified_name RENAME TO name
sub got_RenameStmt_12 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_SEQUENCE,
      relation            => $_[3],
      subname             => NULL,
      newname             => $_[6],
      missing_ok          => FALSE,
   );
}
### RenameStmt_13: ALTER VIEW qualified_name RENAME TO name
sub got_RenameStmt_13 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_VIEW,
      relation            => $_[3],
      subname             => NULL,
      newname             => $_[6],
      missing_ok          => FALSE,
   );
}
### RenameStmt_14: ALTER INDEX qualified_name RENAME TO name
sub got_RenameStmt_14 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_INDEX,
      relation            => $_[3],
      subname             => NULL,
      newname             => $_[6],
      missing_ok          => FALSE,
   );
}
### RenameStmt_15: ALTER FOREIGN TABLE relation_expr RENAME TO name
sub got_RenameStmt_15 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_FOREIGN_TABLE,
      relation            => $_[4],
      subname             => NULL,
      newname             => $_[7],
      missing_ok          => FALSE,
   );
}
### RenameStmt_16: ALTER TABLE relation_expr RENAME opt_column name TO name
sub got_RenameStmt_16 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_COLUMN,
      relationType        => OBJECT_TABLE,
      relation            => $_[3],
      subname             => $_[6],
      newname             => $_[8],
      missing_ok          => FALSE,
   );
}
### RenameStmt_17: ALTER FOREIGN TABLE relation_expr RENAME opt_column name TO name
sub got_RenameStmt_17 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_COLUMN,
      relationType        => OBJECT_FOREIGN_TABLE,
      relation            => $_[4],
      subname             => $_[7],
      newname             => $_[9],
      missing_ok          => FALSE,
   );
}
### RenameStmt_18: ALTER TRIGGER name ON qualified_name RENAME TO name
sub got_RenameStmt_18 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_TRIGGER,
      relation            => $_[5],
      subname             => $_[3],
      newname             => $_[8],
      missing_ok          => FALSE,
   );
}
### RenameStmt_19: ALTER ROLE RoleId RENAME TO RoleId
sub got_RenameStmt_19 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_ROLE,
      subname             => $_[3],
      newname             => $_[6],
      missing_ok          => FALSE,
   );
}
### RenameStmt_20: ALTER USER RoleId RENAME TO RoleId
sub got_RenameStmt_20 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_ROLE,
      subname             => $_[3],
      newname             => $_[6],
      missing_ok          => FALSE,
   );
}
### RenameStmt_21: ALTER TABLESPACE name RENAME TO name
sub got_RenameStmt_21 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_TABLESPACE,
      subname             => $_[3],
      newname             => $_[6],
      missing_ok          => FALSE,
   );
}
### RenameStmt_22: ALTER TABLESPACE name SET reloptions
sub got_RenameStmt_22 {
   return SQL::Translator::Statement::AlterTableSpaceOptions->new(
      tablespacename      => $_[3],
      options             => $_[5],
      isReset             => FALSE,
      missing_ok          => FALSE,
   );
}
### RenameStmt_23: ALTER TABLESPACE name RESET reloptions
sub got_RenameStmt_23 {
   return SQL::Translator::Statement::AlterTableSpaceOptions->new(
      tablespacename      => $_[3],
      options             => $_[5],
      isReset             => TRUE,
      missing_ok          => FALSE,
   );
}
### RenameStmt_24: ALTER TEXT SEARCH PARSER any_name RENAME TO name
sub got_RenameStmt_24 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_TSPARSER,
      object              => $_[5],
      newname             => $_[8],
      missing_ok          => FALSE,
   );
}
### RenameStmt_25: ALTER TEXT SEARCH DICTIONARY any_name RENAME TO name
sub got_RenameStmt_25 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_TSDICTIONARY,
      object              => $_[5],
      newname             => $_[8],
      missing_ok          => FALSE,
   );
}
### RenameStmt_26: ALTER TEXT SEARCH TEMPLATE any_name RENAME TO name
sub got_RenameStmt_26 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_TSTEMPLATE,
      object              => $_[5],
      newname             => $_[8],
      missing_ok          => FALSE,
   );
}
### RenameStmt_27: ALTER TEXT SEARCH CONFIGURATION any_name RENAME TO name
sub got_RenameStmt_27 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_TSCONFIGURATION,
      object              => $_[5],
      newname             => $_[8],
      missing_ok          => FALSE,
   );
}
### RenameStmt_28: ALTER TYPE any_name RENAME TO name
sub got_RenameStmt_28 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_TYPE,
      object              => $_[3],
      newname             => $_[6],
      missing_ok          => FALSE,
   );
}
### RenameStmt_29: ALTER TYPE any_name RENAME ATTRIBUTE name TO name opt_drop_behavior
sub got_RenameStmt_29 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_ATTRIBUTE,
      relationType        => OBJECT_TYPE,
      relation            => $_[0]->makeRangeVarFromAnyName($_[3], $_[0]->YYLLoc($_[3], 3)),
      subname             => $_[6],
      newname             => $_[8],
      behavior            => $_[9],
      missing_ok          => FALSE,
   );
}

#** Just noise... ##
### opt_column_1: COLUMN
sub got_opt_column_1 { OBJECT_COLUMN }

### opt_set_data_1: SET DATA
sub got_opt_set_data_1 { 1 }

### AlterObjectSchemaStmt_1 : ALTER AGGREGATE func_name aggr_args SET SCHEMA name
sub got_AlterObjectSchemaStmt_1  {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_AGGREGATE,
      object     => $_[3],
      objarg     => $_[4],
      newschema  => $_[7],
   );
}
### AlterObjectSchemaStmt_2 : ALTER COLLATION any_name SET SCHEMA name
sub got_AlterObjectSchemaStmt_2  {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_COLLATION,
      object     => $_[3],
      newschema  => $_[6],
   );
}
### AlterObjectSchemaStmt_3 : ALTER CONVERSION any_name SET SCHEMA name
sub got_AlterObjectSchemaStmt_3  {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_CONVERSION,
      object     => $_[3],
      newschema  => $_[6],
   );
}
### AlterObjectSchemaStmt_4 : ALTER DOMAIN any_name SET SCHEMA name
sub got_AlterObjectSchemaStmt_4  {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_DOMAIN,
      object     => $_[3],
      newschema  => $_[6],
   );
}
### AlterObjectSchemaStmt_5 : ALTER EXTENSION any_name SET SCHEMA name
sub got_AlterObjectSchemaStmt_5  {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_EXTENSION,
      object     => $_[3],
      newschema  => $_[6],
   );
}
### AlterObjectSchemaStmt_6 : ALTER FUNCTION function_with_argtypes SET SCHEMA name
sub got_AlterObjectSchemaStmt_6  {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_FUNCTION,
      object     => $_[3]->funcname,
      objarg     => $_[3]->funcargs,
      newschema  => $_[6],
   );
}
### AlterObjectSchemaStmt_7 : ALTER OPERATOR any_operator oper_argtypes SET SCHEMA name
sub got_AlterObjectSchemaStmt_7  {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_OPERATOR,
      object     => $_[3],
      objarg     => $_[4],
      newschema  => $_[7],
   );
}
### AlterObjectSchemaStmt_8 : ALTER OPERATOR CLASS any_name USING access_method SET SCHEMA name
sub got_AlterObjectSchemaStmt_8  {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_OPCLASS,
      object     => $_[4],
      addname    => $_[6],
      newschema  => $_[9],
   );
}
### AlterObjectSchemaStmt_9 : ALTER OPERATOR FAMILY any_name USING access_method SET SCHEMA name
sub got_AlterObjectSchemaStmt_9  {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_OPFAMILY,
      object     => $_[4],
      addname    => $_[6],
      newschema  => $_[9],
   );
}
### AlterObjectSchemaStmt_10: ALTER TABLE relation_expr SET SCHEMA name
sub got_AlterObjectSchemaStmt_10 {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_TABLE,
      relation   => $_[3],
      newschema  => $_[6],
   );
}
### AlterObjectSchemaStmt_11: ALTER TEXT SEARCH PARSER any_name SET SCHEMA name
sub got_AlterObjectSchemaStmt_11 {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_TSPARSER,
      object     => $_[5],
      newschema  => $_[8],
   );
}
### AlterObjectSchemaStmt_12: ALTER TEXT SEARCH DICTIONARY any_name SET SCHEMA name
sub got_AlterObjectSchemaStmt_12 {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_TSDICTIONARY,
      object     => $_[5],
      newschema  => $_[8],
   );
}
### AlterObjectSchemaStmt_13: ALTER TEXT SEARCH TEMPLATE any_name SET SCHEMA name
sub got_AlterObjectSchemaStmt_13 {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_TSTEMPLATE,
      object     => $_[5],
      newschema  => $_[8],
   );
}
### AlterObjectSchemaStmt_14: ALTER TEXT SEARCH CONFIGURATION any_name SET SCHEMA name
sub got_AlterObjectSchemaStmt_14 {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_TSCONFIGURATION,
      object     => $_[5],
      newschema  => $_[8],
   );
}
### AlterObjectSchemaStmt_15: ALTER SEQUENCE qualified_name SET SCHEMA name
sub got_AlterObjectSchemaStmt_15 {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_SEQUENCE,
      relation   => $_[3],
      newschema  => $_[6],
   );
}
### AlterObjectSchemaStmt_16: ALTER VIEW qualified_name SET SCHEMA name
sub got_AlterObjectSchemaStmt_16 {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_VIEW,
      relation   => $_[3],
      newschema  => $_[6],
   );
}
### AlterObjectSchemaStmt_17: ALTER FOREIGN TABLE relation_expr SET SCHEMA name
sub got_AlterObjectSchemaStmt_17 {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_FOREIGN_TABLE,
      relation   => $_[4],
      newschema  => $_[7],
   );
}
### AlterObjectSchemaStmt_18: ALTER TYPE any_name SET SCHEMA name
sub got_AlterObjectSchemaStmt_18 {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_TYPE,
      object     => $_[3],
      newschema  => $_[6],
   );
}

### AlterOwnerStmt_1 : ALTER AGGREGATE func_name aggr_args OWNER TO RoleId
sub got_AlterOwnerStmt_1  {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_AGGREGATE,
      object     => $_[3],
      objarg     => $_[4],
      newowner   => $_[7],
   );
}
### AlterOwnerStmt_2 : ALTER COLLATION any_name OWNER TO RoleId
sub got_AlterOwnerStmt_2  {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_COLLATION,
      object     => $_[3],
      newowner   => $_[6],
   );
}
### AlterOwnerStmt_3 : ALTER CONVERSION any_name OWNER TO RoleId
sub got_AlterOwnerStmt_3  {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_CONVERSION,
      object     => $_[3],
      newowner   => $_[6],
   );
}
### AlterOwnerStmt_4 : ALTER DATABASE database_name OWNER TO RoleId
sub got_AlterOwnerStmt_4  {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_DATABASE,
      object     => $_[0]->lappend($_[3]),
      newowner   => $_[6],
   );
}
### AlterOwnerStmt_5 : ALTER DOMAIN any_name OWNER TO RoleId
sub got_AlterOwnerStmt_5  {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_DOMAIN,
      object     => $_[3],
      newowner   => $_[6],
   );
}
### AlterOwnerStmt_6 : ALTER FUNCTION function_with_argtypes OWNER TO RoleId
sub got_AlterOwnerStmt_6  {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_FUNCTION,
      object     => $_[3]->funcname,
      objarg     => $_[3]->funcargs,
      newowner   => $_[6],
   );
}
### AlterOwnerStmt_7 : ALTER opt_procedural LANGUAGE name OWNER TO RoleId
sub got_AlterOwnerStmt_7  {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_LANGUAGE,
      object     => $_[0]->lappend($_[4]),
      newowner   => $_[7],
   );
}
### AlterOwnerStmt_8 : ALTER LARGE OBJECT NumericOnly OWNER TO RoleId
sub got_AlterOwnerStmt_8  {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_LARGEOBJECT,
      object     => $_[0]->lappend($_[4]),
      newowner   => $_[7],
   );
}
### AlterOwnerStmt_9 : ALTER OPERATOR any_operator oper_argtypes OWNER TO RoleId
sub got_AlterOwnerStmt_9  {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_OPERATOR,
      object     => $_[3],
      objarg     => $_[4],
      newowner   => $_[7],
   );
}
### AlterOwnerStmt_10: ALTER OPERATOR CLASS any_name USING access_method OWNER TO RoleId
sub got_AlterOwnerStmt_10 {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_OPCLASS,
      object     => $_[4],
      addname    => $_[6],
      newowner   => $_[9],
   );
}
### AlterOwnerStmt_11: ALTER OPERATOR FAMILY any_name USING access_method OWNER TO RoleId
sub got_AlterOwnerStmt_11 {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_OPFAMILY,
      object     => $_[4],
      addname    => $_[6],
      newowner   => $_[9],
   );
}
### AlterOwnerStmt_12: ALTER SCHEMA name OWNER TO RoleId
sub got_AlterOwnerStmt_12 {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_SCHEMA,
      object     => $_[0]->lappend($_[3]),
      newowner   => $_[6],
   );
}
### AlterOwnerStmt_13: ALTER TYPE any_name OWNER TO RoleId
sub got_AlterOwnerStmt_13 {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_TYPE,
      object     => $_[3],
      newowner   => $_[6],
   );
}
### AlterOwnerStmt_14: ALTER TABLESPACE name OWNER TO RoleId
sub got_AlterOwnerStmt_14 {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_TABLESPACE,
      object     => $_[0]->lappend($_[3]),
      newowner   => $_[6],
   );
}
### AlterOwnerStmt_15: ALTER TEXT SEARCH DICTIONARY any_name OWNER TO RoleId
sub got_AlterOwnerStmt_15 {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_TSDICTIONARY,
      object     => $_[5],
      newowner   => $_[8],
   );
}
### AlterOwnerStmt_16: ALTER TEXT SEARCH CONFIGURATION any_name OWNER TO RoleId
sub got_AlterOwnerStmt_16 {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_TSCONFIGURATION,
      object     => $_[5],
      newowner   => $_[8],
   );
}
### AlterOwnerStmt_17: ALTER FOREIGN DATA WRAPPER name OWNER TO RoleId
sub got_AlterOwnerStmt_17 {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_FDW,
      object     => $_[0]->lappend($_[5]),
      newowner   => $_[8],
   );
}
### AlterOwnerStmt_18: ALTER SERVER name OWNER TO RoleId
sub got_AlterOwnerStmt_18 {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_FOREIGN_SERVER,
      object     => $_[0]->lappend($_[3]),
      newowner   => $_[6],
   );
}

### RuleStmt: CREATE opt_or_replace RULE name AS ON event TO qualified_name where_clause DO opt_instead RuleActionList
sub got_RuleStmt {
   return SQL::Translator::Statement::Rule->new(
      replace     => $_[2],
      relation    => $_[9],
      rulename    => $_[4],
      whereClause => $_[10],
      event       => $_[7],
      instead     => $_[12],
      actions     => $_[13],
   );
}

### RuleActionList_1: NOTHING
sub got_RuleActionList_1 { NIL }
### RuleActionList_2: RuleActionStmt
sub got_RuleActionList_2 { $_[0]->lappend($_[1]) }
### RuleActionList_3: <LPAREN> RuleActionMulti <RPAREN>
sub got_RuleActionList_3 { $_[2] }

### RuleActionMulti_1: RuleActionMulti <SEMI> RuleActionStmtOrEmpty
sub got_RuleActionMulti_1 { (defined $_[3]) ? $_[0]->lappend($_[1], $_[3]) : $_[1] }
### RuleActionMulti_2: RuleActionStmtOrEmpty
sub got_RuleActionMulti_2 { (defined $_[1]) ? $_[0]->lappend($_[1]) : NIL }

### RuleActionStmtOrEmpty_1: RuleActionStmt
sub got_RuleActionStmtOrEmpty_1 { $_[1] }

### event_1: SELECT
sub got_event_1 { CMD_SELECT }
### event_2: UPDATE
sub got_event_2 { CMD_UPDATE }
### event_3: DELETE
sub got_event_3 { CMD_DELETE }
### event_4: INSERT
sub got_event_4 { CMD_INSERT }

### opt_instead_1: INSTEAD
sub got_opt_instead_1 { TRUE  }
### opt_instead_2: ALSO
sub got_opt_instead_2 { FALSE }

### DropRuleStmt_1: DROP RULE name ON qualified_name opt_drop_behavior
sub got_DropRuleStmt_1 {
   return SQL::Translator::Statement::Drop->new(
      removeType => OBJECT_RULE,
      objects    => [ $_[0]->lappend($_[5], $_[3]) ],
      arguments  => NIL,
      behavior   => $_[6],
      missing_ok => FALSE,
      concurrent => FALSE,
   );
}
### DropRuleStmt_2: DROP RULE IF EXISTS name ON qualified_name opt_drop_behavior
sub got_DropRuleStmt_2 {
   return SQL::Translator::Statement::Drop->new(
      removeType => OBJECT_RULE,
      objects    => [ $_[0]->lappend($_[7], $_[5]) ],
      arguments  => NIL,
      behavior   => $_[8],
      missing_ok => TRUE,
      concurrent => FALSE,
   );
}

### NotifyStmt: NOTIFY ColId notify_payload
sub got_NotifyStmt {
   return SQL::Translator::Statement::Notify->new(
      conditionname => $_[2],
      payload       => $_[3],
   );
}

### notify_payload_1: <COMMA> Sconst
sub got_notify_payload_1 { $_[2] }

### ListenStmt: LISTEN ColId
sub got_ListenStmt {
   return SQL::Translator::Statement::Listen->new(
      conditionname => $_[2],
   );
}

### UnlistenStmt_1: UNLISTEN ColId
sub got_UnlistenStmt_1 {
   return SQL::Translator::Statement::Unlisten->new(
      conditionname => $_[2],
   );
}
### UnlistenStmt_2: UNLISTEN <STAR>
sub got_UnlistenStmt_2 {
   return SQL::Translator::Statement::Unlisten->new(
      conditionname => NULL,
   );
}

### TransactionStmt_1 : ABORT opt_transaction
sub got_TransactionStmt_1  {
   return SQL::Translator::Statement::Transaction->new(
      kind    => TRANS_STMT_ROLLBACK,
      options => NIL,
   );
}
### TransactionStmt_2 : BEGIN opt_transaction transaction_mode_list_or_empty
sub got_TransactionStmt_2  {
   return SQL::Translator::Statement::Transaction->new(
      kind    => TRANS_STMT_BEGIN,
      options => $_[3],
   );
}
### TransactionStmt_3 : START TRANSACTION transaction_mode_list_or_empty
sub got_TransactionStmt_3  {
   return SQL::Translator::Statement::Transaction->new(
      kind    => TRANS_STMT_START,
      options => $_[3],
   );
}
### TransactionStmt_4 : COMMIT opt_transaction
sub got_TransactionStmt_4  {
   return SQL::Translator::Statement::Transaction->new(
      kind    => TRANS_STMT_COMMIT,
      options => NIL,
   );
}
### TransactionStmt_5 : END opt_transaction
sub got_TransactionStmt_5  {
   return SQL::Translator::Statement::Transaction->new(
      kind    => TRANS_STMT_COMMIT,
      options => NIL,
   );
}
### TransactionStmt_6 : ROLLBACK opt_transaction
sub got_TransactionStmt_6  {
   return SQL::Translator::Statement::Transaction->new(
      kind    => TRANS_STMT_ROLLBACK,
      options => NIL,
   );
}
### TransactionStmt_7 : SAVEPOINT ColId
sub got_TransactionStmt_7  {
   return SQL::Translator::Statement::Transaction->new(
      kind    => TRANS_STMT_SAVEPOINT,
      options => $_[0]->lappend($_[0]->makeDefElem("savepoint_name", $_[2])),
   );
}
### TransactionStmt_8 : RELEASE SAVEPOINT ColId
sub got_TransactionStmt_8  {
   return SQL::Translator::Statement::Transaction->new(
      kind    => TRANS_STMT_RELEASE,
      options => $_[0]->lappend($_[0]->makeDefElem("savepoint_name", $_[3])),
   );
}
### TransactionStmt_9 : RELEASE ColId
sub got_TransactionStmt_9  {
   return SQL::Translator::Statement::Transaction->new(
      kind    => TRANS_STMT_RELEASE,
      options => $_[0]->lappend($_[0]->makeDefElem("savepoint_name", $_[2])),
   );
}
### TransactionStmt_10: ROLLBACK opt_transaction TO SAVEPOINT ColId
sub got_TransactionStmt_10 {
   return SQL::Translator::Statement::Transaction->new(
      kind    => TRANS_STMT_ROLLBACK_TO,
      options => $_[0]->lappend($_[0]->makeDefElem("savepoint_name", $_[5])),
   );
}
### TransactionStmt_11: ROLLBACK opt_transaction TO ColId
sub got_TransactionStmt_11 {
   return SQL::Translator::Statement::Transaction->new(
      kind    => TRANS_STMT_ROLLBACK_TO,
      options => $_[0]->lappend($_[0]->makeDefElem("savepoint_name", $_[4])),
   );
}
### TransactionStmt_12: PREPARE TRANSACTION Sconst
sub got_TransactionStmt_12 {
   return SQL::Translator::Statement::Transaction->new(
      kind    => TRANS_STMT_PREPARE,
      gid     => $_[3],
   );
}
### TransactionStmt_13: COMMIT PREPARED Sconst
sub got_TransactionStmt_13 {
   return SQL::Translator::Statement::Transaction->new(
      kind    => TRANS_STMT_COMMIT_PREPARED,
      gid     => $_[3],
   );
}
### TransactionStmt_14: ROLLBACK PREPARED Sconst
sub got_TransactionStmt_14 {
   return SQL::Translator::Statement::Transaction->new(
      kind    => TRANS_STMT_ROLLBACK_PREPARED,
      gid     => $_[3],
   );
}

### opt_transaction_1: WORK
sub got_opt_transaction_1 {}
### opt_transaction_2: TRANSACTION
sub got_opt_transaction_2 {}

### transaction_mode_item_1: ISOLATION LEVEL iso_level
sub got_transaction_mode_item_1 { $_[0]->makeDefElem("transaction_isolation",  $_[0]->makeStringConst($_[3], $_[0]->YYLLoc($_[3], 3))) }
### transaction_mode_item_2: READ ONLY
sub got_transaction_mode_item_2 { $_[0]->makeDefElem("transaction_read_only",  $_[0]->makeIntConst   (TRUE,  $_[0]->YYLLoc($_[1], 1))) }
### transaction_mode_item_3: READ WRITE
sub got_transaction_mode_item_3 { $_[0]->makeDefElem("transaction_read_only",  $_[0]->makeIntConst   (FALSE, $_[0]->YYLLoc($_[1], 1))) }
### transaction_mode_item_4: DEFERRABLE
sub got_transaction_mode_item_4 { $_[0]->makeDefElem("transaction_deferrable", $_[0]->makeIntConst   (TRUE,  $_[0]->YYLLoc($_[1], 1))) }
### transaction_mode_item_5: NOT DEFERRABLE
sub got_transaction_mode_item_5 { $_[0]->makeDefElem("transaction_deferrable", $_[0]->makeIntConst   (FALSE, $_[0]->YYLLoc($_[1], 1))) }

### transaction_mode_list_1: transaction_mode_item
sub got_transaction_mode_list_1 { $_[0]->lappend($_[1])        }
### transaction_mode_list_2: transaction_mode_list <COMMA> transaction_mode_item
sub got_transaction_mode_list_2 { $_[0]->lappend($_[1], $_[3]) }
### transaction_mode_list_3: transaction_mode_list transaction_mode_item
sub got_transaction_mode_list_3 { $_[0]->lappend($_[1], $_[2]) }

### ViewStmt_1: CREATE OptTemp VIEW qualified_name opt_column_list AS SelectStmt opt_check_option
sub got_ViewStmt_1 {
   my $n = SQL::Translator::Statement::View->new(
      view    => $_[4],
      aliases => $_[5],
      query   => $_[7],
      replace => FALSE,
   );
   $n->view->relpersistence($_[2]);
   return $n;
}
### ViewStmt_2: CREATE OR REPLACE OptTemp VIEW qualified_name opt_column_list AS SelectStmt opt_check_option
sub got_ViewStmt_2 {
   my $n = SQL::Translator::Statement::View->new(
      view    => $_[6],
      aliases => $_[7],
      query   => $_[9],
      replace => TRUE,
   );
   $n->view->relpersistence($_[4]);
   return $n;
}

### opt_check_option_1: WITH CHECK OPTION
sub got_opt_check_option_1 {
   $_[0]->ereport(ERROR,
         ERRCODE_FEATURE_NOT_SUPPORTED,
          "WITH CHECK OPTION is not implemented");
}
### opt_check_option_2: WITH CASCADED CHECK OPTION
sub got_opt_check_option_2 {
   $_[0]->ereport(ERROR,
         ERRCODE_FEATURE_NOT_SUPPORTED,
          "WITH CHECK OPTION is not implemented");
}
### opt_check_option_3: WITH LOCAL CHECK OPTION
sub got_opt_check_option_3 {
   $_[0]->ereport(ERROR,
         ERRCODE_FEATURE_NOT_SUPPORTED,
          "WITH CHECK OPTION is not implemented");
}

### LoadStmt: LOAD file_name
sub got_LoadStmt {
   return SQL::Translator::Statement::Load->new(
      filename => $_[2],
   );
}

### CreatedbStmt: CREATE DATABASE database_name opt_with createdb_opt_list
sub got_CreatedbStmt {
   return SQL::Translator::Statement::Createdb->new(
      dbname  => $_[3],
      options => $_[5],
   );
}

### createdb_opt_list_1: createdb_opt_list createdb_opt_item
sub got_createdb_opt_list_1 { $_[0]->lappend($_[1], $_[2]) }

### createdb_opt_item_1 : TABLESPACE opt_equal name
sub got_createdb_opt_item_1  { $_[0]->makeDefElem("tablespace",  $_[3]) }
### createdb_opt_item_2 : TABLESPACE opt_equal DEFAULT
sub got_createdb_opt_item_2  { $_[0]->makeDefElem("tablespace",   NULL) }
### createdb_opt_item_3 : LOCATION opt_equal Sconst
sub got_createdb_opt_item_3  { $_[0]->makeDefElem("location",    $_[3]) }
### createdb_opt_item_4 : LOCATION opt_equal DEFAULT
sub got_createdb_opt_item_4  { $_[0]->makeDefElem("location",     NULL) }
### createdb_opt_item_5 : TEMPLATE opt_equal name
sub got_createdb_opt_item_5  { $_[0]->makeDefElem("template",    $_[3]) }
### createdb_opt_item_6 : TEMPLATE opt_equal DEFAULT
sub got_createdb_opt_item_6  { $_[0]->makeDefElem("template",     NULL) }
### createdb_opt_item_7 : ENCODING opt_equal Sconst
sub got_createdb_opt_item_7  { $_[0]->makeDefElem("encoding",    $_[3]) }
### createdb_opt_item_8 : ENCODING opt_equal Iconst
sub got_createdb_opt_item_8  { $_[0]->makeDefElem("encoding",  $_[3]+0) }
### createdb_opt_item_9 : ENCODING opt_equal DEFAULT
sub got_createdb_opt_item_9  { $_[0]->makeDefElem("encoding",     NULL) }
### createdb_opt_item_10: LC_COLLATE opt_equal Sconst
sub got_createdb_opt_item_10 { $_[0]->makeDefElem("lc_collate",  $_[3]) }
### createdb_opt_item_11: LC_COLLATE opt_equal DEFAULT
sub got_createdb_opt_item_11 { $_[0]->makeDefElem("lc_collate",   NULL) }
### createdb_opt_item_12: LC_CTYPE opt_equal Sconst
sub got_createdb_opt_item_12 { $_[0]->makeDefElem("lc_ctype",    $_[3]) }
### createdb_opt_item_13: LC_CTYPE opt_equal DEFAULT
sub got_createdb_opt_item_13 { $_[0]->makeDefElem("lc_ctype",     NULL) }
### createdb_opt_item_14: CONNECTION LIMIT opt_equal SignedIconst
sub got_createdb_opt_item_14 { $_[0]->makeDefElem("connectionlimit", $_[4]+0) }
### createdb_opt_item_15: OWNER opt_equal name
sub got_createdb_opt_item_15 { $_[0]->makeDefElem("owner",       $_[3]) }
### createdb_opt_item_16: OWNER opt_equal DEFAULT
sub got_createdb_opt_item_16 { $_[0]->makeDefElem("owner",        NULL) }

### opt_equal_1: <EQUAL>
sub got_opt_equal_1 {}

### AlterDatabaseStmt_1: ALTER DATABASE database_name opt_with alterdb_opt_list
sub got_AlterDatabaseStmt_1 {
  return SQL::Translator::Statement::AlterDatabase->new(
     dbname  => $_[3],
     options => $_[5],
  );
}
### AlterDatabaseStmt_2: ALTER DATABASE database_name SET TABLESPACE name
sub got_AlterDatabaseStmt_2 {
  return SQL::Translator::Statement::AlterDatabase->new(
     dbname  => $_[3],
     options => $_[0]->lappend($_[0]->makeDefElem("tablespace", $_[6])),
  );
}

### AlterDatabaseSetStmt: ALTER DATABASE database_name SetResetClause
sub got_AlterDatabaseSetStmt {
   return SQL::Translator::Statement::AlterDatabaseSet->new(
      dbname  => $_[3],
      setstmt => $_[4],
   );
}

### alterdb_opt_list_1: alterdb_opt_list alterdb_opt_item
sub got_alterdb_opt_list_1 { $_[0]->lappend($_[1], $_[2]) }

### alterdb_opt_item: CONNECTION LIMIT opt_equal SignedIconst
sub got_alterdb_opt_item { $_[0]->makeDefElem("connectionlimit", $_[4]+0) }

### DropdbStmt_1: DROP DATABASE database_name
sub got_DropdbStmt_1 {
   return SQL::Translator::Statement::Dropdb->new(
      dbname     => $_[3],
      missing_ok => FALSE,
   );
}
### DropdbStmt_2: DROP DATABASE IF EXISTS database_name
sub got_DropdbStmt_2 {
   return SQL::Translator::Statement::Dropdb->new(
      dbname     => $_[5],
      missing_ok => TRUE,
   );
}

### CreateDomainStmt: CREATE DOMAIN any_name opt_as Typename ColQualList
sub got_CreateDomainStmt {
   my $n = SQL::Translator::Statement::CreateDomain->new(
      domainname => $_[3],
      typeName   => $_[5],
   );
   $_[0]->SplitColQualList($_[6], $n);
   return $n;
}

# ALTER DOMAIN <domain> DROP NOT NULL
### AlterDomainStmt_1: 
# ALTER DOMAIN <domain> {SET DEFAULT <expr>|DROP DEFAULT}
    ALTER DOMAIN any_name alter_column_default
sub got_AlterDomainStmt_1 {
   return SQL::Translator::Statement::AlterDomain->new(
      subtype  => 'T',
      typeName => $_[3],
      def      => $_[4],
   );
}
# ALTER DOMAIN <domain> SET NOT NULL
### AlterDomainStmt_2: ALTER DOMAIN any_name DROP NOT NULL
sub got_AlterDomainStmt_2 {
   return SQL::Translator::Statement::AlterDomain->new(
      subtype  => 'N',
      typeName => $_[3],
   );
}
# ALTER DOMAIN <domain> ADD CONSTRAINT ...
### AlterDomainStmt_3: ALTER DOMAIN any_name SET NOT NULL
sub got_AlterDomainStmt_3 {
   return SQL::Translator::Statement::AlterDomain->new(
      subtype  => 'O',
      typeName => $_[3],
   );
}
# ALTER DOMAIN <domain> DROP CONSTRAINT <name> [RESTRICT|CASCADE]
### AlterDomainStmt_4: ALTER DOMAIN any_name ADD TableConstraint
sub got_AlterDomainStmt_4 {
   return SQL::Translator::Statement::AlterDomain->new(
      subtype  => 'C',
      typeName => $_[3],
      def      => $_[5],
   );
}
### AlterDomainStmt_5: ALTER DOMAIN any_name DROP CONSTRAINT name opt_drop_behavior
sub got_AlterDomainStmt_5 {
   return SQL::Translator::Statement::AlterDomain->new(
      subtype  => 'X',
      typeName => $_[3],
      name     => $_[6],
      behavior => $_[7],
   );
}

### opt_as_1: AS
sub got_opt_as_1 {}

### AlterTSDictionaryStmt: ALTER TEXT SEARCH DICTIONARY any_name definition
sub got_AlterTSDictionaryStmt {
   return SQL::Translator::Statement::AlterTSDictionary->new(
      dictname => $_[5],
      options  => $_[6],
   );
}

### AlterTSConfigurationStmt_1: ALTER TEXT SEARCH CONFIGURATION any_name ADD MAPPING FOR name_list WITH any_name_list
sub got_AlterTSConfigurationStmt_1 {
   return SQL::Translator::Statement::AlterTSConfiguration->new(
      cfgname    => $_[5],
      tokentype  => $_[9],
      dicts      => $_[11],
      override   => FALSE,
      replace    => FALSE,
   );
}
### AlterTSConfigurationStmt_2: ALTER TEXT SEARCH CONFIGURATION any_name ALTER MAPPING FOR name_list WITH any_name_list
sub got_AlterTSConfigurationStmt_2 {
   return SQL::Translator::Statement::AlterTSConfiguration->new(
      cfgname    => $_[5],
      tokentype  => $_[9],
      dicts      => $_[11],
      override   => TRUE,
      replace    => FALSE,
   );
}
### AlterTSConfigurationStmt_3: ALTER TEXT SEARCH CONFIGURATION any_name ALTER MAPPING REPLACE any_name WITH any_name
sub got_AlterTSConfigurationStmt_3 {
   return SQL::Translator::Statement::AlterTSConfiguration->new(
      cfgname    => $_[5],
      tokentype  => NIL,
      dicts      => $_[0]->lappend($_[9],$_[11]),
      override   => FALSE,
      replace    => TRUE,
   );
}
### AlterTSConfigurationStmt_4: ALTER TEXT SEARCH CONFIGURATION any_name ALTER MAPPING FOR name_list REPLACE any_name WITH any_name
sub got_AlterTSConfigurationStmt_4 {
   return SQL::Translator::Statement::AlterTSConfiguration->new(
      cfgname    => $_[5],
      tokentype  => $_[9],
      dicts      => $_[0]->lappend($_[11],$_[13]),
      override   => FALSE,
      replace    => TRUE,
   );
}
### AlterTSConfigurationStmt_5: ALTER TEXT SEARCH CONFIGURATION any_name DROP MAPPING FOR name_list
sub got_AlterTSConfigurationStmt_5 {
   return SQL::Translator::Statement::AlterTSConfiguration->new(
      cfgname    => $_[5],
      tokentype  => $_[9],
      missing_ok => FALSE,
   );
}
### AlterTSConfigurationStmt_6: ALTER TEXT SEARCH CONFIGURATION any_name DROP MAPPING IF EXISTS FOR name_list
sub got_AlterTSConfigurationStmt_6 {
   return SQL::Translator::Statement::AlterTSConfiguration->new(
      cfgname    => $_[5],
      tokentype  => $_[11],
      missing_ok => TRUE,
   );
}

### CreateConversionStmt: CREATE opt_default CONVERSION any_name FOR Sconst TO Sconst FROM any_name
sub got_CreateConversionStmt {
  return SQL::Translator::Statement::CreateConversion->new(
     conversion_name   => $_[4],
     for_encoding_name => $_[6],
     to_encoding_name  => $_[8],
     func_name         => $_[10],
     def               => $_[2],
  );
}

### ClusterStmt_1: CLUSTER opt_verbose qualified_name cluster_index_specification
sub got_ClusterStmt_1 {
    return SQL::Translator::Statement::Cluster->new(
      relation  => $_[3],
      indexname => $_[4],
      verbose   => $_[2],
   );
}
# kept for pre-8.3 compatibility
### ClusterStmt_2: CLUSTER opt_verbose
sub got_ClusterStmt_2 {
   return SQL::Translator::Statement::Cluster->new(
      relation  => NULL,
      indexname => NULL,
      verbose   => $_[2],
   );
}
### ClusterStmt_3: CLUSTER opt_verbose index_name ON qualified_name
sub got_ClusterStmt_3 {
   return SQL::Translator::Statement::Cluster->new(
      relation  => $_[5],
      indexname => $_[3],
      verbose   => $_[2],
   );
}

### cluster_index_specification_1: USING index_name
sub got_cluster_index_specification_1 { $_[2] }

### VacuumStmt_1: VACUUM opt_full opt_freeze opt_verbose
sub got_VacuumStmt_1 {
   return SQL::Translator::Statement::Analyze->new(
      options          => VACOPT_VACUUM |
         $_[2] ? VACOPT_FULL : 0 |
         $_[4] ? VACOPT_VERBOSE : 0,
      freeze_min_age   => $_[3] ? 0 : -1,
      freeze_table_age => $_[3] ? 0 : -1,
      relation         => NULL,
      va_cols          => NIL,
   );
}
### VacuumStmt_2: VACUUM opt_full opt_freeze opt_verbose qualified_name
sub got_VacuumStmt_2 {
   return SQL::Translator::Statement::Analyze->new(
      options          => VACOPT_VACUUM |
         $_[2] ? VACOPT_FULL : 0 |
         $_[4] ? VACOPT_VERBOSE : 0,
      freeze_min_age   => $_[3] ? 0 : -1,
      freeze_table_age => $_[3] ? 0 : -1,
      relation         => $_[5],
      va_cols          => NIL,
   );
}
### VacuumStmt_3: VACUUM opt_full opt_freeze opt_verbose AnalyzeStmt
sub got_VacuumStmt_3 {
   $_[5]->options( VACOPT_VACUUM |
      $_[2] ? VACOPT_FULL : 0 |
      $_[4] ? VACOPT_VERBOSE : 0
   );
   $_[5]->freeze_min_age  ($_[3] ? 0 : -1);
   $_[5]->freeze_table_age($_[3] ? 0 : -1);
   return $_[5];
}
### VacuumStmt_4: VACUUM <LPAREN> vacuum_option_list <RPAREN>
sub got_VacuumStmt_4 {
   my $opt = VACOPT_VACUUM | $_[3];
   return SQL::Translator::Statement::Analyze->new(
      options          => $opt,
      freeze_min_age   => ($opt & VACOPT_FREEZE) ? 0 : -1,
      freeze_table_age => ($opt & VACOPT_FREEZE) ? 0 : -1,
      relation         => NULL,
      va_cols          => NIL,
   );
}
### VacuumStmt_5: VACUUM <LPAREN> vacuum_option_list <RPAREN> qualified_name opt_name_list
sub got_VacuumStmt_5 {
   my $opt = VACOPT_VACUUM | $_[3] | $_[6] ? VACOPT_ANALYZE : 0;  #* implies analyze
   return SQL::Translator::Statement::Analyze->new(
      options          => $opt,
      freeze_min_age   => ($opt & VACOPT_FREEZE) ? 0 : -1,
      freeze_table_age => ($opt & VACOPT_FREEZE) ? 0 : -1,
      relation         => $_[5],
      va_cols          => $_[6],
   );
}

### vacuum_option_list_1: vacuum_option_elem
sub got_vacuum_option_list_1 { $_[1] }
### vacuum_option_list_2: vacuum_option_list <COMMA> vacuum_option_elem
sub got_vacuum_option_list_2 { $_[1] | $_[3] }

### vacuum_option_elem_1: analyze_keyword
sub got_vacuum_option_elem_1 { VACOPT_ANALYZE }
### vacuum_option_elem_2: VERBOSE
sub got_vacuum_option_elem_2 { VACOPT_VERBOSE }
### vacuum_option_elem_3: FREEZE
sub got_vacuum_option_elem_3 { VACOPT_FREEZE  }
### vacuum_option_elem_4: FULL
sub got_vacuum_option_elem_4 { VACOPT_FULL    }

### AnalyzeStmt_1: analyze_keyword opt_verbose
sub got_AnalyzeStmt_1 {
   return SQL::Translator::Statement::Analyze->new(
      options          => VACOPT_ANALYZE | $_[2] ? VACOPT_VERBOSE : 0,
      freeze_min_age   => -1,
      freeze_table_age => -1,
      relation         => NULL,
      va_cols          => NIL,
   );
}
### AnalyzeStmt_2: analyze_keyword opt_verbose qualified_name opt_name_list
sub got_AnalyzeStmt_2 {
   return SQL::Translator::Statement::Analyze->new(
      options          => VACOPT_ANALYZE | $_[2] ? VACOPT_VERBOSE : 0,
      freeze_min_age   => -1,
      freeze_table_age => -1,
      relation         => $_[3],
      va_cols          => $_[4],
   );
}

### analyze_keyword_1: ANALYZE
sub got_analyze_keyword_1 {}
# British
### analyze_keyword_2: ANALYSE
sub got_analyze_keyword_2 {}

### opt_verbose_1: VERBOSE
sub got_opt_verbose_1 { TRUE  }

### opt_full_1: FULL
sub got_opt_full_1 { TRUE  }

### opt_freeze_1: FREEZE
sub got_opt_freeze_1 { TRUE  }

### opt_name_list_1: <LPAREN> name_list <RPAREN>
sub got_opt_name_list_1 { $_[2] }

### ExplainStmt_1: EXPLAIN ExplainableStmt
sub got_ExplainStmt_1 {
   return SQL::Translator::Statement::Explain->new(
      query   => $_[2],
      options => NIL,
   );
}
### ExplainStmt_2: EXPLAIN analyze_keyword opt_verbose ExplainableStmt
sub got_ExplainStmt_2 {
   return SQL::Translator::Statement::Explain->new(
      query   => $_[4],
      options => $_[0]->lappend(
         $_[0]->makeDefElem($_[3] ? "analyze" : "verbose", NULL)
      ),
   );
}
### ExplainStmt_3: EXPLAIN VERBOSE ExplainableStmt
sub got_ExplainStmt_3 {
   return SQL::Translator::Statement::Explain->new(
      query   => $_[3],
      options => $_[0]->lappend($_[0]->makeDefElem("verbose", NULL)),
   );
}
### ExplainStmt_4: EXPLAIN <LPAREN> explain_option_list <RPAREN> ExplainableStmt
sub got_ExplainStmt_4 {
   return SQL::Translator::Statement::Explain->new(
      query   => $_[5],
      options => $_[3],
   );
}

### explain_option_list_1: explain_option_elem
sub got_explain_option_list_1 { $_[0]->lappend($_[1]) }
### explain_option_list_2: explain_option_list <COMMA> explain_option_elem
sub got_explain_option_list_2 { $_[0]->lappend($_[1], $_[3]) }

### explain_option_elem: explain_option_name explain_option_arg
sub got_explain_option_elem { $_[0]->makeDefElem($_[1], $_[2]) }

### explain_option_name_1: ColId
sub got_explain_option_name_1 { $_[1] }
### explain_option_name_2: analyze_keyword
sub got_explain_option_name_2 { "analyze" }
### explain_option_name_3: VERBOSE
sub got_explain_option_name_3 { "verbose" }

### explain_option_arg_1: opt_boolean_or_string
sub got_explain_option_arg_1 { $_[1] }
### explain_option_arg_2: NumericOnly
sub got_explain_option_arg_2 { $_[1] }

### PrepareStmt: PREPARE name prep_type_clause AS PreparableStmt
sub got_PrepareStmt {
   return SQL::Translator::Statement::Prepare->new(
      name     => $_[2],
      argtypes => $_[3],
      query    => $_[5],
   );
}

### prep_type_clause_1: <LPAREN> type_list <RPAREN>
sub got_prep_type_clause_1 { $_[2] }

### ExecuteStmt_1: EXECUTE name execute_param_clause
sub got_ExecuteStmt_1 {
   return SQL::Translator::Statement::Execute->new(
      name   => $_[2],
      params => $_[3],
   );
}
### ExecuteStmt_2: CREATE OptTemp TABLE create_as_target AS EXECUTE name execute_param_clause opt_with_data
sub got_ExecuteStmt_2 {
   $_[4]->rel->relpersistence($_[2]);
   $_[4]->skipData(!$_[9]);
   my $ctas = SQL::Translator::Statement::CreateTableAs->new(
      query          => SQL::Translator::Statement::Execute->new(
         name   => $_[7],
         params => $_[8],
      ),
      into           => $_[4],
      is_select_into => FALSE,
   );
}

### execute_param_clause_1: <LPAREN> expr_list <RPAREN>
sub got_execute_param_clause_1 { $_[2] }

### DeallocateStmt_1: DEALLOCATE name
sub got_DeallocateStmt_1 {
   return SQL::Translator::Statement::Deallocate->new(
      name => $_[2],
   );
}
### DeallocateStmt_2: DEALLOCATE PREPARE name
sub got_DeallocateStmt_2 {
   return SQL::Translator::Statement::Deallocate->new(
      name => $_[3],
   );
}
### DeallocateStmt_3: DEALLOCATE ALL
sub got_DeallocateStmt_3 {
   return SQL::Translator::Statement::Deallocate->new(
      name => NULL,
   );
}
### DeallocateStmt_4: DEALLOCATE PREPARE ALL
sub got_DeallocateStmt_4 {
   return SQL::Translator::Statement::Deallocate->new(
      name => NULL,
   );
}

### InsertStmt: opt_with_clause INSERT INTO qualified_name insert_rest returning_clause
sub got_InsertStmt {
   $_[5]->relation($_[4]);
   $_[5]->returningList($_[6]);
   $_[5]->withClause($_[1]);
   return $_[5];
}

### insert_rest_1: SelectStmt
sub got_insert_rest_1 {
   return SQL::Translator::Statement::Insert->new(
      cols       => NIL,
      selectStmt => $_[1],
   );
}
### insert_rest_2: <LPAREN> insert_column_list <RPAREN> SelectStmt
sub got_insert_rest_2 {
   return SQL::Translator::Statement::Insert->new(
      cols       => $_[2],
      selectStmt => $_[4],
   );
}
### insert_rest_3: DEFAULT VALUES
sub got_insert_rest_3 {
   return SQL::Translator::Statement::Insert->new(
      cols       => NIL,
      selectStmt => NULL,
   );
}

### insert_column_list_1: insert_column_item
sub got_insert_column_list_1 { $_[0]->lappend($_[1]) }
### insert_column_list_2: insert_column_list <COMMA> insert_column_item
sub got_insert_column_list_2 { $_[0]->lappend($_[1], $_[3]) }

### insert_column_item: ColId opt_indirection
sub got_insert_column_item {
   return SQL::Translator::Statement::ResultTarget->new(
      name        => $_[1],
      indirection => $_[0]->check_indirection($_[2]),
      val         => NULL,
      location    => $_[0]->YYLLoc($_[1], 1),
   );
}

### returning_clause_1: RETURNING target_list
sub got_returning_clause_1 { $_[2] }

### DeleteStmt: opt_with_clause DELETE FROM relation_expr_opt_alias using_clause where_or_current_clause returning_clause
sub got_DeleteStmt {
   return SQL::Translator::Statement::Delete->new(
      relation => $_[4],
      usingClause => $_[5],
      whereClause => $_[6],
      returningList => $_[7],
      withClause => $_[1],
   );
}

### using_clause_1: USING from_list
sub got_using_clause_1 { $_[2] }

### LockStmt: LOCK opt_table relation_expr_list opt_lock opt_nowait
sub got_LockStmt {
   return SQL::Translator::Statement::Lock->new(
      relations => $_[3],
      mode      => $_[4],
      nowait    => $_[5],
   );
}

### opt_lock_1: IN lock_type MODE
sub got_opt_lock_1 { $_[2]                      }

### lock_type_1: ACCESS SHARE
sub got_lock_type_1 { 'AccessShareLock'          }
### lock_type_2: ROW SHARE
sub got_lock_type_2 { 'RowShareLock'             }
### lock_type_3: ROW EXCLUSIVE
sub got_lock_type_3 { 'RowExclusiveLock'         }
### lock_type_4: SHARE UPDATE EXCLUSIVE
sub got_lock_type_4 { 'ShareUpdateExclusiveLock' }
### lock_type_5: SHARE
sub got_lock_type_5 { 'ShareLock'                }
### lock_type_6: SHARE ROW EXCLUSIVE
sub got_lock_type_6 { 'ShareRowExclusiveLock'    }
### lock_type_7: EXCLUSIVE
sub got_lock_type_7 { 'ExclusiveLock'            }
### lock_type_8: ACCESS EXCLUSIVE
sub got_lock_type_8 { 'AccessExclusiveLock'      }

### opt_nowait_1: NOWAIT
sub got_opt_nowait_1 { TRUE  }

### UpdateStmt: opt_with_clause UPDATE relation_expr_opt_alias SET set_clause_list from_clause where_or_current_clause returning_clause
sub got_UpdateStmt {
   return SQL::Translator::Statement::Update->new(
      relation      => $_[3],
      targetList    => $_[5],
      fromClause    => $_[6],
      whereClause   => $_[7],
      returningList => $_[8],
      withClause    => $_[1],
   );
}

### set_clause_list_1: set_clause
sub got_set_clause_list_1 { $_[1] }
### set_clause_list_2: set_clause_list <COMMA> set_clause
sub got_set_clause_list_2 { $_[0]->lappend($_[1],$_[3]) }

### set_clause_1: single_set_clause
sub got_set_clause_1 { $_[0]->lappend($_[1]) }
### set_clause_2: multiple_set_clause
sub got_set_clause_2 { $_[1] }

### single_set_clause: set_target <EQUAL> ctext_expr
sub got_single_set_clause {
   $_[1]->val($_[3]);
   return $_[1];
}

### multiple_set_clause: <LPAREN> set_target_list <RPAREN> <EQUAL> ctext_row
sub got_multiple_set_clause {
   #* Break the ctext_row apart, merge individual expressions
   #* into the destination ResTargets.  XXX this approach
   #* cannot work for general row expressions as sources.
   unless (@{$_[2]} == @{$_[5]}) {
      $_[0]->ereport(ERROR,
            ERRCODE_SYNTAX_ERROR,
             "number of columns does not match number of values",
             $_[0]->YYLLoc($_[1], 1));
   }
   pairwise {
      $a->val($b);
   } @{$_[2]}, @{$_[5]};

   return $_[2];
}

### set_target: ColId opt_indirection
sub got_set_target {
   return SQL::Translator::Statement::ResultTarget->new(
      name        => $_[1],
      indirection => $_[0]->check_indirection($_[2]),
      val         => NULL,  #* upper production sets this
      location    => $_[0]->YYLLoc($_[1], 1),
   );
}

### set_target_list_1: set_target
sub got_set_target_list_1 { $_[0]->lappend($_[1]) }
### set_target_list_2: set_target_list <COMMA> set_target
sub got_set_target_list_2 { $_[0]->lappend($_[1],$_[3]) }

### DeclareCursorStmt: DECLARE cursor_name cursor_options CURSOR opt_hold FOR SelectStmt
sub got_DeclareCursorStmt {
   return SQL::Translator::Statement::DeclareCursor->new(
      portalname => $_[2],
      #* currently we always set FAST_PLAN option
      options => $_[3] | $_[5] | CURSOR_OPT_FAST_PLAN,
      query => $_[7],
   );
}

### cursor_name: name
sub got_cursor_name { $_[1] }

### cursor_options_1: cursor_options NO SCROLL
sub got_cursor_options_1 { $_[1] | CURSOR_OPT_NO_SCROLL   }
### cursor_options_2: cursor_options SCROLL
sub got_cursor_options_2 { $_[1] | CURSOR_OPT_SCROLL      }
### cursor_options_3: cursor_options BINARY
sub got_cursor_options_3 { $_[1] | CURSOR_OPT_BINARY      }
### cursor_options_4: cursor_options INSENSITIVE
sub got_cursor_options_4 { $_[1] | CURSOR_OPT_INSENSITIVE }

### opt_hold_1: WITH HOLD
sub got_opt_hold_1 { CURSOR_OPT_HOLD }
### opt_hold_2: WITHOUT HOLD
sub got_opt_hold_2 { 0 }

### select_with_parens_1: <LPAREN> select_no_parens <RPAREN>
sub got_select_with_parens_1 { $_[2] }
### select_with_parens_2: <LPAREN> select_with_parens <RPAREN>
sub got_select_with_parens_2 { $_[2] }

### select_no_parens_1: simple_select
sub got_select_no_parens_1 { $_[1] }
### select_no_parens_2: select_clause sort_clause
sub got_select_no_parens_2 {
   $_[0]->insertSelectOptions($_[1], $_[2], NIL, NULL, NULL, NULL);
   return $_[1];
}
### select_no_parens_3: select_clause opt_sort_clause for_locking_clause opt_select_limit
sub got_select_no_parens_3 {
   $_[0]->insertSelectOptions($_[1], $_[2], $_[3], $_[4]->[0,1], NULL);
   return $_[1];
}
### select_no_parens_4: select_clause opt_sort_clause select_limit opt_for_locking_clause
sub got_select_no_parens_4 {
   $_[0]->insertSelectOptions($_[1], $_[2], $_[4], $_[3]->[0,1], NULL);
   return $_[1];
}
### select_no_parens_5: with_clause select_clause
sub got_select_no_parens_5 {
   $_[0]->insertSelectOptions($_[2], NULL, NIL, NULL, NULL, $_[1]);
   return $_[2];
}
### select_no_parens_6: with_clause select_clause sort_clause
sub got_select_no_parens_6 {
   $_[0]->insertSelectOptions($_[2], $_[3], NIL, NULL, NULL, $_[1]);
   return $_[2];
}
### select_no_parens_7: with_clause select_clause opt_sort_clause for_locking_clause opt_select_limit
sub got_select_no_parens_7 {
   $_[0]->insertSelectOptions($_[2], $_[3], $_[4], $_[5]->[0,1], $_[1]);
   return $_[2];
}
### select_no_parens_8: with_clause select_clause opt_sort_clause select_limit opt_for_locking_clause
sub got_select_no_parens_8 {
   $_[0]->insertSelectOptions($_[2], $_[3], $_[5], $_[4]->[0,1], $_[1]);
   return $_[2];
}

### select_clause_1: simple_select
sub got_select_clause_1 { $_[1] }
### select_clause_2: select_with_parens
sub got_select_clause_2 { $_[1] }

### simple_select_1: SELECT opt_distinct target_list into_clause from_clause where_clause group_clause having_clause window_clause
sub got_simple_select_1 {
   return SQL::Translator::Statement::Select->new(
      distinctClause => $_[2],
      targetList     => $_[3],
      intoClause     => $_[4],
      fromClause     => $_[5],
      whereClause    => $_[6],
      groupClause    => $_[7],
      havingClause   => $_[8],
      windowClause   => $_[9],
   );
}
### simple_select_2: values_clause
sub got_simple_select_2 { $_[1] }
# same as SELECT * FROM relation_expr
### simple_select_3: TABLE relation_expr
sub got_simple_select_3 {
   return SQL::Translator::Statement::Select->new(
      targetList => $_[0]->lappend(
         SQL::Translator::Statement::ResultTarget->new(
            name        => NULL,
            indirection => NIL,
            location    => NULL,
            val         => SQL::Translator::Statement::Column::Reference->new(
               fields   => $_[0]->lappend( SQL::Translator::Statement::A_Star->new() ),
               location => NULL,
            ),
         ),
      ),
      fromClause => $_[0]->lappend($_[2]),
   );
}
### simple_select_4: select_clause UNION opt_all select_clause
sub got_simple_select_4 { $_[0]->makeSetOp(SETOP_UNION,     $_[3], $_[1], $_[4]) }
### simple_select_5: select_clause INTERSECT opt_all select_clause
sub got_simple_select_5 { $_[0]->makeSetOp(SETOP_INTERSECT, $_[3], $_[1], $_[4]) }
### simple_select_6: select_clause EXCEPT opt_all select_clause
sub got_simple_select_6 { $_[0]->makeSetOp(SETOP_EXCEPT,    $_[3], $_[1], $_[4]) }

### with_clause_1: WITH cte_list
sub got_with_clause_1 {
   return SQL::Translator::Statement::WithClause->new(
      ctes      => $_[2],
      recursive => FALSE,
      location  => $_[0]->YYLLoc($_[1], 1),
   );
}
### with_clause_2: WITH RECURSIVE cte_list
sub got_with_clause_2 {
   return SQL::Translator::Statement::WithClause->new(
      ctes      => $_[3],
      recursive => TRUE,
      location  => $_[0]->YYLLoc($_[1], 1),
   );
}

### cte_list_1: common_table_expr
sub got_cte_list_1 { $_[0]->lappend($_[1]) }
### cte_list_2: cte_list <COMMA> common_table_expr
sub got_cte_list_2 { $_[0]->lappend($_[1], $_[3]) }

### common_table_expr: name opt_name_list AS <LPAREN> PreparableStmt <RPAREN>
sub got_common_table_expr {
   return SQL::Translator::Statement::CommonTableExpr->new(
      ctename       => $_[1],
      aliascolnames => $_[2],
      ctequery      => $_[5],
      location      => $_[0]->YYLLoc($_[1], 1),
   );
}

### opt_with_clause_1: with_clause
sub got_opt_with_clause_1 { $_[1] }

### into_clause_1: INTO OptTempTableName
sub got_into_clause_1 {
   return SQL::Translator::Statement::IntoClause->new(
      rel            => $_[2],
      colNames       => NIL,
      options        => NIL,
      onCommit       => ONCOMMIT_NOOP,
      tableSpaceName => NULL,
      skipData       => FALSE,
   );
}

### OptTempTableName_1: TEMPORARY opt_table qualified_name
sub got_OptTempTableName_1 {
   $_[3]->relpersistence(RELPERSISTENCE_TEMP);
   return $_[3];
}
### OptTempTableName_2: TEMP opt_table qualified_name
sub got_OptTempTableName_2 {
   $_[3]->relpersistence(RELPERSISTENCE_TEMP);
   return $_[3];
}
### OptTempTableName_3: LOCAL TEMPORARY opt_table qualified_name
sub got_OptTempTableName_3 {
   $_[4]->relpersistence(RELPERSISTENCE_TEMP);
   return $_[4];
}
### OptTempTableName_4: LOCAL TEMP opt_table qualified_name
sub got_OptTempTableName_4 {
   $_[4]->relpersistence(RELPERSISTENCE_TEMP);
   return $_[4];
}
### OptTempTableName_5: GLOBAL TEMPORARY opt_table qualified_name
sub got_OptTempTableName_5 {
   $_[4]->relpersistence(RELPERSISTENCE_TEMP);
   return $_[4];
}
### OptTempTableName_6: GLOBAL TEMP opt_table qualified_name
sub got_OptTempTableName_6 {
   $_[4]->relpersistence(RELPERSISTENCE_TEMP);
   return $_[4];
}
### OptTempTableName_7: UNLOGGED opt_table qualified_name
sub got_OptTempTableName_7 {
   $_[3]->relpersistence(RELPERSISTENCE_UNLOGGED);
   return $_[3];
}
### OptTempTableName_8: TABLE qualified_name
sub got_OptTempTableName_8 {
   $_[2]->relpersistence(RELPERSISTENCE_PERMANENT);
   return $_[2];
}
### OptTempTableName_9: qualified_name
sub got_OptTempTableName_9 {
   $_[1]->relpersistence(RELPERSISTENCE_PERMANENT);
   return $_[1];
}

### opt_table_1: TABLE
sub got_opt_table_1 {}

### opt_all_1: ALL
sub got_opt_all_1 { TRUE  }
### opt_all_2: DISTINCT
sub got_opt_all_2 { FALSE }

### opt_distinct_1: DISTINCT
sub got_opt_distinct_1 { $_[0]->lappend(NIL) }
### opt_distinct_2: DISTINCT ON <LPAREN> expr_list <RPAREN>
sub got_opt_distinct_2 { $_[4] }
### opt_distinct_3: ALL
sub got_opt_distinct_3 { NIL   }

### opt_sort_clause_1: sort_clause
sub got_opt_sort_clause_1 { $_[1] }

### sort_clause: ORDER BY sortby_list
sub got_sort_clause { $_[3] }

### sortby_list_1: sortby
sub got_sortby_list_1 { $_[0]->lappend($_[1]) }
### sortby_list_2: sortby_list <COMMA> sortby
sub got_sortby_list_2 { $_[0]->lappend($_[1], $_[3]) }

### sortby_1: a_expr USING qual_all_Op opt_nulls_order
sub got_sortby_1 {
   return SQL::Translator::Statement::SortBy->new(
      node         => $_[1],
      sortby_dir   => SORTBY_USING,
      sortby_nulls => $_[4],
      useOp        => $_[3],
      location     => $_[0]->YYLLoc($_[3], 3),
   );
}
### sortby_2: a_expr opt_asc_desc opt_nulls_order
sub got_sortby_2 {
   return SQL::Translator::Statement::SortBy->new(
      node         => $_[1],
      sortby_dir   => $_[2],
      sortby_nulls => $_[3],
      useOp        => NIL,
      location     => NULL,  #* no operator
   );
}

### select_limit_1: limit_clause offset_clause
sub got_select_limit_1 { $_[0]->lappend($_[2], $_[1]) }
### select_limit_2: offset_clause limit_clause
sub got_select_limit_2 { $_[0]->lappend($_[1], $_[2]) }
# Changed to support LimitYX
### select_limit_3: limit_clause
sub got_select_limit_3 { ref $_[1] eq 'ARRAY' ? $_[1] : $_[0]->lappend(NULL, $_[1]) }
### select_limit_4: offset_clause
sub got_select_limit_4 { $_[0]->lappend($_[1], NULL) }

### opt_select_limit_1: select_limit
sub got_opt_select_limit_1 { $_[1] }

### limit_clause_1: LIMIT select_limit_value
sub got_limit_clause_1 { $_[2] }
# SQL:2008 syntax
### limit_clause_2: LIMIT select_limit_value <COMMA> select_offset_value 
# Disabled because it was too confusing, bjm 2002-02-18
    
## SQLite supports this format, so we'll go ahead and re-enable this.
## This would be 'LimitYX' in SQL::Abstract::Limit.
sub got_limit_clause_2 {
   $_[0]->lappend($_[1], $_[3]);
}
### limit_clause_3: FETCH first_or_next opt_select_fetch_first_value row_or_rows ONLY
sub got_limit_clause_3 { $_[3] }

# SQL:2008 syntax
### offset_clause_1: OFFSET select_offset_value
sub got_offset_clause_1 { $_[2] }
### offset_clause_2: OFFSET select_offset_value2 row_or_rows
sub got_offset_clause_2 { $_[2] }

# LIMIT ALL is represented as a NULL constant
### select_limit_value_1: a_expr
sub got_select_limit_value_1 { $_[1] }
### select_limit_value_2: ALL
sub got_select_limit_value_2 { $_[0]->makeNullAConst($_[0]->YYLLoc($_[1], 1)) }

### select_offset_value: a_expr
sub got_select_offset_value { $_[1] }

### opt_select_fetch_first_value_1: SignedIconst
sub got_opt_select_fetch_first_value_1 { $_[0]->makeIntConst($_[1], $_[0]->YYLLoc($_[1], 1)) }
### opt_select_fetch_first_value_2: <LPAREN> a_expr <RPAREN>
sub got_opt_select_fetch_first_value_2 { $_[2] }

### select_offset_value2: c_expr
sub got_select_offset_value2 { $_[1] }

### row_or_rows_1: ROW
sub got_row_or_rows_1 { 0 }
### row_or_rows_2: ROWS
sub got_row_or_rows_2 { 0 }

### first_or_next_1: FIRST
sub got_first_or_next_1 { 0 }
### first_or_next_2: NEXT
sub got_first_or_next_2 { 0 }

### group_clause_1: GROUP BY expr_list
sub got_group_clause_1 { $_[3] }

### having_clause_1: HAVING a_expr
sub got_having_clause_1 { $_[2] }

### for_locking_clause_1: for_locking_items
sub got_for_locking_clause_1 { $_[1] }
### for_locking_clause_2: FOR READ ONLY
sub got_for_locking_clause_2 { NIL   }

### opt_for_locking_clause_1: for_locking_clause
sub got_opt_for_locking_clause_1 { $_[1] }

### for_locking_items_1: for_locking_item
sub got_for_locking_items_1 { $_[0]->lappend($_[1]) }
### for_locking_items_2: for_locking_items for_locking_item
sub got_for_locking_items_2 { $_[0]->lappend($_[1], $_[2]) }

### for_locking_item_1: FOR UPDATE locked_rels_list opt_nowait
sub got_for_locking_item_1 {
   return SQL::Translator::Statement::LockingClause->new(
      lockedRels => $_[3],
      forUpdate  => TRUE,
      noWait     => $_[4],
   );
}
### for_locking_item_2: FOR SHARE locked_rels_list opt_nowait
sub got_for_locking_item_2 {
   return SQL::Translator::Statement::LockingClause->new(
      lockedRels => $_[3],
      forUpdate  => FALSE,
      noWait     => $_[4],
   );
}

### locked_rels_list_1: OF qualified_name_list
sub got_locked_rels_list_1 { $_[2] }

### values_clause_1: VALUES ctext_row
sub got_values_clause_1 {
   return SQL::Translator::Statement::Select->new(
      valuesLists => $_[0]->lappend($_[2]),
   );
}
### values_clause_2: values_clause <COMMA> ctext_row
sub got_values_clause_2 {
   $_[1]->valuesLists( $_[0]->lappend($_[1]->valuesLists, $_[3]) );
   return $_[1];
}

### from_clause_1: FROM from_list
sub got_from_clause_1 { $_[2] }

### from_list_1: table_ref
sub got_from_list_1 { $_[0]->lappend($_[1]) }
### from_list_2: from_list <COMMA> table_ref
sub got_from_list_2 { $_[0]->lappend($_[1], $_[3]) }

### table_ref_1 : relation_expr
sub got_table_ref_1  { $_[1] }
### table_ref_2 : relation_expr alias_clause
sub got_table_ref_2  {
   $_[1]->alias($_[2]);
   return $_[1];
}
### table_ref_3 : func_table
sub got_table_ref_3  {
   return SQL::Translator::Statement::Range::Function->new(
      funccallnode => $_[1],
      coldeflist   => NIL,
   );
}
### table_ref_4 : func_table alias_clause
sub got_table_ref_4  {
   return SQL::Translator::Statement::Range::Function->new(
      funccallnode => $_[1],
      alias        => $_[2],
      coldeflist   => NIL,
   );
}
### table_ref_5 : func_table AS <LPAREN> TableFuncElementList <RPAREN>
sub got_table_ref_5  {
   return SQL::Translator::Statement::Range::Function->new(
      funccallnode => $_[1],
      coldeflist   => $_[4],
   );
}
### table_ref_6 : func_table AS ColId <LPAREN> TableFuncElementList <RPAREN>
sub got_table_ref_6  {
   return SQL::Translator::Statement::Range::Function->new(
      funccallnode => $_[1],
      alias => SQL::Translator::Statement::Alias->new(
         aliasname => $_[3]
      ),
      coldeflist   => $_[5],
   );
}
# The SQL spec does not permit a subselect
# (<derived_table>) without an alias clause,
# so we don't either.  This avoids the problem
# of needing to invent a unique refname for it.
# That could be surmounted if there's sufficient
# popular demand, but for now let's just implement
# the spec and see if anyone complains.
# However, it does seem like a good idea to emit
# an error message that's better than "syntax error".
### table_ref_7 : func_table ColId <LPAREN> TableFuncElementList <RPAREN>
sub got_table_ref_7  {
   return SQL::Translator::Statement::Range::Function->new(
      funccallnode => $_[1],
      alias => SQL::Translator::Statement::Alias->new(
         aliasname => $_[2]
      ),
      coldeflist   => $_[4],
   );
}
### table_ref_8 : select_with_parens
sub got_table_ref_8  {
   $_[0]->ereport(ERROR,
         ERRCODE_SYNTAX_ERROR,
         ($_[1]->isa('SQL::Translator::Statement::Select') && $_[1]->valuesLists ? (
            "VALUES in FROM must have an alias",
            "For example, FROM (VALUES ...) [AS] foo.",
         ) : (
            "Subquery in FROM must have an alias",
            "For example, FROM (SELECT ...) [AS] foo.",
         )),
         $_[0]->YYLLoc($_[1], 1));
   return undef;
}
### table_ref_9 : select_with_parens alias_clause
sub got_table_ref_9  {
   return SQL::Translator::Statement::Range::SubSelect->new(
      subquery => $_[1],
      alias => $_[2],
   );
}
### table_ref_10: joined_table
sub got_table_ref_10 { $_[1] }
### table_ref_11: <LPAREN> joined_table <RPAREN> alias_clause
sub got_table_ref_11 {
   $_[2]->alias($_[4]);
   return $_[2];
}

### joined_table_1: <LPAREN> joined_table <RPAREN>
sub got_joined_table_1 { $_[2] }
# CROSS JOIN is same as unqualified inner join
### joined_table_2: table_ref CROSS JOIN table_ref
sub got_joined_table_2 {
   return SQL::Translator::Statement::JoinExpr->new(
      jointype    => JOIN_INNER,
      isNatural   => FALSE,
      larg        => $_[1],
      rarg        => $_[4],
      usingClause => NIL,
      quals       => NULL,
   );
}
### joined_table_3: table_ref join_type JOIN table_ref join_qual
sub got_joined_table_3 {
   return SQL::Translator::Statement::JoinExpr->new(
      jointype  => $_[2],
      isNatural => FALSE,
      larg      => $_[1],
      rarg      => $_[4],
      (defined $_[5] && $_[5]->isa('SQL::Translator::Statement::List')) ? (
         usingClause => $_[5],  #* USING clause
      ) : (
         quals       => $_[5],  #* ON clause
      ),
   );
}
# letting join_type reduce to empty doesn't work
### joined_table_4: table_ref JOIN table_ref join_qual
sub got_joined_table_4 {
   return SQL::Translator::Statement::JoinExpr->new(
      jointype  => JOIN_INNER,
      isNatural => FALSE,
      larg      => $_[1],
      rarg      => $_[3],
      (defined $_[4] && $_[4]->isa('SQL::Translator::Statement::List')) ? (
         usingClause => $_[4],  #* USING clause
      ) : (
         quals       => $_[4],  #* ON clause
      ),
   );
}
### joined_table_5: table_ref NATURAL join_type JOIN table_ref
sub got_joined_table_5 {
   return SQL::Translator::Statement::JoinExpr->new(
      jointype    => $_[3],
      isNatural   => TRUE,
      larg        => $_[1],
      rarg        => $_[5],
      usingClause => NIL,   #* figure out which columns later...
      quals       => NULL,  #* fill later
   );
}
# letting join_type reduce to empty doesn't work
### joined_table_6: table_ref NATURAL JOIN table_ref
sub got_joined_table_6 {
   return SQL::Translator::Statement::JoinExpr->new(
      jointype    => JOIN_INNER,
      isNatural   => TRUE,
      larg        => $_[1],
      rarg        => $_[4],
      usingClause => NIL,   #* figure out which columns later...
      quals       => NULL,  #* fill later
   );
}

### alias_clause_1: AS ColId <LPAREN> name_list <RPAREN>
sub got_alias_clause_1 {
   return SQL::Translator::Statement::Alias->new(
      aliasname => $_[2],
      colnames  => $_[4],
   );
}
### alias_clause_2: AS ColId
sub got_alias_clause_2 {
   return SQL::Translator::Statement::Alias->new(
      aliasname => $_[2],
   );
}
### alias_clause_3: ColId <LPAREN> name_list <RPAREN>
sub got_alias_clause_3 {
   return SQL::Translator::Statement::Alias->new(
      aliasname => $_[1],
      colnames  => $_[3],
   );
}
### alias_clause_4: ColId
sub got_alias_clause_4 {
   return SQL::Translator::Statement::Alias->new(
      aliasname => $_[1],
   );
}

### join_type_1: FULL join_outer
sub got_join_type_1 { JOIN_FULL }
### join_type_2: LEFT join_outer
sub got_join_type_2 { JOIN_LEFT }
### join_type_3: RIGHT join_outer
sub got_join_type_3 { JOIN_RIGHT }
### join_type_4: INNER
sub got_join_type_4 { JOIN_INNER }

### join_outer_1: OUTER
sub got_join_outer_1 { NULL }

### join_qual_1: USING <LPAREN> name_list <RPAREN>
sub got_join_qual_1 { $_[3] }
### join_qual_2: ON a_expr
sub got_join_qual_2 { $_[2] }

# default inheritance
### relation_expr_1: qualified_name
sub got_relation_expr_1 {
   $_[1]->inhOpt(INH_DEFAULT);
   $_[1]->alias(NULL);
   return $_[1];
}
# inheritance query
### relation_expr_2: qualified_name <STAR>
sub got_relation_expr_2 {
   $_[1]->inhOpt(INH_YES);
   $_[1]->alias(NULL);
   return $_[1];
}
# no inheritance
### relation_expr_3: ONLY qualified_name
sub got_relation_expr_3 {
   $_[2]->inhOpt(INH_NO);
   $_[2]->alias(NULL);
   return $_[2];
}
# no inheritance, SQL99-style syntax
### relation_expr_4: ONLY <LPAREN> qualified_name <RPAREN>
sub got_relation_expr_4 {
   $_[3]->inhOpt(INH_NO);
   $_[3]->alias(NULL);
   return $_[3];
}

### relation_expr_list_1: relation_expr
sub got_relation_expr_list_1 { $_[0]->lappend($_[1]) }
### relation_expr_list_2: relation_expr_list <COMMA> relation_expr
sub got_relation_expr_list_2 { $_[0]->lappend($_[1], $_[3]) }

### relation_expr_opt_alias_1: relation_expr
sub got_relation_expr_opt_alias_1 { $_[1] }
### relation_expr_opt_alias_2: relation_expr ColId
sub got_relation_expr_opt_alias_2 {
   my $alias = SQL::Translator::Statement::Alias->new(
      aliasname => $_[2],
   );
   $_[1]->alias($alias);
   return $_[1];
}
### relation_expr_opt_alias_3: relation_expr AS ColId
sub got_relation_expr_opt_alias_3 {
   my $alias = SQL::Translator::Statement::Alias->new(
      aliasname => $_[3],
   );
   $_[1]->alias($alias);
   return $_[1];
}

### func_table: func_expr
sub got_func_table { $_[1] }

### where_clause_1: WHERE a_expr
sub got_where_clause_1 { $_[2] }

### where_or_current_clause_1: WHERE a_expr
sub got_where_or_current_clause_1 { $_[2] }
### where_or_current_clause_2: WHERE CURRENT OF cursor_name
sub got_where_or_current_clause_2 {
   return SQL::Translator::Statement::CurrentOfExpr->new(
      #* cvarno is filled in by parse analysis
      cursor_name  => $_[4],
      cursor_param => 0,
   );
}

### OptTableFuncElementList_1: TableFuncElementList
sub got_OptTableFuncElementList_1 { $_[1] }

### TableFuncElementList_1: TableFuncElement
sub got_TableFuncElementList_1 { $_[0]->lappend($_[1])        }
### TableFuncElementList_2: TableFuncElementList <COMMA> TableFuncElement
sub got_TableFuncElementList_2 { $_[0]->lappend($_[1], $_[3]) }

### TableFuncElement: ColId Typename opt_collate_clause
sub got_TableFuncElement {
   return SQL::Translator::Statement::Column::Definition->new(
      colname        => $_[1],
      typeName       => $_[2],
      inhcount       => 0,
      is_local       => TRUE,
      is_not_null    => FALSE,
      is_from_type   => FALSE,
      storage        => 0,
      raw_default    => NULL,
      cooked_default => NULL,
      collClause     => $_[3],
      collOid        => InvalidOid,
      constraints    => NIL,
   );
}

### Typename_1: SimpleTypename opt_array_bounds
sub got_Typename_1 {
   $_[1]->arrayBounds($_[2]);
   return $_[1];
}
# SQL standard syntax, currently only one-dimensional
### Typename_2: SETOF SimpleTypename opt_array_bounds
sub got_Typename_2 {
   $_[2]->arrayBounds($_[3]);
   $_[2]->setof(TRUE);
   return $_[2];
}
### Typename_3: SimpleTypename ARRAY <LSQUARE> Iconst <RSQUARE>
sub got_Typename_3 {
   $_[1]->arrayBounds( $_[0]->lappend($_[4]+0) );
   return $_[1];
}
### Typename_4: SETOF SimpleTypename ARRAY <LSQUARE> Iconst <RSQUARE>
sub got_Typename_4 {
   $_[2]->arrayBounds( $_[0]->lappend($_[5]+0) );
   $_[2]->setof(TRUE);
   return $_[2];
}
### Typename_5: SimpleTypename ARRAY
sub got_Typename_5 {
   $_[1]->arrayBounds( $_[0]->lappend(-1) );
   return $_[1];
}
### Typename_6: SETOF SimpleTypename ARRAY
sub got_Typename_6 {
   $_[2]->arrayBounds( $_[0]->lappend(-1) );
   $_[2]->setof(TRUE);
   return $_[2];
}

### opt_array_bounds_1: opt_array_bounds <LSQUARE> <RSQUARE>
sub got_opt_array_bounds_1 { $_[0]->lappend($_[1], -1)      }
### opt_array_bounds_2: opt_array_bounds <LSQUARE> Iconst <RSQUARE>
sub got_opt_array_bounds_2 { $_[0]->lappend($_[1], $_[3]+0) }

### SimpleTypename_1: GenericType
sub got_SimpleTypename_1 { $_[1] }
### SimpleTypename_2: Numeric
sub got_SimpleTypename_2 { $_[1] }
### SimpleTypename_3: Bit
sub got_SimpleTypename_3 { $_[1] }
### SimpleTypename_4: Character
sub got_SimpleTypename_4 { $_[1] }
### SimpleTypename_5: ConstDatetime
sub got_SimpleTypename_5 { $_[1] }
### SimpleTypename_6: ConstInterval opt_interval
sub got_SimpleTypename_6 {
   $_[1]->typmods($_[2]);
   return $_[1];
}
### SimpleTypename_7: ConstInterval <LPAREN> Iconst <RPAREN> opt_interval
sub got_SimpleTypename_7 {
   if (defined $_[5])  {
      $_[0]->ereport(ERROR,
            ERRCODE_SYNTAX_ERROR,
             "Interval precision specified twice",
             $_[0]->YYLLoc($_[1], 1)) if (@{$_[5]} > 1);
      $_[1]->typmods( $_[0]->lappend($_[5], $_[0]->makeIntConst($_[3], $_[0]->YYLLoc($_[3], 3))) );
   }
   else {
      $_[1]->typmods( $_[0]->lappend($_[0]->makeIntConst(INTERVAL_FULL_RANGE, {}), $_[0]->makeIntConst($_[3], $_[0]->YYLLoc($_[3], 3))) );
   }
   return $_[1];
}

### ConstTypename_1: Numeric
sub got_ConstTypename_1 { $_[1] }
### ConstTypename_2: ConstBit
sub got_ConstTypename_2 { $_[1] }
### ConstTypename_3: ConstCharacter
sub got_ConstTypename_3 { $_[1] }
### ConstTypename_4: ConstDatetime
sub got_ConstTypename_4 { $_[1] }

### GenericType_1: type_function_name opt_type_modifiers
sub got_GenericType_1 {
   my $n = $_[0]->makeTypeNameFromNameList([ $_[1] ]);
   $n->typmods($_[2]);
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}
### GenericType_2: type_function_name attrs opt_type_modifiers
sub got_GenericType_2 {
   my $n = $_[0]->makeTypeNameFromNameList($_[0]->lcons($_[1], $_[2]));
   $n->typmods($_[3]);
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}

### opt_type_modifiers_1: <LPAREN> expr_list <RPAREN>
sub got_opt_type_modifiers_1 { $_[2] }

### Numeric_1 : INT
sub got_Numeric_1  {
   my $n = $_[0]->SystemTypeName("int4");
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}
### Numeric_2 : INTEGER
sub got_Numeric_2  {
   my $n = $_[0]->SystemTypeName("int4");
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}
### Numeric_3 : SMALLINT
sub got_Numeric_3  {
   my $n = $_[0]->SystemTypeName("int2");
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}
### Numeric_4 : BIGINT
sub got_Numeric_4  {
   my $n = $_[0]->SystemTypeName("int8");
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}
### Numeric_5 : REAL
sub got_Numeric_5  {
   my $n = $_[0]->SystemTypeName("float4");
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}
### Numeric_6 : FLOAT opt_float
sub got_Numeric_6  {
   $_[2]->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $_[2];
}
### Numeric_7 : DOUBLE PRECISION
sub got_Numeric_7  {
   my $n = $_[0]->SystemTypeName("float8");
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}
### Numeric_8 : DECIMAL opt_type_modifiers
sub got_Numeric_8  {
   my $n = $_[0]->SystemTypeName("numeric");
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   $n->typmods($_[2]);
   return $n;
}
### Numeric_9 : DEC opt_type_modifiers
sub got_Numeric_9  {
   my $n = $_[0]->SystemTypeName("numeric");
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   $n->typmods($_[2]);
   return $n;
}
### Numeric_10: NUMERIC opt_type_modifiers
sub got_Numeric_10 {
   my $n = $_[0]->SystemTypeName("numeric");
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   $n->typmods($_[2]);
   return $n;
}
### Numeric_11: BOOLEAN
sub got_Numeric_11 {
   my $n = $_[0]->SystemTypeName("bool");
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}

### opt_float_1: <LPAREN> Iconst <RPAREN>
sub got_opt_float_1 {
   if ($_[2] < 1) {
      $_[0]->ereport(ERROR,
            ERRCODE_INVALID_PARAMETER_VALUE,
             "precision for type float must be at least 1 bit",
             $_[0]->YYLLoc($_[2], 2));
   }
   elsif ($_[2] <= 24) { $_[0]->SystemTypeName("float4") }
   elsif ($_[2] <= 53) { $_[0]->SystemTypeName("float8") }
   else {
      $_[0]->ereport(ERROR,
            ERRCODE_INVALID_PARAMETER_VALUE,
             "precision for type float must be less than 54 bits",
             $_[0]->YYLLoc($_[2], 2));
   }
}

### Bit_1: BitWithLength
sub got_Bit_1 { $_[1] }
### Bit_2: BitWithoutLength
sub got_Bit_2 { $_[1] }

### ConstBit_1: BitWithLength
sub got_ConstBit_1 { $_[1] }
### ConstBit_2: BitWithoutLength
sub got_ConstBit_2 { $_[1]->typmods(NIL); $_[1]; }

### BitWithLength: BIT opt_varying <LPAREN> expr_list <RPAREN>
sub got_BitWithLength {
   my $n = $_[0]->SystemTypeName($_[2] ? 'varbit' : 'bit');
   $n->typmods($_[4]);
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}

### BitWithoutLength: BIT opt_varying
sub got_BitWithoutLength {
   #* bit defaults to bit(1), varbit to no limit
   if ($_[2]) { return $_[0]->SystemTypeName("varbit"); }
   else {
      my $n = $_[0]->SystemTypeName("bit");
      $n->typmods ( $_[0]->lappend( $_[0]->makeIntConst(1, {}) ) );
      $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
      return $n;
   }
}

### Character_1: CharacterWithLength
sub got_Character_1 { $_[1] }
### Character_2: CharacterWithoutLength
sub got_Character_2 { $_[1] }

# Length was not specified so allow to be unrestricted.
# This handles problems with fixed-length (bpchar) strings
# which in column definitions must default to a length
# of one, but should not be constrained if the length
# was not specified.
### ConstCharacter_1: CharacterWithLength
sub got_ConstCharacter_1 { $_[1] }
### ConstCharacter_2: CharacterWithoutLength
sub got_ConstCharacter_2 {
   $_[1]->typmods(NIL);
   return $_[1];
}

### CharacterWithLength: character <LPAREN> Iconst <RPAREN> opt_charset
sub got_CharacterWithLength {
   $_[1] .= '_'.$_[5]
      if (defined $_[5] && $_[5] eq "sql_text");

   my $n = $_[0]->SystemTypeName($_[1]);
   $n->typmods($_[0]->lappend($_[0]->makeIntConst($_[3], $_[0]->YYLLoc($_[3], 3))));
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}

### CharacterWithoutLength: character opt_charset
sub got_CharacterWithoutLength {
   $_[1] .= '_'.$_[2]
      if (defined $_[2] && $_[2] eq "sql_text");

   my $n = $_[0]->SystemTypeName($_[1]);

   #* char defaults to char(1), varchar to no limit
   $n->typmods($_[0]->lappend($_[0]->makeIntConst(1, {})))
      if ($_[1] eq "bpchar");

   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}

### character_1: CHARACTER opt_varying
sub got_character_1 { $_[2] ? "varchar": "bpchar" }
### character_2: CHAR opt_varying
sub got_character_2 { $_[2] ? "varchar": "bpchar" }
### character_3: VARCHAR
sub got_character_3 { "varchar" }
### character_4: NATIONAL CHARACTER opt_varying
sub got_character_4 { $_[3] ? "varchar": "bpchar" }
### character_5: NATIONAL CHAR opt_varying
sub got_character_5 { $_[3] ? "varchar": "bpchar" }
### character_6: NCHAR opt_varying
sub got_character_6 { $_[2] ? "varchar": "bpchar" }

### opt_varying_1: VARYING
sub got_opt_varying_1 { TRUE  }

### opt_charset_1: CHARACTER SET ColId
sub got_opt_charset_1 { $_[3] }

### ConstDatetime_1: TIMESTAMP <LPAREN> Iconst <RPAREN> opt_timezone
sub got_ConstDatetime_1 {
   my $n = $_[0]->SystemTypeName('timestamp'.($_[5] ? 'tz' : ''));
   $n->typmods($_[0]->lappend($_[0]->makeIntConst($_[3], $_[0]->YYLLoc($_[3], 3))));
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}
### ConstDatetime_2: TIMESTAMP opt_timezone
sub got_ConstDatetime_2 {
   my $n = $_[0]->SystemTypeName('timestamp'.($_[2] ? 'tz' : ''));
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}
### ConstDatetime_3: TIME <LPAREN> Iconst <RPAREN> opt_timezone
sub got_ConstDatetime_3 {
   my $n = $_[0]->SystemTypeName('time'.($_[5] ? 'tz' : ''));
   $n->typmods($_[0]->lappend($_[0]->makeIntConst($_[3], $_[0]->YYLLoc($_[3], 3))));
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}
### ConstDatetime_4: TIME opt_timezone
sub got_ConstDatetime_4 {
   my $n = $_[0]->SystemTypeName('time'.($_[2] ? 'tz' : ''));
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}

### ConstInterval: INTERVAL
sub got_ConstInterval {
   my $n = $_[0]->SystemTypeName("interval");
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}

### opt_timezone_1: WITH_TIME ZONE
sub got_opt_timezone_1 { TRUE  }
### opt_timezone_2: WITHOUT TIME ZONE
sub got_opt_timezone_2 { FALSE }

### opt_interval_1 : YEAR
sub got_opt_interval_1  { $_[0]->lappend( $_[0]->makeIntConst(INTERVAL_MASK_YEAR,   $_[0]->YYLLoc($_[1], 1))) }
### opt_interval_2 : MONTH
sub got_opt_interval_2  { $_[0]->lappend( $_[0]->makeIntConst(INTERVAL_MASK_MONTH,  $_[0]->YYLLoc($_[1], 1))) }
### opt_interval_3 : DAY
sub got_opt_interval_3  { $_[0]->lappend( $_[0]->makeIntConst(INTERVAL_MASK_DAY,    $_[0]->YYLLoc($_[1], 1))) }
### opt_interval_4 : HOUR
sub got_opt_interval_4  { $_[0]->lappend( $_[0]->makeIntConst(INTERVAL_MASK_HOUR,   $_[0]->YYLLoc($_[1], 1))) }
### opt_interval_5 : MINUTE
sub got_opt_interval_5  { $_[0]->lappend( $_[0]->makeIntConst(INTERVAL_MASK_MINUTE, $_[0]->YYLLoc($_[1], 1))) }
### opt_interval_6 : interval_second
sub got_opt_interval_6  { $_[1] }
### opt_interval_7 : YEAR TO MONTH
sub got_opt_interval_7  { $_[0]->lappend($_[0]->makeIntConst(INTERVAL_MASK_YEAR | INTERVAL_MASK_MONTH, $_[0]->YYLLoc($_[1], 1))) }
### opt_interval_8 : DAY TO HOUR
sub got_opt_interval_8  { $_[0]->lappend($_[0]->makeIntConst(INTERVAL_MASK_DAY  | INTERVAL_MASK_HOUR,  $_[0]->YYLLoc($_[1], 1))) }
### opt_interval_9 : DAY TO MINUTE
sub got_opt_interval_9  { $_[0]->lappend($_[0]->makeIntConst(INTERVAL_MASK_DAY  | INTERVAL_MASK_HOUR | INTERVAL_MASK_MINUTE, $_[0]->YYLLoc($_[1], 1))) }
### opt_interval_10: DAY TO interval_second
sub got_opt_interval_10 {
   $_[3]->[0] = $_[0]->makeIntConst(INTERVAL_MASK_DAY | INTERVAL_MASK_HOUR | INTERVAL_MASK_MINUTE | INTERVAL_MASK_SECOND, $_[0]->YYLLoc($_[1], 1));
   return $_[3];
}
### opt_interval_11: HOUR TO MINUTE
sub got_opt_interval_11 { $_[0]->lappend($_[0]->makeIntConst(INTERVAL_MASK_HOUR | INTERVAL_MASK_MINUTE, $_[0]->YYLLoc($_[1], 1))) }
### opt_interval_12: HOUR TO interval_second
sub got_opt_interval_12 {
   $_[3]->[0] = $_[0]->makeIntConst(INTERVAL_MASK_HOUR | INTERVAL_MASK_MINUTE | INTERVAL_MASK_SECOND, $_[0]->YYLLoc($_[1], 1));
   return $_[3];
}
### opt_interval_13: MINUTE TO interval_second
sub got_opt_interval_13 {
   $_[3]->[0] = $_[0]->makeIntConst(INTERVAL_MASK_MINUTE | INTERVAL_MASK_SECOND, $_[0]->YYLLoc($_[1], 1));
   return $_[3];
}

### interval_second_1: SECOND
sub got_interval_second_1 { $_[0]->lappend($_[0]->makeIntConst(INTERVAL_MASK_SECOND, $_[0]->YYLLoc($_[1], 1))) }
### interval_second_2: SECOND <LPAREN> Iconst <RPAREN>
sub got_interval_second_2 { $_[0]->lappend($_[0]->makeIntConst(INTERVAL_MASK_SECOND, $_[0]->YYLLoc($_[1], 1)), $_[0]->makeIntConst($_[3], $_[0]->YYLLoc($_[3], 3))) }

### a_expr_1 : c_expr
sub got_a_expr_1  { $_[1] }
### a_expr_2 : a_expr TYPECAST Typename
sub got_a_expr_2  { $_[0]->makeTypeCast($_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
### a_expr_3 : a_expr COLLATE any_name
sub got_a_expr_3  {
   return SQL::Translator::Statement::CollateClause->new(
      arg           => $_[1],
      collname      => $_[3],
      location      => $_[0]->YYLLoc($_[2], 2),
   );
}
# These operators must be called out explicitly in order to make use
# of bison's automatic operator-precedence handling.  All other
# operator names are handled by the generic productions using "Op",
# below; and all those operators will have the same precedence.
#
# If you add more explicitly-known operators, be sure to add them
# also to b_expr and to the MathOp list above.
### a_expr_4 : a_expr AT TIME ZONE a_expr
sub got_a_expr_4  {
   return SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[0]->SystemFuncName("timezone"),
      args          => $_[0]->lappend($_[5], $_[1]),
      agg_order     => NIL,
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => FALSE,
      over          => NULL,
      location      => $_[0]->YYLLoc($_[2], 2),
   );
}
### a_expr_5 : <PLUS> a_expr
sub got_a_expr_5  { $_[0]->makeSimpleA_Expr(AEXPR_OP,  "+",  NULL, $_[2], $_[0]->YYLLoc($_[1], 1)) }
### a_expr_6 : <DASH> a_expr
sub got_a_expr_6  { $_[0]->doNegate($_[2], $_[0]->YYLLoc($_[1], 1))                                }
### a_expr_7 : a_expr <PLUS> a_expr
sub got_a_expr_7  { $_[0]->makeSimpleA_Expr(AEXPR_OP,  "+", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
### a_expr_8 : a_expr <DASH> a_expr
sub got_a_expr_8  { $_[0]->makeSimpleA_Expr(AEXPR_OP,  "-", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
### a_expr_9 : a_expr <STAR> a_expr
sub got_a_expr_9  { $_[0]->makeSimpleA_Expr(AEXPR_OP,  "*", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
### a_expr_10: a_expr <SLASH> a_expr
sub got_a_expr_10 { $_[0]->makeSimpleA_Expr(AEXPR_OP,  "/", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
### a_expr_11: a_expr <PERCENT> a_expr
sub got_a_expr_11 { $_[0]->makeSimpleA_Expr(AEXPR_OP,  "%", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
### a_expr_12: a_expr <CARET> a_expr
sub got_a_expr_12 { $_[0]->makeSimpleA_Expr(AEXPR_OP,  "^", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
### a_expr_13: a_expr <LANGLE> a_expr
sub got_a_expr_13 { $_[0]->makeSimpleA_Expr(AEXPR_OP,  "<", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
### a_expr_14: a_expr <RANGLE> a_expr
sub got_a_expr_14 { $_[0]->makeSimpleA_Expr(AEXPR_OP,  ">", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
### a_expr_15: a_expr <EQUAL> a_expr
sub got_a_expr_15 { $_[0]->makeSimpleA_Expr(AEXPR_OP,  "=", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
### a_expr_16: a_expr qual_Op a_expr
sub got_a_expr_16 { $_[0]->makeA_Expr(AEXPR_OP,  $_[2], $_[1], $_[3], $_[0]->YYLLoc($_[2], 2))     }
### a_expr_17: qual_Op a_expr
sub got_a_expr_17 { $_[0]->makeA_Expr(AEXPR_OP,  $_[1],  NULL, $_[2], $_[0]->YYLLoc($_[1], 1))     }
### a_expr_18: a_expr qual_Op
sub got_a_expr_18 { $_[0]->makeA_Expr(AEXPR_OP,  $_[2], $_[1],  NULL, $_[0]->YYLLoc($_[2], 2))     }
### a_expr_19: a_expr AND a_expr
sub got_a_expr_19 { $_[0]->makeA_Expr(AEXPR_AND, NIL, $_[1], $_[3], $_[0]->YYLLoc($_[2], 2))       }
### a_expr_20: a_expr OR a_expr
sub got_a_expr_20 { $_[0]->makeA_Expr(AEXPR_OR,  NIL, $_[1], $_[3], $_[0]->YYLLoc($_[2], 2))       }
### a_expr_21: NOT a_expr
sub got_a_expr_21 { $_[0]->makeA_Expr(AEXPR_NOT, NIL,  NULL, $_[2], $_[0]->YYLLoc($_[1], 1))       }
### a_expr_22: a_expr LIKE a_expr
sub got_a_expr_22 { $_[0]->makeSimpleA_Expr(AEXPR_OP, "~~", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
### a_expr_23: a_expr LIKE a_expr ESCAPE a_expr
sub got_a_expr_23 {
   my $n = SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[0]->SystemFuncName("like_escape"),
      args          => $_[0]->lappend($_[3], $_[5]),
      agg_order     => NIL,
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => FALSE,
      over          => NULL,
      location      => $_[0]->YYLLoc($_[2], 2),
   );
   return $_[0]->makeSimpleA_Expr(AEXPR_OP, "~~", $_[1], $n, $_[0]->YYLLoc($_[2], 2));
}
### a_expr_24: a_expr NOT LIKE a_expr
sub got_a_expr_24 { $_[0]->makeSimpleA_Expr(AEXPR_OP, "!~~", $_[1], $_[4], $_[0]->YYLLoc($_[2], 2)) }
### a_expr_25: a_expr NOT LIKE a_expr ESCAPE a_expr
sub got_a_expr_25 {
   my $n = SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[0]->SystemFuncName("like_escape"),
      args          => $_[0]->lappend($_[4], $_[6]),
      agg_order     => NIL,
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => FALSE,
      over          => NULL,
      location      => $_[0]->YYLLoc($_[2], 2),
   );
   return $_[0]->makeSimpleA_Expr(AEXPR_OP, "!~~", $_[1], $n, $_[0]->YYLLoc($_[2], 2));
}
### a_expr_26: a_expr ILIKE a_expr
sub got_a_expr_26 { $_[0]->makeSimpleA_Expr(AEXPR_OP, "~~*", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
### a_expr_27: a_expr ILIKE a_expr ESCAPE a_expr
sub got_a_expr_27 {
   my $n = SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[0]->SystemFuncName("like_escape"),
      args          => $_[0]->lappend($_[3], $_[5]),
      agg_order     => NIL,
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => FALSE,
      over          => NULL,
      location      => $_[0]->YYLLoc($_[2], 2),
   );
   return $_[0]->makeSimpleA_Expr(AEXPR_OP, "~~*", $_[1], $n, $_[0]->YYLLoc($_[2], 2));
}
### a_expr_28: a_expr NOT ILIKE a_expr
sub got_a_expr_28 { $_[0]->makeSimpleA_Expr(AEXPR_OP, "!~~*", $_[1], $_[4], $_[0]->YYLLoc($_[2], 2)) }
### a_expr_29: a_expr NOT ILIKE a_expr ESCAPE a_expr
sub got_a_expr_29 {
   my $n = SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[0]->SystemFuncName("like_escape"),
      args          => $_[0]->lappend($_[4], $_[6]),
      agg_order     => NIL,
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => FALSE,
      over          => NULL,
      location      => $_[0]->YYLLoc($_[2], 2),
   );
   return $_[0]->makeSimpleA_Expr(AEXPR_OP, "!~~*", $_[1], $n, $_[0]->YYLLoc($_[2], 2));
}
### a_expr_30: a_expr SIMILAR TO a_expr
sub got_a_expr_30 {
   my $n = SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[0]->SystemFuncName("similar_escape"),
      args          => $_[0]->lappend($_[4], $_[0]->makeNullAConst({})),
      agg_order     => NIL,
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => FALSE,
      over          => NULL,
      location      => $_[0]->YYLLoc($_[2], 2),
   );
   return $_[0]->makeSimpleA_Expr(AEXPR_OP, "~", $_[1], $n, $_[0]->YYLLoc($_[2], 2));
}
### a_expr_31: a_expr SIMILAR TO a_expr ESCAPE a_expr
sub got_a_expr_31 {
   my $n = SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[0]->SystemFuncName("similar_escape"),
      args          => $_[0]->lappend($_[4], $_[6]),
      agg_order     => NIL,
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => FALSE,
      over          => NULL,
      location      => $_[0]->YYLLoc($_[2], 2),
   );
   return $_[0]->makeSimpleA_Expr(AEXPR_OP, "~", $_[1], $n, $_[0]->YYLLoc($_[2], 2));
}
### a_expr_32: a_expr NOT SIMILAR TO a_expr
sub got_a_expr_32 {
   my $n = SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[0]->SystemFuncName("similar_escape"),
      args          => $_[0]->lappend($_[5], $_[0]->makeNullAConst({})),
      agg_order     => NIL,
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => FALSE,
      over          => NULL,
      location      => $_[0]->YYLLoc($_[2], 2),
   );
   return $_[0]->makeSimpleA_Expr(AEXPR_OP, "!~", $_[1], $n, $_[0]->YYLLoc($_[2], 2));
}
# NullTest clause
# Define SQL92-style Null test clause.
# Allow two forms described in the standard:
#   a IS NULL
#   a IS NOT NULL
# Allow two SQL extensions
#   a ISNULL
#   a NOTNULL
### a_expr_33: a_expr NOT SIMILAR TO a_expr ESCAPE a_expr
sub got_a_expr_33 {
   my $n = SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[0]->SystemFuncName("similar_escape"),
      args          => $_[0]->lappend($_[5], $_[7]),
      agg_order     => NIL,
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => FALSE,
      over          => NULL,
      location      => $_[0]->YYLLoc($_[2], 2),
   );
   return $_[0]->makeSimpleA_Expr(AEXPR_OP, "!~", $_[1], $n, $_[0]->YYLLoc($_[2], 2));
}
### a_expr_34: a_expr IS NULL
sub got_a_expr_34 {
   return SQL::Translator::Statement::NullTest->new(
      arg => $_[1],
      nulltesttype => IS_NULL,
   );
}
### a_expr_35: a_expr ISNULL
sub got_a_expr_35 {
   return SQL::Translator::Statement::NullTest->new(
      arg => $_[1],
      nulltesttype => IS_NULL,
   );
}
### a_expr_36: a_expr IS NOT NULL
sub got_a_expr_36 {
   return SQL::Translator::Statement::NullTest->new(
      arg => $_[1],
      nulltesttype => IS_NOT_NULL,
   );
}
### a_expr_37: a_expr NOTNULL
sub got_a_expr_37 {
   return SQL::Translator::Statement::NullTest->new(
      arg => $_[1],
      nulltesttype => IS_NOT_NULL,
   );
}
### a_expr_38: row OVERLAPS row
sub got_a_expr_38 { $_[0]->makeOverlaps($_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
### a_expr_39: a_expr IS TRUE
sub got_a_expr_39 {
   return SQL::Translator::Statement::BooleanTest->new(
      arg          => $_[1],
      booltesttype => IS_TRUE,
   );
}
### a_expr_40: a_expr IS NOT TRUE
sub got_a_expr_40 {
   return SQL::Translator::Statement::BooleanTest->new(
      arg          => $_[1],
      booltesttype => IS_NOT_TRUE,
   );
}
### a_expr_41: a_expr IS FALSE
sub got_a_expr_41 {
   return SQL::Translator::Statement::BooleanTest->new(
      arg          => $_[1],
      booltesttype => IS_FALSE,
   );
}
### a_expr_42: a_expr IS NOT FALSE
sub got_a_expr_42 {
   return SQL::Translator::Statement::BooleanTest->new(
      arg          => $_[1],
      booltesttype => IS_NOT_FALSE,
   );
}
### a_expr_43: a_expr IS UNKNOWN
sub got_a_expr_43 {
   return SQL::Translator::Statement::BooleanTest->new(
      arg          => $_[1],
      booltesttype => IS_UNKNOWN,
   );
}
### a_expr_44: a_expr IS NOT UNKNOWN
sub got_a_expr_44 {
   return SQL::Translator::Statement::BooleanTest->new(
      arg          => $_[1],
      booltesttype => IS_NOT_UNKNOWN,
   );
}
### a_expr_45: a_expr IS DISTINCT FROM a_expr
sub got_a_expr_45 { $_[0]->makeSimpleA_Expr(AEXPR_DISTINCT, "=", $_[1], $_[5], $_[0]->YYLLoc($_[2], 2)) }
### a_expr_46: a_expr IS NOT DISTINCT FROM a_expr
sub got_a_expr_46 {
   return $_[0]->makeA_Expr(
      AEXPR_NOT, NIL, NULL,
      $_[0]->makeSimpleA_Expr(AEXPR_DISTINCT, "=", $_[1], $_[6], $_[0]->YYLLoc($_[2], 2)),
      $_[0]->YYLLoc($_[2], 2)
   );
}
### a_expr_47: a_expr IS OF <LPAREN> type_list <RPAREN>
sub got_a_expr_47 { $_[0]->makeSimpleA_Expr(AEXPR_OF,  "=", $_[1], $_[5], $_[0]->YYLLoc($_[2], 2)) }
#   Ideally we would not use hard-wired operators below but
#   instead use opclasses.  However, mixed data types and other
#   issues make this difficult:
#   http://archives.postgresql.org/pgsql-hackers/2008-08/msg01142.php
### a_expr_48: a_expr IS NOT OF <LPAREN> type_list <RPAREN>
sub got_a_expr_48 { $_[0]->makeSimpleA_Expr(AEXPR_OF, "<>", $_[1], $_[6], $_[0]->YYLLoc($_[2], 2)) }
### a_expr_49: a_expr BETWEEN opt_asymmetric b_expr AND b_expr
sub got_a_expr_49 {
   return $_[0]->makeA_Expr(
      AEXPR_AND, NIL,
      $_[0]->makeSimpleA_Expr(AEXPR_OP, ">=", $_[1], $_[4], $_[0]->YYLLoc($_[2], 2)),
      $_[0]->makeSimpleA_Expr(AEXPR_OP, "<=", $_[1], $_[6], $_[0]->YYLLoc($_[2], 2)),
      $_[0]->YYLLoc($_[2], 2)
   );
}
### a_expr_50: a_expr NOT BETWEEN opt_asymmetric b_expr AND b_expr
sub got_a_expr_50 {
   return $_[0]->makeA_Expr(
      AEXPR_OR, NIL,
      $_[0]->makeSimpleA_Expr(AEXPR_OP, "<", $_[1], $_[5], $_[0]->YYLLoc($_[2], 2)),
      $_[0]->makeSimpleA_Expr(AEXPR_OP, ">", $_[1], $_[7], $_[0]->YYLLoc($_[2], 2)),
      $_[0]->YYLLoc($_[2], 2)
   );
}
### a_expr_51: a_expr BETWEEN SYMMETRIC b_expr AND b_expr
sub got_a_expr_51 {
   return $_[0]->makeA_Expr(
      AEXPR_OR, NIL,
      $_[0]->makeA_Expr(
         AEXPR_AND, NIL,
         $_[0]->makeSimpleA_Expr(AEXPR_OP, ">=", $_[1], $_[4], $_[0]->YYLLoc($_[2], 2)),
         $_[0]->makeSimpleA_Expr(AEXPR_OP, "<=", $_[1], $_[6], $_[0]->YYLLoc($_[2], 2)),
         $_[0]->YYLLoc($_[2], 2)
      ),
      $_[0]->makeA_Expr(
         AEXPR_AND, NIL,
         $_[0]->makeSimpleA_Expr(AEXPR_OP, ">=", $_[1], $_[6], $_[0]->YYLLoc($_[2], 2)),
         $_[0]->makeSimpleA_Expr(AEXPR_OP, "<=", $_[1], $_[4], $_[0]->YYLLoc($_[2], 2)),
         $_[0]->YYLLoc($_[2], 2)
      ),
      $_[0]->YYLLoc($_[2], 2)
   );
}
### a_expr_52: a_expr NOT BETWEEN SYMMETRIC b_expr AND b_expr
sub got_a_expr_52 {
   return $_[0]->makeA_Expr(
      AEXPR_AND, NIL,
      $_[0]->makeA_Expr(
         AEXPR_OR, NIL,
         $_[0]->makeSimpleA_Expr(AEXPR_OP, "<", $_[1], $_[5], $_[0]->YYLLoc($_[2], 2)),
         $_[0]->makeSimpleA_Expr(AEXPR_OP, ">", $_[1], $_[7], $_[0]->YYLLoc($_[2], 2)),
         $_[0]->YYLLoc($_[2], 2)
      ),
      $_[0]->makeA_Expr(
         AEXPR_OR, NIL,
         $_[0]->makeSimpleA_Expr(AEXPR_OP, "<", $_[1], $_[7], $_[0]->YYLLoc($_[2], 2)),
         $_[0]->makeSimpleA_Expr(AEXPR_OP, ">", $_[1], $_[5], $_[0]->YYLLoc($_[2], 2)),
         $_[0]->YYLLoc($_[2], 2)
      ),
      $_[0]->YYLLoc($_[2], 2)
   );
}
### a_expr_53: a_expr IN in_expr
sub got_a_expr_53 {
   #* in_expr returns a SubLink or a list of a_exprs
   if ($_[3]->isa('SQL::Translator::Statement::SubLink')) {
      #* generate foo = ANY (subquery)
      $_[3]->subLinkType(ANY_SUBLINK);
      $_[3]->testexpr($_[1]);
      $_[3]->operName( $_[0]->lappend("=") );
      $_[3]->_set_location( $_[0]->YYLLoc($_[2], 2) );
      return $_[3];
   }
   else {
      #* generate scalar IN expression
      return $_[0]->makeSimpleA_Expr(AEXPR_IN, "=", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2));
   }
}
### a_expr_54: a_expr NOT IN in_expr
sub got_a_expr_54 {
   #* in_expr returns a SubLink or a list of a_exprs
   if ($_[4]->isa('SQL::Translator::Statement::SubLink')) {
      #* generate foo = ANY (subquery)
      $_[4]->subLinkType(ANY_SUBLINK);
      $_[4]->testexpr($_[1]);
      $_[4]->operName( $_[0]->lappend("=") );
      $_[4]->_set_location( $_[0]->YYLLoc($_[3], 3) );
      #* Stick a NOT on top
      return $_[0]->makeA_Expr(AEXPR_NOT, NIL, NULL, $_[4], $_[0]->YYLLoc($_[2], 2));
   }
   else {
      #* generate scalar NOT IN expression
      return $_[0]->makeSimpleA_Expr(AEXPR_IN, "<>", $_[1], $_[4], $_[0]->YYLLoc($_[2], 2));
   }
}
### a_expr_55: a_expr subquery_Op sub_type select_with_parens
sub got_a_expr_55 {
   return SQL::Translator::Statement::SubLink->new(
      subLinkType => $_[3],
      testexpr    => $_[1],
      operName    => $_[2],
      subselect   => $_[4],
      location    => $_[0]->YYLLoc($_[2], 2),
   );
}
### a_expr_56: a_expr subquery_Op sub_type <LPAREN> a_expr <RPAREN>
sub got_a_expr_56 {
   return $_[0]->makeA_Expr(
      ($_[3] eq ANY_SUBLINK ? AEXPR_OP_ANY : AEXPR_OP_ALL),
      $_[2], $_[1], $_[5], $_[0]->YYLLoc($_[2], 2)
   );
}
# Not sure how to get rid of the parentheses
# but there are lots of shift/reduce errors without them.
#
# Should be able to implement this by plopping the entire
# select into a node, then transforming the target expressions
# from whatever they are into count(*), and testing the
# entire result equal to one.
# But, will probably implement a separate node in the executor.
### a_expr_57: UNIQUE select_with_parens
sub got_a_expr_57 {
   $_[0]->ereport(ERROR,
         ERRCODE_FEATURE_NOT_SUPPORTED,
          "UNIQUE predicate is not yet implemented",
          $_[0]->YYLLoc($_[1], 1));
}
### a_expr_58: a_expr IS DOCUMENT
sub got_a_expr_58 { $_[0]->makeXmlExpr(IS_DOCUMENT, NULL, NIL, $_[0]->lappend($_[1]), $_[0]->YYLLoc($_[2], 2)) }
### a_expr_59: a_expr IS NOT DOCUMENT
sub got_a_expr_59 {
   return $_[0]->makeA_Expr(
      AEXPR_NOT, NIL, NULL,
      $_[0]->makeXmlExpr(
         IS_DOCUMENT, NULL, NIL,
         $_[0]->lappend($_[1]),
         $_[0]->YYLLoc($_[2], 2)
      ),
      $_[0]->YYLLoc($_[2], 2)
   );
}

### b_expr_1 : c_expr
sub got_b_expr_1  { $_[1] }
### b_expr_2 : b_expr TYPECAST Typename
sub got_b_expr_2  { $_[0]->makeTypeCast    ($_[1], $_[3],                      $_[0]->YYLLoc($_[2], 2)) }
### b_expr_3 : <PLUS> b_expr
sub got_b_expr_3  { $_[0]->makeSimpleA_Expr(AEXPR_OP,       "+",  NULL, $_[2], $_[0]->YYLLoc($_[1], 1)) }
### b_expr_4 : <DASH> b_expr
sub got_b_expr_4  { $_[0]->doNegate        ($_[2],                             $_[0]->YYLLoc($_[1], 1)) }
### b_expr_5 : b_expr <PLUS> b_expr
sub got_b_expr_5  { $_[0]->makeSimpleA_Expr(AEXPR_OP,       "+", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
### b_expr_6 : b_expr <DASH> b_expr
sub got_b_expr_6  { $_[0]->makeSimpleA_Expr(AEXPR_OP,       "-", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
### b_expr_7 : b_expr <STAR> b_expr
sub got_b_expr_7  { $_[0]->makeSimpleA_Expr(AEXPR_OP,       "*", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
### b_expr_8 : b_expr <SLASH> b_expr
sub got_b_expr_8  { $_[0]->makeSimpleA_Expr(AEXPR_OP,       "/", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
### b_expr_9 : b_expr <PERCENT> b_expr
sub got_b_expr_9  { $_[0]->makeSimpleA_Expr(AEXPR_OP,       "%", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
### b_expr_10: b_expr <CARET> b_expr
sub got_b_expr_10 { $_[0]->makeSimpleA_Expr(AEXPR_OP,       "^", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
### b_expr_11: b_expr <LANGLE> b_expr
sub got_b_expr_11 { $_[0]->makeSimpleA_Expr(AEXPR_OP,       "<", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
### b_expr_12: b_expr <RANGLE> b_expr
sub got_b_expr_12 { $_[0]->makeSimpleA_Expr(AEXPR_OP,       ">", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
### b_expr_13: b_expr <EQUAL> b_expr
sub got_b_expr_13 { $_[0]->makeSimpleA_Expr(AEXPR_OP,       "=", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
### b_expr_14: b_expr qual_Op b_expr
sub got_b_expr_14 { $_[0]->makeA_Expr      (AEXPR_OP,     $_[2], $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
### b_expr_15: qual_Op b_expr
sub got_b_expr_15 { $_[0]->makeA_Expr      (AEXPR_OP,     $_[1],  NULL, $_[2], $_[0]->YYLLoc($_[1], 1)) }
### b_expr_16: b_expr qual_Op
sub got_b_expr_16 { $_[0]->makeA_Expr      (AEXPR_OP,     $_[2], $_[1],  NULL, $_[0]->YYLLoc($_[2], 2)) }
### b_expr_17: b_expr IS DISTINCT FROM b_expr
sub got_b_expr_17 { $_[0]->makeSimpleA_Expr(AEXPR_DISTINCT, "=", $_[1], $_[5], $_[0]->YYLLoc($_[2], 2)) }
### b_expr_18: b_expr IS NOT DISTINCT FROM b_expr
sub got_b_expr_18 {
   return $_[0]->makeA_Expr(
      AEXPR_NOT, NIL, NULL,
      $_[0]->makeSimpleA_Expr(
         AEXPR_DISTINCT, "=", $_[1], $_[6],
         $_[0]->YYLLoc($_[2], 2)
      ),
      $_[0]->YYLLoc($_[2], 2)
   );
}
### b_expr_19: b_expr IS OF <LPAREN> type_list <RPAREN>
sub got_b_expr_19 { $_[0]->makeSimpleA_Expr(AEXPR_OF,  "=", $_[1],  $_[5], $_[0]->YYLLoc($_[2], 2)) }
### b_expr_20: b_expr IS NOT OF <LPAREN> type_list <RPAREN>
sub got_b_expr_20 { $_[0]->makeSimpleA_Expr(AEXPR_OF, "<>", $_[1],  $_[6], $_[0]->YYLLoc($_[2], 2)) }
### b_expr_21: b_expr IS DOCUMENT
sub got_b_expr_21 {
   return $_[0]->makeXmlExpr(
      IS_DOCUMENT, NULL, NIL,
      $_[0]->lappend($_[1]),
      $_[0]->YYLLoc($_[2], 2)
   );
}
### b_expr_22: b_expr IS NOT DOCUMENT
sub got_b_expr_22 {
   return $_[0]->makeA_Expr(
      AEXPR_NOT, NIL, NULL,
      $_[0]->makeXmlExpr(
         IS_DOCUMENT, NULL, NIL,
         $_[0]->lappend($_[1]),
         $_[0]->YYLLoc($_[2], 2)
      ),
      $_[0]->YYLLoc($_[2], 2)
   );
}

### c_expr_1 : columnref
sub got_c_expr_1  { $_[1] }
### c_expr_2 : AexprConst
sub got_c_expr_2  { $_[1] }
### c_expr_3 : PARAM opt_indirection
sub got_c_expr_3  {
   my $p = SQL::Translator::Statement::ParamRef->new(
      number   => $_[1],
      location => $_[0]->YYLLoc($_[1], 1),
   );
   if ($_[2]) {
      return SQL::Translator::Statement::A_Indirection->new(
         arg         => $p,
         indirection => $_[0]->check_indirection($_[2]),
      );
   }
   return $p;
}
### c_expr_4 : <LPAREN> a_expr <RPAREN> opt_indirection
sub got_c_expr_4  {
   if ($_[4]) {
      return SQL::Translator::Statement::A_Indirection->new(
         arg         => $_[2],
         indirection => $_[0]->check_indirection($_[4]),
      );
   }
   return $_[2];
}
### c_expr_5 : case_expr
sub got_c_expr_5  { $_[1] }
### c_expr_6 : func_expr
sub got_c_expr_6  { $_[1] }
### c_expr_7 : select_with_parens
sub got_c_expr_7  {
   return SQL::Translator::Statement::SubLink->new(
      subLinkType => EXPR_SUBLINK,
      testexpr    => NULL,
      operName    => NIL,
      subselect   => $_[1],
      location    => $_[0]->YYLLoc($_[1], 1),
   );
}
### c_expr_8 : EXISTS select_with_parens
sub got_c_expr_8  {
   return SQL::Translator::Statement::SubLink->new(
      subLinkType => EXISTS_SUBLINK,
      testexpr    => NULL,
      operName    => NIL,
      subselect   => $_[2],
      location    => $_[0]->YYLLoc($_[1], 1),
   );
}
### c_expr_9 : ARRAY select_with_parens
sub got_c_expr_9  {
   return SQL::Translator::Statement::SubLink->new(
      subLinkType => ARRAY_SUBLINK,
      testexpr    => NULL,
      operName    => NIL,
      subselect   => $_[2],
      location    => $_[0]->YYLLoc($_[1], 1),
   );
}
### c_expr_10: ARRAY array_expr
sub got_c_expr_10 {
   #* point outermost A_ArrayExpr to the ARRAY keyword
   $_[2]->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $_[2];
}
### c_expr_11: row
sub got_c_expr_11 {
   return SQL::Translator::Statement::RowExpr->new(
      args       => $_[1],
      row_typeid => InvalidOid,   #* not analyzed yet
      location   => $_[0]->YYLLoc($_[1], 1),
   );
}

### func_expr_1 : func_name <LPAREN> <RPAREN> over_clause
sub got_func_expr_1  {
   return SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[1],
      args          => NIL,
      agg_order     => NIL,
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => FALSE,
      over          => $_[4],
      location      => $_[0]->YYLLoc($_[1], 1),
   );
}
### func_expr_2 : func_name <LPAREN> func_arg_list <RPAREN> over_clause
sub got_func_expr_2  {
   return SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[1],
      args          => $_[3],
      agg_order     => NIL,
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => FALSE,
      over          => $_[5],
      location      => $_[0]->YYLLoc($_[1], 1),
   );
}
### func_expr_3 : func_name <LPAREN> VARIADIC func_arg_expr <RPAREN> over_clause
sub got_func_expr_3  {
   return SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[1],
      args          => $_[0]->lappend($_[4]),
      agg_order     => NIL,
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => TRUE,
      over          => $_[6],
      location      => $_[0]->YYLLoc($_[1], 1),
   );
}
### func_expr_4 : func_name <LPAREN> func_arg_list <COMMA> VARIADIC func_arg_expr <RPAREN> over_clause
sub got_func_expr_4  {
   return SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[1],
      args          => $_[0]->lappend($_[3], $_[6]),
      agg_order     => NIL,
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => TRUE,
      over          => $_[8],
      location      => $_[0]->YYLLoc($_[1], 1),
   );
}
### func_expr_5 : func_name <LPAREN> func_arg_list sort_clause <RPAREN> over_clause
sub got_func_expr_5  {
   return SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[1],
      args          => $_[3],
      agg_order     => $_[4],
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => FALSE,
      over          => $_[6],
      location      => $_[0]->YYLLoc($_[1], 1),
   );
}
### func_expr_6 : func_name <LPAREN> ALL func_arg_list opt_sort_clause <RPAREN> over_clause
sub got_func_expr_6  {
   return SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[1],
      args          => $_[4],
      agg_order     => $_[5],
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      #* Ideally we'd mark the FuncCall node to indicate
      #* "must be an aggregate", but there's no provision
      #* for that in FuncCall at the moment.
      func_variadic => FALSE,
      over          => $_[7],
      location      => $_[0]->YYLLoc($_[1], 1),
   );
}
### func_expr_7 : func_name <LPAREN> DISTINCT func_arg_list opt_sort_clause <RPAREN> over_clause
sub got_func_expr_7  {
   return SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[1],
      args          => $_[4],
      agg_order     => $_[5],
      agg_star      => FALSE,
      agg_distinct  => TRUE,
      func_variadic => FALSE,
      over          => $_[7],
      location      => $_[0]->YYLLoc($_[1], 1),
   );
}
# We consider AGGREGATE(*) to invoke a parameterless
# aggregate.  This does the right thing for COUNT(*),
# and there are no other aggregates in SQL92 that accept
# '*' as parameter.
#
# The FuncCall node is also marked agg_star = TRUE,
# so that later processing can detect what the argument
# really was.
### func_expr_8 : func_name <LPAREN> <STAR> <RPAREN> over_clause
sub got_func_expr_8  {
   return SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[1],
      args          => NIL,
      agg_order     => NIL,
      agg_star      => TRUE,
      agg_distinct  => FALSE,
      func_variadic => FALSE,
      over          => $_[5],
      location      => $_[0]->YYLLoc($_[1], 1),
   );
}
# Translate as "'now'::text::date".
#
# We cannot use "'now'::date" because coerce_type() will
# immediately reduce that to a constant representing
# today's date.  We need to delay the conversion until
# runtime, else the wrong things will happen when
# CURRENT_DATE is used in a column default value or rule.
#
# This could be simplified if we had a way to generate
# an expression tree representing runtime application
# of type-input conversion functions.  (As of PG 7.3
# that is actually possible, but not clear that we want
# to rely on it.)
### func_expr_9 : CURRENT_DATE
sub got_func_expr_9  {
   my $n = $_[0]->makeStringConstCast("now", $_[0]->YYLLoc($_[1], 1), $_[0]->SystemTypeName("text"));
   return $_[0]->makeTypeCast($n, SystemTypeName("date"), {});
}
# Translate as "'now'::text::timetz".
# See comments for CURRENT_DATE.
### func_expr_10: CURRENT_TIME
sub got_func_expr_10 {
   my $n = $_[0]->makeStringConstCast("now", $_[0]->YYLLoc($_[1], 1), $_[0]->SystemTypeName("text"));
   return $_[0]->makeTypeCast($n, SystemTypeName("timetz"), {});
}
# Translate as "'now'::text::timetz(n)".
# See comments for CURRENT_DATE.
### func_expr_11: CURRENT_TIME <LPAREN> Iconst <RPAREN>
sub got_func_expr_11 {
   my $n = $_[0]->makeStringConstCast("now", $_[0]->YYLLoc($_[1], 1), $_[0]->SystemTypeName("text"));
   my $d = $_[0]->SystemTypeName("timetz");
   $d->typmods( $_[0]->lappend($_[0]->makeIntConst($_[3], $_[0]->YYLLoc($_[3], 3))) );
   return $_[0]->makeTypeCast($n, $d, {});
}
# Translate as "now()", since we have a function that
# does exactly what is needed.
### func_expr_12: CURRENT_TIMESTAMP
sub got_func_expr_12 {
   return SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[0]->SystemFuncName("now"),
      args          => NIL,
      agg_order     => NIL,
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => FALSE,
      over          => NULL,
      location      => $_[0]->YYLLoc($_[1], 1),
   );
}
# Translate as "'now'::text::timestamptz(n)".
# See comments for CURRENT_DATE.
### func_expr_13: CURRENT_TIMESTAMP <LPAREN> Iconst <RPAREN>
sub got_func_expr_13 {
   my $n = $_[0]->makeStringConstCast("now", $_[0]->YYLLoc($_[1], 1), $_[0]->SystemTypeName("text"));
   my $d = $_[0]->SystemTypeName("timestamptz");
   $d->typmods( $_[0]->lappend($_[0]->makeIntConst($_[3], $_[0]->YYLLoc($_[3], 3))) );
   return $_[0]->makeTypeCast($n, $d, {});
}
# Translate as "'now'::text::time".
# See comments for CURRENT_DATE.
### func_expr_14: LOCALTIME
sub got_func_expr_14 {
   my $n = $_[0]->makeStringConstCast("now", $_[0]->YYLLoc($_[1], 1), $_[0]->SystemTypeName("text"));
   return $_[0]->makeTypeCast($n, SystemTypeName("time"), {});
}
# Translate as "'now'::text::time(n)".
# See comments for CURRENT_DATE.
### func_expr_15: LOCALTIME <LPAREN> Iconst <RPAREN>
sub got_func_expr_15 {
   my $n = $_[0]->makeStringConstCast("now", $_[0]->YYLLoc($_[1], 1), $_[0]->SystemTypeName("text"));
   my $d = $_[0]->SystemTypeName("time");
   $d->typmods( $_[0]->lappend($_[0]->makeIntConst($_[3], $_[0]->YYLLoc($_[3], 3))) );
   return $_[0]->makeTypeCast($n, $d, {});
}
# Translate as "'now'::text::timestamp".
# See comments for CURRENT_DATE.
### func_expr_16: LOCALTIMESTAMP
sub got_func_expr_16 {
   my $n = $_[0]->makeStringConstCast("now", $_[0]->YYLLoc($_[1], 1), $_[0]->SystemTypeName("text"));
   return $_[0]->makeTypeCast($n, SystemTypeName("timestamp"), {});
}
# Translate as "'now'::text::timestamp(n)".
# See comments for CURRENT_DATE.
### func_expr_17: LOCALTIMESTAMP <LPAREN> Iconst <RPAREN>
sub got_func_expr_17 {
   my $n = $_[0]->makeStringConstCast("now", $_[0]->YYLLoc($_[1], 1), $_[0]->SystemTypeName("text"));
   my $d = $_[0]->SystemTypeName("timestamp");
   $d->typmods( $_[0]->lappend($_[0]->makeIntConst($_[3], $_[0]->YYLLoc($_[3], 3))) );
   return $_[0]->makeTypeCast($n, $d, {});
}
### func_expr_18: CURRENT_ROLE
sub got_func_expr_18 {
   return SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[0]->SystemFuncName("current_user"),
      args          => NIL,
      agg_order     => NIL,
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => FALSE,
      over          => NULL,
      location      => $_[0]->YYLLoc($_[1], 1),
   );
}
### func_expr_19: CURRENT_USER
sub got_func_expr_19 {
   return SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[0]->SystemFuncName("current_user"),
      args          => NIL,
      agg_order     => NIL,
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => FALSE,
      over          => NULL,
      location      => $_[0]->YYLLoc($_[1], 1),
   );
}
### func_expr_20: SESSION_USER
sub got_func_expr_20 {
   return SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[0]->SystemFuncName("session_user"),
      args          => NIL,
      agg_order     => NIL,
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => FALSE,
      over          => NULL,
      location      => $_[0]->YYLLoc($_[1], 1),
   );
}
### func_expr_21: USER
sub got_func_expr_21 {
   return SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[0]->SystemFuncName("current_user"),
      args          => NIL,
      agg_order     => NIL,
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => FALSE,
      over          => NULL,
      location      => $_[0]->YYLLoc($_[1], 1),
   );
}
### func_expr_22: CURRENT_CATALOG
sub got_func_expr_22 {
   return SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[0]->SystemFuncName("current_database"),
      args          => NIL,
      agg_order     => NIL,
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => FALSE,
      over          => NULL,
      location      => $_[0]->YYLLoc($_[1], 1),
   );
}
### func_expr_23: CURRENT_SCHEMA
sub got_func_expr_23 {
   return SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[0]->SystemFuncName("current_schema"),
      args          => NIL,
      agg_order     => NIL,
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => FALSE,
      over          => NULL,
      location      => $_[0]->YYLLoc($_[1], 1),
   );
}
### func_expr_24: CAST <LPAREN> a_expr AS Typename <RPAREN>
sub got_func_expr_24 { $_[0]->makeTypeCast($_[3], $_[5], $_[0]->YYLLoc($_[1], 1)) }
### func_expr_25: EXTRACT <LPAREN> extract_list <RPAREN>
sub got_func_expr_25 {
   return SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[0]->SystemFuncName("date_part"),
      args          => $_[3],
      agg_order     => NIL,
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => FALSE,
      over          => NULL,
      location      => $_[0]->YYLLoc($_[1], 1),
   );
}
# overlay(A PLACING B FROM C FOR D) is converted to
# overlay(A, B, C, D)
# overlay(A PLACING B FROM C) is converted to
# overlay(A, B, C)
### func_expr_26: OVERLAY <LPAREN> overlay_list <RPAREN>
sub got_func_expr_26 {
   return SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[0]->SystemFuncName("overlay"),
      args          => $_[3],
      agg_order     => NIL,
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => FALSE,
      over          => NULL,
      location      => $_[0]->YYLLoc($_[1], 1),
   );
}
# position(A in B) is converted to position(B, A)
### func_expr_27: POSITION <LPAREN> position_list <RPAREN>
sub got_func_expr_27 {
   return SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[0]->SystemFuncName("position"),
      args          => $_[3],
      agg_order     => NIL,
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => FALSE,
      over          => NULL,
      location      => $_[0]->YYLLoc($_[1], 1),
   );
}
# substring(A from B for C) is converted to
# substring(A, B, C) - thomas 2000-11-28
### func_expr_28: SUBSTRING <LPAREN> substr_list <RPAREN>
sub got_func_expr_28 {
   return SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[0]->SystemFuncName("substring"),
      args          => $_[3],
      agg_order     => NIL,
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => FALSE,
      over          => NULL,
      location      => $_[0]->YYLLoc($_[1], 1),
   );
}
# TREAT(expr AS target) converts expr of a particular type to target,
# which is defined to be a subtype of the original expression.
# In SQL99, this is intended for use with structured UDTs,
# but let's make this a generally useful form allowing stronger
# coercions than are handled by implicit casting.
### func_expr_29: TREAT <LPAREN> a_expr AS Typename <RPAREN>
sub got_func_expr_29 {
   return SQL::Translator::Statement::Function::Call->new(
      #* Convert SystemTypeName() to SystemFuncName() even though
      #* at the moment they result in the same thing.
      funcname      => $_[0]->SystemFuncName($_[5]->names->[-1]),
      args          => $_[0]->lappend($_[3]),
      agg_order     => NIL,
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => FALSE,
      over          => NULL,
      location      => $_[0]->YYLLoc($_[1], 1),
   );
}
# various trim expressions are defined in SQL92
# - thomas 1997-07-19
### func_expr_30: TRIM <LPAREN> BOTH trim_list <RPAREN>
sub got_func_expr_30 {
   return SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[0]->SystemFuncName("btrim"),
      args          => $_[4],
      agg_order     => NIL,
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => FALSE,
      over          => NULL,
      location      => $_[0]->YYLLoc($_[1], 1),
   );
}
### func_expr_31: TRIM <LPAREN> LEADING trim_list <RPAREN>
sub got_func_expr_31 {
   return SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[0]->SystemFuncName("ltrim"),
      args          => $_[4],
      agg_order     => NIL,
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => FALSE,
      over          => NULL,
      location      => $_[0]->YYLLoc($_[1], 1),
   );
}
### func_expr_32: TRIM <LPAREN> TRAILING trim_list <RPAREN>
sub got_func_expr_32 {
   return SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[0]->SystemFuncName("rtrim"),
      args          => $_[4],
      agg_order     => NIL,
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => FALSE,
      over          => NULL,
      location      => $_[0]->YYLLoc($_[1], 1),
   );
}
### func_expr_33: TRIM <LPAREN> trim_list <RPAREN>
sub got_func_expr_33 {
   return SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[0]->SystemFuncName("btrim"),
      args          => $_[3],
      agg_order     => NIL,
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => FALSE,
      over          => NULL,
      location      => $_[0]->YYLLoc($_[1], 1),
   );
}
### func_expr_34: NULLIF <LPAREN> a_expr <COMMA> a_expr <RPAREN>
sub got_func_expr_34 { $_[0]->makeSimpleA_Expr(AEXPR_NULLIF, "=", $_[3], $_[5], $_[0]->YYLLoc($_[1], 1)) }
### func_expr_35: COALESCE <LPAREN> expr_list <RPAREN>
sub got_func_expr_35 {
   return SQL::Translator::Statement::CoalesceExpr->new(
      args     => $_[3],
      location => $_[0]->YYLLoc($_[1], 1),
   );
}
### func_expr_36: GREATEST <LPAREN> expr_list <RPAREN>
sub got_func_expr_36 {
   return SQL::Translator::Statement::MinMaxExpr->new(
      args     => $_[3],
      op       => IS_GREATEST,
      location => $_[0]->YYLLoc($_[1], 1),
   );
}
### func_expr_37: LEAST <LPAREN> expr_list <RPAREN>
sub got_func_expr_37 {
   return SQL::Translator::Statement::MinMaxExpr->new(
      args     => $_[3],
      op       => IS_LEAST,
      location => $_[0]->YYLLoc($_[1], 1),
   );
}
### func_expr_38: XMLCONCAT <LPAREN> expr_list <RPAREN>
sub got_func_expr_38 { $_[0]->makeXmlExpr(IS_XMLCONCAT, NULL, NIL, $_[3], $_[0]->YYLLoc($_[1], 1)) }
### func_expr_39: XMLELEMENT <LPAREN> NAME ColLabel <RPAREN>
sub got_func_expr_39 { $_[0]->makeXmlExpr(IS_XMLELEMENT, $_[4], NIL, NIL, $_[0]->YYLLoc($_[1], 1)) }
### func_expr_40: XMLELEMENT <LPAREN> NAME ColLabel <COMMA> xml_attributes <RPAREN>
sub got_func_expr_40 { $_[0]->makeXmlExpr(IS_XMLELEMENT, $_[4], $_[6], NIL, $_[0]->YYLLoc($_[1], 1)) }
### func_expr_41: XMLELEMENT <LPAREN> NAME ColLabel <COMMA> expr_list <RPAREN>
sub got_func_expr_41 { $_[0]->makeXmlExpr(IS_XMLELEMENT, $_[4], NIL, $_[6], $_[0]->YYLLoc($_[1], 1)) }
### func_expr_42: XMLELEMENT <LPAREN> NAME ColLabel <COMMA> xml_attributes <COMMA> expr_list <RPAREN>
sub got_func_expr_42 { $_[0]->makeXmlExpr(IS_XMLELEMENT, $_[4], $_[6], $_[8], $_[0]->YYLLoc($_[1], 1)) }
# xmlexists(A PASSING [BY REF] B [BY REF]) is
# converted to xmlexists(A, B)
### func_expr_43: XMLEXISTS <LPAREN> c_expr xmlexists_argument <RPAREN>
sub got_func_expr_43 {
   return SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[0]->SystemFuncName("xmlexists"),
      args          => $_[0]->lappend($_[3], $_[4]),
      agg_order     => NIL,
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => FALSE,
      over          => NULL,
      location      => $_[0]->YYLLoc($_[1], 1),
   );
}
### func_expr_44: XMLFOREST <LPAREN> xml_attribute_list <RPAREN>
sub got_func_expr_44 { $_[0]->makeXmlExpr(IS_XMLFOREST, NULL, $_[3], NIL, $_[0]->YYLLoc($_[1], 1)) }
### func_expr_45: XMLPARSE <LPAREN> document_or_content a_expr xml_whitespace_option <RPAREN>
sub got_func_expr_45 {
   my $x = $_[0]->makeXmlExpr(IS_XMLPARSE, NULL, NIL,
               $_[0]->lappend($_[4], $_[0]->makeBoolAConst($_[5], {})),
               $_[0]->YYLLoc($_[1], 1));
   $x->xmloption($_[3]);
   return $x;
}
### func_expr_46: XMLPI <LPAREN> NAME ColLabel <RPAREN>
sub got_func_expr_46 { $_[0]->makeXmlExpr(IS_XMLPI, $_[4], NULL, NIL, $_[0]->YYLLoc($_[1], 1)) }
### func_expr_47: XMLPI <LPAREN> NAME ColLabel <COMMA> a_expr <RPAREN>
sub got_func_expr_47 { $_[0]->makeXmlExpr(IS_XMLPI, $_[4], NULL, $_[0]->lappend($_[6]), $_[0]->YYLLoc($_[1], 1)) }
### func_expr_48: XMLROOT <LPAREN> a_expr <COMMA> xml_root_version opt_xml_root_standalone <RPAREN>
sub got_func_expr_48 {
   return $_[0]->makeXmlExpr(IS_XMLROOT, NULL, NIL, $_[0]->lappend($_[3], $_[5], $_[6]), $_[0]->YYLLoc($_[1], 1));
}
### func_expr_49: XMLSERIALIZE <LPAREN> document_or_content a_expr AS SimpleTypename <RPAREN>
sub got_func_expr_49 {
   return SQL::Translator::Statement::XMLSerialize->new(
      xmloption => $_[3],
      expr      => $_[4],
      typeName  => $_[6],
      location  => $_[0]->YYLLoc($_[1], 1),
   );
}

### xml_root_version_1: VERSION a_expr
sub got_xml_root_version_1 { $_[2] }
### xml_root_version_2: VERSION NO VALUE
sub got_xml_root_version_2 { $_[0]->makeNullAConst({}) }

### opt_xml_root_standalone_1: <COMMA> STANDALONE YES
sub got_opt_xml_root_standalone_1 { $_[0]->makeStringConst('XML_STANDALONE_YES',      {}) }
### opt_xml_root_standalone_2: <COMMA> STANDALONE NO
sub got_opt_xml_root_standalone_2 { $_[0]->makeStringConst('XML_STANDALONE_NO',       {}) }
### opt_xml_root_standalone_3: <COMMA> STANDALONE NO VALUE
sub got_opt_xml_root_standalone_3 { $_[0]->makeStringConst('XML_STANDALONE_NO_VALUE', {}) }

### xml_attributes: XMLATTRIBUTES <LPAREN> xml_attribute_list <RPAREN>
sub got_xml_attributes { $_[3] }

### xml_attribute_list_1: xml_attribute_el
sub got_xml_attribute_list_1 { $_[0]->lappend($_[1]) }
### xml_attribute_list_2: xml_attribute_list <COMMA> xml_attribute_el
sub got_xml_attribute_list_2 { $_[0]->lappend($_[1], $_[3]) }

### xml_attribute_el_1: a_expr AS ColLabel
sub got_xml_attribute_el_1 {
   return SQL::Translator::Statement::ResultTarget->new(
      name        => $_[3],
      indirection => NIL,
      val         => $_[1],
      location    => $_[0]->YYLLoc($_[1], 1),
   );
}
### xml_attribute_el_2: a_expr
sub got_xml_attribute_el_2 {
   return SQL::Translator::Statement::ResultTarget->new(
      name        => NULL,
      indirection => NIL,
      val         => $_[1],
      location    => $_[0]->YYLLoc($_[1], 1),
   );
}

### document_or_content_1: DOCUMENT
sub got_document_or_content_1 { XMLOPTION_DOCUMENT }
### document_or_content_2: CONTENT
sub got_document_or_content_2 { XMLOPTION_CONTENT  }

### xml_whitespace_option_1: PRESERVE WHITESPACE
sub got_xml_whitespace_option_1 { TRUE  }
### xml_whitespace_option_2: STRIP WHITESPACE
sub got_xml_whitespace_option_2 { FALSE }

### xmlexists_argument_1: PASSING c_expr
sub got_xmlexists_argument_1 { $_[2] }
### xmlexists_argument_2: PASSING c_expr BY REF
sub got_xmlexists_argument_2 { $_[2] }
### xmlexists_argument_3: PASSING BY REF c_expr
sub got_xmlexists_argument_3 { $_[4] }
### xmlexists_argument_4: PASSING BY REF c_expr BY REF
sub got_xmlexists_argument_4 { $_[4] }

### window_clause_1: WINDOW window_definition_list
sub got_window_clause_1 { $_[2] }

### window_definition_list_1: window_definition
sub got_window_definition_list_1 { $_[0]->lappend($_[1]) }
### window_definition_list_2: window_definition_list <COMMA> window_definition
sub got_window_definition_list_2 { $_[0]->lappend($_[1], $_[3]) }

### window_definition: ColId AS window_specification
sub got_window_definition {
   $_[3]->name($_[1]);
   return $_[3];
}

### over_clause_1: OVER window_specification
sub got_over_clause_1 { $_[2] }
### over_clause_2: OVER ColId
sub got_over_clause_2 {
   return SQL::Translator::Statement::WindowDef->new(
      name            => $_[2],
      refname         => NULL,
      partitionClause => NIL,
      orderClause     => NIL,
      frameOptions    => FRAMEOPTION_DEFAULTS,
      startOffset     => NULL,
      endOffset       => NULL,
      location        => $_[0]->YYLLoc($_[2], 2),
   );
}

### window_specification: <LPAREN> opt_existing_window_name opt_partition_clause opt_sort_clause opt_frame_clause <RPAREN>
sub got_window_specification {
   return SQL::Translator::Statement::WindowDef->new(
      name            => NULL,
      refname         => $_[2],
      partitionClause => $_[3],
      orderClause     => $_[4],
      #* copy relevant fields of opt_frame_clause
      frameOptions    => $_[5]->frameOptions,
      startOffset     => $_[5]->startOffset,
      endOffset       => $_[5]->endOffset,
      location        => $_[0]->YYLLoc($_[1], 1),
   );
}

### opt_existing_window_name_1: ColId
sub got_opt_existing_window_name_1 { $_[1] }

### opt_partition_clause_1: PARTITION BY expr_list
sub got_opt_partition_clause_1 { $_[3] }

### opt_frame_clause_1: RANGE frame_extent
sub got_opt_frame_clause_1 {
   my $n = $_[2];
   $n->frameOptions($n->frameOptions | FRAMEOPTION_NONDEFAULT | FRAMEOPTION_RANGE);

   $_[0]->ereport(
      ERROR,
      ERRCODE_FEATURE_NOT_SUPPORTED,
      "RANGE PRECEDING is only supported with UNBOUNDED",
      $_[0]->YYLLoc($_[1], 1)
   ) if ($n->frameOptions & (FRAMEOPTION_START_VALUE_PRECEDING | FRAMEOPTION_END_VALUE_PRECEDING));

   $_[0]->ereport(ERROR,
      ERRCODE_FEATURE_NOT_SUPPORTED,
      "RANGE FOLLOWING is only supported with UNBOUNDED",
      $_[0]->YYLLoc($_[1], 1)
   ) if ($n->frameOptions & (FRAMEOPTION_START_VALUE_FOLLOWING | FRAMEOPTION_END_VALUE_FOLLOWING));

   return $n;
}
### opt_frame_clause_2: ROWS frame_extent
sub got_opt_frame_clause_2 {
   $_[2]->frameOptions($_[2]->frameOptions | FRAMEOPTION_NONDEFAULT | FRAMEOPTION_ROWS);
   return $_[2];
}

### frame_extent_1: frame_bound
sub got_frame_extent_1 {
   #* reject invalid cases
   $_[0]->ereport(ERROR,
      ERRCODE_WINDOWING_ERROR,
      "Frame start cannot be UNBOUNDED FOLLOWING",
      $_[0]->YYLLoc($_[1], 1)
   ) if ($_[1]->frameOptions & FRAMEOPTION_START_UNBOUNDED_FOLLOWING);

   $_[0]->ereport(ERROR,
      ERRCODE_WINDOWING_ERROR,
      "Frame starting from following row cannot end with current row",
      $_[0]->YYLLoc($_[1], 1)
   ) if ($_[1]->frameOptions & FRAMEOPTION_START_VALUE_FOLLOWING);

   $_[1]->frameOptions($_[1]->frameOptions | FRAMEOPTION_END_CURRENT_ROW);
   return $_[1];
}
### frame_extent_2: BETWEEN frame_bound AND frame_bound
sub got_frame_extent_2 {
   my $n1 = $_[2];
   my $n2 = $_[4];
   #* form merged options
   my $frameOptions = $n1->frameOptions;
   #* shift converts START_ options to END_ options
   $frameOptions |= $n2->frameOptions << 1;
   $frameOptions |= FRAMEOPTION_BETWEEN;

   #* reject invalid cases
   $_[0]->ereport(ERROR,
      ERRCODE_WINDOWING_ERROR,
      "Frame start cannot be UNBOUNDED FOLLOWING",
      $_[0]->YYLLoc($_[2], 2)
   ) if ($frameOptions & FRAMEOPTION_START_UNBOUNDED_FOLLOWING);
   $_[0]->ereport(ERROR,
      ERRCODE_WINDOWING_ERROR,
      "Frame end cannot be UNBOUNDED PRECEDING",
      $_[0]->YYLLoc($_[4], 4)
   ) if ($frameOptions & FRAMEOPTION_START_UNBOUNDED_PRECEDING);
   $_[0]->ereport(ERROR,
      ERRCODE_WINDOWING_ERROR,
      "Frame starting from current row cannot have preceding rows",
      $_[0]->YYLLoc($_[4], 4)
   ) if ($frameOptions & FRAMEOPTION_START_CURRENT_ROW && $frameOptions & FRAMEOPTION_END_VALUE_PRECEDING);
   $_[0]->ereport(ERROR,
      ERRCODE_WINDOWING_ERROR,
      "Frame starting from following row cannot have preceding rows",
      $_[0]->YYLLoc($_[4], 4)
   ) if ($frameOptions & FRAMEOPTION_START_VALUE_FOLLOWING &&
         $frameOptions & (FRAMEOPTION_END_VALUE_PRECEDING | FRAMEOPTION_END_CURRENT_ROW));

   $n1->frameOptions($frameOptions);
   $n1->endOffset( $n2->startOffset );
   return $n1;
}

### frame_bound_1: UNBOUNDED PRECEDING
sub got_frame_bound_1 {
   return SQL::Translator::Statement::WindowDef->new(
      frameOptions => FRAMEOPTION_START_UNBOUNDED_PRECEDING,
      startOffset  => NULL,
      endOffset    => NULL,
   );
}
### frame_bound_2: UNBOUNDED FOLLOWING
sub got_frame_bound_2 {
   return SQL::Translator::Statement::WindowDef->new(
      frameOptions => FRAMEOPTION_START_UNBOUNDED_FOLLOWING,
      startOffset  => NULL,
      endOffset    => NULL,
   );
}
### frame_bound_3: CURRENT ROW
sub got_frame_bound_3 {
   return SQL::Translator::Statement::WindowDef->new(
      frameOptions => FRAMEOPTION_START_CURRENT_ROW,
      startOffset  => NULL,
      endOffset    => NULL,
   );
}
### frame_bound_4: a_expr PRECEDING
sub got_frame_bound_4 {
   return SQL::Translator::Statement::WindowDef->new(
      frameOptions => FRAMEOPTION_START_VALUE_PRECEDING,
      startOffset  => $_[1],
      endOffset    => NULL,
   );
}
### frame_bound_5: a_expr FOLLOWING
sub got_frame_bound_5 {
   return SQL::Translator::Statement::WindowDef->new(
      frameOptions => FRAMEOPTION_START_VALUE_FOLLOWING,
      startOffset  => $_[1],
      endOffset    => NULL,
   );
}

### row_1: ROW <LPAREN> expr_list <RPAREN>
sub got_row_1 { $_[3] }
### row_2: ROW <LPAREN> <RPAREN>
sub got_row_2 { NULL }
### row_3: <LPAREN> expr_list <COMMA> a_expr <RPAREN>
sub got_row_3 { $_[0]->lappend($_[2], $_[4]) }

### sub_type_1: ANY
sub got_sub_type_1 { ANY_SUBLINK }
### sub_type_2: SOME
sub got_sub_type_2 { ANY_SUBLINK }
### sub_type_3: ALL
sub got_sub_type_3 { ALL_SUBLINK }

### all_Op_1: Op
sub got_all_Op_1 { $_[1] }
### all_Op_2: MathOp
sub got_all_Op_2 { $_[1] }

### MathOp_1: <PLUS>
sub got_MathOp_1 { "+" }
### MathOp_2: <DASH>
sub got_MathOp_2 { "-" }
### MathOp_3: <STAR>
sub got_MathOp_3 { "*" }
### MathOp_4: <SLASH>
sub got_MathOp_4 { "/" }
### MathOp_5: <PERCENT>
sub got_MathOp_5 { "%" }
### MathOp_6: <CARET>
sub got_MathOp_6 { "^" }
### MathOp_7: <LANGLE>
sub got_MathOp_7 { "<" }
### MathOp_8: <RANGLE>
sub got_MathOp_8 { ">" }
### MathOp_9: <EQUAL>
sub got_MathOp_9 { "=" }

### qual_Op_1: Op
sub got_qual_Op_1 { $_[0]->lappend($_[1]) }
### qual_Op_2: OPERATOR <LPAREN> any_operator <RPAREN>
sub got_qual_Op_2 { $_[3] }

### qual_all_Op_1: all_Op
sub got_qual_all_Op_1 { $_[0]->lappend($_[1]) }
### qual_all_Op_2: OPERATOR <LPAREN> any_operator <RPAREN>
sub got_qual_all_Op_2 { $_[3] }

### subquery_Op_1: all_Op
sub got_subquery_Op_1 { $_[0]->lappend($_[1])  }
### subquery_Op_2: OPERATOR <LPAREN> any_operator <RPAREN>
sub got_subquery_Op_2 { $_[3]                  }
### subquery_Op_3: LIKE
sub got_subquery_Op_3 { $_[0]->lappend("~~")   }
### subquery_Op_4: NOT LIKE
sub got_subquery_Op_4 { $_[0]->lappend("!~~")  }
### subquery_Op_5: ILIKE
sub got_subquery_Op_5 { $_[0]->lappend("~~*")  }
# cannot put SIMILAR TO here, because SIMILAR TO is a hack.
# the regular expression is preprocessed by a function (similar_escape),
# and the ~ operator for posix regular expressions is used.
#        x SIMILAR TO y     ->    x ~ similar_escape(y)
# this transformation is made on the fly by the parser upwards.
# however the SubLink structure which handles any/some/all stuff
# is not ready for such a thing.
### subquery_Op_6: NOT ILIKE
sub got_subquery_Op_6 { $_[0]->lappend("!~~*") }

### expr_list_1: a_expr
sub got_expr_list_1 { $_[0]->lappend($_[1])        }
### expr_list_2: expr_list <COMMA> a_expr
sub got_expr_list_2 { $_[0]->lappend($_[1], $_[3]) }

### func_arg_list_1: func_arg_expr
sub got_func_arg_list_1 { $_[0]->lappend($_[1])        }
### func_arg_list_2: func_arg_list <COMMA> func_arg_expr
sub got_func_arg_list_2 { $_[0]->lappend($_[1], $_[3]) }

### func_arg_expr_1: a_expr
sub got_func_arg_expr_1 { $_[1] }
### func_arg_expr_2: param_name COLON_EQUALS a_expr
sub got_func_arg_expr_2 {
   return SQL::Translator::Statement::NamedArgExpr->new(
      name      => $_[1],
      arg       => $_[3],
      argnumber => -1,      #* until determined
      location  => $_[0]->YYLLoc($_[1], 1),
   );
}

### type_list_1: Typename
sub got_type_list_1 { $_[0]->lappend($_[1])        }
### type_list_2: type_list <COMMA> Typename
sub got_type_list_2 { $_[0]->lappend($_[1], $_[3]) }

### array_expr_1: <LSQUARE> expr_list <RSQUARE>
sub got_array_expr_1 { $_[0]->makeAArrayExpr($_[2], $_[0]->YYLLoc($_[1], 1)) }
### array_expr_2: <LSQUARE> array_expr_list <RSQUARE>
sub got_array_expr_2 { $_[0]->makeAArrayExpr($_[2], $_[0]->YYLLoc($_[1], 1)) }
### array_expr_3: <LSQUARE> <RSQUARE>
sub got_array_expr_3 { $_[0]->makeAArrayExpr(NIL,   $_[0]->YYLLoc($_[1], 1)) }

### array_expr_list_1: array_expr
sub got_array_expr_list_1 { $_[0]->lappend($_[1])        }
### array_expr_list_2: array_expr_list <COMMA> array_expr
sub got_array_expr_list_2 { $_[0]->lappend($_[1], $_[3]) }

### extract_list_1: extract_arg FROM a_expr
sub got_extract_list_1 { $_[0]->lappend($_[0]->makeStringConst($_[1], $_[0]->YYLLoc($_[1], 1)), $_[3]) }

### extract_arg_1: IDENT
sub got_extract_arg_1 { $_[1]    }
### extract_arg_2: YEAR
sub got_extract_arg_2 { "year"   }
### extract_arg_3: MONTH
sub got_extract_arg_3 { "month"  }
### extract_arg_4: DAY
sub got_extract_arg_4 { "day"    }
### extract_arg_5: HOUR
sub got_extract_arg_5 { "hour"   }
### extract_arg_6: MINUTE
sub got_extract_arg_6 { "minute" }
### extract_arg_7: SECOND
sub got_extract_arg_7 { "second" }
### extract_arg_8: Sconst
sub got_extract_arg_8 { $_[1]    }

### overlay_list_1: a_expr overlay_placing substr_from substr_for
sub got_overlay_list_1 { $_[0]->lappend($_[1], $_[2], $_[3], $_[4]) }
### overlay_list_2: a_expr overlay_placing substr_from
sub got_overlay_list_2 { $_[0]->lappend($_[1], $_[2], $_[3])        }

### overlay_placing: PLACING a_expr
sub got_overlay_placing { $_[2] }

### position_list_1: b_expr IN b_expr
sub got_position_list_1 { $_[0]->lappend($_[3], $_[1]) }

# not legal per SQL99, but might as well allow it
### substr_list_1: a_expr substr_from substr_for
sub got_substr_list_1 { $_[0]->lappend($_[1], $_[2], $_[3]) }
### substr_list_2: a_expr substr_for substr_from
sub got_substr_list_2 { $_[0]->lappend($_[1], $_[3], $_[2]) }
### substr_list_3: a_expr substr_from
sub got_substr_list_3 { $_[0]->lappend($_[1], $_[2]) }
# Since there are no cases where this syntax allows
# a textual FOR value, we forcibly cast the argument
# to int4.  The possible matches in pg_proc are
# substring(text,int4) and substring(text,text),
# and we don't want the parser to choose the latter,
# which it is likely to do if the second argument
# is unknown or doesn't have an implicit cast to int4.
### substr_list_4: a_expr substr_for
sub got_substr_list_4 {
   return $_[0]->lappend(
      $_[1], $_[0]->makeIntConst(1, {}),
      $_[0]->makeTypeCast($_[2], $_[0]->SystemTypeName("int4"), {})
   );
}
### substr_list_5: expr_list
sub got_substr_list_5 { $_[1] }

### substr_from: FROM a_expr
sub got_substr_from { $_[2] }

### substr_for: FOR a_expr
sub got_substr_for { $_[2] }

### trim_list_1: a_expr FROM expr_list
sub got_trim_list_1 { $_[0]->lappend($_[3], $_[1]) }
### trim_list_2: FROM expr_list
sub got_trim_list_2 { $_[2] }
### trim_list_3: expr_list
sub got_trim_list_3 { $_[1] }

### in_expr_1: select_with_parens
sub got_in_expr_1 {
   return SQL::Translator::Statement::SubLink->new(
      subselect => $_[1],
      #* other fields will be filled later
   );
}
### in_expr_2: <LPAREN> expr_list <RPAREN>
sub got_in_expr_2 { $_[2] }

### case_expr: CASE case_arg when_clause_list case_default END
sub got_case_expr {
   return SQL::Translator::Statement::CaseExpr->new(
      casetype  => InvalidOid,  #* not analyzed yet
      arg       => $_[2],
      args      => $_[3],
      defresult => $_[4],
      location  => $_[0]->YYLLoc($_[1], 1),
   );
}

### when_clause_list_1: 
# There must be at least one
    when_clause
sub got_when_clause_list_1 { $_[0]->lappend($_[1])        }
### when_clause_list_2: when_clause_list when_clause
sub got_when_clause_list_2 { $_[0]->lappend($_[1], $_[2]) }

### when_clause: WHEN a_expr THEN a_expr
sub got_when_clause {
   return SQL::Translator::Statement::CaseWhen->new(
      expr     => $_[2],
      result   => $_[4],
      location => $_[0]->YYLLoc($_[1], 1),
   );
}

### case_default_1: ELSE a_expr
sub got_case_default_1 { $_[2] }

### case_arg_1: a_expr
sub got_case_arg_1 { $_[1] }

### columnref_1: ColId
sub got_columnref_1 { $_[0]->makeColumnRef($_[1],   NIL, $_[0]->YYLLoc($_[1], 1)) }
### columnref_2: ColId indirection
sub got_columnref_2 { $_[0]->makeColumnRef($_[1], $_[2], $_[0]->YYLLoc($_[1], 1)) }

### indirection_el_1: <DOT> attr_name
sub got_indirection_el_1 { $_[2] }
### indirection_el_2: <DOT> <STAR>
sub got_indirection_el_2 { SQL::Translator::Statement::A_Star->new() }
### indirection_el_3: <LSQUARE> a_expr <RSQUARE>
sub got_indirection_el_3 {
   return SQL::Translator::Statement::A_Indices->new(
      lidx => NULL,
      uidx => $_[2],
   );
}
### indirection_el_4: <LSQUARE> a_expr <COLON> a_expr <RSQUARE>
sub got_indirection_el_4 {
   return SQL::Translator::Statement::A_Indices->new(
      lidx => $_[2],
      uidx => $_[4],
   );
}

### indirection_1: indirection_el
sub got_indirection_1 { $_[0]->lappend($_[1]) }
### indirection_2: indirection indirection_el
sub got_indirection_2 { $_[0]->lappend($_[1], $_[2]) }

### opt_indirection_1: opt_indirection indirection_el
sub got_opt_indirection_1 { $_[0]->lappend($_[1], $_[2]) }

### ctext_expr_1: a_expr
sub got_ctext_expr_1 {  $_[1] }
### ctext_expr_2: DEFAULT
sub got_ctext_expr_2 {
   return SQL::Translator::Statement::SetToDefault->new(
      location => $_[0]->YYLLoc($_[1], 1),
   );
}

### ctext_expr_list_1: ctext_expr
sub got_ctext_expr_list_1 { $_[0]->lappend($_[1]) }
### ctext_expr_list_2: ctext_expr_list <COMMA> ctext_expr
sub got_ctext_expr_list_2 { $_[0]->lappend($_[1], $_[3]) }

### ctext_row: <LPAREN> ctext_expr_list <RPAREN>
sub got_ctext_row { $_[2] }

### target_list_1: target_el
sub got_target_list_1 { $_[0]->lappend($_[1]) }
### target_list_2: target_list <COMMA> target_el
sub got_target_list_2 { $_[0]->lappend($_[1], $_[3]) }

# We support omitting AS only for column labels that aren't
# any known keyword.  There is an ambiguity against postfix
# operators: is "a ! b" an infix expression, or a postfix
# expression and a column label?  We prefer to resolve this
# as an infix expression, which we accomplish by assigning
# IDENT a precedence higher than POSTFIXOP.
### target_el_1: a_expr AS ColLabel
sub got_target_el_1 {
   return SQL::Translator::Statement::ResultTarget->new(
      name        => $_[3],
      indirection => NIL,
      val         => $_[1],
      location    => $_[0]->YYLLoc($_[1], 1),
   );
}
### target_el_2: a_expr IDENT
sub got_target_el_2 {
   return SQL::Translator::Statement::ResultTarget->new(
      name        => $_[2],
      indirection => NIL,
      val         => $_[1],
      location    => $_[0]->YYLLoc($_[1], 1),
   );
}
### target_el_3: a_expr
sub got_target_el_3 {
   return SQL::Translator::Statement::ResultTarget->new(
      name        => NULL,
      indirection => NIL,
      val         => $_[1],
      location    => $_[0]->YYLLoc($_[1], 1),
   );
}
### target_el_4: <STAR>
sub got_target_el_4 {
   return SQL::Translator::Statement::ResultTarget->new(
      name        => NULL,
      indirection => NIL,
      val         => SQL::Translator::Statement::Column::Reference->new(
         fields   => $_[0]->lappend( SQL::Translator::Statement::A_Star->new() ),
         location => $_[0]->YYLLoc($_[1], 1),
      ),
      location    => $_[0]->YYLLoc($_[1], 1),
   );
}

### qualified_name_list_1: qualified_name
sub got_qualified_name_list_1 { $_[0]->lappend($_[1]) }
### qualified_name_list_2: qualified_name_list <COMMA> qualified_name
sub got_qualified_name_list_2 { $_[0]->lappend($_[1], $_[3]) }

### qualified_name_1: ColId
sub got_qualified_name_1 { $_[0]->makeRangeVar(NULL, $_[1], $_[0]->YYLLoc($_[1], 1)) }
### qualified_name_2: ColId indirection
sub got_qualified_name_2 {
   $_[0]->check_qualified_name($_[2]);
   my $n = $_[0]->makeRangeVar(NULL, NULL, $_[0]->YYLLoc($_[1], 1));
   for (scalar @{$_[2]}) {
      when (1) {
         $n->catalogname(NULL);
         $n->schemaname ($_[1]);
         $n->relname    ($_[2]->[0]);
      }
      when (2) {
         $n->catalogname($_[1]);
         $n->schemaname ($_[2]->[0]);
         $n->relname    ($_[2]->[1]);
      }
      default {
         $_[0]->ereport(ERROR, ERRCODE_SYNTAX_ERROR,
            sprintf("improper qualified name (too many dotted names): %s", NameListToString($_[0]->lcons($_[1], $_[2]))),
            $_[0]->YYLLoc($_[1], 1));
      }
   }
   return $n;
}

### name_list_1: name
sub got_name_list_1 { $_[0]->lappend($_[1]) }
### name_list_2: name_list <COMMA> name
sub got_name_list_2 { $_[0]->lappend($_[1], $_[3]) }

### name: ColId
sub got_name { $_[1] }

### database_name: ColId
sub got_database_name { $_[1] }

### access_method: ColId
sub got_access_method { $_[1] }

### attr_name: ColLabel
sub got_attr_name { $_[1] }

### index_name: ColId
sub got_index_name { $_[1] }

### file_name: Sconst
sub got_file_name { $_[1] }

### func_name_1: type_function_name
sub got_func_name_1 { $_[0]->lappend($_[1]) }
### func_name_2: ColId indirection
sub got_func_name_2 { $_[0]->check_func_name($_[0]->lcons($_[1], $_[2])) }

### AexprConst_1 : Iconst
sub got_AexprConst_1  { $_[0]->makeIntConst      ($_[1], $_[0]->YYLLoc($_[1], 1)) }
### AexprConst_2 : FCONST
sub got_AexprConst_2  { $_[0]->makeFloatConst    ($_[1], $_[0]->YYLLoc($_[1], 1)) }
### AexprConst_3 : Sconst
sub got_AexprConst_3  { $_[0]->makeStringConst   ($_[1], $_[0]->YYLLoc($_[1], 1)) }
# This is a bit constant per SQL99:
# Without Feature F511, "BIT data type",
# a <general literal> shall not be a
# <bit string literal> or a <hex string literal>.
### AexprConst_4 : BCONST
sub got_AexprConst_4  { $_[0]->makeBitStringConst($_[1], $_[0]->YYLLoc($_[1], 1)) }
### AexprConst_5 : XCONST
sub got_AexprConst_5  { $_[0]->makeBitStringConst($_[1], $_[0]->YYLLoc($_[1], 1)) }
### AexprConst_6 : func_name Sconst
sub got_AexprConst_6  {
   #* generic type 'literal' syntax
   my $t = $_[0]->makeTypeNameFromNameList($_[1]);
   $t->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $_[0]->makeStringConstCast($_[2], $_[0]->YYLLoc($_[2], 2), $t);
}
### AexprConst_7 : func_name <LPAREN> func_arg_list <RPAREN> Sconst
sub got_AexprConst_7  {
   #* generic syntax with a type modifier
   my $t = $_[0]->makeTypeNameFromNameList($_[1]);

   #* We must use func_arg_list in the production to avoid
   #* reduce/reduce conflicts, but we don't actually wish
   #* to allow NamedArgExpr in this context.

   for (@{$_[3]}) {
      if (blessed $_->[0] && $_->[0]->isa('SQL::Translator::Statement::NamedArgExpr')) {
         $_[0]->ereport(ERROR,
                ERRCODE_SYNTAX_ERROR,
                 "type modifier cannot have parameter name",
                 $_[0]->YYLLoc($_[1], 1));
      }
   }
   $t->typmods ($_[3]);
   $t->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $_[0]->makeStringConstCast($_[5], $_[0]->YYLLoc($_[5], 5), $t);
}
### AexprConst_8 : ConstTypename Sconst
sub got_AexprConst_8  { $_[0]->makeStringConstCast($_[2], $_[0]->YYLLoc($_[2], 2), $_[1]) }
### AexprConst_9 : ConstInterval Sconst opt_interval
sub got_AexprConst_9  {
   $_[1]->typmods($_[3]);
   return $_[0]->makeStringConstCast($_[2], $_[0]->YYLLoc($_[2], 2), $_[1]);
}
### AexprConst_10: ConstInterval <LPAREN> Iconst <RPAREN> Sconst opt_interval
sub got_AexprConst_10 {
   my $t = $_[1];
   if (defined $_[6]) {
      if (@{$_[6]} != 1) {
         $_[0]->ereport(ERROR,
               ERRCODE_SYNTAX_ERROR,
                "interval precision specified twice",
                $_[0]->YYLLoc($_[1], 1));
      }
      $t->typmods( $_[0]->lappend( $_[6],                                        $_[0]->makeIntConst($_[3], $_[0]->YYLLoc($_[3], 3)) ) );
   }
   else {
      $t->typmods( $_[0]->lappend( $_[0]->makeIntConst(INTERVAL_FULL_RANGE, {}), $_[0]->makeIntConst($_[3], $_[0]->YYLLoc($_[3], 3)) ) );
   }
   return $_[0]->makeStringConstCast($_[5], $_[0]->YYLLoc($_[5], 5), $t);
}
### AexprConst_11: TRUE
sub got_AexprConst_11 { $_[0]->makeBoolAConst(TRUE,  $_[0]->YYLLoc($_[1], 1)) }
### AexprConst_12: FALSE
sub got_AexprConst_12 { $_[0]->makeBoolAConst(FALSE, $_[0]->YYLLoc($_[1], 1)) }
### AexprConst_13: NULL
sub got_AexprConst_13 { $_[0]->makeNullAConst($_[0]->YYLLoc($_[1], 1))        }

### Iconst: ICONST
sub got_Iconst { $_[1] }

### Sconst: SCONST
sub got_Sconst { $_[1] }

### RoleId: ColId
sub got_RoleId { $_[1] }

### SignedIconst_1: Iconst
sub got_SignedIconst_1 { $_[1] }
### SignedIconst_2: <PLUS> Iconst
sub got_SignedIconst_2 { + $_[2] }
### SignedIconst_3: <DASH> Iconst
sub got_SignedIconst_3 { - $_[2] }

### ColId_1: IDENT
sub got_ColId_1 { $_[1] }
### ColId_2: unreserved_keyword
sub got_ColId_2 { $_[1] }
### ColId_3: col_name_keyword
sub got_ColId_3 { $_[1] }

### type_function_name_1: IDENT
sub got_type_function_name_1 { $_[1] }
### type_function_name_2: unreserved_keyword
sub got_type_function_name_2 { $_[1] }
### type_function_name_3: type_func_name_keyword
sub got_type_function_name_3 { $_[1] }

### ColLabel_1: IDENT
sub got_ColLabel_1 { $_[1] }
### ColLabel_2: unreserved_keyword
sub got_ColLabel_2 { $_[1] }
### ColLabel_3: col_name_keyword
sub got_ColLabel_3 { $_[1] }
### ColLabel_4: type_func_name_keyword
sub got_ColLabel_4 { $_[1] }
### ColLabel_5: reserved_keyword
sub got_ColLabel_5 { $_[1] }

## %% ##

use List::AllUtils 'firstidx';
use Scalar::Util qw/blessed looks_like_number/;

### FIXME: The possibility exists that the whole List -> [] change is flawed.  Some of the notes show that ###
###        lfirst actually means "the data in this cell", which would indicate that List is actually an    ###
###        iterator...                                                                                     ###

#* List *
#* $_[0]->lappend(List *list, void *datum)
### Basically, a push statement with an ref ARRAY check ###
sub lappend {
   my $self = shift;
   # Act like a true 'append', by keeping the array ref, if it exists
   my $arr = ref $_[0] eq 'ARRAY' ? shift : [];
   push @$arr, map { ref $_ eq 'ARRAY' ? @$_ : $_ } @_;
   return $arr;
}

#* List *
#* lcons(void *datum, List *list)
### Basically, an unshift statement with an ref ARRAY check ###
sub lcons {
   my $self = shift;
   # Act like a true 'prepend', by keeping the array ref, if it exists
   my $arr = ref $_[-1] eq 'ARRAY' ? pop : [];
   push @$arr, map { ref $_ eq 'ARRAY' ? @$_ : $_ } @_;
   return $arr;
}

#* makeTypeNameFromNameList -
#*  build a TypeName node for a String list representing a qualified name.
#*
#* typmod is defaulted, but can be changed later by caller.

#* TypeName *
#* makeTypeNameFromNameList(List *names)
sub makeTypeNameFromNameList {
   return SQL::Translator::Statement::TypeName->new(
      names    => $_[1],
      typmods  => [],
      typemod  => -1,
      location => {},
   );
}

#* static Node *
#* makeColumnRef(char *colname, List *indirection, int location, core_yyscan_t yyscanner)
sub makeColumnRef {
   #* Generate a ColumnRef node, with an A_Indirection node added if there
   #* is any subscripting in the specified indirection list.  However,
   #* any field selection at the start of the indirection list must be
   #* transposed into the "fields" part of the ColumnRef node.
   my ($self, $colname, $indirection, $location) = @_;
   my $c = SQL::Translator::Statement::Column::Reference->new(
      location => $location,
   );
   my $nfields = 0;

   foreach my $l (@$indirection)   {
      if    ($l->isa('SQL::Translator::Statement::A_Indices')) {
         my $i = SQL::Translator::Statement::A_Indirection->new();

         if ($nfields == 0) {
            #* easy case - all indirection goes to A_Indirection
            $c->fields([ $colname ]);
            $i->indirection( $self->check_indirection($indirection) );
         }
         else {
            #* got to split the list in two
            $i->indirection( $self->check_indirection(@$indirection[$nfields .. $#{$indirection}]) );
            $c->fields( $indirection = [ $colname, splice(@$indirection, 0, $nfields) ] );
         }
         $i->arg($c);
         return $i;
      }
      else {
         #* We only allow '*' at the end of a ColumnRef
         $self->check_indirection($l);
      }
      $nfields++;
   }
   #* No subscripting, so all indirection gets added to field list
   unshift @$indirection, $colname;
   $c->fields($indirection);
   return $c;
}

#* static Node *
#* makeTypeCast(Node *arg, TypeName *typename, int location)
sub makeTypeCast {
   return SQL::Translator::Statement::TypeCast->new(
      arg      => $_[1],
      typeName => $_[2],
      location => $_[3],
   );
}

#* static Node *
#* makeStringConst(char *str, int location)
sub makeStringConst {
   return SQL::Translator::Statement::A_Const->new(
      type     => 'String',
      val      => $_[1],
      location => $_[2],
   );
}

#* static Node *
#* makeStringConstCast(char *str, int location, TypeName *typename)
sub makeStringConstCast {
   return SQL::Translator::Statement::TypeCast->new(
      arg      => $_[0]->makeStringConst($_[1], $_[2]),
      typeName => $_[3],
      location => {},
   );
}

#* static Node *
#* makeIntConst(int val, int location)
sub makeIntConst {
   return SQL::Translator::Statement::A_Const->new(
      type     => 'Integer',
      val      => $_[1],
      location => $_[2],
   );
}

#* static Node *
#* makeFloatConst(char *str, int location)
sub makeFloatConst {
   return SQL::Translator::Statement::A_Const->new(
      type     => 'Float',
      val      => $_[1],
      location => $_[2],
   );
}

#* static Node *
#* makeBitStringConst(char *str, int location)
sub makeBitStringConst {
   return SQL::Translator::Statement::A_Const->new(
      type     => 'BitString',
      val      => $_[1],
      location => $_[2],
   );
}

#* static Node *
#* makeNullAConst(int location)
sub makeNullAConst {
   return SQL::Translator::Statement::A_Const->new(
      type     => 'NULL',
      location => $_[1],
   );
}

### These two are found in makefuncs.c ###

#* makeA_Expr -
#*      makes an A_Expr node

#* A_Expr *
#* makeA_Expr(A_Expr_Kind kind, List *name,
#*            Node *lexpr, Node *rexpr, int location)
sub makeA_Expr {
   return SQL::Translator::Statement::A_Expr->new(
      kind     => $_[1],
      name     => $_[2],
      lexpr    => $_[3],
      rexpr    => $_[4],
      location => $_[5],
   );
}

#* makeSimpleA_Expr -
#*      As above, given a simple (unqualified) operator name

#* A_Expr *
#* makeSimpleA_Expr(A_Expr_Kind kind, char *name,
#*                  Node *lexpr, Node *rexpr, int location)
sub makeSimpleA_Expr { return $_[0]->makeA_Expr($_[1], [ $_[2] ], @_[3,4,5]); }

#* static Node *
#* makeAConst(Value *v, int location)
### This one is a little different in that we have to guess the type,
### since we aren't using the full 'Value' format.  (See notes in
### _classes.pm for A_Const.)

### Fortunately, this is only used in two places, and it shouldn't
### be a string...
sub makeAConst {
   return SQL::Translator::Statement::A_Const->new(
      type     =>
         looks_like_number $_[1] ? (
            blessed $_[1] && $_[1]->isa('Math::BigFloat') || $_[1] =~ /[\.e]/i ?
            'Float' : 'Integer'
         ) : 'String',
      val      => $_[1],
      location => $_[2],
   );
}

#* makeBoolAConst()
#* Create an A_Const string node and put it inside a boolean cast.

#* static Node *
#* makeBoolAConst(bool state, int location)
sub makeBoolAConst {
   return SQL::Translator::Statement::TypeCast->new(
      arg      => $_[0]->makeStringConst( ($_[1] ? 'T' : 'F'), $_[2]),
      typeName => $_[0]->SystemTypeName("bool"),
      location => {},
   );
}

#* makeOverlaps()
#* Create and populate a FuncCall node to support the OVERLAPS operator.

#* static FuncCall *
#* makeOverlaps(List *largs, List *rargs, int location, core_yyscan_t yyscanner)
sub makeOverlaps {
   my ($self, $largs, $rargs, $loc);

   $largs = [ $largs->[0], $largs->[0] ] if (@$largs == 1);
   $rargs = [ $rargs->[0], $rargs->[0] ] if (@$rargs == 1);

   $self->ereport(ERROR,
      ERRCODE_SYNTAX_ERROR,
      "Wrong number of parameters on left side of OVERLAPS expression",
      $loc
   ) unless (@$largs == 2);
   $self->ereport(ERROR,
      ERRCODE_SYNTAX_ERROR,
      "Wrong number of parameters on right side of OVERLAPS expression",
      $loc
   ) unless (@$rargs == 2);

   return SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[0]->SystemFuncName("overlaps"),
      args          => [ @$largs, @$rargs ],
      agg_order     => NIL,
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => FALSE,
      over          => NULL,
      location      => $loc,
   );
}

#* check_qualified_name --- check the result of qualified_name production
#*
#* It's easiest to let the grammar production for qualified_name allow
#* subscripts and '*', which we then must reject here.

#* static void
#* check_qualified_name(List *names, core_yyscan_t yyscanner)
sub check_qualified_name {
   my ($self, $names) = @_;
   foreach my $i (@$names) {
      $self->YYError("syntax error")
         if blessed $i->[0];
   }
}

#* check_func_name --- check the result of func_name production
#*
#* It's easiest to let the grammar production for func_name allow subscripts
#* and '*', which we then must reject here.

#* static List *
#* check_func_name(List *names, core_yyscan_t yyscanner)
### Pfft... this is check_qualified_name with a $names return... ###
sub check_func_name {
   $_[0]->check_qualified_name($_[1]);
   return $_[1];
}

#* check_indirection --- check the result of indirection production
#*
#* We only allow '*' at the end of the list, but it's hard to enforce that
#* in the grammar, so do it here.

#* static List *
#* check_indirection(List *indirection, core_yyscan_t yyscanner)
sub check_indirection {
   my ($self, $list) = @_;
   return $list unless (defined $list and ref $list eq 'ARRAY');
   foreach my $l (0 .. @$list-1) {
      my $i = $list->[$l];
      $self->YYError('improper use of "*"')
         if (blessed $i && $i->isa('SQL::Translator::Statement::A_Star') && @$list-1 != $i);
   }
   return $list;
}

#* extractArgTypes()
#* Given a list of FunctionParameter nodes, extract a list of just the
#* argument types (TypeNames) for input parameters only.  This is what
#* is needed to look up an existing function, which is what is wanted by
#* the productions that use this call.

#* static List *
#* extractArgTypes(List *parameters)
sub extractArgTypes {
   my ($self, $parameters) = @_;
   my $result = [];
   return $result unless (defined $parameters and ref $parameters eq 'ARRAY');

   foreach my $i (@$parameters) {
      next unless (defined $i and ref $i eq 'ARRAY');
      my $p = $i->[0];

      push(@$result, $p->argType)
         if ($p->mode != FUNC_PARAM_OUT && $p->mode != FUNC_PARAM_TABLE);
   }
   return $result;
}

#* findLeftmostSelect()
#* Find the leftmost component SelectStmt in a set-operation parsetree.

#* static SelectStmt *
#* findLeftmostSelect(SelectStmt *node)

### FIXME: Remove this! ###

#sub findLeftmostSelect {
#   my ($self, $node) = @_;
#   while (blessed $node && $node->op != SETOP_NONE) {
#      $node = $node->larg;
#   }
#   die "SelectStmt assertion failed!"
#      unless (blessed $node && $node->isa('SQL::Translator::Statement::Select') && defined $node->larg);
#   return $node;
#}



#* insertSelectOptions()
#* Insert ORDER BY, etc into an already-constructed SelectStmt.
#*
#* This routine is just to avoid duplicating code in SelectStmt productions.

#* static void
#* insertSelectOptions(SelectStmt *stmt,
#*                List *sortClause, List *lockingClause,
#*                Node *limitOffset, Node *limitCount,
#*                WithClause *withClause,
#*                core_yyscan_t yyscanner)
sub insertSelectOptions {
   my ($self, $stmt, $sortClause, $lockingClause, $limitOffset, $limitCount, $withClause) = @_;
   die "SelectStmt assertion failed!"
      unless (blessed $stmt && $stmt->isa('SQL::Translator::Statement::Select'));

   #* Tests here are to reject constructs like
   #*   (SELECT foo ORDER BY bar) ORDER BY baz

   ### ???: Translate Pg's exprLocation sub? ###
   if ($sortClause) {
      $self->ereport(ERROR,
         ERRCODE_SYNTAX_ERROR,
         "Multiple ORDER BY clauses not allowed",
         $sortClause->node->location
      ) if ($stmt->sortClause);
      $stmt->sortClause($sortClause);
   }
   #* We can handle multiple locking clauses, though
   $stmt->lockingClause( $self->lappend($stmt->lockingClause, $lockingClause) );
   if ($limitOffset) {
      $self->ereport(ERROR,
         ERRCODE_SYNTAX_ERROR,
         "Multiple OFFSET clauses not allowed",
         $limitOffset->location
      ) if ($stmt->limitOffset);
      $stmt->limitOffset($limitOffset);
   }
   if ($limitCount) {
      $self->ereport(ERROR,
         ERRCODE_SYNTAX_ERROR,
         "Multiple LIMIT clauses not allowed",
         $limitCount->location
      ) if ($stmt->limitCount);
      $stmt->limitCount($limitCount);
   }
   if ($withClause) {
      $self->ereport(ERROR,
         ERRCODE_SYNTAX_ERROR,
         "Multiple WITH clauses not allowed",
         $withClause->location
      ) if ($stmt->withClause);
      $stmt->withClause($limitCount);
   }
}

#* static Node *
#* makeSetOp(SetOperation op, bool all, Node *larg, Node *rarg)
sub makeSetOp {
   return SQL::Translator::Statement::Select->new(
      op   => $_[1],
      all  => $_[2],
      larg => $_[3],
      rarg => $_[4],
   );
}

#* SystemFuncName()
#* Build a properly-qualified reference to a built-in function.

#* List *
#* SystemFuncName(char *name)
sub SystemFuncName { [ "pg_catalog", $_[1] ]; }   ### XXX: Kill this... ###

#* SystemTypeName()
#* Build a properly-qualified reference to a built-in type.
#*
#* typmod is defaulted, but may be changed afterwards by caller.
#* Likewise for the location.

#* TypeName *
#* SystemTypeName(char *name)
sub SystemTypeName { $_[0]->makeTypeNameFromNameList([ "pg_catalog", $_[1] ]); }

#* doNegate()
#* Handle negation of a numeric constant.
#*
#* Formerly, we did this here because the optimizer couldn't cope with
#* indexquals that looked like "var = -4" --- it wants "var = const"
#* and a unary minus operator applied to a constant didn't qualify.
#* As of Postgres 7.0, that problem doesn't exist anymore because there
#* is a constant-subexpression simplifier in the optimizer.  However,
#* there's still a good reason for doing this here, which is that we can
#* postpone committing to a particular internal representation for simple
#* negative constants.   It's better to leave "-123.456" in string form
#* until we know what the desired type is.

#* static Node *
#* doNegate(Node *n, int location)
sub doNegate {
   my ($self, $n, $loc) = @_;
   if (blessed $n && $n->isa('SQL::Translator::Statement::A_Const')) {
      #* report the constant's location as that of the '-' sign
      $n->location = $loc;

      ### (No need for 'doNegateFloat'... Perl can handle it just fine!) ###
      if ($n->type =~ /Integer|Float/) {
         $n->val(-$n->val);
         return $n;
      }
   }

   return $self->makeSimpleA_Expr(AEXPR_OP, "-", NULL, $n, $loc);
}

#* static Node *
#* makeAArrayExpr(List *elements, int location)
sub makeAArrayExpr {
   return SQL::Translator::Statement::A_ArrayExpr->new(
      elements => $_[1],
      location => $_[2],
   );
}

#* static Node *
#* makeXmlExpr(XmlExprOp op, char *name, List *named_args, List *args,
#*          int location)
sub makeXmlExpr {
   return SQL::Translator::Statement::XMLExpr->new(
      op         => $_[1],
      name       => $_[2],
      #* named_args is a list of ResTarget; it'll be split apart into separate
      #* expression and name lists in transformXmlExpr().
      named_args => $_[3],
      arg_names  => NIL,
      args       => $_[4],
      #* xmloption, if relevant, must be filled in by caller
      #* type and typmod will be filled in during parse analysis
      location   => $_[5],
   );
}

#* Merge the input and output parameters of a table function.

#* static List *
#* mergeTableFuncParameters(List *func_args, List *columns)
sub mergeTableFuncParameters {
   my ($self, $func_args, $columns) = @_;

   #* Explicit OUT and INOUT parameters shouldn't be used in this syntax
   foreach my $lc (@$func_args) {
      my $p = $lc->[0];

      $self->ereport(ERROR,
         ERRCODE_SYNTAX_ERROR,
         "OUT and INOUT arguments aren't allowed in TABLE functions",
         $lc->location
      ) if ($p->mode != FUNC_PARAM_IN && $p->mode != FUNC_PARAM_VARIADIC);
   }

   return [ @$func_args, @$columns ];
}

#* Determine return type of a TABLE function.  A single result column
#* returns setof that column's type; otherwise return setof record.

#* static TypeName *
#* TableFuncTypeName(List *columns)
sub TableFuncTypeName {
   my ($self, $columns) = @_;
   my $result;

   if (@$columns == 1) {
      my $p = $columns->[0];
      $result = dclone($p->argType);
   }
   else { $result = $_[0]->SystemTypeName("record"); }

   $result->setof(TRUE);
   return $result;
}

### This is actually in makefuncs.c ###

#* makeRangeVar -
#*    creates a RangeVar node (rather oversimplified case)

#* RangeVar *
#* makeRangeVar(char *schemaname, char *relname, int location)
sub makeRangeVar {
   return SQL::Translator::Statement::RangeVar->new(
      catalogname    => NULL,
      schemaname     => $_[1],
      relname        => $_[2],
      inhOpt         => INH_DEFAULT,
      relpersistence => RELPERSISTENCE_PERMANENT,
      alias          => NULL,
      location       => $_[3],
   );
}

#* Convert a list of (dotted) names to a RangeVar (like
#* makeRangeVarFromNameList, but with position support).  The
#* "AnyName" refers to the any_name production in the grammar.

#* static RangeVar *
#* makeRangeVarFromAnyName(List *names, int position, core_yyscan_t yyscanner)
sub makeRangeVarFromAnyName {
   my ($self, $names, $loc) = @_;
   my $r = SQL::Translator::Statement::RangeVar->new(
      relpersistence => RELPERSISTENCE_PERMANENT,
      location       => $loc,
   );

   for (scalar @$names) {
      when (1) {
         $r->catalogname(NULL);
         $r->schemaname (NULL);
         $r->relname    ($names->[0].'');
      }
      when (2) {
         $r->catalogname(NULL);
         $r->schemaname ($names->[0].'');
         $r->relname    ($names->[1].'');
      }
      when (3) {
         $r->catalogname($names->[0].'');
         $r->schemaname ($names->[1].'');
         $r->relname    ($names->[2].'');
      }
      default {
         $self->ereport(ERROR,
            ERRCODE_SYNTAX_ERROR,
            "Improper qualified name (too many dotted names): ".join(', ', @$names),
            "A qualified name has a maximum of three parts.",
            $loc
         );
      }
   }

   return $r;
}

#* Separate Constraint nodes from COLLATE clauses in a ColQualList

#* static void
#* SplitColQualList(List *qualList,
#*              List **constraintList, CollateClause **collClause,
#*              core_yyscan_t yyscanner)
sub SplitColQualList {
   ### It's easier to just pass the node object... ###
   my ($self, $qualList, $n) = @_;

   my $cons = $n->constraints([]);
   foreach my $l (@$qualList) {
      if    ($l->isa('SQL::Translator::Statement::Constraint')) {
         push @$cons, $l;
      }
      elsif ($l->isa('SQL::Translator::Statement::CollateClause')) {
         $self->ereport(ERROR,
            ERRCODE_SYNTAX_ERROR,
            "Multiple COLLATE clauses not allowed",
            $l->location
         ) if ($n->collClause);
         $n->collClause($l);
      }
      else {
         my $type = blessed $l;
         $type =~ s/^SQL::Convertor::SPIF:://;
         $self->YYError("unexpected node type $type");
      }
   }
}


#* Process result of ConstraintAttributeSpec, and set appropriate bool flags
#* in the output command node.  Pass NULL for any flags the particular
#* command doesn't support.
#* static void

#* processCASbits(int cas_bits, int location, const char *constrType,
#*             bool *deferrable, bool *initdeferred, bool *not_valid,
#*             bool *no_inherit, core_yyscan_t yyscanner)
sub processCASbits {
   my ($self, $cas_bits, $location, $constr_type, $n,
       $can_deferrable, $can_initdeferred, $can_not_valid, $can_no_inherit) = @_;

   if ($cas_bits & (CAS_DEFERRABLE | CAS_INITIALLY_DEFERRED)) {
      $can_deferrable
         or $self->ereport(ERROR,
            ERRCODE_FEATURE_NOT_SUPPORTED,
            #* translator: %s is CHECK, UNIQUE, or similar
            sprintf("%s constraints cannot be marked DEFERRABLE", $constr_type),
            $location);
      $n->deferrable(TRUE);
   }

   if ($cas_bits & CAS_INITIALLY_DEFERRED) {
      $can_initdeferred
         or $self->ereport(ERROR,
            ERRCODE_FEATURE_NOT_SUPPORTED,
            #* translator: %s is CHECK, UNIQUE, or similar
            sprintf("%s constraints cannot be marked DEFERRABLE", $constr_type),
            $location);
      $n->initdeferred(TRUE);
   }

   if ($cas_bits & CAS_NOT_VALID) {
      $can_not_valid
         or $self->ereport(ERROR,
            ERRCODE_FEATURE_NOT_SUPPORTED,
            #* translator: %s is CHECK, UNIQUE, or similar
            sprintf("%s constraints cannot be marked NOT VALID", $constr_type),
            $location);
      $n->not_valid(TRUE);
   }

   if ($cas_bits & CAS_NO_INHERIT) {
      $can_no_inherit
         or $self->ereport(ERROR,
            ERRCODE_FEATURE_NOT_SUPPORTED,
            #* translator: %s is CHECK, UNIQUE, or similar
            sprintf("%s constraints cannot be marked NO INHERIT", $constr_type),
            $location);
      $n->not_valid(TRUE);
   }
}

#* makeDefElem -
#*  build a DefElem node
#*
#* This is sufficient for the "typical" case with an unqualified option name
#* and no special action.

#* DefElem *
#* makeDefElem(char *name, Node *arg)
sub makeDefElem { $_[0]->makeDefElemExtended(NULL, $_[1], $_[2], DEFELEM_UNSPEC) }

#* makeDefElemExtended -
#*  build a DefElem node with all fields available to be specified

#* DefElem *
#* makeDefElemExtended(char *nameSpace, char *name, Node *arg,
#*                     DefElemAction defaction)
sub makeDefElemExtended {
   return SQL::Translator::Statement::DefElem->new(
      defnamespace => $_[1],
      defname      => $_[2],
      arg          => $_[3],
      defaction    => $_[4],
   );
}

__PACKAGE__->lexer(\&SQL::Translator::Lexer::Pg::Filtered_Lexer);
