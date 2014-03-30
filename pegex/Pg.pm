package SQL::Transform::Parser::Pg::AST;

use Moo;

extends 'Pegex::Tree';

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

sub got_stmtblock { $_[0]->{parsetree} = $_[1]; }
sub got_stmtmulti { defined $_[3] ? $_[0]->lappend($_[1], $_[3]) : $_[1] }
sub got_CreateRoleStmt {
   return SQL::Translator::Statement::CreateRole->new(
      stmt_type => ROLESTMT_ROLE,
      role      => $_[3],
      options   => $_[5],
   );
}
sub got_opt_with {}
sub got_OptRoleList { $_[0]->lappend($_[1], $_[2]) }
sub got_AlterOptRoleList { $_[0]->lappend($_[1], $_[2]) }
sub got_AlterOptRoleElem_1 { $_[0]->makeDefElem("password",            $_[2] ) }
sub got_AlterOptRoleElem_2 { $_[0]->makeDefElem("password",            'NULL') }
sub got_AlterOptRoleElem_3 { $_[0]->makeDefElem("encryptedPassword",   $_[3] ) }
sub got_AlterOptRoleElem_4 { $_[0]->makeDefElem("unencryptedPassword", $_[3] ) }
sub got_AlterOptRoleElem_5 { $_[0]->makeDefElem("inherit",             'TRUE') }
sub got_AlterOptRoleElem_6 { $_[0]->makeDefElem("connectionlimit",     $_[3] ) }
# Supported but not documented for roles, for use by ALTER GROUP.
sub got_AlterOptRoleElem_7 { $_[0]->makeDefElem("validUntil",          $_[3] ) }
sub got_AlterOptRoleElem_8 { $_[0]->makeDefElem("rolemembers",         $_[2] ) }
# We handle identifiers that aren't parser keywords with
# the following special-case codes, to avoid bloating the
# size of the main parser.
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
sub got_CreateOptRoleElem_1 { $_[1] }
sub got_CreateOptRoleElem_2 { $_[0]->makeDefElem("sysid",        $_[2]) }
sub got_CreateOptRoleElem_3 { $_[0]->makeDefElem("adminmembers", $_[2]) }
sub got_CreateOptRoleElem_4 { $_[0]->makeDefElem("rolemembers",  $_[2]) }
sub got_CreateOptRoleElem_5 { $_[0]->makeDefElem("addroleto",    $_[3]) }
sub got_CreateOptRoleElem_6 { $_[0]->makeDefElem("addroleto",    $_[3]) }
sub got_CreateUserStmt {
   return SQL::Translator::Statement::CreateRole->new(
      stmt_type => ROLESTMT_USER,
      role      => $_[3],
      options   => $_[5],
   );
}
sub got_AlterRoleStmt {
  return SQL::Translator::Statement::AlterRole->new(
     role     => $_[3],
     action   => +1,   #* add, if there are members
     options  => $_[5],
  );
}
sub got_opt_in_database { $_[3] }
sub got_AlterRoleSetStmt {
   return SQL::Translator::Statement::AlterRoleSet->new(
      role     => $_[3],
      database => $_[4],
      setstmt  => $_[5],
   );
}
sub got_AlterUserStmt {
  return SQL::Translator::Statement::AlterRole->new(
     role     => $_[3],
     action   => +1,   #* add, if there are members
     options  => $_[5],
  );
}
sub got_AlterUserSetStmt {
   return SQL::Translator::Statement::AlterRoleSet->new(
      role     => $_[3],
      database => NULL,
      setstmt  => $_[4],
   );
}
sub got_DropRoleStmt_1 {
   return SQL::Translator::Statement::DropRole->new(
      missing_ok => FALSE,
      roles      => $_[3],
   );
}
sub got_DropRoleStmt_2 {
   return SQL::Translator::Statement::DropRole->new(
      missing_ok => TRUE,
      roles      => $_[5],
   );
}
sub got_DropUserStmt_1 {
   return SQL::Translator::Statement::DropRole->new(
      missing_ok => FALSE,
      roles      => $_[3],
   );
}
sub got_DropUserStmt_2 {
   return SQL::Translator::Statement::DropRole->new(
      roles      => $_[5],
      missing_ok => TRUE,
   );
}
sub got_CreateGroupStmt {
   return SQL::Translator::Statement::CreateRole->new(
      stmt_type => ROLESTMT_GROUP,
      role      => $_[3],
      options   => $_[5],
   );
}
sub got_AlterGroupStmt {
   return SQL::Translator::Statement::AlterRole->new(
      role    => $_[3],
      action  => $_[4],
      options => $_[0]->lappend( $_[0]->makeDefElem("rolemembers", $_[6]) ),
   );
}
sub got_add_drop_1 { +1 }
sub got_add_drop_2 { -1 }
sub got_DropGroupStmt_1 {
   return SQL::Translator::Statement::DropRole->new(
      missing_ok => FALSE,
      roles      => $_[3],
   );
}
sub got_DropGroupStmt_2 {
   return SQL::Translator::Statement::DropRole->new(
      missing_ok => TRUE,
      roles      => $_[5],
   );
}
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
sub got_CreateSchemaStmt_2 {
   return SQL::Translator::Statement::CreateSchema->new(
      #* ...but not both
      schemaname => $_[3],
      authid     => NULL,
      schemaElts => $_[4],
   );
}
sub got_OptSchemaName { $_[1] }
sub got_OptSchemaEltList { $_[0]->lappend($_[1], $_[2]) }
sub got_VariableSetStmt_1 { $_[2]->is_local(0); $_[2] }
sub got_VariableSetStmt_2 { $_[3]->is_local(1); $_[3] }
sub got_VariableSetStmt_3 { $_[3]->is_local(0); $_[3] }
sub got_set_rest_1 {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_SET_MULTI,
      name => 'TRANSACTION',
      args => $_[2],
   );
}
sub got_set_rest_2 {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_SET_MULTI,
      name => 'SESSION CHARACTERISTICS',
      args => $_[5],
   );
}
sub got_set_rest_more_1  {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_SET_VALUE,
      name => $_[1],
      args => $_[3],
   );
}
sub got_set_rest_more_2  {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_SET_VALUE,
      name => $_[1],
      args => $_[3],
   );
}
sub got_set_rest_more_3  {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_SET_DEFAULT,
      name => $_[1],
   );
}
sub got_set_rest_more_4  {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_SET_DEFAULT,
      name => $_[1],
   );
}
# Special syntaxes mandated by SQL standard:
sub got_set_rest_more_5  {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_SET_CURRENT,
      name => $_[1],
   );
}
sub got_set_rest_more_6  {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_SET_VALUE,
      name => 'timezone',
      (defined $_[3]) ?
         (args => $_[0]->lappend($_[3])) :
         (kind => VAR_SET_DEFAULT)
   );
}
sub got_set_rest_more_7  {
   $_[0]->ereport(ERROR,
         ERRCODE_FEATURE_NOT_SUPPORTED,
          "current database cannot be changed",
          $_[0]->YYLLoc($_[2], 2));
   return NULL; #*not reached
}
sub got_set_rest_more_8  {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_SET_VALUE,
      name => 'search_path',
      args => $_[0]->lappend($_[0]->makeStringConst($_[2], $_[0]->YYLLoc($_[2], 2))),
   );
}
sub got_set_rest_more_9  {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_SET_VALUE,
      name => 'client_encoding',
      (defined $_[2]) ?
         (args => $_[0]->lappend($_[0]->makeStringConst($_[2], $_[0]->YYLLoc($_[2], 2)))) :
         (kind => VAR_SET_DEFAULT),
   );
}
sub got_set_rest_more_10 {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_SET_VALUE,
      name => 'role',
      args => $_[0]->lappend($_[0]->makeStringConst($_[2], $_[0]->YYLLoc($_[2], 2))),
   );
}
sub got_set_rest_more_11 {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_SET_VALUE,
      name => 'session_authorization',
      args => $_[0]->lappend($_[0]->makeStringConst($_[3], $_[0]->YYLLoc($_[3], 3))),
   );
}
sub got_set_rest_more_12 {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_SET_DEFAULT,
      name => 'session_authorization',
   );
}
# Special syntaxes invented by PostgreSQL:
sub got_set_rest_more_13 {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_SET_VALUE,
      name => 'xmloption',
      args => $_[0]->lappend($_[0]->makeStringConst($_[3], $_[0]->YYLLoc($_[3], 3))),
   );
}
sub got_set_rest_more_14 {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_SET_MULTI,
      name => 'transaction_snapshot',
      args => [ $_[0]->makeStringConst($_[3], $_[0]->YYLLoc($_[3], 3)) ],
   );
}
sub got_var_name { sprintf("%s.%s", $_[1], $_[3]) }
sub got_var_list { $_[0]->lappend($_[1], $_[3]) }
sub got_var_value_1 { $_[0]->makeStringConst($_[1], $_[0]->YYLLoc($_[1], 1)) }
sub got_var_value_2 { $_[0]->makeAConst     ($_[1], $_[0]->YYLLoc($_[1], 1)) }
sub got_iso_level_1 { "read uncommitted" }
sub got_iso_level_2 { "read committed"   }
sub got_iso_level_3 { "repeatable read"  }
sub got_iso_level_4 { "serializable"     }
sub got_opt_boolean_or_string_1 { "TRUE" }
sub got_opt_boolean_or_string_2 { "FALSE" }
# OFF is also accepted as a boolean value, but is handled
# by the ColId rule below. The action for booleans and strings
# is the same, so we don't need to distinguish them here.
sub got_opt_boolean_or_string_3 { "on" }
sub got_opt_boolean_or_string_4 { $_[1] }
sub got_zone_value_1 { $_[0]->makeStringConst($_[1], $_[0]->YYLLoc($_[1], 1)) }
sub got_zone_value_2 { $_[0]->makeStringConst($_[1], $_[0]->YYLLoc($_[1], 1)) }
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
sub got_zone_value_5 { $_[0]->makeAConst($_[1], $_[0]->YYLLoc($_[1], 1)) }
sub got_zone_value_6 { NULL }
sub got_zone_value_7 { NULL }
sub got_opt_encoding_1 { $_[1] }
sub got_opt_encoding_2 { NULL  }
sub got_ColId_or_Sconst_1 { $_[1] }
sub got_ColId_or_Sconst_2 { $_[1] }
sub got_VariableResetStmt_1 {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_RESET,
      name => $_[2],
   );
}
sub got_VariableResetStmt_2 {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_RESET,
      name => 'timezone',
   );
}
sub got_VariableResetStmt_3 {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_RESET,
      name => 'transaction_isolation',
   );
}
sub got_VariableResetStmt_4 {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_RESET,
      name => 'session_authorization',
   );
}
sub got_VariableResetStmt_5 {
   return SQL::Translator::Statement::VariableSet->new(
      kind => VAR_RESET_ALL,
   );
}
sub got_SetResetClause_1 { $_[2] }
sub got_SetResetClause_2 { $_[1] }
sub got_FunctionSetResetClause_1 { $_[2] }
sub got_FunctionSetResetClause_2 { $_[1] }
sub got_VariableShowStmt_1 {
   return SQL::Translator::Statement::VariableShow->new(
      name => $_[2],
   );
}
sub got_VariableShowStmt_2 {
   return SQL::Translator::Statement::VariableShow->new(
      name => 'timezone',
   );
}
sub got_VariableShowStmt_3 {
   return SQL::Translator::Statement::VariableShow->new(
      name => 'transaction_isolation',
   );
}
sub got_VariableShowStmt_4 {
   return SQL::Translator::Statement::VariableShow->new(
      name => 'session_authorization',
   );
}
sub got_VariableShowStmt_5 {
   return SQL::Translator::Statement::VariableShow->new(
      name => 'all',
   );
}
sub got_ConstraintsSetStmt {
   return SQL::Translator::Statement::ConstraintsSet->new(
      constraints => $_[3],
      deferred    => $_[4],
   );
}
sub got_constraints_set_list_1 { NIL   }
sub got_constraints_set_list_2 { $_[1] }
sub got_constraints_set_mode_1 { TRUE  }
sub got_constraints_set_mode_2 { FALSE }
sub got_CheckPointStmt { SQL::Translator::Statement::CheckPoint->new() }
sub got_DiscardStmt_1 {
   return SQL::Translator::Statement::Discard->new(
      target => DISCARD_ALL,
   );
}
sub got_DiscardStmt_2 {
   return SQL::Translator::Statement::Discard->new(
      target => DISCARD_TEMP,
   );
}
sub got_DiscardStmt_3 {
   return SQL::Translator::Statement::Discard->new(
      target => DISCARD_TEMP,
   );
}
sub got_DiscardStmt_4 {
   return SQL::Translator::Statement::Discard->new(
      target => DISCARD_PLANS,
   );
}
sub got_AlterTableStmt_1 {
   return SQL::Translator::Statement::AlterTable->new(
      relation   => $_[3],
      cmds       => $_[4],
      relkind    => OBJECT_TABLE,
      missing_ok => FALSE,
   );
}
sub got_AlterTableStmt_2 {
   return SQL::Translator::Statement::AlterTable->new(
      relation   => $_[5],
      cmds       => $_[6],
      relkind    => OBJECT_TABLE,
      missing_ok => TRUE,
   );
}
sub got_AlterTableStmt_3 {
   return SQL::Translator::Statement::AlterTable->new(
      relation   => $_[3],
      cmds       => $_[4],
      relkind    => OBJECT_INDEX,
      missing_ok => FALSE,
   );
}
sub got_AlterTableStmt_4 {
   return SQL::Translator::Statement::AlterTable->new(
      relation   => $_[5],
      cmds       => $_[6],
      relkind    => OBJECT_INDEX,
      missing_ok => TRUE,
   );
}
sub got_AlterTableStmt_5 {
   return SQL::Translator::Statement::AlterTable->new(
      relation   => $_[3],
      cmds       => $_[4],
      relkind    => OBJECT_SEQUENCE,
      missing_ok => FALSE,
   );
}
sub got_AlterTableStmt_6 {
   return SQL::Translator::Statement::AlterTable->new(
      relation   => $_[5],
      cmds       => $_[6],
      relkind    => OBJECT_SEQUENCE,
      missing_ok => TRUE,
   );
}
sub got_AlterTableStmt_7 {
   return SQL::Translator::Statement::AlterTable->new(
      relation   => $_[3],
      cmds       => $_[4],
      relkind    => OBJECT_VIEW,
      missing_ok => FALSE,
   );
}
sub got_AlterTableStmt_8 {
   return SQL::Translator::Statement::AlterTable->new(
      relation   => $_[5],
      cmds       => $_[6],
      relkind    => OBJECT_VIEW,
      missing_ok => TRUE,
   );
}
sub got_alter_table_cmds { $_[0]->lappend($_[1], $_[3]) }
# ALTER TABLE <name> ADD COLUMN <coldef>
sub got_alter_table_cmd_1  {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_AddColumn,
      def        => $_[2],
   );
}
# ALTER TABLE <name> ALTER [COLUMN] <colname> {SET DEFAULT <expr>|DROP DEFAULT}
sub got_alter_table_cmd_2  {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_AddColumn,
      def        => $_[3],
   );
}
# ALTER TABLE <name> ALTER [COLUMN] <colname> DROP NOT NULL
sub got_alter_table_cmd_3  {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_ColumnDefault,
      name       => $_[3],
      def        => $_[4],
   );
}
# ALTER TABLE <name> ALTER [COLUMN] <colname> SET NOT NULL
sub got_alter_table_cmd_4  {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_DropNotNull,
      name       => $_[3],
   );
}
# ALTER TABLE <name> ALTER [COLUMN] <colname> SET STATISTICS <SignedIconst>
sub got_alter_table_cmd_5  {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_SetNotNull,
      name       => $_[3],
   );
}
# ALTER TABLE <name> ALTER [COLUMN] <colname> SET ( column_parameter = value [, ... ] )
sub got_alter_table_cmd_6  {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_SetStatistics,
      name       => $_[3],
      def        => $_[6]+0,
   );
}
# ALTER TABLE <name> ALTER [COLUMN] <colname> SET ( column_parameter = value [, ... ] )
sub got_alter_table_cmd_7  {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_SetOptions,
      name       => $_[3],
      def        => $_[5],
   );
}
# ALTER TABLE <name> ALTER [COLUMN] <colname> SET STORAGE <storagemode>
sub got_alter_table_cmd_8  {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_ResetOptions,
      name       => $_[3],
      def        => $_[5],
   );
}
# ALTER TABLE <name> DROP [COLUMN] IF EXISTS <colname> [RESTRICT|CASCADE]
sub got_alter_table_cmd_9  {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_SetStorage,
      name       => $_[3],
      def        => $_[6],
   );
}
# ALTER TABLE <name> DROP [COLUMN] <colname> [RESTRICT|CASCADE]
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
sub got_alter_table_cmd_11 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_DropColumn,
      name       => $_[3],
      behavior   => $_[4],
      missing_ok => FALSE,
   );
}
# ALTER FOREIGN TABLE <name> ALTER [COLUMN] <colname> OPTIONS
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
sub got_alter_table_cmd_13 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype => AT_AlterColumnGenericOptions,
      name    => $_[3],
      def     => $_[4],
   );
}
# ALTER TABLE <name> VALIDATE CONSTRAINT ...
sub got_alter_table_cmd_14 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_AddConstraint,
      def        => $_[2],
   );
}
# ALTER TABLE <name> DROP CONSTRAINT IF EXISTS <name> [RESTRICT|CASCADE]
sub got_alter_table_cmd_15 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_ValidateConstraint,
      name       => $_[3],
   );
}
# ALTER TABLE <name> DROP CONSTRAINT <name> [RESTRICT|CASCADE]
sub got_alter_table_cmd_16 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_DropConstraint,
      name       => $_[5],
      behavior   => $_[6],
      missing_ok => TRUE,
   );
}
# ALTER TABLE <name> SET WITH OIDS
sub got_alter_table_cmd_17 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_DropConstraint,
      name       => $_[3],
      behavior   => $_[4],
      missing_ok => FALSE,
   );
}
# ALTER TABLE <name> SET WITHOUT OIDS
sub got_alter_table_cmd_18 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_AddOids,
   );
}
# ALTER TABLE <name> CLUSTER ON <indexname>
sub got_alter_table_cmd_19 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_DropOids,
   );
}
# ALTER TABLE <name> SET WITHOUT CLUSTER
sub got_alter_table_cmd_20 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_ClusterOn,
      name       => $_[3],
   );
}
# ALTER TABLE <name> ENABLE TRIGGER <trig>
sub got_alter_table_cmd_21 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_DropCluster,
      name       => NULL,
   );
}
# ALTER TABLE <name> ENABLE ALWAYS TRIGGER <trig>
sub got_alter_table_cmd_22 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_EnableTrig,
      name       => $_[3],
   );
}
# ALTER TABLE <name> ENABLE REPLICA TRIGGER <trig>
sub got_alter_table_cmd_23 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_EnableAlwaysTrig,
      name       => $_[4],
   );
}
# ALTER TABLE <name> ENABLE TRIGGER ALL
sub got_alter_table_cmd_24 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_EnableReplicaTrig,
      name       => $_[4],
   );
}
# ALTER TABLE <name> ENABLE TRIGGER USER
sub got_alter_table_cmd_25 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_EnableTrigAll,
   );
}
# ALTER TABLE <name> DISABLE TRIGGER <trig>
sub got_alter_table_cmd_26 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_EnableTrigUser,
   );
}
# ALTER TABLE <name> DISABLE TRIGGER ALL
sub got_alter_table_cmd_27 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_DisableTrig,
      name       => $_[3],
   );
}
# ALTER TABLE <name> DISABLE TRIGGER USER
sub got_alter_table_cmd_28 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_DisableTrigAll,
   );
}
# ALTER TABLE <name> ENABLE RULE <rule>
sub got_alter_table_cmd_29 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_DisableTrigUser,
   );
}
# ALTER TABLE <name> ENABLE ALWAYS RULE <rule>
sub got_alter_table_cmd_30 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_EnableRule,
      name       => $_[3],
   );
}
# ALTER TABLE <name> ENABLE REPLICA RULE <rule>
sub got_alter_table_cmd_31 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_EnableAlwaysRule,
      name       => $_[4],
   );
}
# ALTER TABLE <name> DISABLE RULE <rule>
sub got_alter_table_cmd_32 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_EnableReplicaRule,
      name       => $_[4],
   );
}
# ALTER TABLE <name> INHERIT <parent>
sub got_alter_table_cmd_33 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_DisableRule,
      name       => $_[3],
   );
}
# ALTER TABLE <name> NO INHERIT <parent>
sub got_alter_table_cmd_34 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_AddInherit,
      def        =>  $_[2],
   );
}
# ALTER TABLE <name> OF <type_name>
sub got_alter_table_cmd_35 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_DropInherit,
      def        =>  $_[3],
   );
}
# ALTER TABLE <name> NOT OF
sub got_alter_table_cmd_36 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_AddOf,
      def        => $_[0]->makeTypeNameFromNameList($_[2]),
   );
}
# ALTER TABLE <name> OWNER TO RoleId
sub got_alter_table_cmd_37 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_DropOf,
   );
}
# ALTER TABLE <name> SET TABLESPACE <tablespacename>
sub got_alter_table_cmd_38 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_ChangeOwner,
      name       => $_[3],
   );
}
# ALTER TABLE <name> SET (...)
sub got_alter_table_cmd_39 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_SetTableSpace,
      name       => $_[3],
   );
}
# ALTER TABLE <name> RESET (...)
sub got_alter_table_cmd_40 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_SetRelOptions,
      def        => $_[2],
   );
}
sub got_alter_table_cmd_41 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_ResetRelOptions,
      def        => $_[2],
   );
}
sub got_alter_table_cmd_42 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_GenericOptions,
      def        => $_[1],
   );
}
sub got_alter_column_default_1 { $_[3] }
sub got_alter_column_default_2 { NULL  }
sub got_opt_drop_behavior_1 { DROP_CASCADE  }
sub got_opt_drop_behavior_2 { DROP_RESTRICT }
sub got_opt_collate_clause {
   return SQL::Translator::Statement::CollateClause->new(
      arg      => NULL,
      collname => $_[2],
      location => $_[0]->YYLLoc($_[1], 1),
   );
}
sub got_alter_using { $_[2] }
sub got_reloptions { $_[2] }
sub got_opt_reloptions { $_[2] }
sub got_reloption_list { $_[0]->lappend($_[1], $_[3]) }
sub got_reloption_elem_1 { $_[0]->makeDefElem        ($_[1], $_[3])                        }
sub got_reloption_elem_2 { $_[0]->makeDefElem        ($_[1], NULL)                         }
sub got_reloption_elem_3 { $_[0]->makeDefElemExtended($_[1], $_[3], $_[5], DEFELEM_UNSPEC) }
sub got_reloption_elem_4 { $_[0]->makeDefElemExtended($_[1], $_[3], NULL,  DEFELEM_UNSPEC) }
sub got_AlterCompositeTypeStmt {
   return SQL::Translator::Statement::AlterTable->new(
      #* can't use qualified_name, sigh
      relation => $_[0]->makeRangeVarFromAnyName($_[3], $_[0]->YYLLoc($_[3], 3)),
      cmds     => $_[4],
      relkind  => OBJECT_TYPE,
   );
}
sub got_alter_type_cmds { $_[0]->lappend($_[1], $_[3]) }
# ALTER TYPE <name> DROP ATTRIBUTE IF EXISTS <attname> [RESTRICT|CASCADE]
sub got_alter_type_cmd_1 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_AddColumn,
      def        => $_[3],
      behavior   => $_[4],
   );
}
# ALTER TYPE <name> DROP ATTRIBUTE <attname> [RESTRICT|CASCADE]
sub got_alter_type_cmd_2 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_DropColumn,
      name       => $_[5],
      behavior   => $_[6],
      missing_ok => TRUE,
   );
}
# ALTER TYPE <name> ALTER ATTRIBUTE <attname> [SET DATA] TYPE <typename> [RESTRICT|CASCADE]
sub got_alter_type_cmd_3 {
   return SQL::Translator::Statement::AlterTable::Command->new(
      subtype    => AT_DropColumn,
      name       => $_[3],
      behavior   => $_[4],
      missing_ok => FALSE,
   );
}
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
sub got_ClosePortalStmt_1 {
   return SQL::Translator::Statement::ClosePortal->new(
      portalname => $_[2],
   );
}
sub got_ClosePortalStmt_2 {
   return SQL::Translator::Statement::ClosePortal->new(
      portalname => NULL,
   );
}
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
sub got_copy_from_1 { TRUE  }
sub got_copy_from_2 { FALSE }
sub got_copy_file_name_1 { $_[1] }
sub got_copy_file_name_2 { NULL  }
sub got_copy_file_name_3 { NULL  }
sub got_copy_options_1 { $_[1] }
sub got_copy_options_2 { $_[2] }
sub got_copy_opt_list { $_[0]->lappend($_[1], $_[2]) }
sub got_copy_opt_item_1  { $_[0]->makeDefElem("format",         "binary") }
sub got_copy_opt_item_2  { $_[0]->makeDefElem("oids",           TRUE);     }
sub got_copy_opt_item_3  { $_[0]->makeDefElem("delimiter",      $_[3]);    }
sub got_copy_opt_item_4  { $_[0]->makeDefElem("null",           $_[3]);    }
sub got_copy_opt_item_5  { $_[0]->makeDefElem("format",         "csv");    }
sub got_copy_opt_item_6  { $_[0]->makeDefElem("header",         TRUE);     }
sub got_copy_opt_item_7  { $_[0]->makeDefElem("quote",          $_[3]);    }
sub got_copy_opt_item_8  { $_[0]->makeDefElem("escape",         $_[3]);    }
sub got_copy_opt_item_9  { $_[0]->makeDefElem("force_quote",    $_[3]);    }
sub got_copy_opt_item_10 { $_[0]->makeDefElem("force_quote",    SQL::Translator::Statement::A_Star->new()) }
sub got_copy_opt_item_11 { $_[0]->makeDefElem("force_not_null", $_[4]);    }
sub got_copy_opt_item_12 { $_[0]->makeDefElem("encoding",       $_[2]);    }
sub got_opt_binary { $_[0]->makeDefElem("format",    "binary") }
sub got_opt_oids { $_[0]->makeDefElem("oids",      TRUE);     }
sub got_copy_delimiter { $_[0]->makeDefElem("delimiter", $_[3]);    }
sub got_opt_using {}
sub got_copy_generic_opt_list { $_[0]->lappend($_[1], $_[3]);     }
sub got_copy_generic_opt_elem { $_[0]->makeDefElem($_[1], $_[2]) }
sub got_copy_generic_opt_arg_1 { $_[1] }
sub got_copy_generic_opt_arg_2 { $_[1] }
sub got_copy_generic_opt_arg_3 { SQL::Translator::Statement::A_Star->new() }
sub got_copy_generic_opt_arg_4 { $_[2] }
sub got_copy_generic_opt_arg_list { $_[0]->lappend($_[1], $_[3]) }
sub got_copy_generic_opt_arg_list_item { $_[1] }
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
sub got_OptTemp_1 { RELPERSISTENCE_TEMP      }
sub got_OptTemp_2 { RELPERSISTENCE_TEMP      }
sub got_OptTemp_3 { RELPERSISTENCE_TEMP      }
sub got_OptTemp_4 { RELPERSISTENCE_TEMP      }
sub got_OptTemp_5 { RELPERSISTENCE_TEMP      }
sub got_OptTemp_6 { RELPERSISTENCE_TEMP      }
sub got_OptTemp_7 { RELPERSISTENCE_UNLOGGED  }
sub got_OptTableElementList { $_[1] }
sub got_OptTypedTableElementList { $_[2] }
sub got_TableElementList { $_[0]->lappend($_[1], $_[3]) }
sub got_TypedTableElementList { $_[0]->lappend($_[1], $_[3]) }
sub got_TableElement_1 { $_[1] }
sub got_TableElement_2 { $_[1] }
sub got_TableElement_3 { $_[1] }
sub got_TypedTableElement_1 { $_[1] }
sub got_TypedTableElement_2 { $_[1] }
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
sub got_ColQualList { $_[0]->lappend($_[1], $_[2]) }
sub got_ColConstraint_1 {
   $_[3]->conname($_[2]);
   $_[3]->_set_location($_[0]->YYLLoc($_[1], 1));
   return $_[3];
}
sub got_ColConstraint_2 { $_[1] }
# Note: the CollateClause is momentarily included in
# the list built by ColQualList, but we split it out
# again in SplitColQualList.
sub got_ColConstraint_3 { $_[1] }
sub got_ColConstraint_4 {
   return SQL::Translator::Statement::CollateClause->new(
      arg      => NULL,
      collname => $_[2],
      location => $_[0]->YYLLoc($_[1], 1),
   );
}
sub got_ColConstraintElem_1 {
   return SQL::Translator::Statement::Constraint->new(
      contype         => CONSTR_NOTNULL,
      location        => $_[0]->YYLLoc($_[1], 1),
   );
}
sub got_ColConstraintElem_2 {
   return SQL::Translator::Statement::Constraint->new(
      contype         => CONSTR_NULL,
      location        => $_[0]->YYLLoc($_[1], 1),
   );
}
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
sub got_ColConstraintElem_5 {
   return SQL::Translator::Statement::Constraint->new(
      contype         => CONSTR_CHECK,
      location        => $_[0]->YYLLoc($_[1], 1),
      is_no_inherit   => $_[5],
      raw_expr        => $_[3],
      cooked_expr     => NULL,
   );
}
sub got_ColConstraintElem_6 {
   return SQL::Translator::Statement::Constraint->new(
      contype         => CONSTR_DEFAULT,
      location        => $_[0]->YYLLoc($_[1], 1),
      raw_expr        => $_[2],
      cooked_expr     => NULL,
   );
}
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
sub got_ConstraintAttr_1 {
   return SQL::Translator::Statement::Constraint->new(
      contype  => CONSTR_ATTR_DEFERRABLE,
      location => $_[0]->YYLLoc($_[1], 1),
   );
}
sub got_ConstraintAttr_2 {
   return SQL::Translator::Statement::Constraint->new(
      contype  => CONSTR_ATTR_NOT_DEFERRABLE,
      location => $_[0]->YYLLoc($_[1], 1),
   );
}
sub got_ConstraintAttr_3 {
   return SQL::Translator::Statement::Constraint->new(
      contype  => CONSTR_ATTR_DEFERRED,
      location => $_[0]->YYLLoc($_[1], 1),
   );
}
sub got_ConstraintAttr_4 {
   return SQL::Translator::Statement::Constraint->new(
      contype  => CONSTR_ATTR_IMMEDIATE,
      location => $_[0]->YYLLoc($_[1], 1),
   );
}
sub got_TableLikeClause {
   return SQL::Translator::Statement::TableLikeClause->new(
      relation => $_[2],
      options  => $_[3],
   );
}
sub got_TableLikeOptionList_1 { $_[1] |  $_[3] }
sub got_TableLikeOptionList_2 { $_[1] & ~$_[3] }
sub got_TableLikeOption_1 { CREATE_TABLE_LIKE_DEFAULTS    }
sub got_TableLikeOption_2 { CREATE_TABLE_LIKE_CONSTRAINTS }
sub got_TableLikeOption_3 { CREATE_TABLE_LIKE_INDEXES     }
sub got_TableLikeOption_4 { CREATE_TABLE_LIKE_STORAGE     }
sub got_TableLikeOption_5 { CREATE_TABLE_LIKE_COMMENTS    }
sub got_TableLikeOption_6 { CREATE_TABLE_LIKE_ALL         }
sub got_TableConstraint_1 {
   $_[3]->conname($_[2]);
   $_[3]->_set_location($_[0]->YYLLoc($_[1], 1));
   return $_[3];
}
sub got_TableConstraint_2 { $_[1] }
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
sub got_opt_no_inherit { TRUE  }
sub got_opt_column_list { $_[2] }
sub got_columnList { $_[0]->lappend($_[1], $_[3]) }
sub got_columnElem { $_[1] }
sub got_key_match_1 { FKCONSTR_MATCH_FULL        }
sub got_key_match_2 { FKCONSTR_MATCH_PARTIAL     }
sub got_key_match_3 { FKCONSTR_MATCH_SIMPLE      }
sub got_ExclusionConstraintList { $_[0]->lappend($_[1], $_[3]) }
# allow OPERATOR() decoration for the benefit of ruleutils.c
sub got_ExclusionConstraintElem_1 { $_[0]->lappend($_[1], $_[3]) }
sub got_ExclusionConstraintElem_2 { $_[0]->lappend($_[1], $_[5]) }
sub got_ExclusionWhereClause { $_[3] }
sub got_key_actions_1 { ($_[1] << 8) | (FKCONSTR_ACTION_NOACTION & 0xFF)                    }
sub got_key_actions_2 { (FKCONSTR_ACTION_NOACTION << 8) | ($_[1] & 0xFF)                    }
sub got_key_actions_3 { ($_[1] << 8) | ($_[2] & 0xFF)                                       }
sub got_key_actions_4 { ($_[2] << 8) | ($_[1] & 0xFF)                                       }
sub got_key_update { $_[3] }
sub got_key_delete { $_[3] }
sub got_key_action_1 { FKCONSTR_ACTION_NOACTION   }
sub got_key_action_2 { FKCONSTR_ACTION_RESTRICT   }
sub got_key_action_3 { FKCONSTR_ACTION_CASCADE    }
sub got_key_action_4 { FKCONSTR_ACTION_SETNULL    }
sub got_key_action_5 { FKCONSTR_ACTION_SETDEFAULT }
sub got_OptInherit { $_[3] }
sub got_OptWith_1 { $_[2] }
sub got_OptWith_2 { $_[0]->lappend(defWithOids(TRUE))  }
sub got_OptWith_3 { $_[0]->lappend(defWithOids(FALSE)) }
sub got_OnCommitOption_1 { ONCOMMIT_DROP          }
sub got_OnCommitOption_2 { ONCOMMIT_DELETE_ROWS   }
sub got_OnCommitOption_3 { ONCOMMIT_PRESERVE_ROWS }
sub got_OptTableSpace { $_[2] }
sub got_OptConsTableSpace { $_[4] }
sub got_ExistingIndex { $_[3] }
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
sub got_opt_with_data_1 { TRUE  }
sub got_opt_with_data_2 { FALSE }
sub got_CreateSeqStmt {
   $_[4]->relpersistence($_[2]);
   return SQL::Translator::Statement::CreateSeq->new(
      sequence => $_[4],
      options  => $_[5],
      ownerId  => InvalidOid,
   );
}
sub got_AlterSeqStmt_1 {
   return SQL::Translator::Statement::AlterSeq->new(
      sequence   => $_[3],
      options    => $_[4],
      missing_ok => FALSE,
   );
}
sub got_AlterSeqStmt_2 {
   return SQL::Translator::Statement::AlterSeq->new(
      sequence   => $_[5],
      options    => $_[6],
      missing_ok => TRUE,
   );
}
sub got_OptSeqOptList { $_[1] }
sub got_SeqOptList { $_[0]->lappend($_[1], $_[2]) }
sub got_SeqOptElem_1  { $_[0]->makeDefElem("cache",     $_[2]) }
sub got_SeqOptElem_2  { $_[0]->makeDefElem("cycle",     TRUE)  }
sub got_SeqOptElem_3  { $_[0]->makeDefElem("cycle",     FALSE) }
sub got_SeqOptElem_4  { $_[0]->makeDefElem("increment", $_[3]) }
sub got_SeqOptElem_5  { $_[0]->makeDefElem("maxvalue",  $_[2]) }
sub got_SeqOptElem_6  { $_[0]->makeDefElem("minvalue",  $_[2]) }
sub got_SeqOptElem_7  { $_[0]->makeDefElem("maxvalue",  NULL)  }
sub got_SeqOptElem_8  { $_[0]->makeDefElem("minvalue",  NULL)  }
sub got_SeqOptElem_9  { $_[0]->makeDefElem("owned_by",  $_[3]) }
sub got_SeqOptElem_10 { $_[0]->makeDefElem("start",     $_[3]) }
sub got_SeqOptElem_11 { $_[0]->makeDefElem("restart",   NULL)  }
sub got_SeqOptElem_12 { $_[0]->makeDefElem("restart",   $_[3]) }
sub got_opt_by {}
sub got_NumericOnly_1 { $_[1]+0 }
sub got_NumericOnly_2 { -$_[2]  }
sub got_NumericOnly_3 { $_[1]+0 }
sub got_NumericOnly_list { $_[0]->lappend($_[1], $_[3]) }
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
sub got_opt_trusted { TRUE  }
sub got_handler_name_1 { $_[0]->lappend($_[1])        }
sub got_handler_name_2 { $_[0]->lcons  ($_[1], $_[2]) }
sub got_opt_inline_handler { $_[2] }
sub got_validator_clause_1 { $_[2] }
sub got_validator_clause_2 { NIL   }
sub got_opt_validator { $_[1] }
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
sub got_opt_procedural {}
sub got_CreateTableSpaceStmt {
   return SQL::Translator::Statement::CreateTableSpace->new(
      tablespacename => $_[3],
      owner          => $_[4],
      location       => $_[6],
   );
}
sub got_OptTableSpaceOwner { $_[2] }
sub got_DropTableSpaceStmt_1 {
   return SQL::Translator::Statement::DropTableSpace->new(
      tablespacename => $_[3],
      missing_ok     => FALSE,
   );
}
sub got_DropTableSpaceStmt_2 {
   return SQL::Translator::Statement::DropTableSpace->new(
      tablespacename => $_[5],
      missing_ok     => TRUE,
   );
}
sub got_CreateExtensionStmt_1 {
   return SQL::Translator::Statement::CreateExtension->new(
      extname       => $_[3],
      if_not_exists => FALSE,
      options       => $_[5],
   );
}
sub got_CreateExtensionStmt_2 {
   return SQL::Translator::Statement::CreateExtension->new(
      extname       => $_[6],
      if_not_exists => TRUE,
      options       => $_[8],
   );
}
sub got_create_extension_opt_list { $_[0]->lappend($_[1], $_[2]) }
sub got_create_extension_opt_item_1 { $_[0]->makeDefElem("schema",      $_[2]) }
sub got_create_extension_opt_item_2 { $_[0]->makeDefElem("new_version", $_[2]) }
sub got_create_extension_opt_item_3 { $_[0]->makeDefElem("old_version", $_[2]) }
sub got_AlterExtensionStmt {
   return SQL::Translator::Statement::AlterExtension->new(
      extname => $_[3],
      options => $_[5],
   );
}
sub got_alter_extension_opt_list { $_[0]->lappend($_[1], $_[2]) }
sub got_alter_extension_opt_item { $_[0]->makeDefElem("new_version", $_[2]) }
sub got_AlterExtensionContentsStmt_1  {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_AGGREGATE,
      objname => $_[6],
      objargs => $_[7],
   );
}
sub got_AlterExtensionContentsStmt_2  {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_CAST,
      objname => $_[0]->lappend($_[7]),
      objargs => $_[0]->lappend($_[9]),
   );
}
sub got_AlterExtensionContentsStmt_3  {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_COLLATION,
      objname => $_[6],
   );
}
sub got_AlterExtensionContentsStmt_4  {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_CONVERSION,
      objname => $_[6],
   );
}
sub got_AlterExtensionContentsStmt_5  {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_DOMAIN,
      objname => $_[6],
   );
}
sub got_AlterExtensionContentsStmt_6  {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_FUNCTION,
      objname => $_[6]->funcname,
      objargs => $_[6]->funcargs,
   );
}
sub got_AlterExtensionContentsStmt_7  {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_LANGUAGE,
      objname => $_[0]->lappend($_[7]),
   );
}
sub got_AlterExtensionContentsStmt_8  {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_OPERATOR,
      objname => $_[6],
      objargs => $_[7],
   );
}
sub got_AlterExtensionContentsStmt_9  {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_OPCLASS,
      objname => $_[7],
      objargs => $_[0]->lappend($_[9]),
   );
}
sub got_AlterExtensionContentsStmt_10 {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_OPFAMILY,
      objname => $_[7],
      objargs => $_[0]->lappend($_[9]),
   );
}
sub got_AlterExtensionContentsStmt_11 {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_SCHEMA,
      objname => $_[0]->lappend($_[6]),
   );
}
sub got_AlterExtensionContentsStmt_12 {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_TABLE,
      objname => $_[6],
   );
}
sub got_AlterExtensionContentsStmt_13 {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_EVENT_TRIGGER,
      objname => $_[0]->lappend($_[7]),
   );
}
sub got_AlterExtensionContentsStmt_14 {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_TSPARSER,
      objname => $_[8],
   );
}
sub got_AlterExtensionContentsStmt_15 {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_TSDICTIONARY,
      objname => $_[8],
   );
}
sub got_AlterExtensionContentsStmt_16 {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_TSTEMPLATE,
      objname => $_[8],
   );
}
sub got_AlterExtensionContentsStmt_17 {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_TSCONFIGURATION,
      objname => $_[8],
   );
}
sub got_AlterExtensionContentsStmt_18 {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_SEQUENCE,
      objname => $_[6],
   );
}
sub got_AlterExtensionContentsStmt_19 {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_VIEW,
      objname => $_[6],
   );
}
sub got_AlterExtensionContentsStmt_20 {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_FOREIGN_TABLE,
      objname => $_[7],
   );
}
sub got_AlterExtensionContentsStmt_21 {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_FDW,
      objname => $_[0]->lappend($_[8]),
   );
}
sub got_AlterExtensionContentsStmt_22 {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_FOREIGN_SERVER,
      objname => $_[0]->lappend($_[6]),
   );
}
sub got_AlterExtensionContentsStmt_23 {
   return SQL::Translator::Statement::AlterExtensionContents->new(
      extname => $_[3],
      action  => $_[4],
      objtype => OBJECT_TYPE,
      objname => $_[6],
   );
}
sub got_CreateFdwStmt {
   return SQL::Translator::Statement::CreateFdw->new(
      fdwname      => $_[5],
      func_options => $_[6],
      options      => $_[7],
   );
}
sub got_fdw_option_1 { $_[0]->makeDefElem("handler",   $_[2]) }
sub got_fdw_option_2 { $_[0]->makeDefElem("handler",   NULL)  }
sub got_fdw_option_3 { $_[0]->makeDefElem("validator", $_[2]) }
sub got_fdw_option_4 { $_[0]->makeDefElem("validator", NULL)  }
sub got_fdw_options { $_[0]->lappend($_[1], $_[2]) }
sub got_opt_fdw_options { $_[1] }
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
sub got_AlterFdwStmt_1 {
   return SQL::Translator::Statement::AlterFdw->new(
      fdwname      => $_[5],
      func_options => $_[6],
      options      => $_[7],
   );
}
sub got_AlterFdwStmt_2 {
   return SQL::Translator::Statement::AlterFdw->new(
      fdwname      => $_[5],
      func_options => $_[6],
      options      => NIL,
   );
}
sub got_create_generic_options { $_[3] }
sub got_generic_option_list { $_[0]->lappend($_[1], $_[3]) }
sub got_alter_generic_options { $_[3] }
sub got_alter_generic_option_list { $_[0]->lappend($_[1], $_[3]) }
sub got_alter_generic_option_elem_1 { $_[1] }
sub got_alter_generic_option_elem_2 { $_[2]->defaction(DEFELEM_SET); $_[2]; }
sub got_alter_generic_option_elem_3 { $_[2]->defaction(DEFELEM_ADD); $_[2]; }
sub got_alter_generic_option_elem_4 { $_[0]->makeDefElemExtended(NULL, $_[2], NULL, DEFELEM_DROP) }
sub got_generic_option_elem { $_[0]->makeDefElem($_[1], $_[2]) }
sub got_generic_option_name { $_[1] }
sub got_generic_option_arg { $_[1] }
sub got_CreateForeignServerStmt {
   return SQL::Translator::Statement::CreateForeignServer->new(
      servername => $_[3],
      servertype => $_[4],
      version    => $_[5],
      fdwname    => $_[9],
      options    => $_[10],
   );
}
sub got_opt_type { $_[2] }
sub got_foreign_server_version_1 { $_[2] }
sub got_foreign_server_version_2 { NULL  }
sub got_opt_foreign_server_version { $_[1] }
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
sub got_AlterForeignServerStmt_1 {
   return SQL::Translator::Statement::AlterForeignServer->new(
      servername  => $_[3],
      version     => $_[4],
      options     => $_[5],
      has_version => TRUE,
   );
}
sub got_AlterForeignServerStmt_2 {
   return SQL::Translator::Statement::AlterForeignServer->new(
      servername  => $_[3],
      version     => $_[4],
      has_version => TRUE,
   );
}
sub got_AlterForeignServerStmt_3 {
   return SQL::Translator::Statement::AlterForeignServer->new(
      servername => $_[3],
      options    => $_[4],
   );
}
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
sub got_OptForeignTableElementList_1 { $_[2] }
sub got_OptForeignTableElementList_2 { NIL   }
sub got_ForeignTableElementList { $_[0]->lappend($_[1], $_[3]) }
sub got_ForeignTableElement { $_[1] }
sub got_AlterForeignTableStmt_1 {
   return SQL::Translator::Statement::AlterTable->new(
      relation   => $_[4],
      cmds       => $_[5],
      relkind    => OBJECT_FOREIGN_TABLE,
      missing_ok => FALSE,
   );
}
sub got_AlterForeignTableStmt_2 {
   return SQL::Translator::Statement::AlterTable->new(
      relation   => $_[6],
      cmds       => $_[7],
      relkind    => OBJECT_FOREIGN_TABLE,
      missing_ok => TRUE,
   );
}
sub got_CreateUserMappingStmt {
   return SQL::Translator::Statement::CreateUserMapping->new(
      username   => $_[5],
      servername => $_[7],
      options    => $_[8],
   );
}
sub got_auth_ident_1 { "current_user" }
sub got_auth_ident_2 { "current_user" }
sub got_auth_ident_3 { ($_[1] eq "public") ? NULL : $_[1] }
sub got_DropUserMappingStmt_1 {
   return SQL::Translator::Statement::DropUserMapping->new(
      username   => $_[5],
      servername => $_[7],
      missing_ok => FALSE,
   );
}
sub got_DropUserMappingStmt_2 {
   return SQL::Translator::Statement::DropUserMapping->new(
      username   => $_[7],
      servername => $_[9],
      missing_ok => TRUE,
   );
}
sub got_AlterUserMappingStmt {
   return SQL::Translator::Statement::AlterUserMapping->new(
      username   => $_[5],
      servername => $_[7],
      options    => $_[8],
   );
}
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
sub got_TriggerActionTime_1 { TRIGGER_TYPE_BEFORE }
sub got_TriggerActionTime_2 { TRIGGER_TYPE_AFTER }
sub got_TriggerActionTime_3 { TRIGGER_TYPE_INSTEAD }
sub got_TriggerEvents {
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
sub got_TriggerOneEvent_1 { $_[0]->lappend(TRIGGER_TYPE_INSERT,   NIL)   }
sub got_TriggerOneEvent_2 { $_[0]->lappend(TRIGGER_TYPE_DELETE,   NIL)   }
sub got_TriggerOneEvent_3 { $_[0]->lappend(TRIGGER_TYPE_UPDATE,   NIL)   }
sub got_TriggerOneEvent_4 { $_[0]->lappend(TRIGGER_TYPE_UPDATE,   $_[3]) }
sub got_TriggerOneEvent_5 { $_[0]->lappend(TRIGGER_TYPE_TRUNCATE, NIL)   }
sub got_TriggerForSpec { $_[3] }
sub got_TriggerForOptEach {}
sub got_TriggerForType_1 { TRUE  }
sub got_TriggerForType_2 { FALSE }
sub got_TriggerWhen { $_[3] }
sub got_TriggerFuncArgs_1 { $_[0]->lappend($_[1])        }
sub got_TriggerFuncArgs_2 { $_[0]->lappend($_[1], $_[3]) }
sub got_TriggerFuncArg_1 { $_[1] }
sub got_TriggerFuncArg_2 { $_[1] }
sub got_TriggerFuncArg_3 { $_[1] }
sub got_TriggerFuncArg_4 { $_[1] }
sub got_OptConstrFromTable { $_[2] }
sub got_ConstraintAttributeSpec {
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
sub got_ConstraintAttributeElem_1 { CAS_NOT_DEFERRABLE      }
sub got_ConstraintAttributeElem_2 { CAS_DEFERRABLE          }
sub got_ConstraintAttributeElem_3 { CAS_INITIALLY_IMMEDIATE }
sub got_ConstraintAttributeElem_4 { CAS_INITIALLY_DEFERRED  }
sub got_ConstraintAttributeElem_5 { CAS_NOT_VALID           }
sub got_ConstraintAttributeElem_6 { CAS_NO_INHERIT          }
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
sub got_CreateEventTrigStmt_1 {
   return SQL::Translator::Statement::CreateEventTrig->new(
      trigname   => $_[4],
      eventname  => $_[6],
      whenclause => NULL,
      funcname   => $_[9],
   );
}
sub got_CreateEventTrigStmt_2 {
   return SQL::Translator::Statement::CreateEventTrig->new(
      trigname   => $_[4],
      eventname  => $_[6],
      whenclause => $_[8],
      funcname   => $_[11],
   );
}
sub got_event_trigger_when_list { $_[0]->lappend($_[1], $_[3]) }
sub got_event_trigger_when_item { $_[0]->makeDefElem($_[1], $_[4]) }
sub got_event_trigger_value_list { $_[0]->lappend($_[1], $_[3]) }
sub got_AlterEventTrigStmt {
   return SQL::Translator::Statement::AlterEventTrig->new(
      trigname   => $_[4],
      tgenabled  => $_[5],
   );
}
sub got_enable_trigger_1 { TRIGGER_FIRES_ON_ORIGIN }
sub got_enable_trigger_2 { TRIGGER_FIRES_ON_REPLICA }
sub got_enable_trigger_3 { TRIGGER_FIRES_ALWAYS }
sub got_enable_trigger_4 { TRIGGER_DISABLED }
sub got_CreateAssertStmt {
   my $n = SQL::Translator::Statement::CreateTrig->new(
      trigname     => $_[3],
      args         => [ $_[6] ],
      isconstraint => TRUE,
   );
   $_[0]->processCASbits($_[8], $_[0]->YYLLoc($_[8], 8), "ASSERTION", $n, 1,1,0,0);
   return $n;
}
sub got_DropAssertStmt {
   return SQL::Translator::Statement::Drop->new(
      objects    => [ [ $_[3] ] ],
      arguments  => NIL,
      behavior   => $_[4],
      removeType => OBJECT_TRIGGER,  #* XXX
   );
}
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
sub got_DefineStmt_2  {
   return SQL::Translator::Statement::Define->new(
      kind       => OBJECT_AGGREGATE,
      oldstyle   => TRUE,
      defnames   => $_[3],
      args       => NIL,
      definition => $_[4],
   );
}
sub got_DefineStmt_3  {
   return SQL::Translator::Statement::Define->new(
      kind       => OBJECT_OPERATOR,
      oldstyle   => FALSE,
      defnames   => $_[3],
      args       => NIL,
      definition => $_[4],
   );
}
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
sub got_DefineStmt_5  {
   return SQL::Translator::Statement::Define->new(
      kind       => OBJECT_TYPE,
      oldstyle   => FALSE,
      defnames   => $_[3],
      args       => NIL,
      definition => NIL,
   );
}
sub got_DefineStmt_6  {
   return SQL::Translator::Statement::CompositeType->new(
      #* can't use qualified_name, sigh
      typevar    => $_[0]->makeRangeVarFromAnyName($_[3], $_[0]->YYLLoc($_[3], 3)),
      coldeflist => $_[6],
   );
}
sub got_DefineStmt_7  {
   return SQL::Translator::Statement::CreateEnum->new(
      typeName => $_[3],
      vals     => $_[7],
   );
}
sub got_DefineStmt_8  {
   return SQL::Translator::Statement::CreateRange->new(
      typeName => $_[3],
      params   => $_[6],
   );
}
sub got_DefineStmt_9  {
   return SQL::Translator::Statement::Define->new(
      kind       => OBJECT_TSPARSER,
      args       => NIL,
      defnames   => $_[5],
      definition => $_[6],
   );
}
sub got_DefineStmt_10 {
   return SQL::Translator::Statement::Define->new(
      kind       => OBJECT_TSDICTIONARY,
      args       => NIL,
      defnames   => $_[5],
      definition => $_[6],
   );
}
sub got_DefineStmt_11 {
   return SQL::Translator::Statement::Define->new(
      kind       => OBJECT_TSTEMPLATE,
      args       => NIL,
      defnames   => $_[5],
      definition => $_[6],
   );
}
sub got_DefineStmt_12 {
   return SQL::Translator::Statement::Define->new(
      kind       => OBJECT_TSCONFIGURATION,
      args       => NIL,
      defnames   => $_[5],
      definition => $_[6],
   );
}
sub got_DefineStmt_13 {
   return SQL::Translator::Statement::Define->new(
      kind       => OBJECT_COLLATION,
      args       => NIL,
      defnames   => $_[3],
      definition => $_[4],
   );
}
sub got_DefineStmt_14 {
   return SQL::Translator::Statement::Define->new(
      kind       => OBJECT_COLLATION,
      args       => NIL,
      defnames   => $_[3],
      definition => $_[0]->lappend($_[0]->makeDefElem("from",  $_[5])),
   );
}
sub got_definition { $_[2] }
sub got_def_list { $_[0]->lappend($_[1], $_[3]) }
sub got_def_elem_1 { $_[0]->makeDefElem($_[1],  $_[3]) }
sub got_def_elem_2 { $_[0]->makeDefElem($_[1], NULL)   }
sub got_def_arg_1 { $_[1] }
sub got_def_arg_2 { $_[1] }
sub got_def_arg_3 { $_[1] }
sub got_def_arg_4 { $_[1] }
sub got_def_arg_5 { $_[1] }
sub got_aggr_args_1 { $_[2] }
sub got_aggr_args_2 { NIL   }
sub got_old_aggr_definition { $_[2] }
sub got_old_aggr_list { $_[0]->lappend($_[1], $_[3]) }
sub got_old_aggr_elem { $_[0]->makeDefElem($_[1], $_[3]) }
sub got_opt_enum_val_list { $_[1] }
sub got_enum_val_list { $_[0]->lappend($_[1], $_[3]) }
sub got_AlterEnumStmt_1 {
   return SQL::Translator::Statement::AlterEnum->new(
      typeName       => $_[3],
      newVal         => $_[6],
      newValNeighbor => NULL,
      newValIsAfter  => TRUE,
   );
}
sub got_AlterEnumStmt_2 {
   return SQL::Translator::Statement::AlterEnum->new(
      typeName       => $_[3],
      newVal         => $_[6],
      newValNeighbor => $_[8],
      newValIsAfter  => FALSE,
   );
}
sub got_AlterEnumStmt_3 {
   return SQL::Translator::Statement::AlterEnum->new(
      typeName       => $_[3],
      newVal         => $_[6],
      newValNeighbor => $_[8],
      newValIsAfter  => TRUE,
   );
}
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
sub got_opclass_item_list { $_[0]->lappend($_[1], $_[3]) }
sub got_opclass_item_1 {
   return SQL::Translator::Statement::CreateOpClass::Item->new(
      itemtype     => OPCLASS_ITEM_OPERATOR,
      name         => $_[3],
      args         => NIL,
      number       => $_[2],
      order_family => $_[4],
   );
}
sub got_opclass_item_2 {
   return SQL::Translator::Statement::CreateOpClass::Item->new(
      itemtype     => OPCLASS_ITEM_OPERATOR,
      name         => $_[3],
      args         => $_[4],
      number       => $_[2],
      order_family => $_[5],
   );
}
sub got_opclass_item_3 {
   return SQL::Translator::Statement::CreateOpClass::Item->new(
      itemtype     => OPCLASS_ITEM_FUNCTION,
      name         => $_[3],
      args         => $_[0]->extractArgTypes($_[4]),
      number       => $_[2],
   );
}
sub got_opclass_item_4 {
   return SQL::Translator::Statement::CreateOpClass::Item->new(
      itemtype     => OPCLASS_ITEM_FUNCTION,
      name         => $_[6],
      args         => $_[0]->extractArgTypes($_[7]),
      number       => $_[2],
      class_args   => $_[4],
   );
}
sub got_opclass_item_5 {
   return SQL::Translator::Statement::CreateOpClass::Item->new(
      itemtype     => OPCLASS_ITEM_STORAGETYPE,
      storedtype   => $_[2],
   );
}
sub got_opt_default { TRUE  }
sub got_opt_opfamily { $_[2] }
sub got_opclass_purpose_1 { NULL  }
sub got_opclass_purpose_2 { $_[4] }
sub got_opt_recheck { TRUE  }
sub got_CreateOpFamilyStmt {
   return SQL::Translator::Statement::CreateOpFamily->new(
      opfamilyname => $_[4],
      amname       => $_[6],
   );
}
sub got_AlterOpFamilyStmt_1 {
   return SQL::Translator::Statement::AlterOpFamily->new(
      opfamilyname => $_[4],
      amname       => $_[6],
      isDrop       => FALSE,
      items        => $_[8],
   );
}
sub got_AlterOpFamilyStmt_2 {
   return SQL::Translator::Statement::AlterOpFamily->new(
      opfamilyname => $_[4],
      amname       => $_[6],
      isDrop       => TRUE,
      items        => $_[8],
   );
}
sub got_opclass_drop_list { $_[0]->lappend($_[1], $_[3]) }
sub got_opclass_drop_1 {
   return SQL::Translator::Statement::CreateOpClass::Item->new(
      itemtype => OPCLASS_ITEM_OPERATOR,
      number   => $_[2],
      args     => $_[4],
   );
}
sub got_opclass_drop_2 {
   return SQL::Translator::Statement::CreateOpClass::Item->new(
      itemtype => OPCLASS_ITEM_FUNCTION,
      number   => $_[2],
      args     => $_[4],
   );
}
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
sub got_DropOwnedStmt {
   return SQL::Translator::Statement::DropOwned->new(
      roles    => $_[4],
      behavior => $_[5],
   );
}
sub got_ReassignOwnedStmt {
   return SQL::Translator::Statement::ReassignOwned->new(
      roles    => $_[4],
      newrole  => $_[6],
   );
}
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
sub got_drop_type_1  { OBJECT_TABLE           }
sub got_drop_type_2  { OBJECT_SEQUENCE        }
sub got_drop_type_3  { OBJECT_VIEW            }
sub got_drop_type_4  { OBJECT_INDEX           }
sub got_drop_type_5  { OBJECT_FOREIGN_TABLE   }
sub got_drop_type_6  { OBJECT_EVENT_TRIGGER   }
sub got_drop_type_7  { OBJECT_TYPE            }
sub got_drop_type_8  { OBJECT_DOMAIN          }
sub got_drop_type_9  { OBJECT_COLLATION       }
sub got_drop_type_10 { OBJECT_CONVERSION      }
sub got_drop_type_11 { OBJECT_SCHEMA          }
sub got_drop_type_12 { OBJECT_EXTENSION       }
sub got_drop_type_13 { OBJECT_TSPARSER        }
sub got_drop_type_14 { OBJECT_TSDICTIONARY    }
sub got_drop_type_15 { OBJECT_TSTEMPLATE      }
sub got_drop_type_16 { OBJECT_TSCONFIGURATION }
sub got_any_name_list { $_[0]->lappend($_[1], $_[3]) }
sub got_any_name_1 { $_[0]->lappend($_[1])      }
sub got_any_name_2 { $_[0]->lcons($_[1], $_[2]) }
sub got_attrs_1 { $_[0]->lappend($_[2])        }
sub got_attrs_2 { $_[0]->lappend($_[1], $_[3]) }
sub got_TruncateStmt {
   return SQL::Translator::Statement::Truncate->new(
      relations    => $_[3],
      restart_seqs => $_[4],
      behavior     => $_[5],
   );
}
sub got_opt_restart_seqs_1 { FALSE }
sub got_opt_restart_seqs_2 { TRUE  }
sub got_CommentStmt_1  {
   return SQL::Translator::Statement::Comment->new(
      objtype => $_[3],
      objname => $_[4],
      objargs => NIL,
      comment => $_[6],
   );
}
sub got_CommentStmt_2  {
   return SQL::Translator::Statement::Comment->new(
      objtype => OBJECT_AGGREGATE,
      objname => $_[4],
      objargs => $_[5],
      comment => $_[7],
   );
}
sub got_CommentStmt_3  {
   return SQL::Translator::Statement::Comment->new(
      objtype => OBJECT_FUNCTION,
      objname => $_[4],
      objargs => $_[0]->extractArgTypes($_[5]),
      comment => $_[7],
   );
}
sub got_CommentStmt_4  {
   return SQL::Translator::Statement::Comment->new(
      objtype => OBJECT_OPERATOR,
      objname => $_[4],
      objargs => $_[5],
      comment => $_[7],
   );
}
sub got_CommentStmt_5  {
   return SQL::Translator::Statement::Comment->new(
      objtype => OBJECT_CONSTRAINT,
      objname => $_[0]->lappend($_[6], $_[4]),
      objargs => NIL,
      comment => $_[8],
   );
}
sub got_CommentStmt_6  {
   return SQL::Translator::Statement::Comment->new(
      objtype => OBJECT_RULE,
      objname => $_[0]->lappend($_[6], $_[4]),
      objargs => NIL,
      comment => $_[8],
   );
}
# Obsolete syntax supported for awhile for compatibility
sub got_CommentStmt_7  {
   return SQL::Translator::Statement::Comment->new(
      objtype => OBJECT_RULE,
      objname => $_[0]->lappend($_[4]),
      objargs => NIL,
      comment => $_[6],
   );
}
sub got_CommentStmt_8  {
   return SQL::Translator::Statement::Comment->new(
      objtype => OBJECT_TRIGGER,
      objname => $_[0]->lappend($_[6], $_[4]),
      objargs => NIL,
      comment => $_[8],
   );
}
sub got_CommentStmt_9  {
   return SQL::Translator::Statement::Comment->new(
      objtype => OBJECT_OPCLASS,
      objname => $_[5],
      objargs => $_[0]->lappend($_[7]),
      comment => $_[9],
   );
}
sub got_CommentStmt_10 {
   return SQL::Translator::Statement::Comment->new(
      objtype => OBJECT_OPFAMILY,
      objname => $_[5],
      objargs => $_[0]->lappend($_[7]),
      comment => $_[9],
   );
}
sub got_CommentStmt_11 {
   return SQL::Translator::Statement::Comment->new(
      objtype => OBJECT_LARGEOBJECT,
      objname => $_[0]->lappend($_[5]),
      objargs => NIL,
      comment => $_[7],
   );
}
sub got_CommentStmt_12 {
   return SQL::Translator::Statement::Comment->new(
      objtype => OBJECT_CAST,
      objname => $_[0]->lappend($_[5]),
      objargs => $_[0]->lappend($_[7]),
      comment => $_[10],
   );
}
sub got_CommentStmt_13 {
   return SQL::Translator::Statement::Comment->new(
      objtype => OBJECT_LANGUAGE,
      objname => $_[5],
      objargs => NIL,
      comment => $_[7],
   );
}
sub got_CommentStmt_14 {
   return SQL::Translator::Statement::Comment->new(
      objtype => OBJECT_TSPARSER,
      objname => $_[6],
      comment => $_[8],
   );
}
sub got_CommentStmt_15 {
   return SQL::Translator::Statement::Comment->new(
      objtype => OBJECT_TSDICTIONARY,
      objname => $_[6],
      comment => $_[8],
   );
}
sub got_CommentStmt_16 {
   return SQL::Translator::Statement::Comment->new(
      objtype => OBJECT_TSTEMPLATE,
      objname => $_[6],
      comment => $_[8],
   );
}
sub got_CommentStmt_17 {
   return SQL::Translator::Statement::Comment->new(
      objtype => OBJECT_TSCONFIGURATION,
      objname => $_[6],
      comment => $_[8],
   );
}
sub got_comment_type_1  { OBJECT_COLUMN         }
sub got_comment_type_2  { OBJECT_DATABASE       }
sub got_comment_type_3  { OBJECT_SCHEMA         }
sub got_comment_type_4  { OBJECT_INDEX          }
sub got_comment_type_5  { OBJECT_SEQUENCE       }
sub got_comment_type_6  { OBJECT_TABLE          }
sub got_comment_type_7  { OBJECT_DOMAIN         }
sub got_comment_type_8  { OBJECT_TYPE           }
sub got_comment_type_9  { OBJECT_VIEW           }
sub got_comment_type_10 { OBJECT_COLLATION      }
sub got_comment_type_11 { OBJECT_CONVERSION     }
sub got_comment_type_12 { OBJECT_TABLESPACE     }
sub got_comment_type_13 { OBJECT_EXTENSION      }
sub got_comment_type_14 { OBJECT_ROLE           }
sub got_comment_type_15 { OBJECT_FOREIGN_TABLE  }
sub got_comment_type_16 { OBJECT_FOREIGN_SERVER }
sub got_comment_type_17 { OBJECT_FDW            }
sub got_comment_type_18 { OBJECT_EVENT_TRIGGER  }
sub got_comment_text_1 { $_[1] }
sub got_comment_text_2 { NULL  }
sub got_SecLabelStmt_1 {
   return SQL::Translator::Statement::SecLabel->new(
      provider => $_[3],
      objtype  => $_[5],
      objname  => $_[6],
      objargs  => NIL,
      label    => $_[8],
   );
}
sub got_SecLabelStmt_2 {
   return SQL::Translator::Statement::SecLabel->new(
      provider => $_[3],
      objtype  => OBJECT_AGGREGATE,
      objname  => $_[6],
      objargs  => $_[7],
      label    => $_[9],
   );
}
sub got_SecLabelStmt_3 {
   return SQL::Translator::Statement::SecLabel->new(
      provider => $_[3],
      objtype  => OBJECT_FUNCTION,
      objname  => $_[6],
      objargs  => $_[0]->extractArgTypes($_[7]),
      label    => $_[9],
   );
}
sub got_SecLabelStmt_4 {
   return SQL::Translator::Statement::SecLabel->new(
      provider => $_[3],
      objtype  => OBJECT_LARGEOBJECT,
      objname  => $_[0]->lappend($_[7]),
      objargs  => NIL,
      label    => $_[9],
   );
}
sub got_SecLabelStmt_5 {
   return SQL::Translator::Statement::SecLabel->new(
      provider => $_[3],
      objtype  => OBJECT_LANGUAGE,
      objname  => $_[7],
      objargs  => NIL,
      label    => $_[9],
   );
}
sub got_opt_provider { $_[2] }
sub got_security_label_type_1  { OBJECT_COLUMN        }
sub got_security_label_type_2  { OBJECT_DATABASE      }
sub got_security_label_type_3  { OBJECT_EVENT_TRIGGER }
sub got_security_label_type_4  { OBJECT_SCHEMA        }
sub got_security_label_type_5  { OBJECT_FOREIGN_TABLE }
sub got_security_label_type_6  { OBJECT_SCHEMA        }
sub got_security_label_type_7  { OBJECT_SEQUENCE      }
sub got_security_label_type_8  { OBJECT_TABLE         }
sub got_security_label_type_9  { OBJECT_TYPE          }
sub got_security_label_type_10 { OBJECT_ROLE          }
sub got_security_label_type_11 { OBJECT_TABLESPACE    }
sub got_security_label_type_12 { OBJECT_TYPE          }
sub got_security_label_type_13 { OBJECT_VIEW          }
sub got_security_label_1 { $_[1] }
sub got_security_label_2 { NULL  }
sub got_FetchStmt_1 {
   $_[2]->ismove(FALSE);
   return $_[2];
}
sub got_FetchStmt_2 {
   $_[2]->ismove(TRUE);
   return $_[2];
}
sub got_fetch_args_1  {
   return SQL::Translator::Statement::Fetch->new(
      portalname => $_[1],
      direction  => FETCH_FORWARD,
      howMany    => 1,
   );
}
sub got_fetch_args_2  {
   return SQL::Translator::Statement::Fetch->new(
      portalname => $_[2],
      direction  => FETCH_FORWARD,
      howMany    => 1,
   );
}
sub got_fetch_args_3  {
   return SQL::Translator::Statement::Fetch->new(
      portalname => $_[3],
      direction  => FETCH_FORWARD,
      howMany    => 1,
   );
}
sub got_fetch_args_4  {
   return SQL::Translator::Statement::Fetch->new(
      portalname => $_[3],
      direction  => FETCH_BACKWARD,
      howMany    => 1,
   );
}
sub got_fetch_args_5  {
   return SQL::Translator::Statement::Fetch->new(
      portalname => $_[3],
      direction  => FETCH_ABSOLUTE,
      howMany    => 1,
   );
}
sub got_fetch_args_6  {
   return SQL::Translator::Statement::Fetch->new(
      portalname => $_[3],
      direction  => FETCH_ABSOLUTE,
      howMany    => -1,
   );
}
sub got_fetch_args_7  {
   return SQL::Translator::Statement::Fetch->new(
      portalname => $_[4],
      direction  => FETCH_ABSOLUTE,
      howMany    => $_[2],
   );
}
sub got_fetch_args_8  {
   return SQL::Translator::Statement::Fetch->new(
      portalname => $_[4],
      direction  => FETCH_RELATIVE,
      howMany    => $_[2],
   );
}
sub got_fetch_args_9  {
   return SQL::Translator::Statement::Fetch->new(
      portalname => $_[3],
      direction  => FETCH_FORWARD,
      howMany    => $_[1],
   );
}
sub got_fetch_args_10 {
   return SQL::Translator::Statement::Fetch->new(
      portalname => $_[3],
      direction  => FETCH_FORWARD,
      howMany    => FETCH_ALL,
   );
}
sub got_fetch_args_11 {
   return SQL::Translator::Statement::Fetch->new(
      portalname => $_[3],
      direction  => FETCH_FORWARD,
      howMany    => 1,
   );
}
sub got_fetch_args_12 {
   return SQL::Translator::Statement::Fetch->new(
      portalname => $_[4],
      direction  => FETCH_FORWARD,
      howMany    => $_[2],
   );
}
sub got_fetch_args_13 {
   return SQL::Translator::Statement::Fetch->new(
      portalname => $_[4],
      direction  => FETCH_FORWARD,
      howMany    => FETCH_ALL,
   );
}
sub got_fetch_args_14 {
   return SQL::Translator::Statement::Fetch->new(
      portalname => $_[3],
      direction  => FETCH_BACKWARD,
      howMany    => 1,
   );
}
sub got_fetch_args_15 {
   return SQL::Translator::Statement::Fetch->new(
      portalname => $_[4],
      direction  => FETCH_BACKWARD,
      howMany    => $_[2],
   );
}
sub got_fetch_args_16 {
   return SQL::Translator::Statement::Fetch->new(
      portalname => $_[4],
      direction  => FETCH_BACKWARD,
      howMany    => FETCH_ALL,
   );
}
sub got_from_in_1 {}
sub got_from_in_2 {}
sub got_opt_from_in {}
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
sub got_privileges_1 { $_[1] }
sub got_privileges_2 { NULL }
sub got_privileges_3 { NULL }
sub got_privileges_4 {
   return [ SQL::Translator::Statement::AccessPriv->new(
      priv_name => NULL,
      cols      => $_[3],
   ) ];
}
sub got_privileges_5 {
   return [ SQL::Translator::Statement::AccessPriv->new(
      priv_name => NULL,
      cols      => $_[4],
   ) ];
}
sub got_privilege_list { $_[0]->lappend($_[1], $_[3]) }
sub got_privilege_1 {
   return SQL::Translator::Statement::AccessPriv->new(
      priv_name => $_[1],
      cols      => $_[2],
   );
}
sub got_privilege_2 {
   return SQL::Translator::Statement::AccessPriv->new(
      priv_name => $_[1],
      cols      => $_[2],
   );
}
sub got_privilege_3 {
   return SQL::Translator::Statement::AccessPriv->new(
      priv_name => $_[1],
      cols      => $_[2],
   );
}
sub got_privilege_4 {
   return SQL::Translator::Statement::AccessPriv->new(
      priv_name => $_[1],
      cols      => $_[2],
   );
}
sub got_privilege_target_1  {
   return SQL::Translator::Statement::PrivTarget->new(
      targtype => ACL_TARGET_OBJECT,
      objtype  => ACL_OBJECT_RELATION,
      objs     => $_[1],
   );
}
sub got_privilege_target_2  {
   return SQL::Translator::Statement::PrivTarget->new(
      targtype => ACL_TARGET_OBJECT,
      objtype  => ACL_OBJECT_RELATION,
      objs     => $_[2],
   );
}
sub got_privilege_target_3  {
   return SQL::Translator::Statement::PrivTarget->new(
      targtype => ACL_TARGET_OBJECT,
      objtype  => ACL_OBJECT_SEQUENCE,
      objs     => $_[2],
   );
}
sub got_privilege_target_4  {
   return SQL::Translator::Statement::PrivTarget->new(
      targtype => ACL_TARGET_OBJECT,
      objtype  => ACL_OBJECT_FDW,
      objs     => $_[4],
   );
}
sub got_privilege_target_5  {
   return SQL::Translator::Statement::PrivTarget->new(
      targtype => ACL_TARGET_OBJECT,
      objtype  => ACL_OBJECT_FOREIGN_SERVER,
      objs     => $_[3],
   );
}
sub got_privilege_target_6  {
   return SQL::Translator::Statement::PrivTarget->new(
      targtype => ACL_TARGET_OBJECT,
      objtype  => ACL_OBJECT_FUNCTION,
      objs     => $_[2],
   );
}
sub got_privilege_target_7  {
   return SQL::Translator::Statement::PrivTarget->new(
      targtype => ACL_TARGET_OBJECT,
      objtype  => ACL_OBJECT_DATABASE,
      objs     => $_[2],
   );
}
sub got_privilege_target_8  {
   return SQL::Translator::Statement::PrivTarget->new(
      targtype => ACL_TARGET_OBJECT,
      objtype  => ACL_OBJECT_DOMAIN,
      objs     => $_[2],
   );
}
sub got_privilege_target_9  {
   return SQL::Translator::Statement::PrivTarget->new(
      targtype => ACL_TARGET_OBJECT,
      objtype  => ACL_OBJECT_LANGUAGE,
      objs     => $_[2],
   );
}
sub got_privilege_target_10 {
   return SQL::Translator::Statement::PrivTarget->new(
      targtype => ACL_TARGET_OBJECT,
      objtype  => ACL_OBJECT_LARGEOBJECT,
      objs     => $_[3],
   );
}
sub got_privilege_target_11 {
   return SQL::Translator::Statement::PrivTarget->new(
      targtype => ACL_TARGET_OBJECT,
      objtype  => ACL_OBJECT_NAMESPACE,
      objs     => $_[2],
   );
}
sub got_privilege_target_12 {
   return SQL::Translator::Statement::PrivTarget->new(
      targtype => ACL_TARGET_OBJECT,
      objtype  => ACL_OBJECT_TABLESPACE,
      objs     => $_[2],
   );
}
sub got_privilege_target_13 {
   return SQL::Translator::Statement::PrivTarget->new(
      targtype => ACL_TARGET_OBJECT,
      objtype  => ACL_OBJECT_TYPE,
      objs     => $_[2],
   );
}
sub got_privilege_target_14 {
   return SQL::Translator::Statement::PrivTarget->new(
      targtype => ACL_TARGET_ALL_IN_SCHEMA,
      objtype  => ACL_OBJECT_RELATION,
      objs     => $_[5],
   );
}
sub got_privilege_target_15 {
   return SQL::Translator::Statement::PrivTarget->new(
      targtype => ACL_TARGET_ALL_IN_SCHEMA,
      objtype  => ACL_OBJECT_SEQUENCE,
      objs     => $_[5],
   );
}
sub got_privilege_target_16 {
   return SQL::Translator::Statement::PrivTarget->new(
      targtype => ACL_TARGET_ALL_IN_SCHEMA,
      objtype  => ACL_OBJECT_FUNCTION,
      objs     => $_[5],
   );
}
sub got_grantee_list { $_[0]->lappend($_[1], $_[3]) }
sub got_grantee_1 {
   return SQL::Translator::Statement::PrivGrantee->new(
      #* This hack lets us avoid reserving PUBLIC as a keyword
      rolname => ($_[1] eq "public") ? NULL : $_[1],
   );
}
sub got_grantee_2 {
   return SQL::Translator::Statement::PrivGrantee->new(
      #* Treat GROUP PUBLIC as a synonym for PUBLIC
      rolname => ($_[2] eq "public") ? NULL : $_[2],
   );
}
sub got_opt_grant_grant_option { TRUE  }
sub got_function_with_argtypes_list { $_[0]->lappend($_[1], $_[3]) }
sub got_function_with_argtypes {
   return SQL::Translator::Statement::FuncWithArgs->new(
      funcname => $_[1],
      funcargs => $_[0]->extractArgTypes($_[2]),
   );
}
sub got_GrantRoleStmt {
   return SQL::Translator::Statement::GrantRole->new(
      is_grant      => TRUE,
      granted_roles => $_[2],
      grantee_roles => $_[4],
      admin_opt     => $_[5],
      grantor       => $_[6],
   );
}
sub got_RevokeRoleStmt_1 {
   return SQL::Translator::Statement::GrantRole->new(
      is_grant      => FALSE,
      admin_opt     => FALSE,
      granted_roles => $_[2],
      grantee_roles => $_[4],
      behavior      => $_[6],
   );
}
sub got_RevokeRoleStmt_2 {
   return SQL::Translator::Statement::GrantRole->new(
      is_grant      => FALSE,
      admin_opt     => TRUE,
      granted_roles => $_[5],
      grantee_roles => $_[7],
      behavior      => $_[9],
   );
}
sub got_opt_grant_admin_option { TRUE  }
sub got_opt_granted_by { $_[3] }
sub got_AlterDefaultPrivilegesStmt {
   return SQL::Translator::Statement::AlterDefaultPrivileges->new(
      options => $_[4],
      action  => $_[5],
   );
}
sub got_DefACLOptionList { $_[0]->lappend($_[1], $_[2]) }
sub got_DefACLOption_1 { $_[0]->makeDefElem("schemas", $_[3]) }
sub got_DefACLOption_2 { $_[0]->makeDefElem("roles",   $_[3]) }
sub got_DefACLOption_3 { $_[0]->makeDefElem("roles",   $_[3]) }
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
sub got_defacl_privilege_target_1 { ACL_OBJECT_RELATION }
sub got_defacl_privilege_target_2 { ACL_OBJECT_FUNCTION }
sub got_defacl_privilege_target_3 { ACL_OBJECT_SEQUENCE }
sub got_defacl_privilege_target_4 { ACL_OBJECT_TYPE     }
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
sub got_opt_unique { TRUE  }
sub got_opt_concurrently { TRUE  }
sub got_opt_index_name { $_[1] }
sub got_access_method_clause { $_[2] }
sub got_index_params { $_[0]->lappend($_[1], $_[3]) }
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
sub got_opt_collate { $_[2] }
sub got_opt_class_1 { $_[1] }
sub got_opt_class_2 { $_[2] }
sub got_opt_asc_desc_1 { SORTBY_ASC     }
sub got_opt_asc_desc_2 { SORTBY_DESC    }
sub got_opt_nulls_order_1 { SORTBY_NULLS_FIRST   }
sub got_opt_nulls_order_2 { SORTBY_NULLS_LAST    }
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
sub got_opt_or_replace { TRUE  }
sub got_func_args_1 { $_[2] }
sub got_func_args_2 { NULL  }
sub got_func_args_list { $_[0]->lappend($_[1], $_[3]) }
sub got_func_args_with_defaults_1 { $_[2] }
sub got_func_args_with_defaults_2 { NULL  }
sub got_func_args_with_defaults_list { $_[0]->lappend($_[1], $_[3]) }
sub got_func_arg_1 {
   return SQL::Translator::Statement::Function::Parameter->new(
      name    => $_[2],
      argType => $_[3],
      mode    => $_[1],
      defexpr => NULL,
   );
}
sub got_func_arg_2 {
   return SQL::Translator::Statement::Function::Parameter->new(
      name    => $_[1],
      argType => $_[3],
      mode    => $_[2],
      defexpr => NULL,
   );
}
sub got_func_arg_3 {
   return SQL::Translator::Statement::Function::Parameter->new(
      name    => $_[1],
      argType => $_[2],
      mode    => 'IN',
      defexpr => NULL,
   );
}
sub got_func_arg_4 {
   return SQL::Translator::Statement::Function::Parameter->new(
      name    => NULL,
      argType => $_[2],
      mode    => $_[1],
      defexpr => NULL,
   );
}
sub got_func_arg_5 {
   return SQL::Translator::Statement::Function::Parameter->new(
      name    => NULL,
      argType => $_[1],
      mode    => 'IN',
      defexpr => NULL,
   );
}
sub got_arg_class_1 { FUNC_PARAM_IN       }
sub got_arg_class_2 { FUNC_PARAM_OUT      }
sub got_arg_class_3 { FUNC_PARAM_INOUT    }
sub got_arg_class_4 { FUNC_PARAM_INOUT    }
sub got_arg_class_5 { FUNC_PARAM_VARIADIC }
# We can catch over-specified results here if we want to,
# but for now better to silently swallow typmod, etc.
# - thomas 2000-03-22
sub got_func_return { $_[1] }
sub got_func_type_1 { $_[1] }
sub got_func_type_2 {
   my $n = $_[0]->makeTypeNameFromNameList($_[0]->lcons($_[1], $_[2]));
   $n->pct_type(TRUE);
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}
sub got_func_type_3 {
   my $n = $_[0]->makeTypeNameFromNameList($_[0]->lcons($_[2], $_[3]));
   $n->pct_type(TRUE);
   $n->setof   (TRUE);
   $n->_set_location( $_[0]->YYLLoc($_[2], 2) );
   return $n;
}
sub got_func_arg_with_default_1 { $_[1] }
sub got_func_arg_with_default_2 {
   $_[1]->defexpr($_[3]);
   return $_[1];
}
sub got_func_arg_with_default_3 {
   $_[1]->defexpr($_[3]);
   return $_[1];
}
sub got_createfunc_opt_list { $_[0]->lappend($_[1], $_[2]) }
sub got_common_func_opt_item_1  { $_[0]->makeDefElem("strict",     FALSE      ) }
sub got_common_func_opt_item_2  { $_[0]->makeDefElem("strict",     TRUE       ) }
sub got_common_func_opt_item_3  { $_[0]->makeDefElem("strict",     TRUE       ) }
sub got_common_func_opt_item_4  { $_[0]->makeDefElem("volatility", "immutable") }
sub got_common_func_opt_item_5  { $_[0]->makeDefElem("volatility", "stable"   ) }
sub got_common_func_opt_item_6  { $_[0]->makeDefElem("volatility", "volatile" ) }
sub got_common_func_opt_item_7  { $_[0]->makeDefElem("security",   TRUE       ) }
sub got_common_func_opt_item_8  { $_[0]->makeDefElem("security",   FALSE      ) }
sub got_common_func_opt_item_9  { $_[0]->makeDefElem("security",   TRUE       ) }
sub got_common_func_opt_item_10 { $_[0]->makeDefElem("security",   FALSE      ) }
sub got_common_func_opt_item_11 { $_[0]->makeDefElem("leakproof",  TRUE       ) }
sub got_common_func_opt_item_12 { $_[0]->makeDefElem("leakproof",  FALSE      ) }
sub got_common_func_opt_item_13 { $_[0]->makeDefElem("cost",       $_[2]      ) }
sub got_common_func_opt_item_14 { $_[0]->makeDefElem("rows",       $_[2]      ) }
# we abuse the normal content of a DefElem here
sub got_common_func_opt_item_15 { $_[0]->makeDefElem("set",        $_[1]      ) }
sub got_createfunc_opt_item_1 { $_[0]->makeDefElem("as",       $_[2]) }
sub got_createfunc_opt_item_2 { $_[0]->makeDefElem("language", $_[2]) }
sub got_createfunc_opt_item_3 { $_[0]->makeDefElem("window",    TRUE) }
sub got_createfunc_opt_item_4 { $_[1] }
sub got_func_as_1 { $_[0]->lappend($_[1])        }
sub got_func_as_2 { $_[0]->lappend($_[1], $_[3]) }
sub got_opt_definition { $_[2] }
sub got_table_func_column {
   return SQL::Translator::Statement::Function::Parameter->new(
      name    => $_[1],
      argType => $_[2],
      mode    => FUNC_PARAM_TABLE,
      defexpr => NULL,
   );
}
sub got_table_func_column_list { $_[0]->lappend($_[1], $_[3]) }
sub got_AlterFunctionStmt {
   return SQL::Translator::Statement::AlterFunction->new(
      func    => $_[3],
      actions => $_[4],
   );
}
sub got_alterfunc_opt_list { $_[0]->lappend($_[1], $_[2]) }
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
sub got_oper_argtypes_1 {
   $_[0]->ereport(ERROR,
         ERRCODE_SYNTAX_ERROR,
         "missing argument",
         "Use NONE to denote the missing argument of a unary operator.",
         $_[0]->YYLLoc($_[3], 3));
}
sub got_oper_argtypes_2 { $_[0]->lappend($_[2], $_[4]) }
# left unary
sub got_oper_argtypes_3 { $_[0]->lappend(NULL,  $_[4]) }
# right unary
sub got_oper_argtypes_4 { $_[0]->lappend($_[2], NULL ) }
sub got_any_operator_1 { $_[0]->lappend($_[1])      }
sub got_any_operator_2 { $_[0]->lcons($_[1], $_[3]) }
sub got_DoStmt {
   return SQL::Translator::Statement::Do->new(
      args => $_[2],
   );
}
sub got_dostmt_opt_list { $_[0]->lappend($_[1], $_[2]) }
sub got_dostmt_opt_item_1 { $_[0]->makeDefElem("as",       $_[1]) }
sub got_dostmt_opt_item_2 { $_[0]->makeDefElem("language", $_[2]) }
sub got_CreateCastStmt_1 {
   return SQL::Translator::Statement::CreateCast->new(
      sourcetype => $_[4],
      targettype => $_[6],
      func       => $_[10],
      context    => $_[11],
      inout      => FALSE,
   );
}
sub got_CreateCastStmt_2 {
   return SQL::Translator::Statement::CreateCast->new(
      sourcetype => $_[4],
      targettype => $_[6],
      func       => NULL,
      context    => $_[10],
      inout      => FALSE,
   );
}
sub got_CreateCastStmt_3 {
   return SQL::Translator::Statement::CreateCast->new(
      sourcetype => $_[4],
      targettype => $_[6],
      func       => NULL,
      context    => $_[10],
      inout      => TRUE,
   );
}
sub got_cast_context_1 { COERCION_IMPLICIT   }
sub got_cast_context_2 { COERCION_ASSIGNMENT }
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
sub got_opt_if_exists { TRUE  }
sub got_ReindexStmt_1 {
   return SQL::Translator::Statement::Reindex->new(
      kind      => $_[2],
      relation  => $_[3],
      name      => NULL,
   );
}
sub got_ReindexStmt_2 {
   return SQL::Translator::Statement::Reindex->new(
      kind      => OBJECT_DATABASE,
      name      => $_[3],
      relation  => NULL,
      do_system => TRUE,
      do_user   => FALSE,
   );
}
sub got_ReindexStmt_3 {
   return SQL::Translator::Statement::Reindex->new(
      kind      => OBJECT_DATABASE,
      name      => $_[3],
      relation  => NULL,
      do_system => TRUE,
      do_user   => TRUE,
   );
}
sub got_reindex_type_1 { 'INDEX' }
sub got_reindex_type_2 { 'TABLE' }
sub got_opt_force { TRUE  }
sub got_RenameStmt_1  {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_AGGREGATE,
      object              => $_[3],
      objarg              => $_[4],
      newname             => $_[7],
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_2  {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_COLLATION,
      object              => $_[3],
      newname             => $_[6],
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_3  {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_CONVERSION,
      object              => $_[3],
      newname             => $_[6],
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_4  {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_DATABASE,
      subname             => $_[3],
      newname             => $_[6],
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_5  {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_DOMAIN,
      subname             => $_[3],
      newname             => $_[6],
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_6  {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_CONSTRAINT,
      relationType        => OBJECT_DOMAIN,
      object              => $_[3],
      subname             => $_[6],
      newname             => $_[8],
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_7  {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_FDW,
      subname             => $_[5],
      newname             => $_[8],
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_8  {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_FUNCTION,
      object              => $_[3]->funcname,
      objarg              => $_[3]->funcargs,
      newname             => $_[6],
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_9  {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_ROLE,
      subname             => $_[3],
      newname             => $_[6],
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_10 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_LANGUAGE,
      subname             => $_[4],
      newname             => $_[7],
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_11 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_OPCLASS,
      object              => $_[4],
      subname             => $_[6],
      newname             => $_[9],
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_12 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_OPFAMILY,
      object              => $_[4],
      subname             => $_[6],
      newname             => $_[9],
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_13 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_SCHEMA,
      subname             => $_[3],
      newname             => $_[6],
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_14 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_FOREIGN_SERVER,
      subname             => $_[3],
      newname             => $_[6],
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_15 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_TABLE,
      relation            => $_[3],
      subname             => NULL,
      newname             => $_[6],
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_16 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_TABLE,
      relation            => $_[5],
      subname             => NULL,
      newname             => $_[8],
      missing_ok          => TRUE,
   );
}
sub got_RenameStmt_17 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_SEQUENCE,
      relation            => $_[3],
      subname             => NULL,
      newname             => $_[6],
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_18 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_SEQUENCE,
      relation            => $_[5],
      subname             => NULL,
      newname             => $_[7],
      missing_ok          => TRUE,
   );
}
sub got_RenameStmt_19 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_VIEW,
      relation            => $_[3],
      subname             => NULL,
      newname             => $_[6],
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_20 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_VIEW,
      relation            => $_[5],
      subname             => NULL,
      newname             => $_[8],
      missing_ok          => TRUE,
   );
}
sub got_RenameStmt_21 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_INDEX,
      relation            => $_[3],
      subname             => NULL,
      newname             => $_[6],
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_22 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_INDEX,
      relation            => $_[5],
      subname             => NULL,
      newname             => $_[8],
      missing_ok          => TRUE,
   );
}
sub got_RenameStmt_23 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_FOREIGN_TABLE,
      relation            => $_[4],
      subname             => NULL,
      newname             => $_[7],
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_24 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_FOREIGN_TABLE,
      relation            => $_[6],
      subname             => NULL,
      newname             => $_[9],
      missing_ok          => TRUE,
   );
}
sub got_RenameStmt_25 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_COLUMN,
      relationType        => OBJECT_TABLE,
      relation            => $_[3],
      subname             => $_[6],
      newname             => $_[8],
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_26 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_COLUMN,
      relationType        => OBJECT_TABLE,
      relation            => $_[5],
      subname             => $_[8],
      newname             => $_[10],
      missing_ok          => TRUE,
   );
}
sub got_RenameStmt_27 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_CONSTRAINT,
      relationType        => OBJECT_TABLE,
      relation            => $_[3],
      subname             => $_[6],
      newname             => $_[8],
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_28 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_COLUMN,
      relationType        => OBJECT_FOREIGN_TABLE,
      relation            => $_[4],
      subname             => $_[7],
      newname             => $_[9],
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_29 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_COLUMN,
      relationType        => OBJECT_FOREIGN_TABLE,
      relation            => $_[6],
      subname             => $_[9],
      newname             => $_[11],
      missing_ok          => TRUE,
   );
}
sub got_RenameStmt_30 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_TRIGGER,
      relation            => $_[5],
      subname             => $_[3],
      newname             => $_[8],
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_31 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_EVENT_TRIGGER,
      subname             => $_[3],
      newname             => $_[8],
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_32 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_ROLE,
      subname             => $_[3],
      newname             => $_[6],
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_33 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_ROLE,
      subname             => $_[3],
      newname             => $_[6],
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_34 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_TABLESPACE,
      subname             => $_[3],
      newname             => $_[6],
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_35 {
   return SQL::Translator::Statement::AlterTableSpaceOptions->new(
      tablespacename      => $_[3],
      options             => $_[5],
      isReset             => FALSE,
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_36 {
   return SQL::Translator::Statement::AlterTableSpaceOptions->new(
      tablespacename      => $_[3],
      options             => $_[5],
      isReset             => TRUE,
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_37 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_TSPARSER,
      object              => $_[5],
      newname             => $_[8],
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_38 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_TSDICTIONARY,
      object              => $_[5],
      newname             => $_[8],
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_39 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_TSTEMPLATE,
      object              => $_[5],
      newname             => $_[8],
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_40 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_TSCONFIGURATION,
      object              => $_[5],
      newname             => $_[8],
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_41 {
   return SQL::Translator::Statement::Rename->new(
      renameType          => OBJECT_TYPE,
      object              => $_[3],
      newname             => $_[6],
      missing_ok          => FALSE,
   );
}
sub got_RenameStmt_42 {
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
sub got_opt_column { OBJECT_COLUMN }
sub got_opt_set_data { 1 }
sub got_AlterObjectSchemaStmt_1  {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_AGGREGATE,
      object     => $_[3],
      objarg     => $_[4],
      newschema  => $_[7],
   );
}
sub got_AlterObjectSchemaStmt_2  {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_COLLATION,
      object     => $_[3],
      newschema  => $_[6],
   );
}
sub got_AlterObjectSchemaStmt_3  {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_CONVERSION,
      object     => $_[3],
      newschema  => $_[6],
   );
}
sub got_AlterObjectSchemaStmt_4  {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_DOMAIN,
      object     => $_[3],
      newschema  => $_[6],
   );
}
sub got_AlterObjectSchemaStmt_5  {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_EXTENSION,
      object     => $_[3],
      newschema  => $_[6],
   );
}
sub got_AlterObjectSchemaStmt_6  {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_FUNCTION,
      object     => $_[3]->funcname,
      objarg     => $_[3]->funcargs,
      newschema  => $_[6],
   );
}
sub got_AlterObjectSchemaStmt_7  {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_OPERATOR,
      object     => $_[3],
      objarg     => $_[4],
      newschema  => $_[7],
   );
}
sub got_AlterObjectSchemaStmt_8  {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_OPCLASS,
      object     => $_[4],
      addname    => $_[6],
      newschema  => $_[9],
   );
}
sub got_AlterObjectSchemaStmt_9  {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_OPFAMILY,
      object     => $_[4],
      addname    => $_[6],
      newschema  => $_[9],
   );
}
sub got_AlterObjectSchemaStmt_10 {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_TABLE,
      relation   => $_[3],
      newschema  => $_[6],
      missing_ok => FALSE,
   );
}
sub got_AlterObjectSchemaStmt_11 {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_TABLE,
      relation   => $_[3],
      newschema  => $_[6],
      missing_ok => TRUE,
   );
}
sub got_AlterObjectSchemaStmt_12 {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_TSPARSER,
      object     => $_[5],
      newschema  => $_[8],
   );
}
sub got_AlterObjectSchemaStmt_13 {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_TSDICTIONARY,
      object     => $_[5],
      newschema  => $_[8],
   );
}
sub got_AlterObjectSchemaStmt_14 {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_TSTEMPLATE,
      object     => $_[5],
      newschema  => $_[8],
   );
}
sub got_AlterObjectSchemaStmt_15 {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_TSCONFIGURATION,
      object     => $_[5],
      newschema  => $_[8],
   );
}
sub got_AlterObjectSchemaStmt_16 {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_SEQUENCE,
      relation   => $_[3],
      newschema  => $_[6],
      missing_ok => FALSE,                  
   );
}
sub got_AlterObjectSchemaStmt_17 {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_SEQUENCE,
      relation   => $_[5],
      newschema  => $_[8],
      missing_ok => TRUE,
   );
}
sub got_AlterObjectSchemaStmt_18 {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_VIEW,
      relation   => $_[3],
      newschema  => $_[6],
      missing_ok => FALSE,
   );
}
sub got_AlterObjectSchemaStmt_19 {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_VIEW,
      relation   => $_[5],
      newschema  => $_[8],
      missing_ok => TRUE,
   );
}
sub got_AlterObjectSchemaStmt_20 {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_FOREIGN_TABLE,
      relation   => $_[4],
      newschema  => $_[7],
      missing_ok => FALSE,
   );
}
sub got_AlterObjectSchemaStmt_21 {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_FOREIGN_TABLE,
      relation   => $_[6],
      newschema  => $_[9],
      missing_ok => TRUE,
   );
}
sub got_AlterObjectSchemaStmt_22 {
   return SQL::Translator::Statement::AlterObjectSchema->new(
      objectType => OBJECT_TYPE,
      object     => $_[3],
      newschema  => $_[6],
   );
}
sub got_AlterOwnerStmt_1  {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_AGGREGATE,
      object     => $_[3],
      objarg     => $_[4],
      newowner   => $_[7],
   );
}
sub got_AlterOwnerStmt_2  {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_COLLATION,
      object     => $_[3],
      newowner   => $_[6],
   );
}
sub got_AlterOwnerStmt_3  {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_CONVERSION,
      object     => $_[3],
      newowner   => $_[6],
   );
}
sub got_AlterOwnerStmt_4  {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_DATABASE,
      object     => $_[0]->lappend($_[3]),
      newowner   => $_[6],
   );
}
sub got_AlterOwnerStmt_5  {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_DOMAIN,
      object     => $_[3],
      newowner   => $_[6],
   );
}
sub got_AlterOwnerStmt_6  {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_FUNCTION,
      object     => $_[3]->funcname,
      objarg     => $_[3]->funcargs,
      newowner   => $_[6],
   );
}
sub got_AlterOwnerStmt_7  {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_LANGUAGE,
      object     => $_[0]->lappend($_[4]),
      newowner   => $_[7],
   );
}
sub got_AlterOwnerStmt_8  {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_LARGEOBJECT,
      object     => $_[0]->lappend($_[4]),
      newowner   => $_[7],
   );
}
sub got_AlterOwnerStmt_9  {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_OPERATOR,
      object     => $_[3],
      objarg     => $_[4],
      newowner   => $_[7],
   );
}
sub got_AlterOwnerStmt_10 {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_OPCLASS,
      object     => $_[4],
      addname    => $_[6],
      newowner   => $_[9],
   );
}
sub got_AlterOwnerStmt_11 {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_OPFAMILY,
      object     => $_[4],
      addname    => $_[6],
      newowner   => $_[9],
   );
}
sub got_AlterOwnerStmt_12 {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_SCHEMA,
      object     => $_[0]->lappend($_[3]),
      newowner   => $_[6],
   );
}
sub got_AlterOwnerStmt_13 {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_TYPE,
      object     => $_[3],
      newowner   => $_[6],
   );
}
sub got_AlterOwnerStmt_14 {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_TABLESPACE,
      object     => $_[0]->lappend($_[3]),
      newowner   => $_[6],
   );
}
sub got_AlterOwnerStmt_15 {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_TSDICTIONARY,
      object     => $_[5],
      newowner   => $_[8],
   );
}
sub got_AlterOwnerStmt_16 {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_TSCONFIGURATION,
      object     => $_[5],
      newowner   => $_[8],
   );
}
sub got_AlterOwnerStmt_17 {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_FDW,
      object     => $_[0]->lappend($_[5]),
      newowner   => $_[8],
   );
}
sub got_AlterOwnerStmt_18 {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_FOREIGN_SERVER,
      object     => $_[0]->lappend($_[3]),
      newowner   => $_[6],
   );
}
sub got_AlterOwnerStmt_19 {
   return SQL::Translator::Statement::AlterOwner->new(
      objectType => OBJECT_EVENT_TRIGGER,
      object     => $_[0]->lappend($_[4]),
      newowner   => $_[7],
   );
}
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
sub got_RuleActionList_1 { NIL }
sub got_RuleActionList_2 { $_[0]->lappend($_[1]) }
sub got_RuleActionList_3 { $_[2] }
sub got_RuleActionMulti { (defined $_[3]) ? $_[0]->lappend($_[1], $_[3]) : $_[1] }
sub got_RuleActionStmtOrEmpty { $_[1] }
sub got_event_1 { CMD_SELECT }
sub got_event_2 { CMD_UPDATE }
sub got_event_3 { CMD_DELETE }
sub got_event_4 { CMD_INSERT }
sub got_opt_instead_1 { TRUE  }
sub got_opt_instead_2 { FALSE }
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
sub got_NotifyStmt {
   return SQL::Translator::Statement::Notify->new(
      conditionname => $_[2],
      payload       => $_[3],
   );
}
sub got_notify_payload { $_[2] }
sub got_ListenStmt {
   return SQL::Translator::Statement::Listen->new(
      conditionname => $_[2],
   );
}
sub got_UnlistenStmt_1 {
   return SQL::Translator::Statement::Unlisten->new(
      conditionname => $_[2],
   );
}
sub got_UnlistenStmt_2 {
   return SQL::Translator::Statement::Unlisten->new(
      conditionname => NULL,
   );
}
sub got_TransactionStmt_1  {
   return SQL::Translator::Statement::Transaction->new(
      kind    => TRANS_STMT_ROLLBACK,
      options => NIL,
   );
}
sub got_TransactionStmt_2  {
   return SQL::Translator::Statement::Transaction->new(
      kind    => TRANS_STMT_BEGIN,
      options => $_[3],
   );
}
sub got_TransactionStmt_3  {
   return SQL::Translator::Statement::Transaction->new(
      kind    => TRANS_STMT_START,
      options => $_[3],
   );
}
sub got_TransactionStmt_4  {
   return SQL::Translator::Statement::Transaction->new(
      kind    => TRANS_STMT_COMMIT,
      options => NIL,
   );
}
sub got_TransactionStmt_5  {
   return SQL::Translator::Statement::Transaction->new(
      kind    => TRANS_STMT_COMMIT,
      options => NIL,
   );
}
sub got_TransactionStmt_6  {
   return SQL::Translator::Statement::Transaction->new(
      kind    => TRANS_STMT_ROLLBACK,
      options => NIL,
   );
}
sub got_TransactionStmt_7  {
   return SQL::Translator::Statement::Transaction->new(
      kind    => TRANS_STMT_SAVEPOINT,
      options => $_[0]->lappend($_[0]->makeDefElem("savepoint_name", $_[2])),
   );
}
sub got_TransactionStmt_8  {
   return SQL::Translator::Statement::Transaction->new(
      kind    => TRANS_STMT_RELEASE,
      options => $_[0]->lappend($_[0]->makeDefElem("savepoint_name", $_[3])),
   );
}
sub got_TransactionStmt_9  {
   return SQL::Translator::Statement::Transaction->new(
      kind    => TRANS_STMT_RELEASE,
      options => $_[0]->lappend($_[0]->makeDefElem("savepoint_name", $_[2])),
   );
}
sub got_TransactionStmt_10 {
   return SQL::Translator::Statement::Transaction->new(
      kind    => TRANS_STMT_ROLLBACK_TO,
      options => $_[0]->lappend($_[0]->makeDefElem("savepoint_name", $_[5])),
   );
}
sub got_TransactionStmt_11 {
   return SQL::Translator::Statement::Transaction->new(
      kind    => TRANS_STMT_ROLLBACK_TO,
      options => $_[0]->lappend($_[0]->makeDefElem("savepoint_name", $_[4])),
   );
}
sub got_TransactionStmt_12 {
   return SQL::Translator::Statement::Transaction->new(
      kind    => TRANS_STMT_PREPARE,
      gid     => $_[3],
   );
}
sub got_TransactionStmt_13 {
   return SQL::Translator::Statement::Transaction->new(
      kind    => TRANS_STMT_COMMIT_PREPARED,
      gid     => $_[3],
   );
}
sub got_TransactionStmt_14 {
   return SQL::Translator::Statement::Transaction->new(
      kind    => TRANS_STMT_ROLLBACK_PREPARED,
      gid     => $_[3],
   );
}
sub got_opt_transaction_1 {}
sub got_opt_transaction_2 {}
sub got_transaction_mode_item_1 { $_[0]->makeDefElem("transaction_isolation",  $_[0]->makeStringConst($_[3], $_[0]->YYLLoc($_[3], 3))) }
sub got_transaction_mode_item_2 { $_[0]->makeDefElem("transaction_read_only",  $_[0]->makeIntConst   (TRUE,  $_[0]->YYLLoc($_[1], 1))) }
sub got_transaction_mode_item_3 { $_[0]->makeDefElem("transaction_read_only",  $_[0]->makeIntConst   (FALSE, $_[0]->YYLLoc($_[1], 1))) }
sub got_transaction_mode_item_4 { $_[0]->makeDefElem("transaction_deferrable", $_[0]->makeIntConst   (TRUE,  $_[0]->YYLLoc($_[1], 1))) }
sub got_transaction_mode_item_5 { $_[0]->makeDefElem("transaction_deferrable", $_[0]->makeIntConst   (FALSE, $_[0]->YYLLoc($_[1], 1))) }
sub got_transaction_mode_list_1 { $_[0]->lappend($_[1])        }
sub got_transaction_mode_list_2 { $_[0]->lappend($_[1], $_[3]) }
sub got_transaction_mode_list_3 { $_[0]->lappend($_[1], $_[2]) }
sub got_ViewStmt_1 {
   my $n = SQL::Translator::Statement::View->new(
      view    => $_[4],
      aliases => $_[5],
      query   => $_[8],
      options => $_[6],
      replace => FALSE,
   );
   $n->view->relpersistence($_[2]);
   return $n;
}
sub got_ViewStmt_2 {
   my $n = SQL::Translator::Statement::View->new(
      view    => $_[6],
      aliases => $_[7],
      query   => $_[10],
      options => $_[8],
      replace => TRUE,
   );
   $n->view->relpersistence($_[4]);
   return $n;
}
sub got_opt_check_option_1 {
   $_[0]->ereport(ERROR,
         ERRCODE_FEATURE_NOT_SUPPORTED,
          "WITH CHECK OPTION is not implemented");
}
sub got_opt_check_option_2 {
   $_[0]->ereport(ERROR,
         ERRCODE_FEATURE_NOT_SUPPORTED,
          "WITH CHECK OPTION is not implemented");
}
sub got_opt_check_option_3 {
   $_[0]->ereport(ERROR,
         ERRCODE_FEATURE_NOT_SUPPORTED,
          "WITH CHECK OPTION is not implemented");
}
sub got_LoadStmt {
   return SQL::Translator::Statement::Load->new(
      filename => $_[2],
   );
}
sub got_CreatedbStmt {
   return SQL::Translator::Statement::Createdb->new(
      dbname  => $_[3],
      options => $_[5],
   );
}
sub got_createdb_opt_list { $_[0]->lappend($_[1], $_[2]) }
sub got_createdb_opt_item_1  { $_[0]->makeDefElem("tablespace",  $_[3]) }
sub got_createdb_opt_item_2  { $_[0]->makeDefElem("tablespace",   NULL) }
sub got_createdb_opt_item_3  { $_[0]->makeDefElem("location",    $_[3]) }
sub got_createdb_opt_item_4  { $_[0]->makeDefElem("location",     NULL) }
sub got_createdb_opt_item_5  { $_[0]->makeDefElem("template",    $_[3]) }
sub got_createdb_opt_item_6  { $_[0]->makeDefElem("template",     NULL) }
sub got_createdb_opt_item_7  { $_[0]->makeDefElem("encoding",    $_[3]) }
sub got_createdb_opt_item_8  { $_[0]->makeDefElem("encoding",  $_[3]+0) }
sub got_createdb_opt_item_9  { $_[0]->makeDefElem("encoding",     NULL) }
sub got_createdb_opt_item_10 { $_[0]->makeDefElem("lc_collate",  $_[3]) }
sub got_createdb_opt_item_11 { $_[0]->makeDefElem("lc_collate",   NULL) }
sub got_createdb_opt_item_12 { $_[0]->makeDefElem("lc_ctype",    $_[3]) }
sub got_createdb_opt_item_13 { $_[0]->makeDefElem("lc_ctype",     NULL) }
sub got_createdb_opt_item_14 { $_[0]->makeDefElem("connectionlimit", $_[4]+0) }
sub got_createdb_opt_item_15 { $_[0]->makeDefElem("owner",       $_[3]) }
sub got_createdb_opt_item_16 { $_[0]->makeDefElem("owner",        NULL) }
sub got_opt_equal {}
sub got_AlterDatabaseStmt_1 {
  return SQL::Translator::Statement::AlterDatabase->new(
     dbname  => $_[3],
     options => $_[5],
  );
}
sub got_AlterDatabaseStmt_2 {
  return SQL::Translator::Statement::AlterDatabase->new(
     dbname  => $_[3],
     options => $_[0]->lappend($_[0]->makeDefElem("tablespace", $_[6])),
  );
}
sub got_AlterDatabaseSetStmt {
   return SQL::Translator::Statement::AlterDatabaseSet->new(
      dbname  => $_[3],
      setstmt => $_[4],
   );
}
sub got_alterdb_opt_list { $_[0]->lappend($_[1], $_[2]) }
sub got_alterdb_opt_item { $_[0]->makeDefElem("connectionlimit", $_[4]+0) }
sub got_DropdbStmt_1 {
   return SQL::Translator::Statement::Dropdb->new(
      dbname     => $_[3],
      missing_ok => FALSE,
   );
}
sub got_DropdbStmt_2 {
   return SQL::Translator::Statement::Dropdb->new(
      dbname     => $_[5],
      missing_ok => TRUE,
   );
}
sub got_CreateDomainStmt {
   my $n = SQL::Translator::Statement::CreateDomain->new(
      domainname => $_[3],
      typeName   => $_[5],
   );
   $_[0]->SplitColQualList($_[6], $n);
   return $n;
}
# ALTER DOMAIN <domain> DROP NOT NULL
sub got_AlterDomainStmt_1 {
   return SQL::Translator::Statement::AlterDomain->new(
      subtype  => 'T',
      typeName => $_[3],
      def      => $_[4],
   );
}
# ALTER DOMAIN <domain> SET NOT NULL
sub got_AlterDomainStmt_2 {
   return SQL::Translator::Statement::AlterDomain->new(
      subtype  => 'N',
      typeName => $_[3],
   );
}
# ALTER DOMAIN <domain> ADD CONSTRAINT ...
sub got_AlterDomainStmt_3 {
   return SQL::Translator::Statement::AlterDomain->new(
      subtype  => 'O',
      typeName => $_[3],
   );
}
# ALTER DOMAIN <domain> DROP CONSTRAINT <name> [RESTRICT|CASCADE]
sub got_AlterDomainStmt_4 {
   return SQL::Translator::Statement::AlterDomain->new(
      subtype  => 'C',
      typeName => $_[3],
      def      => $_[5],
   );
}
# ALTER DOMAIN <domain> DROP CONSTRAINT IF EXISTS <name> [RESTRICT|CASCADE]
sub got_AlterDomainStmt_5 {
   return SQL::Translator::Statement::AlterDomain->new(
      subtype    => 'X',
      typeName   => $_[3],
      name       => $_[6],
      behavior   => $_[7],
      missing_ok => FALSE,
   );
}
# ALTER DOMAIN <domain> VALIDATE CONSTRAINT <name>
sub got_AlterDomainStmt_6 {
   return SQL::Translator::Statement::AlterDomain->new(
      subtype    => 'X',
      typeName   => $_[3],
      name       => $_[8],
      behavior   => $_[9],
      missing_ok => TRUE,
   );
}
sub got_AlterDomainStmt_7 {
   return SQL::Translator::Statement::AlterDomain->new(
      subtype    => 'V',
      typeName   => $_[3],
      name       => $_[6],
   );
}
sub got_opt_as {}
sub got_AlterTSDictionaryStmt {
   return SQL::Translator::Statement::AlterTSDictionary->new(
      dictname => $_[5],
      options  => $_[6],
   );
}
sub got_AlterTSConfigurationStmt_1 {
   return SQL::Translator::Statement::AlterTSConfiguration->new(
      cfgname    => $_[5],
      tokentype  => $_[9],
      dicts      => $_[11],
      override   => FALSE,
      replace    => FALSE,
   );
}
sub got_AlterTSConfigurationStmt_2 {
   return SQL::Translator::Statement::AlterTSConfiguration->new(
      cfgname    => $_[5],
      tokentype  => $_[9],
      dicts      => $_[11],
      override   => TRUE,
      replace    => FALSE,
   );
}
sub got_AlterTSConfigurationStmt_3 {
   return SQL::Translator::Statement::AlterTSConfiguration->new(
      cfgname    => $_[5],
      tokentype  => NIL,
      dicts      => $_[0]->lappend($_[9],$_[11]),
      override   => FALSE,
      replace    => TRUE,
   );
}
sub got_AlterTSConfigurationStmt_4 {
   return SQL::Translator::Statement::AlterTSConfiguration->new(
      cfgname    => $_[5],
      tokentype  => $_[9],
      dicts      => $_[0]->lappend($_[11],$_[13]),
      override   => FALSE,
      replace    => TRUE,
   );
}
sub got_AlterTSConfigurationStmt_5 {
   return SQL::Translator::Statement::AlterTSConfiguration->new(
      cfgname    => $_[5],
      tokentype  => $_[9],
      missing_ok => FALSE,
   );
}
sub got_AlterTSConfigurationStmt_6 {
   return SQL::Translator::Statement::AlterTSConfiguration->new(
      cfgname    => $_[5],
      tokentype  => $_[11],
      missing_ok => TRUE,
   );
}
sub got_CreateConversionStmt {
  return SQL::Translator::Statement::CreateConversion->new(
     conversion_name   => $_[4],
     for_encoding_name => $_[6],
     to_encoding_name  => $_[8],
     func_name         => $_[10],
     def               => $_[2],
  );
}
sub got_ClusterStmt_1 {
    return SQL::Translator::Statement::Cluster->new(
      relation  => $_[3],
      indexname => $_[4],
      verbose   => $_[2],
   );
}
# kept for pre-8.3 compatibility
sub got_ClusterStmt_2 {
   return SQL::Translator::Statement::Cluster->new(
      relation  => NULL,
      indexname => NULL,
      verbose   => $_[2],
   );
}
sub got_ClusterStmt_3 {
   return SQL::Translator::Statement::Cluster->new(
      relation  => $_[5],
      indexname => $_[3],
      verbose   => $_[2],
   );
}
sub got_cluster_index_specification { $_[2] }
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
sub got_VacuumStmt_3 {
   $_[5]->options( VACOPT_VACUUM |
      $_[2] ? VACOPT_FULL : 0 |
      $_[4] ? VACOPT_VERBOSE : 0
   );
   $_[5]->freeze_min_age  ($_[3] ? 0 : -1);
   $_[5]->freeze_table_age($_[3] ? 0 : -1);
   return $_[5];
}
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
sub got_vacuum_option_list { $_[1] | $_[3] }
sub got_vacuum_option_elem_1 { VACOPT_ANALYZE }
sub got_vacuum_option_elem_2 { VACOPT_VERBOSE }
sub got_vacuum_option_elem_3 { VACOPT_FREEZE  }
sub got_vacuum_option_elem_4 { VACOPT_FULL    }
sub got_AnalyzeStmt_1 {
   return SQL::Translator::Statement::Analyze->new(
      options          => VACOPT_ANALYZE | $_[2] ? VACOPT_VERBOSE : 0,
      freeze_min_age   => -1,
      freeze_table_age => -1,
      relation         => NULL,
      va_cols          => NIL,
   );
}
sub got_AnalyzeStmt_2 {
   return SQL::Translator::Statement::Analyze->new(
      options          => VACOPT_ANALYZE | $_[2] ? VACOPT_VERBOSE : 0,
      freeze_min_age   => -1,
      freeze_table_age => -1,
      relation         => $_[3],
      va_cols          => $_[4],
   );
}
sub got_analyze_keyword_1 {}
# British
sub got_analyze_keyword_2 {}
sub got_opt_verbose { TRUE  }
sub got_opt_full { TRUE  }
sub got_opt_freeze { TRUE  }
sub got_opt_name_list { $_[2] }
sub got_ExplainStmt_1 {
   return SQL::Translator::Statement::Explain->new(
      query   => $_[2],
      options => NIL,
   );
}
sub got_ExplainStmt_2 {
   return SQL::Translator::Statement::Explain->new(
      query   => $_[4],
      options => $_[0]->lappend(
         $_[0]->makeDefElem($_[3] ? "analyze" : "verbose", NULL)
      ),
   );
}
sub got_ExplainStmt_3 {
   return SQL::Translator::Statement::Explain->new(
      query   => $_[3],
      options => $_[0]->lappend($_[0]->makeDefElem("verbose", NULL)),
   );
}
sub got_ExplainStmt_4 {
   return SQL::Translator::Statement::Explain->new(
      query   => $_[5],
      options => $_[3],
   );
}
sub got_explain_option_list { $_[0]->lappend($_[1], $_[3]) }
sub got_explain_option_elem { $_[0]->makeDefElem($_[1], $_[2]) }
sub got_explain_option_name_1 { $_[1] }
sub got_explain_option_name_2 { "analyze" }
sub got_explain_option_name_3 { "verbose" }
sub got_explain_option_arg_1 { $_[1] }
sub got_explain_option_arg_2 { $_[1] }
sub got_PrepareStmt {
   return SQL::Translator::Statement::Prepare->new(
      name     => $_[2],
      argtypes => $_[3],
      query    => $_[5],
   );
}
sub got_prep_type_clause { $_[2] }
sub got_ExecuteStmt_1 {
   return SQL::Translator::Statement::Execute->new(
      name   => $_[2],
      params => $_[3],
   );
}
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
sub got_execute_param_clause { $_[2] }
sub got_DeallocateStmt_1 {
   return SQL::Translator::Statement::Deallocate->new(
      name => $_[2],
   );
}
sub got_DeallocateStmt_2 {
   return SQL::Translator::Statement::Deallocate->new(
      name => $_[3],
   );
}
sub got_DeallocateStmt_3 {
   return SQL::Translator::Statement::Deallocate->new(
      name => NULL,
   );
}
sub got_DeallocateStmt_4 {
   return SQL::Translator::Statement::Deallocate->new(
      name => NULL,
   );
}
sub got_InsertStmt {
   $_[5]->relation($_[4]);
   $_[5]->returningList($_[6]);
   $_[5]->withClause($_[1]);
   return $_[5];
}
sub got_insert_rest_1 {
   return SQL::Translator::Statement::Insert->new(
      cols       => NIL,
      selectStmt => $_[1],
   );
}
sub got_insert_rest_2 {
   return SQL::Translator::Statement::Insert->new(
      cols       => $_[2],
      selectStmt => $_[4],
   );
}
sub got_insert_rest_3 {
   return SQL::Translator::Statement::Insert->new(
      cols       => NIL,
      selectStmt => NULL,
   );
}
sub got_insert_column_list { $_[0]->lappend($_[1], $_[3]) }
sub got_insert_column_item {
   return SQL::Translator::Statement::ResultTarget->new(
      name        => $_[1],
      indirection => $_[0]->check_indirection($_[2]),
      val         => NULL,
      location    => $_[0]->YYLLoc($_[1], 1),
   );
}
sub got_returning_clause { $_[2] }
sub got_DeleteStmt {
   return SQL::Translator::Statement::Delete->new(
      relation => $_[4],
      usingClause => $_[5],
      whereClause => $_[6],
      returningList => $_[7],
      withClause => $_[1],
   );
}
sub got_using_clause { $_[2] }
sub got_LockStmt {
   return SQL::Translator::Statement::Lock->new(
      relations => $_[3],
      mode      => $_[4],
      nowait    => $_[5],
   );
}
sub got_opt_lock { $_[2]                      }
sub got_lock_type_1 { 'AccessShareLock'          }
sub got_lock_type_2 { 'RowShareLock'             }
sub got_lock_type_3 { 'RowExclusiveLock'         }
sub got_lock_type_4 { 'ShareUpdateExclusiveLock' }
sub got_lock_type_5 { 'ShareLock'                }
sub got_lock_type_6 { 'ShareRowExclusiveLock'    }
sub got_lock_type_7 { 'ExclusiveLock'            }
sub got_lock_type_8 { 'AccessExclusiveLock'      }
sub got_opt_nowait { TRUE  }
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
sub got_set_clause_list { $_[0]->lappend($_[1],$_[3]) }
sub got_set_clause_1 { $_[0]->lappend($_[1]) }
sub got_set_clause_2 { $_[1] }
sub got_single_set_clause {
   $_[1]->val($_[3]);
   return $_[1];
}
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
sub got_set_target {
   return SQL::Translator::Statement::ResultTarget->new(
      name        => $_[1],
      indirection => $_[0]->check_indirection($_[2]),
      val         => NULL,  #* upper production sets this
      location    => $_[0]->YYLLoc($_[1], 1),
   );
}
sub got_set_target_list { $_[0]->lappend($_[1],$_[3]) }
sub got_DeclareCursorStmt {
   return SQL::Translator::Statement::DeclareCursor->new(
      portalname => $_[2],
      #* currently we always set FAST_PLAN option
      options => $_[3] | $_[5] | CURSOR_OPT_FAST_PLAN,
      query => $_[7],
   );
}
sub got_cursor_name { $_[1] }
sub got_cursor_options_1 { $_[1] | CURSOR_OPT_NO_SCROLL   }
sub got_cursor_options_2 { $_[1] | CURSOR_OPT_SCROLL      }
sub got_cursor_options_3 { $_[1] | CURSOR_OPT_BINARY      }
sub got_cursor_options_4 { $_[1] | CURSOR_OPT_INSENSITIVE }
sub got_opt_hold_1 { CURSOR_OPT_HOLD }
sub got_opt_hold_2 { 0 }
sub got_select_with_parens_1 { $_[2] }
sub got_select_with_parens_2 { $_[2] }
sub got_select_no_parens_1 { $_[1] }
sub got_select_no_parens_2 {
   $_[0]->insertSelectOptions($_[1], $_[2], NIL, NULL, NULL, NULL);
   return $_[1];
}
sub got_select_no_parens_3 {
   $_[0]->insertSelectOptions($_[1], $_[2], $_[3], $_[4]->[0,1], NULL);
   return $_[1];
}
sub got_select_no_parens_4 {
   $_[0]->insertSelectOptions($_[1], $_[2], $_[4], $_[3]->[0,1], NULL);
   return $_[1];
}
sub got_select_no_parens_5 {
   $_[0]->insertSelectOptions($_[2], NULL, NIL, NULL, NULL, $_[1]);
   return $_[2];
}
sub got_select_no_parens_6 {
   $_[0]->insertSelectOptions($_[2], $_[3], NIL, NULL, NULL, $_[1]);
   return $_[2];
}
sub got_select_no_parens_7 {
   $_[0]->insertSelectOptions($_[2], $_[3], $_[4], $_[5]->[0,1], $_[1]);
   return $_[2];
}
sub got_select_no_parens_8 {
   $_[0]->insertSelectOptions($_[2], $_[3], $_[5], $_[4]->[0,1], $_[1]);
   return $_[2];
}
sub got_select_clause_1 { $_[1] }
sub got_select_clause_2 { $_[1] }
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
sub got_simple_select_2 { $_[1] }
# same as SELECT * FROM relation_expr
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
sub got_simple_select_4 { $_[0]->makeSetOp(SETOP_UNION,     $_[3], $_[1], $_[4]) }
sub got_simple_select_5 { $_[0]->makeSetOp(SETOP_INTERSECT, $_[3], $_[1], $_[4]) }
sub got_simple_select_6 { $_[0]->makeSetOp(SETOP_EXCEPT,    $_[3], $_[1], $_[4]) }
sub got_with_clause_1 {
   return SQL::Translator::Statement::WithClause->new(
      ctes      => $_[2],
      recursive => FALSE,
      location  => $_[0]->YYLLoc($_[1], 1),
   );
}
sub got_with_clause_2 {
   return SQL::Translator::Statement::WithClause->new(
      ctes      => $_[3],
      recursive => TRUE,
      location  => $_[0]->YYLLoc($_[1], 1),
   );
}
sub got_cte_list { $_[0]->lappend($_[1], $_[3]) }
sub got_common_table_expr {
   return SQL::Translator::Statement::CommonTableExpr->new(
      ctename       => $_[1],
      aliascolnames => $_[2],
      ctequery      => $_[5],
      location      => $_[0]->YYLLoc($_[1], 1),
   );
}
sub got_opt_with_clause { $_[1] }
sub got_into_clause {
   return SQL::Translator::Statement::IntoClause->new(
      rel            => $_[2],
      colNames       => NIL,
      options        => NIL,
      onCommit       => ONCOMMIT_NOOP,
      tableSpaceName => NULL,
      skipData       => FALSE,
   );
}
sub got_OptTempTableName_1 {
   $_[3]->relpersistence(RELPERSISTENCE_TEMP);
   return $_[3];
}
sub got_OptTempTableName_2 {
   $_[3]->relpersistence(RELPERSISTENCE_TEMP);
   return $_[3];
}
sub got_OptTempTableName_3 {
   $_[4]->relpersistence(RELPERSISTENCE_TEMP);
   return $_[4];
}
sub got_OptTempTableName_4 {
   $_[4]->relpersistence(RELPERSISTENCE_TEMP);
   return $_[4];
}
sub got_OptTempTableName_5 {
   $_[4]->relpersistence(RELPERSISTENCE_TEMP);
   return $_[4];
}
sub got_OptTempTableName_6 {
   $_[4]->relpersistence(RELPERSISTENCE_TEMP);
   return $_[4];
}
sub got_OptTempTableName_7 {
   $_[3]->relpersistence(RELPERSISTENCE_UNLOGGED);
   return $_[3];
}
sub got_OptTempTableName_8 {
   $_[2]->relpersistence(RELPERSISTENCE_PERMANENT);
   return $_[2];
}
sub got_OptTempTableName_9 {
   $_[1]->relpersistence(RELPERSISTENCE_PERMANENT);
   return $_[1];
}
sub got_opt_table {}
sub got_opt_all_1 { TRUE  }
sub got_opt_all_2 { FALSE }
sub got_opt_distinct_1 { $_[0]->lappend(NIL) }
sub got_opt_distinct_2 { $_[4] }
sub got_opt_distinct_3 { NIL   }
sub got_opt_sort_clause { $_[1] }
sub got_sort_clause { $_[3] }
sub got_sortby_list { $_[0]->lappend($_[1], $_[3]) }
sub got_sortby_1 {
   return SQL::Translator::Statement::SortBy->new(
      node         => $_[1],
      sortby_dir   => SORTBY_USING,
      sortby_nulls => $_[4],
      useOp        => $_[3],
      location     => $_[0]->YYLLoc($_[3], 3),
   );
}
sub got_sortby_2 {
   return SQL::Translator::Statement::SortBy->new(
      node         => $_[1],
      sortby_dir   => $_[2],
      sortby_nulls => $_[3],
      useOp        => NIL,
      location     => NULL,  #* no operator
   );
}
sub got_select_limit_1 { $_[0]->lappend($_[2], $_[1]) }
sub got_select_limit_2 { $_[0]->lappend($_[1], $_[2]) }
# Changed to support LimitYX
sub got_select_limit_3 { ref $_[1] eq 'ARRAY' ? $_[1] : $_[0]->lappend(NULL, $_[1]) }
sub got_select_limit_4 { $_[0]->lappend($_[1], NULL) }
sub got_opt_select_limit { $_[1] }
sub got_limit_clause_1 { $_[2] }
# SQL:2008 syntax
sub got_limit_clause_2 {
   $_[0]->lappend($_[1], $_[3]);
}
sub got_limit_clause_3 { $_[3] }
# SQL:2008 syntax
sub got_offset_clause_1 { $_[2] }
sub got_offset_clause_2 { $_[2] }
# LIMIT ALL is represented as a NULL constant
sub got_select_limit_value_1 { $_[1] }
sub got_select_limit_value_2 { $_[0]->makeNullAConst($_[0]->YYLLoc($_[1], 1)) }
sub got_select_offset_value { $_[1] }
sub got_opt_select_fetch_first_value_1 { $_[0]->makeIntConst($_[1], $_[0]->YYLLoc($_[1], 1)) }
sub got_opt_select_fetch_first_value_2 { $_[2] }
sub got_select_offset_value2 { $_[1] }
sub got_row_or_rows_1 { 0 }
sub got_row_or_rows_2 { 0 }
sub got_first_or_next_1 { 0 }
sub got_first_or_next_2 { 0 }
sub got_group_clause { $_[3] }
sub got_having_clause { $_[2] }
sub got_for_locking_clause_1 { $_[1] }
sub got_for_locking_clause_2 { NIL   }
sub got_opt_for_locking_clause { $_[1] }
sub got_for_locking_items { $_[0]->lappend($_[1], $_[2]) }
sub got_for_locking_item_1 {
   return SQL::Translator::Statement::LockingClause->new(
      lockedRels => $_[3],
      forUpdate  => TRUE,
      noWait     => $_[4],
   );
}
sub got_for_locking_item_2 {
   return SQL::Translator::Statement::LockingClause->new(
      lockedRels => $_[3],
      forUpdate  => FALSE,
      noWait     => $_[4],
   );
}
sub got_locked_rels_list { $_[2] }
sub got_values_clause_1 {
   return SQL::Translator::Statement::Select->new(
      valuesLists => $_[0]->lappend($_[2]),
   );
}
sub got_values_clause_2 {
   $_[1]->valuesLists( $_[0]->lappend($_[1]->valuesLists, $_[3]) );
   return $_[1];
}
sub got_from_clause { $_[2] }
sub got_from_list { $_[0]->lappend($_[1], $_[3]) }
sub got_table_ref_1 {
    $_[1]->alias($_[2]);
    return $_[1];
}
sub got_table_ref_2 {
	RangeFunction *n = makeNode(RangeFunction);
	n->lateral = false;
	n->funccallnode = $1;
	n->alias = linitial($2);
	n->coldeflist = lsecond($2);
	$$ = (Node *) n;
}
sub got_table_ref_3 {
	RangeFunction *n = makeNode(RangeFunction);
	n->lateral = true;
	n->funccallnode = $2;
	n->alias = linitial($3);
	n->coldeflist = lsecond($3);
	$$ = (Node *) n;
}
sub got_table_ref_4 {
    return SQL::Translator::Statement::Range::Function->new(
       lateral      => FALSE,
       funccallnode => $_[1],
       alias        => $_[2]->[0],
       coldeflist   => $_[2]->[1],
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
sub got_table_ref_5 {
    return SQL::Translator::Statement::Range::Function->new(
       lateral      => TRUE,
       funccallnode => $_[2],
       alias        => $_[3]->[0],
       coldeflist   => $_[3]->[1],
    );
}
sub got_table_ref_6 {
   $_[0]->ereport(ERROR,
         ERRCODE_SYNTAX_ERROR,
         ($_[1]->isa('SQL::Translator::Statement::Select') && $_[1]->valuesLists ? (
            "VALUES in FROM must have an alias",
            "For example, FROM (VALUES ...) [AS] foo.",
         ) : (
            "Subquery in FROM must have an alias",
            "For example, FROM (SELECT ...) [AS] foo.",
         )),
         $_[0]->YYLLoc($_[1], 1))
      unless ($_[2]);

   return SQL::Translator::Statement::Range::SubSelect->new(
      subquery => $_[1],
      alias    => $_[2],
   );
}
sub got_table_ref_7 {
   $_[0]->ereport(ERROR,
         ERRCODE_SYNTAX_ERROR,
         ($_[2]->isa('SQL::Translator::Statement::Select') && $_[2]->valuesLists ? (
            "VALUES in FROM must have an alias",
            "For example, FROM (VALUES ...) [AS] foo.",
         ) : (
            "Subquery in FROM must have an alias",
            "For example, FROM (SELECT ...) [AS] foo.",
         )),
         $_[0]->YYLLoc($_[2], 1))
      unless ($_[3]);

   return SQL::Translator::Statement::Range::SubSelect->new(
      subquery => $_[2],
      alias    => $_[3],
   );
}
sub got_table_ref_8 { $_[1] }
sub got_table_ref_9 {
   $_[2]->alias($_[4]);
   return $_[2];
}
sub got_joined_table_1 { $_[2] }
# CROSS JOIN is same as unqualified inner join
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
sub got_alias_clause_1 {
   return SQL::Translator::Statement::Alias->new(
      aliasname => $_[2],
      colnames  => $_[4],
   );
}
sub got_alias_clause_2 {
   return SQL::Translator::Statement::Alias->new(
      aliasname => $_[2],
   );
}
sub got_alias_clause_3 {
   return SQL::Translator::Statement::Alias->new(
      aliasname => $_[1],
      colnames  => $_[3],
   );
}
sub got_alias_clause_4 {
   return SQL::Translator::Statement::Alias->new(
      aliasname => $_[1],
   );
}
sub got_opt_alias_clause { $_[1] }
sub got_func_alias_clause_1 { [ $_[1], NIL  ] }
sub got_func_alias_clause_2 { [ NULL, $_[3] ] }
sub got_func_alias_clause_3 { [
               SQL::Translator::Statement::Alias->new( aliasname => $_[2] ),
               $_[4]
            ] }
sub got_func_alias_clause_4 { [
               SQL::Translator::Statement::Alias->new( aliasname => $_[1] ),
               $_[3]
            ] }
sub got_join_type_1 { JOIN_FULL }
sub got_join_type_2 { JOIN_LEFT }
sub got_join_type_3 { JOIN_RIGHT }
sub got_join_type_4 { JOIN_INNER }
sub got_join_outer { NULL }
sub got_join_qual_1 { $_[3] }
sub got_join_qual_2 { $_[2] }
# default inheritance
sub got_relation_expr_1 {
   $_[1]->inhOpt(INH_DEFAULT);
   $_[1]->alias(NULL);
   return $_[1];
}
# inheritance query
sub got_relation_expr_2 {
   $_[1]->inhOpt(INH_YES);
   $_[1]->alias(NULL);
   return $_[1];
}
# no inheritance
sub got_relation_expr_3 {
   $_[2]->inhOpt(INH_NO);
   $_[2]->alias(NULL);
   return $_[2];
}
# no inheritance, SQL99-style syntax
sub got_relation_expr_4 {
   $_[3]->inhOpt(INH_NO);
   $_[3]->alias(NULL);
   return $_[3];
}
sub got_relation_expr_list { $_[0]->lappend($_[1], $_[3]) }
sub got_relation_expr_opt_alias_1 { $_[1] }
sub got_relation_expr_opt_alias_2 {
   my $alias = SQL::Translator::Statement::Alias->new(
      aliasname => $_[2],
   );
   $_[1]->alias($alias);
   return $_[1];
}
sub got_relation_expr_opt_alias_3 {
   my $alias = SQL::Translator::Statement::Alias->new(
      aliasname => $_[3],
   );
   $_[1]->alias($alias);
   return $_[1];
}
sub got_func_table { $_[1] }
sub got_where_clause { $_[2] }
sub got_where_or_current_clause_1 { $_[2] }
sub got_where_or_current_clause_2 {
   return SQL::Translator::Statement::CurrentOfExpr->new(
      #* cvarno is filled in by parse analysis
      cursor_name  => $_[4],
      cursor_param => 0,
   );
}
sub got_OptTableFuncElementList { $_[1] }
sub got_TableFuncElementList { $_[0]->lappend($_[1], $_[3]) }
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
sub got_Typename_1 {
   $_[1]->arrayBounds($_[2]);
   return $_[1];
}
# SQL standard syntax, currently only one-dimensional
sub got_Typename_2 {
   $_[2]->arrayBounds($_[3]);
   $_[2]->setof(TRUE);
   return $_[2];
}
sub got_Typename_3 {
   $_[1]->arrayBounds( $_[0]->lappend($_[4]+0) );
   return $_[1];
}
sub got_Typename_4 {
   $_[2]->arrayBounds( $_[0]->lappend($_[5]+0) );
   $_[2]->setof(TRUE);
   return $_[2];
}
sub got_Typename_5 {
   $_[1]->arrayBounds( $_[0]->lappend(-1) );
   return $_[1];
}
sub got_Typename_6 {
   $_[2]->arrayBounds( $_[0]->lappend(-1) );
   $_[2]->setof(TRUE);
   return $_[2];
}
sub got_opt_array_bounds_1 { $_[0]->lappend($_[1], -1)      }
sub got_opt_array_bounds_2 { $_[0]->lappend($_[1], $_[3]+0) }
sub got_SimpleTypename_1 { $_[1] }
sub got_SimpleTypename_2 { $_[1] }
sub got_SimpleTypename_3 { $_[1] }
sub got_SimpleTypename_4 { $_[1] }
sub got_SimpleTypename_5 { $_[1] }
sub got_SimpleTypename_6 {
   $_[1]->typmods($_[2]);
   return $_[1];
}
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
sub got_ConstTypename_1 { $_[1] }
sub got_ConstTypename_2 { $_[1] }
sub got_ConstTypename_3 { $_[1] }
sub got_ConstTypename_4 { $_[1] }
sub got_GenericType_1 {
   my $n = $_[0]->makeTypeNameFromNameList([ $_[1] ]);
   $n->typmods($_[2]);
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}
sub got_GenericType_2 {
   my $n = $_[0]->makeTypeNameFromNameList($_[0]->lcons($_[1], $_[2]));
   $n->typmods($_[3]);
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}
sub got_opt_type_modifiers { $_[2] }
sub got_Numeric_1  {
   my $n = $_[0]->SystemTypeName("int4");
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}
sub got_Numeric_2  {
   my $n = $_[0]->SystemTypeName("int4");
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}
sub got_Numeric_3  {
   my $n = $_[0]->SystemTypeName("int2");
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}
sub got_Numeric_4  {
   my $n = $_[0]->SystemTypeName("int8");
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}
sub got_Numeric_5  {
   my $n = $_[0]->SystemTypeName("float4");
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}
sub got_Numeric_6  {
   $_[2]->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $_[2];
}
sub got_Numeric_7  {
   my $n = $_[0]->SystemTypeName("float8");
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}
sub got_Numeric_8  {
   my $n = $_[0]->SystemTypeName("numeric");
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   $n->typmods($_[2]);
   return $n;
}
sub got_Numeric_9  {
   my $n = $_[0]->SystemTypeName("numeric");
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   $n->typmods($_[2]);
   return $n;
}
sub got_Numeric_10 {
   my $n = $_[0]->SystemTypeName("numeric");
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   $n->typmods($_[2]);
   return $n;
}
sub got_Numeric_11 {
   my $n = $_[0]->SystemTypeName("bool");
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}
sub got_opt_float {
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
sub got_Bit_1 { $_[1] }
sub got_Bit_2 { $_[1] }
sub got_ConstBit_1 { $_[1] }
sub got_ConstBit_2 { $_[1]->typmods(NIL); $_[1]; }
sub got_BitWithLength {
   my $n = $_[0]->SystemTypeName($_[2] ? 'varbit' : 'bit');
   $n->typmods($_[4]);
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}
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
sub got_Character_1 { $_[1] }
sub got_Character_2 { $_[1] }
# Length was not specified so allow to be unrestricted.
# This handles problems with fixed-length (bpchar) strings
# which in column definitions must default to a length
# of one, but should not be constrained if the length
# was not specified.
sub got_ConstCharacter_1 { $_[1] }
sub got_ConstCharacter_2 {
   $_[1]->typmods(NIL);
   return $_[1];
}
sub got_CharacterWithLength {
   $_[1] .= '_'.$_[5]
      if (defined $_[5] && $_[5] eq "sql_text");

   my $n = $_[0]->SystemTypeName($_[1]);
   $n->typmods($_[0]->lappend($_[0]->makeIntConst($_[3], $_[0]->YYLLoc($_[3], 3))));
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}
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
sub got_character_1 { $_[2] ? "varchar": "bpchar" }
sub got_character_2 { $_[2] ? "varchar": "bpchar" }
sub got_character_3 { "varchar" }
sub got_character_4 { $_[3] ? "varchar": "bpchar" }
sub got_character_5 { $_[3] ? "varchar": "bpchar" }
sub got_character_6 { $_[2] ? "varchar": "bpchar" }
sub got_opt_varying { TRUE  }
sub got_opt_charset { $_[3] }
sub got_ConstDatetime_1 {
   my $n = $_[0]->SystemTypeName('timestamp'.($_[5] ? 'tz' : ''));
   $n->typmods($_[0]->lappend($_[0]->makeIntConst($_[3], $_[0]->YYLLoc($_[3], 3))));
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}
sub got_ConstDatetime_2 {
   my $n = $_[0]->SystemTypeName('timestamp'.($_[2] ? 'tz' : ''));
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}
sub got_ConstDatetime_3 {
   my $n = $_[0]->SystemTypeName('time'.($_[5] ? 'tz' : ''));
   $n->typmods($_[0]->lappend($_[0]->makeIntConst($_[3], $_[0]->YYLLoc($_[3], 3))));
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}
sub got_ConstDatetime_4 {
   my $n = $_[0]->SystemTypeName('time'.($_[2] ? 'tz' : ''));
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}
sub got_ConstInterval {
   my $n = $_[0]->SystemTypeName("interval");
   $n->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $n;
}
sub got_opt_timezone_1 { TRUE  }
sub got_opt_timezone_2 { FALSE }
sub got_opt_interval_1  { $_[0]->lappend( $_[0]->makeIntConst(INTERVAL_MASK_YEAR,   $_[0]->YYLLoc($_[1], 1))) }
sub got_opt_interval_2  { $_[0]->lappend( $_[0]->makeIntConst(INTERVAL_MASK_MONTH,  $_[0]->YYLLoc($_[1], 1))) }
sub got_opt_interval_3  { $_[0]->lappend( $_[0]->makeIntConst(INTERVAL_MASK_DAY,    $_[0]->YYLLoc($_[1], 1))) }
sub got_opt_interval_4  { $_[0]->lappend( $_[0]->makeIntConst(INTERVAL_MASK_HOUR,   $_[0]->YYLLoc($_[1], 1))) }
sub got_opt_interval_5  { $_[0]->lappend( $_[0]->makeIntConst(INTERVAL_MASK_MINUTE, $_[0]->YYLLoc($_[1], 1))) }
sub got_opt_interval_6  { $_[1] }
sub got_opt_interval_7  { $_[0]->lappend($_[0]->makeIntConst(INTERVAL_MASK_YEAR | INTERVAL_MASK_MONTH, $_[0]->YYLLoc($_[1], 1))) }
sub got_opt_interval_8  { $_[0]->lappend($_[0]->makeIntConst(INTERVAL_MASK_DAY  | INTERVAL_MASK_HOUR,  $_[0]->YYLLoc($_[1], 1))) }
sub got_opt_interval_9  { $_[0]->lappend($_[0]->makeIntConst(INTERVAL_MASK_DAY  | INTERVAL_MASK_HOUR | INTERVAL_MASK_MINUTE, $_[0]->YYLLoc($_[1], 1))) }
sub got_opt_interval_10 {
   $_[3]->[0] = $_[0]->makeIntConst(INTERVAL_MASK_DAY | INTERVAL_MASK_HOUR | INTERVAL_MASK_MINUTE | INTERVAL_MASK_SECOND, $_[0]->YYLLoc($_[1], 1));
   return $_[3];
}
sub got_opt_interval_11 { $_[0]->lappend($_[0]->makeIntConst(INTERVAL_MASK_HOUR | INTERVAL_MASK_MINUTE, $_[0]->YYLLoc($_[1], 1))) }
sub got_opt_interval_12 {
   $_[3]->[0] = $_[0]->makeIntConst(INTERVAL_MASK_HOUR | INTERVAL_MASK_MINUTE | INTERVAL_MASK_SECOND, $_[0]->YYLLoc($_[1], 1));
   return $_[3];
}
sub got_opt_interval_13 {
   $_[3]->[0] = $_[0]->makeIntConst(INTERVAL_MASK_MINUTE | INTERVAL_MASK_SECOND, $_[0]->YYLLoc($_[1], 1));
   return $_[3];
}
sub got_interval_second_1 { $_[0]->lappend($_[0]->makeIntConst(INTERVAL_MASK_SECOND, $_[0]->YYLLoc($_[1], 1))) }
sub got_interval_second_2 { $_[0]->lappend($_[0]->makeIntConst(INTERVAL_MASK_SECOND, $_[0]->YYLLoc($_[1], 1)), $_[0]->makeIntConst($_[3], $_[0]->YYLLoc($_[3], 3))) }
sub got_a_expr_1  { $_[1] }
sub got_a_expr_2  { $_[0]->makeTypeCast($_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
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
sub got_a_expr_5  { $_[0]->makeSimpleA_Expr(AEXPR_OP,  "+",  NULL, $_[2], $_[0]->YYLLoc($_[1], 1)) }
sub got_a_expr_6  { $_[0]->doNegate($_[2], $_[0]->YYLLoc($_[1], 1))                                }
sub got_a_expr_7  { $_[0]->makeSimpleA_Expr(AEXPR_OP,  "+", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
sub got_a_expr_8  { $_[0]->makeSimpleA_Expr(AEXPR_OP,  "-", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
sub got_a_expr_9  { $_[0]->makeSimpleA_Expr(AEXPR_OP,  "*", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
sub got_a_expr_10 { $_[0]->makeSimpleA_Expr(AEXPR_OP,  "/", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
sub got_a_expr_11 { $_[0]->makeSimpleA_Expr(AEXPR_OP,  "%", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
sub got_a_expr_12 { $_[0]->makeSimpleA_Expr(AEXPR_OP,  "^", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
sub got_a_expr_13 { $_[0]->makeSimpleA_Expr(AEXPR_OP,  "<", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
sub got_a_expr_14 { $_[0]->makeSimpleA_Expr(AEXPR_OP,  ">", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
sub got_a_expr_15 { $_[0]->makeSimpleA_Expr(AEXPR_OP,  "=", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
sub got_a_expr_16 { $_[0]->makeA_Expr(AEXPR_OP,  $_[2], $_[1], $_[3], $_[0]->YYLLoc($_[2], 2))     }
sub got_a_expr_17 { $_[0]->makeA_Expr(AEXPR_OP,  $_[1],  NULL, $_[2], $_[0]->YYLLoc($_[1], 1))     }
sub got_a_expr_18 { $_[0]->makeA_Expr(AEXPR_OP,  $_[2], $_[1],  NULL, $_[0]->YYLLoc($_[2], 2))     }
sub got_a_expr_19 { $_[0]->makeA_Expr(AEXPR_AND, NIL, $_[1], $_[3], $_[0]->YYLLoc($_[2], 2))       }
sub got_a_expr_20 { $_[0]->makeA_Expr(AEXPR_OR,  NIL, $_[1], $_[3], $_[0]->YYLLoc($_[2], 2))       }
sub got_a_expr_21 { $_[0]->makeA_Expr(AEXPR_NOT, NIL,  NULL, $_[2], $_[0]->YYLLoc($_[1], 1))       }
sub got_a_expr_22 { $_[0]->makeSimpleA_Expr(AEXPR_OP, "~~", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
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
sub got_a_expr_24 { $_[0]->makeSimpleA_Expr(AEXPR_OP, "!~~", $_[1], $_[4], $_[0]->YYLLoc($_[2], 2)) }
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
sub got_a_expr_26 { $_[0]->makeSimpleA_Expr(AEXPR_OP, "~~*", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
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
sub got_a_expr_28 { $_[0]->makeSimpleA_Expr(AEXPR_OP, "!~~*", $_[1], $_[4], $_[0]->YYLLoc($_[2], 2)) }
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
sub got_a_expr_34 {
   return SQL::Translator::Statement::NullTest->new(
      arg => $_[1],
      nulltesttype => IS_NULL,
   );
}
sub got_a_expr_35 {
   return SQL::Translator::Statement::NullTest->new(
      arg => $_[1],
      nulltesttype => IS_NULL,
   );
}
sub got_a_expr_36 {
   return SQL::Translator::Statement::NullTest->new(
      arg => $_[1],
      nulltesttype => IS_NOT_NULL,
   );
}
sub got_a_expr_37 {
   return SQL::Translator::Statement::NullTest->new(
      arg => $_[1],
      nulltesttype => IS_NOT_NULL,
   );
}
sub got_a_expr_38 { $_[0]->makeOverlaps($_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
sub got_a_expr_39 {
   return SQL::Translator::Statement::BooleanTest->new(
      arg          => $_[1],
      booltesttype => IS_TRUE,
   );
}
sub got_a_expr_40 {
   return SQL::Translator::Statement::BooleanTest->new(
      arg          => $_[1],
      booltesttype => IS_NOT_TRUE,
   );
}
sub got_a_expr_41 {
   return SQL::Translator::Statement::BooleanTest->new(
      arg          => $_[1],
      booltesttype => IS_FALSE,
   );
}
sub got_a_expr_42 {
   return SQL::Translator::Statement::BooleanTest->new(
      arg          => $_[1],
      booltesttype => IS_NOT_FALSE,
   );
}
sub got_a_expr_43 {
   return SQL::Translator::Statement::BooleanTest->new(
      arg          => $_[1],
      booltesttype => IS_UNKNOWN,
   );
}
sub got_a_expr_44 {
   return SQL::Translator::Statement::BooleanTest->new(
      arg          => $_[1],
      booltesttype => IS_NOT_UNKNOWN,
   );
}
sub got_a_expr_45 { $_[0]->makeSimpleA_Expr(AEXPR_DISTINCT, "=", $_[1], $_[5], $_[0]->YYLLoc($_[2], 2)) }
sub got_a_expr_46 {
   return $_[0]->makeA_Expr(
      AEXPR_NOT, NIL, NULL,
      $_[0]->makeSimpleA_Expr(AEXPR_DISTINCT, "=", $_[1], $_[6], $_[0]->YYLLoc($_[2], 2)),
      $_[0]->YYLLoc($_[2], 2)
   );
}
sub got_a_expr_47 { $_[0]->makeSimpleA_Expr(AEXPR_OF,  "=", $_[1], $_[5], $_[0]->YYLLoc($_[2], 2)) }
#   Ideally we would not use hard-wired operators below but
#   instead use opclasses.  However, mixed data types and other
#   issues make this difficult:
#   http://archives.postgresql.org/pgsql-hackers/2008-08/msg01142.php
sub got_a_expr_48 { $_[0]->makeSimpleA_Expr(AEXPR_OF, "<>", $_[1], $_[6], $_[0]->YYLLoc($_[2], 2)) }
sub got_a_expr_49 {
   return $_[0]->makeA_Expr(
      AEXPR_AND, NIL,
      $_[0]->makeSimpleA_Expr(AEXPR_OP, ">=", $_[1], $_[4], $_[0]->YYLLoc($_[2], 2)),
      $_[0]->makeSimpleA_Expr(AEXPR_OP, "<=", $_[1], $_[6], $_[0]->YYLLoc($_[2], 2)),
      $_[0]->YYLLoc($_[2], 2)
   );
}
sub got_a_expr_50 {
   return $_[0]->makeA_Expr(
      AEXPR_OR, NIL,
      $_[0]->makeSimpleA_Expr(AEXPR_OP, "<", $_[1], $_[5], $_[0]->YYLLoc($_[2], 2)),
      $_[0]->makeSimpleA_Expr(AEXPR_OP, ">", $_[1], $_[7], $_[0]->YYLLoc($_[2], 2)),
      $_[0]->YYLLoc($_[2], 2)
   );
}
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
sub got_a_expr_55 {
   return SQL::Translator::Statement::SubLink->new(
      subLinkType => $_[3],
      testexpr    => $_[1],
      operName    => $_[2],
      subselect   => $_[4],
      location    => $_[0]->YYLLoc($_[2], 2),
   );
}
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
sub got_a_expr_57 {
   $_[0]->ereport(ERROR,
         ERRCODE_FEATURE_NOT_SUPPORTED,
          "UNIQUE predicate is not yet implemented",
          $_[0]->YYLLoc($_[1], 1));
}
sub got_a_expr_58 { $_[0]->makeXmlExpr(IS_DOCUMENT, NULL, NIL, $_[0]->lappend($_[1]), $_[0]->YYLLoc($_[2], 2)) }
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
sub got_b_expr_1  { $_[1] }
sub got_b_expr_2  { $_[0]->makeTypeCast    ($_[1], $_[3],                      $_[0]->YYLLoc($_[2], 2)) }
sub got_b_expr_3  { $_[0]->makeSimpleA_Expr(AEXPR_OP,       "+",  NULL, $_[2], $_[0]->YYLLoc($_[1], 1)) }
sub got_b_expr_4  { $_[0]->doNegate        ($_[2],                             $_[0]->YYLLoc($_[1], 1)) }
sub got_b_expr_5  { $_[0]->makeSimpleA_Expr(AEXPR_OP,       "+", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
sub got_b_expr_6  { $_[0]->makeSimpleA_Expr(AEXPR_OP,       "-", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
sub got_b_expr_7  { $_[0]->makeSimpleA_Expr(AEXPR_OP,       "*", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
sub got_b_expr_8  { $_[0]->makeSimpleA_Expr(AEXPR_OP,       "/", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
sub got_b_expr_9  { $_[0]->makeSimpleA_Expr(AEXPR_OP,       "%", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
sub got_b_expr_10 { $_[0]->makeSimpleA_Expr(AEXPR_OP,       "^", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
sub got_b_expr_11 { $_[0]->makeSimpleA_Expr(AEXPR_OP,       "<", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
sub got_b_expr_12 { $_[0]->makeSimpleA_Expr(AEXPR_OP,       ">", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
sub got_b_expr_13 { $_[0]->makeSimpleA_Expr(AEXPR_OP,       "=", $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
sub got_b_expr_14 { $_[0]->makeA_Expr      (AEXPR_OP,     $_[2], $_[1], $_[3], $_[0]->YYLLoc($_[2], 2)) }
sub got_b_expr_15 { $_[0]->makeA_Expr      (AEXPR_OP,     $_[1],  NULL, $_[2], $_[0]->YYLLoc($_[1], 1)) }
sub got_b_expr_16 { $_[0]->makeA_Expr      (AEXPR_OP,     $_[2], $_[1],  NULL, $_[0]->YYLLoc($_[2], 2)) }
sub got_b_expr_17 { $_[0]->makeSimpleA_Expr(AEXPR_DISTINCT, "=", $_[1], $_[5], $_[0]->YYLLoc($_[2], 2)) }
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
sub got_b_expr_19 { $_[0]->makeSimpleA_Expr(AEXPR_OF,  "=", $_[1],  $_[5], $_[0]->YYLLoc($_[2], 2)) }
sub got_b_expr_20 { $_[0]->makeSimpleA_Expr(AEXPR_OF, "<>", $_[1],  $_[6], $_[0]->YYLLoc($_[2], 2)) }
sub got_b_expr_21 {
   return $_[0]->makeXmlExpr(
      IS_DOCUMENT, NULL, NIL,
      $_[0]->lappend($_[1]),
      $_[0]->YYLLoc($_[2], 2)
   );
}
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
sub got_c_expr_1  { $_[1] }
sub got_c_expr_2  { $_[1] }
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
sub got_c_expr_4  {
   if ($_[4]) {
      return SQL::Translator::Statement::A_Indirection->new(
         arg         => $_[2],
         indirection => $_[0]->check_indirection($_[4]),
      );
   }
   return $_[2];
}
sub got_c_expr_5  { $_[1] }
sub got_c_expr_6  { $_[1] }
sub got_c_expr_7  {
   return SQL::Translator::Statement::SubLink->new(
      subLinkType => EXPR_SUBLINK,
      testexpr    => NULL,
      operName    => NIL,
      subselect   => $_[1],
      location    => $_[0]->YYLLoc($_[1], 1),
   );
}
sub got_c_expr_8  {
   return SQL::Translator::Statement::SubLink->new(
      subLinkType => EXISTS_SUBLINK,
      testexpr    => NULL,
      operName    => NIL,
      subselect   => $_[2],
      location    => $_[0]->YYLLoc($_[1], 1),
   );
}
sub got_c_expr_9  {
   return SQL::Translator::Statement::SubLink->new(
      subLinkType => ARRAY_SUBLINK,
      testexpr    => NULL,
      operName    => NIL,
      subselect   => $_[2],
      location    => $_[0]->YYLLoc($_[1], 1),
   );
}
sub got_c_expr_10 {
   #* point outermost A_ArrayExpr to the ARRAY keyword
   $_[2]->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $_[2];
}
sub got_c_expr_11 {
   return SQL::Translator::Statement::RowExpr->new(
      args       => $_[1],
      row_typeid => InvalidOid,   #* not analyzed yet
      colnames   => NIL,          #* to be filled in during analysis
      location   => $_[0]->YYLLoc($_[1], 1),
   );
}
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
sub got_func_expr_9  {
   return SQL::Translator::Statement::Function::Call->new(
      funcname      => $_[0]->SystemFuncName("pg_collation_for"),
      args          => [ $_[4] ],
      agg_order     => NIL,
      agg_star      => FALSE,
      agg_distinct  => FALSE,
      func_variadic => FALSE,
      over          => NULL,
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
sub got_func_expr_10 {
   my $n = $_[0]->makeStringConstCast("now", $_[0]->YYLLoc($_[1], 1), $_[0]->SystemTypeName("text"));
   return $_[0]->makeTypeCast($n, SystemTypeName("date"), {});
}
# Translate as "'now'::text::timetz".
# See comments for CURRENT_DATE.
sub got_func_expr_11 {
   my $n = $_[0]->makeStringConstCast("now", $_[0]->YYLLoc($_[1], 1), $_[0]->SystemTypeName("text"));
   return $_[0]->makeTypeCast($n, SystemTypeName("timetz"), {});
}
# Translate as "'now'::text::timetz(n)".
# See comments for CURRENT_DATE.
sub got_func_expr_12 {
   my $n = $_[0]->makeStringConstCast("now", $_[0]->YYLLoc($_[1], 1), $_[0]->SystemTypeName("text"));
   my $d = $_[0]->SystemTypeName("timetz");
   $d->typmods( $_[0]->lappend($_[0]->makeIntConst($_[3], $_[0]->YYLLoc($_[3], 3))) );
   return $_[0]->makeTypeCast($n, $d, {});
}
# Translate as "now()", since we have a function that
# does exactly what is needed.
sub got_func_expr_13 {
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
sub got_func_expr_14 {
   my $n = $_[0]->makeStringConstCast("now", $_[0]->YYLLoc($_[1], 1), $_[0]->SystemTypeName("text"));
   my $d = $_[0]->SystemTypeName("timestamptz");
   $d->typmods( $_[0]->lappend($_[0]->makeIntConst($_[3], $_[0]->YYLLoc($_[3], 3))) );
   return $_[0]->makeTypeCast($n, $d, {});
}
# Translate as "'now'::text::time".
# See comments for CURRENT_DATE.
sub got_func_expr_15 {
   my $n = $_[0]->makeStringConstCast("now", $_[0]->YYLLoc($_[1], 1), $_[0]->SystemTypeName("text"));
   return $_[0]->makeTypeCast($n, SystemTypeName("time"), {});
}
# Translate as "'now'::text::time(n)".
# See comments for CURRENT_DATE.
sub got_func_expr_16 {
   my $n = $_[0]->makeStringConstCast("now", $_[0]->YYLLoc($_[1], 1), $_[0]->SystemTypeName("text"));
   my $d = $_[0]->SystemTypeName("time");
   $d->typmods( $_[0]->lappend($_[0]->makeIntConst($_[3], $_[0]->YYLLoc($_[3], 3))) );
   return $_[0]->makeTypeCast($n, $d, {});
}
# Translate as "'now'::text::timestamp".
# See comments for CURRENT_DATE.
sub got_func_expr_17 {
   my $n = $_[0]->makeStringConstCast("now", $_[0]->YYLLoc($_[1], 1), $_[0]->SystemTypeName("text"));
   return $_[0]->makeTypeCast($n, SystemTypeName("timestamp"), {});
}
# Translate as "'now'::text::timestamp(n)".
# See comments for CURRENT_DATE.
sub got_func_expr_18 {
   my $n = $_[0]->makeStringConstCast("now", $_[0]->YYLLoc($_[1], 1), $_[0]->SystemTypeName("text"));
   my $d = $_[0]->SystemTypeName("timestamp");
   $d->typmods( $_[0]->lappend($_[0]->makeIntConst($_[3], $_[0]->YYLLoc($_[3], 3))) );
   return $_[0]->makeTypeCast($n, $d, {});
}
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
sub got_func_expr_20 {
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
sub got_func_expr_21 {
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
sub got_func_expr_22 {
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
sub got_func_expr_23 {
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
sub got_func_expr_24 {
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
sub got_func_expr_25 { $_[0]->makeTypeCast($_[3], $_[5], $_[0]->YYLLoc($_[1], 1)) }
sub got_func_expr_26 {
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
sub got_func_expr_27 {
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
sub got_func_expr_28 {
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
sub got_func_expr_29 {
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
sub got_func_expr_30 {
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
sub got_func_expr_31 {
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
sub got_func_expr_32 {
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
sub got_func_expr_33 {
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
sub got_func_expr_34 {
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
sub got_func_expr_35 { $_[0]->makeSimpleA_Expr(AEXPR_NULLIF, "=", $_[3], $_[5], $_[0]->YYLLoc($_[1], 1)) }
sub got_func_expr_36 {
   return SQL::Translator::Statement::CoalesceExpr->new(
      args     => $_[3],
      location => $_[0]->YYLLoc($_[1], 1),
   );
}
sub got_func_expr_37 {
   return SQL::Translator::Statement::MinMaxExpr->new(
      args     => $_[3],
      op       => IS_GREATEST,
      location => $_[0]->YYLLoc($_[1], 1),
   );
}
sub got_func_expr_38 {
   return SQL::Translator::Statement::MinMaxExpr->new(
      args     => $_[3],
      op       => IS_LEAST,
      location => $_[0]->YYLLoc($_[1], 1),
   );
}
sub got_func_expr_39 { $_[0]->makeXmlExpr(IS_XMLCONCAT, NULL, NIL, $_[3], $_[0]->YYLLoc($_[1], 1)) }
sub got_func_expr_40 { $_[0]->makeXmlExpr(IS_XMLELEMENT, $_[4], NIL, NIL, $_[0]->YYLLoc($_[1], 1)) }
sub got_func_expr_41 { $_[0]->makeXmlExpr(IS_XMLELEMENT, $_[4], $_[6], NIL, $_[0]->YYLLoc($_[1], 1)) }
sub got_func_expr_42 { $_[0]->makeXmlExpr(IS_XMLELEMENT, $_[4], NIL, $_[6], $_[0]->YYLLoc($_[1], 1)) }
sub got_func_expr_43 { $_[0]->makeXmlExpr(IS_XMLELEMENT, $_[4], $_[6], $_[8], $_[0]->YYLLoc($_[1], 1)) }
# xmlexists(A PASSING [BY REF] B [BY REF]) is
# converted to xmlexists(A, B)
sub got_func_expr_44 {
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
sub got_func_expr_45 { $_[0]->makeXmlExpr(IS_XMLFOREST, NULL, $_[3], NIL, $_[0]->YYLLoc($_[1], 1)) }
sub got_func_expr_46 {
   my $x = $_[0]->makeXmlExpr(IS_XMLPARSE, NULL, NIL,
               $_[0]->lappend($_[4], $_[0]->makeBoolAConst($_[5], {})),
               $_[0]->YYLLoc($_[1], 1));
   $x->xmloption($_[3]);
   return $x;
}
sub got_func_expr_47 { $_[0]->makeXmlExpr(IS_XMLPI, $_[4], NULL, NIL, $_[0]->YYLLoc($_[1], 1)) }
sub got_func_expr_48 { $_[0]->makeXmlExpr(IS_XMLPI, $_[4], NULL, $_[0]->lappend($_[6]), $_[0]->YYLLoc($_[1], 1)) }
sub got_func_expr_49 {
   return $_[0]->makeXmlExpr(IS_XMLROOT, NULL, NIL, $_[0]->lappend($_[3], $_[5], $_[6]), $_[0]->YYLLoc($_[1], 1));
}
sub got_func_expr_50 {
   return SQL::Translator::Statement::XMLSerialize->new(
      xmloption => $_[3],
      expr      => $_[4],
      typeName  => $_[6],
      location  => $_[0]->YYLLoc($_[1], 1),
   );
}
sub got_xml_root_version_1 { $_[2] }
sub got_xml_root_version_2 { $_[0]->makeNullAConst({}) }
sub got_opt_xml_root_standalone_1 { $_[0]->makeStringConst('XML_STANDALONE_YES',      {}) }
sub got_opt_xml_root_standalone_2 { $_[0]->makeStringConst('XML_STANDALONE_NO',       {}) }
sub got_opt_xml_root_standalone_3 { $_[0]->makeStringConst('XML_STANDALONE_NO_VALUE', {}) }
sub got_xml_attributes { $_[3] }
sub got_xml_attribute_list { $_[0]->lappend($_[1], $_[3]) }
sub got_xml_attribute_el_1 {
   return SQL::Translator::Statement::ResultTarget->new(
      name        => $_[3],
      indirection => NIL,
      val         => $_[1],
      location    => $_[0]->YYLLoc($_[1], 1),
   );
}
sub got_xml_attribute_el_2 {
   return SQL::Translator::Statement::ResultTarget->new(
      name        => NULL,
      indirection => NIL,
      val         => $_[1],
      location    => $_[0]->YYLLoc($_[1], 1),
   );
}
sub got_document_or_content_1 { XMLOPTION_DOCUMENT }
sub got_document_or_content_2 { XMLOPTION_CONTENT  }
sub got_xml_whitespace_option_1 { TRUE  }
sub got_xml_whitespace_option_2 { FALSE }
sub got_xmlexists_argument_1 { $_[2] }
sub got_xmlexists_argument_2 { $_[2] }
sub got_xmlexists_argument_3 { $_[4] }
sub got_xmlexists_argument_4 { $_[4] }
sub got_window_clause { $_[2] }
sub got_window_definition_list { $_[0]->lappend($_[1], $_[3]) }
sub got_window_definition {
   $_[3]->name($_[1]);
   return $_[3];
}
sub got_over_clause_1 { $_[2] }
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
sub got_opt_existing_window_name { $_[1] }
sub got_opt_partition_clause { $_[3] }
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
sub got_opt_frame_clause_2 {
   $_[2]->frameOptions($_[2]->frameOptions | FRAMEOPTION_NONDEFAULT | FRAMEOPTION_ROWS);
   return $_[2];
}
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
sub got_frame_bound_1 {
   return SQL::Translator::Statement::WindowDef->new(
      frameOptions => FRAMEOPTION_START_UNBOUNDED_PRECEDING,
      startOffset  => NULL,
      endOffset    => NULL,
   );
}
sub got_frame_bound_2 {
   return SQL::Translator::Statement::WindowDef->new(
      frameOptions => FRAMEOPTION_START_UNBOUNDED_FOLLOWING,
      startOffset  => NULL,
      endOffset    => NULL,
   );
}
sub got_frame_bound_3 {
   return SQL::Translator::Statement::WindowDef->new(
      frameOptions => FRAMEOPTION_START_CURRENT_ROW,
      startOffset  => NULL,
      endOffset    => NULL,
   );
}
sub got_frame_bound_4 {
   return SQL::Translator::Statement::WindowDef->new(
      frameOptions => FRAMEOPTION_START_VALUE_PRECEDING,
      startOffset  => $_[1],
      endOffset    => NULL,
   );
}
sub got_frame_bound_5 {
   return SQL::Translator::Statement::WindowDef->new(
      frameOptions => FRAMEOPTION_START_VALUE_FOLLOWING,
      startOffset  => $_[1],
      endOffset    => NULL,
   );
}
sub got_row_1 { $_[3] }
sub got_row_2 { NULL }
sub got_row_3 { $_[0]->lappend($_[2], $_[4]) }
sub got_sub_type_1 { ANY_SUBLINK }
sub got_sub_type_2 { ANY_SUBLINK }
sub got_sub_type_3 { ALL_SUBLINK }
sub got_all_Op_1 { $_[1] }
sub got_all_Op_2 { $_[1] }
sub got_MathOp_1 { "+" }
sub got_MathOp_2 { "-" }
sub got_MathOp_3 { "*" }
sub got_MathOp_4 { "/" }
sub got_MathOp_5 { "%" }
sub got_MathOp_6 { "^" }
sub got_MathOp_7 { "<" }
sub got_MathOp_8 { ">" }
sub got_MathOp_9 { "=" }
sub got_qual_Op_1 { $_[0]->lappend($_[1]) }
sub got_qual_Op_2 { $_[3] }
sub got_qual_all_Op_1 { $_[0]->lappend($_[1]) }
sub got_qual_all_Op_2 { $_[3] }
sub got_subquery_Op_1 { $_[0]->lappend($_[1])  }
sub got_subquery_Op_2 { $_[3]                  }
sub got_subquery_Op_3 { $_[0]->lappend("~~")   }
sub got_subquery_Op_4 { $_[0]->lappend("!~~")  }
sub got_subquery_Op_5 { $_[0]->lappend("~~*")  }
# cannot put SIMILAR TO here, because SIMILAR TO is a hack.
# the regular expression is preprocessed by a function (similar_escape),
# and the ~ operator for posix regular expressions is used.
#        x SIMILAR TO y     ->    x ~ similar_escape(y)
# this transformation is made on the fly by the parser upwards.
# however the SubLink structure which handles any/some/all stuff
# is not ready for such a thing.
sub got_subquery_Op_6 { $_[0]->lappend("!~~*") }
sub got_expr_list { $_[0]->lappend($_[1], $_[3]) }
sub got_func_arg_list { $_[0]->lappend($_[1], $_[3]) }
sub got_func_arg_expr_1 { $_[1] }
sub got_func_arg_expr_2 {
   return SQL::Translator::Statement::NamedArgExpr->new(
      name      => $_[1],
      arg       => $_[3],
      argnumber => -1,      #* until determined
      location  => $_[0]->YYLLoc($_[1], 1),
   );
}
sub got_type_list { $_[0]->lappend($_[1], $_[3]) }
sub got_array_expr_1 { $_[0]->makeAArrayExpr($_[2], $_[0]->YYLLoc($_[1], 1)) }
sub got_array_expr_2 { $_[0]->makeAArrayExpr($_[2], $_[0]->YYLLoc($_[1], 1)) }
sub got_array_expr_3 { $_[0]->makeAArrayExpr(NIL,   $_[0]->YYLLoc($_[1], 1)) }
sub got_array_expr_list { $_[0]->lappend($_[1], $_[3]) }
sub got_extract_list { $_[0]->lappend($_[0]->makeStringConst($_[1], $_[0]->YYLLoc($_[1], 1)), $_[3]) }
sub got_extract_arg_1 { $_[1]    }
sub got_extract_arg_2 { "year"   }
sub got_extract_arg_3 { "month"  }
sub got_extract_arg_4 { "day"    }
sub got_extract_arg_5 { "hour"   }
sub got_extract_arg_6 { "minute" }
sub got_extract_arg_7 { "second" }
sub got_extract_arg_8 { $_[1]    }
sub got_overlay_list_1 { $_[0]->lappend($_[1], $_[2], $_[3], $_[4]) }
sub got_overlay_list_2 { $_[0]->lappend($_[1], $_[2], $_[3])        }
sub got_overlay_placing { $_[2] }
sub got_position_list { $_[0]->lappend($_[3], $_[1]) }
# not legal per SQL99, but might as well allow it
sub got_substr_list_1 { $_[0]->lappend($_[1], $_[2], $_[3]) }
sub got_substr_list_2 { $_[0]->lappend($_[1], $_[3], $_[2]) }
sub got_substr_list_3 { $_[0]->lappend($_[1], $_[2]) }
# Since there are no cases where this syntax allows
# a textual FOR value, we forcibly cast the argument
# to int4.  The possible matches in pg_proc are
# substring(text,int4) and substring(text,text),
# and we don't want the parser to choose the latter,
# which it is likely to do if the second argument
# is unknown or doesn't have an implicit cast to int4.
sub got_substr_list_4 {
   return $_[0]->lappend(
      $_[1], $_[0]->makeIntConst(1, {}),
      $_[0]->makeTypeCast($_[2], $_[0]->SystemTypeName("int4"), {})
   );
}
sub got_substr_list_5 { $_[1] }
sub got_substr_from { $_[2] }
sub got_substr_for { $_[2] }
sub got_trim_list_1 { $_[0]->lappend($_[3], $_[1]) }
sub got_trim_list_2 { $_[2] }
sub got_trim_list_3 { $_[1] }
sub got_in_expr_1 {
   return SQL::Translator::Statement::SubLink->new(
      subselect => $_[1],
      #* other fields will be filled later
   );
}
sub got_in_expr_2 { $_[2] }
sub got_case_expr {
   return SQL::Translator::Statement::CaseExpr->new(
      casetype  => InvalidOid,  #* not analyzed yet
      arg       => $_[2],
      args      => $_[3],
      defresult => $_[4],
      location  => $_[0]->YYLLoc($_[1], 1),
   );
}
sub got_when_clause_list { $_[0]->lappend($_[1], $_[2]) }
sub got_when_clause {
   return SQL::Translator::Statement::CaseWhen->new(
      expr     => $_[2],
      result   => $_[4],
      location => $_[0]->YYLLoc($_[1], 1),
   );
}
sub got_case_default { $_[2] }
sub got_case_arg { $_[1] }
sub got_columnref_1 { $_[0]->makeColumnRef($_[1],   NIL, $_[0]->YYLLoc($_[1], 1)) }
sub got_columnref_2 { $_[0]->makeColumnRef($_[1], $_[2], $_[0]->YYLLoc($_[1], 1)) }
sub got_indirection_el_1 { $_[2] }
sub got_indirection_el_2 { SQL::Translator::Statement::A_Star->new() }
sub got_indirection_el_3 {
   return SQL::Translator::Statement::A_Indices->new(
      lidx => NULL,
      uidx => $_[2],
   );
}
sub got_indirection_el_4 {
   return SQL::Translator::Statement::A_Indices->new(
      lidx => $_[2],
      uidx => $_[4],
   );
}
sub got_indirection { $_[0]->lappend($_[1], $_[2]) }
sub got_opt_indirection { $_[0]->lappend($_[1], $_[2]) }
sub got_ctext_expr_1 {  $_[1] }
sub got_ctext_expr_2 {
   return SQL::Translator::Statement::SetToDefault->new(
      location => $_[0]->YYLLoc($_[1], 1),
   );
}
sub got_ctext_expr_list { $_[0]->lappend($_[1], $_[3]) }
sub got_ctext_row { $_[2] }
sub got_target_list { $_[0]->lappend($_[1], $_[3]) }
# We support omitting AS only for column labels that aren't
# any known keyword.  There is an ambiguity against postfix
# operators: is "a ! b" an infix expression, or a postfix
# expression and a column label?  We prefer to resolve this
# as an infix expression, which we accomplish by assigning
# IDENT a precedence higher than POSTFIXOP.
sub got_target_el_1 {
   return SQL::Translator::Statement::ResultTarget->new(
      name        => $_[3],
      indirection => NIL,
      val         => $_[1],
      location    => $_[0]->YYLLoc($_[1], 1),
   );
}
sub got_target_el_2 {
   return SQL::Translator::Statement::ResultTarget->new(
      name        => $_[2],
      indirection => NIL,
      val         => $_[1],
      location    => $_[0]->YYLLoc($_[1], 1),
   );
}
sub got_target_el_3 {
   return SQL::Translator::Statement::ResultTarget->new(
      name        => NULL,
      indirection => NIL,
      val         => $_[1],
      location    => $_[0]->YYLLoc($_[1], 1),
   );
}
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
sub got_qualified_name_list { $_[0]->lappend($_[1], $_[3]) }
sub got_qualified_name_1 { $_[0]->makeRangeVar(NULL, $_[1], $_[0]->YYLLoc($_[1], 1)) }
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
sub got_name_list { $_[0]->lappend($_[1], $_[3]) }
sub got_name { $_[1] }
sub got_database_name { $_[1] }
sub got_access_method { $_[1] }
sub got_attr_name { $_[1] }
sub got_index_name { $_[1] }
sub got_file_name { $_[1] }
sub got_func_name_1 { $_[0]->lappend($_[1]) }
sub got_func_name_2 { $_[0]->check_func_name($_[0]->lcons($_[1], $_[2])) }
sub got_AexprConst_1  { $_[0]->makeIntConst      ($_[1], $_[0]->YYLLoc($_[1], 1)) }
sub got_AexprConst_2  { $_[0]->makeFloatConst    ($_[1], $_[0]->YYLLoc($_[1], 1)) }
sub got_AexprConst_3  { $_[0]->makeStringConst   ($_[1], $_[0]->YYLLoc($_[1], 1)) }
# This is a bit constant per SQL99:
# Without Feature F511, "BIT data type",
# a <general literal> shall not be a
# <bit string literal> or a <hex string literal>.
sub got_AexprConst_4  { $_[0]->makeBitStringConst($_[1], $_[0]->YYLLoc($_[1], 1)) }
sub got_AexprConst_5  { $_[0]->makeBitStringConst($_[1], $_[0]->YYLLoc($_[1], 1)) }
sub got_AexprConst_6  {
   #* generic type 'literal' syntax
   my $t = $_[0]->makeTypeNameFromNameList($_[1]);
   $t->_set_location( $_[0]->YYLLoc($_[1], 1) );
   return $_[0]->makeStringConstCast($_[2], $_[0]->YYLLoc($_[2], 2), $t);
}
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
sub got_AexprConst_8  { $_[0]->makeStringConstCast($_[2], $_[0]->YYLLoc($_[2], 2), $_[1]) }
sub got_AexprConst_9  {
   $_[1]->typmods($_[3]);
   return $_[0]->makeStringConstCast($_[2], $_[0]->YYLLoc($_[2], 2), $_[1]);
}
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
sub got_AexprConst_11 { $_[0]->makeBoolAConst(TRUE,  $_[0]->YYLLoc($_[1], 1)) }
sub got_AexprConst_12 { $_[0]->makeBoolAConst(FALSE, $_[0]->YYLLoc($_[1], 1)) }
sub got_AexprConst_13 { $_[0]->makeNullAConst($_[0]->YYLLoc($_[1], 1))        }
sub got_Iconst { $_[1] }
sub got_Sconst { $_[1] }
sub got_RoleId { $_[1] }
sub got_SignedIconst_1 { $_[1] }
sub got_SignedIconst_2 { + $_[2] }
sub got_SignedIconst_3 { - $_[2] }
sub got_ColId_1 { $_[1] }
sub got_ColId_2 { $_[1] }
sub got_ColId_3 { $_[1] }
sub got_type_function_name_1 { $_[1] }
sub got_type_function_name_2 { $_[1] }
sub got_type_function_name_3 { $_[1] }
sub got_ColLabel_1 { $_[1] }
sub got_ColLabel_2 { $_[1] }
sub got_ColLabel_3 { $_[1] }
sub got_ColLabel_4 { $_[1] }
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

