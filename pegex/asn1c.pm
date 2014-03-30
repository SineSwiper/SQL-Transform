package CLASS::AST;

use Moo;

extends 'Pegex::Tree';

## %% ##


## %% ##

sub got_ParsedGrammar {
   *(void **)param = $1;
}
sub got_ModuleList {
   $$ = $1;
   TQ_ADD(&($$->modules), $2, mod_next);
}
sub got_ModuleDefinition {

   $$ = currentModule;

   if($8) {
      asn1p_module_t tmp = *($$);
      *($$) = *($8);
      *($8) = tmp;
      asn1p_module_free($8);
   } else {
      /* There's a chance that a module is just plain empty */
   }

   $$->ModuleName = $1;
   $$->module_oid = $3;
   $$->module_flags = $5;
}
sub got_optObjectIdentifier { $$ = $1; }
sub got_ObjectIdentifier_1 {
   $$ = $2;
}
sub got_ObjectIdentifier_2 {
   $$ = 0;
}
sub got_ObjectIdentifierBody {
   $$ = $1;
   asn1p_oid_add_arc($$, &$2);
   if($2.name)
      free($2.name);
}
sub got_ObjectIdentifierElement_1 {               /* iso */
   $$.name = $1;
   $$.number = -1;
}
sub got_ObjectIdentifierElement_2 {      /* iso(1) */
   $$.name = $1;
   $$.number = $3;
}
sub got_ObjectIdentifierElement_3 {               /* 1 */
   $$.name = 0;
   $$.number = $1;
}
sub got_optModuleDefinitionFlags {
   $$ = $1;
}
sub got_ModuleDefinitionFlags {
   $$ = $1 | $2;
}
sub got_ModuleDefinitionFlag_1 {
   $$ = MSF_EXPLICIT_TAGS;
}
sub got_ModuleDefinitionFlag_2 {
   $$ = MSF_IMPLICIT_TAGS;
}
sub got_ModuleDefinitionFlag_3 {
   $$ = MSF_AUTOMATIC_TAGS;
}
# EncodingReferenceDefault
sub got_ModuleDefinitionFlag_4 {
   $$ = MSF_EXTENSIBILITY_IMPLIED;
}
sub got_optModuleBody {
   $$ = $1;
}
sub got_ModuleBody {
   $$ = asn1p_module_new();
   AL_IMPORT($$, exports, $1, xp_next);
   AL_IMPORT($$, imports, $2, xp_next);
   AL_IMPORT($$, members, $3, next);
}
sub got_AssignmentList {
   if($1) {
      $$ = $1;
   } else {
      $$ = $2;
      break;
   }
   AL_IMPORT($$, members, $2, next);
}
sub got_Assignment_1 {
   $$ = asn1p_module_new();
   checkmem($$);
   assert($1->expr_type != A1TC_INVALID);
   assert($1->meta_type != AMT_INVALID);
   TQ_ADD(&($$->members), $1, next);
}
# Value set definition
# === EXAMPLE ===
# EvenNumbers INTEGER ::= { 2 | 4 | 6 | 8 }
# === EOF ===
sub got_Assignment_2 {
   $$ = asn1p_module_new();
   checkmem($$);
   assert($1->expr_type != A1TC_INVALID);
   assert($1->meta_type != AMT_INVALID);
   TQ_ADD(&($$->members), $1, next);
}
sub got_Assignment_3 {
   $$ = asn1p_module_new();
   checkmem($$);
   assert($1->expr_type != A1TC_INVALID);
   assert($1->meta_type != AMT_INVALID);
   TQ_ADD(&($$->members), $1, next);
}
sub got_Assignment_4 {
   return yyerror(
      "Attempt to redefine a standard basic string type, "
      "please comment out or remove this type redefinition.");
}
# Some error cases.
sub got_ImportsDefinition_1 {
   if(!saved_aid && 0)
      return yyerror("Unterminated IMPORTS FROM, "
            "expected semicolon ';'");
   saved_aid = 0;
   $$ = $2;
}
# ...
sub got_ImportsDefinition_2 {
   return yyerror("Empty IMPORTS list");
}
sub got_ImportsBundleSet {
   $$ = $1;
   TQ_ADD(&($$->imports), $2, xp_next);
}
sub got_AssignedIdentifier { $$.oid = $1; }
sub got_ImportsBundle {
   $$ = $1;
   $$->fromModuleName = $3;
   $$->identifier = $4;
   /* This stupid thing is used for look-back hack. */
   saved_aid = $$->identifier.oid ? 0 : &($$->identifier);
   checkmem($$);
}
sub got_ImportsList {
   $$ = $1;
   TQ_ADD(&($$->members), $3, next);
}
sub got_ImportsElement_1 {
   $$ = NEW_EXPR();
   checkmem($$);
   $$->Identifier = $1;
   $$->expr_type = A1TC_REFERENCE;
}
sub got_ImportsElement_2 {      /* Completely equivalent to above */
   $$ = NEW_EXPR();
   checkmem($$);
   $$->Identifier = $1;
   $$->expr_type = A1TC_REFERENCE;
}
sub got_ImportsElement_3 {
   $$ = NEW_EXPR();
   checkmem($$);
   $$->Identifier = $1;
   $$->expr_type = A1TC_REFERENCE;
}
sub got_optExports {
   $$ = asn1p_module_new();
   checkmem($$);
   if($1) {
      TQ_ADD(&($$->exports), $1, xp_next);
   } else {
      /* "EXPORTS ALL;" */
   }
}
sub got_ExportsDefinition_1 {
   $$ = $2;
}
sub got_ExportsDefinition_2 {
   $$ = 0;
}
sub got_ExportsDefinition_3 {
   /* Empty EXPORTS clause effectively prohibits export. */
   $$ = asn1p_xports_new();
   checkmem($$);
}
sub got_ExportsBody {
   $$ = $1;
   TQ_ADD(&($$->members), $3, next);
}
sub got_ExportsElement_1 {
   $$ = NEW_EXPR();
   checkmem($$);
   $$->Identifier = $1;
   $$->expr_type = A1TC_EXPORTVAR;
}
sub got_ExportsElement_2 {
   $$ = NEW_EXPR();
   checkmem($$);
   $$->Identifier = $1;
   $$->expr_type = A1TC_EXPORTVAR;
}
sub got_ExportsElement_3 {
   $$ = NEW_EXPR();
   checkmem($$);
   $$->Identifier = $1;
   $$->expr_type = A1TC_EXPORTVAR;
}
sub got_ValueSet { $$ = $2; }
sub got_ValueSetTypeAssignment {
   $$ = $2;
   assert($$->Identifier == 0);
   $$->Identifier = $1;
   $$->meta_type = AMT_VALUESET;
   $$->constraints = $4;
}
# A DefinedType reference.
# "CLASS1.&id.&id2"
# or
# "Module.Type"
# or
# "Module.identifier"
# or
# "Type"
sub got_DefinedType_1 {
   $$ = $1;
}
# A parameterized assignment.
sub got_DefinedType_2 {
   $$ = NEW_EXPR();
   checkmem($$);
   $$->reference = $1;
   $$->expr_type = A1TC_REFERENCE;
   $$->meta_type = AMT_TYPEREF;
}
sub got_DefinedType_3 {
   $$ = NEW_EXPR();
   checkmem($$);
   $$->reference = $1;
   $$->rhs_pspecs = $3;
   $$->expr_type = A1TC_REFERENCE;
   $$->meta_type = AMT_TYPEREF;
}
sub got_DataTypeReference_1 {
   $$ = $3;
   $$->Identifier = $1;
   assert($$->expr_type);
   assert($$->meta_type);
}
# Parameterized <Type> declaration:
# === EXAMPLE ===
#   SIGNED { ToBeSigned } ::= SEQUENCE {
#      toBeSigned  ToBeSigned,
#      algorithm   AlgorithmIdentifier,
#      signature   BIT STRING
#   }
# === EOF ===
sub got_DataTypeReference_2 {
   $$ = $3;
   $$->Identifier = $1;
   assert($$->expr_type == A1TC_CLASSDEF);
   assert($$->meta_type == AMT_OBJECTCLASS);
}
# Parameterized CLASS declaration
sub got_DataTypeReference_3 {
   $$ = $6;
   $$->Identifier = $1;
   $$->lhs_params = $3;
}
sub got_DataTypeReference_4 {
   $$ = $6;
   $$->Identifier = $1;
   $$->lhs_params = $3;
}
sub got_ParameterArgumentList {
   int ret;
   $$ = $1;
   ret = asn1p_paramlist_add_param($$, $3.governor, $3.argument);
   checkmem(ret == 0);
   if($3.governor) asn1p_ref_free($3.governor);
   if($3.argument) free($3.argument);
}
sub got_ParameterArgumentName_1 {
   $$.governor = NULL;
   $$.argument = $1;
}
sub got_ParameterArgumentName_2 {
   int ret;
   $$.governor = asn1p_ref_new(yylineno);
   ret = asn1p_ref_add_component($$.governor, $1, 0);
   checkmem(ret == 0);
   $$.argument = $3;
}
sub got_ParameterArgumentName_3 {
   int ret;
   $$.governor = asn1p_ref_new(yylineno);
   ret = asn1p_ref_add_component($$.governor, $1, 0);
   checkmem(ret == 0);
   $$.argument = $3;
}
sub got_ParameterArgumentName_4 {
   int ret;
   $$.governor = asn1p_ref_new(yylineno);
   ret = asn1p_ref_add_component($$.governor,
      ASN_EXPR_TYPE2STR($1), 1);
   checkmem(ret == 0);
   $$.argument = $3;
}
sub got_ParameterArgumentName_5 {
   int ret;
   $$.governor = asn1p_ref_new(yylineno);
   ret = asn1p_ref_add_component($$.governor,
      ASN_EXPR_TYPE2STR($1), 1);
   checkmem(ret == 0);
   $$.argument = $3;
}
sub got_ActualParameterList {
   $$ = $1;
   asn1p_expr_add($$, $3);
}
sub got_ActualParameter_1 {
   $$ = $1;
}
sub got_ActualParameter_2 {
   $$ = NEW_EXPR();
   checkmem($$);
   $$->Identifier = "?";
   $$->expr_type = A1TC_REFERENCE;
   $$->meta_type = AMT_VALUE;
   $$->value = $1;
}
sub got_ActualParameter_3 {
   asn1p_ref_t *ref;
   $$ = NEW_EXPR();
   checkmem($$);
   $$->Identifier = $1;
   $$->expr_type = A1TC_REFERENCE;
   $$->meta_type = AMT_VALUE;
   ref = asn1p_ref_new(yylineno);
   asn1p_ref_add_component(ref, $1, RLT_lowercase);
   $$->value = asn1p_value_fromref(ref, 0);
}
sub got_ActualParameter_4 {
   $$ = NEW_EXPR();
   $$->expr_type = A1TC_VALUESET;
   $$->meta_type = AMT_VALUESET;
   $$->constraints = $1;
}
sub got_optComponentTypeLists { $$ = $1; }
sub got_ComponentTypeLists {
   $$ = $1;
   asn1p_expr_add($$, $3);
}
sub got_ComponentType_1 {
   $$ = $2;
   assert($$->Identifier == 0);
   $$->Identifier = $1;
   $3.flags |= $$->marker.flags;
   $$->marker = $3;
}
sub got_ComponentType_2 {
   $$ = $1;
   $2.flags |= $$->marker.flags;
   $$->marker = $2;
   _fixup_anonymous_identifier($$);
}
sub got_ComponentType_3 {
   $$ = NEW_EXPR();
   checkmem($$);
   $$->meta_type = $3->meta_type;
   $$->expr_type = A1TC_COMPONENTS_OF;
   asn1p_expr_add($$, $3);
}
sub got_ComponentType_4 {
   $$ = $1;
}
sub got_AlternativeTypeLists {
   $$ = $1;
   asn1p_expr_add($$, $3);
}
sub got_AlternativeType_1 {
   $$ = $2;
   assert($$->Identifier == 0);
   $$->Identifier = $1;
}
sub got_AlternativeType_2 {
   $$ = $1;
}
sub got_AlternativeType_3 {
   $$ = $1;
   _fixup_anonymous_identifier($$);
}
sub got_ObjectClass {
   $$ = $3;
   checkmem($$);
   $$->with_syntax = $5;
   assert($$->expr_type == A1TC_CLASSDEF);
   assert($$->meta_type == AMT_OBJECTCLASS);
}
sub got_optUnique { $$ = 1; }
sub got_FieldSpec {
   $$ = $1;
   asn1p_expr_add($$, $3);
}
# FixedTypeValueFieldSpec ::= valuefieldreference Type UNIQUE ? ValueOptionalitySpec ?
sub got_ClassField_1 {
   $$ = NEW_EXPR();
   checkmem($$);
   $$->Identifier = $1;
   $$->meta_type = AMT_OBJECTFIELD;
   $$->expr_type = A1TC_CLASSFIELD_TFS;   /* TypeFieldSpec */
   $$->marker = $2;
}
# VariableTypeValueFieldSpec ::= valuefieldreference FieldName ValueOptionalitySpec ?
sub got_ClassField_2 {
   $$ = NEW_EXPR();
   $$->Identifier = $1;
   $$->meta_type = AMT_OBJECTFIELD;
   $$->expr_type = A1TC_CLASSFIELD_FTVFS;   /* FixedTypeValueFieldSpec */
   $$->unique = $3;
   $$->marker = $4;
   asn1p_expr_add($$, $2);
}
#  ObjectFieldSpec ::= objectfieldreference DefinedObjectClass ObjectOptionalitySpec ?
sub got_ClassField_3 {
   $$ = NEW_EXPR();
   $$->Identifier = $1;
   $$->meta_type = AMT_OBJECTFIELD;
   $$->expr_type = A1TC_CLASSFIELD_VTVFS;
   $$->reference = $2;
   $$->marker = $3;
}
# VariableTypeValueSetFieldSpec ::= valuesetfieldreference FieldName ValueOptionalitySpec ?
sub got_ClassField_4 {
   $$ = NEW_EXPR();
   checkmem($$);
   $$->Identifier = $1;
   $$->reference = $2;
   $$->meta_type = AMT_OBJECTFIELD;
   $$->expr_type = A1TC_CLASSFIELD_OFS;
   $$->marker = $3;
}
# FixedTypeValueSetFieldSpec ::= valuesetfieldreference Type ValueSetOptionalitySpec ?
sub got_ClassField_5 {
   $$ = NEW_EXPR();
   $$->Identifier = $1;
   $$->meta_type = AMT_OBJECTFIELD;
   $$->expr_type = A1TC_CLASSFIELD_VTVSFS;
   $$->reference = $2;
   $$->marker = $3;
}
#  ObjectSetFieldSpec ::= objectsetfieldreference DefinedObjectClass ObjectOptionalitySpec ?
sub got_ClassField_6 {
   $$ = NEW_EXPR();
   checkmem($$);
   $$->Identifier = $1;
   $$->meta_type = AMT_OBJECTFIELD;
   $$->expr_type = A1TC_CLASSFIELD_FTVSFS;
   asn1p_expr_add($$, $2);
   $$->marker = $3;
}
sub got_ClassField_7 {
   $$ = NEW_EXPR();
   checkmem($$);
   $$->Identifier = $1;
   $$->reference = $2;
   $$->meta_type = AMT_OBJECTFIELD;
   $$->expr_type = A1TC_CLASSFIELD_OSFS;
   $$->marker = $3;
}
sub got_optWithSyntax {
   $$ = $1;
}
sub got_WithSyntax {
   $$ = $5;
}
sub got_WithSyntaxList {
   $$ = $1;
   TQ_ADD(&($$->chunks), $2, next);
}
sub got_WithSyntaxToken_1 {
   $$ = asn1p_wsyntx_chunk_fromstring($1.buf, 0);
   $$->type = WC_WHITESPACE;
}
sub got_WithSyntaxToken_2 {
   $$ = asn1p_wsyntx_chunk_fromstring($1, 0);
}
sub got_WithSyntaxToken_3 {
   $$ = asn1p_wsyntx_chunk_fromstring($1.name, 0);
   $$->type = WC_FIELD;
}
sub got_WithSyntaxToken_4 {
   $$ = asn1p_wsyntx_chunk_fromsyntax($2);
}
sub got_ExtensionAndException_1 {
   $$ = NEW_EXPR();
   checkmem($$);
   $$->Identifier = strdup("...");
   checkmem($$->Identifier);
   $$->expr_type = A1TC_EXTENSIBLE;
   $$->meta_type = AMT_TYPE;
}
sub got_ExtensionAndException_2 {
   $$ = NEW_EXPR();
   checkmem($$);
   $$->Identifier = strdup("...");
   checkmem($$->Identifier);
   $$->value = $3;
   $$->expr_type = A1TC_EXTENSIBLE;
   $$->meta_type = AMT_TYPE;
}
sub got_ExtensionAndException_3 {
   $$ = NEW_EXPR();
   checkmem($$);
   $$->Identifier = strdup("...");
   $$->value = $3;
   checkmem($$->Identifier);
   $$->expr_type = A1TC_EXTENSIBLE;
   $$->meta_type = AMT_TYPE;
}
sub got_Type {
   $$ = $2;
   $$->tag = $1;
   /*
    * Outer constraint for SEQUENCE OF and SET OF applies
    * to the inner type.
    */
   if($$->expr_type == ASN_CONSTR_SEQUENCE_OF
   || $$->expr_type == ASN_CONSTR_SET_OF) {
      assert(!TQ_FIRST(&($$->members))->constraints);
      TQ_FIRST(&($$->members))->constraints = $3;
   } else {
      if($$->constraints) {
         assert(!$2);
      } else {
         $$->constraints = $3;
      }
   }
}
sub got_TypeDeclaration {
   $$ = $2;
   $$->marker.flags |= $1;

   if(($$->marker.flags & EM_INDIRECT)
   && ($$->marker.flags & EM_OPTIONAL) != EM_OPTIONAL) {
      fprintf(stderr,
         "INFO: Directive <ASN1C:RepresentAsPointer> "
         "applied to %s at line %d\n",
         ASN_EXPR_TYPE2STR($$->expr_type)
            ?  ASN_EXPR_TYPE2STR($$->expr_type)
            : "member",
         $$->_lineno
      );
   }
}
sub got_TypeDeclarationSet_1 {
   $$ = $1;
}
sub got_TypeDeclarationSet_2 {
   $$ = $3;
   assert($$->expr_type == A1TC_INVALID);
   $$->expr_type = ASN_CONSTR_CHOICE;
   $$->meta_type = AMT_TYPE;
}
sub got_TypeDeclarationSet_3 {
   $$ = $3;
   assert($$->expr_type == A1TC_INVALID);
   $$->expr_type = ASN_CONSTR_SEQUENCE;
   $$->meta_type = AMT_TYPE;
}
sub got_TypeDeclarationSet_4 {
   $$ = $3;
   assert($$->expr_type == A1TC_INVALID);
   $$->expr_type = ASN_CONSTR_SET;
   $$->meta_type = AMT_TYPE;
}
sub got_TypeDeclarationSet_5 {
   $$ = NEW_EXPR();
   checkmem($$);
   $$->constraints = $2;
   $$->expr_type = ASN_CONSTR_SEQUENCE_OF;
   $$->meta_type = AMT_TYPE;
   $6->Identifier = $4;
   $6->tag = $5;
   asn1p_expr_add($$, $6);
}
sub got_TypeDeclarationSet_6 {
   $$ = NEW_EXPR();
   checkmem($$);
   $$->constraints = $2;
   $$->expr_type = ASN_CONSTR_SET_OF;
   $$->meta_type = AMT_TYPE;
   $6->Identifier = $4;
   $6->tag = $5;
   asn1p_expr_add($$, $6);
}
sub got_TypeDeclarationSet_7 {
   $$ = NEW_EXPR();
   checkmem($$);
   $$->expr_type = ASN_TYPE_ANY;
   $$->meta_type = AMT_TYPE;
}
sub got_TypeDeclarationSet_8 {
   int ret;
   $$ = NEW_EXPR();
   checkmem($$);
   $$->reference = asn1p_ref_new(yylineno);
   ret = asn1p_ref_add_component($$->reference,
      $4, RLT_lowercase);
   checkmem(ret == 0);
   $$->expr_type = ASN_TYPE_ANY;
   $$->meta_type = AMT_TYPE;
}
sub got_TypeDeclarationSet_9 {
   $$ = NEW_EXPR();
   checkmem($$);
   $$->reference = $3;
   $$->expr_type = A1TC_INSTANCE;
   $$->meta_type = AMT_TYPE;
}
sub got_ComplexTypeReference_1 {
   int ret;
   $$ = asn1p_ref_new(yylineno);
   checkmem($$);
   ret = asn1p_ref_add_component($$, $1, RLT_UNKNOWN);
   checkmem(ret == 0);
   free($1);
}
sub got_ComplexTypeReference_2 {
   int ret;
   $$ = asn1p_ref_new(yylineno);
   checkmem($$);
   ret = asn1p_ref_add_component($$, $1, RLT_UNKNOWN);
   checkmem(ret == 0);
   ret = asn1p_ref_add_component($$, $3, RLT_UNKNOWN);
   checkmem(ret == 0);
   free($1);
}
sub got_ComplexTypeReference_3 {
   int ret;
   $$ = asn1p_ref_new(yylineno);
   checkmem($$);
   ret = asn1p_ref_add_component($$, $1, RLT_UNKNOWN);
   checkmem(ret == 0);
   ret = asn1p_ref_add_component($$, $3, RLT_UNKNOWN);
   checkmem(ret == 0);
   free($1);
}
sub got_ComplexTypeReference_4 {
   int ret;
   $$ = asn1p_ref_new(yylineno);
   checkmem($$);
   ret = asn1p_ref_add_component($$, $1, RLT_UNKNOWN);
   checkmem(ret == 0);
   ret = asn1p_ref_add_component($$, $3, RLT_lowercase);
   checkmem(ret == 0);
   free($1);
}
sub got_ComplexTypeReference_5 {
   int ret;
   $$ = asn1p_ref_new(yylineno);
   checkmem($$);
   ret = asn1p_ref_add_component($$, $1, RLT_CAPITALS);
   free($1);
   checkmem(ret == 0);
}
sub got_ComplexTypeReference_6 {
   int ret;
   $$ = $3;
   ret = asn1p_ref_add_component($$, $1, RLT_CAPITALS);
   free($1);
   checkmem(ret == 0);
   /*
    * Move the last element infront.
    */
   {
      struct asn1p_ref_component_s tmp_comp;
      tmp_comp = $$->components[$$->comp_count-1];
      memmove(&$$->components[1],
         &$$->components[0],
         sizeof($$->components[0])
         * ($$->comp_count - 1));
      $$->components[0] = tmp_comp;
   }
}
sub got_ComplexTypeReferenceAmpList {
   int ret;
   $$ = $1;
   ret = asn1p_ref_add_component($$, $3.name, $3.lex_type);
   free($3.name);
   checkmem(ret == 0);
}
# "&id"
sub got_PrimitiveFieldReference_1 {
   $$.lex_type = RLT_AmpUppercase;
   $$.name = $1;
}
sub got_PrimitiveFieldReference_2 {
   $$.lex_type = RLT_Amplowercase;
   $$.name = $1;
}
sub got_FieldName_1 {
   $$ = asn1p_ref_new(yylineno);
   asn1p_ref_add_component($$, $1, RLT_AmpUppercase);
}
sub got_FieldName_2 {
   $$ = $$;
   asn1p_ref_add_component($$, $3, RLT_AmpUppercase);
}
sub got_FieldName_3 {
   $$ = $$;
   asn1p_ref_add_component($$, $3, RLT_Amplowercase);
}
#   | TypeRefName '.' TOK_capitalreference {
#      $$ = asn1p_ref_new(yylineno);
#      asn1p_ref_add_component($$, $1, RLT_AmpUppercase);
#      asn1p_ref_add_component($$, $3, RLT_CAPITALS);
#   }
sub got_DefinedObjectClass {
   $$ = asn1p_ref_new(yylineno);
   asn1p_ref_add_component($$, $1, RLT_CAPITALS);
}
sub got_ValueAssignment {
   $$ = $2;
   assert($$->Identifier == NULL);
   $$->Identifier = $1;
   $$->meta_type = AMT_VALUE;
   $$->value = $4;
}
sub got_Value_1 {
   $$ = asn1p_value_fromint(0);
   checkmem($$);
   $$->type = ATV_CHOICE_IDENTIFIER;
   $$->value.choice_identifier.identifier = $1;
   $$->value.choice_identifier.value = $3;
}
sub got_Value_2 {
   $$ = asn1p_value_frombuf($3.buf, $3.len, 0);
   checkmem($$);
   $$->type = ATV_UNPARSED;
}
sub got_Value_3 {
   $$ = asn1p_value_fromint(0);
   checkmem($$);
   $$->type = ATV_NULL;
}
sub got_SimpleValue_1 {
   $$ = asn1p_value_fromint(0);
   checkmem($$);
   $$->type = ATV_FALSE;
}
sub got_SimpleValue_2 {
   $$ = asn1p_value_fromint(0);
   checkmem($$);
   $$->type = ATV_TRUE;
}
sub got_SimpleValue_3 {
   $$ = _convert_bitstring2binary($1, 'B');
   checkmem($$);
}
sub got_SimpleValue_4 {
   $$ = _convert_bitstring2binary($1, 'H');
   checkmem($$);
}
sub got_SimpleValue_5 {
   $$ = $$;
}
sub got_SimpleValue_6 {
   $$ = $1;
}
sub got_DefinedValue_1 {
   asn1p_ref_t *ref;
   int ret;
   ref = asn1p_ref_new(yylineno);
   checkmem(ref);
   ret = asn1p_ref_add_component(ref, $1, RLT_lowercase);
   checkmem(ret == 0);
   $$ = asn1p_value_fromref(ref, 0);
   checkmem($$);
   free($1);
}
sub got_DefinedValue_2 {
   asn1p_ref_t *ref;
   int ret;
   ref = asn1p_ref_new(yylineno);
   checkmem(ref);
   ret = asn1p_ref_add_component(ref, $1, RLT_UNKNOWN);
   checkmem(ret == 0);
   ret = asn1p_ref_add_component(ref, $3, RLT_lowercase);
   checkmem(ret == 0);
   $$ = asn1p_value_fromref(ref, 0);
   checkmem($$);
   free($1);
   free($3);
}
sub got_RestrictedCharacterStringValue_1 {
   $$ = asn1p_value_frombuf($1.buf, $1.len, 0);
   checkmem($$);
}
sub got_RestrictedCharacterStringValue_2 {
   $$ = asn1p_value_fromint($1);
   checkmem($$);
   $$->type = ATV_TUPLE;
}
sub got_RestrictedCharacterStringValue_3 {
   $$ = asn1p_value_fromint($1);
   checkmem($$);
   $$->type = ATV_QUADRUPLE;
}
sub got_Opaque {
   int newsize = $1.len + $2.len;
   char *p = malloc(newsize + 1);
   checkmem(p);
   memcpy(p         , $1.buf, $1.len);
   memcpy(p + $1.len, $2.buf, $2.len);
   p[newsize] = '\0';
   free($1.buf);
   free($2.buf);
   $$.buf = p;
   $$.len = newsize;
}
sub got_BasicTypeId_1  { $$ = ASN_BASIC_BOOLEAN; }
sub got_BasicTypeId_2  { $$ = ASN_BASIC_NULL; }
sub got_BasicTypeId_3  { $$ = ASN_BASIC_REAL; }
sub got_BasicTypeId_4  { $$ = $1; }
sub got_BasicTypeId_5  { $$ = ASN_BASIC_OCTET_STRING; }
sub got_BasicTypeId_6  { $$ = ASN_BASIC_OBJECT_IDENTIFIER; }
sub got_BasicTypeId_7  { $$ = ASN_BASIC_RELATIVE_OID; }
sub got_BasicTypeId_8  { $$ = ASN_BASIC_EXTERNAL; }
sub got_BasicTypeId_9  { $$ = ASN_BASIC_EMBEDDED_PDV; }
sub got_BasicTypeId_10 { $$ = ASN_BASIC_CHARACTER_STRING; }
sub got_BasicTypeId_11 { $$ = ASN_BASIC_UTCTime; }
sub got_BasicTypeId_12 { $$ = ASN_BASIC_GeneralizedTime; }
sub got_BasicTypeId_13 { $$ = $1; }
sub got_BasicTypeId_UniverationCompatible_1 { $$ = ASN_BASIC_INTEGER; }
sub got_BasicTypeId_UniverationCompatible_2 { $$ = ASN_BASIC_ENUMERATED; }
sub got_BasicTypeId_UniverationCompatible_3 { $$ = ASN_BASIC_BIT_STRING; }
sub got_BasicType_1 {
   $$ = NEW_EXPR();
   checkmem($$);
   $$->expr_type = $1;
   $$->meta_type = AMT_TYPE;
}
sub got_BasicType_2 {
   if($2) {
      $$ = $2;
   } else {
      $$ = NEW_EXPR();
      checkmem($$);
   }
   $$->expr_type = $1;
   $$->meta_type = AMT_TYPE;
}
sub got_BasicString_1  { $$ = ASN_STRING_BMPString; }
sub got_BasicString_2  {
   $$ = ASN_STRING_GeneralString;
   fprintf(stderr, "WARNING: GeneralString is not fully supported\n");
}
sub got_BasicString_3  {
   $$ = ASN_STRING_GraphicString;
   fprintf(stderr, "WARNING: GraphicString is not fully supported\n");
}
sub got_BasicString_4  { $$ = ASN_STRING_IA5String; }
sub got_BasicString_5  { $$ = ASN_STRING_ISO646String; }
sub got_BasicString_6  { $$ = ASN_STRING_NumericString; }
sub got_BasicString_7  { $$ = ASN_STRING_PrintableString; }
sub got_BasicString_8  {
   $$ = ASN_STRING_T61String;
   fprintf(stderr, "WARNING: T61String is not fully supported\n");
}
sub got_BasicString_9  { $$ = ASN_STRING_TeletexString; }
sub got_BasicString_10 { $$ = ASN_STRING_UniversalString; }
sub got_BasicString_11 { $$ = ASN_STRING_UTF8String; }
sub got_BasicString_12 {
   $$ = ASN_STRING_VideotexString;
   fprintf(stderr, "WARNING: VideotexString is not fully supported\n");
}
sub got_BasicString_13 { $$ = ASN_STRING_VisibleString; }
sub got_BasicString_14 { $$ = ASN_STRING_ObjectDescriptor; }
sub got_optConstraints {
   $$ = $1;
}
sub got_SubtypeConstraint_1 {
   CONSTRAINT_INSERT($$, ACT_CA_SET, $1, 0);
}
sub got_SubtypeConstraint_2 {
   /*
    * This is a special case, for compatibility purposes.
    * It goes without parentheses.
    */
   CONSTRAINT_INSERT($$, ACT_CT_SIZE, $3, 0);
}
sub got_SetOfConstraints_1 {
   $$ = $2;
}
sub got_SetOfConstraints_2 {
   CONSTRAINT_INSERT($$, ACT_CA_SET, $1, $3);
}
sub got_ElementSetSpecs_1 {
   $$ = asn1p_constraint_new(yylineno);
   $$->type = ACT_EL_EXT;
}
sub got_ElementSetSpecs_2 {
   $$ = $1;
}
sub got_ElementSetSpecs_3 {
   asn1p_constraint_t *ct;
   ct = asn1p_constraint_new(yylineno);
   ct->type = ACT_EL_EXT;
   CONSTRAINT_INSERT($$, ACT_CA_CSV, $1, ct);
}
sub got_ElementSetSpecs_4 {
   asn1p_constraint_t *ct;
   ct = asn1p_constraint_new(yylineno);
   ct->type = ACT_EL_EXT;
   CONSTRAINT_INSERT($$, ACT_CA_CSV, $1, ct);
   ct = $$;
   CONSTRAINT_INSERT($$, ACT_CA_CSV, ct, $5);
}
sub got_ElementSetSpecs_5 {
   $$ = $1;
}
sub got_ElementSetSpec_1 {
   CONSTRAINT_INSERT($$, ACT_CA_AEX, $3, 0);
}
sub got_Unions {
   CONSTRAINT_INSERT($$, ACT_CA_UNI, $1, $3);
}
sub got_Intersections {
   CONSTRAINT_INSERT($$, ACT_CA_INT, $1, $3);
}
sub got_IntersectionElements_1 {
   CONSTRAINT_INSERT($$, ACT_CA_EXC, $1, $3);
}
sub got_ConstraintSubtypeElement_1  {
   int ret;
   $$ = asn1p_constraint_new(yylineno);
   checkmem($$);
   $$->type = $1;
   ret = asn1p_constraint_insert($$, $3);
   checkmem(ret == 0);
}
sub got_ConstraintSubtypeElement_2  {
   int ret;
   $$ = asn1p_constraint_new(yylineno);
   checkmem($$);
   $$->type = ACT_CA_SET;
   ret = asn1p_constraint_insert($$, $2);
   checkmem(ret == 0);
}
sub got_ConstraintSubtypeElement_3  {
   $$ = asn1p_constraint_new(yylineno);
   checkmem($$);
   $$->type = ACT_EL_VALUE;
   $$->value = $1;
}
sub got_ConstraintSubtypeElement_4  {
   $$ = asn1p_constraint_new(yylineno);
   checkmem($$);
   $$->type = ACT_EL_TYPE;
   $$->containedSubtype = $1;
}
sub got_ConstraintSubtypeElement_5  {
   $$ = asn1p_constraint_new(yylineno);
   checkmem($$);
   $$->type = $2;
   $$->range_start = $1;
   $$->range_stop = $3;
}
sub got_ConstraintSubtypeElement_6  {
   $$ = asn1p_constraint_new(yylineno);
   checkmem($$);
   $$->type = $2;
   $$->range_start = asn1p_value_fromint(-123);
   $$->range_stop = $3;
   $$->range_start->type = ATV_MIN;
}
sub got_ConstraintSubtypeElement_7  {
   $$ = asn1p_constraint_new(yylineno);
   checkmem($$);
   $$->type = $2;
   $$->range_start = $1;
   $$->range_stop = asn1p_value_fromint(321);
   $$->range_stop->type = ATV_MAX;
}
sub got_ConstraintSubtypeElement_8  {
   $$ = asn1p_constraint_new(yylineno);
   checkmem($$);
   $$->type = $2;
   $$->range_start = asn1p_value_fromint(-123);
   $$->range_stop = asn1p_value_fromint(321);
   $$->range_start->type = ATV_MIN;
   $$->range_stop->type = ATV_MAX;
}
sub got_ConstraintSubtypeElement_9  {
   $$ = $1;
}
sub got_ConstraintSubtypeElement_10 {
   $$ = $1;
}
sub got_PatternConstraint_1 {
   $$ = asn1p_constraint_new(yylineno);
   $$->type = ACT_CT_PATTERN;
   $$->value = asn1p_value_frombuf($2.buf, $2.len, 0);
}
sub got_PatternConstraint_2 {
   asn1p_ref_t *ref;
   $$ = asn1p_constraint_new(yylineno);
   $$->type = ACT_CT_PATTERN;
   ref = asn1p_ref_new(yylineno);
   asn1p_ref_add_component(ref, $2, RLT_lowercase);
   $$->value = asn1p_value_fromref(ref, 0);
}
sub got_ConstraintSpec_1 {
   $$ = ACT_CT_SIZE;
}
sub got_ConstraintSpec_2 {
   $$ = ACT_CT_FROM;
}
sub got_SingleValue_1 {
   $$ = asn1p_value_fromint(0);
   checkmem($$);
   $$->type = ATV_FALSE;
}
sub got_SingleValue_2 {
   $$ = asn1p_value_fromint(1);
   checkmem($$);
   $$->type = ATV_TRUE;
}
sub got_SingleValue_3 {
   asn1p_ref_t *ref;
   int ret;
   ref = asn1p_ref_new(yylineno);
   checkmem(ref);
   ret = asn1p_ref_add_component(ref, $1, RLT_lowercase);
   checkmem(ret == 0);
   $$ = asn1p_value_fromref(ref, 0);
   checkmem($$);
   free($1);
}
sub got_BitStringValue_1 {
   $$ = _convert_bitstring2binary($1, 'B');
   checkmem($$);
}
sub got_BitStringValue_2 {
   $$ = _convert_bitstring2binary($1, 'H');
   checkmem($$);
}
sub got_ContainedSubtype {
   asn1p_ref_t *ref;
   int ret;
   ref = asn1p_ref_new(yylineno);
   checkmem(ref);
   ret = asn1p_ref_add_component(ref, $1, RLT_UNKNOWN);
   checkmem(ret == 0);
   $$ = asn1p_value_fromref(ref, 0);
   checkmem($$);
   free($1);
}
sub got_InnerTypeConstraint_1 {
   CONSTRAINT_INSERT($$, ACT_CT_WCOMP, $3, 0);
}
sub got_InnerTypeConstraint_2 {
   CONSTRAINT_INSERT($$, ACT_CT_WCOMPS, $4, 0);
}
sub got_WithComponentsList {
   CONSTRAINT_INSERT($$, ACT_CT_WCOMPS, $1, $3);
}
sub got_WithComponentsElement_1 {
   $$ = asn1p_constraint_new(yylineno);
   checkmem($$);
   $$->type = ACT_EL_EXT;
   $$->value = asn1p_value_frombuf("...", 3, 1);
}
sub got_WithComponentsElement_2 {
   $$ = asn1p_constraint_new(yylineno);
   checkmem($$);
   $$->type = ACT_EL_VALUE;
   $$->value = asn1p_value_frombuf($1, strlen($1), 0);
   $$->presence = $3;
   if($2) asn1p_constraint_insert($$, $2);
}
sub got_optPresenceConstraint { $$ = $1; }
sub got_PresenceConstraint_1 {
   $$ = ACPRES_PRESENT;
}
sub got_PresenceConstraint_2 {
   $$ = ACPRES_ABSENT;
}
sub got_PresenceConstraint_3 {
   $$ = ACPRES_OPTIONAL;
}
sub got_UserDefinedConstraint {
   $$ = asn1p_constraint_new(yylineno);
   checkmem($$);
   $$->type = ACT_CT_CTDBY;
   $$->value = asn1p_value_frombuf($5.buf, $5.len, 0);
   checkmem($$->value);
   $$->value->type = ATV_UNPARSED;
}
sub got_ContentsConstraint {
   $$ = asn1p_constraint_new(yylineno);
   $$->type = ACT_CT_CTNG;
   $$->value = asn1p_value_fromtype($2);
}
sub got_ConstraintRangeSpec_1 { $$ = ACT_EL_RANGE; }
sub got_ConstraintRangeSpec_2 { $$ = ACT_EL_RLRANGE; }
sub got_ConstraintRangeSpec_3 { $$ = ACT_EL_LLRANGE; }
sub got_ConstraintRangeSpec_4 { $$ = ACT_EL_ULRANGE; }
sub got_TableConstraint_1 {
   $$ = $1;
}
sub got_TableConstraint_2 {
   $$ = $1;
}
sub got_SimpleTableConstraint {
   asn1p_ref_t *ref = asn1p_ref_new(yylineno);
   asn1p_constraint_t *ct;
   int ret;
   ret = asn1p_ref_add_component(ref, $2, 0);
   checkmem(ret == 0);
   ct = asn1p_constraint_new(yylineno);
   checkmem($$);
   ct->type = ACT_EL_VALUE;
   ct->value = asn1p_value_fromref(ref, 0);
   CONSTRAINT_INSERT($$, ACT_CA_CRC, ct, 0);
}
sub got_ComponentRelationConstraint {
   CONSTRAINT_INSERT($$, ACT_CA_CRC, $1, $3);
}
sub got_AtNotationList {
   asn1p_constraint_t *ct;
   ct = asn1p_constraint_new(yylineno);
   checkmem(ct);
   ct->type = ACT_EL_VALUE;
   ct->value = asn1p_value_fromref($3, 0);
   CONSTRAINT_INSERT($$, ACT_CA_CSV, $1, ct);
}
sub got_AtNotationElement_1 {
   char *p = malloc(strlen($2) + 2);
   int ret;
   *p = '@';
   strcpy(p + 1, $2);
   $$ = asn1p_ref_new(yylineno);
   ret = asn1p_ref_add_component($$, p, 0);
   checkmem(ret == 0);
   free(p);
   free($2);
}
sub got_AtNotationElement_2 {
   char *p = malloc(strlen($3) + 3);
   int ret;
   p[0] = '@';
   p[1] = '.';
   strcpy(p + 2, $3);
   $$ = asn1p_ref_new(yylineno);
   ret = asn1p_ref_add_component($$, p, 0);
   checkmem(ret == 0);
   free(p);
   free($3);
}
sub got_ComponentIdList {
   int l1 = strlen($1);
   int l3 = strlen($3);
   $$ = malloc(l1 + 1 + l3 + 1);
   memcpy($$, $1, l1);
   $$[l1] = '.';
   memcpy($$ + l1 + 1, $3, l3);
   $$[l1 + 1 + l3] = '\0';
}
sub got_optMarker { $$ = $1; }
sub got_Marker_1 {
   $$.flags = EM_OPTIONAL | EM_INDIRECT;
   $$.default_value = 0;
}
sub got_Marker_2 {
   $$.flags = EM_DEFAULT;
   $$.default_value = $2;
}
sub got_UniverationDefinition_1 {
   $$ = NEW_EXPR();
   checkmem($$);
}
sub got_UniverationDefinition_2 {
   $$ = $2;
}
sub got_UniverationList {
   $$ = $1;
   asn1p_expr_add($$, $3);
}
sub got_UniverationElement_1 {
   $$ = NEW_EXPR();
   checkmem($$);
   $$->expr_type = A1TC_UNIVERVAL;
   $$->meta_type = AMT_VALUE;
   $$->Identifier = $1;
}
sub got_UniverationElement_2 {
   $$ = NEW_EXPR();
   checkmem($$);
   $$->expr_type = A1TC_UNIVERVAL;
   $$->meta_type = AMT_VALUE;
   $$->Identifier = $1;
   $$->value = $3;
}
sub got_UniverationElement_3 {
   $$ = NEW_EXPR();
   checkmem($$);
   $$->expr_type = A1TC_UNIVERVAL;
   $$->meta_type = AMT_VALUE;
   $$->Identifier = $1;
   $$->value = $3;
}
sub got_UniverationElement_4 {
   $$ = NEW_EXPR();
   checkmem($$);
   $$->expr_type = A1TC_UNIVERVAL;
   $$->meta_type = AMT_VALUE;
   $$->value = $1;
}
sub got_UniverationElement_5 {
   $$ = NEW_EXPR();
   checkmem($$);
   $$->Identifier = strdup("...");
   checkmem($$->Identifier);
   $$->expr_type = A1TC_EXTENSIBLE;
   $$->meta_type = AMT_VALUE;
}
sub got_SignedNumber_1 {
   $$ = asn1p_value_fromint($1);
   checkmem($$);
}
sub got_SignedNumber_2 {
   $$ = asn1p_value_fromint($1);
   checkmem($$);
}
sub got_RealValue_1 {
   $$ = asn1p_value_fromdouble($1);
   checkmem($$);
}
sub got_optTag { $$ = $1; }
sub got_Tag {
   $$ = $1;
   $$.tag_mode = $2.tag_mode;
}
sub got_TagTypeValue {
   $$ = $2;
   $$.tag_value = $3;
}
sub got_TagClass_1 { $$.tag_class = TC_UNIVERSAL; }
sub got_TagClass_2 { $$.tag_class = TC_APPLICATION; }
sub got_TagClass_3 { $$.tag_class = TC_PRIVATE; }
sub got_TagPlicit_1 { $$.tag_mode = TM_IMPLICIT; }
sub got_TagPlicit_2 { $$.tag_mode = TM_EXPLICIT; }
sub got_TypeRefName_1 {
   checkmem($1);
   $$ = $1;
}
sub got_TypeRefName_2 {
   checkmem($1);
   $$ = $1;
}
sub got_ObjectClassReference {
   checkmem($1);
   $$ = $1;
}
sub got_optIdentifier {
   $$ = $1;
}
sub got_Identifier {
   checkmem($1);
   $$ = $1;
}
## %% ##

use BLAH;

