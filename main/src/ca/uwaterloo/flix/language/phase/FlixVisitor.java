// Generated from Flix.g4 by ANTLR 4.5.3

package ca.uwaterloo.flix.language.phase;

import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link FlixParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface FlixVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link FlixParser#tscomment}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTscomment(FlixParser.TscommentContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#ws}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitWs(FlixParser.WsContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#start}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStart(FlixParser.StartContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#optSC}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOptSC(FlixParser.OptSCContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#ident}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIdent(FlixParser.IdentContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#nname}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNname(FlixParser.NnameContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#lowerqname}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLowerqname(FlixParser.LowerqnameContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#upperqname}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUpperqname(FlixParser.UpperqnameContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#annotationName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAnnotationName(FlixParser.AnnotationNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#attributeName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAttributeName(FlixParser.AttributeNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#className}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitClassName(FlixParser.ClassNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#definitionName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDefinitionName(FlixParser.DefinitionNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#qualifiedDefinitionName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitQualifiedDefinitionName(FlixParser.QualifiedDefinitionNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#tableName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTableName(FlixParser.TableNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#qualifiedTableName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitQualifiedTableName(FlixParser.QualifiedTableNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#tagName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTagName(FlixParser.TagNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#typeName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypeName(FlixParser.TypeNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#qualifiedTypeName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitQualifiedTypeName(FlixParser.QualifiedTypeNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#variableName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitVariableName(FlixParser.VariableNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#variableNames}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitVariableNames(FlixParser.VariableNamesContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#argument}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArgument(FlixParser.ArgumentContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#arguments}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArguments(FlixParser.ArgumentsContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#formalparams}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFormalparams(FlixParser.FormalparamsContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#attribute}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAttribute(FlixParser.AttributeContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#attributes}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAttributes(FlixParser.AttributesContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#index}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIndex(FlixParser.IndexContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#indexes}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIndexes(FlixParser.IndexesContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#match_rule}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMatch_rule(FlixParser.Match_ruleContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#match_rules}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMatch_rules(FlixParser.Match_rulesContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#switch_rule}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSwitch_rule(FlixParser.Switch_ruleContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#switch_rules}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSwitch_rules(FlixParser.Switch_rulesContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#typeparam}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypeparam(FlixParser.TypeparamContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#typeparams}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypeparams(FlixParser.TypeparamsContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#class_typeparams}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitClass_typeparams(FlixParser.Class_typeparamsContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#contextBound}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitContextBound(FlixParser.ContextBoundContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#contextBounds}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitContextBounds(FlixParser.ContextBoundsContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#contextBoundsList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitContextBoundsList(FlixParser.ContextBoundsListContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#implContextBoundsList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitImplContextBoundsList(FlixParser.ImplContextBoundsListContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#annotation}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAnnotation(FlixParser.AnnotationContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#annotations}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAnnotations(FlixParser.AnnotationsContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#s_import}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitS_import(FlixParser.S_importContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#import_wildcard}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitImport_wildcard(FlixParser.Import_wildcardContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#import_definition}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitImport_definition(FlixParser.Import_definitionContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#import_namespace}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitImport_namespace(FlixParser.Import_namespaceContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#decl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDecl(FlixParser.DeclContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#decls_namespace}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDecls_namespace(FlixParser.Decls_namespaceContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#decls_enum}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDecls_enum(FlixParser.Decls_enumContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#dcases}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDcases(FlixParser.DcasesContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#dcase}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDcase(FlixParser.DcaseContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#decls_relation}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDecls_relation(FlixParser.Decls_relationContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#decls_lattice}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDecls_lattice(FlixParser.Decls_latticeContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#decls_index}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDecls_index(FlixParser.Decls_indexContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#decls_signature}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDecls_signature(FlixParser.Decls_signatureContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#decls_external}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDecls_external(FlixParser.Decls_externalContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#decls_definition}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDecls_definition(FlixParser.Decls_definitionContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#decls_law}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDecls_law(FlixParser.Decls_lawContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#decls_class}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDecls_class(FlixParser.Decls_classContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#class_body}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitClass_body(FlixParser.Class_bodyContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#class_decl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitClass_decl(FlixParser.Class_declContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#decls_fact}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDecls_fact(FlixParser.Decls_factContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#decls_rule}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDecls_rule(FlixParser.Decls_ruleContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#elms}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitElms(FlixParser.ElmsContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#decls_letlattice}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDecls_letlattice(FlixParser.Decls_letlatticeContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#decls_impl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDecls_impl(FlixParser.Decls_implContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#decls_impl_body}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDecls_impl_body(FlixParser.Decls_impl_bodyContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#expression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExpression(FlixParser.ExpressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#logical}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLogical(FlixParser.LogicalContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#expressions}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExpressions(FlixParser.ExpressionsContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#comparison}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitComparison(FlixParser.ComparisonContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#additive}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAdditive(FlixParser.AdditiveContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#multiplicative}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMultiplicative(FlixParser.MultiplicativeContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#infix}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInfix(FlixParser.InfixContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#extended}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExtended(FlixParser.ExtendedContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#unary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnary(FlixParser.UnaryContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#ascribe}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAscribe(FlixParser.AscribeContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#e_primary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitE_primary(FlixParser.E_primaryContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#e_letMatch}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitE_letMatch(FlixParser.E_letMatchContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#e_ifThenElse}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitE_ifThenElse(FlixParser.E_ifThenElseContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#e_match}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitE_match(FlixParser.E_matchContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#e_switch}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitE_switch(FlixParser.E_switchContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#e_apply}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitE_apply(FlixParser.E_applyContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#e_unaryLambda}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitE_unaryLambda(FlixParser.E_unaryLambdaContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#e_lambda}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitE_lambda(FlixParser.E_lambdaContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#e_literal}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitE_literal(FlixParser.E_literalContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#e_sname}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitE_sname(FlixParser.E_snameContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#e_qname}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitE_qname(FlixParser.E_qnameContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#e_tag}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitE_tag(FlixParser.E_tagContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#e_tuple}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitE_tuple(FlixParser.E_tupleContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#e_keyValue}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitE_keyValue(FlixParser.E_keyValueContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#e_keyValues}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitE_keyValues(FlixParser.E_keyValuesContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#e_userError}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitE_userError(FlixParser.E_userErrorContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#e_wild}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitE_wild(FlixParser.E_wildContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#e_fNil}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitE_fNil(FlixParser.E_fNilContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#e_fList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitE_fList(FlixParser.E_fListContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#e_fVec}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitE_fVec(FlixParser.E_fVecContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#e_fSet}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitE_fSet(FlixParser.E_fSetContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#e_fMap}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitE_fMap(FlixParser.E_fMapContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#existential}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExistential(FlixParser.ExistentialContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#universal}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUniversal(FlixParser.UniversalContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#pattern}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPattern(FlixParser.PatternContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#patterns}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPatterns(FlixParser.PatternsContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#simple}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSimple(FlixParser.SimpleContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#p_keyValue}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitP_keyValue(FlixParser.P_keyValueContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#p_keyValues}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitP_keyValues(FlixParser.P_keyValuesContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#p_literal}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitP_literal(FlixParser.P_literalContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#p_tag}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitP_tag(FlixParser.P_tagContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#p_tuple}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitP_tuple(FlixParser.P_tupleContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#p_wild}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitP_wild(FlixParser.P_wildContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#p_fNil}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitP_fNil(FlixParser.P_fNilContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#p_variable}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitP_variable(FlixParser.P_variableContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#p_fList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitP_fList(FlixParser.P_fListContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#p_fVec}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitP_fVec(FlixParser.P_fVecContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#p_fSet}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitP_fSet(FlixParser.P_fSetContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#p_fMap}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitP_fMap(FlixParser.P_fMapContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#bools}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBools(FlixParser.BoolsContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#chars}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitChars(FlixParser.CharsContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#strs}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStrs(FlixParser.StrsContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#negative}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNegative(FlixParser.NegativeContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#float32}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFloat32(FlixParser.Float32Context ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#float64}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFloat64(FlixParser.Float64Context ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#floatDefault}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFloatDefault(FlixParser.FloatDefaultContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#floats}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFloats(FlixParser.FloatsContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#int8}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInt8(FlixParser.Int8Context ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#int16}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInt16(FlixParser.Int16Context ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#int32}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInt32(FlixParser.Int32Context ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#int64}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInt64(FlixParser.Int64Context ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#bigInt}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBigInt(FlixParser.BigIntContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#intDefault}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIntDefault(FlixParser.IntDefaultContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#ints}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInts(FlixParser.IntsContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#literal}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLiteral(FlixParser.LiteralContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#primary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrimary(FlixParser.PrimaryContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#var}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitVar(FlixParser.VarContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#ref}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRef(FlixParser.RefContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#type}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitType(FlixParser.TypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#arrow}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArrow(FlixParser.ArrowContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#tuple_unit}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTuple_unit(FlixParser.Tuple_unitContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#tuple_singleton}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTuple_singleton(FlixParser.Tuple_singletonContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#tuple_multi}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTuple_multi(FlixParser.Tuple_multiContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#tuple}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTuple(FlixParser.TupleContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#apply}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitApply(FlixParser.ApplyContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#unary_ops}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnary_ops(FlixParser.Unary_opsContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#logical_ops}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLogical_ops(FlixParser.Logical_opsContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#comparison_ops}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitComparison_ops(FlixParser.Comparison_opsContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#multipve_ops}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMultipve_ops(FlixParser.Multipve_opsContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#addve_ops}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAddve_ops(FlixParser.Addve_opsContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#extbin_ops}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExtbin_ops(FlixParser.Extbin_opsContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#predicate}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPredicate(FlixParser.PredicateContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#predicates}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPredicates(FlixParser.PredicatesContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#pred_true}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPred_true(FlixParser.Pred_trueContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#pred_false}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPred_false(FlixParser.Pred_falseContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#pred_filter}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPred_filter(FlixParser.Pred_filterContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#pred_table}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPred_table(FlixParser.Pred_tableContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#pred_notequal}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPred_notequal(FlixParser.Pred_notequalContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlixParser#pred_loop}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPred_loop(FlixParser.Pred_loopContext ctx);
}