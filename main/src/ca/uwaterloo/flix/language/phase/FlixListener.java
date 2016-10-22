// Generated from Flix.g4 by ANTLR 4.5.3

package ca.uwaterloo.flix.language.phase;

import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link FlixParser}.
 */
public interface FlixListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link FlixParser#start}.
	 * @param ctx the parse tree
	 */
	void enterStart(FlixParser.StartContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#start}.
	 * @param ctx the parse tree
	 */
	void exitStart(FlixParser.StartContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#optSC}.
	 * @param ctx the parse tree
	 */
	void enterOptSC(FlixParser.OptSCContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#optSC}.
	 * @param ctx the parse tree
	 */
	void exitOptSC(FlixParser.OptSCContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#nname}.
	 * @param ctx the parse tree
	 */
	void enterNname(FlixParser.NnameContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#nname}.
	 * @param ctx the parse tree
	 */
	void exitNname(FlixParser.NnameContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#qname}.
	 * @param ctx the parse tree
	 */
	void enterQname(FlixParser.QnameContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#qname}.
	 * @param ctx the parse tree
	 */
	void exitQname(FlixParser.QnameContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#annotation}.
	 * @param ctx the parse tree
	 */
	void enterAnnotation(FlixParser.AnnotationContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#annotation}.
	 * @param ctx the parse tree
	 */
	void exitAnnotation(FlixParser.AnnotationContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#argument}.
	 * @param ctx the parse tree
	 */
	void enterArgument(FlixParser.ArgumentContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#argument}.
	 * @param ctx the parse tree
	 */
	void exitArgument(FlixParser.ArgumentContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#arguments}.
	 * @param ctx the parse tree
	 */
	void enterArguments(FlixParser.ArgumentsContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#arguments}.
	 * @param ctx the parse tree
	 */
	void exitArguments(FlixParser.ArgumentsContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#params}.
	 * @param ctx the parse tree
	 */
	void enterParams(FlixParser.ParamsContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#params}.
	 * @param ctx the parse tree
	 */
	void exitParams(FlixParser.ParamsContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#attribute}.
	 * @param ctx the parse tree
	 */
	void enterAttribute(FlixParser.AttributeContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#attribute}.
	 * @param ctx the parse tree
	 */
	void exitAttribute(FlixParser.AttributeContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#attributes}.
	 * @param ctx the parse tree
	 */
	void enterAttributes(FlixParser.AttributesContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#attributes}.
	 * @param ctx the parse tree
	 */
	void exitAttributes(FlixParser.AttributesContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#index}.
	 * @param ctx the parse tree
	 */
	void enterIndex(FlixParser.IndexContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#index}.
	 * @param ctx the parse tree
	 */
	void exitIndex(FlixParser.IndexContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#indexes}.
	 * @param ctx the parse tree
	 */
	void enterIndexes(FlixParser.IndexesContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#indexes}.
	 * @param ctx the parse tree
	 */
	void exitIndexes(FlixParser.IndexesContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#idents}.
	 * @param ctx the parse tree
	 */
	void enterIdents(FlixParser.IdentsContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#idents}.
	 * @param ctx the parse tree
	 */
	void exitIdents(FlixParser.IdentsContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#match_rule}.
	 * @param ctx the parse tree
	 */
	void enterMatch_rule(FlixParser.Match_ruleContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#match_rule}.
	 * @param ctx the parse tree
	 */
	void exitMatch_rule(FlixParser.Match_ruleContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#match_rules}.
	 * @param ctx the parse tree
	 */
	void enterMatch_rules(FlixParser.Match_rulesContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#match_rules}.
	 * @param ctx the parse tree
	 */
	void exitMatch_rules(FlixParser.Match_rulesContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#switch_rule}.
	 * @param ctx the parse tree
	 */
	void enterSwitch_rule(FlixParser.Switch_ruleContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#switch_rule}.
	 * @param ctx the parse tree
	 */
	void exitSwitch_rule(FlixParser.Switch_ruleContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#switch_rules}.
	 * @param ctx the parse tree
	 */
	void enterSwitch_rules(FlixParser.Switch_rulesContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#switch_rules}.
	 * @param ctx the parse tree
	 */
	void exitSwitch_rules(FlixParser.Switch_rulesContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#typeparam}.
	 * @param ctx the parse tree
	 */
	void enterTypeparam(FlixParser.TypeparamContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#typeparam}.
	 * @param ctx the parse tree
	 */
	void exitTypeparam(FlixParser.TypeparamContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#typeparams}.
	 * @param ctx the parse tree
	 */
	void enterTypeparams(FlixParser.TypeparamsContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#typeparams}.
	 * @param ctx the parse tree
	 */
	void exitTypeparams(FlixParser.TypeparamsContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#class_typeparams}.
	 * @param ctx the parse tree
	 */
	void enterClass_typeparams(FlixParser.Class_typeparamsContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#class_typeparams}.
	 * @param ctx the parse tree
	 */
	void exitClass_typeparams(FlixParser.Class_typeparamsContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#contextBound}.
	 * @param ctx the parse tree
	 */
	void enterContextBound(FlixParser.ContextBoundContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#contextBound}.
	 * @param ctx the parse tree
	 */
	void exitContextBound(FlixParser.ContextBoundContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#contextBounds}.
	 * @param ctx the parse tree
	 */
	void enterContextBounds(FlixParser.ContextBoundsContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#contextBounds}.
	 * @param ctx the parse tree
	 */
	void exitContextBounds(FlixParser.ContextBoundsContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#contextBoundsList}.
	 * @param ctx the parse tree
	 */
	void enterContextBoundsList(FlixParser.ContextBoundsListContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#contextBoundsList}.
	 * @param ctx the parse tree
	 */
	void exitContextBoundsList(FlixParser.ContextBoundsListContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#functions}.
	 * @param ctx the parse tree
	 */
	void enterFunctions(FlixParser.FunctionsContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#functions}.
	 * @param ctx the parse tree
	 */
	void exitFunctions(FlixParser.FunctionsContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#s_import}.
	 * @param ctx the parse tree
	 */
	void enterS_import(FlixParser.S_importContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#s_import}.
	 * @param ctx the parse tree
	 */
	void exitS_import(FlixParser.S_importContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#s_imports}.
	 * @param ctx the parse tree
	 */
	void enterS_imports(FlixParser.S_importsContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#s_imports}.
	 * @param ctx the parse tree
	 */
	void exitS_imports(FlixParser.S_importsContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#import_wildcard}.
	 * @param ctx the parse tree
	 */
	void enterImport_wildcard(FlixParser.Import_wildcardContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#import_wildcard}.
	 * @param ctx the parse tree
	 */
	void exitImport_wildcard(FlixParser.Import_wildcardContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#import_definition}.
	 * @param ctx the parse tree
	 */
	void enterImport_definition(FlixParser.Import_definitionContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#import_definition}.
	 * @param ctx the parse tree
	 */
	void exitImport_definition(FlixParser.Import_definitionContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#import_namespace}.
	 * @param ctx the parse tree
	 */
	void enterImport_namespace(FlixParser.Import_namespaceContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#import_namespace}.
	 * @param ctx the parse tree
	 */
	void exitImport_namespace(FlixParser.Import_namespaceContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#decl}.
	 * @param ctx the parse tree
	 */
	void enterDecl(FlixParser.DeclContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#decl}.
	 * @param ctx the parse tree
	 */
	void exitDecl(FlixParser.DeclContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#decls}.
	 * @param ctx the parse tree
	 */
	void enterDecls(FlixParser.DeclsContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#decls}.
	 * @param ctx the parse tree
	 */
	void exitDecls(FlixParser.DeclsContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#decls_namespace}.
	 * @param ctx the parse tree
	 */
	void enterDecls_namespace(FlixParser.Decls_namespaceContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#decls_namespace}.
	 * @param ctx the parse tree
	 */
	void exitDecls_namespace(FlixParser.Decls_namespaceContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#decls_enum}.
	 * @param ctx the parse tree
	 */
	void enterDecls_enum(FlixParser.Decls_enumContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#decls_enum}.
	 * @param ctx the parse tree
	 */
	void exitDecls_enum(FlixParser.Decls_enumContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#cases}.
	 * @param ctx the parse tree
	 */
	void enterCases(FlixParser.CasesContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#cases}.
	 * @param ctx the parse tree
	 */
	void exitCases(FlixParser.CasesContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#decls_relation}.
	 * @param ctx the parse tree
	 */
	void enterDecls_relation(FlixParser.Decls_relationContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#decls_relation}.
	 * @param ctx the parse tree
	 */
	void exitDecls_relation(FlixParser.Decls_relationContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#decls_lattice}.
	 * @param ctx the parse tree
	 */
	void enterDecls_lattice(FlixParser.Decls_latticeContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#decls_lattice}.
	 * @param ctx the parse tree
	 */
	void exitDecls_lattice(FlixParser.Decls_latticeContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#decls_index}.
	 * @param ctx the parse tree
	 */
	void enterDecls_index(FlixParser.Decls_indexContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#decls_index}.
	 * @param ctx the parse tree
	 */
	void exitDecls_index(FlixParser.Decls_indexContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#decls_signature}.
	 * @param ctx the parse tree
	 */
	void enterDecls_signature(FlixParser.Decls_signatureContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#decls_signature}.
	 * @param ctx the parse tree
	 */
	void exitDecls_signature(FlixParser.Decls_signatureContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#decls_external}.
	 * @param ctx the parse tree
	 */
	void enterDecls_external(FlixParser.Decls_externalContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#decls_external}.
	 * @param ctx the parse tree
	 */
	void exitDecls_external(FlixParser.Decls_externalContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#decls_function}.
	 * @param ctx the parse tree
	 */
	void enterDecls_function(FlixParser.Decls_functionContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#decls_function}.
	 * @param ctx the parse tree
	 */
	void exitDecls_function(FlixParser.Decls_functionContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#decls_law}.
	 * @param ctx the parse tree
	 */
	void enterDecls_law(FlixParser.Decls_lawContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#decls_law}.
	 * @param ctx the parse tree
	 */
	void exitDecls_law(FlixParser.Decls_lawContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#decls_class}.
	 * @param ctx the parse tree
	 */
	void enterDecls_class(FlixParser.Decls_classContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#decls_class}.
	 * @param ctx the parse tree
	 */
	void exitDecls_class(FlixParser.Decls_classContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#class_body}.
	 * @param ctx the parse tree
	 */
	void enterClass_body(FlixParser.Class_bodyContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#class_body}.
	 * @param ctx the parse tree
	 */
	void exitClass_body(FlixParser.Class_bodyContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#decls_fact}.
	 * @param ctx the parse tree
	 */
	void enterDecls_fact(FlixParser.Decls_factContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#decls_fact}.
	 * @param ctx the parse tree
	 */
	void exitDecls_fact(FlixParser.Decls_factContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#decls_rule}.
	 * @param ctx the parse tree
	 */
	void enterDecls_rule(FlixParser.Decls_ruleContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#decls_rule}.
	 * @param ctx the parse tree
	 */
	void exitDecls_rule(FlixParser.Decls_ruleContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#elms}.
	 * @param ctx the parse tree
	 */
	void enterElms(FlixParser.ElmsContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#elms}.
	 * @param ctx the parse tree
	 */
	void exitElms(FlixParser.ElmsContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#decls_letlattice}.
	 * @param ctx the parse tree
	 */
	void enterDecls_letlattice(FlixParser.Decls_letlatticeContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#decls_letlattice}.
	 * @param ctx the parse tree
	 */
	void exitDecls_letlattice(FlixParser.Decls_letlatticeContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#expression}.
	 * @param ctx the parse tree
	 */
	void enterExpression(FlixParser.ExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#expression}.
	 * @param ctx the parse tree
	 */
	void exitExpression(FlixParser.ExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#expressions}.
	 * @param ctx the parse tree
	 */
	void enterExpressions(FlixParser.ExpressionsContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#expressions}.
	 * @param ctx the parse tree
	 */
	void exitExpressions(FlixParser.ExpressionsContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#comparison}.
	 * @param ctx the parse tree
	 */
	void enterComparison(FlixParser.ComparisonContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#comparison}.
	 * @param ctx the parse tree
	 */
	void exitComparison(FlixParser.ComparisonContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#additive}.
	 * @param ctx the parse tree
	 */
	void enterAdditive(FlixParser.AdditiveContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#additive}.
	 * @param ctx the parse tree
	 */
	void exitAdditive(FlixParser.AdditiveContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#multiplicative}.
	 * @param ctx the parse tree
	 */
	void enterMultiplicative(FlixParser.MultiplicativeContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#multiplicative}.
	 * @param ctx the parse tree
	 */
	void exitMultiplicative(FlixParser.MultiplicativeContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#infix}.
	 * @param ctx the parse tree
	 */
	void enterInfix(FlixParser.InfixContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#infix}.
	 * @param ctx the parse tree
	 */
	void exitInfix(FlixParser.InfixContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#extended}.
	 * @param ctx the parse tree
	 */
	void enterExtended(FlixParser.ExtendedContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#extended}.
	 * @param ctx the parse tree
	 */
	void exitExtended(FlixParser.ExtendedContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#unary}.
	 * @param ctx the parse tree
	 */
	void enterUnary(FlixParser.UnaryContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#unary}.
	 * @param ctx the parse tree
	 */
	void exitUnary(FlixParser.UnaryContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#ascribe}.
	 * @param ctx the parse tree
	 */
	void enterAscribe(FlixParser.AscribeContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#ascribe}.
	 * @param ctx the parse tree
	 */
	void exitAscribe(FlixParser.AscribeContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#e_primary}.
	 * @param ctx the parse tree
	 */
	void enterE_primary(FlixParser.E_primaryContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#e_primary}.
	 * @param ctx the parse tree
	 */
	void exitE_primary(FlixParser.E_primaryContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#e_letMatch}.
	 * @param ctx the parse tree
	 */
	void enterE_letMatch(FlixParser.E_letMatchContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#e_letMatch}.
	 * @param ctx the parse tree
	 */
	void exitE_letMatch(FlixParser.E_letMatchContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#e_ifThenElse}.
	 * @param ctx the parse tree
	 */
	void enterE_ifThenElse(FlixParser.E_ifThenElseContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#e_ifThenElse}.
	 * @param ctx the parse tree
	 */
	void exitE_ifThenElse(FlixParser.E_ifThenElseContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#e_match}.
	 * @param ctx the parse tree
	 */
	void enterE_match(FlixParser.E_matchContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#e_match}.
	 * @param ctx the parse tree
	 */
	void exitE_match(FlixParser.E_matchContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#e_switch}.
	 * @param ctx the parse tree
	 */
	void enterE_switch(FlixParser.E_switchContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#e_switch}.
	 * @param ctx the parse tree
	 */
	void exitE_switch(FlixParser.E_switchContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#e_apply}.
	 * @param ctx the parse tree
	 */
	void enterE_apply(FlixParser.E_applyContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#e_apply}.
	 * @param ctx the parse tree
	 */
	void exitE_apply(FlixParser.E_applyContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#e_var}.
	 * @param ctx the parse tree
	 */
	void enterE_var(FlixParser.E_varContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#e_var}.
	 * @param ctx the parse tree
	 */
	void exitE_var(FlixParser.E_varContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#e_tag}.
	 * @param ctx the parse tree
	 */
	void enterE_tag(FlixParser.E_tagContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#e_tag}.
	 * @param ctx the parse tree
	 */
	void exitE_tag(FlixParser.E_tagContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#e_tuple}.
	 * @param ctx the parse tree
	 */
	void enterE_tuple(FlixParser.E_tupleContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#e_tuple}.
	 * @param ctx the parse tree
	 */
	void exitE_tuple(FlixParser.E_tupleContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#e_keyValue}.
	 * @param ctx the parse tree
	 */
	void enterE_keyValue(FlixParser.E_keyValueContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#e_keyValue}.
	 * @param ctx the parse tree
	 */
	void exitE_keyValue(FlixParser.E_keyValueContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#e_keyValues}.
	 * @param ctx the parse tree
	 */
	void enterE_keyValues(FlixParser.E_keyValuesContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#e_keyValues}.
	 * @param ctx the parse tree
	 */
	void exitE_keyValues(FlixParser.E_keyValuesContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#e_wild}.
	 * @param ctx the parse tree
	 */
	void enterE_wild(FlixParser.E_wildContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#e_wild}.
	 * @param ctx the parse tree
	 */
	void exitE_wild(FlixParser.E_wildContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#e_fNil}.
	 * @param ctx the parse tree
	 */
	void enterE_fNil(FlixParser.E_fNilContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#e_fNil}.
	 * @param ctx the parse tree
	 */
	void exitE_fNil(FlixParser.E_fNilContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#e_fNone}.
	 * @param ctx the parse tree
	 */
	void enterE_fNone(FlixParser.E_fNoneContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#e_fNone}.
	 * @param ctx the parse tree
	 */
	void exitE_fNone(FlixParser.E_fNoneContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#e_fSome}.
	 * @param ctx the parse tree
	 */
	void enterE_fSome(FlixParser.E_fSomeContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#e_fSome}.
	 * @param ctx the parse tree
	 */
	void exitE_fSome(FlixParser.E_fSomeContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#e_fList}.
	 * @param ctx the parse tree
	 */
	void enterE_fList(FlixParser.E_fListContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#e_fList}.
	 * @param ctx the parse tree
	 */
	void exitE_fList(FlixParser.E_fListContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#e_fVec}.
	 * @param ctx the parse tree
	 */
	void enterE_fVec(FlixParser.E_fVecContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#e_fVec}.
	 * @param ctx the parse tree
	 */
	void exitE_fVec(FlixParser.E_fVecContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#e_fSet}.
	 * @param ctx the parse tree
	 */
	void enterE_fSet(FlixParser.E_fSetContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#e_fSet}.
	 * @param ctx the parse tree
	 */
	void exitE_fSet(FlixParser.E_fSetContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#e_fMap}.
	 * @param ctx the parse tree
	 */
	void enterE_fMap(FlixParser.E_fMapContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#e_fMap}.
	 * @param ctx the parse tree
	 */
	void exitE_fMap(FlixParser.E_fMapContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#e_unaryLambda}.
	 * @param ctx the parse tree
	 */
	void enterE_unaryLambda(FlixParser.E_unaryLambdaContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#e_unaryLambda}.
	 * @param ctx the parse tree
	 */
	void exitE_unaryLambda(FlixParser.E_unaryLambdaContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#e_lambda}.
	 * @param ctx the parse tree
	 */
	void enterE_lambda(FlixParser.E_lambdaContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#e_lambda}.
	 * @param ctx the parse tree
	 */
	void exitE_lambda(FlixParser.E_lambdaContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#existential}.
	 * @param ctx the parse tree
	 */
	void enterExistential(FlixParser.ExistentialContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#existential}.
	 * @param ctx the parse tree
	 */
	void exitExistential(FlixParser.ExistentialContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#universal}.
	 * @param ctx the parse tree
	 */
	void enterUniversal(FlixParser.UniversalContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#universal}.
	 * @param ctx the parse tree
	 */
	void exitUniversal(FlixParser.UniversalContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#pattern}.
	 * @param ctx the parse tree
	 */
	void enterPattern(FlixParser.PatternContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#pattern}.
	 * @param ctx the parse tree
	 */
	void exitPattern(FlixParser.PatternContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#patterns}.
	 * @param ctx the parse tree
	 */
	void enterPatterns(FlixParser.PatternsContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#patterns}.
	 * @param ctx the parse tree
	 */
	void exitPatterns(FlixParser.PatternsContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#simple}.
	 * @param ctx the parse tree
	 */
	void enterSimple(FlixParser.SimpleContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#simple}.
	 * @param ctx the parse tree
	 */
	void exitSimple(FlixParser.SimpleContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#p_keyValue}.
	 * @param ctx the parse tree
	 */
	void enterP_keyValue(FlixParser.P_keyValueContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#p_keyValue}.
	 * @param ctx the parse tree
	 */
	void exitP_keyValue(FlixParser.P_keyValueContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#p_keyValues}.
	 * @param ctx the parse tree
	 */
	void enterP_keyValues(FlixParser.P_keyValuesContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#p_keyValues}.
	 * @param ctx the parse tree
	 */
	void exitP_keyValues(FlixParser.P_keyValuesContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#p_tag}.
	 * @param ctx the parse tree
	 */
	void enterP_tag(FlixParser.P_tagContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#p_tag}.
	 * @param ctx the parse tree
	 */
	void exitP_tag(FlixParser.P_tagContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#p_tuple}.
	 * @param ctx the parse tree
	 */
	void enterP_tuple(FlixParser.P_tupleContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#p_tuple}.
	 * @param ctx the parse tree
	 */
	void exitP_tuple(FlixParser.P_tupleContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#p_fNil}.
	 * @param ctx the parse tree
	 */
	void enterP_fNil(FlixParser.P_fNilContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#p_fNil}.
	 * @param ctx the parse tree
	 */
	void exitP_fNil(FlixParser.P_fNilContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#p_fNone}.
	 * @param ctx the parse tree
	 */
	void enterP_fNone(FlixParser.P_fNoneContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#p_fNone}.
	 * @param ctx the parse tree
	 */
	void exitP_fNone(FlixParser.P_fNoneContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#p_fVec}.
	 * @param ctx the parse tree
	 */
	void enterP_fVec(FlixParser.P_fVecContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#p_fVec}.
	 * @param ctx the parse tree
	 */
	void exitP_fVec(FlixParser.P_fVecContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#p_fSet}.
	 * @param ctx the parse tree
	 */
	void enterP_fSet(FlixParser.P_fSetContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#p_fSet}.
	 * @param ctx the parse tree
	 */
	void exitP_fSet(FlixParser.P_fSetContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#p_fMap}.
	 * @param ctx the parse tree
	 */
	void enterP_fMap(FlixParser.P_fMapContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#p_fMap}.
	 * @param ctx the parse tree
	 */
	void exitP_fMap(FlixParser.P_fMapContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#bools}.
	 * @param ctx the parse tree
	 */
	void enterBools(FlixParser.BoolsContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#bools}.
	 * @param ctx the parse tree
	 */
	void exitBools(FlixParser.BoolsContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#negative}.
	 * @param ctx the parse tree
	 */
	void enterNegative(FlixParser.NegativeContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#negative}.
	 * @param ctx the parse tree
	 */
	void exitNegative(FlixParser.NegativeContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#float32}.
	 * @param ctx the parse tree
	 */
	void enterFloat32(FlixParser.Float32Context ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#float32}.
	 * @param ctx the parse tree
	 */
	void exitFloat32(FlixParser.Float32Context ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#float64}.
	 * @param ctx the parse tree
	 */
	void enterFloat64(FlixParser.Float64Context ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#float64}.
	 * @param ctx the parse tree
	 */
	void exitFloat64(FlixParser.Float64Context ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#floatDefault}.
	 * @param ctx the parse tree
	 */
	void enterFloatDefault(FlixParser.FloatDefaultContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#floatDefault}.
	 * @param ctx the parse tree
	 */
	void exitFloatDefault(FlixParser.FloatDefaultContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#floats}.
	 * @param ctx the parse tree
	 */
	void enterFloats(FlixParser.FloatsContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#floats}.
	 * @param ctx the parse tree
	 */
	void exitFloats(FlixParser.FloatsContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#int8}.
	 * @param ctx the parse tree
	 */
	void enterInt8(FlixParser.Int8Context ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#int8}.
	 * @param ctx the parse tree
	 */
	void exitInt8(FlixParser.Int8Context ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#int16}.
	 * @param ctx the parse tree
	 */
	void enterInt16(FlixParser.Int16Context ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#int16}.
	 * @param ctx the parse tree
	 */
	void exitInt16(FlixParser.Int16Context ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#int32}.
	 * @param ctx the parse tree
	 */
	void enterInt32(FlixParser.Int32Context ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#int32}.
	 * @param ctx the parse tree
	 */
	void exitInt32(FlixParser.Int32Context ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#int64}.
	 * @param ctx the parse tree
	 */
	void enterInt64(FlixParser.Int64Context ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#int64}.
	 * @param ctx the parse tree
	 */
	void exitInt64(FlixParser.Int64Context ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#bigInt}.
	 * @param ctx the parse tree
	 */
	void enterBigInt(FlixParser.BigIntContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#bigInt}.
	 * @param ctx the parse tree
	 */
	void exitBigInt(FlixParser.BigIntContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#intDefault}.
	 * @param ctx the parse tree
	 */
	void enterIntDefault(FlixParser.IntDefaultContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#intDefault}.
	 * @param ctx the parse tree
	 */
	void exitIntDefault(FlixParser.IntDefaultContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#ints}.
	 * @param ctx the parse tree
	 */
	void enterInts(FlixParser.IntsContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#ints}.
	 * @param ctx the parse tree
	 */
	void exitInts(FlixParser.IntsContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#literals}.
	 * @param ctx the parse tree
	 */
	void enterLiterals(FlixParser.LiteralsContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#literals}.
	 * @param ctx the parse tree
	 */
	void exitLiterals(FlixParser.LiteralsContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#primary}.
	 * @param ctx the parse tree
	 */
	void enterPrimary(FlixParser.PrimaryContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#primary}.
	 * @param ctx the parse tree
	 */
	void exitPrimary(FlixParser.PrimaryContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#type}.
	 * @param ctx the parse tree
	 */
	void enterType(FlixParser.TypeContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#type}.
	 * @param ctx the parse tree
	 */
	void exitType(FlixParser.TypeContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#lambda}.
	 * @param ctx the parse tree
	 */
	void enterLambda(FlixParser.LambdaContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#lambda}.
	 * @param ctx the parse tree
	 */
	void exitLambda(FlixParser.LambdaContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#tuple_unit}.
	 * @param ctx the parse tree
	 */
	void enterTuple_unit(FlixParser.Tuple_unitContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#tuple_unit}.
	 * @param ctx the parse tree
	 */
	void exitTuple_unit(FlixParser.Tuple_unitContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#tuple_singleton}.
	 * @param ctx the parse tree
	 */
	void enterTuple_singleton(FlixParser.Tuple_singletonContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#tuple_singleton}.
	 * @param ctx the parse tree
	 */
	void exitTuple_singleton(FlixParser.Tuple_singletonContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#tuple_multi}.
	 * @param ctx the parse tree
	 */
	void enterTuple_multi(FlixParser.Tuple_multiContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#tuple_multi}.
	 * @param ctx the parse tree
	 */
	void exitTuple_multi(FlixParser.Tuple_multiContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#tuple}.
	 * @param ctx the parse tree
	 */
	void enterTuple(FlixParser.TupleContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#tuple}.
	 * @param ctx the parse tree
	 */
	void exitTuple(FlixParser.TupleContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#parametric}.
	 * @param ctx the parse tree
	 */
	void enterParametric(FlixParser.ParametricContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#parametric}.
	 * @param ctx the parse tree
	 */
	void exitParametric(FlixParser.ParametricContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#unary_ops}.
	 * @param ctx the parse tree
	 */
	void enterUnary_ops(FlixParser.Unary_opsContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#unary_ops}.
	 * @param ctx the parse tree
	 */
	void exitUnary_ops(FlixParser.Unary_opsContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#logical_ops}.
	 * @param ctx the parse tree
	 */
	void enterLogical_ops(FlixParser.Logical_opsContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#logical_ops}.
	 * @param ctx the parse tree
	 */
	void exitLogical_ops(FlixParser.Logical_opsContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#comparison_ops}.
	 * @param ctx the parse tree
	 */
	void enterComparison_ops(FlixParser.Comparison_opsContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#comparison_ops}.
	 * @param ctx the parse tree
	 */
	void exitComparison_ops(FlixParser.Comparison_opsContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#multipve_ops}.
	 * @param ctx the parse tree
	 */
	void enterMultipve_ops(FlixParser.Multipve_opsContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#multipve_ops}.
	 * @param ctx the parse tree
	 */
	void exitMultipve_ops(FlixParser.Multipve_opsContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#addve_ops}.
	 * @param ctx the parse tree
	 */
	void enterAddve_ops(FlixParser.Addve_opsContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#addve_ops}.
	 * @param ctx the parse tree
	 */
	void exitAddve_ops(FlixParser.Addve_opsContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#extbin_ops}.
	 * @param ctx the parse tree
	 */
	void enterExtbin_ops(FlixParser.Extbin_opsContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#extbin_ops}.
	 * @param ctx the parse tree
	 */
	void exitExtbin_ops(FlixParser.Extbin_opsContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#predicate}.
	 * @param ctx the parse tree
	 */
	void enterPredicate(FlixParser.PredicateContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#predicate}.
	 * @param ctx the parse tree
	 */
	void exitPredicate(FlixParser.PredicateContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#predicates}.
	 * @param ctx the parse tree
	 */
	void enterPredicates(FlixParser.PredicatesContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#predicates}.
	 * @param ctx the parse tree
	 */
	void exitPredicates(FlixParser.PredicatesContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#pred_true}.
	 * @param ctx the parse tree
	 */
	void enterPred_true(FlixParser.Pred_trueContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#pred_true}.
	 * @param ctx the parse tree
	 */
	void exitPred_true(FlixParser.Pred_trueContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#pred_false}.
	 * @param ctx the parse tree
	 */
	void enterPred_false(FlixParser.Pred_falseContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#pred_false}.
	 * @param ctx the parse tree
	 */
	void exitPred_false(FlixParser.Pred_falseContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#pred_ambiguous}.
	 * @param ctx the parse tree
	 */
	void enterPred_ambiguous(FlixParser.Pred_ambiguousContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#pred_ambiguous}.
	 * @param ctx the parse tree
	 */
	void exitPred_ambiguous(FlixParser.Pred_ambiguousContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#pred_equal}.
	 * @param ctx the parse tree
	 */
	void enterPred_equal(FlixParser.Pred_equalContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#pred_equal}.
	 * @param ctx the parse tree
	 */
	void exitPred_equal(FlixParser.Pred_equalContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#pred_notequal}.
	 * @param ctx the parse tree
	 */
	void enterPred_notequal(FlixParser.Pred_notequalContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#pred_notequal}.
	 * @param ctx the parse tree
	 */
	void exitPred_notequal(FlixParser.Pred_notequalContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlixParser#pred_loop}.
	 * @param ctx the parse tree
	 */
	void enterPred_loop(FlixParser.Pred_loopContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlixParser#pred_loop}.
	 * @param ctx the parse tree
	 */
	void exitPred_loop(FlixParser.Pred_loopContext ctx);
}