package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.{ParsedAst, _}
import scala.collection.immutable.Seq;
import collection.JavaConversions._;
import org.antlr.v4.runtime.Token;

class AST_FlixVisitor(source: SourceInput, input: String) /*extends FlixVisitor[Object]*/{

	//====================
	//SOURCE POSITION NODE
	//====================
	def visitStartSp(tk: Token): SourcePosition = SourcePosition(
			source,
			tk.getLine(),
			tk.getCharPositionInLine(),
			Some(input)
		)
	def visitStopSp(tk: Token): SourcePosition = SourcePosition(
			source,
			tk.getLine(),
			tk.getCharPositionInLine()+tk.getStopIndex(),
			Some(input)
		)

	//=====================
	//TRIPLE SLASH COMMENTS
	//=====================
	def visitTscomment(ctxs: Seq[FlixParser.TscommentContext]) = 
		if (ctxs.isEmpty) Option(null)
		else Option(ParsedAst.Documentation(
			visitStartSp(ctxs.head.getStart()),
			ctxs.map(x => x.TripleSlashComment().getText()),
			visitStopSp(ctxs.last.getStop())
		))

	//=========
	//ROOT NODE
	//=========
	def visitStart(ctx: FlixParser.StartContext): ParsedAst.Root = ParsedAst.Root(
			ctx.s_import().map(visitS_import).toList.toSeq,
			ctx.decl().map(visitDecl).toList.toSeq
		)
	//==========
	//NAME RULES
	//==========

	def visitIdent(ctx: FlixParser.IdentContext) =  Name.Ident(
			visitStartSp(ctx.getStart()),
			ctx.getChild(0).getText(),
			visitStopSp(ctx.getStop())
		)
	def visitUpperLowerIdent(tk: Token) = Name.Ident(
			visitStartSp(tk),
			tk.getText(),
			visitStopSp(tk)
		)

	def visitNname(ctx: FlixParser.NnameContext) = Name.NName(
			visitStartSp(ctx.getStart()),
			ctx.ident().map(visitIdent).toList,
			visitStopSp(ctx.getStop())
		)

	def visitLowerqname(ctx: FlixParser.LowerqnameContext) = ctx.nname match {
			case null => Name.QName(
				visitStartSp(ctx.getStart()),
				Name.RootNS,
				visitUpperLowerIdent(ctx.LowerIdent().getSymbol()),
				visitStopSp(ctx.getStop())
			)
			case x: FlixParser.NnameContext =>Name.QName(
				visitStartSp(ctx.getStart()),
				visitNname(ctx.nname()),
				visitUpperLowerIdent(ctx.LowerIdent().getSymbol()),
				visitStopSp(ctx.getStop())
			)
		}
	def visitUpperqname(ctx: FlixParser.UpperqnameContext) = ctx.nname match {
			case null => Name.QName(
				visitStartSp(ctx.getStart()),
				Name.RootNS,
				visitUpperLowerIdent(ctx.UpperIdent().getSymbol()),
				visitStopSp(ctx.getStop())
			)
			case x: FlixParser.NnameContext =>Name.QName(
				visitStartSp(ctx.getStart()),
				visitNname(ctx.nname()),
				visitUpperLowerIdent(ctx.UpperIdent().getSymbol()),
				visitStopSp(ctx.getStop())
			)
		}
		
	def visitAnnotationName(ctx: FlixParser.AnnotationNameContext) = visitUpperLowerIdent(ctx.LowerIdent().getSymbol())
	def visitAttributeName(ctx: FlixParser.AttributeNameContext) = visitUpperLowerIdent(ctx.LowerIdent().getSymbol())
	def visitClassName(ctx: FlixParser.ClassNameContext) = visitUpperLowerIdent(ctx.UpperIdent().getSymbol())
	def visitDefinitionName(ctx: FlixParser.DefinitionNameContext) = visitUpperLowerIdent(ctx.LowerIdent().getSymbol())
	def visitQualifiedDefinitionName(ctx: FlixParser.QualifiedDefinitionNameContext) = visitLowerqname(ctx.lowerqname());
	def visitTableName(ctx: FlixParser.TableNameContext) = visitUpperLowerIdent(ctx.UpperIdent().getSymbol())
	def visitQualifiedTableName(ctx: FlixParser.QualifiedTableNameContext) = visitUpperqname(ctx.upperqname());
	def visitTagName(ctx: FlixParser.TagNameContext) = visitUpperLowerIdent(ctx.UpperIdent().getSymbol())
	def visitTypeName(ctx: FlixParser.TypeNameContext) = visitUpperLowerIdent(ctx.UpperIdent().getSymbol())
	def visitQualifiedTypeName(ctx: FlixParser.QualifiedTypeNameContext) = visitUpperqname(ctx.upperqname())
	def visitVariableName(ctx: FlixParser.VariableNameContext) = visitUpperLowerIdent(ctx.LowerIdent().getSymbol())

	//===============
	//RULES FOR LISTS
	//===============

	def visitVariableNames(ctx: FlixParser.VariableNamesContext) = ctx.variableName().map(visitVariableName).toList.toSeq

	def visitArgument(ctx : FlixParser.ArgumentContext) = ParsedAst.FormalParam(
			visitStartSp(ctx.getStart()),
			visitVariableName(ctx.variableName()),
			visitType(ctx.`type`()),
			visitStopSp(ctx.getStop())
		)

	def visitArguments(ctx : FlixParser.ArgumentsContext) = ctx.argument().map(visitArgument).toList.toSeq

	def visitFormalparams(ctx : FlixParser.FormalparamsContext) = ctx.arguments match{
			case null => Option(null)
			case x: FlixParser.ArgumentsContext => Option(visitArguments(x))
		}

	def visitIndex(ctx: FlixParser.IndexContext) = ctx.attributeName().map(visitAttributeName).toList.toSeq
	def visitIndexes(ctx: FlixParser.IndexesContext) = ctx.index().map(visitIndex).toList.toSeq
	

	def visitAttribute(ctx: FlixParser.AttributeContext) = ParsedAst.Attribute(
			visitStartSp(ctx.getStart()),
			visitAttributeName(ctx.attributeName()),
			visitType(ctx.`type`()),
			visitStopSp(ctx.getStop())
		)
	def visitAttributes(ctx: FlixParser.AttributesContext) = ctx.attribute().map(visitAttribute).toList.toSeq


	def visitContextBound(ctx: FlixParser.ContextBoundContext) = ParsedAst.ContextBound(
			visitStartSp(ctx.getStart()),
			visitClassName(ctx.className()),
			visitClass_typeparams(ctx.class_typeparams()),
			visitStopSp(ctx.getStop())
		)
	def visitContextBounds(ctx: FlixParser.ContextBoundsContext) = ctx.contextBound().map(visitContextBound).toList.toSeq

	def visitClass_typeparams(ctx: FlixParser.Class_typeparamsContext) = ctx.`type`().map(visitType).toList.toSeq

	def visitTypeparam(ctx: FlixParser.TypeparamContext) = ctx.`type`() match {
			case null => ParsedAst.ContextBound(
				visitStartSp(ctx.getStart()),
				visitVariableName(ctx.variableName()),
				Seq(),
				visitStopSp(ctx.getStop())
				)
			case x: FlixParser.TypeContext => ParsedAst.ContextBound(
				visitStartSp(ctx.getStart()),
				visitVariableName(ctx.variableName()),
				Seq(visitType(x)),
				visitStopSp(ctx.getStop())
				)
		}

	def visitTypeparams(ctx: FlixParser.TypeparamsContext) = ctx.typeparam().map(visitTypeparam).toList.toSeq

	def visitDcase(ctx: FlixParser.DcaseContext) = ctx.tuple() match {
			case null => ParsedAst.Case(
					visitStartSp(ctx.getStart()),
					visitTagName(ctx.tagName()),
					ParsedAst.Type.Unit(visitStartSp(ctx.getStart()),visitStopSp(ctx.getStop())),
					visitStopSp(ctx.getStop())

				)
			case x: FlixParser.TupleContext => ParsedAst.Case(
					visitStartSp(ctx.getStart()),
					visitTagName(ctx.tagName()),
					visitTuple(x),
					visitStopSp(ctx.getStop())
				)
		}
	def visitDcases(ctx: FlixParser.DcasesContext) = ctx.dcase().map(visitDcase).toList.toSeq

	def visitAnnotation(ctx: FlixParser.AnnotationContext) = ParsedAst.Annotation(
			visitStartSp(ctx.getStart()),
			visitAnnotationName(ctx.annotationName()),
			visitStopSp(ctx.getStop())
		)

	def visitAnnotations(ctx: FlixParser.AnnotationsContext) = ctx.annotation().map(visitAnnotation).toList.toSeq
	
	//============
	//IMPORT RULES
	//============

	def visitS_import(ctx: FlixParser.S_importContext): ParsedAst.Import =  ctx.getChild(ctx.getChildCount-1) match {
			case x: FlixParser.Import_wildcardContext => visitImport_wildcard(x)
			case x: FlixParser.Import_definitionContext => visitImport_definition(x)
			case x: FlixParser.Import_namespaceContext =>  visitImport_namespace(x)
		}

	def visitImport_wildcard(ctx: FlixParser.Import_wildcardContext) = ParsedAst.Import.Wild(
			visitStartSp(ctx.getStart()),
			visitNname(ctx.nname()),
			visitStopSp(ctx.getStop())
		)

	def visitImport_definition(ctx: FlixParser.Import_definitionContext) = ParsedAst.Import.Definition(
			visitStartSp(ctx.getStart()),
			visitNname(ctx.nname()),
			visitIdent(ctx.ident()),
			visitStopSp(ctx.getStop())
		)

	def visitImport_namespace(ctx: FlixParser.Import_namespaceContext) =ParsedAst.Import.Namespace(
			visitStartSp(ctx.getStart()),
			visitNname(ctx.nname()),
			visitStopSp(ctx.getStop())
		)

	//=================
	//DECLARATION RULES
	//=================
	//fix stop SP
	def visitDecl(ctx: FlixParser.DeclContext): ParsedAst.Declaration = ctx.getChild(0) match {
			case x: FlixParser.Decls_namespaceContext => visitDecls_namespace(x)
			case x: FlixParser.Decls_enumContext => visitDecls_enum(x)
			case x: FlixParser.Decls_relationContext => visitDecls_relation(x)
			case x: FlixParser.Decls_latticeContext => visitDecls_lattice(x)
			case x: FlixParser.Decls_indexContext => visitDecls_index(x)
			case x: FlixParser.Decls_signatureContext => visitDecls_signature(x)
			case x: FlixParser.Decls_externalContext => visitDecls_external(x)
			case x: FlixParser.Decls_definitionContext => visitDecls_definition(x)
			case x: FlixParser.Decls_lawContext => visitDecls_law(x)
			case x: FlixParser.Decls_classContext => visitDecls_class(x)
			case x: FlixParser.Decls_factContext => visitDecls_fact(x)
			case x: FlixParser.Decls_ruleContext => visitDecls_rule(x)
			case x: FlixParser.Decls_letlatticeContext => visitDecls_letlattice(x)
		}

	def visitDecls_namespace(ctx: FlixParser.Decls_namespaceContext) = ParsedAst.Declaration.Namespace(
			visitStartSp(ctx.getStart()),
			visitNname(ctx.nname()),
			ctx.decl().map(visitDecl).toList.toSeq,
			visitStopSp(ctx.getStop())
		)
	
	def visitDecls_enum(ctx: FlixParser.Decls_enumContext) = ParsedAst.Declaration.Enum(
			visitTscomment(ctx.tscomment().toList.toSeq),
			visitStartSp(ctx.ENUM().getSymbol()),
			visitTypeName(ctx.typeName()),
			visitTypeparams(ctx.typeparams()),
			visitDcases(ctx.dcases()),
			visitStopSp(ctx.getStop())
		)
	
	def visitDecls_relation(ctx: FlixParser.Decls_relationContext) = ParsedAst.Declaration.Relation(
			visitTscomment(ctx.tscomment().toList.toSeq),
			visitStartSp(ctx.REL().getSymbol()),
			visitTableName(ctx.tableName()),
			if (ctx.attributes()!=null) visitAttributes(ctx.attributes()) else Seq(),
			visitStopSp(ctx.getStop())
		)
	
	def visitDecls_lattice(ctx: FlixParser.Decls_latticeContext) = ParsedAst.Declaration.Lattice(
			visitTscomment(ctx.tscomment().toList.toSeq),
			visitStartSp(ctx.LAT().getSymbol()),
			visitTableName(ctx.tableName()),
			if (ctx.attributes()!=null) visitAttributes(ctx.attributes()) else Seq(),
			visitStopSp(ctx.getStop())
		)
	
	def visitDecls_index(ctx: FlixParser.Decls_indexContext) = ParsedAst.Declaration.Index(
			visitStartSp(ctx.INDEX().getSymbol()),
			visitQualifiedTableName(ctx.qualifiedTableName()),
			if (ctx.indexes()!=null) visitIndexes(ctx.indexes()) else Seq(),
			visitStopSp(ctx.getStop())
		)
	
	def visitDecls_signature(ctx: FlixParser.Decls_signatureContext) = ParsedAst.Declaration.Signature(
			visitTscomment(ctx.tscomment().toList.toSeq),
			visitStartSp(ctx.DEF().getSymbol()),
			visitDefinitionName(ctx.definitionName()),
			visitFormalparams(ctx.formalparams),
			visitType(ctx.`type`()),
			visitStopSp(ctx.getStop())
		)
	
	def visitDecls_external(ctx: FlixParser.Decls_externalContext) = ParsedAst.Declaration.External(
			visitTscomment(ctx.tscomment().toList.toSeq),
			visitStartSp(ctx.EXTERNAL().getSymbol()),
			visitDefinitionName(ctx.definitionName()),
			visitFormalparams(ctx.formalparams),
			visitType(ctx.`type`()),
			visitStopSp(ctx.getStop())
		)
	
	def visitDecls_definition(ctx: FlixParser.Decls_definitionContext) = ParsedAst.Declaration.Definition(
			visitTscomment(ctx.tscomment().toList.toSeq),
			if (ctx.annotations()!=null) visitAnnotations(ctx.annotations()) else Seq(),
			visitStartSp(ctx.DEF().getSymbol()),
			visitDefinitionName(ctx.definitionName()),
			visitTypeparams(ctx.typeparams),
			visitFormalparams(ctx.formalparams),
			visitType(ctx.`type`()),
			visitExpression(ctx.expression()),
			visitStopSp(ctx.getStop())
		)
	
	def visitDecls_law(ctx: FlixParser.Decls_lawContext) = ParsedAst.Declaration.Law(
			visitTscomment(ctx.tscomment().toList.toSeq),
			visitStartSp(ctx.LAW().getSymbol()),
			visitDefinitionName(ctx.definitionName()),
			visitTypeparams(ctx.typeparams),
			visitFormalparams(ctx.formalparams),
			visitType(ctx.`type`()),
			visitExpression(ctx.expression()),
			visitStopSp(ctx.getStop())
		)
	
	def visitDecls_class(ctx: FlixParser.Decls_classContext);
	
	def visitClass_body(ctx: FlixParser.Class_bodyContext);
	
	def visitDecls_fact(ctx: FlixParser.Decls_factContext) = ParsedAst.Declaration.Fact(
			visitStartSp(ctx.getStart()),
			visitPredicate(ctx.predicate()),
			visitStopSp(ctx.getStop())
		)
	
	def visitDecls_rule(ctx: FlixParser.Decls_ruleContext) = ParsedAst.Declaration.Rule(
			visitStartSp(ctx.getStart()),
			visitPredicate(ctx.predicate()),
			visitPredicates(ctx.predicates()),
			visitStopSp(ctx.getStop())
		)
	
	def visitElms(ctx: FlixParser.ElmsContext) = visitExpressions(ctx.expressions())
	
	def visitDecls_letlattice(ctx: FlixParser.Decls_letlatticeContext) = ParsedAst.Declaration.BoundedLattice(
			visitStartSp(ctx.getStart()),
			visitType(ctx.`type`()),
			visitElms(ctx.elms()),
			visitStopSp(ctx.getStop())
		)
	
	def visitDecls_impl(ctx: FlixParser.Decls_implContext);





	//===========
	//EXPRESSIONS
	//===========


	def visitExpression(ctx: FlixParser.ExpressionContext) = ctx.logical() match {
			case  null => visitExpression(ctx.expression())
			case x : FlixParser.LogicalContext => visitLogical(x);
		}

	def visitExpressions(ctx: FlixParser.ExpressionsContext) = ctx.expression().map(visitExpression).toList.toSeq

	def visitLogical(ctx: FlixParser.LogicalContext) = ctx.getChildCount match {
			case 1 => visitComparison(ctx.comparison(0))
			case _ => ParsedAst.Expression.Binary(
					visitComparison(ctx.comparison(0)),
					visitLogical_ops(ctx.logical_ops()),
					visitComparison(ctx.comparison(1)),
					visitStopSp(ctx.getStop())
				)
		}

	def visitComparison(ctx: FlixParser.ComparisonContext) = ctx.getChildCount match {
			case 1 => visitAdditive(ctx.additive(0))
			case _ => ParsedAst.Expression.Binary(
					visitAdditive(ctx.additive(0)),
					visitComparison_ops(ctx.comparison_ops()),
					visitAdditive(ctx.additive(1)),
					visitStopSp(ctx.getStop())
				)
		}

	//REVISIT for more than 2 multiplicatives
	def visitAdditive(ctx: FlixParser.AdditiveContext) = ctx.getChildCount match {
			case 1 => visitMultiplicative(ctx.multiplicative(0))
			case _ => ParsedAst.Expression.Binary(
					visitMultiplicative(ctx.multiplicative(0)),
					visitAddve_ops(ctx.addve_ops()),
					visitMultiplicative(ctx.multiplicative(1)),
					visitStopSp(ctx.getStop())
				)
		}

	//REVISIT for more than 2 infixes
	def visitMultiplicative(ctx: FlixParser.MultiplicativeContext)  = ctx.getChildCount match {
			case 1 => visitInfix(ctx.infix(0))
			case _ => ParsedAst.Expression.Binary(
					visitInfix(ctx.infix(0)),
					visitMultipve_ops(ctx.multipve_ops()),
					visitInfix(ctx.infix(1)),
					visitStopSp(ctx.getStop())
				)
		}

	def visitInfix(ctx: FlixParser.InfixContext) = ctx.getChildCount match {
			case 1 => visitExtended(ctx.extended(0))
			case _ => ParsedAst.Expression.Infix(
					visitExtended(ctx.extended(0)),
					visitQualifiedDefinitionName(ctx.qualifiedDefinitionName()),
					visitExtended(ctx.extended(1)),
					visitStopSp(ctx.getStop())
				)
		}

	def visitExtended(ctx: FlixParser.ExtendedContext) = ctx.getChildCount match {
			case 1 => visitUnary(ctx.unary(0))
			case _ => ParsedAst.Expression.ExtendedBinary(
					visitUnary(ctx.unary(0)),
					visitExtbin_ops(ctx.extbin_ops()),
					visitUnary(ctx.unary(1)),
					visitStopSp(ctx.getStop())
				)
		}

	def visitUnary(ctx: FlixParser.UnaryContext) = ctx.getChildCount match {
			case 1 => visitAscribe(ctx.ascribe(0))
			case _ => ParsedAst.Expression.Unary(
					visitUnary_ops(ctx.unary_ops()),
					visitUnary(ctx.unary()),
					visitStopSp(ctx.getStop())
				)
		}

	def visitAscribe(ctx: FlixParser.AscribeContext) = ctx.getChildCount match {
			case 1 => visitE_fList(ctx.e_fList())
			case _ => ParsedAst.Expression.Ascribe(
					visitE_fList(ctx.e_fList()),
					visitType(ctx.`type`()),
					visitStopSp(ctx.getStop())
				)
		}

	def visitE_primary(ctx: FlixParser.E_primaryContext)  = ctx.getChild(0) match {
			case x: FlixParser.E_letMatchContext => visitE_letMatch(x)
			case x: FlixParser.E_ifThenElseContext => visitE_ifThenElse(x)
			case x: FlixParser.E_matchContext => visitE_match(x)
			case x: FlixParser.E_switchContext => visitE_switch(x)
			case x: FlixParser.E_tagContext => visitE_tag(x)
			case x: FlixParser.E_lambdaContext => visitE_lambda(x)
			case x: FlixParser.E_tupleContext => visitE_tuple(x)
			case x: FlixParser.E_fNilContext => visitE_fNil(x)
			case x: FlixParser.E_fVecContext => visitE_fVec(x)
			case x: FlixParser.E_fSetContext => visitE_fSet(x)
			case x: FlixParser.E_fMapContext => visitE_fMap(x)
			case x: FlixParser.LiteralContext => visitLiteral(x)
			case x: FlixParser.ExistentialContext => visitExistential(x)
			case x: FlixParser.UniversalContext => visitUniversal(x)
			case x: FlixParser.E_qnameContext => visitE_qname(x)
			case x: FlixParser.E_unaryLambdaContext => visitE_unaryLambda(x)
			case x: FlixParser.E_wildContext => visitE_wild(x)
			case x: FlixParser.E_snameContext => visitE_sname(x)
			case x: FlixParser.E_userErrorContext => visitE_userError(x)
		}

	def visitE_letMatch(ctx: FlixParser.E_letMatchContext) = ParsedAst.Expression.LetMatch(
			visitStartSp(ctx.getStart()),
			visitPattern(ctx.pattern()),
			visitExpression(ctx.expression(0)),
			visitExpression(ctx.expression(1)),
			visitStopSp(ctx.getStop())
		)
	def visitE_ifThenElse(ctx: FlixParser.E_ifThenElseContext) = ParsedAst.Expression.IfThenElse(
			visitStartSp(ctx.getStart()),
			visitExpression(ctx.expression(0)),
			visitExpression(ctx.expression(1)),
			visitExpression(ctx.expression(2)),
			visitStopSp(ctx.getStop())
		)

	def visitMatch_rule(ctx: FlixParser.Match_ruleContext) = (
			visitPattern(ctx.pattern()),
			visitExpression(ctx.expression())
		)

	def visitMatch_rules(ctx: FlixParser.Match_rulesContext) = ctx.match_rule().map(visitMatch_rule).toList.toSeq

	def visitE_match(ctx: FlixParser.E_matchContext) = ParsedAst.Expression.Match(
			visitStartSp(ctx.getStart()),
			visitExpression(ctx.expression(0)),
			visitMatch_rules(ctx.match_rules()),
			visitStopSp(ctx.getStop())
		)

	def visitSwitch_rule(ctx: FlixParser.Switch_ruleContext) = (
			visitExpression(ctx.expression(0)),
			visitExpression(ctx.expression(1))
		)

	def visitSwitch_rules(ctx: FlixParser.Switch_rulesContext) = ctx.switch_rule().map(visitSwitch_rule).toList.toSeq

	def visitE_switch(ctx: FlixParser.E_switchContext) = ParsedAst.Expression.Switch(
			visitStartSp(ctx.getStart()),
			visitSwitch_rules(ctx.switch_rules()),
			visitStopSp(ctx.getStop())
		)
	def visitE_apply(ctx: FlixParser.E_applyContext) = ParsedAst.Expression.Apply(
			visitE_primary(ctx.e_primary()),
			if (ctx.expressions()!=null) visitExpressions(ctx.expressions()) else Seq(),
			visitStopSp(ctx.getStop())
		)

	def visitE_sname(ctx: FlixParser.E_snameContext) = ParsedAst.Expression.SName(
			visitStartSp(ctx.getStart()),
			visitVariableName(ctx.variableName()),
			visitStopSp(ctx.getStop())
		)

	def visitE_qname(ctx: FlixParser.E_qnameContext) = ParsedAst.Expression.QName(
			visitStartSp(ctx.getStart()),
			visitQualifiedDefinitionName(ctx.qualifiedDefinitionName()),
			visitStopSp(ctx.getStop())
		)

	def visitE_tag(ctx: FlixParser.E_tagContext)  = ParsedAst.Expression.Tag(
			visitStartSp(ctx.getStart()),

			if(ctx.qualifiedTypeName()!=null)
				Option(visitQualifiedTypeName(ctx.qualifiedTypeName()))
			else Option(null),

			visitTagName(ctx.tagName()),

			if(ctx.e_tuple()!=null)
				Option(visitE_tuple(ctx.e_tuple()))
			else Option(null),

			visitStopSp(ctx.getStop())
		)

	def visitE_tuple(ctx: FlixParser.E_tupleContext) = ParsedAst.Expression.Tuple(
			visitStartSp(ctx.getStart()),

			if(ctx.expressions()!=null) visitExpressions(ctx.expressions())
			else Seq(),
			
			visitStopSp(ctx.getStop())
		)

	def visitE_keyValue(ctx: FlixParser.E_keyValueContext) = ( visitExpression(ctx.expression(0)),visitExpression(ctx.expression(1)) );

	def visitE_keyValues(ctx: FlixParser.E_keyValuesContext) = ctx.e_keyValue().map(visitE_keyValue).toList.toSeq

	def visitE_userError(ctx: FlixParser.E_userErrorContext) = ParsedAst.Expression.UserError( visitStartSp(ctx.getStart()),visitStopSp(ctx.getStop()) )

	def visitE_wild(ctx: FlixParser.E_wildContext) = ParsedAst.Expression.Wild( visitStartSp(ctx.getStart()),visitStopSp(ctx.getStop()) )

	def visitE_fNil(ctx: FlixParser.E_fNilContext) = ParsedAst.Expression.FNil( visitStartSp(ctx.getStart()),visitStopSp(ctx.getStop()) )

	def visitE_fList(ctx: FlixParser.E_fListContext) = 
		if ( ctx.expression() != null )
			ParsedAst.Expression.FList(
				visitE_apply(ctx.e_apply()),
				visitExpression(ctx.expression()),
				visitStopSp(ctx.getStop())
			)
		else visitE_apply(ctx.e_apply())

	def visitE_fVec(ctx: FlixParser.E_fVecContext) = ParsedAst.Expression.FVec(
			visitStartSp(ctx.getStart()),
			if (ctx.expressions() != null) visitExpressions(ctx.expressions())
			else Seq(),
			visitStopSp(ctx.getStop())
		)

	def visitE_fSet(ctx: FlixParser.E_fSetContext) = ParsedAst.Expression.FSet(
			visitStartSp(ctx.getStart()),
			if (ctx.expressions() != null) visitExpressions(ctx.expressions())
			else Seq(),
			visitStopSp(ctx.getStop())
		)
	def visitE_fMap(ctx: FlixParser.E_fMapContext) = ParsedAst.Expression.FMap(
			visitStartSp(ctx.getStart()),
			if (ctx.e_keyValues() != null) visitE_keyValues(ctx.e_keyValues())
			else Seq(),
			visitStopSp(ctx.getStop())
		)

	def visitE_unaryLambda(ctx: FlixParser.E_unaryLambdaContext) = ParsedAst.Expression.Lambda(
			visitStartSp(ctx.getStart()),
			Seq( visitVariableName(ctx.variableName()) ),
			visitExpression(ctx.expression()),
			visitStopSp(ctx.getStop())
		)

	def visitE_lambda(ctx: FlixParser.E_lambdaContext) = ParsedAst.Expression.Lambda(
			visitStartSp(ctx.getStart()),
			visitVariableNames(ctx.variableNames()),
			visitExpression(ctx.expression()),
			visitStopSp(ctx.getStop())
		)

	def visitExistential(ctx: FlixParser.ExistentialContext) = ParsedAst.Expression.Existential(
			visitStartSp(ctx.getStart()),
			visitFormalparams(ctx.formalparams()),
			visitExpression(ctx.expression()),
			visitStopSp(ctx.getStop())
		)

	def visitUniversal(ctx: FlixParser.UniversalContext) = ParsedAst.Expression.Universal(
			visitStartSp(ctx.getStart()),
			visitFormalparams(ctx.formalparams()),
			visitExpression(ctx.expression()),
			visitStopSp(ctx.getStop())
		)


	//========
	//PATTERNS
	//========


	def visitPattern(ctx: FlixParser.PatternContext) = visitP_fList(ctx.p_fList())

	def visitPatterns(ctx: FlixParser.PatternsContext) = ctx.pattern().map(visitPattern).toList.toSeq

	def visitSimple(ctx: FlixParser.SimpleContext)  = ctx.getChild(0) match {
			case x: FlixParser.P_fNil => visitP_fNil(x)
			case x: FlixParser.Literal => visitLiteral(x)
			case x: FlixParser.P_variable => visitP_variable(x)
			case x: FlixParser.WILD => visitWILD(x)
			case x: FlixParser.P_tag => visitP_tag(x)
			case x: FlixParser.P_tuple => visitP_tuple(x)
			case x: FlixParser.P_fVec => visitP_fVec(x)
			case x: FlixParser.P_fSet => visitP_fSet(x)
			case x: FlixParser.P_fMap => visitP_fMap(x)
		}

	def visitP_keyValue(ctx: FlixParser.P_keyValueContext) = ( visitPattern(ctx.pattern(0)),visitPattern(ctx.pattern(1)) );

	def visitP_keyValues(ctx: FlixParser.P_keyValuesContext) = ctx.p_keyValue().map(visitP_keyValue).toList.toSeq

	def visitP_tag(ctx: FlixParser.P_tagContext) = ParsedAst.Pattern.Tag(
			visitStartSp(ctx.getStart()),

			if(ctx.qualifiedTypeName()!=null)
				Option( visitQualifiedTypeName(ctx.qualifiedTypeName()) )
			else Option(null),

			visitTagName(ctx.tagName()),

			if(ctx.pattern()!=null)
				Option( visitPattern(ctx.pattern()) )
			else Option(null),

			visitStopSp(ctx.getStop())
		)

	def visitP_tuple(ctx: FlixParser.P_tupleContext) = ParsedAst.Pattern.Tuple(
			visitStartSp(ctx.getStart()),

			if( ctx.patterns()!=null ) visitPatterns(ctx.patterns())
			else Seq(),
			
			visitStopSp(ctx.getStop())
		)

	def visitP_wild(ctx: FlixParser.P_wildContext) = ParsedAst.Pattern.Wild( visitStartSp(ctx.getStart()),visitStopSp(ctx.getStop()) )

	def visitP_fNil(ctx: FlixParser.P_fNilContext) = ParsedAst.Pattern.FNil( visitStartSp(ctx.getStart()),visitStopSp(ctx.getStop()) )

	def visitP_variable(ctx: FlixParser.P_variableContext) = visitVariableName(ctx.variableName())

	def visitP_fList(ctx: FlixParser.P_fListContext) = 
		if ( ctx.expression() != null )
			ParsedAst.Pattern.FList(
				visitSimple(ctx.simple()),
				visitPattern(ctx.pattern()),
				visitStopSp(ctx.getStop())
			)
		else visitSimple(ctx.simple())

	def visitP_fVec(ctx: FlixParser.P_fVecContext) = ParsedAst.Pattern.FVec(
			visitStartSp(ctx.getStart()),
			if (ctx.patterns() != null) visitPatterns(ctx.patterns())
			else Seq(),
			if( ctx.pattern() != null ) Option( visitPattern(ctx.pattern()) )
			else Option(null),
			visitStopSp(ctx.getStop())
		)

	def visitP_fSet(ctx: FlixParser.P_fSetContext) = ParsedAst.Pattern.FSet(
			visitStartSp(ctx.getStart()),
			if (ctx.patterns() != null) visitPatterns(ctx.patterns())
			else Seq(),
			if( ctx.pattern() != null ) Option( visitPattern(ctx.pattern()) )
			else Option(null),
			visitStopSp(ctx.getStop())
		)

	def visitP_fMap(ctx: FlixParser.P_fMapContext) = ParsedAst.Pattern.FMap(
			visitStartSp(ctx.getStart()),
			if (ctx.p_keyValues() != null) visitP_keyValues(ctx.p_keyValues())
			else Seq(),
			if( ctx.pattern() != null ) Option( visitPattern(ctx.pattern()) )
			else Option(null),
			visitStopSp(ctx.getStop())
		)



	//=====
	//TYPES
	//=====
	
	def visitPrimary(ctx: FlixParser.PrimaryContext) = ctx.getChild(0) match {
			case x: FlixParser.ArrowContext => visitArrow(x)
			case x: FlixParser.TupleContext => visitTuple(x)
			case x: FlixParser.ApplyContext => visitApply(x)
			case x: FlixParser.VarContext => visitVar(x)
			case x: FlixParser.RefContext => visitRef(x)
		}

	def visitArrow(ctx: FlixParser.ArrowContext) = ParsedAst.Type.Arrow(
			visitStartSp(ctx.getStart()),
			ctx.`type`().dropRight(1).map(visitType).toList.toSeq,
			visitType(ctx.`type`().last),
			visitStopSp(ctx.getStop())
		)

	def visitTuple_unit(ctx: FlixParser.Tuple_unitContext) = ParsedAst.Type.Unit(
			visitStartSp(ctx.getStart()),
			visitStopSp(ctx.getStop())
		)

	def visitTuple_singleton(ctx: FlixParser.Tuple_singletonContext) =ParsedAst.Type.Tuple(
			visitStartSp(ctx.getStart()),
			Seq( visitType(ctx.`type`()) ),
			visitStopSp(ctx.getStop())
		)

	def visitTuple_multi(ctx: FlixParser.Tuple_multiContext) = ParsedAst.Type.Tuple(
			visitStartSp(ctx.getStart()),
			ctx.`type`().map(visitType).toList.toSeq,
			visitStopSp(ctx.getStop())
		)
	def visitTuple(ctx: FlixParser.TupleContext) = ctx.getChild(0) match{
			case x: FlixParser.Tuple_unitContext => visitTuple_unit(x)
			case x: FlixParser.Tuple_singletonContext => visitTuple_singleton(x)
			case x: FlixParser.Tuple_multiContext => visitTuple_multi(x)
		}
	
	def visitApply(ctx: FlixParser.ApplyContext) = ParsedAst.Type.Apply(
			visitStartSp(ctx.getStart()),
			visitRef(ctx.ref()),
			ctx.`type`().map(visitType).toList.toSeq,
			visitStopSp(ctx.getStop())
		)

	def visitVar(ctx: FlixParser.VarContext) = ParsedAst.Type.Var(
			visitStartSp(ctx.getStart()),
			visitVariableName(ctx.variableName()),
			visitStopSp(ctx.getStop())
		)

	def visitRef(ctx: FlixParser.RefContext) = ParsedAst.Type.Ref(
			visitStartSp(ctx.getStart()),
			visitQualifiedTypeName(ctx.qualifiedTypeName()),
			visitStopSp(ctx.getStop())
		)

	def visitType(ctx: FlixParser.TypeContext) : ParsedAst.Type = ctx.`type`() match {
			case  null => visitPrimary(ctx.primary())
			case x : FlixParser.TypeContext => ParsedAst.Type.Arrow(
				visitStartSp(ctx.getStart()),
				Seq(visitPrimary(ctx.primary)),
				visitType(x),
				visitStopSp(ctx.getStop())
			)
		}


	//=========
	//OPERATORS
	//=========


	def visitUnary_ops(ctx: FlixParser.Unary_opsContext) = ctx.getText() match {
			case "+" => UnaryOperator.Plus
			case "-" => UnaryOperator.Minus
			case "~" => UnaryOperator.BitwiseNegate
			case "¬" => UnaryOperator.LogicalNot
			case "!" => UnaryOperator.LogicalNot
		}

	def visitLogical_ops(ctx: FlixParser.Logical_opsContext) = ctx.getText() match {
			case "&&" => BinaryOperator.LogicalAnd
			case "||" => BinaryOperator.LogicalOr
			case "&" => BinaryOperator.BitwiseAnd
			case "|" => BinaryOperator.BitwiseOr
			case "==>" => BinaryOperator.Implication
			case "<==>" => BinaryOperator.Biconditional
			case "^" => BinaryOperator.BitwiseXor
			case "<<" => BinaryOperator.BitwiseLeftShift
			case ">>" => BinaryOperator.BitwiseRightShift
			case "∧" => BinaryOperator.LogicalAnd
			case "∨" => BinaryOperator.LogicalOr
			case "→" => BinaryOperator.Implication
			case "↔" => BinaryOperator.Biconditional
		}

	def visitComparison_ops(ctx: FlixParser.Comparison_opsContext) = ctx.getText() match {
			case "<=" => BinaryOperator.LessEqual
			case ">=" => BinaryOperator.GreaterEqual
			case "<" => BinaryOperator.Less
			case ">" => BinaryOperator.Greater
			case "==" => BinaryOperator.Equal
			case "!=" => BinaryOperator.NotEqual
			case "≡" => BinaryOperator.Equal
		}

	def visitMultipve_ops(ctx: FlixParser.Multipve_opsContext) = ctx.getText() match {
			case "**" => BinaryOperator.Exponentiate
			case "*" => BinaryOperator.Times
			case "/" => BinaryOperator.Divide
			case "%" => BinaryOperator.Modulo
		}

	def visitAddve_ops(ctx: FlixParser.Addve_opsContext) = ctx.getText() match {
			case "+" => BinaryOperator.Plus
			case "-" => BinaryOperator.Minus
		}

	def visitExtbin_ops(ctx: FlixParser.Extbin_opsContext) = ctx.getText() match {
			case "⊑" => ExtBinaryOperator.Leq
			case "⊔" => ExtBinaryOperator.Lub
			case "⊓" => ExtBinaryOperator.Glb
			case "▽" => ExtBinaryOperator.Widen
			case "△" => ExtBinaryOperator.Narrow
		}

	//==========
	//PREDICATES
	//==========
	def visitPredicate(ctx: FlixParser.PredicateContext) = ctx.getChild(0) match{
			case x: FlixParser.Pred_trueContext => visitPred_true(x)
			case x: FlixParser.Pred_falseContext => visitPred_false(x)
			case x: FlixParser.Pred_filterContext => visitPred_filter(x)
			case x: FlixParser.Pred_tableContext => visitPred_table(x)
			case x: FlixParser.Pred_notequalContext => visitPred_notequal(x)
			case x: FlixParser.Pred_loopContext => visitPred_loop(x)
		}

	def visitPredicates(ctx: FlixParser.PredicatesContext) = ctx.predicate().map(visitPredicate).toList.toSeq

	def visitPred_true(ctx: FlixParser.Pred_trueContext) = ParsedAst.Predicate.True(visitStartSp(ctx.getStart()),visitStopSp(ctx.getStop()))

	def visitPred_false(ctx: FlixParser.Pred_falseContext) = ParsedAst.Predicate.False(visitStartSp(ctx.getStart()),visitStopSp(ctx.getStop()))

	def visitPred_filter(ctx: FlixParser.Pred_filterContext) = ParsedAst.Predicate.Filter(
			visitStartSp(ctx.getStart()),
			visitQualifiedDefinitionName(ctx.qualifiedDefinitionName()),
			visitExpressions(ctx.expressions()),
			visitStopSp(ctx.getStop())
		)

	def visitPred_table(ctx: FlixParser.Pred_tableContext) = ParsedAst.Predicate.Table(
			visitStartSp(ctx.getStart()),
			visitQualifiedTableName(ctx.qualifiedTableName()),
			visitExpressions(ctx.expressions()),
			visitStopSp(ctx.getStop())
		)

	def visitPred_notequal(ctx: FlixParser.Pred_notequalContext) = ParsedAst.Predicate.NotEqual(
			visitStartSp(ctx.getStart()),
			visitVariableName(ctx.variableName(0)),
			visitVariableName(ctx.variableName(1)),
			visitStopSp(ctx.getStop())
		)

	def visitPred_loop(ctx: FlixParser.Pred_loopContext) = ParsedAst.Predicate.Loop(
			visitStartSp(ctx.getStart()),
			visitVariableName(ctx.variableName()),
			visitExpression(ctx.expression()),
			visitStopSp(ctx.getStop())
		)


}//class AST_FlixVisitor