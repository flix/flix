package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.{ParsedAst, _}
import scala.collection.immutable.Seq;
import collection.JavaConversions._;
import org.antlr.v4.runtime.{Token,ParserRuleContext};
import org.antlr.v4.runtime.tree.{ParseTree,RuleNode,ErrorNode,TerminalNode};

class AST_FlixVisitor(source: SourceInput, input: String) extends FlixVisitor[Object]{

	//Satisfy visitor interface.
	def visitWs(ctx: FlixParser.WsContext) = None
	def visitNegative(ctx: FlixParser.NegativeContext) = None
	def visitOptSC(ctx: FlixParser.OptSCContext) = None
	def visit(pt: ParseTree) = None
	def visitChildren(rn: RuleNode) = None
	def visitErrorNode(en: ErrorNode) = None
	def visitTerminal(tn: TerminalNode) = None

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

	def visitTscomment(ctx: FlixParser.TscommentContext) = ctx.TripleSlashComment().getText().trim.substring(3)

	def visitTscommentHelper(ctxs: Vector[FlixParser.TscommentContext]) = 
		if (ctxs.isEmpty) Option(null)
		else Option(ParsedAst.Documentation(
			visitStartSp(ctxs.head.getStart()),
			ctxs.map(visitTscomment),
			visitStopSp(ctxs.last.getStop())
		))

	//=========
	//ROOT NODE
	//=========
	def visitStart(ctx: FlixParser.StartContext): ParsedAst.Root = ParsedAst.Root(
			if ( ctx.s_import() != null) ctx.s_import().map(visitS_import).toList.toVector else Vector(),
			if ( ctx.decl() != null) ctx.decl().map(visitDecl).toList.toVector else Vector()
		)
	//==========
	//NAME RULES
	//==========

	def visitIdent(ctx: FlixParser.IdentContext) =  Name.Ident(
			visitStartSp(ctx.getStart()),
			ctx.getChild(0).getText(),
			visitStopSp(ctx.getStop())
		)

	def visitUpperLowerIdent(tk: Token): Name.Ident = {
		return Name.Ident(
			visitStartSp(tk),
			tk.getText(),
			visitStopSp(tk)
		);}

	def visitNname(ctx: FlixParser.NnameContext) = Name.NName(
			visitStartSp(ctx.getStart()),
			ctx.ident().map(visitIdent).toList,
			visitStopSp(ctx.getStop())
		)

	def visitLowerqname(ctx: FlixParser.LowerqnameContext) : Name.QName = ctx.nname() match {
			case null => Name.QName(
				visitStartSp(ctx.getStart()),
				Name.RootNS,
				visitUpperLowerIdent(ctx.getStop()),
				visitStopSp(ctx.getStop())
			)
			case x: FlixParser.NnameContext =>Name.QName(
				visitStartSp(ctx.getStart()),
				visitNname(ctx.nname()),
				visitUpperLowerIdent(ctx.getStop()),
				visitStopSp(ctx.getStop())
			)
		}
	def visitUpperqname(ctx: FlixParser.UpperqnameContext) : Name.QName = ctx.nname() match {
			case null => Name.QName(
				visitStartSp(ctx.getStart()),
				Name.RootNS,
				visitUpperLowerIdent(ctx.getStop()),
				visitStopSp(ctx.getStop())
			)
			case x: FlixParser.NnameContext =>Name.QName(
				visitStartSp(ctx.getStart()),
				visitNname(ctx.nname()),
				visitUpperLowerIdent(ctx.getStop()),
				visitStopSp(ctx.getStop())
			)
		}
		
	def visitAnnotationName(ctx: FlixParser.AnnotationNameContext) = visitUpperLowerIdent(ctx.getStop())
	def visitAttributeName(ctx: FlixParser.AttributeNameContext) = visitUpperLowerIdent(ctx.getStop())
	def visitClassName(ctx: FlixParser.ClassNameContext) = visitUpperLowerIdent(ctx.getStop())
	def visitDefinitionName(ctx: FlixParser.DefinitionNameContext) = visitUpperLowerIdent(ctx.getStop())
	def visitQualifiedDefinitionName(ctx: FlixParser.QualifiedDefinitionNameContext) = visitLowerqname(ctx.lowerqname());
	def visitTableName(ctx: FlixParser.TableNameContext) = visitUpperLowerIdent(ctx.getStop())
	def visitQualifiedTableName(ctx: FlixParser.QualifiedTableNameContext) = visitUpperqname(ctx.upperqname());
	def visitTagName(ctx: FlixParser.TagNameContext) = visitUpperLowerIdent(ctx.getStop())
	def visitTypeName(ctx: FlixParser.TypeNameContext) = visitUpperLowerIdent(ctx.getStop())
	def visitQualifiedTypeName(ctx: FlixParser.QualifiedTypeNameContext) = visitUpperqname(ctx.upperqname())
	def visitVariableName(ctx: FlixParser.VariableNameContext) = visitUpperLowerIdent(ctx.getStop())

	//===============
	//RULES FOR LISTS
	//===============

	def visitVariableNames(ctx: FlixParser.VariableNamesContext) : Vector[Name.Ident] = if ( ctx.variableName() != null) ctx.variableName().map(visitVariableName).toList.toVector else Vector()

	def visitArgument(ctx : FlixParser.ArgumentContext) = ParsedAst.FormalParam(
			visitStartSp(ctx.getStart()),
			visitVariableName(ctx.variableName()),
			visitType(ctx.`type`()),
			visitStopSp(ctx.getStop())
		)

	def visitArguments(ctx : FlixParser.ArgumentsContext) : Vector[ParsedAst.FormalParam] = if ( ctx.argument() != null) ctx.argument().map(visitArgument).toList.toVector else Vector()

	def visitFormalparams(ctx : FlixParser.FormalparamsContext) : Option[Vector[ParsedAst.FormalParam]] = ctx.getChildCount() match{
			case 0 => Option(null)
			case _ => ctx.arguments() match{
				case null => Option(Vector())
				case x: FlixParser.ArgumentsContext => Option(visitArguments(x))
			}
		}
		

	def visitIndex(ctx: FlixParser.IndexContext) : Vector[Name.Ident] = if ( ctx.attributeName() != null) ctx.attributeName().map(visitAttributeName).toList.toVector else Vector()
	def visitIndexes(ctx: FlixParser.IndexesContext) : Vector[Vector[Name.Ident]] = if ( ctx.index() != null) ctx.index().map(visitIndex).toList.toVector else Vector()
	

	def visitAttribute(ctx: FlixParser.AttributeContext) = ParsedAst.Attribute(
			visitStartSp(ctx.getStart()),
			visitAttributeName(ctx.attributeName()),
			visitType(ctx.`type`()),
			visitStopSp(ctx.getStop())
		)
	def visitAttributes(ctx: FlixParser.AttributesContext) : Vector[ParsedAst.Attribute] = if ( ctx.attribute() != null) ctx.attribute().map(visitAttribute).toList.toVector else Vector()


	def visitContextBound(ctx: FlixParser.ContextBoundContext) = ParsedAst.ContextBound(
			visitStartSp(ctx.getStart()),
			visitClassName(ctx.className()),
			visitClass_typeparams(ctx.class_typeparams()),
			visitStopSp(ctx.getStop())
		)

	def visitContextBounds(ctx: FlixParser.ContextBoundsContext) : Vector[ParsedAst.ContextBound] = if ( ctx.contextBound() != null) ctx.contextBound().map(visitContextBound).toList.toVector else Vector()

	def visitContextBoundsList(ctx: FlixParser.ContextBoundsListContext) : Vector[ParsedAst.ContextBound] = 
		if ( ctx.contextBounds() != null )
			visitContextBounds(ctx.contextBounds())
		else Vector()

	def visitImplContextBoundsList(ctx: FlixParser.ImplContextBoundsListContext) : Vector[ParsedAst.ContextBound] = 
		if ( ctx.contextBounds() != null )
			visitContextBounds(ctx.contextBounds())
		else Vector()

	def visitClass_typeparams(ctx: FlixParser.Class_typeparamsContext) = ctx.`type`().map(visitType).toList.toVector

	def visitTypeparam(ctx: FlixParser.TypeparamContext) : ParsedAst.ContextBound = ctx.`type`() match {
			case null => ParsedAst.ContextBound(
				visitStartSp(ctx.getStart()),
				visitVariableName(ctx.variableName()),
				Vector(),
				visitStopSp(ctx.getStop())
				)
			case x: FlixParser.TypeContext => ParsedAst.ContextBound(
				visitStartSp(ctx.getStart()),
				visitVariableName(ctx.variableName()),
				Seq(visitType(x)),
				visitStopSp(ctx.getStop())
				)
		}

	def visitTypeparams(ctx: FlixParser.TypeparamsContext) : Vector[ParsedAst.ContextBound] = if ( ctx.typeparam() != null) ctx.typeparam().map(visitTypeparam).toList.toVector else Vector()

	def visitDcase(ctx: FlixParser.DcaseContext) : ParsedAst.Case = ctx.tuple() match {
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
	def visitDcases(ctx: FlixParser.DcasesContext) : Vector[ParsedAst.Case] = if ( ctx.dcase() != null) ctx.dcase().map(visitDcase).toList.toVector else Vector()

	def visitAnnotation(ctx: FlixParser.AnnotationContext) = ParsedAst.Annotation(
			visitStartSp(ctx.getStart()),
			visitAnnotationName(ctx.annotationName()),
			visitStopSp(ctx.getStop())
		)

	def visitAnnotations(ctx: FlixParser.AnnotationsContext) : Vector[ParsedAst.Annotation] = if ( ctx.annotation() != null) ctx.annotation().map(visitAnnotation).toList.toVector else Vector()
	
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
			case x: FlixParser.Decls_implContext => visitDecls_impl(x)
			case x: FlixParser.Decls_letlatticeContext => visitDecls_letlattice(x)
		}

	def visitDecls_namespace(ctx: FlixParser.Decls_namespaceContext) = ParsedAst.Declaration.Namespace(
			visitStartSp(ctx.getStart()),
			visitNname(ctx.nname()),
			if ( ctx.decl() != null) ctx.decl().map(visitDecl).toList.toVector else Vector(),
			visitStopSp(ctx.getStop())
		)
	
	def visitDecls_enum(ctx: FlixParser.Decls_enumContext) = ParsedAst.Declaration.Enum(
			visitTscommentHelper(ctx.tscomment().toList.toVector),
			visitStartSp(ctx.ENUM().getSymbol()),
			visitTypeName(ctx.typeName()),
			visitTypeparams(ctx.typeparams()),
			visitDcases(ctx.dcases()),
			visitStopSp(ctx.getStop())
		)
	
	def visitDecls_relation(ctx: FlixParser.Decls_relationContext) = ParsedAst.Declaration.Relation(
			visitTscommentHelper(ctx.tscomment().toList.toVector),
			visitStartSp(ctx.REL().getSymbol()),
			visitTableName(ctx.tableName()),
			if (ctx.attributes()!=null) visitAttributes(ctx.attributes()) else Vector(),
			visitStopSp(ctx.getStop())
		)
	
	def visitDecls_lattice(ctx: FlixParser.Decls_latticeContext) = ParsedAst.Declaration.Lattice(
			visitTscommentHelper(ctx.tscomment().toList.toVector),
			visitStartSp(ctx.LAT().getSymbol()),
			visitTableName(ctx.tableName()),
			if (ctx.attributes()!=null) visitAttributes(ctx.attributes()) else Vector(),
			visitStopSp(ctx.getStop())
		)
	
	def visitDecls_index(ctx: FlixParser.Decls_indexContext) = ParsedAst.Declaration.Index(
			visitStartSp(ctx.INDEX().getSymbol()),
			visitQualifiedTableName(ctx.qualifiedTableName()),
			if (ctx.indexes()!=null) visitIndexes(ctx.indexes()) else Vector(),
			visitStopSp(ctx.getStop())
		)
	
	def visitDecls_signature(ctx: FlixParser.Decls_signatureContext) = ParsedAst.Declaration.Signature(
			visitTscommentHelper(ctx.tscomment().toList.toVector),
			visitStartSp(ctx.DEF().getSymbol()),
			visitDefinitionName(ctx.definitionName()),
			visitFormalparams(ctx.formalparams),
			visitType(ctx.`type`()),
			visitStopSp(ctx.getStop())
		)
	
	def visitDecls_external(ctx: FlixParser.Decls_externalContext) = ParsedAst.Declaration.External(
			visitTscommentHelper(ctx.tscomment().toList.toVector),
			visitStartSp(ctx.EXTERNAL().getSymbol()),
			visitDefinitionName(ctx.definitionName()),
			visitFormalparams(ctx.formalparams),
			visitType(ctx.`type`()),
			visitStopSp(ctx.getStop())
		)
	
	def visitDecls_definition(ctx: FlixParser.Decls_definitionContext) = ParsedAst.Declaration.Definition(
			visitTscommentHelper(ctx.tscomment().toList.toVector),
			if (ctx.annotations()!=null) visitAnnotations(ctx.annotations()) else Vector(),
			visitStartSp(ctx.DEF().getSymbol()),
			visitDefinitionName(ctx.definitionName()),
			visitTypeparams(ctx.typeparams),
			visitFormalparams(ctx.formalparams),
			visitType(ctx.`type`()),
			visitExpression(ctx.expression()),
			visitStopSp(ctx.getStop())
		)
	
	def visitDecls_law(ctx: FlixParser.Decls_lawContext) = ParsedAst.Declaration.Law(
			visitTscommentHelper(ctx.tscomment().toList.toVector),
			visitStartSp(ctx.LAW().getSymbol()),
			visitDefinitionName(ctx.definitionName()),
			visitTypeparams(ctx.typeparams),
			visitFormalparams(ctx.formalparams),
			visitType(ctx.`type`()),
			visitExpression(ctx.expression()),
			visitStopSp(ctx.getStop())
		)
	
	def visitDecls_class(ctx: FlixParser.Decls_classContext) = ParsedAst.Declaration.Class(
			visitTscommentHelper(ctx.tscomment().toList.toVector),
			visitStartSp(ctx.CLASS().getSymbol()),
			visitClassName(ctx.className()),
			visitClass_typeparams(ctx.class_typeparams()),
			visitContextBoundsList(ctx.contextBoundsList),
			visitClass_body(ctx.class_body()),
			visitStopSp(ctx.getStop())
		)
	
	def visitClass_body(ctx: FlixParser.Class_bodyContext) : Vector[ParsedAst.Declaration] = if ( ctx.class_decl() != null) ctx.class_decl().map(visitClass_decl).toList.toVector else Vector()

	def visitClass_decl(ctx: FlixParser.Class_declContext) : ParsedAst.Declaration = ctx.getChild(0) match{
			case x: FlixParser.Decls_definitionContext => visitDecls_definition(x)
			case x: FlixParser.Decls_signatureContext => visitDecls_signature(x)
			case x: FlixParser.Decls_lawContext => visitDecls_law(x)
		}
	
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

	def visitDecls_impl_body(ctx: FlixParser.Decls_impl_bodyContext) : Vector[ParsedAst.Declaration.Definition] = 
		if ( ctx.decls_definition() != null )
			ctx.decls_definition().map(visitDecls_definition).toList.toVector
		else Vector()
	
	def visitDecls_impl(ctx: FlixParser.Decls_implContext) = ParsedAst.Declaration.Impl(
			visitTscommentHelper(ctx.tscomment().toList.toVector),
			visitStartSp(ctx.IMPL().getSymbol()),
			visitClassName(ctx.className()),
			visitClass_typeparams(ctx.class_typeparams()),
			visitImplContextBoundsList(ctx.implContextBoundsList()),
			visitDecls_impl_body(ctx.decls_impl_body()),
			visitStopSp(ctx.getStop())
		)





	//===========
	//EXPRESSIONS
	//===========


	def visitExpression(ctx: FlixParser.ExpressionContext) : ParsedAst.Expression = ctx.logical() match {
			case  null => visitExpression(ctx.expression())
			case x : FlixParser.LogicalContext => visitLogical(x);
		}

	def visitExpressions(ctx: FlixParser.ExpressionsContext) : Vector[ParsedAst.Expression] = if ( ctx.expression() != null) ctx.expression().map(visitExpression).toList.toVector else Vector()

	def visitLogical(ctx: FlixParser.LogicalContext) : ParsedAst.Expression = ctx.getChildCount() match {
			case 1 => visitComparison(ctx.comparison(0))
			case _ => ParsedAst.Expression.Binary(
					visitComparison(ctx.comparison(0)),
					visitLogical_ops(ctx.logical_ops()),
					visitComparison(ctx.comparison(1)),
					visitStopSp(ctx.getStop())
				)
		}

	def visitComparison(ctx: FlixParser.ComparisonContext) : ParsedAst.Expression = ctx.getChildCount() match {
			case 1 => visitAdditive(ctx.additive(0))
			case _ => ParsedAst.Expression.Binary(
					visitAdditive(ctx.additive(0)),
					visitComparison_ops(ctx.comparison_ops()),
					visitAdditive(ctx.additive(1)),
					visitStopSp(ctx.getStop())
				)
		}

	//REVISIT for more than 2 multiplicatives
	def visitAdditive(ctx: FlixParser.AdditiveContext) : ParsedAst.Expression = ctx.getChildCount() match {
			case 1 => visitMultiplicative(ctx.multiplicative())
			case _ => ParsedAst.Expression.Binary(
					visitAdditive(ctx.additive()),
					visitAddve_ops(ctx.addve_ops()),
					visitMultiplicative(ctx.multiplicative()),
					visitStopSp(ctx.getStop())
				)
		}

	//REVISIT for more than 2 infixes
	def visitMultiplicative(ctx: FlixParser.MultiplicativeContext) : ParsedAst.Expression  = ctx.getChildCount() match {
			case 1 => visitInfix(ctx.infix())
			case _ => ParsedAst.Expression.Binary(
					visitMultiplicative(ctx.multiplicative()),
					visitMultipve_ops(ctx.multipve_ops()),
					visitInfix(ctx.infix()),
					visitStopSp(ctx.getStop())
				)
		}

	def visitInfix(ctx: FlixParser.InfixContext) : ParsedAst.Expression = ctx.getChildCount() match {
			case 1 => visitExtended(ctx.extended(0))
			case _ => ParsedAst.Expression.Infix(
					visitExtended(ctx.extended(0)),
					visitQualifiedDefinitionName(ctx.qualifiedDefinitionName()),
					visitExtended(ctx.extended(1)),
					visitStopSp(ctx.getStop())
				)
		}

	def visitExtended(ctx: FlixParser.ExtendedContext) : ParsedAst.Expression = ctx.getChildCount() match {
			case 1 => visitUnary(ctx.unary(0))
			case _ => ParsedAst.Expression.ExtendedBinary(
					visitUnary(ctx.unary(0)),
					visitExtbin_ops(ctx.extbin_ops()),
					visitUnary(ctx.unary(1)),
					visitStopSp(ctx.getStop())
				)
		}

	def visitUnary(ctx: FlixParser.UnaryContext) : ParsedAst.Expression = ctx.getChildCount() match {
			case 1 => visitAscribe(ctx.ascribe())
			case _ => ParsedAst.Expression.Unary(
					visitStartSp(ctx.getStart()),
					visitUnary_ops(ctx.unary_ops()),
					visitUnary(ctx.unary()),
					visitStopSp(ctx.getStop())
				)
		}

	def visitAscribe(ctx: FlixParser.AscribeContext) : ParsedAst.Expression = ctx.getChildCount() match {
			case 1 => visitE_fList(ctx.e_fList())
			case _ => ParsedAst.Expression.Ascribe(
					visitE_fList(ctx.e_fList()),
					visitType(ctx.`type`()),
					visitStopSp(ctx.getStop())
				)
		}

	def visitE_primary(ctx: FlixParser.E_primaryContext) : ParsedAst.Expression  = ctx.getChild(0) match {
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
			case x: FlixParser.E_literalContext => visitE_literal(x)
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

	def visitMatch_rules(ctx: FlixParser.Match_rulesContext) : Vector[(ParsedAst.Pattern,ParsedAst.Expression)] = if ( ctx.match_rule() != null) ctx.match_rule().map(visitMatch_rule).toList.toVector else Vector()

	def visitE_match(ctx: FlixParser.E_matchContext) = ParsedAst.Expression.Match(
			visitStartSp(ctx.getStart()),
			visitExpression(ctx.expression()),
			visitMatch_rules(ctx.match_rules()),
			visitStopSp(ctx.getStop())
		)

	def visitSwitch_rule(ctx: FlixParser.Switch_ruleContext) = (
			visitExpression(ctx.expression(0)),
			visitExpression(ctx.expression(1))
		)

	def visitSwitch_rules(ctx: FlixParser.Switch_rulesContext) : Vector[(ParsedAst.Expression,ParsedAst.Expression)] = if ( ctx.switch_rule() != null) ctx.switch_rule().map(visitSwitch_rule).toList.toVector else Vector()

	def visitE_switch(ctx: FlixParser.E_switchContext) = ParsedAst.Expression.Switch(
			visitStartSp(ctx.getStart()),
			visitSwitch_rules(ctx.switch_rules()),
			visitStopSp(ctx.getStop())
		)
	def visitE_apply(ctx: FlixParser.E_applyContext) : ParsedAst.Expression = ctx.getChildCount() match{
			case 1 => visitE_primary(ctx.e_primary())
			case _ => ctx.expressions() match {
				case null => ParsedAst.Expression.Apply(
						visitE_primary(ctx.e_primary()),
						Vector(),
						visitStopSp(ctx.getStop())
					)
				case x: FlixParser.ExpressionsContext => 
					ParsedAst.Expression.Apply(
						visitE_primary(ctx.e_primary()),
						visitExpressions(ctx.expressions()),
						visitStopSp(ctx.getStop())
					)
			}
		}

	

	def visitE_literal(ctx: FlixParser.E_literalContext) = ParsedAst.Expression.Lit(
			visitStartSp(ctx.getStart()),
			visitLiteral(ctx.literal()),
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
			else Vector(),
			
			visitStopSp(ctx.getStop())
		)

	def visitE_keyValue(ctx: FlixParser.E_keyValueContext) = ( visitExpression(ctx.expression(0)),visitExpression(ctx.expression(1)) );

	def visitE_keyValues(ctx: FlixParser.E_keyValuesContext): Vector[(ParsedAst.Expression,ParsedAst.Expression)] = if ( ctx.e_keyValue() != null) ctx.e_keyValue().map(visitE_keyValue).toList.toVector else Vector()

	def visitE_userError(ctx: FlixParser.E_userErrorContext) = ParsedAst.Expression.UserError( visitStartSp(ctx.getStart()),visitStopSp(ctx.getStop()) )

	def visitE_wild(ctx: FlixParser.E_wildContext) = ParsedAst.Expression.Wild( visitStartSp(ctx.getStart()),visitStopSp(ctx.getStop()) )

	def visitE_fNil(ctx: FlixParser.E_fNilContext) = ParsedAst.Expression.FNil( visitStartSp(ctx.getStart()),visitStopSp(ctx.getStop()) )

	def visitE_fList(ctx: FlixParser.E_fListContext) : ParsedAst.Expression = 
		if ( ctx.expression() != null )
			ParsedAst.Expression.FCons(
				visitE_apply(ctx.e_apply()),
				visitExpression(ctx.expression()),
				visitStopSp(ctx.getStop())
			)
		else visitE_apply(ctx.e_apply())

	def visitE_fVec(ctx: FlixParser.E_fVecContext) = ParsedAst.Expression.FVec(
			visitStartSp(ctx.getStart()),
			if (ctx.expressions() != null) visitExpressions(ctx.expressions())
			else Vector(),
			visitStopSp(ctx.getStop())
		)

	def visitE_fSet(ctx: FlixParser.E_fSetContext) = ParsedAst.Expression.FSet(
			visitStartSp(ctx.getStart()),
			if (ctx.expressions() != null) visitExpressions(ctx.expressions())
			else Vector(),
			visitStopSp(ctx.getStop())
		)
	def visitE_fMap(ctx: FlixParser.E_fMapContext) = ParsedAst.Expression.FMap(
			visitStartSp(ctx.getStart()),
			if (ctx.e_keyValues() != null) visitE_keyValues(ctx.e_keyValues())
			else Vector(),
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


	def visitPattern(ctx: FlixParser.PatternContext) : ParsedAst.Pattern = visitP_fList(ctx.p_fList())

	def visitPatterns(ctx: FlixParser.PatternsContext) : Vector[ParsedAst.Pattern] = if ( ctx.pattern() != null) ctx.pattern().map(visitPattern).toList.toVector else Vector()

	def visitSimple(ctx: FlixParser.SimpleContext) : ParsedAst.Pattern = ctx.getChild(0) match {
			case x: FlixParser.P_fNilContext => visitP_fNil(x)
			case x: FlixParser.P_literalContext => visitP_literal(x)
			case x: FlixParser.P_variableContext => visitP_variable(x)
			case x: FlixParser.P_wildContext => visitP_wild(x)
			case x: FlixParser.P_tagContext => visitP_tag(x)
			case x: FlixParser.P_tupleContext => visitP_tuple(x)
			case x: FlixParser.P_fVecContext => visitP_fVec(x)
			case x: FlixParser.P_fSetContext => visitP_fSet(x)
			case x: FlixParser.P_fMapContext => visitP_fMap(x)
		}

	def visitP_keyValue(ctx: FlixParser.P_keyValueContext) = ( visitPattern(ctx.pattern(0)),visitPattern(ctx.pattern(1)) );

	def visitP_keyValues(ctx: FlixParser.P_keyValuesContext) : Vector[(ParsedAst.Pattern,ParsedAst.Pattern)] = if ( ctx.p_keyValue() != null) ctx.p_keyValue().map(visitP_keyValue).toList.toVector else Vector()


	def visitP_literal(ctx: FlixParser.P_literalContext) = ParsedAst.Pattern.Lit(
			visitStartSp(ctx.getStart()),
			visitLiteral(ctx.literal()),
			visitStopSp(ctx.getStop())
		)

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
			else Vector(),
			
			visitStopSp(ctx.getStop())
		)

	def visitP_wild(ctx: FlixParser.P_wildContext) = ParsedAst.Pattern.Wild( visitStartSp(ctx.getStart()),visitStopSp(ctx.getStop()) )

	def visitP_fNil(ctx: FlixParser.P_fNilContext) = ParsedAst.Pattern.FNil( visitStartSp(ctx.getStart()),visitStopSp(ctx.getStop()) )

	def visitP_variable(ctx: FlixParser.P_variableContext) = ParsedAst.Pattern.Var(
			visitStartSp(ctx.getStart()),
			visitVariableName(ctx.variableName()),
			visitStopSp(ctx.getStop())
		)

	def visitP_fList(ctx: FlixParser.P_fListContext) : ParsedAst.Pattern = 
		if ( ctx.pattern() != null )
			ParsedAst.Pattern.FCons(
				visitSimple(ctx.simple()),
				visitPattern(ctx.pattern()),
				visitStopSp(ctx.getStop())
			)
		else visitSimple(ctx.simple())

	def visitP_fVec(ctx: FlixParser.P_fVecContext) = ParsedAst.Pattern.FVec(
			visitStartSp(ctx.getStart()),
			if (ctx.patterns() != null) visitPatterns(ctx.patterns())
			else Vector(),
			if( ctx.pattern() != null ) Option( visitPattern(ctx.pattern()) )
			else Option(null),
			visitStopSp(ctx.getStop())
		)

	def visitP_fSet(ctx: FlixParser.P_fSetContext) = ParsedAst.Pattern.FSet(
			visitStartSp(ctx.getStart()),
			if (ctx.patterns() != null) visitPatterns(ctx.patterns())
			else Vector(),
			if( ctx.pattern() != null ) Option( visitPattern(ctx.pattern()) )
			else Option(null),
			visitStopSp(ctx.getStop())
		)

	def visitP_fMap(ctx: FlixParser.P_fMapContext) = ParsedAst.Pattern.FMap(
			visitStartSp(ctx.getStart()),
			if (ctx.p_keyValues() != null) visitP_keyValues(ctx.p_keyValues())
			else Vector(),
			if( ctx.pattern() != null ) Option( visitPattern(ctx.pattern()) )
			else Option(null),
			visitStopSp(ctx.getStop())
		)

	//========
	//LITERALS
	//========


	def visitLiteral(ctx: FlixParser.LiteralContext) : ParsedAst.Literal = ctx.getChild(0) match {
			case x: FlixParser.BoolsContext => visitBools(x)
			case x: FlixParser.CharsContext => visitChars(x)
			case x: FlixParser.FloatsContext => visitFloats(x)
			case x: FlixParser.IntsContext => visitInts(x)
			case x: FlixParser.StrsContext => visitStrs(x)
		}

	def visitBools(ctx: FlixParser.BoolsContext) : ParsedAst.Literal = ctx.getText() match {
			case "true" => ParsedAst.Literal.True( visitStartSp(ctx.getStart()) , visitStopSp(ctx.getStop()) )
			case "false" => ParsedAst.Literal.False( visitStartSp(ctx.getStart()) , visitStopSp(ctx.getStop()) )
		}

	def visitFloats(ctx: FlixParser.FloatsContext) : ParsedAst.Literal = ctx.getChild(0) match {
			case x: FlixParser.Float32Context => visitFloat32(x)
			case x: FlixParser.Float64Context => visitFloat64(x)
			case x: FlixParser.FloatDefaultContext => visitFloatDefault(x)
		}

	def visitFloat32(ctx: FlixParser.Float32Context) = ParsedAst.Literal.Float32(
			visitStartSp(ctx.getStart()),
			if ( ctx.negative() != null ) true else false,
			ctx.Digits(0).getText(),
			ctx.Digits(1).getText(),
			visitStopSp(ctx.getStop())
		)
	def visitFloat64(ctx: FlixParser.Float64Context) = ParsedAst.Literal.Float64(
			visitStartSp(ctx.getStart()),
			if ( ctx.negative() != null ) true else false,
			ctx.Digits(0).getText(),
			ctx.Digits(1).getText(),
			visitStopSp(ctx.getStop())
		)

	def visitFloatDefault(ctx: FlixParser.FloatDefaultContext) = ParsedAst.Literal.Float64(
			visitStartSp(ctx.getStart()),
			if ( ctx.negative() != null ) true else false,
			ctx.Digits(0).getText(),
			ctx.Digits(1).getText(),
			visitStopSp(ctx.getStop())
		)

	def visitInts(ctx: FlixParser.IntsContext) : ParsedAst.Literal = ctx.getChild(0) match {
			case x: FlixParser.Int8Context => visitInt8(x)
			case x: FlixParser.Int16Context => visitInt16(x)
			case x: FlixParser.Int32Context => visitInt32(x)
			case x: FlixParser.Int64Context => visitInt64(x)
			case x: FlixParser.BigIntContext => visitBigInt(x)
			case x: FlixParser.IntDefaultContext => visitIntDefault(x)
		}

	def visitInt8(ctx: FlixParser.Int8Context) = ParsedAst.Literal.Int8(
			visitStartSp(ctx.getStart()),
			if ( ctx.negative() != null ) true else false,
			ctx.Digits().getText(),
			visitStopSp(ctx.getStop())
		)

	def visitInt16(ctx: FlixParser.Int16Context) = ParsedAst.Literal.Int16(
			visitStartSp(ctx.getStart()),
			if ( ctx.negative() != null ) true else false,
			ctx.Digits().getText(),
			visitStopSp(ctx.getStop())
		)

	def visitInt32(ctx: FlixParser.Int32Context) = ParsedAst.Literal.Int32(
			visitStartSp(ctx.getStart()),
			if ( ctx.negative() != null ) true else false,
			ctx.Digits().getText(),
			visitStopSp(ctx.getStop())
		)

	def visitInt64(ctx: FlixParser.Int64Context) = ParsedAst.Literal.Int64(
			visitStartSp(ctx.getStart()),
			if ( ctx.negative() != null ) true else false,
			ctx.Digits().getText(),
			visitStopSp(ctx.getStop())
		)

	def visitBigInt(ctx: FlixParser.BigIntContext) = ParsedAst.Literal.BigInt(
			visitStartSp(ctx.getStart()),
			if ( ctx.negative() != null ) true else false,
			ctx.Digits().getText(),
			visitStopSp(ctx.getStop())
		)

	def visitIntDefault(ctx: FlixParser.IntDefaultContext) = ParsedAst.Literal.Int32(
			visitStartSp(ctx.getStart()),
			if ( ctx.negative() != null ) true else false,
			ctx.Digits().getText(),
			visitStopSp(ctx.getStop())
		)

	//revisit after confirming special chars ie '\t', '\n', etc...
	def visitStrs(ctx: FlixParser.StrsContext) = ParsedAst.Literal.Str(
			visitStartSp(ctx.getStart()),
			StringContext.treatEscapes( ctx.getText().drop(1).dropRight(1) ),
			visitStopSp(ctx.getStop())
		)

	//revisit after confirming special chars ie '\t', '\n', etc...
	def visitChars(ctx: FlixParser.CharsContext) = ParsedAst.Literal.Char(
			visitStartSp(ctx.getStart()),
			StringContext.treatEscapes( ctx.getText().drop(1).dropRight(1) ),
			visitStopSp(ctx.getStop())
		)

	//=====
	//TYPES
	//=====
	
	def visitPrimary(ctx: FlixParser.PrimaryContext) : ParsedAst.Type = ctx.getChild(0) match {
			case x: FlixParser.ArrowContext => visitArrow(x)
			case x: FlixParser.TupleContext => visitTuple(x)
			case x: FlixParser.ApplyContext => visitApply(x)
			case x: FlixParser.VarContext => visitVar(x)
			case x: FlixParser.RefContext => visitRef(x)
		}

	def visitArrow(ctx: FlixParser.ArrowContext) = ParsedAst.Type.Arrow(
			visitStartSp(ctx.getStart()),
			ctx.`type`().dropRight(1).map(visitType).toList.toVector,
			visitType(ctx.`type`().last),
			visitStopSp(ctx.getStop())
		)

	def visitTuple_unit(ctx: FlixParser.Tuple_unitContext) = ParsedAst.Type.Unit(
			visitStartSp(ctx.getStart()),
			visitStopSp(ctx.getStop())
		)

	def visitTuple_singleton(ctx: FlixParser.Tuple_singletonContext) = visitType(ctx.`type`())

	def visitTuple_multi(ctx: FlixParser.Tuple_multiContext) = ParsedAst.Type.Tuple(
			visitStartSp(ctx.getStart()),
			ctx.`type`().map(visitType).toList.toVector,
			visitStopSp(ctx.getStop())
		)
	def visitTuple(ctx: FlixParser.TupleContext) : ParsedAst.Type = ctx.getChild(0) match{
			case x: FlixParser.Tuple_unitContext => visitTuple_unit(x)
			case x: FlixParser.Tuple_singletonContext => visitTuple_singleton(x)
			case x: FlixParser.Tuple_multiContext => visitTuple_multi(x)
		}
	
	def visitApply(ctx: FlixParser.ApplyContext) = ParsedAst.Type.Apply(
			visitStartSp(ctx.getStart()),
			visitRef(ctx.ref()),
			ctx.`type`().map(visitType).toList.toVector,
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


	def visitUnary_ops(ctx: FlixParser.Unary_opsContext) : UnaryOperator = ctx.getText() match {
			case "+" => UnaryOperator.Plus
			case "-" => UnaryOperator.Minus
			case "~" => UnaryOperator.BitwiseNegate
			case "¬" => UnaryOperator.LogicalNot
			case "!" => UnaryOperator.LogicalNot
		}

	def visitLogical_ops(ctx: FlixParser.Logical_opsContext) : BinaryOperator = ctx.getText() match {
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

	def visitComparison_ops(ctx: FlixParser.Comparison_opsContext) : BinaryOperator = ctx.getText() match {
			case "<=" => BinaryOperator.LessEqual
			case ">=" => BinaryOperator.GreaterEqual
			case "<" => BinaryOperator.Less
			case ">" => BinaryOperator.Greater
			case "==" => BinaryOperator.Equal
			case "!=" => BinaryOperator.NotEqual
			case "≡" => BinaryOperator.Equal
		}

	def visitMultipve_ops(ctx: FlixParser.Multipve_opsContext) : BinaryOperator = ctx.getText() match {
			case "**" => BinaryOperator.Exponentiate
			case "*" => BinaryOperator.Times
			case "/" => BinaryOperator.Divide
			case "%" => BinaryOperator.Modulo
		}

	def visitAddve_ops(ctx: FlixParser.Addve_opsContext) : BinaryOperator = ctx.getText() match {
			case "+" => BinaryOperator.Plus
			case "-" => BinaryOperator.Minus
		}

	def visitExtbin_ops(ctx: FlixParser.Extbin_opsContext) : ExtBinaryOperator = ctx.getText() match {
			case "⊑" => ExtBinaryOperator.Leq
			case "⊔" => ExtBinaryOperator.Lub
			case "⊓" => ExtBinaryOperator.Glb
			case "▽" => ExtBinaryOperator.Widen
			case "△" => ExtBinaryOperator.Narrow
		}

	//==========
	//PREDICATES
	//==========
	def visitPredicate(ctx: FlixParser.PredicateContext) : ParsedAst.Predicate = ctx.getChild(0) match{
			case x: FlixParser.Pred_trueContext => visitPred_true(x)
			case x: FlixParser.Pred_falseContext => visitPred_false(x)
			case x: FlixParser.Pred_filterContext => visitPred_filter(x)
			case x: FlixParser.Pred_tableContext => visitPred_table(x)
			case x: FlixParser.Pred_notequalContext => visitPred_notequal(x)
			case x: FlixParser.Pred_loopContext => visitPred_loop(x)
		}

	def visitPredicates(ctx: FlixParser.PredicatesContext) : Vector[ParsedAst.Predicate] = if ( ctx.predicate() != null) ctx.predicate().map(visitPredicate).toList.toVector else Vector()

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