package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.{ParsedAst, _}
import scala.collection.immutable.Seq;
import collection.JavaConversions._;
import org.antlr.v4.runtime.Token;

class AST_FlixVisitor(source: SourceInput, input: String) /*extends FlixVisitor[Object]*/{


	//SOURCE POSITION NODE
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

	//Triple Slash Comments
	def visitTscomment(ctxs: Seq[FlixParser.TscommentContext]) = 
		if (ctxs.isEmpty) Option(null)
		else Option(ParsedAst.Documentation(
			visitStartSp(ctxs.head.getStart()),
			ctxs.map(x => x.TripleSlashComment().getText()),
			visitStopSp(ctxs.last.getStop())
		))

	//ROOT NODE
	def visitStart(ctx: FlixParser.StartContext): ParsedAst.Root = ParsedAst.Root(
			ctx.s_import().map(visitS_import).toList.toSeq,
			ctx.decl().map(visitDecl).toList.toSeq
		)

	//NAME RULES
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

	//RULES FOR LISTS

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

	//IMPORT RULES

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


	//DECLARATION RULES
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
			if (ctx.attributes()) visitAttributes(ctx.attributes()) else Seq(),
			visitStopSp(ctx.getStop())
		)
	
	def visitDecls_lattice(ctx: FlixParser.Decls_latticeContext) = ParsedAst.Declaration.Lattice(
			visitTscomment(ctx.tscomment().toList.toSeq),
			visitStartSp(ctx.LAT().getSymbol()),
			visitTableName(ctx.tableName()),
			if (ctx.attributes()) visitAttributes(ctx.attributes()) else Seq(),
			visitStopSp(ctx.getStop())
		)
	
	def visitDecls_index(ctx: FlixParser.Decls_indexContext) = ParsedAst.Declaration.Index(
			visitTscomment(ctx.tscomment().toList.toSeq),
			visitStartSp(ctx.INDEX().getSymbol()),
			visitQualifiedTableName(ctx.qualifiedTableName()),
			if (ctx.indexes()) visitIndexes(ctx.indexes()) else Seq(),
			visitStopSp(ctx.getStop())
		)
	
	def visitDecls_signature(ctx: FlixParser.Decls_signatureContext);
	
	def visitDecls_external(ctx: FlixParser.Decls_externalContext);
	
	def visitDecls_definition(ctx: FlixParser.Decls_definitionContext);
	
	def visitDecls_law(ctx: FlixParser.Decls_lawContext);
	
	def visitDecls_class(ctx: FlixParser.Decls_classContext);
	
	def visitClass_body(ctx: FlixParser.Class_bodyContext);
	
	def visitDecls_fact(ctx: FlixParser.Decls_factContext);
	
	def visitDecls_rule(ctx: FlixParser.Decls_ruleContext);
	
	def visitElms(ctx: FlixParser.ElmsContext);
	
	def visitDecls_letlattice(ctx: FlixParser.Decls_letlatticeContext);
	
	def visitDecls_impl(ctx: FlixParser.Decls_implContext);



	//Types
	
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
}