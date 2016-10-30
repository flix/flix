package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.{ParsedAst, _}
import scala.collection.immutable.Seq;
import collection.JavaConversions._;
import org.antlr.v4.runtime.Token;

class AST_FlixVisitor(source: SourceInput, input: String) /*extends FlixVisitor[Object]*/{


	//SOURCE POSITION NODE
	def visitSp(tk: Token): SourcePosition = SourcePosition(
			source,
			tk.getLine(),
			tk.getCharPositionInLine(),
			Some(input)
		)

	//ROOT NODE
	def visitStart(ctx: FlixParser.StartContext): ParsedAst.Root = ParsedAst.Root(
			ctx.s_import().map(visitS_import).toList.toSeq,
			Nil
			//ctx.decl().map(visitDecl).toList.toSeq
		)

	//NAME RULES
	def visitIdent(ctx: FlixParser.IdentContext) =  Name.Ident(
			visitSp(ctx.getStart()),
			ctx.getChild(0).getText(),
			visitSp(ctx.getStop())
		)

	def visitNname(ctx: FlixParser.NnameContext) = Name.NName(
			visitSp(ctx.getStart()),
			ctx.ident().map(visitIdent).toList,
			visitSp(ctx.getStop())
		)

	//IMPORT RULES
	def visitS_import(ctx: FlixParser.S_importContext): ParsedAst.Import =  ctx.getChild(ctx.getChildCount-1) match {
			case x: FlixParser.Import_wildcardContext => visitImport_wildcard(x)
			case x: FlixParser.Import_definitionContext => visitImport_definition(x)
			case x: FlixParser.Import_namespaceContext =>  visitImport_namespace(x)
		}

	def visitImport_wildcard(ctx: FlixParser.Import_wildcardContext) = ParsedAst.Import.Wild(
			visitSp(ctx.getStart()),
			visitNname(ctx.nname()),
			visitSp(ctx.getStop())
		)

	def visitImport_definition(ctx: FlixParser.Import_definitionContext) = ParsedAst.Import.Definition(
			visitSp(ctx.getStart()),
			visitNname(ctx.nname()),
			visitIdent(ctx.ident()),
			visitSp(ctx.getStop())
		)

	def visitImport_namespace(ctx: FlixParser.Import_namespaceContext) =ParsedAst.Import.Namespace(
			visitSp(ctx.getStart()),
			visitNname(ctx.nname()),
			visitSp(ctx.getStop())
		)


	//DECLARATION RULES
	// def visitDecl(ctx: FlixParser.DeclContext): ParsedAst.Declaration = ParsedAst.Declaration


}