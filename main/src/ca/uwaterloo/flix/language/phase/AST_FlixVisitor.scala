package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.{ParsedAst, _}
import scala.collection.immutable.Seq;
import collection.JavaConversions._;

class AST_FlixVisitor extends FlixVisitor[Object]{

	//ROOT NODE
	def visitStart(ctx: FlixParser.StartContext): ParsedAst.Root = ParsedAst.Root(
			ctx.s_import().map(visitS_import).toList.toSeq,
			ctx.decl().map(visitDecl).toList.toSeq
		)

	//IMPORT RULES
	def visitS_import(ctx: FlixParser.S_importContext): ParsedAst.Import =  ctx.getChild(0) match {
			case x: FlixParser.Import_wildcardContext => visitImport_wildcard(x)
			case x: FlixParser.Import_definitionContext => visitImport_definition(x)
			case x: FlixParser.Import_namespaceContext =>  visitImport_namespace(x)
		}

	def visitImport_wildcard(ctx: FlixParser.Import_wildcardContext) = ParsedAst.Import.Wild

	def visitImport_definition(ctx: FlixParser.Import_definitionContext) = ParsedAst.Import.Definition

	def visitImport_namespace(ctx: FlixParser.Import_namespaceContext) =ParsedAst.Import.Namespace


	//DECLARATION RULES
	def visitDecl(ctx: FlixParser.DeclContext): ParsedAst.Declaration = ParsedAst.Declaration

}