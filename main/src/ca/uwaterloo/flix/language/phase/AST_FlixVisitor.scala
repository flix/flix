package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.{ParsedAst, _}
import scala.collection.immutable.Seq;
import collection.JavaConversions._;

class AST_FlixVisitor{

	def visitStart(ctx: FlixParser.StartContext): ParsedAst.Root = ParsedAst.Root(
			visitS_imports(ctx.s_imports()),
			visitDecls(ctx.decls())
		)

	def visitS_imports(ctx: FlixParser.S_importsContext): Seq[ParsedAst.Import] = ctx.s_import().toSeq() map visitS_import _

	def visitS_import(ctx: FlixParser.S_importContext): ParsedAst.Import =  ctx.getChild(0) match {
			case x: Import_wildcardContext => visitImport_wildcard(x)
			case x: Import_definitionContext => visitImport_definition(x)
			case x: Import_namespaceContext =>  visitImport_namespace(x)
		}

	def visitImport_wildcard(ctx: FlixParser.Import_wildcardContext) = println("wild")

	def visitImport_definition(ctx: FlixParser.Import_definitionContext) =println("definition")

	def visitImport_namespace(ctx: FlixParser.Import_namespaceContext) =println("namespace")

	def visitDecls(ctx: FlixParser.DeclsContext): Seq[ParsedAst.Declaration] = Seq()

}