package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.{ParsedAst, _}
import scala.collection.immutable.Seq;

class AST_FlixVisitor{

	def visitStart(ctx: FlixParser.StartContext): ParsedAst.Root = ParsedAst.Root(
			visitS_Imports(ctx.s_imports()),
			visitDecls(ctx.decls())
		)

	def visitS_Imports(ctx: FlixParser.S_importsContext): Seq[ParsedAst.Import] = { println("yahoo"); return Seq()}

	def visitDecls(ctx: FlixParser.DeclsContext): Seq[ParsedAst.Declaration] = Seq()

}