package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Ast, DesugaredAst, SourceLocation, WeededAst}
import ca.uwaterloo.flix.util.ParOps

object Desugar {

  /**
    * Performs desugaring on the entire program.
    */
  def run(program: WeededAst.Root)(implicit flix: Flix): DesugaredAst.Root = flix.phase("Desugar") {
    val unitsVal = ParOps.parMap(program.units) {
      case (k, v) => visitUnit(k, v)
    }

    val desugaredCompilationUnits = unitsVal.foldLeft(Map.empty[Ast.Source, DesugaredAst.CompilationUnit]) {
      case (macc, (k, v)) => macc + (k -> v)
    }

    DesugaredAst.Root(desugaredCompilationUnits, program.entryPoint, program.names)
  }

  /**
    * Performs naming on the given compilation unit `unit` under the given (partial) program `prog0`.
    */
  private def visitUnit(src: Ast.Source, unit: WeededAst.CompilationUnit)(implicit flix: Flix): (Ast.Source, DesugaredAst.CompilationUnit) = unit match {
    case WeededAst.CompilationUnit(usesAndImports0, decls0, loc) =>
      val usesAndImports = usesAndImports0.map(visitUseOrImport)
      val decls = decls0.map(visitDecl)
      src -> DesugaredAst.CompilationUnit(usesAndImports.flatten, decls.flatten, loc)
  }

  /**
    * Performs weeding on the given use or import `u0`.
    */
  private def visitUseOrImport(u0: WeededAst.UseOrImport): List[DesugaredAst.UseOrImport] = ???

  /**
    * Compiles the given parsed declaration `decl` to a list of weeded declarations.
    */
  private def visitDecl(decl: WeededAst.Declaration)(implicit flix: Flix): List[DesugaredAst.Declaration] = ???
}
