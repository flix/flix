package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.{TypedAst, WeededAst}
import ca.uwaterloo.flix.language.{CompilationError, Compiler}

object Namer {

  import NamerError._

  /**
    * A common super-type for naming errors.
    */
  sealed trait NamerError extends CompilationError

  object NamerError {

    implicit val consoleCtx = Compiler.ConsoleCtx

  }

  /**
    * Performs naming on the given `program`.
    */
  def namer(program: WeededAst.Program)(implicit genSym: GenSym): TypedAst.Root = {
    ???
  }

  /**
    * Performs naming on the given AST `root`.
    */
  def namer(root: WeededAst.Root)(implicit genSym: GenSym): TypedAst.Root = {
    ???
  }



}
