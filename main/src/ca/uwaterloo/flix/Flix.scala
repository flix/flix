package ca.uwaterloo.flix

import java.nio.file.Path

import ca.uwaterloo.flix.language.Compiler
import ca.uwaterloo.flix.language.ast.{Ast, Name}
import ca.uwaterloo.flix.runtime.{Model, Solver}
import ca.uwaterloo.flix.util.{Options, Validation}

object Flix {

  // TODO: Introduce builder.

  /**
    * A common super-type for all Flix compilation and runtime errors.
    */
  trait FlixError {
    /**
      * Returns a human readable string representation of the error.
      */
    def format: String
  }

  // TODO: Consider unifying these, such that you can give an arbitrary sequence of strings and files.

  /**
    * Solves the given Flix program.
    *
    * @param input   a sequence strings representing the Flix program.
    * @param options the options.
    * @return the minimal model.
    */
  def mkStr(input: Seq[String], hooks: Map[Name.Resolved, Ast.Hook] = Map.empty, options: Options = Options.Default): Validation[Model, FlixError] =
    Compiler.compileStrings(input, hooks) map {
      case ast => new Solver()(Solver.SolverContext(ast, options)).solve()
    }

  /**
    * Solves the given Flix program.
    *
    * @param input   a sequence of paths representing the Flix program.
    * @param options the options.
    * @return the minimal model.
    */
  def mkPath(input: Seq[Path], hooks: Map[Name.Resolved, Ast.Hook] = Map.empty, options: Options = Options.Default): Validation[Model, FlixError] =
    Compiler.compilePaths(input, hooks) map {
      case ast => new Solver()(Solver.SolverContext(ast, options)).solve()
    }

  /**
    * Alias for [[mkStr]] with one input and default options.
    */
  def mkStr(input: String): Validation[Model, FlixError] =
    mkStr(Seq(input), Map.empty)

  /**
    * Alias for [[mkPath]] with one input and default options.
    */
  def mkPath(input: Path): Validation[Model, FlixError] =
    mkPath(Seq(input), Map.empty)

}
