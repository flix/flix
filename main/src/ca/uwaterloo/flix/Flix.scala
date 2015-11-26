package ca.uwaterloo.flix

import java.nio.file.{Path, InvalidPathException, Files, Paths}

import ca.uwaterloo.flix.language.Compiler
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.TypedAst.Directive
import ca.uwaterloo.flix.runtime.{Model, Solver}
import ca.uwaterloo.flix.util.{Options, AsciiTable, Validation}
import ca.uwaterloo.flix.util.Validation._

object Flix {

  /**
   * A common super-type for all Flix compilation and runtime errors.
   */
  trait FlixError {
    /**
     * Returns a human readable string representation of the error.
     */
    def format: String
  }

  /**
   * Solves the given Flix program.
   *
   * @param input a sequence strings representing the Flix program.
   * @param options the options.
   * @return the minimal model.
   */
  def mkStr(input: Seq[String], options: Options = Options.Default): Validation[Model, FlixError] = Compiler.compileStrings(input) map {
    case ast => new Solver()(Solver.SolverContext(ast, options)).solve()
  }

  /**
   * Solves the given Flix program.
   *
   * @param input a sequence of paths representing the Flix program.
   * @param options the options.
   * @return the minimal model.
   */
  def mkPath(input: Seq[Path], options: Options = Options.Default): Validation[Model, FlixError] = Compiler.compilePaths(input) map {
    case ast => new Solver()(Solver.SolverContext(ast, options)).solve()
  }

  /**
   * Alias for [[mkStr]] with one input and default options.
   */
  def mkStr(input: String): Validation[Model, FlixError] =
    mkStr(Seq(input))

  /**
   * Alias for [[mkPath]] with one input and default options.
   */
  def mkPath(input: Path): Validation[Model, FlixError] =
    mkPath(Seq(input))

}
