package ca.uwaterloo.flix

import java.nio.file.{Path, InvalidPathException, Files, Paths}

import ca.uwaterloo.flix.language.Compiler
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.TypedAst.Directive
import ca.uwaterloo.flix.runtime.{Model, Solver}
import ca.uwaterloo.flix.util.{AsciiTable, Validation}
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
   * @param strings a sequence of strings representing the Flix program.
   * @return the minimal model.
   */
  def fromStrings(strings: String*): Validation[Model, FlixError] = Compiler.compileStrings(strings) map {
    case ast => new Solver()(Solver.SolverContext(ast)).solve()
  }

  /**
   * Solves the given Flix program.
   *
   * @param paths a sequence of paths representing the Flix program.
   * @return the minimal model.
   */
  def fromPaths(paths: Path*): Validation[Model, FlixError] = Compiler.compilePaths(paths) map {
    case ast => new Solver()(Solver.SolverContext(ast)).solve()
  }

}
