package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast.TypedAst

object Solver {

  case class SolverContext(root: TypedAst.Root)

}

trait Solver {

  /**
   * Runs the solver.
   */
  def solve(): Unit

}
