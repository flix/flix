package syntax

import impl.logic.Program
import syntax.HornClauses._

/**
 * Embedded DSL syntax for programs.
 */
object Programs {

  /**
   * Rich Programs
   */
  implicit class RichProgram(p: Program) {
    def fmt: String = p.clauses.toList.map(h => h.fmt).mkString("\n")
  }

}
