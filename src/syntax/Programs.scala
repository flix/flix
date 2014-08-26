package syntax

import impl.logic.Program
import syntax.Constraints._

/**
 * Embedded DSL syntax for programs.
 */
object Programs {

  /**
   * Rich Programs
   */
  implicit class RichProgram(p: Program) {
    def fmt: String = p.constraints.toList.map(h => h.fmt).mkString("\n")
  }

}
