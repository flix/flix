package syntax

import impl.logic.HornClause
import syntax.Predicates._

/**
 * Embedded DSL syntax for horn clauses.
 */
object HornClauses {

  /**
   * Rich Horn Clauses.
   */
  implicit class RichHornClause(h: HornClause) {
    def fmt: String =
      if (h.isFact)
        h.head.fmt + "."
      else
        h.head.fmt + " :- " + h.body.map(p => p.fmt).mkString(", ") + "."
  }

}
