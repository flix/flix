package syntax

import impl.logic.Predicate
import syntax.Symbols._
import syntax.Terms._

/**
 * Embedded DSL syntax for predicates.
 */
object Predicates {

  /**
   * Rich Predicates
   */
  implicit class RichPredicate(p: Predicate) {
    def fmt: String = p.name.fmt + "(" + p.terms.map(t => t.fmt).mkString(", ") + ")"
  }

}
