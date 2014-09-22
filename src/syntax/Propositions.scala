package syntax

import impl.logic.Proposition
import syntax.Terms.RichTerm

/**
 * Embedded DSL syntax for propositions.
 */
object Propositions {

  /**
   * Rich proposition.
   */
  implicit class RichProposition(f: Proposition) {
    def fmt: String = f match {
      case Proposition.Not(p) => "(not " + p.fmt + ")"
      case Proposition.Conj(ps) => "(and " + ps.map(_.fmt).mkString(" ") + ")"
      case Proposition.Disj(ps) => "(or " + ps.map(_.fmt).mkString(" ") + ")"
      case Proposition.Eq(x, y) => "(== " + x.fmt + " " + y.fmt + ")"
      case Proposition.NotEq(x, y) => "(!= " + x.fmt + " " + y.fmt + ")"
    }
  }

}
