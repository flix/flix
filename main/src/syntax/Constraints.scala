package syntax

import impl.logic.Constraint
import syntax.Propositions._
import syntax.Predicates._

/**
 * Embedded DSL syntax for constraints.
 */
object Constraints {

  /**
   * Rich Horn Clauses.
   */
  implicit class RichConstraint(c: Constraint) {
    def fmt: String = c match {
      case Constraint.Fact(head) => head.fmt + "."
      case Constraint.Rule(head, body, None) => head.fmt + " :- " + body.map(p => p.fmt).mkString(", ") + "."
      case Constraint.Rule(head, body, Some(proposition)) => head.fmt + " :- " + body.map(p => p.fmt).mkString(", ") + ", " + proposition.fmt + "."
    }
  }

}
