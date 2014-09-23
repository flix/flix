package impl.verifier

import impl.logic._
import syntax.Symbols._
import syntax.Terms._
import syntax.Terms.RichTerm

sealed trait Property

object Property {

  /**
   * Reflexivity: ∀x. x ⊑ x
   */
  def reflexivity(leq: Term.Abs): Term =
    Term.Abs('x, leq.typ, leq.call('x, 'x))


}
