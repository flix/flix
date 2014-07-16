package impl.verifier

import impl.logic.Symbol.{LatticeSymbol => LSym, PredicateSymbol => PSym}
import syntax._

object LatticeLub {
  /**
   * Upper Bound: ∀x, y, z. x ⊑ x ⨆ y ∧ y ⊑ x ⨆ y.
   */
  def upperBound(sort: LSym, leq: PSym, lub: PSym): String = smt"""
    |;; Upper Bound: ∀x, y, z. x ⊑ x ⨆ y ∧ y ⊑ x ⨆ y.
    |(define-fun $sort-upper-bound () Bool
    |    (forall ((x $sort) (y $sort) (z $sort))
    |        (and
    |            (=> ($lub x y z) ($leq x z))
    |            (=> ($lub x y z) ($leq y z)))))
    |(push)
    |(assert $sort-upper-bound)
    |(check-sat)
    |(pop)
    """.stripMargin

  /**
   * Least Upper Bound: ∀x, y, z. x ⊑ z ∧ y ⊑ z ⇒ x ⨆ y ⊑ z.
   */
  def leastUpperBound(sort: LSym, leq: PSym, lub: PSym): String = smt"""
    |;; Least Upper Bound: ∀x, y, z. x ⊑ z ∧ y ⊑ z ⇒ x ⨆ y ⊑ z.
    |(define-fun $sort-least-upper-bound () Bool
    |    (forall ((x $sort) (y $sort) (z $sort) (w $sort))
    |        (=>
    |            (and ($leq x z)
    |                 ($leq y z)
    |                 ($lub x y w))
    |        ($leq w z))))
    |(push)
    |(assert $sort-least-upper-bound)
    |(check-sat)
    |(pop)
    """.stripMargin
}
