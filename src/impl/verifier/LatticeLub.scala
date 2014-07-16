package impl.verifier

import impl.logic.Symbol.{LatticeSymbol => LSym, PredicateSymbol => PSym}
import syntax._

object LatticeLub {
  /**
   * Join Lub 1: ∀x, y, z. x ⊑ x ⨆ y ∧ y ⊑ x ⨆ y.
   */
  def joinLub1(sort: LSym, leq: PSym, join: PSym): String = smt"""
    |;; Join-Lub-1: ∀x, y, z. x ⊑ x ⨆ y ∧ y ⊑ x ⨆ y.
    |(define-fun join-lub-1 () Bool
    |    (forall ((x $sort) (y $sort) (z $sort))
    |        (and
    |            (=> ($join x y z) ($leq x z))
    |            (=> ($join x y z) ($leq y z)))))
    |(assert join-lub-1)
    |(check-sat)
    """.stripMargin

  /**
   * Join-Lub-2: ∀x, y, z. x ⊑ z ∧ y ⊑ z ⇒ x ⨆ y ⊑ z.
   */
  def joinLub2(sort: LSym, leq: PSym, join: PSym): String = smt"""
    |;; Join-Lub-2: ∀x, y, z. x ⊑ z ∧ y ⊑ z ⇒ x ⨆ y ⊑ z.
    |(define-fun join-lub-2 () Bool
    |    (forall ((x $sort) (y $sort) (z $sort) (w $sort))
    |        (=>
    |            (and ($leq x z)
    |                 ($leq y z)
    |                 ($join x y w))
    |        ($leq w z))))
    |(assert join-lub-2)
    |(check-sat)
    """.stripMargin
}
