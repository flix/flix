package impl.verifier

import impl.logic.Symbol.{LatticeSymbol => LSym, PredicateSymbol => PSym}
import syntax._

object LatticeHeight {
  /**
   * Stricly-Decreasing: ∀x, y. x ⊑ y ∧ x != y ⇒ f(x) > f(y).
   */
  def strictlyDecreasing(sort: LSym, f: PSym, leq: PSym): String = smt"""
    |;; Stricly-Decreasing: ∀x, y. x ⊑ y ∧ x != y ⇒ f(x) > f(y).
    |(define-fun $f-strictly-decreasing () Bool
    |    (forall ((x $sort) (y $sort) (r1 Int) (r2 Int))
    |        (=>
    |            (and (distinct x y)
    |                 ($leq x y)
    |                 ($f x r1)
    |                 ($f y r2))
    |            (> r1 r2))))
    |(push)
    |(assert  $f-strictly-decreasing)
    |(check-sat)
    |(pop)
    """.stripMargin

  /**
   * Non-Negative: ∀x. f(x) > 0.
   */
  def nonNegative(sort: LSym, f: PSym): String = smt"""
    |;; Non-Negative: ∀x. f(x) > 0.
    |(define-fun $f-non-negative () Bool
    |    (forall ((x $sort) (r Int))
    |        (=>
    |            ($f x r)
    |                (> r 0))))
    |(push)
    |(assert $f-non-negative)
    |(check-sat)
    |(pop)
    """.stripMargin
}
