package impl.verifier

import impl.logic.Symbol.{LatticeSymbol => LSym, PredicateSymbol => PSym}
import impl.logic.Value
import syntax._

object Transfer {
  /**
   * Strict ∀x. f(⊥, x) = ⊥ ∧ f(x, ⊥) = ⊥.
   */
  def isStrict2(sort: LSym, bot: Value, f: PSym): String = smt"""
    |;; Strict ∀x. f(⊥, x) = ⊥ ∧ f(x, ⊥) = ⊥.
    |(define-fun $f-strict () Bool
    |    (forall ((x $sort))
    |        (and
    |            ($f $bot x $bot)
    |            ($f x $bot $bot))))
    |(push)
    |(assert $f-strict)
    |(check-sat)
    |(pop)
    """.stripMargin

  /**
   * Monotone: ∀x1, x2, y1, y2. x1 ⊑ x2 ∧ y1 ⊑ y2 ⇒ f(x1, y1) ⊑ f(x2, y2).
   */
  def isMonotone2(sort: LSym, f: PSym, leq: PSym): String = smt"""
    |;; Monotone: ∀x1, x2, y1, y2. x1 ⊑ x2 ∧ y1 ⊑ y2 ⇒ f(x1, y1) ⊑ f(x2, y2).
    |(define-fun sum-monotone () Bool
    |    (forall ((x1 $sort) (x2 $sort) (y1 $sort) (y2 $sort) (r1 $sort) (r2 $sort))
    |        (=>
    |            (and
    |                ($leq x1 x2)
    |                ($leq y1 y2)
    |                ($f x1 y1 r1)
    |                ($f x2 y2 r2))
    |                ($leq r1 r2))))
    |(push)
    |(assert $f-strict)
    |(check-sat)
    |(pop)
    """.stripMargin
}
