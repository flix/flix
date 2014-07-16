package impl.verifier

import impl.logic.Symbol.{LatticeSymbol => LSym, PredicateSymbol => PSym}
import impl.logic.Value
import syntax._

object LatticeLeq {
  /**
   * Reflexivity: ∀x. x ⊑ x
   */
  def reflexivity(sort: LSym, leq: PSym): String = smt"""
    |;; Reflexivity: ∀x. x ⊑ x
    |(define-fun reflexivity () Bool
    |    (forall ((x $sort))
    |        ($leq x x)))
    |(push)
    |(assert reflexivity)
    |(check-sat)
    |(pop)
    """.stripMargin

  /**
   * Anti-symmetri: ∀x, y. x ⊑ y ∧ x ⊒ y ⇒ x = y
   */
  def antiSymmetri(sort: LSym, leq: PSym): String = smt"""
    |;; Anti-symmetri: ∀x, y. x ⊑ y ∧ x ⊒ y ⇒ x = y
    |(define-fun anti-symmetri () Bool
    |    (forall ((x $sort) (y $sort))
    |        (=>
    |            (and ($leq x y)
    |                 ($leq y x))
    |            (= x y))))
    |(push)
    |(assert anti-symmetri)
    |(check-sat)
    |(pop)
    """.stripMargin

  /**
   * Transitivity: ∀x, y, z. x ⊑ y ∧ y ⊑ z ⇒ x ⊑ z.
   */
  def transitivity(sort: LSym, leq: PSym): String = smt"""
    |;; Transitivity: ∀x, y, z. x ⊑ y ∧ y ⊑ z ⇒ x ⊑ z.
    |(define-fun transitivity () Bool
    |    (forall ((x $sort) (y $sort) (z $sort))
    |        (=>
    |            (and ($leq x y)
    |                 ($leq y z))
    |            ($leq x z))))
    |(push)
    |(assert transitivity)
    |(check-sat)
    |(pop)
    """.stripMargin

  /**
   * Least Element: ∀x. ⊥ ⊑ x.
   */
  def leastElement(sort: LSym, bot: Value, leq: PSym): String = smt"""
    |;; Least Element: ∀x. ⊥ ⊑ x.
    |(define-fun least-element () Bool
    |    (forall ((x $sort))
    |        ($leq $bot x)))
    |(push)
    |(assert least-element)
    |(check-sat)
    |(pop)
    """.stripMargin
}
