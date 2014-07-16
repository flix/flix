package impl.verifier

import impl.logic.Symbol.{LatticeSymbol => LSym, PredicateSymbol => PSym}
import syntax._

object Function2 {
  /**
   * Functional: ∀x1, x2, y1, y2. (x1 = x2 ∧ y1 = y2) ⇒ f(x1, y1) = f(x2, y2).
   */
  def isFunction(name: String, sort: LSym, f: PSym): String = smt"""
    |;; Functional: ∀x1, x2, y1, y2. (x1 = x2 ∧ y1 = y2) ⇒ f(x1, y1) = f(x2, y2).
    |(define-fun $name () Bool
    |    (forall ((x1 $sort) (x2 $sort) (y1 $sort) (y2 $sort) (r1 $sort) (r2 $sort))
    |        (=>
    |            (and
    |                (= x1 x2)
    |                (= y1 y2)
    |                ($f x1 y1 r1)
    |                ($f x2 y2 r2))
    |        (= r1 r2))))
    |(assert $name)
    |(check-sat)
     """.stripMargin

  /**
   * Total: ∀x, y, ∃z. z = f(x, y).
   */
  def isTotal(name: String, sort: LSym, f: PSym): String = smt"""
    |;; Total: ∀x, y, ∃z. z = f(x, y).
    |(define-fun $name () Bool
    |    (forall ((x $sort) (y $sort))
    |        (exists ((z $sort))
    |            ($f x y z))))
    |(assert $name)
    |(check-sat)
     """.stripMargin
}
