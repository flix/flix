package impl.verifier

import impl.logic.Symbol.{LatticeSymbol => LSym, PredicateSymbol => PSym}
import syntax._

object Function1 {

  // TODO: Problem with types

  /**
   * Functional: ∀x, y. x = y ⇒ f(x) = f(y).
   */
  def isFunction(sort: LSym, f: PSym): String = smt"""
    |;; Functional: ∀x, y. x = y ⇒ f(x) = f(y).
    |(define-fun $f-functional () Bool
    |    (forall ((x $sort) (y $sort) (r1 Int) (r2 Int))
    |        (=>
    |            (and
    |                (= x y)
    |                ($f x r1)
    |                ($f y r2))
    |                (= r1 r2))))
    |(assert $f-functional)
    |(check-sat)
     """.stripMargin

  /**
   * Total: ∀x. ∃y. y = f(x).
   */
  def isTotal(sort: LSym, f: PSym): String = smt"""
    | ;; Total: ∀x. ∃y. y = f(x).
    |(define-fun $f-total () Bool
    |    (forall ((x $sort))
    |        (exists ((r Int))
    |            ($f x r))))
    |(assert $f-total)
    |(check-sat)
     """.stripMargin
}
