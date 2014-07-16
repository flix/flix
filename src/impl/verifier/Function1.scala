package impl.verifier

import impl.logic.Symbol.{PredicateSymbol => PSym}
import syntax._

object Function1 {

  /**
   * Functional: ∀x, y. x = y ⇒ f(x) = f(y).
   */
  def isFunction(argSort: String, resSort: String, f: PSym): String = smt"""
    |;; Functional: ∀x, y. x = y ⇒ f(x) = f(y).
    |(define-fun $f-functional () Bool
    |    (forall ((x $argSort) (y $argSort) (r1 $resSort) (r2 $resSort))
    |        (=>
    |            (and
    |                (= x y)
    |                ($f x r1)
    |                ($f y r2))
    |                (= r1 r2))))
    |(push)
    |(assert $f-functional)
    |(check-sat)
    |(pop)
     """.stripMargin

  /**
   * Total: ∀x. ∃y. y = f(x).
   */
  def isTotal(argSort: String, resSort: String, f: PSym): String = smt"""
    | ;; Total: ∀x. ∃y. y = f(x).
    |(define-fun $f-total () Bool
    |    (forall ((x $argSort))
    |        (exists ((r $resSort))
    |            ($f x r))))
    |(push)
    |(assert $f-total)
    |(check-sat)
    |(pop)
     """.stripMargin
}
