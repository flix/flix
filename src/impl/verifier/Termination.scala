package impl.verifier

import impl.logic.Symbol.{LatticeSymbol => LSym, PredicateSymbol => PSym}
import syntax._

object Termination {

  /**
   * Stricly-Decreasing: ∀x, y. x ⊑ y ∧ x != y ⇒ f(x) > f(y).
   */
  def dereasing(): String = smt"""
    |;; Stricly-Decreasing: ∀x, y. x ⊑ y ∧ x != y ⇒ f(x) > f(y).
    |(define-fun height-decreasing () Bool
    |    (forall ((x Sign) (y Sign) (h1 Int) (h2 Int))
    |        (=>
    (and (distinct x y)
      (Sign.leq x y)
      (Sign.height x h1)
      (Sign.height y h2))
    (> h1 h2))))
         |
         |"""".stripMargin



  //  ;; Height-Function: ∀x, y. x = y ⇒ h(x) = h(y)
  //  (define-fun height-function () Bool
  //    (forall ((x Sign) (y Sign) (r1 Int) (r2 Int))
  //      (=>
  //  (and
  //    (= x y)
  //  (Sign.height x r1)
  //  (Sign.height y r2))
  //  (= r1 r2))))
  //
  //  ;; Height-Total: ∀x. ∃y. y = h(x).
  //  (define-fun height-total () Bool
  //    (forall ((x Sign))
  //      (exists ((r Int))
  //        (Sign.height x r))))
  //
  //  ;; Height-NonNegative: ∀x. h(x) > 0.
  //  (define-fun height-non-negative () Bool
  //    (forall ((x Sign) (h Int))
  //      (=>
  //  (Sign.height x h)
  //  (> h 0))))
  //

}
