package impl.verifier

object LatticeHeight {
  /**
   * Stricly-Decreasing: ∀x, y. x ⊑ y ∧ x != y ⇒ f(x) > f(y).
   */
  def strictlyDecreasing(typ: String, fn: String, leq: String): String = s"""
    |;; Stricly-Decreasing: ∀x, y. x ⊑ y ∧ x != y ⇒ f(x) > f(y).
    |(define-fun $fn-strictly-decreasing () Bool
    |    (forall ((x $typ) (y $typ) (r1 Int) (r2 Int))
    |        (=>
    |            (and (distinct x y)
    |                 ($leq x y)
    |                 ($fn x r1)
    |                 ($fn y r2))
    |            (> r1 r2))))
    |(push)
    |(assert  $fn-strictly-decreasing)
    |(check-sat)
    |(pop)
    """.stripMargin

  /**
   * Non-Negative: ∀x. f(x) > 0.
   */
  def nonNegative(typ: String, fn: String): String = s"""
    |;; Non-Negative: ∀x. f(x) > 0.
    |(define-fun $fn-non-negative () Bool
    |    (forall ((x $typ) (r Int))
    |        (=>
    |            ($fn x r)
    |                (> r 0))))
    |(push)
    |(assert $fn-non-negative)
    |(check-sat)
    |(pop)
    """.stripMargin
}
