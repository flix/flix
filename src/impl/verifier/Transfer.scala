package impl.verifier

object Transfer {
  /**
   * Strict ∀x. f(⊥, x) = ⊥ ∧ f(x, ⊥) = ⊥.
   */
  def isStrict(typ: String, bot: String, fn: String): String = s"""
    |;; Strict ∀x. f(⊥, x) = ⊥ ∧ f(x, ⊥) = ⊥.
    |(define-fun $fn-strict () Bool
    |    (forall ((x $typ))
    |        (and
    |            ($fn $bot x $bot)
    |            ($fn x $bot $bot))))
    |(push)
    |(assert $fn-strict)
    |(check-sat)
    |(pop)
    """.stripMargin

  /**
   * Monotone: ∀x1, x2, y1, y2. x1 ⊑ x2 ∧ y1 ⊑ y2 ⇒ f(x1, y1) ⊑ f(x2, y2).
   */
  def isMonotone(typ: String, fn: String, leq: String): String = s"""
    |;; Monotone: ∀x1, x2, y1, y2. x1 ⊑ x2 ∧ y1 ⊑ y2 ⇒ f(x1, y1) ⊑ f(x2, y2).
    |(define-fun $fn-monotone () Bool
    |    (forall ((x1 $typ) (x2 $typ) (y1 $typ) (y2 $typ) (r1 $typ) (r2 $typ))
    |        (=>
    |            (and
    |                ($leq x1 x2)
    |                ($leq y1 y2)
    |                ($fn x1 y1 r1)
    |                ($fn x2 y2 r2))
    |                ($leq r1 r2))))
    |(push)
    |(assert $fn-monotone)
    |(check-sat)
    |(pop)
    """.stripMargin
}
