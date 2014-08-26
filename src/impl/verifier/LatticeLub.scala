package impl.verifier

object LatticeLub {
  /**
   * Upper Bound: ∀x, y, z. x ⊑ x ⨆ y ∧ y ⊑ x ⨆ y.
   */
  def upperBound(typ: String, leq: String, lub: String): String = s"""
    |;; Upper Bound: ∀x, y, z. x ⊑ x ⨆ y ∧ y ⊑ x ⨆ y.
    |(define-fun $typ-upper-bound () Bool
    |    (forall ((x $typ) (y $typ) (z $typ))
    |        (and
    |            (=> ($lub x y z) ($leq x z))
    |            (=> ($lub x y z) ($leq y z)))))
    |(push)
    |(assert $typ-upper-bound)
    |(check-sat)
    |(pop)
    """.stripMargin

  /**
   * Least Upper Bound: ∀x, y, z. x ⊑ z ∧ y ⊑ z ⇒ x ⨆ y ⊑ z.
   */
  def leastUpperBound(typ: String, leq: String, lub: String): String = s"""
    |;; Least Upper Bound: ∀x, y, z. x ⊑ z ∧ y ⊑ z ⇒ x ⨆ y ⊑ z.
    |(define-fun $typ-least-upper-bound () Bool
    |    (forall ((x $typ) (y $typ) (z $typ) (w $typ))
    |        (=>
    |            (and ($leq x z)
    |                 ($leq y z)
    |                 ($lub x y w))
    |        ($leq w z))))
    |(push)
    |(assert $typ-least-upper-bound)
    |(check-sat)
    |(pop)
    """.stripMargin
}
