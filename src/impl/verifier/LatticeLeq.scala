package impl.verifier

object LatticeLeq {
  /**
   * Reflexivity: ∀x. x ⊑ x
   */
  def reflexivity(typ: String, leq: String): String = s"""
    |;; Reflexivity: ∀x. x ⊑ x
    |(define-fun reflexivity () Bool
    |    (forall ((x $typ))
    |        ($leq x x)))
    |(push)
    |(assert reflexivity)
    |(check-sat)
    |(pop)
    """.stripMargin

  /**
   * Anti-symmetri: ∀x, y. x ⊑ y ∧ x ⊒ y ⇒ x = y
   */
  def antiSymmetri(typ: String, leq: String): String = s"""
    |;; Anti-symmetri: ∀x, y. x ⊑ y ∧ x ⊒ y ⇒ x = y
    |(define-fun anti-symmetri () Bool
    |    (forall ((x $typ) (y $typ))
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
  def transitivity(typ: String, leq: String): String = s"""
    |;; Transitivity: ∀x, y, z. x ⊑ y ∧ y ⊑ z ⇒ x ⊑ z.
    |(define-fun transitivity () Bool
    |    (forall ((x $typ) (y $typ) (z $typ))
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
  def leastElement(typ: String, bot: String, leq: String): String = s"""
    |;; Least Element: ∀x. ⊥ ⊑ x.
    |(define-fun least-element () Bool
    |    (forall ((x $typ))
    |        ($leq $bot x)))
    |(push)
    |(assert least-element)
    |(check-sat)
    |(pop)
    """.stripMargin
}
