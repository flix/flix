/**
 * A join semi lattice is a partial order equipped with a bottom element and a least upper bound.
 */
class JoinSemiLattice[E] => PartialOrder[E] {

    /**
     * Returns the least element of the lattice.
     */
    fn bot: E

    /**
     * Returns the least upper bound of the two elements `x` and `y`.
     */
    fn lub(x: E, y: E): E

    /**
     * Alias for the least element of the lattice.
     */
    fn ⊥: E = bot

    /**
     * Alias for the least upper bound operator.
     */
    fn ⊑(x: E, y: E): E = lub(x, y)

    /**
     * The least element law asserts that every element of the lattice is
     * less than or equal to the bottom element.
     */
    law LeastElement: Prop = ∀(x: E). ⊥ ⊑ x

    /**
     * The upper bound law asserts that the least upper bound operator returns an
     * element that is greater than or equal to each of its arguments.
     */
    law UpperBound: Prop = ∀(x: E, y: E). (x ⊑ (x ⊔ y)) ∧ (y ⊑ (x ⊔ y))

    /**
     * The least upper bound law asserts that the least upper bound operator returns
     * the smallest element that is larger than its two arguments.
     */
    law LeastUpperBound: Prop = ∀(x: E, y: E, z: E). ((x ⊑ z) ∧ (y ⊑ z)) → (x ⊔ y) ⊑ z

}
