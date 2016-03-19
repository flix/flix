/**
 * A meet semi lattice is a partial order equipped with a top element and a greatest lower bound.
 */
class MeetSemiLattice[E] => PartialOrder[E] {

    /**
     * Returns the greatest element of the lattice.
     */
    fn top: E

    /**
     * Returns the greatest lower bound of the two elements `x` and `y`.
     */
    fn glb(x: E, y: E): E


    /**
     * Alias for the greatest element of the lattice.
     */
    fn ⊤: E

    /**
     * Alias for the greatest lower bound operator.
     */
    fn ⊓(x: E, y: E): E = glb(x, y)

    /**
     * The greatest element law asserts that every element of the lattice is
     * greater than or equal to the top element.
     */
    law GreatestElement: Prop = ∀(x: E). x ⊑ ⊤

    /**
     * The lower bound law asserts that the greatest lower bound operator returns an
     * element that is less than or equal to each of its arguments.
     */
    law LowerBound: Prop = ∀(x: E, y: E). ((x ⊓ y) ⊑ x) ∧ ((x ⊓ y) ⊑ y)

    /**
     * The greatest lower bound law asserts that the greatest lower bound operator returns
     * the largest element that is smaller than its two arguments.
     */
    law GreatestLowerBound: Prop = ∀(x: E, y: E, z: E). ((z ⊑ x) ∧ (z ⊑ y)) → (z ⊑ (x ⊓ y))

}
