/**
 * A meet semi lattice is a partial order equipped with a top element and a greatest lower bound.
 */
class MeetSemiLattice[E] => PartialOrder[E] {

    /**
     * Returns the greatest element of the lattice.
     */
    def top: E

    /**
     * Returns the greatest lower bound of the two elements `x` and `y`.
     */
    def glb(x: E, y: E): E


    /**
     * Alias for the greatest element of the lattice.
     */
    def ⊤: E

    /**
     * Alias for the greatest lower bound operator.
     */
    def ⊓(x: E, y: E): E = glb(x, y)

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

/*
 * ## Implementations of [[MeetSemiLattice]] for tuples.
 */

/**
 * Tuple (A, B)
 */
impl MeetSemiLattice[(A, B)] <= MeetSemiLattice[A], MeetSemiLattice[B] {
    def top: (A, B) = (⊤, ⊤)

    def glb(x: (A, B), y: (A, B)): Bool =
        let (a1, b1) = x in
        let (a2, b2) = y in
            (a1 ⊓ a2, b1 ⊓ b2)
}

/**
 * Tuple (A, B, D)
 */
impl MeetSemiLattice[(A, B, C)] <= MeetSemiLattice[A], MeetSemiLattice[B], MeetSemiLattice[C] {
    def top: (A, B, C) = (⊤, ⊤, ⊤)

    def glb(x: (A, B, C), y: (A, B, C)): Bool =
        let (a1, b1, c1) = x in
        let (a2, b2, c2) = y in
            (a1 ⊓ a2, b1 ⊓ b2, c1 ⊓ c2)
}

/**
 * Tuple (A, B, D, E)
 */
impl MeetSemiLattice[(A, B, C, D)] <= MeetSemiLattice[A], MeetSemiLattice[B], MeetSemiLattice[C], MeetSemiLattice[D] {
    def top: (A, B, C, D) = (⊤, ⊤, ⊤, ⊤)

    def glb(x: (A, B, C), y: (A, B, C)): Bool =
        let (a1, b1, c1) = x in
        let (a2, b2, c2) = y in
            (a1 ⊓ a2, b1 ⊓ b2, c1 ⊓ c2)
}

