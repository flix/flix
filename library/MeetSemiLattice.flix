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

//
// ## Tuples.
//

/**
 * Implementation of `MeetSemiLattice` for `(A, B)`.
 */
impl MeetSemiLattice[(A, B)] <= MeetSemiLattice[A], MeetSemiLattice[B] {
    def top: (A, B) = (⊤, ⊤)

    def glb(x: (A, B), y: (A, B)): Bool =
        let (a1, b1) = x in
        let (a2, b2) = y in
            (a1 ⊓ a2, b1 ⊓ b2)
}

/**
 * Implementation of `MeetSemiLattice` for `(A, B, C)`.
 */
impl MeetSemiLattice[(A, B, C)] <= MeetSemiLattice[A], MeetSemiLattice[B], MeetSemiLattice[C] {
    def top: (A, B, C) = (⊤, ⊤, ⊤)

    def glb(x: (A, B, C), y: (A, B, C)): Bool =
        let (a1, b1, c1) = x in
        let (a2, b2, c2) = y in
            (a1 ⊓ a2, b1 ⊓ b2, c1 ⊓ c2)
}

/**
 * Implementation of `MeetSemiLattice` for `(A, B, C, D)`.
 */
impl MeetSemiLattice[(A, B, C, D)] <= MeetSemiLattice[A], MeetSemiLattice[B], MeetSemiLattice[C], MeetSemiLattice[D] {
    def top: (A, B, C, D) = (⊤, ⊤, ⊤, ⊤)

    def glb(x: (A, B, C, D), y: (A, B, C, D)): Bool =
        let (a1, b1, c1, d1) = x in
        let (a2, b2, c2, d2) = y in
            (a1 ⊓ a2, b1 ⊓ b2, c1 ⊓ c2, d1 ⊓ d2)
}

/**
 * Implementation of `MeetSemiLattice` for `(A, B, C, D, E)`.
 */
impl MeetSemiLattice[(A, B, C, D, E)] <= MeetSemiLattice[A], MeetSemiLattice[B], MeetSemiLattice[C], MeetSemiLattice[D], MeetSemiLattice[E] {
    def top: (A, B, C, D, E) = (⊤, ⊤, ⊤, ⊤, ⊤)

    def glb(x: (A, B, C, D, E), y: (A, B, C, D, E)): Bool =
        let (a1, b1, c1, d1, e1) = x in
        let (a2, b2, c2, d2, e2) = y in
            (a1 ⊓ a2, b1 ⊓ b2, c1 ⊓ c2, d1 ⊓ d2, e1 ⊓ e2)
}

//
// ## Options.
//

/**
 * Implementation of `MeetSemiLattice` for `Opt[A]`.
 *
 * Here `None` is considered as the smallest element.
 */
impl MeetSemiLattice[Opt[A]] <= MeetSemiLattice[A] {
    def glb(x: Opt[A], y: Opt[A]): Bool = match (x, y) with {
        case (None, _) => None
        case (_, None) => None
        case (Some(v1), Some(v2)) => Some(v1 ⊓ v2)
    }
}

//
// ## Lists.
//

/**
 * Implementation of `JoinSemiLattice` for `List[A]`.
 */
impl MeetSemiLattice[List[A]] <= MeetSemiLattice[A] {
    def top: List[A] = ??? // TODO: how

    def glb(x: List[A], y: List[A]): List[A] = ??? // TODO: Need a way to zip over the longest list.
}

//
// ## Sets.
//

// See `IncSet` and `DecSet`.

//
// ## Maps.
//

/**
 * Implementation of `JoinSemiLattice` for `Map[A, B]`.
 */
impl MeetSemiLattice[Map[A, B]] <= MeetSemiLattice[B] {
    def top: Map[A, B] = ??? // TODO: How?

    def glb(x: Map[A, B], y: Map[A, B]): Bool =
        map/unionWith(glb, x, y)
}

//
// ## Vectors.
//

/**
 * Implementation of `PartialOrder` for `Vector[A]`.
 */
impl MeetSemiLattice[Vector[A]] <= MeetSemiLattice[A] {
    def bot: Vector[A] = ??? // TODO: How?

    def glb(x: Vector[A], y: Vector[A]): Vector[A] =
        vector/zipWith(glb, x, y)
}
