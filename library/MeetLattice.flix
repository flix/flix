/**
 * A meet semi lattice is a partial order equipped with a top element and a greatest lower bound.
 */
class MeetLattice[E] => PartialOrder[E] {

    /**
     * Returns the greatest lower bound of the two elements `x` and `y`.
     */
    def glb(x: E, y: E): E

    /**
     * Alias for the greatest lower bound operator.
     */
    def ⊓(x: E, y: E): E = glb(x, y)

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
 * Implementation of `MeetLattice` for `(A, B)`.
 */
impl MeetLattice[(A, B)] <= MeetLattice[A], MeetLattice[B] {
    def top: (A, B) = (⊤, ⊤)

    def glb(x: (A, B), y: (A, B)): Bool =
        let (a1, b1) = x in
        let (a2, b2) = y in
            (a1 ⊓ a2, b1 ⊓ b2)
}

/**
 * Implementation of `MeetLattice` for `(A, B, C)`.
 */
impl MeetLattice[(A, B, C)] <= MeetLattice[A], MeetLattice[B], MeetLattice[C] {
    def top: (A, B, C) = (⊤, ⊤, ⊤)

    def glb(x: (A, B, C), y: (A, B, C)): Bool =
        let (a1, b1, c1) = x in
        let (a2, b2, c2) = y in
            (a1 ⊓ a2, b1 ⊓ b2, c1 ⊓ c2)
}

/**
 * Implementation of `MeetLattice` for `(A, B, C, D)`.
 */
impl MeetLattice[(A, B, C, D)] <= MeetLattice[A], MeetLattice[B], MeetLattice[C], MeetLattice[D] {
    def top: (A, B, C, D) = (⊤, ⊤, ⊤, ⊤)

    def glb(x: (A, B, C, D), y: (A, B, C, D)): Bool =
        let (a1, b1, c1, d1) = x in
        let (a2, b2, c2, d2) = y in
            (a1 ⊓ a2, b1 ⊓ b2, c1 ⊓ c2, d1 ⊓ d2)
}

/**
 * Implementation of `MeetLattice` for `(A, B, C, D, E)`.
 */
impl MeetLattice[(A, B, C, D, E)] <= MeetLattice[A], MeetLattice[B], MeetLattice[C], MeetLattice[D], MeetLattice[E] {
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
 * Implementation of `MeetLattice` for `Opt[A]`.
 *
 * Here `None` is considered as the smallest element.
 */
impl MeetLattice[Opt[A]] <= MeetLattice[A] {
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
 * Implementation of `MeetLattice` for `List[A]`.
 */
impl MeetLattice[List[A]] <= MeetLattice[A] {
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
 * Implementation of `MeetLattice` for `Map[A, B]`.
 */
impl MeetLattice[Map[A, B]] <= MeetLattice[B] {
    def top: Map[A, B] = ??? // TODO: How?

    def glb(x: Map[A, B], y: Map[A, B]): Bool =
        map/unionWith(glb, x, y)
}

//
// ## Vectors.
//

/**
 * Implementation of `MeetLattice` for `Vector[A]`.
 */
impl MeetLattice[Vector[A]] <= MeetLattice[A] {
    def top: Vector[A] = ??? // TODO: How?

    def glb(x: Vector[A], y: Vector[A]): Vector[A] =
        vector/zipWith(glb, x, y)
}
