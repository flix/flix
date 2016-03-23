/**
 * A join semi lattice is a partial order equipped with a bottom element and a least upper bound.
 */
class JoinSemiLattice[E] => PartialOrder[E] {

    /**
     * Returns the least element of the lattice.
     */
    def bot: E

    /**
     * Returns the least upper bound of the two elements `x` and `y`.
     */
    def lub(x: E, y: E): E

    /**
     * Alias for the least element of the lattice.
     */
    def ⊥: E = bot

    /**
     * Alias for the least upper bound operator.
     */
    def ⊑(x: E, y: E): E = lub(x, y)

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

//
// ## Tuples.
//

/**
 * Implementation of `JoinSemiLattice` for `(A, B)`.
 */
impl JoinSemiLattice[(A, B)] <= JoinSemiLattice[A], JoinSemiLattice[B] {
    def bot: (A, B) = (⊥, ⊥)

    def lub(x: (A, B), y: (A, B)): Bool =
        let (a1, b1) = x in
        let (a2, b2) = y in
            (a1 ⊔ a2, b1 ⊔ b2)
}

/**
 * Implementation of `JoinSemiLattice` for `(A, B, C)`.
 */
impl JoinSemiLattice[(A, B, C)] <= JoinSemiLattice[A], JoinSemiLattice[B], JoinSemiLattice[C] {
    def bot: (A, B, C) = (⊥, ⊥, ⊥)

    def lub(x: (A, B, C), y: (A, B, C)): Bool =
        let (a1, b1, c1) = x in
        let (a2, b2, c2) = y in
            (a1 ⊔ a2, b1 ⊔ b2, c1 ⊔ c2)
}

/**
 * Implementation of `JoinSemiLattice` for `(A, B, C, D)`.
 */
impl JoinSemiLattice[(A, B, C, D)] <= JoinSemiLattice[A], JoinSemiLattice[B], JoinSemiLattice[C], JoinSemiLattice[D] {
    def bot: (A, B, C, D) = (⊥, ⊥, ⊥, ⊥)

    def lub(x: (A, B, C, D), y: (A, B, C, D)): Bool =
        let (a1, b1, c1, d1) = x in
        let (a2, b2, c2, d2) = y in
            (a1 ⊔ a2, b1 ⊔ b2, c1 ⊔ c2, d1 ⊔ d2)
}

/**
 * Implementation of `JoinSemiLattice` for `(A, B, C, D, E)`.
 */
impl JoinSemiLattice[(A, B, C, D, E)] <= JoinSemiLattice[A], JoinSemiLattice[B], JoinSemiLattice[C], JoinSemiLattice[D], JoinSemiLattice[E] {
    def bot: (A, B, C, D, E) = (⊥, ⊥, ⊥, ⊥, ⊥)

    def lub(x: (A, B, C, D, E), y: (A, B, C, D, E)): Bool =
        let (a1, b1, c1, d1, e1) = x in
        let (a2, b2, c2, d2, e2) = y in
            (a1 ⊔ a2, b1 ⊔ b2, c1 ⊔ c2, d1 ⊔ d2, e1 ⊔ e2)
}

//
// ## Options.
//

/**
 * Implementation of `JoinSemiLattice` for `Opt[A]`.
 *
 * Here `None` is considered as the smallest element.
 */
impl JoinSemiLattice[Opt[A]] <= JoinSemiLattice[A] {
    def bot: Opt[A] = None

    def lub(x: Opt[A], y: Opt[A]): Opt[A] = match (x, y) with {
        case (None, _) => y
        case (_, None) => x
        case (Some(v1), Some(v2)) => Some(v1 ⊔ v2)
    }
}

//
// ## Lists.
//

/**
 * Implementation of `JoinSemiLattice` for `List[A]`.
 */
impl JoinSemiLattice[List[A]] <= JoinSemiLattice[A] {
    def bot: List[A] = Nil

    def lub(x: List[A], y: List[A]): List[A] = ??? // TODO: Need a way to zip over the longest list.
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
impl JoinSemiLattice[Map[A, B]] <= JoinSemiLattice[B] {
    def bot: Map[A, B] = @{}

    def lub(x: Map[A, B], y: Map[A, B]): Bool =
        map/unionWith(lub, x, y)
}

//
// ## Vectors.
//

/**
 * Implementation of `PartialOrder` for `Vector[A]`.
 */
impl JoinSemiLattice[Vector[A]] <= JoinSemiLattice[A] {
    def bot: Vector[A] = #[]

    def lub(x: Vector[A], y: Vector[A]): Vector[A] =
        vector/zipWith(lub, x, y)
}
