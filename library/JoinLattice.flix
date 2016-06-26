/**
 * A join semi lattice is a partial order equipped with a bottom element and a least upper bound.
 */
class JoinLattice[E] => PartialOrder[E] {

    /**
     * Returns the least upper bound of the two elements `x` and `y`.
     */
    def lub(x: E, y: E): E

    /**
     * Alias for the least upper bound operator.
     */
    def ⊑(x: E, y: E): E = lub(x, y)

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
 * Implementation of `JoinLattice` for `(A, B)`.
 */
impl JoinLattice[(A, B)] <= JoinLattice[A], JoinLattice[B] {
    def bot: (A, B) = (⊥, ⊥)

    def lub(x: (A, B), y: (A, B)): Bool =
        let (a1, b1) = x in
        let (a2, b2) = y in
            (a1 ⊔ a2, b1 ⊔ b2)
}

/**
 * Implementation of `JoinLattice` for `(A, B, C)`.
 */
impl JoinLattice[(A, B, C)] <= JoinLattice[A], JoinLattice[B], JoinLattice[C] {
    def bot: (A, B, C) = (⊥, ⊥, ⊥)

    def lub(x: (A, B, C), y: (A, B, C)): Bool =
        let (a1, b1, c1) = x in
        let (a2, b2, c2) = y in
            (a1 ⊔ a2, b1 ⊔ b2, c1 ⊔ c2)
}

/**
 * Implementation of `JoinLattice` for `(A, B, C, D)`.
 */
impl JoinLattice[(A, B, C, D)] <= JoinLattice[A], JoinLattice[B], JoinLattice[C], JoinLattice[D] {
    def bot: (A, B, C, D) = (⊥, ⊥, ⊥, ⊥)

    def lub(x: (A, B, C, D), y: (A, B, C, D)): Bool =
        let (a1, b1, c1, d1) = x in
        let (a2, b2, c2, d2) = y in
            (a1 ⊔ a2, b1 ⊔ b2, c1 ⊔ c2, d1 ⊔ d2)
}

/**
 * Implementation of `JoinLattice` for `(A, B, C, D, E)`.
 */
impl JoinLattice[(A, B, C, D, E)] <= JoinLattice[A], JoinLattice[B], JoinLattice[C], JoinLattice[D], JoinLattice[E] {
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
 * Implementation of `JoinLattice` for `Opt[A]`.
 *
 * Here `None` is considered as the smallest element.
 */
impl JoinLattice[Opt[A]] <= JoinLattice[A] {
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
 * Implementation of `JoinLattice` for `List[A]`.
 */
impl JoinLattice[List[A]] <= JoinLattice[A] {
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
 * Implementation of `JoinLattice` for `Map[A, B]`.
 */
impl JoinLattice[Map[A, B]] <= JoinLattice[B] {
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
impl JoinLattice[Vector[A]] <= JoinLattice[A] {
    def bot: Vector[A] = #[]

    def lub(x: Vector[A], y: Vector[A]): Vector[A] =
        vector/zipWith(lub, x, y)
}
