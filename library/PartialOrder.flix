/**
 * Types equipped with a partial order.
 *
 * Informally,
 *
 *  - If `leq(x, y)` then `x` is less than or equal to `y`.
 *  - If both `leq(x, y)` and `leq(y, x)` hold then `x == y`.
 *  - If neither `leq(x, y)` and `leq(y, x)` are true then `x` and `y` are incomparable.
 */
class PartialOrder[E] {

    /**
     * Returns `true` iff the element `x` is less than or equal to the element `y`.
     */
    def leq(x: E, y: E): Bool

    /**
     * Alias for `leq`.
     */
    def ⊑(x: E, y: E): Bool = leq(x, y)

    /**
     * The reflexivity law asserts that any element is less than or equal to itself.
     */
    law Reflexivity: Prop = ∀(x: E). x ⊑ x

    /**
     * The anti-symmetry law asserts that if `x` is less than or equal to `y`
     * and vice versa then the two elements must be equal.
     */
    law AntiSymmetry: Prop = ∀(x: E, y: E). (x ⊑ y ∧ y ⊑ x) → (x ≡ y)

    /**
     * The transitivity law asserts that if `x` less than or equal to `y`
     * and `y` is less than or equal to `z` then `x` must be less than or
     * equal to `z`.
     */
    law Transitivity: Prop = ∀(x: E, y: E, z: E). (x ⊑ y ∧ y ⊑ z) → (x ⊑ z)

}

/*
 * ## Implementations of [[PartialOrder]] for tuples.
 */

/**
 * Tuple (A, B)
 */
impl PartialOrder[(A, B)] <= PartialOrder[A], PartialOrder[B] {
    def leq(x: (A, B), y: (A, B)): Bool =
        let (a1, b1) = x in
        let (a2, b2) = y in
            leq(a1, a2) && leq(b1, b2)
}

/**
 * Tuple (A, B, C)
 */
impl PartialOrder[(A, B, C)] <= PartialOrder[A], PartialOrder[B], PartialOrder[C] {
    def leq(x: (A, B, C), y: (A, B, C)): Bool =
        let (a1, b1, c1) = x in
        let (a2, b2, c2) = y in
            leq(a1, a2) && leq(b1, b2) && leq(c1, c2)
}

/**
 * Tuple (A, B, C, D)
 */
impl PartialOrder[(A, B, C, D)] <= PartialOrder[A], PartialOrder[B], PartialOrder[C], PartialOrder[D] {
    def leq(x: (A, B, C, D), y: (A, B, C, D)): Bool =
        let (a1, b1, c1, d1) = x in
        let (a2, b2, c2, d2) = y in
            leq(a1, a2) && leq(b1, b2) && leq(c1, c2) && leq(d1, d2)
}

/**
 * Tuple (A, B, C, D, E)
 */
impl PartialOrder[(A, B, C, D, E)] <= PartialOrder[A], PartialOrder[B], PartialOrder[C], PartialOrder[D], PartialOrder[E] {
    def leq(x: (A, B, C, D, E), y: (A, B, C, D, E)): Bool =
        let (a1, b1, c1, d1, e1) = x in
        let (a2, b2, c2, d2, e2) = y in
            leq(a1, a2) && leq(b1, b2) && leq(c1, c2) && leq(d1, d2) && leq(e1, e2)
}
