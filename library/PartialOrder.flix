/**
 * Types equipped with a partial order.
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

//
// ## Total Orders.
//

/**
 * Implementation of `PartialOrder` for `TotalOrder[A]`.
 */
impl PartialOrder[A] <= Ord[A] {
    def leq(x: A, y: B): Bool = leq(x, y)
}

//
// ## Tuples.
//

/**
 * Implementation of `PartialOrder` for `(A, B)`.
 */
impl PartialOrder[(A, B)] <= PartialOrder[A], PartialOrder[B] {
    def leq(x: (A, B), y: (A, B)): Bool =
        let (a1, b1) = x in
        let (a2, b2) = y in
            (a1 ⊑ a2) && (b1 ⊑ b2)
}

/**
 * Implementation of `PartialOrder` for `(A, B, C)`.
 */
impl PartialOrder[(A, B, C)] <= PartialOrder[A], PartialOrder[B], PartialOrder[C] {
    def leq(x: (A, B, C), y: (A, B, C)): Bool =
        let (a1, b1, c1) = x in
        let (a2, b2, c2) = y in
            (a1 ⊑ a2) && (b1 ⊑ b2) && (c1 ⊑ c2)
}

/**
 * Implementation of `PartialOrder` for `(A, B, C, D)`.
 */
impl PartialOrder[(A, B, C, D)] <= PartialOrder[A], PartialOrder[B], PartialOrder[C], PartialOrder[D] {
    def leq(x: (A, B, C, D), y: (A, B, C, D)): Bool =
        let (a1, b1, c1, d1) = x in
        let (a2, b2, c2, d2) = y in
            (a1 ⊑ a2) && (b1 ⊑ b2) && (c1 ⊑ c2) && (d1 ⊑ d2)
}

/**
 * Implementation of `PartialOrder` for `(A, B, C, D, E)`.
 */
impl PartialOrder[(A, B, C, D, E)] <= PartialOrder[A], PartialOrder[B], PartialOrder[C], PartialOrder[D], PartialOrder[E] {
    def leq(x: (A, B, C, D, E), y: (A, B, C, D, E)): Bool =
        let (a1, b1, c1, d1, e1) = x in
        let (a2, b2, c2, d2, e2) = y in
            (a1 ⊑ a2) && (b1 ⊑ b2) && (c1 ⊑ c2) && (d1 ⊑ d2) && (e1 ⊑ e2)
}

//
// ## Options.
//

/**
 * Implementation of `PartialOrder` for `Opt[A]`.
 *
 * Here `None` is considered as the smallest element.
 */
impl PartialOrder[Opt[A]] <= PartialOrder[A] {
    def leq(x: Opt[A], y: Opt[A]): Bool = match (x, y) with {
        case (None, _) => true
        case (Some(v1), Some(v2)) => v1 ⊑ v2
        case _ => false
    }
}

//
// ## Lists.
//

/**
 * Implementation of `PartialOrder` for `List[A]`.
 *
 * A list `x` is smaller than a list `y` if it has fewer or the same number elements
 * and each element is pairwise smaller according to the partial order on `A`.
 */
impl PartialOrder[List[A]] <= PartialOrder[A] {
    def leq(x: List[A], y: List[A]): Bool =
        (length(x) <= length(y)) && list/and(list/zipWith(leq, x, y))
}

//
// ## Sets.
//

// See `IncSet` and `DecSet`.

//
// ## Maps.
//

/**
 * Implementation of `PartialOrder` for `Map[A, B]`.
 *
 * A map `x` is smaller than a map `y` if every key in `x` is in `y`
 * and the value of every key in `x` is smaller than its corresponding value in `y`.
 */
impl PartialOrder[Map[A, B]] <= PartialOrder[B] {
    def leq(x: Map[A, B], y: Map[A, B]): Bool =
        let f = k -> lookup(k, x) `leq` lookup(k, y) in
            forall(f, keysOf(x))
}

//
// ## Vectors.
//

/**
 * Implementation of `PartialOrder` for `Vector[A]`.
 *
  * A vector `x` is smaller than a vector `y` if it has fewer or the same number elements
  * and each element is pairwise smaller according to the partial order on `A`.
 */
impl PartialOrder[Vector[A]] <= PartialOrder[A] {
    def leq(x: Vector[A], y: Vector[A]): Bool =
        (length(x) <= length(y)) && list/and(list/zipWith(leq, x, y))
}
