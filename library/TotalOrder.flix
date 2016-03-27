/**
 * Types equipped with a total order.
 *
 * A total order is a partial order with one additional property:
 * Every pair of elements must be comparable.
 */
class TotalOrder[E] => PartialOrder[E] {

    /*
     * No defs. `TotalOrder` is a marker interface.
     */

    /**
     * The totality law asserts that either `x` is less than or equal to `y`
     * or `y` is less than or equal to `x`, i.e. every pair of elements must
     * be mutually comparable.
     */
    law Totality: Prop = ∀(x: E, y: E). (x ⊑ y) ∨ (y ⊑ x)

}

//
// ## Ints
//

impl TotalOrder[Int8] {
    /* no defs */
}

impl TotalOrder[Int16] {
    /* no defs */
}

impl TotalOrder[Int32] {
    /* no defs */
}

impl TotalOrder[Int64] {
    /* no defs */
}

//
// ## Tuples
//

/**
 * Implementation of `TotalOrder` for `(A, B)`.
 */
impl TotalOrder[(A, B)] <= TotalOrder[A], TotalOrder[B] {
    /* no defs */
}

/**
 * Implementation of `TotalOrder` for `(A, B, C)`.
 */
impl TotalOrder[(A, B, C)] <= TotalOrder[A], TotalOrder[B], TotalOrder[C] {
    /* no defs */
}

/**
 * Implementation of `TotalOrder` for `(A, B, C, D)`.
 */
impl TotalOrder[(A, B, C, D)] <= TotalOrder[A], TotalOrder[B], TotalOrder[C], TotalOrder[D] {
    /* no defs */
}

/**
 * Implementation of `TotalOrder` for `(A, B, C, D, E)`.
 */
impl TotalOrder[(A, B, C, D, E)] <= TotalOrder[A], TotalOrder[B], TotalOrder[C], TotalOrder[D], TotalOrder[E] {
    /* no defs */

