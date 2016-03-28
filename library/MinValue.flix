/**
 * Types equipped with a minimum value.
 */
class MinValue[E] => PartialOrder[E] {
    /**
     * Returns the smallest value of E.
     */
    def minValue: E

    /**
     * The smallest value law asserts that the `minValue` is indeed the smallest value.
     */
    law SmallestValue: Prop = ∀(x: E). minValue <= x
}

//
// ## Floats
//
impl MinValue[Float32] {
    def minValue: Float32 = Float32/minValue
}

impl MinValue[Float64] {
    def minValue: Float64 = Float64/minValue
}

//
// # Ints
//
impl MinValue[Int8] {
    def minValue: Int8 = Int8/minValue
}

impl MinValue[Int16] {
    def minValue: Int16 = Int16/minValue
}

impl MinValue[Int32] {
    def minValue: Int32 = Int32/minValue
}

impl MinValue[Int64] {
    def minValue: Int64 = Int64/minValue
}

// TODO: UInt

//
// # Tuples
//

/**
 * Implementation of `MinValue` for `(A, B)`.
 */
impl MinValue[(A, B)] <= MinValue[A], MinValue[B] {
    def minValue: (A, B) = (minValue, minValue)
}

/**
 * Implementation of `MinValue` for `(A, B, C)`.
 */
impl MinValue[(A, B, C)] <= MinValue[A], MinValue[B], MinValue[C] {
    def minValue: (A, B, C) = (minValue, minValue, minValue)
}

/**
 * Implementation of `MinValue` for `(A, B, C, D)`.
 */
impl MinValue[(A, B, C, D)] <= MinValue[A], MinValue[B], MinValue[C], MinValue[D] {
    def minValue: (A, B, C, D) = (minValue, minValue, minValue, minValue)
}

/**
 * Implementation of `MinValue` for `(A, B, C, D, E)`.
 */
impl MinValue[(A, B, C, D, E)] <= MinValue[A], MinValue[B], MinValue[C], MinValue[D], MinValue[E] {
    def minValue: (A, B, C, D, E) = (minValue, minValue, minValue, minValue, minValue)
}

//
// # Sets
//

/**
 * Implementation of `MinValue` for `Set[A]`.
 */
impl MinValue[Set[A]] {
    def minValue: Set[A] = #{}
}

// TODO: Others?

