/**
 * Types equipped with a maximum value.
 */
class MaxValue[E] => PartialOrder[E] {
    /**
     * Returns the largest value of E.
     */
    def maxValue: E

    /**
     * The largest value law asserts that the `MaxValue` is indeed the largest value.
     */
    law LargestValue: Prop = ∀(x: E). MaxValue <= x
}

//
// ## Floats
//
impl MaxValue[Float32] {
    def MaxValue: Float32 = Float32/maxValue
}

impl MaxValue[Float64] {
    def MaxValue: Float64 = Float64/maxValue
}

//
// # Ints
//
impl MaxValue[Int8] {
    def MaxValue: Int8 = Int8/maxValue
}

impl MaxValue[Int16] {
    def MaxValue: Int16 = Int16/maxValue
}

impl MaxValue[Int32] {
    def MaxValue: Int32 = Int32/maxValue
}

impl MaxValue[Int64] {
    def MaxValue: Int64 = Int64/maxValue
}

// TODO: UInt

//
// # Tuples
//

/**
 * Implementation of `MaxValue` for `(A, B)`.
 */
impl MaxValue[(A, B)] <= MaxValue[A], MaxValue[B] {
    def MaxValue: (A, B) = (maxValue, maxValue)
}

/**
 * Implementation of `MaxValue` for `(A, B, C)`.
 */
impl MaxValue[(A, B, C)] <= MaxValue[A], MaxValue[B], MaxValue[C] {
    def MaxValue: (A, B, C) = (maxValue, maxValue, maxValue)
}

/**
 * Implementation of `MaxValue` for `(A, B, C, D)`.
 */
impl MaxValue[(A, B, C, D)] <= MaxValue[A], MaxValue[B], MaxValue[C], MaxValue[D] {
    def MaxValue: (A, B, C, D) = (maxValue, maxValue, maxValue, maxValue)
}

/**
 * Implementation of `MaxValue` for `(A, B, C, D, E)`.
 */
impl MaxValue[(A, B, C, D, E)] <= MaxValue[A], MaxValue[B], MaxValue[C], MaxValue[D], MaxValue[E] {
    def MaxValue: (A, B, C, D, E) = (maxValue, maxValue, maxValue, maxValue, maxValue)
}

// TODO: Others?
