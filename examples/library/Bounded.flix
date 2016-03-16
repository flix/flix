/**
 * A type is bounded if it has some minimum and maximum value.
 */
class Bounded[E] {
    /**
     * Returns the smallest value of E.
     */
    def minValue: E

    /**
     * Returns the largest value of E.
     */
    def maxValue: E
}

/*
 * Implementations of Bounded.
 */
impl Bounded[Float32] {
    def minValue: Float32 = Float32/minValue
    def maxValue: Float32 = Float32/maxValue
}

impl Bounded[Float64] {
    def minValue: Float64 = Float64/minValue
    def maxValue: Float64 = Float64/maxValue
}

impl Bounded[Int8] {
    def minValue: Int8 = Int8/minValue
    def maxValue: Int8 = Int8/maxValue
}

impl Bounded[Int16] {
    def minValue: Int16 = Int16/minValue
    def maxValue: Int16 = Int16/maxValue
}

impl Bounded[Int32] {
    def minValue: Int32 = Int32/minValue
    def maxValue: Int32 = Int32/maxValue
}

impl Bounded[Int64] {
    def minValue: Int64 = Int64/minValue
    def maxValue: Int64 = Int64/maxValue
}

