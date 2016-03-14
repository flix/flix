class Bounded[E] {

    def minValue: E

    def maxValue: E
}

/*
 * Bounded Type Class Implementations.
 */
impl Bounded[Int8] {
    def minValue: Int8 = Int8/minValue
    def maxValue: Int8 = Int8/maxValue
}

