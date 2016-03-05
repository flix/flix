// TODO: What kind of overloading can we allow for minValue, what about min??

namespace Int {

    /**
     * Returns the minimum number representable by an `Int32` equivalent to `-2^16`.
     */
    extern def minValue: Int32

    /**
     * Returns the maximum number representable by an `Int32` equivalent to `2^16 - 1`.
     */
    extern def maxValue: Int32

    /**
     * Returns the smaller of `x` and `y`.
     */
    extern def min(x: Int32, y: Int32): Int32

    /**
     * Returns the larger of `x` and `y`.
     */
    extern def max(x: Int32, y: Int32): Int32

}