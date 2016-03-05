// TODO: What kind of overloading can we allow for minValue, what about min??

namespace Int {

    /**
     * Returns the minimum number representable by an `Int32` equivalent to `-2^16`.
     */
    external def minValue: Int32

    /**
     * Returns the maximum number representable by an `Int32` equivalent to `2^16 - 1`.
     */
    external def maxValue: Int32

    /**
     * Returns the smaller of `x` and `y`.
     */
    external def min(x: Int32, y: Int32): Int32

    /**
     * Returns the larger of `x` and `y`.
     */
    external def max(x: Int32, y: Int32): Int32

}