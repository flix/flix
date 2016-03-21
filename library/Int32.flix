namespace Int32 {

    /**
     * Returns the minimum number representable by an `Int32`.
     */
    def minValue: Int32 = - 2 ** 31

    /**
     * Returns the maximum number representable by an `Int32`.
     */
    def maxValue: Int32 = 2 ** 31 - 1

    /**
     * Returns the smaller of `x` and `y`.
     */
    def min(x: Int32, y: Int32): Int32 = if (x <= y) x else y

    /**
     * Returns the larger of `x` and `y`.
     */
    def max(x: Int32, y: Int32): Int32 = if (x <= y) x else y

}