namespace Int64 {

    /**
     * Returns the minimum number representable by an `Int64`.
     */
    def minValue: Int64 = - 2 ** 63

    /**
     * Returns the maximum number representable by an `Int64`.
     */
    def maxValue: Int64 = 2 ** 63 - 1

    /**
     * Returns the smaller of `x` and `y`.
     */
    def min(x: Int64, y: Int64): Int64 = if (x <= y) x else y

    /**
     * Returns the larger of `x` and `y`.
     */
    def max(x: Int64, y: Int64): Int64 = if (x <= y) x else y

}