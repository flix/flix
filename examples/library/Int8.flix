namespace Int8 {

    /**
     * Returns the minimum number representable by an `Int8`.
     */
    def minValue: Int8 = - 2 ** 7

    /**
     * Returns the maximum number representable by an `Int8`.
     */
    def maxValue: Int8 = 2 ** 7 - 1

    /**
     * Returns the smaller of `x` and `y`.
     */
    def min(x: Int8, y: Int8): Int8 = if (x <= y) x else y

    /**
     * Returns the larger of `x` and `y`.
     */
    def max(x: Int8, y: Int8): Int8 = if (x <= y) x else y

}