namespace Int16 {

    /**
     * Returns the minimum number representable by an `Int16`.
     */
    def minValue: Int16 = - 2 ** 15

    /**
     * Returns the maximum number representable by an `Int16`.
     */
    def maxValue: Int16 = 2 ** 15 - 1

    /**
     * Returns the smaller of `x` and `y`.
     */
    def min(x: Int16, y: Int16): Int16 = if (x <= y) x else y

    /**
     * Returns the larger of `x` and `y`.
     */
    def max(x: Int16, y: Int16): Int16 = if (x <= y) x else y

}