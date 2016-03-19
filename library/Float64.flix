namespace Float64 {

    /**
     * Returns the minimum number representable by a `Float64`.
     */
    def minValue: Float64 = ??? : Float64 // TODO

    /**
     * Returns the maximum number representable by a `Float64`.
     */
    def maxValue: Float64 = ??? : Float64 // TODO

    /**
     * Returns the smaller of `x` and `y`.
     */
    def min(x: Float64, y: Float64): Float64 = if (x <= y) x else y

    /**
     * Returns the larger of `x` and `y`.
     */
    def max(x: Float64, y: Float64): Float64 = if (x <= y) x else y

}