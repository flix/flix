namespace Float32 {

    /**
     * Returns the minimum number representable by a `Float32`.
     */
    def minValue: Float32 = ??? : Float32 // TODO

    /**
     * Returns the maximum number representable by a `Float32`.
     */
    def maxValue: Float32 = ??? : Float32 // TODO

    /**
     * Returns the smaller of `x` and `y`.
     */
    def min(x: Float32, y: Float32): Float32 = if (x <= y) x else y

    /**
     * Returns the larger of `x` and `y`.
     */
    def max(x: Float32, y: Float32): Float32 = if (x <= y) x else y

}