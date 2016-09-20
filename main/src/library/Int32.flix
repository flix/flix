namespace Int32 {

    /**
     * Returns the minimum number representable by an `Int32`.
     */
    def minValue: Int32 = - 2 ** (size() - 1)

    /**
     * Returns the maximum number representable by an `Int32`.
     */
    def maxValue: Int32 = (2 ** (size() - 2) - 1) * 2 + 1 // Avoid arithmetic overflow

    /**
     * Returns the number of bits used to represent an `Int32`.
     */
    def size: Int32 = 32

    /**
     * Returns the smaller of `x` and `y`.
     */
    def min(x: Int32, y: Int32): Int32 = if (x <= y) x else y

    /**
     * Returns the larger of `x` and `y`.
     */
    def max(x: Int32, y: Int32): Int32 = if (x >= y) x else y

    /**
     * Returns the absolute value of `x`.
     */
    def abs(x: Int32): Int32 = if (x >= 0) x else -x

    /**
     * Returns the distance between `x` and `y`.
     */
    def dist(x: Int32, y: Int32): Int32 = abs(x - y)

    /**
     * Returns 1 if x > y, -1 if x < y, and 0 if x = y.
     * The sign of x - y
     */
    def compare(x: Int32, y: Int32): Int32 =
        if (x > y) 1 else (if (x < y) -1 else 0)

    /**
     * Returns 1 if x > 0, -1 if x < 0, and 0 if x = 0.
     * This sign of x
     */
    def signum(x: Int32): Int32 = compare(x, 0)

    /**
     * Returns the logical right shift of `x` by `distance`.
     * A zero is shifted into the leftmost position regardless
     * of sign extension.
     */
    def logicalRightShift(x: Int32, distance: Int32): Int32 =
        if (x < 0 && distance % size() != 0)
            ((x >> 1) & maxValue()) >> (distance - 1)
        else
            x >> distance

    /**
     * Returns the number of one-bits in the two's complement binary
     * representation of `x`
     */
    def bitCount(x: Int32): Int32 =
        if (x == 0)
            0
        else
            if (x % 2 != 0)
                1 + bitCount(logicalRightShift(x, 1))
            else
                bitCount(logicalRightShift(x, 1))

    /**
     * Returns the the value obtained by rotating the two's complement
     * binary representation of `x` right by `distance` bits
     */
    def rotateRight(x: Int32, distance: Int32): Int32 =
        (logicalRightShift(x, distance)) | (x << -distance)

    /**
     * Returns the the value obtained by rotating the two's complement
     * binary representation of `x` left by `distance` bits
     */
    def rotateLeft(x: Int32, distance: Int32): Int32 =
        (logicalRightShift(x, -distance)) | (x << distance)
}
