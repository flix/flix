/*
 * Copyright 2016 Liam Palmer
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

namespace Int32 {

    /**
     * Returns the minimum number representable by an `Int32`.
     */
    def minValue: Int32 = - 2 ** (size() - 1)

    /**
     * Returns the maximum number representable by an `Int32`.
     */
    def maxValue: Int32 = (2 ** (size() - 2) - 1) * 2 + 1
                          // Avoid arithmetic overflow

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
     * If this distance exceeds maxValue(), -1 is returned
     */
    def dist(x: Int32, y: Int32): Int32 = switch {
        case x >= 0 && y >= 0                   => abs(x - y)
        case x < 0 && y < 0                     => abs(x - y)
        case x == minValue() || y == minValue() => -1
        case minValue() + abs(x) + abs(y) >= 0  => -1
        case minValue() + abs(x) + abs(y) < 0   => abs(x - y)
    }

    /**
     * Returns 1 if x > y, -1 if x < y, and 0 if x = y.
     * The sign of x - y
     */
    def compare(x: Int32, y: Int32): Int32 = switch {
        case x == y  => 0
        case x < y   => -1
        case x > y   => 1
    }

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
    def bitCount(x: Int32): Int32 = switch {
        case x == 0     => 0
        case x % 2 != 0 => bitCount(logicalRightShift(x, 1)) + 1
        case x % 2 == 0 => bitCount(logicalRightShift(x, 1))
    }

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

    /**
     * Returns the position of the highest-order/leftmost one-bit in `x`
     * Possible return values: 0 (rightmost bit) - 31 (leftmost bit)
     *                         -1 if x = 0
     */
    def highestOneBitPosition(x: Int32): Int32 =
        // Start at bit 31 and scan right
        oneBitPositionHelper(x, 31, -1)

    /**
     * Returns the position of the lowest-order/rightmost one-bit in `x`
     * Possible return values: 0 (rightmost bit) - 31 (leftmost bit)
     *                         -1 if x = 0
     */
    def lowestOneBitPosition(x: Int32): Int32 =
        // Start at bit 0 and scan left
        oneBitPositionHelper(x, 0, 1)

    /**
     * Helper function for highestOneBitPosition and lowestOneBitPosition
     * If `x` has a one-bit at `position`, return that position
     * Otherwise recursively check the next bit in the same way
     */
    def oneBitPositionHelper(x: Int32, position: Int32, delta: Int32): Int32 = switch {
        case position < 0           => -1
        case position > 31          => -1
        case bit(x, position) == 1  => position
        case delta == 0             => -1
        case bit(x, position) == 0  => oneBitPositionHelper(x, position + delta, delta)
    }

    /**
     * Returns a value with at most a single one-bit, in the position
     * of the highest-order/leftmost one-bit in `x`
     * Returns 0 if x=0
     */
    def highestOneBit(x: Int32): Int32 =
        bitPositionToInt(highestOneBitPosition(x))

    /**
     * Returns a value with at most a single one-bit, in the position
     * of the highest-order/leftmost one-bit in `x`
     * Returns 0 if x=0
     */
    def lowestOneBit(x: Int32): Int32 =
        bitPositionToInt(lowestOneBitPosition(x))

    /**
     * Helper function for highestOneBit and lowestOneBit
     * Returns a value with a single one-bit at bit number `position`
     * Returns 0 if `position` is outside the range 0-31 inclusive
     */
    def bitPositionToInt(position: Int32): Int32 = switch {
        case position < 0 || position > 31  => 0
        case position == 31                 => minValue()
        case position != 31                 => 1 << position
    }

    /**
     * Returns the number of zero bits preceding the
     * highest-order/leftmost one-bit in `x`.
     * Returns 32 if x=0.
     */
    def numberOfLeadingZeros(x: Int32): Int32 =
        if (x == 0) 32 else 31 - highestOneBitPosition(x)

    /**
     * Returns the number of zero bits following the
     * lowest-order/rightmost one-bit in `x`.
     * Returns 32 if x=0.
     */
    def numberOfTrailingZeros(x: Int32): Int32 =
        if (x == 0) 32 else lowestOneBitPosition(x)

    /**
     * Returns the bit of `x` at `position` (either 0 or 1).
     * Considers the 5 rightmost bits of `position` (`position` mod 32).
     * The bits of x have positions: 0 (rightmost bit) - 31 (leftmost bit)
     */
    def bit(x: Int32, position: Int32): Int32 =
        if ((x >> position) % 2 == 0) 0 else 1
}
