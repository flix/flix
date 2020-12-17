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

namespace Int8 {

    ///
    /// Returns the number of bits used to represent an `Int8`.
    ///
    @Time(1) @Space(1)
    pub def size(): Int32 = 8

    ///
    /// Returns the minimum number representable by an `Int8`.
    ///
    @Time(1) @Space(1)
    pub def minValue(): Int8 = 1i8 <<< (size() - 1)

    ///
    /// Returns the maximum number representable by an `Int8`.
    ///
    @Time(1) @Space(1)
    pub def maxValue(): Int8 = ~~~minValue()

    ///
    /// Returns the smaller of `x` and `y`.
    ///
    @Time(1) @Space(1)
    pub def min(x: Int8, y: Int8): Int8 = if (x <= y) x else y

    ///
    /// Returns the larger of `x` and `y`.
    ///
    @Time(1) @Space(1)
    pub def max(x: Int8, y: Int8): Int8 = if (x >= y) x else y

    ///
    /// Returns the absolute value of `x`.
    /// If the absolute value exceeds maxValue(), -1 is returned.
    ///
    @Time(1) @Space(1)
    pub def abs(x: Int8): Int8 = {
        if      (x >= 0i8)          x
        else if (x == minValue())   -1i8
        else                        -x
    }

    ///
    /// Returns the distance between `x` and `y`.
    /// If this distance exceeds maxValue(), -1 is returned.
    ///
    @Time(1) @Space(1)
    pub def dist(x: Int8, y: Int8): Int8 = {
        if      (x >= 0i8 and y >= 0i8)                  abs(x - y)
        else if (x < 0i8 and y < 0i8)                    abs(x - y)
        else if (x == minValue() or y == minValue())    -1i8
        else if (minValue() + abs(x) + abs(y) >= 0i8)   -1i8
        else                                            abs(x - y)
    }

    ///
    /// Returns 1 if x > y, -1 if x < y, and 0 if x = y.
    /// The sign of x - y.
    ///
    @Time(1) @Space(1)
    pub def compare(x: Int8, y: Int8): Int32 = {
        if      (x == y)    0
        else if (x < y)     -1
        else                1
    }

    ///
    /// Returns 1 if x > 0, -1 if x < 0, and 0 if x = 0.
    /// The sign of x.
    ///
    @Time(1) @Space(1)
    pub def signum(x: Int8): Int32 = compare(x, 0i8)

    ///
    /// Returns the logical right shift of `x` by `distance`.
    /// Only the rightmost 5 bits of `distance` are considered (ie. `distance % 32`).
    /// A zero is shifted into the leftmost position regardless of sign extension.
    ///
    @Time(1) @Space(1)
    pub def logicalRightShift(x: Int8, distance: Int32): Int8 =
        if (x < 0i8 and distance % 32 != 0)
            ((x >>> 1) &&& maxValue()) >>> (distance - 1)
        else
            x >>> distance

    ///
    /// Returns the number of one-bits in the two's complement binary
    /// representation of `x`.
    ///
    @Time(1) @Space(1)
    pub def bitCount(x: Int8): Int32 = {
        if      (x == 0i8)          0
        else if (x % 2i8 != 0i8)    bitCount(logicalRightShift(x, 1)) + 1
        else                        bitCount(logicalRightShift(x, 1))
    }

    ///
    /// Returns the the value obtained by rotating the two's complement
    /// binary representation of `x` right by `distance` bits.
    ///
    @Time(1) @Space(1)
    pub def rotateRight(x: Int8, distance: Int32): Int8 =
        let rem = distance % size();
        let rot = if (rem >= 0) rem else rem + size();
        (logicalRightShift(x, rot)) ||| (x <<< (size() - rot))

    ///
    /// Returns the the value obtained by rotating the two's complement
    /// binary representation of `x` left by `distance` bits.
    ///
    @Time(1) @Space(1)
    pub def rotateLeft(x: Int8, distance: Int32): Int8 =
        let rem = distance % size();
        let rot = if (rem >= 0) rem else rem + size();
        (logicalRightShift(x, size() - rot)) ||| (x <<< rot)

    ///
    /// Returns the value obtained by reversing the bits in the
    /// two's complement binary representation of `x`.
    ///
    @Time(1) @Space(1)
    pub def reverse(x: Int8): Int8 = reverseHelper(x, 0, size()-1)

    ///
    /// Helper function for `reverse`.
    ///
    def reverseHelper(x: Int8, l: Int32, r: Int32): Int8 =
        if (l >= r) x else reverseHelper(swap(x, l, r), l+1, r-1)

    ///
    /// Helper function for `reverse`.
    ///
    def swap(x: Int8, l: Int32, r: Int32): Int8 = match (getBit(x, l), getBit(x, r)) {
        case (1, 0) => clearBit(setBit(x, r), l)
        case (0, 1) => clearBit(setBit(x, l), r)
        case _ => x
    }

    ///
    /// Returns the position of the highest-order/leftmost one-bit in `x`.
    /// Possible return values: 0 (rightmost bit) - 7 (leftmost bit)
    ///                         -1 if x = 0
    ///
    @Time(1) @Space(1)
    pub def highestOneBitPosition(x: Int8): Int32 =
        // Start at bit 7 and scan right
        oneBitPositionHelper(x, size() - 1, -1)

    ///
    /// Returns the position of the lowest-order/rightmost one-bit in `x`.
    /// Possible return values: 0 (rightmost bit) - 7 (leftmost bit)
    ///                         -1 if x = 0
    ///
    @Time(1) @Space(1)
    pub def lowestOneBitPosition(x: Int8): Int32 =
        // Start at bit 0 and scan left
        oneBitPositionHelper(x, 0, 1)

    ///
    /// Helper function for highestOneBitPosition and lowestOneBitPosition.
    /// If `x` has a one-bit at `position`, return that position.
    /// Otherwise recursively check the next bit in the same way.
    ///
    def oneBitPositionHelper(x: Int8, position: Int32, delta: Int32): Int32 = {
        if      (position < 0)              -1
        else if (position > (size() - 1))   -1
        else if (getBit(x, position) == 1)  position
        else if (delta == 0)                -1
        else                                oneBitPositionHelper(x, position + delta, delta)
    }

    ///
    /// Returns a value with at most a single one-bit, in the position
    /// of the highest-order/leftmost one-bit in `x`.
    /// Returns 0 if x=0.
    ///
    @Time(1) @Space(1)
    pub def highestOneBit(x: Int8): Int8 =
        bitPositionToInt(highestOneBitPosition(x))

    ///
    /// Returns a value with at most a single one-bit, in the position
    /// of the highest-order/leftmost one-bit in `x`.
    /// Returns 0 if x=0.
    ///
    @Time(1) @Space(1)
    pub def lowestOneBit(x: Int8): Int8 =
        bitPositionToInt(lowestOneBitPosition(x))

    ///
    /// Helper function for highestOneBit and lowestOneBit.
    /// Returns a value with a single one-bit at bit number `position`.
    /// Returns 0 if `position` is outside the range 0-7 inclusive.
    ///
    def bitPositionToInt(position: Int32): Int8 =
        if (position < 0 or position > size() - 1) 0i8 else 1i8 <<< position

    ///
    /// Returns the number of zero bits preceding the
    /// highest-order/leftmost one-bit in `x`.
    /// Returns 8 if x=0.
    ///
    @Time(1) @Space(1)
    pub def numberOfLeadingZeros(x: Int8): Int32 =
        if (x == 0i8) size() else size() - 1 - highestOneBitPosition(x)

    ///
    /// Returns the number of zero bits following the
    /// lowest-order/rightmost one-bit in `x`.
    /// Returns 8 if x=0.
    ///
    @Time(1) @Space(1)
    pub def numberOfTrailingZeros(x: Int8): Int32 =
        if (x == 0i8) size() else lowestOneBitPosition(x)

    ///
    /// Returns the bit of `x` at `position` (either 0 or 1).
    /// Considers the 5 rightmost bits of `position` (`position` mod 32).
    /// The bits of x have positions: 0 (rightmost bit) - 7 (leftmost bit)
    ///
    @Time(1) @Space(1)
    pub def getBit(x: Int8, position: Int32): Int32 =
        if ((x >>> position) % 2i8 == 0i8) 0 else 1

    ///
    /// Returns `x` with the bit at position `position` set (to 1).
    /// Considers the 5 rightmost bits of `position` (`position` mod 32).
    /// The bits of x have positions: 0 (rightmost bit) - 7 (leftmost bit)
    ///
    @Time(1) @Space(1)
    pub def setBit(x: Int8, position: Int32): Int8 = x ||| (1i8 <<< position)

    ///
    /// Returns `x` with the bit at position `position` cleared (to 0).
    /// Considers the 5 rightmost bits of `position` (`position` mod 32).
    /// The bits of x have positions: 0 (rightmost bit) - 7 (leftmost bit)
    ///
    @Time(1) @Space(1)
    pub def clearBit(x: Int8, position: Int32): Int8 = x &&& ~~~(1i8 <<< position)

    ///
    /// Returns `x` with the bit at position `position` flipped.
    /// Considers the 5 rightmost bits of `position` (`position` mod 32).
    /// The bits of x have positions: 0 (rightmost bit) - 7 (leftmost bit)
    ///
    @Time(1) @Space(1)
    pub def flipBit(x: Int8, position: Int32): Int8 = x ^^^ (1i8 <<< position)

    ///
    /// Return a string representation of `x`.
    ///
    @Time(1) @Space(1)
    pub def toString(x: Int8): String = ToString.toString(x)

    ///
    /// Parse the string `s` as an Int8, leading or trailing whitespace is trimmed.
    /// A successful parse is wrapped with `OK(x)`, a parse failure is indicated by `Err(_)`.
    ///
    @Time(1) @Space(1)
    pub def fromString(s: String): Result[Int8, String] = try {
        import java.lang.String.strip();
        import java.lang.Byte:parseByte(String);
        Ok(s.strip().parseByte()) as & Pure
    } catch {
        case _: ##java.lang.NumberFormatException => Err("Int8.fromString")
    }

    ///
    /// Convert `x` to an Int16.
    ///
    /// The numeric value of `x` is preserved exactly.
    ///
    @Time(1) @Space(1)
    pub def toInt16(x: Int8): Int16 =
        import java.lang.Byte:valueOf(Int8);
        import java.lang.Byte.shortValue();
        valueOf(x).shortValue() as & Pure

    ///
    /// Convert `x` to an Int32.
    ///
    /// The numeric value of `x` is preserved exactly.
    ///
    @Time(1) @Space(1)
    pub def toInt32(x: Int8): Int32 =
        import java.lang.Byte:valueOf(Int8);
        import java.lang.Byte.intValue();
        valueOf(x).intValue() as & Pure

    ///
    /// Convert `x` to an Int64.
    ///
    /// The numeric value of `x` is preserved exactly.
    ///
    @Time(1) @Space(1)
    pub def toInt64(x: Int8): Int64 =
        import java.lang.Byte:valueOf(Int8);
        import java.lang.Byte.longValue();
        valueOf(x).longValue() as & Pure

    ///
    /// Convert `x` to a BigInt.
    ///
    /// The numeric value of `x` is preserved exactly.
    ///
    @Time(1) @Space(1)
    pub def toBigInt(x: Int8): BigInt =
        import java.lang.Byte:valueOf(Int8) as i8ValueOf;
        import java.lang.Byte.longValue();
        import java.math.BigInteger:valueOf(Int64) as asBigInt;
        i8ValueOf(x).longValue().asBigInt() as & Pure

    ///
    /// Convert `x` to a Float32.
    ///
    /// The numeric value of `x` is preserved exactly.
    ///
    @Time(1) @Space(1)
    pub def toFloat32(x: Int8): Float32 =
        import java.lang.Byte:valueOf(Int8);
        import java.lang.Byte.floatValue();
        valueOf(x).floatValue() as & Pure

    ///
    /// Convert `x` to a Float64.
    ///
    /// The numeric value of `x` is preserved exactly.
    ///
    @Time(1) @Space(1)
    pub def toFloat64(x: Int8): Float64 =
        import java.lang.Byte:valueOf(Int8);
        import java.lang.Byte.doubleValue();
        valueOf(x).doubleValue() as & Pure

    /////////////////////////////////////////////////////////////////////////////
    // Lints: Simplifications                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @lint
    def leftAdditionByZero(): Bool =
        forall (x: Int8).
            (0i8 + x) === x

    @lint
    def rightAdditionByZero(): Bool =
        forall (x: Int8).
            (x + 0i8) === x

    @lint
    def subtractionByZero(): Bool =
        forall (x: Int8).
            (x - 0i8) === 0i8

    @lint
    def subtractionBySelf(): Bool =
        forall (x: Int8).
            (x - x) === 0i8

    @lint
    def leftMultiplicationByZero(): Bool =
        forall (x: Int8).
            (0i8 * x) === 0i8

    @lint
    def rightMultiplicationByZero(): Bool =
        forall (x: Int8).
            (x * 0i8) === 0i8

    @lint
    def leftMultiplicationByOne(): Bool =
        forall (x: Int8).
            (1i8 * x) === x

    @lint
    def rightMultiplicationByOne(): Bool =
        forall (x: Int8).
            (x * 1i8) === x

    @lint
    def divisionByZero(): Bool =
        forall (x: Int8).
            (x / 0i8) === 0i8

    @lint
    def divisionByOne(): Bool =
        forall (x: Int8).
            (x / 1i8) === x

    @lint
    def divisionBySelf(): Bool =
        forall (x: Int8).
            (x / x) === 1i8

}
