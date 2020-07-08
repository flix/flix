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

    ///
    /// Returns the number of bits used to represent an `Int32`.
    ///
    pub def size(): Int32 = 32

    ///
    /// Returns the minimum number representable by an `Int32`.
    ///
    pub def minValue(): Int32 = 1 <<< (size() - 1)

    ///
    /// Returns the maximum number representable by an `Int32`.
    ///
    pub def maxValue(): Int32 = ~~~minValue()

    ///
    /// Returns the smaller of `x` and `y`.
    ///
    pub def min(x: Int32, y: Int32): Int32 = if (x <= y) x else y

    ///
    /// Returns the larger of `x` and `y`.
    ///
    pub def max(x: Int32, y: Int32): Int32 = if (x >= y) x else y

    ///
    /// Returns the absolute value of `x`.
    /// If the absolute value exceeds maxValue(), -1 is returned.
    ///
    pub def abs(x: Int32): Int32 = {
        if      (x >= 0)            x
        else if (x == minValue())   -1
        else                        -x
    }

    ///
    /// Returns the distance between `x` and `y`.
    /// If this distance exceeds maxValue(), -1 is returned.
    ///
    pub def dist(x: Int32, y: Int32): Int32 = {
        if      (x >= 0 && y >= 0)                      abs(x - y)
        else if (x < 0 && y < 0)                        abs(x - y)
        else if (x == minValue() || y == minValue())    -1
        else if (minValue() + abs(x) + abs(y) >= 0)     -1
        else                                            abs(x - y)
    }

    ///
    /// Returns 1 if x > y, -1 if x < y, and 0 if x = y.
    /// The sign of x - y.
    ///
    pub def compare(x: Int32, y: Int32): Int32 = {
        if      (x == y)    0
        else if (x < y)     -1
        else                1
    }

    ///
    /// Returns 1 if x > 0, -1 if x < 0, and 0 if x = 0.
    /// The sign of x.
    ///
    pub def signum(x: Int32): Int32 = compare(x, 0)

    ///
    /// Returns the logical right shift of `x` by `distance`.
    /// Only the rightmost 5 bits of `distance` are considered (ie. `distance % 32`).
    /// A zero is shifted into the leftmost position regardless of sign extension.
    ///
    pub def logicalRightShift(x: Int32, distance: Int32): Int32 =
        if (x < 0 && distance % size() != 0)
            ((x >>> 1) &&& maxValue()) >>> (distance - 1)
        else
            x >>> distance

    ///
    /// Returns the number of one-bits in the two's complement binary
    /// representation of `x`.
    ///
    pub def bitCount(x: Int32): Int32 = {
        if      (x == 0)        0
        else if (x % 2 != 0)    bitCount(logicalRightShift(x, 1)) + 1
        else                    bitCount(logicalRightShift(x, 1))
    }

    ///
    /// Returns the the value obtained by rotating the two's complement
    /// binary representation of `x` right by `distance` bits.
    ///
    pub def rotateRight(x: Int32, distance: Int32): Int32 =
        (logicalRightShift(x, distance)) ||| (x <<< -distance)

    ///
    /// Returns the the value obtained by rotating the two's complement
    /// binary representation of `x` left by `distance` bits.
    ///
    pub def rotateLeft(x: Int32, distance: Int32): Int32 =
        (logicalRightShift(x, -distance)) ||| (x <<< distance)

    ///
    /// Returns the value obtained by reversing the bits in the
    /// two's complement binary representation of `x`.
    ///
    pub def reverse(x: Int32): Int32 = reverseHelper(x, 0, size()-1)

    ///
    /// Helper function for `reverse`.
    ///
    def reverseHelper(x: Int32, l: Int32, r: Int32): Int32 =
        if (l >= r) x else reverseHelper(swap(x, l, r), l+1, r-1)

    ///
    /// Helper function for `reverse`.
    ///
    def swap(x: Int32, l: Int32, r: Int32): Int32 = match (getBit(x, l), getBit(x, r)) {
        case (1, 0) => clearBit(setBit(x, r), l)
        case (0, 1) => clearBit(setBit(x, l), r)
        case _ => x
    }

    ///
    /// Returns the position of the highest-order/leftmost one-bit in `x`.
    /// Possible return values: 0 (rightmost bit) - 31 (leftmost bit)
    ///                         -1 if x = 0
    ///
    pub def highestOneBitPosition(x: Int32): Int32 =
        // Start at bit 31 and scan right
        oneBitPositionHelper(x, size() - 1, -1)

    ///
    /// Returns the position of the lowest-order/rightmost one-bit in `x`.
    /// Possible return values: 0 (rightmost bit) - 31 (leftmost bit)
    ///                         -1 if x = 0
    ///
    pub def lowestOneBitPosition(x: Int32): Int32 =
        // Start at bit 0 and scan left
        oneBitPositionHelper(x, 0, 1)

    ///
    /// Helper function for highestOneBitPosition and lowestOneBitPosition.
    /// If `x` has a one-bit at `position`, return that position.
    /// Otherwise recursively check the next bit in the same way.
    ///
    def oneBitPositionHelper(x: Int32, position: Int32, delta: Int32): Int32 = {
        if      (position < 0)              -1
        else if (position > size() - 1)     -1
        else if (getBit(x, position) == 1)  position
        else if (delta == 0)                -1
        else                                oneBitPositionHelper(x, position + delta, delta)
    }

    ///
    /// Returns a value with at most a single one-bit, in the position
    /// of the highest-order/leftmost one-bit in `x`.
    /// Returns 0 if x=0.
    ///
    pub def highestOneBit(x: Int32): Int32 =
        bitPositionToInt(highestOneBitPosition(x))

    ///
    /// Returns a value with at most a single one-bit, in the position
    /// of the highest-order/leftmost one-bit in `x`.
    /// Returns 0 if x=0.
    ///
    pub def lowestOneBit(x: Int32): Int32 =
        bitPositionToInt(lowestOneBitPosition(x))

    ///
    /// Helper function for highestOneBit and lowestOneBit.
    /// Returns a value with a single one-bit at bit number `position`.
    /// Returns 0 if `position` is outside the range 0-31 inclusive.
    ///
    def bitPositionToInt(position: Int32): Int32 =
        if (position < 0 || position > size() - 1) 0 else 1 <<< position

    ///
    /// Returns the number of zero bits preceding the
    /// highest-order/leftmost one-bit in `x`.
    /// Returns 32 if x=0.
    ///
    pub def numberOfLeadingZeros(x: Int32): Int32 =
        size() - 1 - highestOneBitPosition(x)

    ///
    /// Returns the number of zero bits following the
    /// lowest-order/rightmost one-bit in `x`.
    /// Returns 32 if x=0.
    ///
    pub def numberOfTrailingZeros(x: Int32): Int32 =
        if (x == 0) 32 else lowestOneBitPosition(x)

    ///
    /// Returns the bit of `x` at `position` (either 0 or 1).
    /// Considers the 5 rightmost bits of `position` (`position` mod 32).
    /// The bits of x have positions: 0 (rightmost bit) - 31 (leftmost bit)
    ///
    pub def getBit(x: Int32, position: Int32): Int32 =
        if ((x >>> position) % 2 == 0) 0 else 1

    ///
    /// Returns `x` with the bit at position `position` set (to 1).
    /// Considers the 5 rightmost bits of `position` (`position` mod 32).
    /// The bits of x have positions: 0 (rightmost bit) - 31 (leftmost bit)
    ///
    pub def setBit(x: Int32, position: Int32): Int32 = x ||| (1 <<< position)

    ///
    /// Returns `x` with the bit at position `position` cleared (to 0).
    /// Considers the 5 rightmost bits of `position` (`position` mod 32).
    /// The bits of x have positions: 0 (rightmost bit) - 31 (leftmost bit)
    ///
    pub def clearBit(x: Int32, position: Int32): Int32 = x &&& ~~~(1 <<< position)

    ///
    /// Returns `x` with the bit at position `position` flipped.
    /// Considers the 5 rightmost bits of `position` (`position` mod 32).
    /// The bits of x have positions: 0 (rightmost bit) - 31 (leftmost bit)
    ///
    pub def flipBit(x: Int32, position: Int32): Int32 = x ^^^ (1 <<< position)

    ///
    /// Returns the integer binary logarithm of `x`.
    ///
    pub def log2(x: Int32): Int32 = highestOneBitPosition(x)

    ///
    /// Returns the factorial of `x`.
    ///
    pub def factorial(x: Int32): Int32 = factorialHelper(x, 1)

    ///
    /// Helper function for `factorial`.
    ///
    def factorialHelper(x: Int32, acc: Int32): Int32 =
        if (x == 0) acc else factorialHelper(x - 1, x * acc)

    ///
    /// Return a string representation of `x`.
    ///
    pub def toString(x: Int32): String =
        import java.lang.Integer:toString(Int32);
        x.toString() as & Pure

    ///
    /// Parse the string `s` as an Int32, leading or trailing whitespace is trimmed.
    /// A successful parse is wrapped with `OK(x)`, a parse failure is indicated by `Err(_)`.
    ///
    pub def fromString(s: String): Result[Int32, String] = try {
        import java.lang.String.strip();
        import java.lang.Integer:parseInt(String);
        Ok(s.strip().parseInt()) as & Pure
    } catch {
        case _: ##java.lang.NumberFormatException => Err("Int32.fromString")
    }

    ///
    /// Convert `x` to an `Option[Int8]`.
    ///
    /// Returns `Some(x as Int8)` if the numeric value of `x` can be represented exactly.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Int8
    /// (i.e. -128 to 127).
    ///
    pub def tryToInt8(x: Int32): Option[Int8] =
        import java.lang.Integer:valueOf(Int32);
        import java.lang.Integer.byteValue();
        if (x < Int8.toInt32(Int8.minValue()) || x > Int8.toInt32(Int8.maxValue()))
            None
        else
            Some(valueOf(x).byteValue()) as & Pure

    ///
    /// Convert `x` to an `Option[Int16]`.
    ///
    /// Returns `Some(x as Int16)` if the numeric value of `x` can be represented exactly.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Int16
    /// (i.e. -32768 to 32767).
    ///
    pub def tryToInt16(x: Int32): Option[Int16] =
        import java.lang.Integer:valueOf(Int32);
        import java.lang.Integer.shortValue();
        if (x < Int16.toInt32(Int16.minValue()) || x > Int16.toInt32(Int16.maxValue()))
            None
        else
            Some(valueOf(x).shortValue()) as & Pure

    ///
    /// Convert `x` to a Int64.
    ///
    /// The numeric value of `x` is preserved exactly.
    ///
    pub def toInt64(x: Int32): Int64 =
        import java.lang.Integer:valueOf(Int32);
        import java.lang.Integer.longValue();
        valueOf(x).longValue() as & Pure

    ///
    /// Convert `x` to a BigInt.
    ///
    /// The numeric value of `x` is preserved exactly.
    ///
    pub def toBigInt(x: Int32): BigInt =
        import java.lang.Integer:valueOf(Int32) as i16ValueOf;
        import java.lang.Integer.longValue();
        import java.math.BigInteger:valueOf(Int64) as asBigInt;
        i16ValueOf(x).longValue().asBigInt() as & Pure

    ///
    /// Convert `x` to an Float32.
    ///
    /// The numeric value of `x` may lose precision.
    ///
    pub def toFloat32(x: Int32): Float32 =
        import java.lang.Integer:valueOf(Int32);
        import java.lang.Integer.floatValue();
        valueOf(x).floatValue() as & Pure

    ///
    /// Convert `x` to a Float64.
    ///
    /// The numeric value of `x` is preserved exactly.
    ///
    pub def toFloat64(x: Int32): Float64 =
        import java.lang.Integer:valueOf(Int32);
        import java.lang.Integer.doubleValue();
        valueOf(x).doubleValue() as & Pure

    ///
    /// Helper function for the `clamp` conversion functions.
    ///
    def clamp(x: Int32, minimum: Int32, maximum: Int32): Int32 =
        if (x < minimum)
             minimum
        else
            if (x > maximum)
                maximum
            else
                x

    ///
    /// Convert `x` to an `Int8`.
    ///
    /// Returns `x` clamped within the Int8 range `minimum` to `maximum`.
    ///
    pub def clampToInt8(x: Int32, minimum: Int8, maximum: Int8): Int8 =
        import java.lang.Integer:valueOf(Int32);
        import java.lang.Integer.byteValue();
        let mini32 = Int8.toInt32(minimum);
        let maxi32 = Int8.toInt32(maximum);
        valueOf(clamp(x, mini32, maxi32)).byteValue() as & Pure


    ///
    /// Convert `x` to an `Int16`.
    ///
    /// Returns `x` clamped within the Int16 range `minimum` to `maximum`.
    ///
    pub def clampToInt16(x: Int32, minimum: Int16, maximum: Int16): Int16 =
        import java.lang.Integer:valueOf(Int32);
        import java.lang.Integer.shortValue();
        let mini32 = Int16.toInt32(minimum);
        let maxi32 = Int16.toInt32(maximum);
        valueOf(clamp(x, mini32, maxi32)).shortValue() as & Pure

    /////////////////////////////////////////////////////////////////////////////
    // Lints: Simplifications                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @lint
    def leftAdditionByZero(): Bool =
        forall (x: Int32).
            (0i32 + x) === x

    @lint
    def rightAdditionByZero(): Bool =
        forall (x: Int32).
            (x + 0i32) === x

    @lint
    def subtractionByZero(): Bool =
        forall (x: Int32).
            (x - 0i32) === 0i32

    @lint
    def subtractionBySelf(): Bool =
        forall (x: Int32).
            (x - x) === 0i32

    @lint
    def leftMultiplicationByZero(): Bool =
        forall (x: Int32).
            (0i32 * x) === 0i32

    @lint
    def rightMultiplicationByZero(): Bool =
        forall (x: Int32).
            (x * 0i32) === 0i32

    @lint
    def leftMultiplicationByOne(): Bool =
        forall (x: Int32).
            (1i32 * x) === x

    @lint
    def rightMultiplicationByOne(): Bool =
        forall (x: Int32).
            (x * 1i32) === x

    @lint
    def divisionByZero(): Bool =
        forall (x: Int32).
            (x / 0i32) === 0i32

    @lint
    def divisionByOne(): Bool =
        forall (x: Int32).
            (x / 1i32) === x

    @lint
    def divisionBySelf(): Bool =
        forall (x: Int32).
            (x / x) === 1i32

}
