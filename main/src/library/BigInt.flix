/*
 * Copyright 2019 Liam Palmer, Magnus Madsen
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

mod BigInt {

    import java.math.BigDecimal
    import java.math.BigInteger
    import java.lang.NumberFormatException
    import java.lang.ArithmeticException

    ///
    /// Returns the smaller of `x` and `y`.
    ///
    pub def min(x: BigInt, y: BigInt): BigInt = if (x <= y) x else y

    ///
    /// Returns the larger of `x` and `y`.
    ///
    pub def max(x: BigInt, y: BigInt): BigInt = if (x >= y) x else y

    ///
    /// Returns the absolute value of `x`.
    ///
    pub def abs(x: BigInt): BigInt = if (x >= 0ii) x else -x

    ///
    /// Returns `base` raised to the power of `n`.
    ///
    pub def pow(base: {base = BigInt}, n: Int32): BigInt =
        base#base.pow(n)

    ///
    /// Returns the Euclidean modulo of `x` and `n`.
    /// The result is always non-negative.
    ///
    pub def modulo(x: BigInt, n: BigInt): BigInt =
        if (n == 0ii)
            0ii
        else
            ((x.remainder(n)) + n).remainder(n)

    ///
    /// Returns the remainder of `x / n`.
    /// The result can be negative.
    ///
    /// See also `BigInt.modulo`.
    ///
    pub def remainder(x: BigInt, n: BigInt): BigInt =
        if (n == 0ii)
            0ii
        else
            x.remainder(n)

    ///
    /// Returns the distance between `x` and `y`.
    ///
    pub def dist(x: BigInt, y: BigInt): BigInt = abs(x - y)

    ///
    /// Returns `1` if `x > y`, `-1` if `x < y`, and `0` if `x = y`.
    ///
    pub def compare(x: BigInt, y: BigInt): Int32 = {
        if (x == y)     0
        else if (x < y) -1
        else            1
    }

    ///
    /// Returns `1` if `x > 0`, `-1` if `x < 0`, and `0` if `x = 0`.
    ///
    pub def signum(x: BigInt): Int32 = compare(x, 0ii)

    ///
    /// Returns the bit of `x` at `position` (either 0 or 1).
    ///
    /// The bits of x have positions: 0 (rightmost bit), 1, 2, ...
    ///
    pub def getBit(x: BigInt, position: Int32): Int32 =
        if (rightShift(x, position) `remainder` 2ii == 0ii) 0 else 1

    ///
    /// Returns the greatest common non-negative divisor of `x` and `y`.
    ///
    pub def gcd(x: BigInt, y: BigInt): BigInt = if (y == 0ii) abs(x) else gcd(y, x `remainder` y)

    ///
    /// Returns `x` with the bit at position `position` set (to 1).
    ///
    pub def setBit(x: BigInt, position: Int32): BigInt = bitwiseOr(x, leftShift(1ii, position))

    ///
    /// Returns `x` with the bit at position `position` cleared (to 0).
    ///
    pub def clearBit(x: BigInt, position: Int32): BigInt = bitwiseAnd(x, bitwiseNot(leftShift(1ii, position)))

    ///
    /// Returns `x` with the bit at position `position` flipped.
    ///
    pub def flipBit(x: BigInt, position: Int32): BigInt = bitwiseXor(x, leftShift(1ii, position))

    ///
    /// Returns the integer binary logarithm of `x`.
    ///
    pub def log2(x: BigInt): Int32 =
        x.bitLength()

    ///
    /// Returns `x` with the bits shifted left by `y` places
    ///
    pub def leftShift(x: BigInt, y: Int32): BigInt =
        x.shiftLeft(y)

    ///
    /// Returns `x` with the bits shifted right by `y` places
    ///
    pub def rightShift(x: BigInt, y: Int32): BigInt =
        x.shiftRight(y)

    ///
    /// Returns the bitwise AND of `x` and `y`.
    ///
    pub def bitwiseAnd(x: BigInt, y: BigInt): BigInt =
        x.$and(y)

    ///
    /// Returns the bitwise NOT of `x`.
    ///
    pub def bitwiseNot(x: BigInt): BigInt =
        x.$not()

    ///
    /// Returns the bitwise OR of `x` and `y`.
    ///
    pub def bitwiseOr(x: BigInt, y: BigInt): BigInt =
        x.$or(y)

    ///
    /// Returns the bitwise XOR of `x` and `y`.
    ///
    pub def bitwiseXor(x: BigInt, y: BigInt): BigInt =
        x.$xor(y)

    ///
    /// Return a string representation of `x`.
    ///
    pub def toString(x: BigInt): String = ToString.toString(x)

    ///
    /// Parse the string `s` as an BigInt, leading or trailing whitespace is trimmed.
    /// A successful parse is wrapped with `Some(x)`, a parse failure is indicated by `None`.
    ///
    pub def fromString(s: String): Option[BigInt] = try {
        Some(new BigInteger(s.strip()))
    } catch {
        case _: NumberFormatException => None
    }

    ///
    /// Convert `x` to an `Option[Int8]`.
    ///
    /// Returns `Some(x as Int8)` if the numeric value of `x` can be represented exactly.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Int8
    /// (i.e. -128 to 127).
    ///
    pub def tryToInt8(x: BigInt): Option[Int8] =
        try {
            Some(x.byteValueExact())
        } catch {
            case _ : ArithmeticException => None
        }

    ///
    /// Convert `x` to an `Option[Int16]`.
    ///
    /// Returns `Some(x as Int16)` if the numeric value of `x` can be represented exactly.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Int16
    /// (i.e. -32768 to 32767).
    ///
    pub def tryToInt16(x: BigInt): Option[Int16] =
        try {
            Some(x.shortValueExact())
        } catch {
            case _ : ArithmeticException => None
        }

    ///
    /// Convert `x` to an `Option[Int32]`.
    ///
    /// Returns `Some(x as Int32)` if the numeric value of `x` can be represented exactly.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Int32
    /// (i.e. -2147483648 to 2147483647).
    ///
    pub def tryToInt32(x: BigInt): Option[Int32] =
        try {
            Some(x.intValueExact())
        } catch {
            case _ : ArithmeticException => None
        }

    ///
    /// Convert `x` to an `Option[Int64]`.
    ///
    /// Returns `Some(x as Int64)` if the numeric value of `x` can be represented exactly.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Int64
    /// (i.e. -9223372036854775808 to 9223372036854775807).
    ///
    pub def tryToInt64(x: BigInt): Option[Int64] =
        try {
            Some(x.longValueExact())
        } catch {
            case _ : ArithmeticException => None
        }

    ///
    /// Convert `x` to an `Option[Float32]`.
    ///
    /// Returns `Some(x as Float32)` if the numeric value of `x` is within the range of Float32.
    ///
    /// Warning: even if `x` is in the range of Float32 it may lose precision.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Float32
    /// (i.e. 1.4E-45 to 3.4028235E38).
    ///
    pub def tryToFloat32(x: BigInt): Option[Float32] =
        let xd = new BigDecimal(x);
        let d = xd.floatValue();
        forM (
            f32min <- Float32.tryToBigInt(Float32.minValue());
            f32max <- Float32.tryToBigInt(Float32.maxValue());
            if f32min <= x and x <= f32max
        ) yield d

    ///
    /// Convert `x` to an `Option[Float64]`.
    ///
    /// Returns `Some(x as Float64)` if the numeric value of `x` is within the range of Float32.
    ///
    /// Warning: even if `x` is in the range of Float64 it may lose precision.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Float64
    /// (i.e 4.9E-324 to 1.7976931348623157E308).
    ///
    pub def tryToFloat64(x: BigInt): Option[Float64] =
        let xd = new BigDecimal(x);
        let d = xd.doubleValue();
        forM (
            f64min <- Float64.tryToBigInt(Float64.minValue());
            f64max <- Float64.tryToBigInt(Float64.maxValue());
            if f64min <= x and x <= f64max
        ) yield d

    ///
    /// Convert `x` to an `BigDecimal`.
    ///
    pub def toBigDecimal(x: BigInt): BigDecimal =
        new BigDecimal(x)

    ///
    /// Helper function for the `clamp` conversion functions.
    ///
    def clamp(min: {min = BigInt}, max: {max = BigInt}, x: BigInt): BigInt =
        if (x < min#min)
             min#min
        else
            if (x > max#max)
                max#max
            else
                x

    ///
    /// Convert `x` to an `Int8`.
    ///
    /// Returns `x` clamped within the Int8 range `min` to `max`.
    ///
    pub def clampToInt8(min: {min = Int8}, max: {max = Int8}, x: BigInt): Int8 =
        let minii = Int8.toBigInt(min#min);
        let maxii = Int8.toBigInt(max#max);
        try {
            let y = clamp(min = minii, max = maxii, x);
            y.byteValueExact()
        } catch {
            // This should be impossible!
            case _ : ArithmeticException => min#min
        }

    ///
    /// Convert `x` to an `Int16`.
    ///
    /// Returns `x` clamped within the Int16 range `min` to `max`.
    ///
    pub def clampToInt16(min: {min = Int16}, max: {max = Int16}, x: BigInt): Int16 =
        let minii = Int16.toBigInt(min#min);
        let maxii = Int16.toBigInt(max#max);
        try {
            let y = clamp(min = minii, max = maxii, x);
            y.shortValueExact()
        } catch {
            // This should be impossible!
            case _ : ArithmeticException => min#min
        }

    ///
    /// Convert `x` to an `Int32`.
    ///
    /// Returns `x` clamped within the Int32 range `min` to `max`.
    ///
    pub def clampToInt32(min: {min = Int32}, max: {max = Int32}, x: BigInt): Int32 =
        let minii = Int32.toBigInt(min#min);
        let maxii = Int32.toBigInt(max#max);
        try {
            let y = clamp(min = minii, max = maxii, x);
            y.intValueExact()
        } catch {
            // This should be impossible!
            case _ : ArithmeticException => min#min
        }

    ///
    /// Convert `x` to an `Int64`.
    ///
    /// Returns `x` clamped within the Int64 range `min` to `max`.
    ///
    pub def clampToInt64(min: {min = Int64}, max: {max = Int64}, x: BigInt): Int64 =
        let minii = Int64.toBigInt(min#min);
        let maxii = Int64.toBigInt(max#max);
        try {
            let y = clamp(min = minii, max = maxii, x);
            y.longValueExact()
        } catch {
            // This should be impossible!
            case _ : ArithmeticException => min#min
        }

    ///
    /// Convert `x` to a `Float32`.
    ///
    /// Returns `x` clamped within the Float32 range `min` to `max`.
    ///
    pub def clampToFloat32(min: {min = Float32}, max: {max = Float32}, x: BigInt): Float32 =
        let step = () -> {
            forM (
                f32min <- Float32.tryToBigInt(min#min);
                f32max <- Float32.tryToBigInt(max#max)
            ) yield {
                let x1 = clamp(min = f32min, max = f32max, x);
                let xd = new BigDecimal(x1);
                xd.floatValue()
            }
        };
        match step() {
            case None => Float32.nan()
            case Some(d) => d
        }

    ///
    /// Convert `x` to a `Float64`.
    ///
    /// Returns `x` clamped within the Float64 range `min` to `max`.
    ///
    pub def clampToFloat64(min: {min = Float64}, max: {max = Float64}, x: BigInt): Float64 =
        let step = () -> {
            forM (
                f64min <- Float64.tryToBigInt(min#min);
                f64max <- Float64.tryToBigInt(max#max)
            ) yield {
                let x1 = clamp(min = f64min, max = f64max, x);
                let xd = new BigDecimal(x1);
                xd.doubleValue()
            }
        };
        match step() {
            case None => Float64.nan()
            case Some(d) => d
        }

}
