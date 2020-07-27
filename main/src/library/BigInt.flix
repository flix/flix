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

namespace BigInt {

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
        if ((x >>> position) % 2ii == 0ii) 0 else 1

    ///
    /// Returns the greatest common non-negative divisor of `x` and `y`.
    ///
    pub def gcd(x: BigInt, y: BigInt): BigInt = if (y == 0ii) abs(x) else gcd(y, x % y)

    ///
    /// Returns `x` with the bit at position `position` set (to 1).
    ///
    pub def setBit(x: BigInt, position: Int32): BigInt = x ||| (1ii <<< position)

    ///
    /// Returns `x` with the bit at position `position` cleared (to 0).
    ///
    pub def clearBit(x: BigInt, position: Int32): BigInt = x &&& ~~~(1ii <<< position)

    ///
    /// Returns `x` with the bit at position `position` flipped.
    ///
    pub def flipBit(x: BigInt, position: Int32): BigInt = x ^^^ (1ii <<< position)

    ///
    /// Returns the integer binary logarithm of `x`.
    ///
    pub def log2(x: BigInt): Int32 =
        import java.math.BigInteger.bitLength();
        bitLength(x) as & Pure

    ///
    /// Return a string representation of `x`.
    ///
    pub def toString(x: BigInt): String =
        import java.math.BigInteger.toString();
        x.toString() as & Pure

    ///
    /// Parse the string `s` as an BigInt, leading or trailing whitespace is trimmed.
    /// A successful parse is wrapped with `OK(x)`, a parse failure is indicated by `Err(_)`.
    ///
    pub def fromString(s: String): Result[BigInt, String] = try {
        import java.lang.String.strip();
        import new java.math.BigInteger(String) as parseBigInt;
        Ok(s.strip().parseBigInt()) as & Pure
    } catch {
        case _: ##java.lang.NumberFormatException => Err("BigInt.fromString")
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
        import java.math.BigInteger.byteValueExact();
        try {
            Some(byteValueExact(x)) as & Pure
        } catch {
            case _ : ##java.lang.ArithmeticException => None
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
        import java.math.BigInteger.shortValueExact();
        try {
            Some(shortValueExact(x)) as & Pure
        } catch {
            case _ : ##java.lang.ArithmeticException => None
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
        import java.math.BigInteger.intValueExact();
        try {
            Some(intValueExact(x)) as & Pure
        } catch {
            case _ : ##java.lang.ArithmeticException => None
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
        import java.math.BigInteger.longValueExact();
        try {
            Some(longValueExact(x)) as & Pure
        } catch {
            case _ : ##java.lang.ArithmeticException => None
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
        use Option.flatMap;
        import new java.math.BigDecimal(BigInt) as newBigDecimal;
        import java.math.BigDecimal.floatValue();
        let xd = newBigDecimal(x) as & Pure;
        let d = xd.floatValue() as & Pure;
        let* f32min = Float32.tryToBigInt(Float32.minValue());
        let* f32max = Float32.tryToBigInt(Float32.maxValue());
        if (x < f32min || x > f32max)
            None
        else
            Some(d)

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
        use Option.flatMap;
        import new java.math.BigDecimal(BigInt) as newBigDecimal;
        import java.math.BigDecimal.doubleValue();
        let xd = newBigDecimal(x) as & Pure;
        let d = xd.doubleValue() as & Pure;
        let* f64min = Float64.tryToBigInt(Float64.minValue());
        let* f64max = Float64.tryToBigInt(Float64.maxValue());
        if (x < f64min || x > f64max)
            None
        else
            Some(d)

    ///
    /// Helper function for the `clamp` conversion functions.
    ///
    def clamp(x: BigInt, minimum: BigInt, maximum: BigInt): BigInt =
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
    pub def clampToInt8(x: BigInt, minimum: Int8, maximum: Int8): Int8 =
        import java.math.BigInteger.byteValueExact();
        let minii = Int8.toBigInt(minimum);
        let maxii = Int8.toBigInt(maximum);
        try {
            byteValueExact(clamp(x, minii, maxii)) as & Pure
        } catch {
            // This should be impossible!
            case _ : ##java.lang.ArithmeticException => minimum
        }

    ///
    /// Convert `x` to an `Int16`.
    ///
    /// Returns `x` clamped within the Int16 range `minimum` to `maximum`.
    ///
    pub def clampToInt16(x: BigInt, minimum: Int16, maximum: Int16): Int16 =
        import java.math.BigInteger.shortValueExact();
        let minii = Int16.toBigInt(minimum);
        let maxii = Int16.toBigInt(maximum);
        try {
            shortValueExact(clamp(x, minii, maxii)) as & Pure
        } catch {
            // This should be impossible!
            case _ : ##java.lang.ArithmeticException => minimum
        }

    ///
    /// Convert `x` to an `Int32`.
    ///
    /// Returns `x` clamped within the Int32 range `minimum` to `maximum`.
    ///
    pub def clampToInt32(x: BigInt, minimum: Int32, maximum: Int32): Int32 =
        import java.math.BigInteger.intValueExact();
        let minii = Int32.toBigInt(minimum);
        let maxii = Int32.toBigInt(maximum);
        try {
            intValueExact(clamp(x, minii, maxii)) as & Pure
        } catch {
            // This should be impossible!
            case _ : ##java.lang.ArithmeticException => minimum
        }

    ///
    /// Convert `x` to an `Int64`.
    ///
    /// Returns `x` clamped within the Int64 range `minimum` to `maximum`.
    ///
    pub def clampToInt64(x: BigInt, minimum: Int64, maximum: Int64): Int64 =
        import java.math.BigInteger.longValueExact();
        let minii = Int64.toBigInt(minimum);
        let maxii = Int64.toBigInt(maximum);
        try {
            longValueExact(clamp(x, minii, maxii)) as & Pure
        } catch {
            // This should be impossible!
            case _ : ##java.lang.ArithmeticException => minimum
        }

    ///
    /// Convert `x` to a `Float32`.
    ///
    /// Returns `x` clamped within the Float32 range `minimum` to `maximum`.
    ///
    pub def clampToFloat32(x: BigInt, minimum: Float32, maximum: Float32): Float32 =
        use Option.flatMap;
        import new java.math.BigDecimal(BigInt) as newBigDecimal;
        import java.math.BigDecimal.floatValue();
        let step = () -> {
            let* f32min = Float32.tryToBigInt(minimum);
            let* f32max = Float32.tryToBigInt(maximum);
            let x1 = clamp(x, f32min, f32max);
            let xd = newBigDecimal(x1) as & Pure;
            Some(xd.floatValue()) as & Pure
        };
        match step() {
            case None => Float32.nan()
            case Some(d) => d
        }

    ///
    /// Convert `x` to a `Float64`.
    ///
    /// Returns `x` clamped within the Float64 range `minimum` to `maximum`.
    ///
    pub def clampToFloat64(x: BigInt, minimum: Float64, maximum: Float64): Float64 =
        use Option.flatMap;
        import new java.math.BigDecimal(BigInt) as newBigDecimal;
        import java.math.BigDecimal.doubleValue();
        let step = () -> {
            let* f64min = Float64.tryToBigInt(minimum);
            let* f64max = Float64.tryToBigInt(maximum);
            let x1 = clamp(x, f64min, f64max);
            let xd = newBigDecimal(x1) as & Pure;
            Some(xd.doubleValue()) as & Pure
        };
        match step() {
            case None => Float64.nan()
            case Some(d) => d
        }

    /////////////////////////////////////////////////////////////////////////////
    // Lints: Simplifications                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @lint
    def leftAdditionByZero(): Bool =
        forall (x: BigInt).
            (0ii + x) === x

    @lint
    def rightAdditionByZero(): Bool =
        forall (x: BigInt).
            (x + 0ii) === x

    @lint
    def subtractionByZero(): Bool =
        forall (x: BigInt).
            (x - 0ii) === 0ii

    @lint
    def subtractionBySelf(): Bool =
        forall (x: BigInt).
            (x - x) === 0ii

    @lint
    def leftMultiplicationByZero(): Bool =
        forall (x: BigInt).
            (0ii * x) === 0ii

    @lint
    def rightMultiplicationByZero(): Bool =
        forall (x: BigInt).
            (x * 0ii) === 0ii

    @lint
    def leftMultiplicationByOne(): Bool =
        forall (x: BigInt).
            (1ii * x) === x

    @lint
    def rightMultiplicationByOne(): Bool =
        forall (x: BigInt).
            (x * 1ii) === x

    @lint
    def divisionByZero(): Bool =
        forall (x: BigInt).
            (x / 0ii) === 0ii

    @lint
    def divisionByOne(): Bool =
        forall (x: BigInt).
            (x / 1ii) === x

    @lint
    def divisionBySelf(): Bool =
        forall (x: BigInt).
            (x / x) === 1ii

}
