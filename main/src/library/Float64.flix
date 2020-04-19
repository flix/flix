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

namespace Float64 {

    ///
    /// Returns the number of bits used to represent a `Float64`.
    ///
    pub def size(): Int32 = 64

    ///
    /// Returns the maximum exponent that a `Float64` may have.
    ///
    pub def maxExponent(): Int32 = 1023

    ///
    /// Returns the minimum exponent that a `Float64` may have.
    ///
    pub def minExponent(): Int32 = -1022

    ///
    /// Returns the maximum number representable by a `Float64`.
    ///
    pub def maxValue(): Float64 = (2.0f64 - 2.0f64 ** -52.0f64) * (2.0f64 ** 1023.0f64)

    ///
    /// Returns the minimum number representable by a `Float64`.
    ///
    pub def minValue(): Float64 = -maxValue()

    ///
    /// Returns the minimum positive number representable by a `Float64`.
    ///
    pub def minPositiveValue(): Float64 = 2.0f64 ** -1074.0f64

    ///
    /// Returns the NaN (not a number) value of type `Float64`.
    ///
    pub def nan(): Float64 = 0.0f64 / 0.0f64

    ///
    /// Returns the positive infinity value of type `Float64`.
    ///
    pub def positiveInfinity(): Float64 = 1.0f64 / 0.0f64

    ///
    /// Returns the negative infinity value of type `Float64`.
    ///
    pub def negativeInfinity(): Float64 = -1.0f64 / 0.0f64

    ///
    /// Returns true if and only if `x` is a non-infinite and non-Nan `Float64` value.
    ///
    pub def isFinite(x: Float64): Bool = x >= minValue() && x <= maxValue()

    ///
    /// Returns true if and only if `x` is an infinite and non-Nan `Float64` value.
    ///
    pub def isInfinite(x: Float64): Bool = x == positiveInfinity() || x == negativeInfinity()

    ///
    /// Returns true if and only if `x` is the NaN value of type `Float64`.
    ///
    pub def isNan(x: Float64): Bool = x != x

    ///
    /// Returns the smaller of `x` and `y`.
    ///
    pub def min(x: Float64, y: Float64): Float64 = if (x <= y) x else y

    ///
    /// Returns the larger of `x` and `y`.
    ///
    pub def max(x: Float64, y: Float64): Float64 = if (x >= y) x else y

    ///
    /// Return a string representation of `x`.
    ///
    pub def toString(x: Float64): String =
        import java.lang.Double:toString(Float64);
        x.toString() as & Pure

    ///
    /// Return a decimal representation of `x`.
    ///
    pub def toDecimalString(decimalPlaces: Int32, x: Float64): String =
        toDecimalStringHelper(decimalPlaces, x) as & Pure

    ///
    /// Helper function for `toDecimalString`.
    ///
    pub def toDecimalStringHelper(decimalPlaces: Int32, x: Float64): String & Impure =
        import new java.text.DecimalFormat() as newDecimalFormat;
        import java.text.DecimalFormat.setMaximumFractionDigits(Int32);
        import java.text.NumberFormat.format(Float64);
        let df = newDecimalFormat();
        df.setMaximumFractionDigits(decimalPlaces);
        let nf = df as ##java.text.NumberFormat;
        nf.format(x)

    ///
    /// Parse the string `s` as a Float64, leading or trailing whitespace is trimmed.
    /// A successful parse is wrapped with `OK(x)`, a parse failure is indicated by `Err(_)`.
    ///
    pub def fromString(s: String): Result[Float64, String] = try {
        import java.lang.String.strip();
        import java.lang.Double:parseDouble(String);
        Ok(s.strip().parseDouble()) as & Pure
    } catch {
        case _: ##java.lang.NumberFormatException => Err("Float64.fromString")
    }

    ///
    /// Convert `x` to an `Option[Int8]`.
    ///
    /// If the numeric value of `x` is within the range of Int8 return `Some(x as Int8)`,
    /// rounding `x` towards 0`.
    ///
    /// If the numeric value of `x` is outside the range of Int8 (i.e. -128 to 127),
    /// or it is NaN or infinity, return `None`.
    ///
    pub def toInt8(x: Float64): Option[Int8] =
        import java.lang.Double:valueOf(Float64);
        import java.lang.Double.byteValue();
        if (x < Int8.toFloat64(Int8.minValue()) || x > Int8.toFloat64(Int8.maxValue()) || isNan(x) || isInfinite(x))
            None
        else
            Some(valueOf(x).byteValue()) as & Pure

    ///
    /// Convert `x` to an `Option[Int16]`.
    ///
    /// If the numeric value of `x` is within the range of Int16 return `Some(x as Int16)`,
    /// rounding `x` towards 0`.
    ///
    /// If the numeric value of `x` is outside the range of Int16 (i.e. -32768 to 32767),
    /// or it is NaN or infinity, return `None`.
    ///
    pub def toInt16(x: Float64): Option[Int16] =
        import java.lang.Double:valueOf(Float64);
        import java.lang.Double.shortValue();
        if (x < Int16.toFloat64(Int16.minValue()) || x > Int16.toFloat64(Int16.maxValue()) || isNan(x) || isInfinite(x))
            None
        else
            Some(valueOf(x).shortValue()) as & Pure

    ///
    /// Convert `x` to an `Option[Int32]`.
    ///
    /// If the numeric value of `x` is within the range of Int32 return `Some(x as Int32)`,
    /// rounding `x` towards 0`.
    ///
    /// If the numeric value of `x` is outside the range of Int32 (i.e. -2147483648 to
    /// 2147483647), or it is NaN or infinity, return `None`.
    ///
    pub def toInt32(x: Float64): Option[Int32] =
        import java.lang.Double:valueOf(Float64);
        import java.lang.Double.intValue();
        if (x < Int32.toFloat64(Int32.minValue()) || x > Int32.toFloat64(Int32.maxValue()) || isNan(x) || isInfinite(x))
            None
        else
            Some(valueOf(x).intValue()) as & Pure

    ///
    /// Convert `x` to an `Option[Int64]`.
    ///
    /// If the numeric value of `x` is within the range of Int64 return `Some(x as Int64)`, rounding `x` towards 0`.
    ///
    /// If the numeric value of `x` is outside the range of Int64 (i.e. -9223372036854775808
    /// to 9223372036854775807), or it is NaN or infinity, return `None`.
    ///
    /// Note: while the range of an Int64 is precisely defined using Int64 values, converting this range to
    /// Float64 values is imprecise.
    ///
    pub def toInt64(x: Float64): Option[Int64] =
        import java.lang.Double:valueOf(Float64);
        import java.lang.Double.longValue();
        if (x < Int64.toFloat64(Int64.minValue()) || x > Int64.toFloat64(Int64.maxValue()) || isNan(x) || isInfinite(x))
            None
        else
            Some(valueOf(x).longValue()) as & Pure

    ///
    /// Convert `x` to an `Option[BigInt]`.
    ///
    /// If the numeric value of `x` is representable as a BigInt return `Some(x as BigInt)`.
    ///
    /// If the value of `x` is NaN or infinity return `None`.
    ///
    pub def toBigInt(x: Float64): Option[BigInt] =
        import java.math.BigDecimal:valueOf(Float64);
        import java.math.BigDecimal.toBigInteger();
        try {
            Some(x.valueOf().toBigInteger()) as & Pure
        } catch {
            // Catch NaN, isInfinite.
            case _: ##java.lang.NumberFormatException => None
        }

    ///
    /// Convert `x` to an `Option[Float32]`.
    ///
    /// If the numeric value of `x` is within the range of Float32 return `Some(x as Float32)`, loss
    /// of precision may occur.
    ///
    /// If the numeric value of `x` is outside the range of Float32 (i.e. 1.4E-45
    /// to 3.4028235E38) return `None`.
    ///
    /// If `x` is NaN return `Some(Float32.NaN)``, if `x` is positive or negative infinity return
    /// `Some` wrapping the corresponding Float32 infinity.
    ///
    pub def toFloat32(x: Float64): Option[Float32] =
        import java.lang.Double:valueOf(Float64);
        import java.lang.Double.floatValue();
        if (x < Float32.toFloat64(Float32.minValue()) || x > Float32.toFloat64(Float32.maxValue()))
            None
        else
            Some(valueOf(x).floatValue()) as & Pure

}
