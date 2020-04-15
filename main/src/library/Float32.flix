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

namespace Float32 {

    ///
    /// Returns the number of bits used to represent a `Float32`.
    ///
    pub def size(): Int32 = 32

    ///
    /// Returns the maximum exponent that a `Float32` may have.
    ///
    pub def maxExponent(): Int32 = 127

    ///
    /// Returns the minimum exponent that a `Float32` may have.
    ///
    pub def minExponent(): Int32 = -126

    ///
    /// Returns the maximum number representable by a `Float32`.
    ///
    pub def maxValue(): Float32 = (2.0f32 - 2.0f32 ** -23.0f32) * (2.0f32 ** 127.0f32)

    ///
    /// Returns the minimum number representable by a `Float32`.
    ///
    pub def minValue(): Float32 = -maxValue()

    ///
    /// Returns the minimum positive number representable by a `Float32`.
    ///
    pub def minPositiveValue(): Float32 = 2.0f32 ** -149.0f32

    ///
    /// Returns the NaN (not a number) value of type `Float32`.
    ///
    pub def nan(): Float32 = 0.0f32 / 0.0f32

    ///
    /// Returns the positive infinity value of type `Float32`.
    ///
    pub def positiveInfinity(): Float32 = 1.0f32 / 0.0f32

    ///
    /// Returns the negative infinity value of type `Float32`.
    ///
    pub def negativeInfinity(): Float32 = -1.0f32 / 0.0f32

    ///
    /// Returns true if and only if `x` is a non-infinite and non-Nan `Float32` value.
    ///
    pub def isFinite(x: Float32): Bool = x >= minValue() && x <= maxValue()

    ///
    /// Returns true if and only if `x` is an infinite and non-Nan `Float32` value.
    ///
    pub def isInfinite(x: Float32): Bool = x == positiveInfinity() || x == negativeInfinity()

    ///
    /// Returns true if and only if `x` is the NaN value of type `Float32`.
    ///
    pub def isNan(x: Float32): Bool = x != x

    ///
    /// Returns the smaller of `x` and `y`.
    ///
    pub def min(x: Float32, y: Float32): Float32 = if (x <= y) x else y

    ///
    /// Returns the larger of `x` and `y`.
    ///
    pub def max(x: Float32, y: Float32): Float32 = if (x >= y) x else y

    ///
    /// Return a string representation of `x`.
    ///
    pub def toString(x: Float32): String =
        import java.lang.Float:toString(Float32);
        x.toString() as & Pure

    ///
    /// Return a decimal representation of `x`.
    ///
    pub def toDecimalString(decimalPlaces: Int32, x: Float32): String =
        Float64.toDecimalString(decimalPlaces, toFloat64(x))

    ///
    /// Parse the string `s` as a Float32, leading or trailing whitespace is trimmed.
    /// A successful parse is wrapped with `OK(x)`, a parse failure is indicated by `Err(_)`.
    ///
    pub def fromString(s: String): Result[Float32, String] = try {
        import java.lang.String.strip();
        import java.lang.Float:parseFloat(String);
        Ok(s.strip().parseFloat()) as & Pure
     } catch {
         case _: ##java.lang.NumberFormatException => Err("Float32.fromString")
     }

    /// Narrowing!
    pub def toInt8(x: Float32): Option[Int8] =
        import java.lang.Float:valueOf(Float32);
        import java.lang.Float.byteValue();
        if (x < Int8.toFloat32(Int8.minValue()) || x > Int8.toFloat32(Int8.maxValue()) || isNan(x) || isInfinite(x))
            None
        else
            Some(valueOf(x).byteValue()) as & Pure

    /// Narrowing!
    pub def toInt16(x: Float32): Option[Int16] =
        import java.lang.Float:valueOf(Float32);
        import java.lang.Float.shortValue();
        if (x < Int16.toFloat32(Int16.minValue()) || x > Int16.toFloat32(Int16.maxValue()) || isNan(x) || isInfinite(x))
            None
        else
            Some(valueOf(x).shortValue()) as & Pure

    /// Narrowing!
    pub def toInt32(x: Float32): Option[Int32] =
        import java.lang.Float:valueOf(Float32);
        import java.lang.Float.intValue();
        if (x < Int32.toFloat32(Int32.minValue()) || x > Int32.toFloat32(Int32.maxValue()) || isNan(x) || isInfinite(x))
            None
        else
            Some(valueOf(x).intValue()) as & Pure

    /// Narrowing!
    pub def toInt64(x: Float32): Option[Int64] =
        import java.lang.Float:valueOf(Float32);
        import java.lang.Float.longValue();
        if (x < Int64.toFloat32(Int64.minValue()) || x > Int64.toFloat32(Int64.maxValue()) || isNan(x) || isInfinite(x))
            None
        else
            Some(valueOf(x).longValue()) as & Pure

    /// Narrowing! ??? (NaN, inifinte, ...)
    pub def toBigInt(x: Float32): Option[BigInt] =
        Float64.toBigInt(toFloat64(x))

    pub def toFloat64(x: Float32): Float64 =
        import java.lang.Float:valueOf(Float32);
        import java.lang.Float.doubleValue();
        valueOf(x).doubleValue() as & Pure

}
