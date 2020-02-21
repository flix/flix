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
}
