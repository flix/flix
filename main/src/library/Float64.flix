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
}
