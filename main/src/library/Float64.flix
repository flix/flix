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
    def size: Int32 = 64

    ///
    /// Returns the maximum exponent that a `Float64` may have.
    ///
    def maxExponent: Int32 = 1023

    ///
    /// Returns the minimum exponent that a `Float64` may have.
    ///
    def minExponent: Int32 = -1022

    ///
    /// Returns the maximum number representable by a `Float64`.
    ///
    def maxValue: Float64 = (2.0f64 - 2.0f64 ** -52.0f64) * (2.0f64 ** 1023.0f64)

    ///
    /// Returns the minimum number representable by a `Float64`.
    ///
    def minValue: Float64 = -maxValue()

    ///
    /// Returns the minimum positive number representable by a `Float64`.
    ///
    def minPositiveValue: Float64 = 2.0f64 ** -1074.0f64

    ///
    /// Returns the NaN (not a number) value of type `Float64`.
    ///
    def undefined: Float64 = 0.0f64 / 0.0f64

    ///
    /// Returns the positive infinity value of type `Float64`.
    ///
    def positiveInfinity: Float64 = 1.0f64 / 0.0f64

    ///
    /// Returns the negative infinity value of type `Float64`.
    ///
    def negativeInfinity: Float64 = -1.0f64 / 0.0f64

    ///
    /// Returns the smaller of `x` and `y`.
    ///
    def min(x: Float64, y: Float64): Float64 = if (x <= y) x else y

    ///
    /// Returns the larger of `x` and `y`.
    ///
    def max(x: Float64, y: Float64): Float64 = if (x >= y) x else y
}
