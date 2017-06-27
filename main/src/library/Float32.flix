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
    def size(): Int32 = 32

    ///
    /// Returns the maximum exponent that a `Float32` may have.
    ///
    def maxExponent(): Int32 = 127

    ///
    /// Returns the minimum exponent that a `Float32` may have.
    ///
    def minExponent(): Int32 = -126

    ///
    /// Returns the maximum number representable by a `Float32`.
    ///
    def maxValue(): Float32 = (2.0f32 - 2.0f32 ** -23.0f32) * (2.0f32 ** 127.0f32)

    ///
    /// Returns the minimum number representable by a `Float32`.
    ///
    def minValue(): Float32 = -maxValue()

    ///
    /// Returns the minimum positive number representable by a `Float32`.
    ///
    def minPositiveValue(): Float32 = 2.0f32 ** -149.0f32

    ///
    /// Returns the NaN (not a number) value of type `Float32`.
    ///
    def nan(): Float32 = 0.0f32 / 0.0f32

    ///
    /// Returns the positive infinity value of type `Float32`.
    ///
    def positiveInfinity(): Float32 = 1.0f32 / 0.0f32

    ///
    /// Returns the negative infinity value of type `Float32`.
    ///
    def negativeInfinity(): Float32 = -1.0f32 / 0.0f32

    ///
    /// Returns true if and only if `x` is a non-infinite and non-Nan `Float32` value.
    ///
    def isFinite(x: Float32): Bool = x >= minValue() && x <= maxValue()

    ///
    /// Returns true if and only if `x` is an infinite and non-Nan `Float32` value.
    ///
    def isInfinite(x: Float32): Bool = x == positiveInfinity() || x == negativeInfinity()

    ///
    /// Returns true if and only if `x` is the NaN value of type `Float32`.
    ///
    def isNan(x: Float32): Bool = x != x

    ///
    /// Returns the smaller of `x` and `y`.
    ///
    def min(x: Float32, y: Float32): Float32 = if (x <= y) x else y

    ///
    /// Returns the larger of `x` and `y`.
    ///
    def max(x: Float32, y: Float32): Float32 = if (x >= y) x else y
}
