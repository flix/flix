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

instance LowerBound[Float32] {
    pub def minValue(): Float32 = Float32.minValue()
}

instance UpperBound[Float32] {
    pub def maxValue(): Float32 = Float32.maxValue()
}

pub mod Float32 {

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
    pub def maxValue(): Float32 = (2.0f32 - pow(base = 2.0f32, -23.0f32)) * pow(base = 2.0f32, 127.0f32)

    ///
    /// Returns the minimum number representable by a `Float32`.
    ///
    pub def minValue(): Float32 = -maxValue()

    ///
    /// Returns the minimum positive number representable by a `Float32`.
    ///
    pub def minPositiveValue(): Float32 = pow(base = 2.0f32, -149.0f32)

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
    pub def isFinite(x: Float32): Bool = x >= minValue() and x <= maxValue()

    ///
    /// Returns true if and only if `x` is an infinite and non-Nan `Float32` value.
    ///
    pub def isInfinite(x: Float32): Bool = x == positiveInfinity() or x == negativeInfinity()

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
    pub def toString(x: Float32): String = ToString.toString(x)

    ///
    /// Parse the string `s` as a Float32, leading or trailing whitespace is trimmed.
    /// A successful parse is wrapped with `Some(x)`, a parse failure is indicated by `None`.
    ///
    pub def fromString(s: String): Option[Float32] = {
        let (ok, v) = %%FLOAT32_FROM_STRING%%(s);
        if (ok) Some(v) else None
    }

    ///
    /// Convert `x` to an `Option[Int8]`.
    ///
    /// Returns `Some(x as Int8)` if the numeric value of `x` is within the range of Int8,
    /// rounding `x` towards 0`.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Int8
    /// (i.e. -128 to 127), or it is NaN or infinity.
    ///
    pub def tryToInt8(x: Float32): Option[Int8] =
        if (x < Int8.toFloat32(Int8.minValue()) or x > Int8.toFloat32(Int8.maxValue()) or isNan(x) or isInfinite(x))
            None
        else
            Some(%%FLOAT32_TO_INT8%%(x))

    ///
    /// Convert `x` to an `Option[Int16]`.
    ///
    /// Returns `Some(x as Int16)` if the numeric value of `x` is within the range of Int16,
    /// rounding `x` towards 0`.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Int16
    /// (i.e. -32768 to 32767), or it is NaN or infinity.
    ///
    pub def tryToInt16(x: Float32): Option[Int16] =
        if (x < Int16.toFloat32(Int16.minValue()) or x > Int16.toFloat32(Int16.maxValue()) or isNan(x) or isInfinite(x))
            None
        else
            Some(%%FLOAT32_TO_INT16%%(x))

    ///
    /// Convert `x` to an `Option[Int32]`.
    ///
    /// Returns `Some(x as Int32)` if the numeric value of `x` is within the range of Int32,
    /// rounding `x` towards 0`.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Int32
    /// (i.e. -2147483648 to 2147483647), or it is NaN or infinity.
    ///
    /// Note: while the range of an Int32 is precisely defined using Int32 values, converting this range to
    /// Float32 values is imprecise.
    ///
    pub def tryToInt32(x: Float32): Option[Int32] =
        if (x < Int32.toFloat32(Int32.minValue()) or x > Int32.toFloat32(Int32.maxValue()) or isNan(x) or isInfinite(x))
            None
        else
            Some(%%FLOAT32_TO_INT32%%(x))

    ///
    /// Convert `x` to an `Option[Int64]`.
    ///
    /// Returns `Some(x as Int64)` if the numeric value of `x` is within the range of Int64,
    /// rounding `x` towards 0`.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Int64
    /// (i.e. -9223372036854775808 to 9223372036854775807), or it is NaN or infinity.
    ///
    /// Note: while the range of an Int64 is precisely defined using Int64 values, converting
    /// this range to Float32 values is imprecise.
    ///
    pub def tryToInt64(x: Float32): Option[Int64] =
        if (x < Int64.toFloat32(Int64.minValue()) or x > Int64.toFloat32(Int64.maxValue()) or isNan(x) or isInfinite(x))
            None
        else
            Some(%%FLOAT32_TO_INT64%%(x))

    ///
    /// Convert `x` to an Float64.
    ///
    pub def toFloat64(x: Float32): Float64 =
        %%FLOAT32_TO_FLOAT64%%(x)

    ///
    /// Helper function for the `clamp` conversion functions.
    ///
    def clamp(min: {min = Float32}, max: {max = Float32}, x: Float32): Float32 =
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
    /// Warning: it is recommended to test `x` for NaN (not-a-number) before calling this
    /// function. Relying on `nanValue` to convert NaN to a permissable Int8 risks masking it.
    ///
    pub def clampToInt8(min: {min = Int8}, max: {max = Int8}, nanValue: {nanValue = Int8}, x: Float32): Int8 =
        let minf32 = Int8.toFloat32(min#min);
        let maxf32 = Int8.toFloat32(max#max);
        if (isNan(x))
            nanValue#nanValue
        else
            %%FLOAT32_TO_INT8%%(clamp(min = minf32, max = maxf32, x))

    ///
    /// Convert `x` to an `Int16`.
    ///
    /// Returns `x` clamped within the Int16 range `min` to `max`.
    ///
    /// Warning: it is recommended to test `x` for NaN (not-a-number) before calling this
    /// function. Relying on `nanValue` to convert NaN to a permissable Int16 risks masking it.
    ///
    pub def clampToInt16(min: {min = Int16}, max: {max = Int16}, nanValue: {nanValue = Int16}, x: Float32): Int16 =
        let minf32 = Int16.toFloat32(min#min);
        let maxf32 = Int16.toFloat32(max#max);
        if (isNan(x))
            nanValue#nanValue
        else
            %%FLOAT32_TO_INT16%%(clamp(min = minf32, max = maxf32, x))

    ///
    /// Convert `x` to an `Int32`.
    ///
    /// Returns `x` clamped within the Int32 range `min` to `max`.
    ///
    /// Warning: it is recommended to test `x` for NaN (not-a-number) before calling this
    /// function. Relying on `nanValue` to convert NaN to a permissable Int32 risks masking it.
    ///
    pub def clampToInt32(min: {min = Int32}, max: {max = Int32}, nanValue: {nanValue = Int32}, x: Float32): Int32 =
        let minf32 = Int32.toFloat32(min#min);
        let maxf32 = Int32.toFloat32(max#max);
        if (isNan(x))
            nanValue#nanValue
        else
            %%FLOAT32_TO_INT32%%(clamp(min = minf32, max = maxf32, x))

    ///
    /// Convert `x` to an `Int64`.
    ///
    /// Returns `x` clamped within the Int64 range `min` to `max`.
    ///
    /// Warning: it is recommended to test `x` for NaN (not-a-number) before calling this
    /// function. Relying on `nanValue` to convert NaN to a permissable Int64 risks masking it.
    ///
    pub def clampToInt64(min: {min = Int64}, max: {max = Int64}, nanValue: {nanValue = Int64}, x: Float32): Int64 =
        let minf32 = Int64.toFloat32(min#min);
        let maxf32 = Int64.toFloat32(max#max);
        if (isNan(x))
            nanValue#nanValue
        else
            %%FLOAT32_TO_INT64%%(clamp(min = minf32, max = maxf32, x))

    ///
    /// Returns `base` raised to the power of `n`.
    ///
    pub def pow(base: {base = Float32}, n: Float32): Float32 = %%FLOAT32_EXP%%(base#base, n)

}
