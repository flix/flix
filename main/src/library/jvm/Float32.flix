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

pub mod Float32 {

    import java.lang.Double
    import java.lang.Float
    import java.lang.Math

    ///
    /// Convert `x` to an `Option[BigInt]`.
    ///
    /// Returns `Some(x as BigInt)` if the numeric value of `x` is representable as a BigInt.
    ///
    /// Returns `None` if the value of `x` is NaN or infinity.
    ///
    pub def tryToBigInt(x: Float32): Option[BigInt] =
        Float64.tryToBigInt(toFloat64(x))

    ///
    /// Convert `x` to an `Option[BigDecimal]`.
    ///
    /// Returns `Some(x as BigDecimal)` if the numeric value of `x` is representable
    /// as a BigDecimal value.
    ///
    /// If `x` is NaN or infinity return `None`.
    ///
    pub def tryToBigDecimal(x: Float32): Option[BigDecimal] =
        toFloat64(x) |> Float64.tryToBigDecimal

    ///
    /// Returns the absolute value of `x`.
    ///
    pub def abs(x: Float32): Float32 =
        Math.abs(x)

    ///
    /// Returns `x` rounded up to a Float32 representing the nearest larger integer value.
    ///
    pub def ceil(x: Float32): Float32 =
        let x1 = Math.ceil(Float.valueOf(x).doubleValue());
        Double.valueOf(x1).floatValue()

    ///
    /// Returns `x` rounded down to a Float32 representing the nearest smaller integer value.
    ///
    pub def floor(x: Float32): Float32 =
        let x1 = Math.floor(Float.valueOf(x).doubleValue());
        Double.valueOf(x1).floatValue()

    ///
    /// Returns `x` rounded to a Float32 representing the nearest integer value.
    ///
    /// The rounding may be upwards or downwards. If the rounding up and rounding down are equally
    /// close, `x` will be rounded to an even value (i.e. `round(0.5f32) == 0.0f32`).
    ///
    pub def round(x: Float32): Float32 =
        let x1 = Math.rint(Float.valueOf(x).doubleValue());
        Double.valueOf(x1).floatValue()

    ///
    /// Get the primitive Float32 value from its object representation (i.e. java.lang.Float).
    ///
    /// This function is expected to be used when marshaling Float32s from Java. Generally in Flix
    /// code you should not need to use `java.lang.Float`.
    ///
    pub def floatValue(d: Float): Float32 =
        d.floatValue()

    ///
    /// Convert an Float32 value to its object representation (i.e. java.lang.Float).
    ///
    /// This function is expected to be used when marshaling Float32s to Java. Generally in Flix
    /// code you should not need to use `java.lang.Float`.
    ///
    pub def valueOf(d: Float32): Float =
        Float.valueOf(d)

}
