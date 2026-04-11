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

pub mod Float64 {

    import java.lang.Double
    import java.lang.Math
    import java.math.BigDecimal
    import java.lang.NumberFormatException

    ///
    /// Convert `x` to an `Option[BigInt]`.
    ///
    /// Returns `Some(x as BigInt)` if the numeric value of `x` is representable as a BigInt.
    ///
    /// Returns `None` if the value of `x` is NaN or infinity.
    ///
    pub def tryToBigInt(x: Float64): Option[BigInt] =
        try {
            Some(BigDecimal.valueOf(x).toBigInteger())
        } catch {
            // Catch NaN, isInfinite.
            case _: NumberFormatException => None
        }

    ///
    /// Convert `x` to an `Option[BigDecimal]`.
    ///
    /// Returns `Some(x as BigDecimal)` if the numeric value of `x` is representable
    /// as a BigDecimal value.
    ///
    /// If `x` is NaN or infinity return `None`.
    ///
    pub def tryToBigDecimal(x: Float64): Option[BigDecimal] =
        try {
            Some(new BigDecimal(x))
        } catch {
            case _: NumberFormatException => None
        }

    ///
    /// Returns the absolute value of `x`.
    ///
    pub def abs(x: Float64): Float64 =
        Math.abs(x)

    ///
    /// Returns `x` rounded up to a Float64 representing the nearest larger integer value.
    ///
    pub def ceil(x: Float64): Float64 =
        Math.ceil(x)

    ///
    /// Returns `x` rounded down to a Float64 representing the nearest smaller integer value.
    ///
    pub def floor(x: Float64): Float64 =
        Math.floor(x)

    ///
    /// Returns `x` rounded to a Float64 representing the nearest integer value.
    ///
    /// The rounding may be upwards or downwards. If the rounding up and rounding down are equally
    /// close, `x` will be rounded to an even value (i.e. `round(0.5f64) == 0.0f64`).
    ///
    pub def round(x: Float64): Float64 =
        Math.rint(x)

    ///
    /// Returns the exponential of `x`.
    ///
    pub def exp(x: Float64): Float64 =
        Math.exp(x)

    ///
    /// Get the primitive Float64 value from its object representation (i.e. java.lang.Double).
    ///
    /// This function is expected to be used when marshaling Float64s from Java. Generally in Flix
    /// code you should not need to use `java.lang.Double`.
    ///
    pub def doubleValue(i: Double): Float64 =
        i.doubleValue()

    ///
    /// Convert an Float64 value to its object representation (i.e. java.lang.Double).
    ///
    /// This function is expected to be used when marshaling Float64s to Java. Generally in Flix
    /// code you should not need to use `java.lang.Double`.
    ///
    pub def valueOf(i: Float64): Double =
        Double.valueOf(i)

}
