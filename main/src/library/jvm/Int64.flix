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

pub mod Int64 {

    import java.lang.Long
    import java.math.BigDecimal
    import java.math.BigInteger

    ///
    /// Convert `x` to a BigInt.
    ///
    /// The numeric value of `x` is preserved exactly.
    ///
    pub def toBigInt(x: Int64): BigInt =
        BigInteger.valueOf(Long.valueOf(x).longValue())

    ///
    /// Convert `x` to a BigDecimal.
    ///
    /// Warning: The numeric value of `x` may lose precision.
    ///
    pub def toBigDecimal(x: Int64): BigDecimal =
        new BigDecimal(x)

    ///
    /// Get the primitive Int64 value from its object representation (i.e. Long).
    ///
    /// This function is expected to be used when marshaling Int64s from Java. Generally in Flix
    /// code you should not need to use `Long`.
    ///
    pub def longValue(i: Long): Int64 =
        i.longValue()

    ///
    /// Convert an Int64 value to its object representation (i.e. Long).
    ///
    /// This function is expected to be used when marshaling Int64s to Java. Generally in Flix
    /// code you should not need to use `Long`.
    ///
    pub def valueOf(i: Int64): Long =
        Long.valueOf(i)

}
