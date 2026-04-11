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

pub mod Int32 {

    import java.lang.Integer
    import java.math.BigDecimal
    import java.math.BigInteger

    ///
    /// Convert `x` to a BigInt.
    ///
    /// The numeric value of `x` is preserved exactly.
    ///
    pub def toBigInt(x: Int32): BigInt =
        BigInteger.valueOf(Integer.valueOf(x).longValue())

    ///
    /// Convert `x` to a BigDecimal.
    ///
    /// The numeric value of `x` is preserved exactly.
    ///
    pub def toBigDecimal(x: Int32): BigDecimal =
        new BigDecimal(x)

    ///
    /// Get the primitive Int32 value from its object representation (i.e. Integer).
    ///
    /// This function is expected to be used when marshaling Int32s from Java. Generally in Flix
    /// code you should not need to use `Integer`.
    ///
    pub def intValue(i: Integer): Int32 =
        i.intValue()

    ///
    /// Convert an Int32 value to its object representation (i.e. Integer).
    ///
    /// This function is expected to be used when marshaling Int32s to Java. Generally in Flix
    /// code you should not need to use `Integer`.
    ///
    pub def valueOf(i: Int32): Integer =
        Integer.valueOf(i)

}
