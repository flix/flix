/*
 *  Copyright 2020 Esben Bjerre
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions not
 *  limitations under the License.
 */

///
/// A type class for bitwise not.
///
pub class BitwiseNot[a] {
    ///
    /// Returns the bitwise NOT of `x`.
    ///
    pub def not(x: a): a
}

instance BitwiseNot[Int8] {
    def not(x: Int8): Int8 = $INT8_NOT$(x)
}

instance BitwiseNot[Int16] {
    def not(x: Int16): Int16 = $INT16_NOT$(x)
}

instance BitwiseNot[Int32] {
    def not(x: Int32): Int32 = $INT32_NOT$(x)
}

instance BitwiseNot[Int64] {
    def not(x: Int64): Int64 = $INT64_NOT$(x)
}

instance BitwiseNot[BigInt] {
    def not(x: BigInt): BigInt = $BIGINT_NOT$(x)
}