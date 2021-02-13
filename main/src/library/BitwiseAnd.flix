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
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

///
/// A type class for bitwise and.
///
pub lawless class BitwiseAnd[a] {
    ///
    /// Returns the bitwise AND of `x` and `y`.
    ///
    pub def and(x: a, y: a): a
}

instance BitwiseAnd[Int8] {
    def and(x: Int8, y: Int8): Int8 = $INT8_AND$(x, y)
}

instance BitwiseAnd[Int16] {
    def and(x: Int16, y: Int16): Int16 = $INT16_AND$(x, y)
}

instance BitwiseAnd[Int32] {
    def and(x: Int32, y: Int32): Int32 = $INT32_AND$(x, y)
}

instance BitwiseAnd[Int64] {
    def and(x: Int64, y: Int64): Int64 = $INT64_AND$(x, y)
}

instance BitwiseAnd[BigInt] {
    def and(x: BigInt, y: BigInt): BigInt = $BIGINT_AND$(x, y)
}