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
/// A type class for bitwise left shift.
///
pub lawless class BitwiseShl[a] {
    ///
    /// Returns `x` shifted left by `n` bits.
    ///
    pub def shl(x: a, n: Int32): a
}

instance BitwiseShl[Int8] {
    def shl(x: Int8, n: Int32): Int8 = $INT8_SHL$(x, n)
}

instance BitwiseShl[Int16] {
    def shl(x: Int16, n: Int32): Int16 = $INT16_SHL$(x, n)
}

instance BitwiseShl[Int32] {
    def shl(x: Int32, n: Int32): Int32 = $INT32_SHL$(x, n)
}

instance BitwiseShl[Int64] {
    def shl(x: Int64, n: Int32): Int64 = $INT64_SHL$(x, n)
}

instance BitwiseShl[BigInt] {
    def shl(x: BigInt, n: Int32): BigInt = $BIGINT_SHL$(x, n)
}