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
/// A type class for bitwise right shift.
///
pub class Shr[a] {
    ///
    /// Returns `x` shifted right by `n` bits.
    ///
    pub def shr(x: a, n: Int32): a
}

instance Shr[Int8] {
    def shr(x: Int8, n: Int32): Int8 = $INT8_SHR$(x, n)
}

instance Shr[Int16] {
    def shr(x: Int16, n: Int32): Int16 = $INT16_SHR$(x, n)
}

instance Shr[Int32] {
    def shr(x: Int32, n: Int32): Int32 = $INT32_SHR$(x, n)
}

instance Shr[Int64] {
    def shr(x: Int64, n: Int32): Int64 = $INT64_SHR$(x, n)
}

instance Shr[BigInt] {
    def shr(x: BigInt, n: Int32): BigInt = $BIGINT_SHR$(x, n)
}