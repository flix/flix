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
pub lawless class BitwiseShr[a] {
    ///
    /// Returns `x` shifted right by `n` bits.
    ///
    pub def shr(x: a, n: Int32): a
}

instance BitwiseShr[Int8] {
    pub def shr(x: Int8, n: Int32): Int8 = $INT8_SHR$(x, n)
}

instance BitwiseShr[Int16] {
    pub def shr(x: Int16, n: Int32): Int16 = $INT16_SHR$(x, n)
}

instance BitwiseShr[Int32] {
    pub def shr(x: Int32, n: Int32): Int32 = $INT32_SHR$(x, n)
}

instance BitwiseShr[Int64] {
    pub def shr(x: Int64, n: Int32): Int64 = $INT64_SHR$(x, n)
}

instance BitwiseShr[BigInt] {
    pub def shr(x: BigInt, n: Int32): BigInt = $BIGINT_SHR$(x, n)
}
