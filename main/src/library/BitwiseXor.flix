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
/// A type class for bitwise xor.
///
pub lawless class BitwiseXor[a] {
    ///
    /// Returns the bitwise XOR of `x` and `y`.
    ///
    pub def xor(x: a, y: a): a
}

instance BitwiseXor[Int8] {
    pub def xor(x: Int8, y: Int8): Int8 = $INT8_XOR$(x, y)
}

instance BitwiseXor[Int16] {
    pub def xor(x: Int16, y: Int16): Int16 = $INT16_XOR$(x, y)
}

instance BitwiseXor[Int32] {
    pub def xor(x: Int32, y: Int32): Int32 = $INT32_XOR$(x, y)
}

instance BitwiseXor[Int64] {
    pub def xor(x: Int64, y: Int64): Int64 = $INT64_XOR$(x, y)
}

instance BitwiseXor[BigInt] {
    pub def xor(x: BigInt, y: BigInt): BigInt = $BIGINT_XOR$(x, y)
}