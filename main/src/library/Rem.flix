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
/// A type class for modulo (i.e. remainder).
///
pub lawless class Rem[a] {
    ///
    /// Returns `x` modulo `n`.
    ///
    pub def rem(x: a, n: a): a
}

instance Rem[Float32] {
    pub def rem(x: Float32, n: Float32): Float32 = $FLOAT32_REM$(x, n)
}

instance Rem[Float64] {
    pub def rem(x: Float64, n: Float64): Float64 = $FLOAT64_REM$(x, n)
}

instance Rem[Int8] {
    pub def rem(x: Int8, n: Int8): Int8 = $INT8_REM$(x, n)
}

instance Rem[Int16] {
    pub def rem(x: Int16, n: Int16): Int16 = $INT16_REM$(x, n)
}

instance Rem[Int32] {
    pub def rem(x: Int32, n: Int32): Int32 = $INT32_REM$(x, n)
}

instance Rem[Int64] {
    pub def rem(x: Int64, n: Int64): Int64 = $INT64_REM$(x, n)
}

instance Rem[BigInt] {
    pub def rem(x: BigInt, n: BigInt): BigInt = $BIGINT_REM$(x, n)
}