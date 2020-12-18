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
/// A type class for bitwise or.
///
pub class Or[a] {
    ///
    /// Returns the bitwise OR of `x` and `y`.
    ///
    pub def or(x: a, y: a): a
}

instance Or[Int8] {
    def or(x: Int8, y: Int8): Int8 = $INT8_OR$(x, y)
}

instance Or[Int16] {
    def or(x: Int16, y: Int16): Int16 = $INT16_OR$(x, y)
}

instance Or[Int32] {
    def or(x: Int32, y: Int32): Int32 = $INT32_OR$(x, y)
}

instance Or[Int64] {
    def or(x: Int64, y: Int64): Int64 = $INT64_OR$(x, y)
}

instance Or[BigInt] {
    def or(x: BigInt, y: BigInt): BigInt = $BIGINT_OR$(x, y)
}