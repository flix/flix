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
/// A type class for exponentiation.
///
pub class Exp[a] {
    ///
    /// Returns `b` raised to the power of `n`.
    ///
    pub def exp(b: a, n: a): a
}

instance Exp[Float32] {
    pub def exp(b: Float32, n: Float32): Float32 = $FLOAT32_EXP$(b, n)
}

instance Exp[Float64] {
    pub def exp(b: Float64, n: Float64): Float64 = $FLOAT64_EXP$(b, n)
}

instance Exp[Int8] {
    pub def exp(b: Int8, n: Int8): Int8 = $INT8_EXP$(b, n)
}

instance Exp[Int16] {
    pub def exp(b: Int16, n: Int16): Int16 = $INT16_EXP$(b, n)
}

instance Exp[Int32] {
    pub def exp(b: Int32, n: Int32): Int32 = $INT32_EXP$(b, n)
}

instance Exp[Int64] {
    pub def exp(b: Int64, n: Int64): Int64 = $INT64_EXP$(b, n)
}
