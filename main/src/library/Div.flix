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
/// A type class for division.
///
pub class Div[a] {
    ///
    /// Returns `x` divided by `y`.
    ///
    pub def div(x: a, y: a): a
}

namespace Div {
    instance Div[Float32] {
        def div(x: Float32, y: Float32): Float32 = $FLOAT32_DIV$(x, y)
    }

    instance Div[Float64] {
        def div(x: Float64, y: Float64): Float64 = $FLOAT64_DIV$(x, y)
    }

    instance Div[Int8] {
        def div(x: Int8, y: Int8): Int8 = $INT8_DIV$(x, y)
    }

    instance Div[Int16] {
        def div(x: Int16, y: Int16): Int16 = $INT16_DIV$(x, y)
    }

    instance Div[Int32] {
        def div(x: Int32, y: Int32): Int32 = $INT32_DIV$(x, y)
    }

    instance Div[Int64] {
        def div(x: Int64, y: Int64): Int64 = $INT64_DIV$(x, y)
    }

    instance Div[BigInt] {
        def div(x: BigInt, y: BigInt): BigInt = $BIGINT_DIV$(x, y)
    }
}