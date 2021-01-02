/*
 * Copyright 2020 Esben Bjerre
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

///
/// A type class for types with a total order.
///
pub class Ord[a] {

    /// Returns `true` if and only if `x` < `y`.
    pub def less(x: a, y: a): Bool

    /// Returns `true` if and only if `x` <= `y`.
    pub def lessEqual(x: a, y: a): Bool

    /// Returns `true` if and only if `x` > `y`.
    pub def greater(x: a, y: a): Bool

    /// Returns `true` if and only if `x` >= `y`.
    pub def greaterEqual(x: a, y: a): Bool

}

instance Ord[Unit] {

    def less(x: Unit, y: Unit): Bool = false
    def lessEqual(x: Unit, y: Unit): Bool = true
    def greater(x: Unit, y: Unit): Bool = false
    def greaterEqual(x: Unit, y: Unit): Bool = true

}

instance Ord[Bool] {

    def less(x: Bool, y: Bool): Bool = false
    def lessEqual(x: Bool, y: Bool): Bool = true
    def greater(x: Bool, y: Bool): Bool = false
    def greaterEqual(x: Bool, y: Bool): Bool = true

}

instance Ord[Char] {

    def less(x: Char, y: Char): Bool = $CHAR_LT$(x, y)
    def lessEqual(x: Char, y: Char): Bool = $CHAR_LE$(x, y)
    def greater(x: Char, y: Char): Bool = $CHAR_GT$(x, y)
    def greaterEqual(x: Char, y: Char): Bool = $CHAR_GE$(x, y)

}

instance Ord[Float32] {

    def less(x: Float32, y: Float32): Bool = $FLOAT32_LT$(x, y)
    def lessEqual(x: Float32, y: Float32): Bool = $FLOAT32_LE$(x, y)
    def greater(x: Float32, y: Float32): Bool = $FLOAT32_GT$(x, y)
    def greaterEqual(x: Float32, y: Float32): Bool = $FLOAT32_GE$(x, y)

}

instance Ord[Float64] {

    def less(x: Float64, y: Float64): Bool = $FLOAT64_LT$(x, y)
    def lessEqual(x: Float64, y: Float64): Bool = $FLOAT64_LE$(x, y)
    def greater(x: Float64, y: Float64): Bool = $FLOAT64_GT$(x, y)
    def greaterEqual(x: Float64, y: Float64): Bool = $FLOAT64_GE$(x, y)

}

instance Ord[Int8] {

    def less(x: Int8, y: Int8): Bool = $INT8_LT$(x, y)
    def lessEqual(x: Int8, y: Int8): Bool = $INT8_LE$(x, y)
    def greater(x: Int8, y: Int8): Bool = $INT8_GT$(x, y)
    def greaterEqual(x: Int8, y: Int8): Bool = $INT8_GE$(x, y)

}

instance Ord[Int16] {

    def less(x: Int16, y: Int16): Bool = $INT16_LT$(x, y)
    def lessEqual(x: Int16, y: Int16): Bool = $INT16_LE$(x, y)
    def greater(x: Int16, y: Int16): Bool = $INT16_GT$(x, y)
    def greaterEqual(x: Int16, y: Int16): Bool = $INT16_GE$(x, y)

}

instance Ord[Int32] {

    def less(x: Int32, y: Int32): Bool = $INT32_LT$(x, y)
    def lessEqual(x: Int32, y: Int32): Bool = $INT32_LE$(x, y)
    def greater(x: Int32, y: Int32): Bool = $INT32_GT$(x, y)
    def greaterEqual(x: Int32, y: Int32): Bool = $INT32_GE$(x, y)

}

instance Ord[Int64] {

    def less(x: Int64, y: Int64): Bool = $INT64_LT$(x, y)
    def lessEqual(x: Int64, y: Int64): Bool = $INT64_LE$(x, y)
    def greater(x: Int64, y: Int64): Bool = $INT64_GT$(x, y)
    def greaterEqual(x: Int64, y: Int64): Bool = $INT64_GE$(x, y)

}

instance Ord[BigInt] {

    def less(x: BigInt, y: BigInt): Bool = $BIGINT_LT$(x, y)
    def lessEqual(x: BigInt, y: BigInt): Bool = $BIGINT_LE$(x, y)
    def greater(x: BigInt, y: BigInt): Bool = $BIGINT_GT$(x, y)
    def greaterEqual(x: BigInt, y: BigInt): Bool = $BIGINT_GE$(x, y)

}