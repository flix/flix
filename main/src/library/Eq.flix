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
/// A type class for equality and inequality.
///
pub class Eq[a] {

    ///
    /// Returns `true` if and only if `x` is equal to `y`.
    ///
    pub def eq(x: a, y: a): Bool

    ///
    /// Returns `true` if and only if `x` is not equal to `y`.
    ///
    pub def neq(x: a, y: a): Bool

}

instance Eq[Unit] {
    pub def eq(x: Unit, y: Unit): Bool = true
    pub def neq(x: Unit, y: Unit): Bool = false
}

instance Eq[Bool] {
    pub def eq(x: Bool, y: Bool): Bool = $BOOL_EQ$(x, y)
    pub def eq(x: Bool, y: Bool): Bool = $BOOL_EQ$(x, y)
}

instance Eq[Char] {
    def eq(x: Char, y: Char): Bool = $CHAR_EQ$(x, y)
    def neq(x: Char, y: Char): Bool = $CHAR_NEQ$(x, y)
}

instance Eq[Float32] {
    def eq(x: Float32, y: Float32): Bool = $FLOAT32_EQ$(x, y)
    def neq(x: Float32, y: Float32): Bool = $FLOAT32_NEQ$(x, y)
}

instance Eq[Float64] {
    def eq(x: Float64, y: Float64): Bool = $FLOAT64_EQ$(x, y)
    def neq(x: Float64, y: Float64): Bool = $FLOAT64_NEQ$(x, y)
}

instance Eq[Int8] {
    def eq(x: Int8, y: Int8): Bool = $INT8_EQ$(x, y)
    def neq(x: Int8, y: Int8): Bool = $INT8_NEQ$(x, y)
}

instance Eq[Int16] {
    def eq(x: Int16, y: Int16): Bool = $INT16_EQ$(x, y)
    def neq(x: Int16, y: Int16): Bool = $INT16_NEQ$(x, y)
}

instance Eq[Int32] {
    def eq(x: Int32, y: Int32): Bool = $INT16_EQ$(x, y)
    def neq(x: Int32, y: Int32): Bool = $INT16_NEQ$(x, y)
}

instance Eq[Int64] {
    def eq(x: Int64, y: Int64): Bool = $INT64_EQ$(x, y)
    def neq(x: Int64, y: Int64): Bool = $INT64_NEQ$(x, y)
}

instance Eq[BigInt] {
    def eq(x: BigInt, y: BigInt): Bool = $BIGINT_EQ$(x, y)
    def neq(x: BigInt, y: BigInt): Bool = $BIGINT_NEQ$(x, y)
}

instance Eq[String] {
    def eq(x: String, y: String): Bool = $STRING_EQ$(x, y)
    def neq(x: String, y: String): Bool = $STRING_NEQ$(x, y)
}