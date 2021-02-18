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

    law neqInverse(): a -> a -> Bool = x -> y -> (x == y) == not (x != y)

    law reflexivity(): a -> Bool = x -> x == x

}

instance Eq[Unit] {
    def eq(x: Unit, y: Unit): Bool = true
    def neq(x: Unit, y: Unit): Bool = false
}

instance Eq[Bool] {
    def eq(x: Bool, y: Bool): Bool = $BOOL_EQ$(x, y)
    def neq(x: Bool, y: Bool): Bool = $BOOL_NEQ$(x, y)
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
    pub def eq(x: Float64, y: Float64): Bool = $FLOAT64_EQ$(x, y)
    pub def neq(x: Float64, y: Float64): Bool = $FLOAT64_NEQ$(x, y)
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

instance Eq[(a1, a2)] with [a1 : Eq, a2 : Eq] {

    def eq(t1: (a1, a2), t2: (a1, a2)): Bool =
        let (x1, x2) = t1;
        let (y1, y2) = t2;
            x1 == y1 and x2 == y2

    def neq(t1: (a1, a2), t2: (a1, a2)): Bool = not (t1 == t2)

}

instance Eq[(a1, a2, a3)] with [a1 : Eq, a2 : Eq, a3 : Eq] {

    def eq(t1: (a1, a2, a3), t2: (a1, a2, a3)): Bool =
        let (x1, x2, x3) = t1;
        let (y1, y2, y3) = t2;
            x1 == y1 and x2 == y2 and x3 == y3

    def neq(t1: (a1, a2, a3), t2: (a1, a2, a3)): Bool = not (t1 == t2)

}

instance Eq[(a1, a2, a3, a4)] with [a1 : Eq, a2 : Eq, a3 : Eq, a4 : Eq] {

    def eq(t1: (a1, a2, a3, a4), t2: (a1, a2, a3, a4)): Bool =
        let (x1, x2, x3, x4) = t1;
        let (y1, y2, y3, y4) = t2;
            x1 == y1 and x2 == y2 and x3 == y3 and x4 == y4

    def neq(t1: (a1, a2, a3, a4), t2: (a1, a2, a3, a4)): Bool = not (t1 == t2)

}

instance Eq[(a1, a2, a3, a4, a5)] with [a1 : Eq, a2 : Eq, a3 : Eq, a4 : Eq, a5 : Eq] {

    def eq(t1: (a1, a2, a3, a4, a5), t2: (a1, a2, a3, a4, a5)): Bool =
        let (x1, x2, x3, x4, x5) = t1;
        let (y1, y2, y3, y4, y5) = t2;
            x1 == y1 and x2 == y2 and x3 == y3 and x4 == y4 and x5 == y5

    def neq(t1: (a1, a2, a3, a4, a5), t2: (a1, a2, a3, a4, a5)): Bool = not (t1 == t2)

}

instance Eq[(a1, a2, a3, a4, a5, a6)] with [a1 : Eq, a2 : Eq, a3 : Eq, a4 : Eq, a5 : Eq, a6 : Eq] {

    def eq(t1: (a1, a2, a3, a4, a5, a6), t2: (a1, a2, a3, a4, a5, a6)): Bool =
        let (x1, x2, x3, x4, x5, x6) = t1;
        let (y1, y2, y3, y4, y5, y6) = t2;
            x1 == y1 and x2 == y2 and x3 == y3 and x4 == y4 and x5 == y5 and x6 == y6

    def neq(t1: (a1, a2, a3, a4, a5, a6), t2: (a1, a2, a3, a4, a5, a6)): Bool = not (t1 == t2)

}

instance Eq[(a1, a2, a3, a4, a5, a6, a7)] with [a1 : Eq, a2 : Eq, a3 : Eq, a4 : Eq, a5 : Eq, a6 : Eq, a7 : Eq] {

    def eq(t1: (a1, a2, a3, a4, a5, a6, a7), t2: (a1, a2, a3, a4, a5, a6, a7)): Bool =
        let (x1, x2, x3, x4, x5, x6, x7) = t1;
        let (y1, y2, y3, y4, y5, y6, y7) = t2;
            x1 == y1 and x2 == y2 and x3 == y3 and x4 == y4 and x5 == y5 and x6 == y6 and x7 == y7

    def neq(t1: (a1, a2, a3, a4, a5, a6, a7), t2: (a1, a2, a3, a4, a5, a6, a7)): Bool = not (t1 == t2)

}

instance Eq[(a1, a2, a3, a4, a5, a6, a7, a8)] with [a1 : Eq, a2 : Eq, a3 : Eq, a4 : Eq, a5 : Eq, a6 : Eq, a7 : Eq, a8 : Eq] {

    def eq(t1: (a1, a2, a3, a4, a5, a6, a7, a8), t2: (a1, a2, a3, a4, a5, a6, a7, a8)): Bool =
        let (x1, x2, x3, x4, x5, x6, x7, x8) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8) = t2;
            x1 == y1 and x2 == y2 and x3 == y3 and x4 == y4 and x5 == y5 and x6 == y6 and x7 == y7 and x8 == y8

    def neq(t1: (a1, a2, a3, a4, a5, a6, a7, a8), t2: (a1, a2, a3, a4, a5, a6, a7, a8)): Bool = not (t1 == t2)

}

instance Eq[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with [a1 : Eq, a2 : Eq, a3 : Eq, a4 : Eq, a5 : Eq, a6 : Eq, a7 : Eq, a8 : Eq, a9 : Eq] {

    def eq(t1: (a1, a2, a3, a4, a5, a6, a7, a8, a9), t2: (a1, a2, a3, a4, a5, a6, a7, a8, a9)): Bool =
        let (x1, x2, x3, x4, x5, x6, x7, x8, x9) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8, y9) = t2;
            x1 == y1 and x2 == y2 and x3 == y3 and x4 == y4 and x5 == y5 and x6 == y6 and x7 == y7 and x8 == y8 and x9 == y9

    def neq(t1: (a1, a2, a3, a4, a5, a6, a7, a8, a9), t2: (a1, a2, a3, a4, a5, a6, a7, a8, a9)): Bool = not (t1 == t2)

}

instance Eq[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with [a1 : Eq, a2 : Eq, a3 : Eq, a4 : Eq, a5 : Eq, a6 : Eq, a7 : Eq, a8 : Eq, a9 : Eq, a10 : Eq] {

    def eq(t1: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10), t2: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)): Bool =
        let (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10) = t2;
            x1 == y1 and x2 == y2 and x3 == y3 and x4 == y4 and x5 == y5 and x6 == y6 and x7 == y7 and x8 == y8 and x9 == y9 and x10 == y10

    def neq(t1: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10), t2: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)): Bool = not (t1 == t2)

}