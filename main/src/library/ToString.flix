/*
 *  Copyright 2020 Matthew Lutze
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
/// A trait for types that can be converted to strings.
///
pub trait ToString[a] {
    type Aef: Eff
    ///
    /// Returns a string representation of the given x.
    ///
    pub def toString(x: a): String \ ToString.Aef[a]
}

instance ToString[Unit] {
    type Aef = Pure
    pub def toString(_x: Unit): String = "()"
}

instance ToString[Bool] {
    type Aef = Pure
    pub def toString(x: Bool): String = match x {
        case true  => "true"
        case false => "false"
    }
}

instance ToString[Char] {
    type Aef = Pure
    pub def toString(x: Char): String =
        import static java.lang.Character.toString(Char): String \ {};
        toString(x)
}

instance ToString[Float32] {
    type Aef = Pure
    pub def toString(x: Float32): String =
        import static java.lang.Float.toString(Float32): String \ {};
        toString(x)
}

instance ToString[Float64] {
    type Aef = Pure
    pub def toString(x: Float64): String =
        import static java.lang.Double.toString(Float64): String \ {};
        toString(x)
}

instance ToString[Int8] {
    type Aef = Pure
    pub def toString(x: Int8): String =
        import static java.lang.Byte.toString(Int8): String \ {};
        toString(x)
}

instance ToString[Int16] {
    type Aef = Pure
    pub def toString(x: Int16): String =
        import static java.lang.Short.toString(Int16): String \ {};
        toString(x)
}

instance ToString[Int32] {
    type Aef = Pure
    pub def toString(x: Int32): String =
        import static java.lang.Integer.toString(Int32): String \ {};
        toString(x)
}

instance ToString[Int64] {
    type Aef = Pure
    pub def toString(x: Int64): String =
        import static java.lang.Long.toString(Int64): String \ {};
        toString(x)
}

instance ToString[String] {
    type Aef = Pure
    pub def toString(x: String): String = x
}

instance ToString[BigInt] {
    type Aef = Pure
    pub def toString(x: BigInt): String =
        import java.math.BigInteger.toString(): String \ {};
        toString(x)
}

instance ToString[BigDecimal] {
    type Aef = Pure
    pub def toString(x: BigDecimal): String =
        import java.math.BigDecimal.toString(): String \ {};
        toString(x)
}

instance ToString[Regex] {
    type Aef = Pure
    pub def toString(x: Regex): String =
        import java.util.regex.Pattern.toString(): String \ {};
        toString(x)
}

instance ToString[(a1, a2)] with ToString[a1], ToString[a2] {
    type Aef = ToString.Aef[a1] + ToString.Aef[a2]
    pub def toString(t: (a1, a2)): String \ ToString.Aef[a1] + ToString.Aef[a2] = match t {
        case (x1, x2) =>
            "(${x1}, ${x2})"
    }
}

instance ToString[(a1, a2, a3)] with ToString[a1], ToString[a2], ToString[a3] {
    type Aef = ToString.Aef[a1] + ToString.Aef[a2] + ToString.Aef[a3]
    pub def toString(t: (a1, a2, a3)): String \ ToString.Aef[a1] + ToString.Aef[a2] + ToString.Aef[a3] = match t {
        case (x1, x2, x3) =>
            "(${x1}, ${x2}, ${x3})"
    }
}

instance ToString[(a1, a2, a3, a4)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4] {
    type Aef = ToString.Aef[a1] + ToString.Aef[a2] + ToString.Aef[a3] + ToString.Aef[a4]
    pub def toString(t: (a1, a2, a3, a4)): String \ ToString.Aef[a1] + ToString.Aef[a2] + ToString.Aef[a3] + ToString.Aef[a4] = match t {
        case (x1, x2, x3, x4) =>
            "(${x1}, ${x2}, ${x3}, ${x4})"
    }
}

instance ToString[(a1, a2, a3, a4, a5)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5] {
    type Aef = ToString.Aef[a1] + ToString.Aef[a2] + ToString.Aef[a3] + ToString.Aef[a4] + ToString.Aef[a5]
    pub def toString(t: (a1, a2, a3, a4, a5)): String \ ToString.Aef[a1] + ToString.Aef[a2] + ToString.Aef[a3] + ToString.Aef[a4] + ToString.Aef[a5] = match t {
        case (x1, x2, x3, x4, x5) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6] {
    type Aef = ToString.Aef[a1] + ToString.Aef[a2] + ToString.Aef[a3] + ToString.Aef[a4] + ToString.Aef[a5] + ToString.Aef[a6]
    pub def toString(t: (a1, a2, a3, a4, a5, a6)): String \ ToString.Aef[a1] + ToString.Aef[a2] + ToString.Aef[a3] + ToString.Aef[a4] + ToString.Aef[a5] + ToString.Aef[a6] = match t {
        case (x1, x2, x3, x4, x5, x6) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6})"
    }
}
instance ToString[(a1, a2, a3, a4, a5, a6, a7)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7] {
    type Aef = ToString.Aef[a1] + ToString.Aef[a2] + ToString.Aef[a3] + ToString.Aef[a4] + ToString.Aef[a5] + ToString.Aef[a6] + ToString.Aef[a7]
    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7)): String \ ToString.Aef[a1] + ToString.Aef[a2] + ToString.Aef[a3] + ToString.Aef[a4] + ToString.Aef[a5] + ToString.Aef[a6] + ToString.Aef[a7] = match t {
        case (x1, x2, x3, x4, x5, x6, x7) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7, a8)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7], ToString[a8] {
    type Aef = ToString.Aef[a1] + ToString.Aef[a2] + ToString.Aef[a3] + ToString.Aef[a4] + ToString.Aef[a5] + ToString.Aef[a6] + ToString.Aef[a7] + ToString.Aef[a8]
    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8)): String \ ToString.Aef[a1] + ToString.Aef[a2] + ToString.Aef[a3] + ToString.Aef[a4] + ToString.Aef[a5] + ToString.Aef[a6] + ToString.Aef[a7] + ToString.Aef[a8] = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7], ToString[a8], ToString[a9] {
    type Aef = ToString.Aef[a1] + ToString.Aef[a2] + ToString.Aef[a3] + ToString.Aef[a4] + ToString.Aef[a5] + ToString.Aef[a6] + ToString.Aef[a7] + ToString.Aef[a8] + ToString.Aef[a9]
    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9)): String \ ToString.Aef[a1] + ToString.Aef[a2] + ToString.Aef[a3] + ToString.Aef[a4] + ToString.Aef[a5] + ToString.Aef[a6] + ToString.Aef[a7] + ToString.Aef[a8] + ToString.Aef[a9] = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8, x9) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8}, ${x9})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7], ToString[a8], ToString[a9], ToString[a10] {
    type Aef = ToString.Aef[a1] + ToString.Aef[a2] + ToString.Aef[a3] + ToString.Aef[a4] + ToString.Aef[a5] + ToString.Aef[a6] + ToString.Aef[a7] + ToString.Aef[a8] + ToString.Aef[a9] + ToString.Aef[a10]
    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)): String \ ToString.Aef[a1] + ToString.Aef[a2] + ToString.Aef[a3] + ToString.Aef[a4] + ToString.Aef[a5] + ToString.Aef[a6] + ToString.Aef[a7] + ToString.Aef[a8] + ToString.Aef[a9] + ToString.Aef[a10] = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8}, ${x9}, ${x10})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7], ToString[a8], ToString[a9], ToString[a10], ToString[a11] {
    type Aef = ToString.Aef[a1] + ToString.Aef[a2] + ToString.Aef[a3] + ToString.Aef[a4] + ToString.Aef[a5] + ToString.Aef[a6] + ToString.Aef[a7] + ToString.Aef[a8] + ToString.Aef[a9] + ToString.Aef[a10] + ToString.Aef[a11]
    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)): String \ ToString.Aef[a1] + ToString.Aef[a2] + ToString.Aef[a3] + ToString.Aef[a4] + ToString.Aef[a5] + ToString.Aef[a6] + ToString.Aef[a7] + ToString.Aef[a8] + ToString.Aef[a9] + ToString.Aef[a10] + ToString.Aef[a11] = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8}, ${x9}, ${x10}, ${x11})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7], ToString[a8], ToString[a9], ToString[a10], ToString[a11], ToString[a12] {
    type Aef = ToString.Aef[a1] + ToString.Aef[a2] + ToString.Aef[a3] + ToString.Aef[a4] + ToString.Aef[a5] + ToString.Aef[a6] + ToString.Aef[a7] + ToString.Aef[a8] + ToString.Aef[a9] + ToString.Aef[a10] + ToString.Aef[a11] + ToString.Aef[a12]
    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)): String \ ToString.Aef[a1] + ToString.Aef[a2] + ToString.Aef[a3] + ToString.Aef[a4] + ToString.Aef[a5] + ToString.Aef[a6] + ToString.Aef[a7] + ToString.Aef[a8] + ToString.Aef[a9] + ToString.Aef[a10] + ToString.Aef[a11] + ToString.Aef[a12] = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8}, ${x9}, ${x10}, ${x11}, ${x12})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7], ToString[a8], ToString[a9], ToString[a10], ToString[a11], ToString[a12], ToString[a13] {
    type Aef = ToString.Aef[a1] + ToString.Aef[a2] + ToString.Aef[a3] + ToString.Aef[a4] + ToString.Aef[a5] + ToString.Aef[a6] + ToString.Aef[a7] + ToString.Aef[a8] + ToString.Aef[a9] + ToString.Aef[a10] + ToString.Aef[a11] + ToString.Aef[a12] + ToString.Aef[a13]
    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)): String \ ToString.Aef[a1] + ToString.Aef[a2] + ToString.Aef[a3] + ToString.Aef[a4] + ToString.Aef[a5] + ToString.Aef[a6] + ToString.Aef[a7] + ToString.Aef[a8] + ToString.Aef[a9] + ToString.Aef[a10] + ToString.Aef[a11] + ToString.Aef[a12] + ToString.Aef[a13] = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8}, ${x9}, ${x10}, ${x11}, ${x12}, ${x13})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7], ToString[a8], ToString[a9], ToString[a10], ToString[a11], ToString[a12], ToString[a13], ToString[a14] {
    type Aef = ToString.Aef[a1] + ToString.Aef[a2] + ToString.Aef[a3] + ToString.Aef[a4] + ToString.Aef[a5] + ToString.Aef[a6] + ToString.Aef[a7] + ToString.Aef[a8] + ToString.Aef[a9] + ToString.Aef[a10] + ToString.Aef[a11] + ToString.Aef[a12] + ToString.Aef[a13] + ToString.Aef[a14]
    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)): String \ ToString.Aef[a1] + ToString.Aef[a2] + ToString.Aef[a3] + ToString.Aef[a4] + ToString.Aef[a5] + ToString.Aef[a6] + ToString.Aef[a7] + ToString.Aef[a8] + ToString.Aef[a9] + ToString.Aef[a10] + ToString.Aef[a11] + ToString.Aef[a12] + ToString.Aef[a13] + ToString.Aef[a14] = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8}, ${x9}, ${x10}, ${x11}, ${x12}, ${x13}, ${x14})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7], ToString[a8], ToString[a9], ToString[a10], ToString[a11], ToString[a12], ToString[a13], ToString[a14], ToString[a15] {
    type Aef = ToString.Aef[a1] + ToString.Aef[a2] + ToString.Aef[a3] + ToString.Aef[a4] + ToString.Aef[a5] + ToString.Aef[a6] + ToString.Aef[a7] + ToString.Aef[a8] + ToString.Aef[a9] + ToString.Aef[a10] + ToString.Aef[a11] + ToString.Aef[a12] + ToString.Aef[a13] + ToString.Aef[a14] + ToString.Aef[a15]
    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)): String \ ToString.Aef[a1] + ToString.Aef[a2] + ToString.Aef[a3] + ToString.Aef[a4] + ToString.Aef[a5] + ToString.Aef[a6] + ToString.Aef[a7] + ToString.Aef[a8] + ToString.Aef[a9] + ToString.Aef[a10] + ToString.Aef[a11] + ToString.Aef[a12] + ToString.Aef[a13] + ToString.Aef[a14] + ToString.Aef[a15] = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8}, ${x9}, ${x10}, ${x11}, ${x12}, ${x13}, ${x14}, ${x15})"
    }
}
