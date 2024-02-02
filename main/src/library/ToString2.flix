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
pub trait ToString2[a] {
    type Aef: Eff
    ///
    /// Returns a string representation of the given x.
    ///
    pub def toString(x: a): String \ ToString2.Aef[a]
}

instance ToString2[Unit] {
    type Aef = Pure
    pub def toString(_x: Unit): String = "()"
}

instance ToString2[Bool] {
    type Aef = Pure
    pub def toString(x: Bool): String = match x {
        case true  => "true"
        case false => "false"
    }
}

instance ToString2[Char] {
    type Aef = Pure
    pub def toString(x: Char): String =
        import static java.lang.Character.toString(Char): String \ {};
        toString(x)
}

instance ToString2[Float32] {
    type Aef = Pure
    pub def toString(x: Float32): String =
        import static java.lang.Float.toString(Float32): String \ {};
        toString(x)
}

instance ToString2[Float64] {
    type Aef = Pure
    pub def toString(x: Float64): String =
        import static java.lang.Double.toString(Float64): String \ {};
        toString(x)
}

instance ToString2[Int8] {
    type Aef = Pure
    pub def toString(x: Int8): String =
        import static java.lang.Byte.toString(Int8): String \ {};
        toString(x)
}

instance ToString2[Int16] {
    type Aef = Pure
    pub def toString(x: Int16): String =
        import static java.lang.Short.toString(Int16): String \ {};
        toString(x)
}

instance ToString2[Int32] {
    type Aef = Pure
    pub def toString(x: Int32): String =
        import static java.lang.Integer.toString(Int32): String \ {};
        toString(x)
}

instance ToString2[Int64] {
    type Aef = Pure
    pub def toString(x: Int64): String =
        import static java.lang.Long.toString(Int64): String \ {};
        toString(x)
}

instance ToString2[String] {
    type Aef = Pure
    pub def toString(x: String): String = x
}

instance ToString2[BigInt] {
    type Aef = Pure
    pub def toString(x: BigInt): String =
        import java.math.BigInteger.toString(): String \ {};
        toString(x)
}

instance ToString2[BigDecimal] {
    type Aef = Pure
    pub def toString(x: BigDecimal): String =
        import java.math.BigDecimal.toString(): String \ {};
        toString(x)
}

instance ToString2[Regex] {
    type Aef = Pure
    pub def toString(x: Regex): String =
        import java.util.regex.Pattern.toString(): String \ {};
        toString(x)
}

instance ToString2[(a1, a2)] with ToString2[a1], ToString2[a2] {
    type Aef = ToString2.Aef[a1] + ToString2.Aef[a2]
    pub def toString(t: (a1, a2)): String \ ToString2.Aef[a1] + ToString2.Aef[a2] = match t {
        case (x1, x2) =>
            "(" + ToString2.toString(x1) + ", " + ToString2.toString(x2) + ")"
    }
}

//instance ToString2[(a1, a2, a3)] with ToString2[a1], ToString2[a2], ToString2[a3] {
//    type Aef = ToString2.Aef[a1] + ToString2.Aef[a2] + ToString2.Aef[a3]
//    pub def toString(t: (a1, a2, a3)): String \ ToString2.Aef[a1] + ToString2.Aef[a2] + ToString2.Aef[a3] = match t {
//        case (x1, x2, x3) =>
//            "(" + ToString2.toString(x1) + ", " + ToString2.toString(x2) + ", " + ToString2.toString(x3) + ")"
//    }
//}

instance ToString2[Array[a, r]] with ToString2[a] {
    type Aef = r + ToString2.Aef[a]

    pub def toString(a: Array[a, r]): String \ r + ToString2.Aef[a] = region rc2 {
        let sb = StringBuilder.new(rc2);
        let first = ref true @ rc2;

        StringBuilder.appendString!("Array#{", sb);

        let f = (x) -> {
            if (not deref first) StringBuilder.appendString!(", ", sb) else first := false;
            StringBuilder.appendString!(ToString2.toString(x), sb)
        };
        Array.forEach(f, a);

        StringBuilder.appendString!("}", sb);
        StringBuilder.toString(sb)
    }
}

//instance ToString2[(a1, a2, a3, a4)] with ToString2[a1], ToString2[a2], ToString2[a3], ToString2[a4] {
//    pub def toString(t: (a1, a2, a3, a4)): String = match t {
//        case (x1, x2, x3, x4) =>
//            "(${x1}, ${x2}, ${x3}, ${x4})"
//    }
//}
//
//instance ToString2[(a1, a2, a3, a4, a5)] with ToString2[a1], ToString2[a2], ToString2[a3], ToString2[a4], ToString2[a5] {
//    pub def toString(t: (a1, a2, a3, a4, a5)): String = match t {
//        case (x1, x2, x3, x4, x5) =>
//            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5})"
//    }
//}
//
//instance ToString2[(a1, a2, a3, a4, a5, a6)] with ToString2[a1], ToString2[a2], ToString2[a3], ToString2[a4], ToString2[a5], ToString2[a6] {
//    pub def toString(t: (a1, a2, a3, a4, a5, a6)): String = match t {
//        case (x1, x2, x3, x4, x5, x6) =>
//            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6})"
//    }
//}
//
//instance ToString2[(a1, a2, a3, a4, a5, a6, a7)] with ToString2[a1], ToString2[a2], ToString2[a3], ToString2[a4], ToString2[a5], ToString2[a6], ToString2[a7] {
//    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7)): String = match t {
//        case (x1, x2, x3, x4, x5, x6, x7) =>
//            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7})"
//    }
//}
//
//instance ToString2[(a1, a2, a3, a4, a5, a6, a7, a8)] with ToString2[a1], ToString2[a2], ToString2[a3], ToString2[a4], ToString2[a5], ToString2[a6], ToString2[a7], ToString2[a8] {
//    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8)): String = match t {
//        case (x1, x2, x3, x4, x5, x6, x7, x8) =>
//            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8})"
//    }
//}
//
//instance ToString2[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with ToString2[a1], ToString2[a2], ToString2[a3], ToString2[a4], ToString2[a5], ToString2[a6], ToString2[a7], ToString2[a8], ToString2[a9] {
//    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9)): String = match t {
//        case (x1, x2, x3, x4, x5, x6, x7, x8, x9) =>
//            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8}, ${x9})"
//    }
//}
//
//instance ToString2[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with ToString2[a1], ToString2[a2], ToString2[a3], ToString2[a4], ToString2[a5], ToString2[a6], ToString2[a7], ToString2[a8], ToString2[a9], ToString2[a10] {
//    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)): String = match t {
//        case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) =>
//            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8}, ${x9}, ${x10})"
//    }
//}
//
//instance ToString2[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)] with ToString2[a1], ToString2[a2], ToString2[a3], ToString2[a4], ToString2[a5], ToString2[a6], ToString2[a7], ToString2[a8], ToString2[a9], ToString2[a10], ToString2[a11] {
//    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)): String = match t {
//        case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) =>
//            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8}, ${x9}, ${x10}, ${x11})"
//    }
//}
//
//instance ToString2[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)] with ToString2[a1], ToString2[a2], ToString2[a3], ToString2[a4], ToString2[a5], ToString2[a6], ToString2[a7], ToString2[a8], ToString2[a9], ToString2[a10], ToString2[a11], ToString2[a12] {
//    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)): String = match t {
//        case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) =>
//            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8}, ${x9}, ${x10}, ${x11}, ${x12})"
//    }
//}
//
//instance ToString2[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)] with ToString2[a1], ToString2[a2], ToString2[a3], ToString2[a4], ToString2[a5], ToString2[a6], ToString2[a7], ToString2[a8], ToString2[a9], ToString2[a10], ToString2[a11], ToString2[a12], ToString2[a13] {
//    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)): String = match t {
//        case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) =>
//            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8}, ${x9}, ${x10}, ${x11}, ${x12}, ${x13})"
//    }
//}
//
//instance ToString2[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)] with ToString2[a1], ToString2[a2], ToString2[a3], ToString2[a4], ToString2[a5], ToString2[a6], ToString2[a7], ToString2[a8], ToString2[a9], ToString2[a10], ToString2[a11], ToString2[a12], ToString2[a13], ToString2[a14] {
//    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)): String = match t {
//        case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) =>
//            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8}, ${x9}, ${x10}, ${x11}, ${x12}, ${x13}, ${x14})"
//    }
//}
//
//instance ToString2[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)] with ToString2[a1], ToString2[a2], ToString2[a3], ToString2[a4], ToString2[a5], ToString2[a6], ToString2[a7], ToString2[a8], ToString2[a9], ToString2[a10], ToString2[a11], ToString2[a12], ToString2[a13], ToString2[a14], ToString2[a15] {
//    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)): String = match t {
//        case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) =>
//            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8}, ${x9}, ${x10}, ${x11}, ${x12}, ${x13}, ${x14}, ${x15})"
//    }
//}
