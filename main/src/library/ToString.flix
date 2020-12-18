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
/// A type class for types that can be converted to strings.
///
pub class ToString[a] {
    ///
    /// Returns a string representation of the given x.
    ///
    pub def toString(x: a): String
}

instance ToString[Unit] {
    def toString(x: Unit): String = "()"
}

instance ToString[Bool] {
    def toString(x: Bool): String = match x {
        case true  => "true"
        case false => "false"
    }
}

instance ToString[Char] {
    def toString(x: Char): String =
        import java.lang.Character:toString(Char);
        x.toString() as & Pure
}

instance ToString[Float32] {
    def toString(x: Float32): String =
        import java.lang.Float:toString(Float32);
        x.toString() as & Pure
}

instance ToString[Float64] {
    def toString(x: Float64): String =
        import java.lang.Double:toString(Float64);
        x.toString() as & Pure
}

instance ToString[Int8] {
    def toString(x: Int8): String =
        import java.lang.Byte:toString(Int8);
        x.toString() as & Pure
}

instance ToString[Int16] {
    def toString(x: Int16): String =
        import java.lang.Short:toString(Int16);
        x.toString() as & Pure
}

instance ToString[Int32] {
    def toString(x: Int32): String =
        import java.lang.Integer:toString(Int32);
        x.toString() as & Pure
}

instance ToString[Int64] {
    def toString(x: Int64): String =
        import java.lang.Long:toString(Int64);
        x.toString() as & Pure
}

instance ToString[String] {
    def toString(x: String): String = x
}

instance ToString[BigInt] {
    def toString(x: BigInt): String =
        import java.math.BigInteger.toString();
        x.toString() as & Pure
}

instance ToString[(a1, a2)] with [a1 : ToString, a2 : ToString] {
    def toString(t: (a1, a2)): String = match t {
        case (x1, x2) =>
            "(${x1}, ${x2})"
    }
}

instance ToString[(a1, a2, a3)] with [a1 : ToString, a2 : ToString, a3 : ToString] {
    def toString(t: (a1, a2, a3)): String = match t {
        case (x1, x2, x3) =>
            "(${x1}, ${x2}, ${x3})"
    }
}

instance ToString[(a1, a2, a3, a4)] with [a1 : ToString, a2 : ToString, a3 : ToString, a4 : ToString] {
    def toString(t: (a1, a2, a3, a4)): String = match t {
        case (x1, x2, x3, x4) =>
            "(${x1}, ${x2}, ${x3}, ${x4})"
    }
}

instance ToString[(a1, a2, a3, a4, a5)] with [a1 : ToString, a2 : ToString, a3 : ToString, a4 : ToString, a5 : ToString] {
    def toString(t: (a1, a2, a3, a4, a5)): String = match t {
        case (x1, x2, x3, x4, x5) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6)] with [a1 : ToString, a2 : ToString, a3 : ToString, a4 : ToString, a5 : ToString, a6 : ToString] {
    def toString(t: (a1, a2, a3, a4, a5, a6)): String = match t {
        case (x1, x2, x3, x4, x5, x6) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7)] with [a1 : ToString, a2 : ToString, a3 : ToString, a4 : ToString, a5 : ToString, a6 : ToString, a7 : ToString] {
    def toString(t: (a1, a2, a3, a4, a5, a6, a7)): String = match t {
        case (x1, x2, x3, x4, x5, x6, x7) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7, a8)] with [a1 : ToString, a2 : ToString, a3 : ToString, a4 : ToString, a5 : ToString, a6 : ToString, a7 : ToString, a8 : ToString] {
    def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8)): String = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with [a1 : ToString, a2 : ToString, a3 : ToString, a4 : ToString, a5 : ToString, a6 : ToString, a7 : ToString, a8 : ToString, a9 : ToString] {
    def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9)): String = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8, x9) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8}, ${x9})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with [a1 : ToString, a2 : ToString, a3 : ToString, a4 : ToString, a5 : ToString, a6 : ToString, a7 : ToString, a8 : ToString, a9 : ToString, a10 : ToString] {
    def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)): String = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8}, ${x9}, ${x10})"
    }
}