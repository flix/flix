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
    def toString(x: Char): String = Char.toString(x)
}

instance ToString[Float32] {
    def toString(x: Float32): String = Float32.toString(x)
}

instance ToString[Float64] {
    def toString(x: Float64): String = Float64.toString(x)
}

instance ToString[Int8] {
    def toString(x: Int8): String = Int8.toString(x)
}

instance ToString[Int16] {
    def toString(x: Int16): String = Int16.toString(x)
}

instance ToString[Int32] {
    def toString(x: Int32): String = Int32.toString(x)
}

instance ToString[Int64] {
    def toString(x: Int64): String = Int64.toString(x)
}

instance ToString[String] {
    def toString(x: String): String = x
}

instance ToString[BigInt] {
    def toString(x: BigInt): String = BigInt.toString(x)
}

instance ToString[(a1, a2)] with [a1 : ToString, a2 : ToString] {
    def toString(t: (a1, a2)): String = match t {
        case (x1, x2) =>
            "(${ToString.toString(x1)}, ${ToString.toString(x2)})"
    }
}

instance ToString[(a1, a2, a3)] with [a1 : ToString, a2 : ToString, a3 : ToString] {
    def toString(t: (a1, a2, a3)): String = match t {
        case (x1, x2, x3) =>
            "(${ToString.toString(x1)}, ${ToString.toString(x2)}, ${ToString.toString(x3)})"
    }
}

instance ToString[(a1, a2, a3, a4)] with [a1 : ToString, a2 : ToString, a3 : ToString, a4 : ToString] {
    def toString(t: (a1, a2, a3, a4)): String = match t {
        case (x1, x2, x3, x4) =>
            "(${ToString.toString(x1)}, ${ToString.toString(x2)}, ${ToString.toString(x3)}, ${ToString.toString(x4)})"
    }
}

instance ToString[(a1, a2, a3, a4, a5)] with [a1 : ToString, a2 : ToString, a3 : ToString, a4 : ToString, a5 : ToString] {
    def toString(t: (a1, a2, a3, a4, a5)): String = match t {
        case (x1, x2, x3, x4, x5) =>
            "(${ToString.toString(x1)}, ${ToString.toString(x2)}, ${ToString.toString(x3)}, ${ToString.toString(x4)}, ${ToString.toString(x5)})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6)] with [a1 : ToString, a2 : ToString, a3 : ToString, a4 : ToString, a5 : ToString, a6 : ToString] {
    def toString(t: (a1, a2, a3, a4, a5, a6)): String = match t {
        case (x1, x2, x3, x4, x5, x6) =>
            "(${ToString.toString(x1)}, ${ToString.toString(x2)}, ${ToString.toString(x3)}, ${ToString.toString(x4)}, ${ToString.toString(x5)}, ${ToString.toString(x6)})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7)] with [a1 : ToString, a2 : ToString, a3 : ToString, a4 : ToString, a5 : ToString, a6 : ToString, a7 : ToString] {
    def toString(t: (a1, a2, a3, a4, a5, a6, a7)): String = match t {
        case (x1, x2, x3, x4, x5, x6, x7) =>
            "(${ToString.toString(x1)}, ${ToString.toString(x2)}, ${ToString.toString(x3)}, ${ToString.toString(x4)}, ${ToString.toString(x5)}, ${ToString.toString(x6)}, ${ToString.toString(x7)})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7, a8)] with [a1 : ToString, a2 : ToString, a3 : ToString, a4 : ToString, a5 : ToString, a6 : ToString, a7 : ToString, a8 : ToString] {
    def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8)): String = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8) =>
            "(${ToString.toString(x1)}, ${ToString.toString(x2)}, ${ToString.toString(x3)}, ${ToString.toString(x4)}, ${ToString.toString(x5)}, ${ToString.toString(x6)}, ${ToString.toString(x7)}, ${ToString.toString(x8)})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with [a1 : ToString, a2 : ToString, a3 : ToString, a4 : ToString, a5 : ToString, a6 : ToString, a7 : ToString, a8 : ToString, a9 : ToString] {
    def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9)): String = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8, x9) =>
            "(${ToString.toString(x1)}, ${ToString.toString(x2)}, ${ToString.toString(x3)}, ${ToString.toString(x4)}, ${ToString.toString(x5)}, ${ToString.toString(x6)}, ${ToString.toString(x7)}, ${ToString.toString(x8)}, ${ToString.toString(x9)})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with [a1 : ToString, a2 : ToString, a3 : ToString, a4 : ToString, a5 : ToString, a6 : ToString, a7 : ToString, a8 : ToString, a9 : ToString, a10 : ToString] {
    def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)): String = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) =>
            "(${ToString.toString(x1)}, ${ToString.toString(x2)}, ${ToString.toString(x3)}, ${ToString.toString(x4)}, ${ToString.toString(x5)}, ${ToString.toString(x6)}, ${ToString.toString(x7)}, ${ToString.toString(x8)}, ${ToString.toString(x9)}, ${ToString.toString(x10)})"
    }
}