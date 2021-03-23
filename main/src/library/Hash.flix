/*
 *  Copyright 2020 Matthew Lutze, Esben Bjerre
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
/// A type class for types that can be hashed.
///
pub lawless class Hash[a] {
    ///
    /// Returns a hash value for the given x.
    ///
    pub def hash(x: a): Int
}

instance Hash[Unit] {
    pub def hash(_x: Unit): Int = 0
}

instance Hash[Bool] {
    pub def hash(x: Bool): Int = match x {
        case true => 1231
        case false => 1237
    }
}

instance Hash[Char] {
    pub def hash(x: Char): Int =
        import java.lang.Character:hashCode(Char);
        hashCode(x) as & Pure
}

instance Hash[Float32] {
    pub def hash(x: Float32): Int =
        import java.lang.Float:hashCode(Float32);
        hashCode(x) as & Pure
}

instance Hash[Float64] {
    pub def hash(x: Float64): Int =
        import java.lang.Double:hashCode(Float64);
        hashCode(x) as & Pure
}

instance Hash[Int8] {
    pub def hash(x: Int8): Int =
        import java.lang.Byte:hashCode(Int8);
        hashCode(x) as & Pure
}

instance Hash[Int16] {
    pub def hash(x: Int16): Int =
        import java.lang.Short:hashCode(Int16);
        hashCode(x) as & Pure
}

instance Hash[Int32] {
    pub def hash(x: Int32): Int =
        import java.lang.Integer:hashCode(Int32);
        hashCode(x) as & Pure
}

instance Hash[Int64] {
    pub def hash(x: Int64): Int =
        import java.lang.Long:hashCode(Int64);
        hashCode(x) as & Pure
}

instance Hash[String] {
    pub def hash(x: String): Int =
        import java.lang.String.hashCode();
        hashCode(x) as & Pure
}

instance Hash[BigInt] {
    pub def hash(x: BigInt): Int =
        import java.math.BigInteger.hashCode();
        hashCode(x) as & Pure
}

instance Hash[(a1, a2)] with Hash[a1], Hash[a2] {
    pub def hash(t: (a1, a2)): Int = match t {
        case (x1, x2) => 2 * 3 * Hash.hash(x1) + 5 * Hash.hash(x2)
    }
}

instance Hash[(a1, a2, a3)] with Hash[a1], Hash[a2], Hash[a3] {
    pub def hash(t: (a1, a2, a3)): Int = match t {
        case (x1, x2, x3) => 3 * 5 * Hash.hash(x1) + 7 * Hash.hash(x2) + 11 * Hash.hash(x3)
    }
}

instance Hash[(a1, a2, a3, a4)] with Hash[a1], Hash[a2], Hash[a3], Hash[a4] {
    pub def hash(t: (a1, a2, a3, a4)): Int = match t {
        case (x1, x2, x3, x4) =>
            4 * 5 * Hash.hash(x1) + 7 * Hash.hash(x2) + 11 * Hash.hash(x3) + 13 * Hash.hash(x4)
    }
}

instance Hash[(a1, a2, a3, a4, a5)] with Hash[a1], Hash[a2], Hash[a3], Hash[a4], Hash[a5] {
    pub def hash(t: (a1, a2, a3, a4, a5)): Int = match t {
        case (x1, x2, x3, x4, x5) =>
            5 * 5 * Hash.hash(x1) + 7 * Hash.hash(x2) + 11 * Hash.hash(x3) + 13 * Hash.hash(x4)
            + 17 * Hash.hash(x5)
    }
}

instance Hash[(a1, a2, a3, a4, a5, a6)] with Hash[a1], Hash[a2], Hash[a3], Hash[a4], Hash[a5], Hash[a6] {
    pub def hash(t: (a1, a2, a3, a4, a5, a6)): Int = match t {
        case (x1, x2, x3, x4, x5, x6) =>
            6 * 5 * Hash.hash(x1) + 7 * Hash.hash(x2) + 11 * Hash.hash(x3) + 13 * Hash.hash(x4)
            + 17 * Hash.hash(x5) + 19 * Hash.hash(x6)
    }
}

instance Hash[(a1, a2, a3, a4, a5, a6, a7)] with Hash[a1], Hash[a2], Hash[a3], Hash[a4], Hash[a5], Hash[a6], Hash[a7] {
    pub def hash(t: (a1, a2, a3, a4, a5, a6, a7)): Int = match t {
        case (x1, x2, x3, x4, x5, x6, x7) =>
            7 * 5 * Hash.hash(x1) + 7 * Hash.hash(x2) + 11 * Hash.hash(x3) + 13 * Hash.hash(x4)
            + 17 * Hash.hash(x5) + 19 * Hash.hash(x6) + 23 * Hash.hash(x7)
    }
}

instance Hash[(a1, a2, a3, a4, a5, a6, a7, a8)] with Hash[a1], Hash[a2], Hash[a3], Hash[a4], Hash[a5], Hash[a6], Hash[a7], Hash[a8] {
    pub def hash(t: (a1, a2, a3, a4, a5, a6, a7, a8)): Int = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8) =>
            8 * 5 * Hash.hash(x1) + 7 * Hash.hash(x2) + 11 * Hash.hash(x3) + 13 * Hash.hash(x4)
            + 17 * Hash.hash(x5) + 19 * Hash.hash(x6) + 23 * Hash.hash(x7) + 29 * Hash.hash(x8)
    }
}

instance Hash[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with Hash[a1], Hash[a2], Hash[a3], Hash[a4], Hash[a5], Hash[a6], Hash[a7], Hash[a8], Hash[a9] {
    pub def hash(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9)): Int = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8, x9) =>
            9 * 5 * Hash.hash(x1) + 7 * Hash.hash(x2) + 11 * Hash.hash(x3) + 13 * Hash.hash(x4)
            + 17 * Hash.hash(x5) + 19 * Hash.hash(x6) + 23 * Hash.hash(x7) + 29 * Hash.hash(x8)
            + 31 * Hash.hash(x9)
    }
}

instance Hash[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with Hash[a1], Hash[a2], Hash[a3], Hash[a4], Hash[a5], Hash[a6], Hash[a7], Hash[a8], Hash[a9], Hash[a10] {
    pub def hash(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)): Int = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) =>
            10 * 5 * Hash.hash(x1) + 7 * Hash.hash(x2) + 11 * Hash.hash(x3) + 13 * Hash.hash(x4)
            + 17 * Hash.hash(x5) + 19 * Hash.hash(x6) + 23 * Hash.hash(x7) + 29 * Hash.hash(x8)
            + 31 * Hash.hash(x9) + 37 * Hash.hash(x10)
    }
}
