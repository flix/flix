/*
 *  Copyright 2021 Esben Bjerre
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
/// A type class for types that form a semigroup.
///
pub class SemiGroup[a] {
    ///
    /// An associative binary operation on `a`.
    ///
    pub def combine(x: a, y: a): a

    ///
    /// Returns `x` combined with itself `n` times.
    ///
    pub def combineN(x: a, n: Int32): a =
        if (n <= 1) x
        else SemiGroup.combine(x, SemiGroup.combineN(x, n - 1))

    law associative: forall(x: a, y: a, z: a) with Eq[a] . SemiGroup.combine(SemiGroup.combine(x, y), z) == SemiGroup.combine(x, SemiGroup.combine(y, z))

}

///
/// Alias for `SemiGroup.combine`.
///
pub def ++(x: a, y: a): a with SemiGroup[a] = SemiGroup.combine(x, y)

instance SemiGroup[Unit] {
    pub def combine(_: Unit, _: Unit): Unit = ()
}

instance SemiGroup[Int8] {
    pub def combine(x: Int8, y: Int8): Int8 = x + y
}

instance SemiGroup[Int16] {
    pub def combine(x: Int16, y: Int16): Int16 = x + y
}

instance SemiGroup[Int32] {
    pub def combine(x: Int32, y: Int32): Int32 = x + y
}

instance SemiGroup[Int64] {
    pub def combine(x: Int64, y: Int64): Int64 = x + y
}

instance SemiGroup[BigInt] {
    pub def combine(x: BigInt, y: BigInt): BigInt = x + y
}

instance SemiGroup[Float32] {
    pub def combine(x: Float32, y: Float32): Float32 = x + y
}

instance SemiGroup[Float64] {
    pub def combine(x: Float64, y: Float64): Float64 = x + y
}

instance SemiGroup[BigDecimal] {
    pub def combine(x: BigDecimal, y: BigDecimal): BigDecimal = x + y
}

instance SemiGroup[String] {
    pub def combine(x: String, y: String): String = x + y
}

instance SemiGroup[(a1, a2)] with SemiGroup[a1], SemiGroup[a2] {
    pub def combine(x: (a1, a2), y: (a1, a2)): (a1, a2) = match (x, y) {
        case ((x1, x2), (y1, y2)) => (SemiGroup.combine(x1, y1), SemiGroup.combine(x2, y2))
    }
}

instance SemiGroup[(a1, a2, a3)] with SemiGroup[a1], SemiGroup[a2], SemiGroup[a3] {
    pub def combine(x: (a1, a2, a3), y: (a1, a2, a3)): (a1, a2, a3) = match (x, y) {
        case ((x1, x2, x3), (y1, y2, y3)) => (SemiGroup.combine(x1, y1), SemiGroup.combine(x2, y2), SemiGroup.combine(x3, y3))
    }
}

instance SemiGroup[(a1, a2, a3, a4)] with SemiGroup[a1], SemiGroup[a2], SemiGroup[a3], SemiGroup[a4] {
    pub def combine(x: (a1, a2, a3, a4), y: (a1, a2, a3, a4)): (a1, a2, a3, a4) = match (x, y) {
        case ((x1, x2, x3, x4), (y1, y2, y3, y4)) => (SemiGroup.combine(x1, y1), SemiGroup.combine(x2, y2), SemiGroup.combine(x3, y3), SemiGroup.combine(x4, y4))
    }
}

instance SemiGroup[(a1, a2, a3, a4, a5)] with SemiGroup[a1], SemiGroup[a2], SemiGroup[a3], SemiGroup[a4], SemiGroup[a5] {
    pub def combine(x: (a1, a2, a3, a4, a5), y: (a1, a2, a3, a4, a5)): (a1, a2, a3, a4, a5) = match (x, y) {
        case ((x1, x2, x3, x4, x5), (y1, y2, y3, y4, y5)) => (SemiGroup.combine(x1, y1), SemiGroup.combine(x2, y2), SemiGroup.combine(x3, y3), SemiGroup.combine(x4, y4), SemiGroup.combine(x5, y5))
    }
}

instance SemiGroup[(a1, a2, a3, a4, a5, a6)] with SemiGroup[a1], SemiGroup[a2], SemiGroup[a3], SemiGroup[a4], SemiGroup[a5], SemiGroup[a6] {
    pub def combine(x: (a1, a2, a3, a4, a5, a6), y: (a1, a2, a3, a4, a5, a6)): (a1, a2, a3, a4, a5, a6) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6), (y1, y2, y3, y4, y5, y6)) => (SemiGroup.combine(x1, y1), SemiGroup.combine(x2, y2), SemiGroup.combine(x3, y3), SemiGroup.combine(x4, y4), SemiGroup.combine(x5, y5), SemiGroup.combine(x6, y6))
    }
}

instance SemiGroup[(a1, a2, a3, a4, a5, a6, a7)] with SemiGroup[a1], SemiGroup[a2], SemiGroup[a3], SemiGroup[a4], SemiGroup[a5], SemiGroup[a6], SemiGroup[a7] {
    pub def combine(x: (a1, a2, a3, a4, a5, a6, a7), y: (a1, a2, a3, a4, a5, a6, a7)): (a1, a2, a3, a4, a5, a6, a7) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7), (y1, y2, y3, y4, y5, y6, y7)) => (SemiGroup.combine(x1, y1), SemiGroup.combine(x2, y2), SemiGroup.combine(x3, y3), SemiGroup.combine(x4, y4), SemiGroup.combine(x5, y5), SemiGroup.combine(x6, y6), SemiGroup.combine(x7, y7))
    }
}

instance SemiGroup[(a1, a2, a3, a4, a5, a6, a7, a8)] with SemiGroup[a1], SemiGroup[a2], SemiGroup[a3], SemiGroup[a4], SemiGroup[a5], SemiGroup[a6], SemiGroup[a7], SemiGroup[a8] {
    pub def combine(x: (a1, a2, a3, a4, a5, a6, a7, a8), y: (a1, a2, a3, a4, a5, a6, a7, a8)): (a1, a2, a3, a4, a5, a6, a7, a8) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7, x8), (y1, y2, y3, y4, y5, y6, y7, y8)) => (SemiGroup.combine(x1, y1), SemiGroup.combine(x2, y2), SemiGroup.combine(x3, y3), SemiGroup.combine(x4, y4), SemiGroup.combine(x5, y5), SemiGroup.combine(x6, y6), SemiGroup.combine(x7, y7), SemiGroup.combine(x8, y8))
    }
}

instance SemiGroup[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with SemiGroup[a1], SemiGroup[a2], SemiGroup[a3], SemiGroup[a4], SemiGroup[a5], SemiGroup[a6], SemiGroup[a7], SemiGroup[a8], SemiGroup[a9] {
    pub def combine(x: (a1, a2, a3, a4, a5, a6, a7, a8, a9), y: (a1, a2, a3, a4, a5, a6, a7, a8, a9)): (a1, a2, a3, a4, a5, a6, a7, a8, a9) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7, x8, x9), (y1, y2, y3, y4, y5, y6, y7, y8, y9)) => (SemiGroup.combine(x1, y1), SemiGroup.combine(x2, y2), SemiGroup.combine(x3, y3), SemiGroup.combine(x4, y4), SemiGroup.combine(x5, y5), SemiGroup.combine(x6, y6), SemiGroup.combine(x7, y7), SemiGroup.combine(x8, y8), SemiGroup.combine(x9, y9))
    }
}

instance SemiGroup[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with SemiGroup[a1], SemiGroup[a2], SemiGroup[a3], SemiGroup[a4], SemiGroup[a5], SemiGroup[a6], SemiGroup[a7], SemiGroup[a8], SemiGroup[a9], SemiGroup[a10] {
    pub def combine(x: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10), y: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)): (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10), (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10)) => (SemiGroup.combine(x1, y1), SemiGroup.combine(x2, y2), SemiGroup.combine(x3, y3), SemiGroup.combine(x4, y4), SemiGroup.combine(x5, y5), SemiGroup.combine(x6, y6), SemiGroup.combine(x7, y7), SemiGroup.combine(x8, y8), SemiGroup.combine(x9, y9), SemiGroup.combine(x10, y10))
    }
}

mod SemiGroup {

    @internal
    pub enum Any(Bool)

    instance SemiGroup[Any] {
        pub def combine(x: Any, y: Any): Any = match (x, y) {
            case (Any.Any(a), Any.Any(b)) => Any.Any(a or b)
        }
    }

    instance Monoid[Any] {
        pub def empty(): Any = Any.Any(false)
    }

    instance CommutativeMonoid[Any]


    @internal
    pub enum All(Bool)

    instance SemiGroup[All] {
        pub def combine(x: All, y: All): All = match (x, y) {
            case (All.All(a), All.All(b)) => All.All(a and b)
        }
    }

    instance Monoid[All] {
        pub def empty(): All = All.All(true)
    }

    instance CommutativeMonoid[All]

}
