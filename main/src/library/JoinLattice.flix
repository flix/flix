/*
 *  Copyright 2021 Jonathan Lindegaard Starup
 *  Copyright 2020 Magnus Madsen
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

use Bool.{==>}

///
/// A trait for join semi lattices.
///
pub lawful trait JoinLattice[a] with PartialOrder[a] {

    ///
    /// Returns the least upper bound of `x` and `y`.
    ///
    pub def leastUpperBound(x: a, y: a): a

    ///
    /// The law asserts that the least upper bound operator returns
    /// an element that is greater than or equal to each of its arguments.
    ///
    law leastUpperBound1: forall(x: a, y: a)
        PartialOrder.lessEqual(x, JoinLattice.leastUpperBound(x, y)) and
        PartialOrder.lessEqual(y, JoinLattice.leastUpperBound(x, y))

    ///
    /// The law asserts that the least upper bound operator returns
    /// the smallest element that is larger than its two arguments.
    ///
    law leastUpperBound2: forall(x: a, y: a, z: a)
        (PartialOrder.lessEqual(x, z) and PartialOrder.lessEqual(y, z)) ==>
        PartialOrder.lessEqual(JoinLattice.leastUpperBound(x, y), z)

}

instance JoinLattice[Int8] {
    pub def leastUpperBound(x: Int8, y: Int8): Int8 = Order.max(x, y)
}

instance JoinLattice[Int16] {
    pub def leastUpperBound(x: Int16, y: Int16): Int16 = Order.max(x, y)
}

instance JoinLattice[Int32] {
    pub def leastUpperBound(x: Int32, y: Int32): Int32 = Order.max(x, y)
}

instance JoinLattice[Int64] {
    pub def leastUpperBound(x: Int64, y: Int64): Int64 = Order.max(x, y)
}

instance JoinLattice[BigInt] {
    pub def leastUpperBound(x: BigInt, y: BigInt): BigInt = Order.max(x, y)
}

instance JoinLattice[(a1, a2)] with JoinLattice[a1], JoinLattice[a2] {
    pub def leastUpperBound(x: (a1, a2),
                            y: (a1, a2)): (a1, a2) = match (x, y) {
        case ((x1, x2), (y1, y2)) => (
            JoinLattice.leastUpperBound(x1, y1),
            JoinLattice.leastUpperBound(x2, y2)
        )
    }
}

instance JoinLattice[(a1, a2, a3)] with JoinLattice[a1], JoinLattice[a2],
                                        JoinLattice[a3] {
    pub def leastUpperBound(x: (a1, a2, a3),
                            y: (a1, a2, a3)): (a1, a2, a3) = match (x, y) {
        case ((x1, x2, x3), (y1, y2, y3)) => (
            JoinLattice.leastUpperBound(x1, y1),
            JoinLattice.leastUpperBound(x2, y2),
            JoinLattice.leastUpperBound(x3, y3)
        )
    }
}

instance JoinLattice[(a1, a2, a3, a4)] with JoinLattice[a1], JoinLattice[a2],
                                            JoinLattice[a3], JoinLattice[a4] {
    pub def leastUpperBound(x: (a1, a2, a3, a4),
                            y: (a1, a2, a3, a4)): (a1, a2, a3, a4) = match (x, y) {
        case ((x1, x2, x3, x4), (y1, y2, y3, y4)) => (
            JoinLattice.leastUpperBound(x1, y1),
            JoinLattice.leastUpperBound(x2, y2),
            JoinLattice.leastUpperBound(x3, y3),
            JoinLattice.leastUpperBound(x4, y4)
        )
    }
}

instance JoinLattice[(a1, a2, a3, a4, a5)] with JoinLattice[a1], JoinLattice[a2],
                                                JoinLattice[a3], JoinLattice[a4],
                                                JoinLattice[a5] {
    pub def leastUpperBound(x: (a1, a2, a3, a4, a5),
                            y: (a1, a2, a3, a4, a5)): (a1, a2, a3, a4, a5) = match (x, y) {
        case ((x1, x2, x3, x4, x5), (y1, y2, y3, y4, y5)) => (
            JoinLattice.leastUpperBound(x1, y1),
            JoinLattice.leastUpperBound(x2, y2),
            JoinLattice.leastUpperBound(x3, y3),
            JoinLattice.leastUpperBound(x4, y4),
            JoinLattice.leastUpperBound(x5, y5)
        )
    }
}

instance JoinLattice[(a1, a2, a3, a4, a5, a6)] with JoinLattice[a1], JoinLattice[a2],
                                                    JoinLattice[a3], JoinLattice[a4],
                                                    JoinLattice[a5], JoinLattice[a6] {
    pub def leastUpperBound(x: (a1, a2, a3, a4, a5, a6),
                            y: (a1, a2, a3, a4, a5, a6)): (a1, a2, a3, a4, a5, a6) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6), (y1, y2, y3, y4, y5, y6)) => (
            JoinLattice.leastUpperBound(x1, y1),
            JoinLattice.leastUpperBound(x2, y2),
            JoinLattice.leastUpperBound(x3, y3),
            JoinLattice.leastUpperBound(x4, y4),
            JoinLattice.leastUpperBound(x5, y5),
            JoinLattice.leastUpperBound(x6, y6)
        )
    }
}

instance JoinLattice[(a1, a2, a3, a4, a5, a6, a7)] with JoinLattice[a1], JoinLattice[a2],
                                                        JoinLattice[a3], JoinLattice[a4],
                                                        JoinLattice[a5], JoinLattice[a6],
                                                        JoinLattice[a7] {
    pub def leastUpperBound(x: (a1, a2, a3, a4, a5, a6, a7),
                            y: (a1, a2, a3, a4, a5, a6, a7)): (a1, a2, a3, a4, a5, a6, a7) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7), (y1, y2, y3, y4, y5, y6, y7)) => (
            JoinLattice.leastUpperBound(x1, y1),
            JoinLattice.leastUpperBound(x2, y2),
            JoinLattice.leastUpperBound(x3, y3),
            JoinLattice.leastUpperBound(x4, y4),
            JoinLattice.leastUpperBound(x5, y5),
            JoinLattice.leastUpperBound(x6, y6),
            JoinLattice.leastUpperBound(x7, y7)
        )
    }
}

instance JoinLattice[(a1, a2, a3, a4, a5, a6, a7, a8)] with JoinLattice[a1], JoinLattice[a2],
                                                            JoinLattice[a3], JoinLattice[a4],
                                                            JoinLattice[a5], JoinLattice[a6],
                                                            JoinLattice[a7], JoinLattice[a8] {
    pub def leastUpperBound(x: (a1, a2, a3, a4, a5, a6, a7, a8),
                            y: (a1, a2, a3, a4, a5, a6, a7, a8)): (a1, a2, a3, a4, a5, a6, a7, a8) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7, x8), (y1, y2, y3, y4, y5, y6, y7, y8)) => (
            JoinLattice.leastUpperBound(x1, y1),
            JoinLattice.leastUpperBound(x2, y2),
            JoinLattice.leastUpperBound(x3, y3),
            JoinLattice.leastUpperBound(x4, y4),
            JoinLattice.leastUpperBound(x5, y5),
            JoinLattice.leastUpperBound(x6, y6),
            JoinLattice.leastUpperBound(x7, y7),
            JoinLattice.leastUpperBound(x8, y8)
        )
    }
}

instance JoinLattice[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with JoinLattice[a1], JoinLattice[a2],
                                                                JoinLattice[a3], JoinLattice[a4],
                                                                JoinLattice[a5], JoinLattice[a6],
                                                                JoinLattice[a7], JoinLattice[a8],
                                                                JoinLattice[a9] {
    pub def leastUpperBound(x: (a1, a2, a3, a4, a5, a6, a7, a8, a9),
                            y: (a1, a2, a3, a4, a5, a6, a7, a8, a9)): (a1, a2, a3, a4, a5, a6, a7, a8, a9) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7, x8, x9), (y1, y2, y3, y4, y5, y6, y7, y8, y9)) => (
            JoinLattice.leastUpperBound(x1, y1),
            JoinLattice.leastUpperBound(x2, y2),
            JoinLattice.leastUpperBound(x3, y3),
            JoinLattice.leastUpperBound(x4, y4),
            JoinLattice.leastUpperBound(x5, y5),
            JoinLattice.leastUpperBound(x6, y6),
            JoinLattice.leastUpperBound(x7, y7),
            JoinLattice.leastUpperBound(x8, y8),
            JoinLattice.leastUpperBound(x9, y9)
        )
    }
}

instance JoinLattice[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with JoinLattice[a1], JoinLattice[a2],
                                                                     JoinLattice[a3], JoinLattice[a4],
                                                                     JoinLattice[a5], JoinLattice[a6],
                                                                     JoinLattice[a7], JoinLattice[a8],
                                                                     JoinLattice[a9], JoinLattice[a10] {
    pub def leastUpperBound(x: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10),
                            y: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)): (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10), (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10)) => (
            JoinLattice.leastUpperBound(x1, y1),
            JoinLattice.leastUpperBound(x2, y2),
            JoinLattice.leastUpperBound(x3, y3),
            JoinLattice.leastUpperBound(x4, y4),
            JoinLattice.leastUpperBound(x5, y5),
            JoinLattice.leastUpperBound(x6, y6),
            JoinLattice.leastUpperBound(x7, y7),
            JoinLattice.leastUpperBound(x8, y8),
            JoinLattice.leastUpperBound(x9, y9),
            JoinLattice.leastUpperBound(x10, y10)
        )
    }
}
