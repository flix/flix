/*
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
/// A trait for meet semi lattices.
/// A Meet Lattice is pair of functions (⊑, ⊓) where ⊑ is a partial order and ⊓ satisfy two properties:
/// lower-bound and greatest-lower-bound.
///
pub lawful trait MeetLattice[a] with PartialOrder[a] {

    ///
    /// Returns the greatest lower bound of `x` and `y`.
    ///
    pub def greatestLowerBound(x: a, y: a): a

    ///
    /// The lower bound law asserts that the greatest lower bound operator returns an element that
    /// is less than or equal to each of its arguments.
    ///
    law greatestLowerBound1: forall(x: a, y: a)
        PartialOrder.lessEqual(MeetLattice.greatestLowerBound(x, y), x) and
        PartialOrder.lessEqual(MeetLattice.greatestLowerBound(x, y), y)

    ///
    /// The greatest lower bound law asserts that the greatest lower bound operator returns the
    /// largest element that is smaller than its two arguments.
    ///
    law greatestLowerBound2: forall(x: a, y: a, z: a)
        (PartialOrder.lessEqual(z, x) and PartialOrder.lessEqual(z, y)) ==>
        PartialOrder.lessEqual(z, MeetLattice.greatestLowerBound(x, y))

}

instance MeetLattice[Int8] {
    pub def greatestLowerBound(x: Int8, y: Int8): Int8 = Order.min(x, y)
}

instance MeetLattice[Int16] {
    pub def greatestLowerBound(x: Int16, y: Int16): Int16 = Order.min(x, y)
}

instance MeetLattice[Int32] {
    pub def greatestLowerBound(x: Int32, y: Int32): Int32 = Order.min(x, y)
}

instance MeetLattice[Int64] {
    pub def greatestLowerBound(x: Int64, y: Int64): Int64 = Order.min(x, y)
}

instance MeetLattice[BigInt] {
    pub def greatestLowerBound(x: BigInt, y: BigInt): BigInt = Order.min(x, y)
}

instance MeetLattice[(a1, a2)] with MeetLattice[a1], MeetLattice[a2] {
    pub def greatestLowerBound(x: (a1, a2),
                            y: (a1, a2)): (a1, a2) = match (x, y) {
        case ((x1, x2), (y1, y2)) => (
            MeetLattice.greatestLowerBound(x1, y1),
            MeetLattice.greatestLowerBound(x2, y2)
        )
    }
}

instance MeetLattice[(a1, a2, a3)] with MeetLattice[a1], MeetLattice[a2],
                                        MeetLattice[a3] {
    pub def greatestLowerBound(x: (a1, a2, a3),
                            y: (a1, a2, a3)): (a1, a2, a3) = match (x, y) {
        case ((x1, x2, x3), (y1, y2, y3)) => (
            MeetLattice.greatestLowerBound(x1, y1),
            MeetLattice.greatestLowerBound(x2, y2),
            MeetLattice.greatestLowerBound(x3, y3)
        )
    }
}

instance MeetLattice[(a1, a2, a3, a4)] with MeetLattice[a1], MeetLattice[a2],
                                            MeetLattice[a3], MeetLattice[a4] {
    pub def greatestLowerBound(x: (a1, a2, a3, a4),
                            y: (a1, a2, a3, a4)): (a1, a2, a3, a4) = match (x, y) {
        case ((x1, x2, x3, x4), (y1, y2, y3, y4)) => (
            MeetLattice.greatestLowerBound(x1, y1),
            MeetLattice.greatestLowerBound(x2, y2),
            MeetLattice.greatestLowerBound(x3, y3),
            MeetLattice.greatestLowerBound(x4, y4)
        )
    }
}

instance MeetLattice[(a1, a2, a3, a4, a5)] with MeetLattice[a1], MeetLattice[a2],
                                                MeetLattice[a3], MeetLattice[a4],
                                                MeetLattice[a5] {
    pub def greatestLowerBound(x: (a1, a2, a3, a4, a5),
                            y: (a1, a2, a3, a4, a5)): (a1, a2, a3, a4, a5) = match (x, y) {
        case ((x1, x2, x3, x4, x5), (y1, y2, y3, y4, y5)) => (
            MeetLattice.greatestLowerBound(x1, y1),
            MeetLattice.greatestLowerBound(x2, y2),
            MeetLattice.greatestLowerBound(x3, y3),
            MeetLattice.greatestLowerBound(x4, y4),
            MeetLattice.greatestLowerBound(x5, y5)
        )
    }
}

instance MeetLattice[(a1, a2, a3, a4, a5, a6)] with MeetLattice[a1], MeetLattice[a2],
                                                    MeetLattice[a3], MeetLattice[a4],
                                                    MeetLattice[a5], MeetLattice[a6] {
    pub def greatestLowerBound(x: (a1, a2, a3, a4, a5, a6),
                            y: (a1, a2, a3, a4, a5, a6)): (a1, a2, a3, a4, a5, a6) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6), (y1, y2, y3, y4, y5, y6)) => (
            MeetLattice.greatestLowerBound(x1, y1),
            MeetLattice.greatestLowerBound(x2, y2),
            MeetLattice.greatestLowerBound(x3, y3),
            MeetLattice.greatestLowerBound(x4, y4),
            MeetLattice.greatestLowerBound(x5, y5),
            MeetLattice.greatestLowerBound(x6, y6)
        )
    }
}

instance MeetLattice[(a1, a2, a3, a4, a5, a6, a7)] with MeetLattice[a1], MeetLattice[a2],
                                                        MeetLattice[a3], MeetLattice[a4],
                                                        MeetLattice[a5], MeetLattice[a6],
                                                        MeetLattice[a7] {
    pub def greatestLowerBound(x: (a1, a2, a3, a4, a5, a6, a7),
                            y: (a1, a2, a3, a4, a5, a6, a7)): (a1, a2, a3, a4, a5, a6, a7) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7), (y1, y2, y3, y4, y5, y6, y7)) => (
            MeetLattice.greatestLowerBound(x1, y1),
            MeetLattice.greatestLowerBound(x2, y2),
            MeetLattice.greatestLowerBound(x3, y3),
            MeetLattice.greatestLowerBound(x4, y4),
            MeetLattice.greatestLowerBound(x5, y5),
            MeetLattice.greatestLowerBound(x6, y6),
            MeetLattice.greatestLowerBound(x7, y7)
        )
    }
}

instance MeetLattice[(a1, a2, a3, a4, a5, a6, a7, a8)] with MeetLattice[a1], MeetLattice[a2],
                                                            MeetLattice[a3], MeetLattice[a4],
                                                            MeetLattice[a5], MeetLattice[a6],
                                                            MeetLattice[a7], MeetLattice[a8] {
    pub def greatestLowerBound(x: (a1, a2, a3, a4, a5, a6, a7, a8),
                            y: (a1, a2, a3, a4, a5, a6, a7, a8)): (a1, a2, a3, a4, a5, a6, a7, a8) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7, x8), (y1, y2, y3, y4, y5, y6, y7, y8)) => (
            MeetLattice.greatestLowerBound(x1, y1),
            MeetLattice.greatestLowerBound(x2, y2),
            MeetLattice.greatestLowerBound(x3, y3),
            MeetLattice.greatestLowerBound(x4, y4),
            MeetLattice.greatestLowerBound(x5, y5),
            MeetLattice.greatestLowerBound(x6, y6),
            MeetLattice.greatestLowerBound(x7, y7),
            MeetLattice.greatestLowerBound(x8, y8)
        )
    }
}

instance MeetLattice[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with MeetLattice[a1], MeetLattice[a2],
                                                                MeetLattice[a3], MeetLattice[a4],
                                                                MeetLattice[a5], MeetLattice[a6],
                                                                MeetLattice[a7], MeetLattice[a8],
                                                                MeetLattice[a9] {
    pub def greatestLowerBound(x: (a1, a2, a3, a4, a5, a6, a7, a8, a9),
                            y: (a1, a2, a3, a4, a5, a6, a7, a8, a9)): (a1, a2, a3, a4, a5, a6, a7, a8, a9) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7, x8, x9), (y1, y2, y3, y4, y5, y6, y7, y8, y9)) => (
            MeetLattice.greatestLowerBound(x1, y1),
            MeetLattice.greatestLowerBound(x2, y2),
            MeetLattice.greatestLowerBound(x3, y3),
            MeetLattice.greatestLowerBound(x4, y4),
            MeetLattice.greatestLowerBound(x5, y5),
            MeetLattice.greatestLowerBound(x6, y6),
            MeetLattice.greatestLowerBound(x7, y7),
            MeetLattice.greatestLowerBound(x8, y8),
            MeetLattice.greatestLowerBound(x9, y9)
        )
    }
}

instance MeetLattice[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with MeetLattice[a1], MeetLattice[a2],
                                                                     MeetLattice[a3], MeetLattice[a4],
                                                                     MeetLattice[a5], MeetLattice[a6],
                                                                     MeetLattice[a7], MeetLattice[a8],
                                                                     MeetLattice[a9], MeetLattice[a10] {
    pub def greatestLowerBound(x: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10),
                            y: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)): (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10), (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10)) => (
            MeetLattice.greatestLowerBound(x1, y1),
            MeetLattice.greatestLowerBound(x2, y2),
            MeetLattice.greatestLowerBound(x3, y3),
            MeetLattice.greatestLowerBound(x4, y4),
            MeetLattice.greatestLowerBound(x5, y5),
            MeetLattice.greatestLowerBound(x6, y6),
            MeetLattice.greatestLowerBound(x7, y7),
            MeetLattice.greatestLowerBound(x8, y8),
            MeetLattice.greatestLowerBound(x9, y9),
            MeetLattice.greatestLowerBound(x10, y10)
        )
    }
}
