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
lawless class PartialOrder[a] {

    ///
    /// Returns `true` if `x` is smaller or equal to `y`.
    ///
    pub def partialCompare(x: a, y: a): Bool
    // TODO This should not return a Boolean, but instead perform a three-way comparison.

}

///
/// A Partial Order is a function ⊑ which satisfies three properties: reflexivity, anti-symmetry, and transitivity.
///
namespace PartialOrder {

    // TODO: Move this somewhere else.

    ///
    /// The monotonicity law asserts that a function `f` is order-preserving w.r.t.
    /// the partial order `dom` on its domain and the partial order `codom` on its co-domain.
    ///
    /// NB: This particular law is for unary functions.
    ///
//    law monotone1[a, b](f: a -> b, dom: (a, a) -> Bool, codom: (b, b) -> Bool): Bool =
//        let implies = (x, y) -> (not x) or y;
//        ∀(x: a, y: a). (x `dom` y) `implies` (f(x) `codom` f(y))

    ///
    /// The monotonicity law asserts that a function `f` is order-preserving w.r.t.
    /// the partial order `dom` on its domain and the partial order `codom` on its co-domain.
    ///
    /// NB: This particular law is for binary functions.
    ///
//    law monotone2[a, b, c](f: (a, b) -> c, dom1: (a, a) -> Bool, dom2: (b, b) -> Bool, codom: (c, c) -> Bool): Bool =
//        let implies = (x, y) -> (not x) or y;
//        ∀(x1: a, x2: a, y1: b, y2: b). ((x1 `dom1` x2) and (y1 `dom2` y2)) `implies` (f(x1, y1) `codom` f(x2, y2))

    ///
    /// The safety law asserts that the abstract function `fa` is an over-approximation of the concrete
    /// function `fc` according to the partial order `leq` and abstraction function `alpha`.
    ///
    /// NB: This particular law is for unary functions.
    ///
//    law safe1[a1, a2, c1, c2](fa: a1 -> a2, fc: c1 -> c2, alpha1: c1 -> a1, alpha2: c2 -> a2, leq: (a2, a2) -> Bool): Bool =
//        ∀(x: c1). alpha2(fc(x)) `leq` fa(alpha1(x))

    ///
    /// The safety law asserts that the abstract function `fa` is an over-approximation of the concrete
    /// function `fc` according to the partial order `leq` and abstraction functions `alpha1` and `alpha2`.
    ///
    /// NB: This particular law is for binary functions.
    ///
//    law safe2[a1, a2, c1, c2](fa: (a1, a1) -> a2, fc: (c1, c1) -> c2, alpha1: c1 -> a1, alpha2: c2 -> a2, leq: (a2, a2) -> Bool): Bool =
//        ∀(x: c1, y: c1). alpha2(fc(x, y)) `leq` fa(alpha1(x), alpha1(y))

}

instance PartialOrder[Int8] {
    pub def partialCompare(x: Int8, y: Int8): Bool = $INT8_LE$(x, y)
}

instance PartialOrder[Int16] {
    pub def partialCompare(x: Int16, y: Int16): Bool = $INT16_LE$(x, y)
}

instance PartialOrder[Int32] {
    pub def partialCompare(x: Int32, y: Int32): Bool = $INT32_LE$(x, y)
}

instance PartialOrder[Int64] {
    pub def partialCompare(x: Int64, y: Int64): Bool = $INT64_LE$(x, y)
}

instance PartialOrder[BigInt] {
    pub def partialCompare(x: BigInt, y: BigInt): Bool = $BIGINT_LE$(x, y)
}

instance PartialOrder[(a1, a2)] with PartialOrder[a1], PartialOrder[a2] {
    pub def partialCompare(x: (a1, a2),
                           y: (a1, a2)): Bool = match (x, y) {
        case ((x1, x2), (y1, y2)) =>
            PartialOrder.partialCompare(x1, y1) and
            PartialOrder.partialCompare(x2, y2)
    }
}

instance PartialOrder[(a1, a2, a3)] with PartialOrder[a1], PartialOrder[a2],
                                         PartialOrder[a3] {
    pub def partialCompare(x: (a1, a2, a3),
                           y: (a1, a2, a3)): Bool = match (x, y) {
        case ((x1, x2, x3), (y1, y2, y3)) =>
            PartialOrder.partialCompare(x1, y1) and
            PartialOrder.partialCompare(x2, y2) and
            PartialOrder.partialCompare(x3, y3)
    }
}

instance PartialOrder[(a1, a2, a3, a4)] with PartialOrder[a1], PartialOrder[a2],
                                             PartialOrder[a3], PartialOrder[a4] {
    pub def partialCompare(x: (a1, a2, a3, a4),
                           y: (a1, a2, a3, a4)): Bool = match (x, y) {
        case ((x1, x2, x3, x4), (y1, y2, y3, y4)) =>
            PartialOrder.partialCompare(x1, y1) and
            PartialOrder.partialCompare(x2, y2) and
            PartialOrder.partialCompare(x3, y3) and
            PartialOrder.partialCompare(x4, y4)
    }
}

instance PartialOrder[(a1, a2, a3, a4, a5)] with PartialOrder[a1], PartialOrder[a2],
                                                 PartialOrder[a3], PartialOrder[a4],
                                                 PartialOrder[a5] {
    pub def partialCompare(x: (a1, a2, a3, a4, a5),
                           y: (a1, a2, a3, a4, a5)): Bool = match (x, y) {
        case ((x1, x2, x3, x4, x5), (y1, y2, y3, y4, y5)) =>
            PartialOrder.partialCompare(x1, y1) and
            PartialOrder.partialCompare(x2, y2) and
            PartialOrder.partialCompare(x3, y3) and
            PartialOrder.partialCompare(x4, y4) and
            PartialOrder.partialCompare(x5, y5)
    }
}

instance PartialOrder[(a1, a2, a3, a4, a5, a6)] with PartialOrder[a1], PartialOrder[a2],
                                                     PartialOrder[a3], PartialOrder[a4],
                                                     PartialOrder[a5], PartialOrder[a6] {
    pub def partialCompare(x: (a1, a2, a3, a4, a5, a6),
                           y: (a1, a2, a3, a4, a5, a6)): Bool = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6), (y1, y2, y3, y4, y5, y6)) =>
            PartialOrder.partialCompare(x1, y1) and
            PartialOrder.partialCompare(x2, y2) and
            PartialOrder.partialCompare(x3, y3) and
            PartialOrder.partialCompare(x4, y4) and
            PartialOrder.partialCompare(x5, y5) and
            PartialOrder.partialCompare(x6, y6)
    }
}

instance PartialOrder[(a1, a2, a3, a4, a5, a6, a7)] with PartialOrder[a1], PartialOrder[a2],
                                                         PartialOrder[a3], PartialOrder[a4],
                                                         PartialOrder[a5], PartialOrder[a6],
                                                         PartialOrder[a7] {
    pub def partialCompare(x: (a1, a2, a3, a4, a5, a6, a7),
                           y: (a1, a2, a3, a4, a5, a6, a7)): Bool = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7), (y1, y2, y3, y4, y5, y6, y7)) =>
            PartialOrder.partialCompare(x1, y1) and
            PartialOrder.partialCompare(x2, y2) and
            PartialOrder.partialCompare(x3, y3) and
            PartialOrder.partialCompare(x4, y4) and
            PartialOrder.partialCompare(x5, y5) and
            PartialOrder.partialCompare(x6, y6) and
            PartialOrder.partialCompare(x7, y7)
    }
}

instance PartialOrder[(a1, a2, a3, a4, a5, a6, a7, a8)] with PartialOrder[a1], PartialOrder[a2],
                                                             PartialOrder[a3], PartialOrder[a4],
                                                             PartialOrder[a5], PartialOrder[a6],
                                                             PartialOrder[a7], PartialOrder[a8] {
    pub def partialCompare(x: (a1, a2, a3, a4, a5, a6, a7, a8),
                           y: (a1, a2, a3, a4, a5, a6, a7, a8)): Bool = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7, x8), (y1, y2, y3, y4, y5, y6, y7, y8)) =>
            PartialOrder.partialCompare(x1, y1) and
            PartialOrder.partialCompare(x2, y2) and
            PartialOrder.partialCompare(x3, y3) and
            PartialOrder.partialCompare(x4, y4) and
            PartialOrder.partialCompare(x5, y5) and
            PartialOrder.partialCompare(x6, y6) and
            PartialOrder.partialCompare(x7, y7) and
            PartialOrder.partialCompare(x8, y8)
    }
}

instance PartialOrder[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with PartialOrder[a1], PartialOrder[a2],
                                                                 PartialOrder[a3], PartialOrder[a4],
                                                                 PartialOrder[a5], PartialOrder[a6],
                                                                 PartialOrder[a7], PartialOrder[a8],
                                                                 PartialOrder[a9] {
    pub def partialCompare(x: (a1, a2, a3, a4, a5, a6, a7, a8, a9),
                           y: (a1, a2, a3, a4, a5, a6, a7, a8, a9)): Bool = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7, x8, x9), (y1, y2, y3, y4, y5, y6, y7, y8, y9)) =>
            PartialOrder.partialCompare(x1, y1) and
            PartialOrder.partialCompare(x2, y2) and
            PartialOrder.partialCompare(x3, y3) and
            PartialOrder.partialCompare(x4, y4) and
            PartialOrder.partialCompare(x5, y5) and
            PartialOrder.partialCompare(x6, y6) and
            PartialOrder.partialCompare(x7, y7) and
            PartialOrder.partialCompare(x8, y8) and
            PartialOrder.partialCompare(x9, y9)
    }
}

instance PartialOrder[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with PartialOrder[a1], PartialOrder[a2],
                                                                      PartialOrder[a3], PartialOrder[a4],
                                                                      PartialOrder[a5], PartialOrder[a6],
                                                                      PartialOrder[a7], PartialOrder[a8],
                                                                      PartialOrder[a9], PartialOrder[a10] {
    pub def partialCompare(x: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10),
                           y: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)): Bool = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10), (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10)) =>
            PartialOrder.partialCompare(x1, y1) and
            PartialOrder.partialCompare(x2, y2) and
            PartialOrder.partialCompare(x3, y3) and
            PartialOrder.partialCompare(x4, y4) and
            PartialOrder.partialCompare(x5, y5) and
            PartialOrder.partialCompare(x6, y6) and
            PartialOrder.partialCompare(x7, y7) and
            PartialOrder.partialCompare(x8, y8) and
            PartialOrder.partialCompare(x9, y9) and
            PartialOrder.partialCompare(x10, y10)
    }
}
