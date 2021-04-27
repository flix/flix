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
    /// Returns `true` if `x` is smaller equal to `y`.
    ///
    pub def partialCompare(x: a, y: a): Bool
    // TODO This should not return a Boolean, but instead perform a three-way comparison.

}

// TODO: Add instances for tuples.

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
