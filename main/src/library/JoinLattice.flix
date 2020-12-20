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
class JoinLattice[a] {

    ///
    /// Returns the least upper bound of `x` and `y`.
    ///
    pub def lub(x: a, y: a): a

}

instance JoinLattice[(a1, a2)] with [a1 : JoinLattice, a2 : JoinLattice] {
    def lub(x: (a1, a2), y: (a1, a2)): (a1, a2) = match (x, y) {
        case ((x1, x2), (y1, y2)) => (JoinLattice.lub(x1, y1), JoinLattice.lub(x2, y2))
    }
}

// TODO: Add rest for N up to 10.

///
/// A Join Lattice is pair of functions (⊑, ⊔) where ⊑ is a partial order and ⊔ satisfy two properties:
/// upper-bound and least-upper-bound.
///
namespace JoinLattice {

    // TODO: Move this somewhere else.

    ///
    /// The upper bound law asserts that the least upper bound operator returns an element that is
    /// greater than or equal to each of its arguments.
    ///
    law upperBound[e](⊑: (e, e) -> Bool, ⊔: (e, e) -> e): Bool =
        ∀(x: e, y: e). (x ⊑ (x ⊔ y)) and (y ⊑ (x ⊔ y))

    ///
    /// The least upper bound law asserts that the least upper bound operator returns the smallest
    /// element that is larger than its two arguments.
    ///
    law leastUpperBound[e](⊑: (e, e) -> Bool, ⊔: (e, e) -> e): Bool =
        let implies = (x, y) -> (not x) or y;
        ∀(x: e, y: e, z: e). ((x ⊑ z) and (y ⊑ z)) `implies` ((x ⊔ y) ⊑ z)

}
