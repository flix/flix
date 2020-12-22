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

//
// A type class for join semi lattices.
//
class JoinLattice[a] {

    ///
    /// Returns the least upper bound of `x` and `y`.
    ///
    pub def leastUpperBound(x: a, y: a): a

    // TODO: Add laws
    // TODO: Add super class.

}

instance JoinLattice[(a1, a2)] with [a1 : JoinLattice, a2 : JoinLattice] {
    def leastUpperBound(x: (a1, a2), y: (a1, a2)): (a1, a2) = match (x, y) {
        case ((x1, x2), (y1, y2)) => (JoinLattice.leastUpperBound(x1, y1), JoinLattice.leastUpperBound(x2, y2))
    }
}

// TODO: Add instances for pairs N = 2 to N = 10.

namespace JoinLattice {

    ///
    /// The law asserts that the least upper bound operator returns
    //  an element that is greater than or equal to each of its arguments.
    ///
    law leastUpperBound1[e](⊑: (e, e) -> Bool, ⊔: (e, e) -> e): Bool =
        ∀(x: e, y: e). (x ⊑ (x ⊔ y)) and (y ⊑ (x ⊔ y))

    ///
    /// The law asserts that the least upper bound operator returns
    /// the smallest element that is larger than its two arguments.
    ///
    law leastUpperBound2[e](⊑: (e, e) -> Bool, ⊔: (e, e) -> e): Bool =
        let implies = (x, y) -> (not x) or y;
        ∀(x: e, y: e, z: e). ((x ⊑ z) and (y ⊑ z)) `implies` ((x ⊔ y) ⊑ z)

}
