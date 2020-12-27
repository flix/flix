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

///
/// A type class for meet semi lattices.
///
class MeetLattice[a] {

    ///
    /// Returns the greatest lower bound of `x` and `y`.
    ///
    pub def greatestLowerBound(x: a, y: a): a

    // TODO: Add laws
    // TODO: Add super class.

}

instance MeetLattice[(a1, a2)] with [a1 : MeetLattice, a2 : MeetLattice] {
    def glb(x: (a1, a2), y: (a1, a2)): (a1, a2) = match (x, y) {
        case ((x1, x2), (y1, y2)) => (MeetLattice.glb(x1, y1), MeetLattice.glb(x2, y2))
    }
}

// TODO: Add instances for pairs N = 2 to N = 10.

///
/// A Meet Lattice is pair of functions (⊑, ⊓) where ⊑ is a partial order and ⊓ satisfy two properties:
/// lower-bound and greatest-lower-bound.
///
namespace MeetLattice {

    ///
    /// The lower bound law asserts that the greatest lower bound operator returns an element that
    /// is less than or equal to each of its arguments.
    ///
    law greatestLowerBound1[e](⊑: (e, e) -> Bool, ⊓: (e, e) -> e): Bool =
        ∀(x: e, y: e). ((x ⊓ y) ⊑ x) and ((x ⊓ y) ⊑ y)

    ///
    /// The greatest lower bound law asserts that the greatest lower bound operator returns the
    /// largest element that is smaller than its two arguments.
    ///
    law greatestLowerBound2[e](⊑: (e, e) -> Bool, ⊓: (e, e) -> e): Bool =
        let implies = (x, y) -> (not x) or y;
        ∀(x: e, y: e, z: e). ((z ⊑ x) and (z ⊑ y)) `implies` (z ⊑ (x ⊓ y))

}
