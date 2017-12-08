/*
 * Copyright 2017 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

pub class PartialEq[a] {

    ///
    /// Returns `true` if `x` is equal to `y`.
    ///
    def eq(x: a, y: a): Bool

    ///
    /// Symmetry Law: If an element `x` is equal to `y` then `y` must be equal to `x`.
    ///
    law symmetric(): Bool = ∀(x: a, y: a). eq(x, y) → eq(y, x)

    ///
    /// Transitive Law: If an element `x` is equal to `y` and `y` is equal to `z` then `x` must be equal to `z`.
    ///
    law transitive(): Bool = ∀(x: a, y: a, z: a). (eq(x, y) ∧ eq(y, z)) → eq(x, z)

}

impl PartialEq[Bool] {
    def eq(x: Bool, y: Bool): Bool = x == y
}

impl PartialEq[Int32] {
    def eq(x: Int32, y: Int32): Bool = x == y
}

impl PartialEq[(a, b)] <= PartialEq[a], PartialEq[b] {
    def eq(x: (a, b), y: (a, b)): Bool = match (x, y) with {
        case ((x1, x2), (y1, y2)) => eq(x1, y1) && eq(x2, y2)
    }
}

