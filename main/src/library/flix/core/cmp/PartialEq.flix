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

/**
 * Partial Equality type class.
 */
pub class PartialEq[a] {

    ///
    /// Returns `true` if `x` is equal to `y`.
    ///
    def eqTODO(x: a, y: a): Bool

    ///
    /// Returns `true` if `x` is not equal to `y`.
    ///
    // TODO: Allow implementations in type classes?
    //def neq(x: a, y: a): Bool = not eq(x, y)

    ///
    /// Symmetry Law: If an element `x` is equal to `y` then `y` must be equal to `x`.
    ///
    law symmetric(): Bool = ∀(x: a, y: a). eq(x, y) → eq(y, x)

    ///
    /// Transitive Law: If an element `x` is equal to `y` and `y` is equal to `z` then `x` must be equal to `z`.
    ///
    law transitive(): Bool = ∀(x: a, y: a, z: a). (eq(x, y) ∧ eq(y, z)) → eq(x, z)

}

// TODO: Add remaining primitive types.
// TODO: Add remaining compound types.

impl PartialEq[Bool] {
    def eq(x: Bool, y: Bool): Bool = x == y
}

impl PartialEq[Int32] {
    def eq(x: Int32, y: Int32): Bool = x == y
}

impl PartialEq[(a, b)] <= PartialEq[a], PartialEq[b] {
    def eq(x: (a, b), y: (a, b)): Bool = match (x, y) {
        case ((x1, x2), (y1, y2)) => eq(x1, y1) and eq(x2, y2)
    }
}

impl PartialEq[Option[a]] <= PartialEq[a] {
    def eq(x: Option[a], y: Option[a]): Bool = match (x, y) {
        case (None, None)           => true
        case (Some(v1), Some(v2))   => eq(v1, v2)
        case _                      => false
    }
}

impl PartialEq[List[a]] <= PartialEq[a] {
    def eq(x: List[a], y: List[a]): Bool = match (x, y) {
        case (Nil, Nil)             => true
        case (v1 :: xs, v2 :: ys)   => eq(v1, v2) and eq(xs, ys)
        case _                      => false
    }
}

impl PartialEq[Result[a, b]] <= PartialEq[a], PartialEq[b] {
    def eq(x: Result[a, b], y: Result[a, b]): Bool = match (x, y) {
        case (Ok(v1), Ok(v2))       => eq(v1, v2)
        case (Err(v1), Err(v2))     => eq(v1, v2)
        case _                      => false
    }
}

