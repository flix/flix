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
 * Partial Order type class.
 */
pub class PartialOrd[a] <= PartialEq[a] {

    ///
    /// Optionally returns the result of comparing the value `x` to the value `y`.
    ///
    def partialCmp(x: a, y: a): Option[Ordering]

    ///
    /// Anti-Symmetry Law: If `x` < `y` then !(`x` > `y`) and if `x` > `y` then !(`x` < `y`).
    ///
    law antiSymmetry(): Bool = ∀(x: a, y: a). (lt(x, y) → not gt(x, y)) ∧ (gt(x, y) → not lt(x, y))

    ///
    /// Transitivity Law: If `x` < `y` and `y` < `z` then `x` < `z`.
    ///
    law transitivity1(): Bool = ∀(x: a, y: a, z: a). (lt(x, y) ∧ lt(y, z)) → lt(x, z)

    ///
    /// Transitivity Law: If `x` <= `y` and `y` <= `z` then `x` <= `z`.
    ///
    law transitivity2(): Bool = ∀(x: a, y: a, z: a). (le(x, y) ∧ le(y, z)) → le(x, z)

    ///
    /// Transitivity Law: If `x` > `y` and `y` > `z` then `x` > `z`.
    ///
    law transitivity3(): Bool = ∀(x: a, y: a, z: a). (gt(x, y) ∧ gt(y, z)) → gt(x, z)

    ///
    /// Transitivity Law: If `x` >= `y` and `y` >= `z` then `x` >= `z`.
    ///
    law transitivity4(): Bool = ∀(x: a, y: a, z: a). (ge(x, y) ∧ ge(y, z)) → ge(x, z)

}

impl PartialOrd[Int32] {
    def partialCmp(x: Int32, y: Int32): Option[Ordering] = ???
}

impl PartialOrd[(a, b)] <= PartialOrd[a], PartialOrd[b] {
    def partialCmp(x: (a, b), y: (a, b)): Option[Ordering] = ???
}

impl PartialOrd[Option[a]] <= PartialOrd[a] {
    def partialCmp(x: Option[a], y: Option[a]): Option[Ordering] = ???
}

impl PartialOrd[List[a]] <= PartialOrd[a] {
    def partialCmp(x: List[a], y: List[a]): Option[Ordering] = ???
}

impl PartialOrd[Result[a, b]] <= PartialOrd[a], PartialOrd[b] {
    def partialCmp(x: Result[a, b], y: Result[a, b]): Option[Ordering] = ???
}

