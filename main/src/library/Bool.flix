/*
 * Copyright 2020 Magnus Madsen
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

instance LowerBound[Bool] {
    pub def minValue(): Bool = false
}

instance UpperBound[Bool] {
    pub def maxValue(): Bool = true
}

mod Bool {

    ///
    /// Alias for logical conjunction.
    ///
    pub def ∧(x: Bool, y: Bool): Bool = x and y

    ///
    /// Alias for logical disjunction.
    ///
    pub def ∨(x: Bool, y: Bool): Bool = x or y

    ///
    /// Alias for logical implication.
    ///
    pub def →(x: Bool, y: Bool): Bool = not x ∨ y

    ///
    /// Alias for logical bi-implication.
    ///
    pub def ↔(x: Bool, y: Bool): Bool = (x → y) ∧ (y → x)

    ///
    /// Alias for exclusive or.
    ///
    pub def ⊕(x: Bool, y: Bool): Bool = (x ∨ y) ∧ not (x ∧ y)

    ///
    /// Negation of `x`
    ///
    pub def not(x: Bool): Bool = not x

    ///
    /// Logical and.
    ///
    pub def and(x: Bool, y: Lazy[Bool]): Bool = if(x) force y else x

    ///
    /// Logical or.
    ///
    pub def or(x: Bool, y: Lazy[Bool]): Bool = if(x) x else force y

    ///
    /// Returns `true` if `x` implies `y` logically holds.
    ///
    pub def ==>(x: Bool, y: Bool): Bool = not x or y

    ///
    /// Returns true if `x` implies `y` and vise versa.
    ///
    pub def <==>(x: Bool, y: Bool): Bool = x == y

}
