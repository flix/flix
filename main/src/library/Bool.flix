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
    /// Returns `true` if `x` implies `y` logically holds.
    ///
    pub def ==>(x: Bool, y: Bool): Bool = not x or y

    ///
    /// Returns true if `x` implies `y` and vise versa.
    ///
    pub def <==>(x: Bool, y: Bool): Bool = x == y

    ///
    /// Calls `f` if `cond` is true.
    ///
    pub def runIf(f: Unit -> Unit \ ef, cond: Bool): Unit \ ef = if (cond) { f() }

}
