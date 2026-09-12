/*
 * Copyright 2020 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

instance LowerBound[Bool] {
    pub def minValue(): Bool = false
}

instance UpperBound[Bool] {
    pub def maxValue(): Bool = true
}

pub mod Bool {

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
