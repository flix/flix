/*
 *  Copyright 2023 Jakob Schneider Villumsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

struct Ref[a, r] {
    mut v: a
}

pub mod Ref {

    ///
    /// Returns a new reference to `x` in the region `rc`.
    ///
    pub def fresh(rc: Region[r], x: a): Ref[a, r] \ r =
        new Ref @ rc {v = x}

    ///
    /// Returns the element referenced by `rf`.
    ///
    pub def get(rf: Ref[a, r]): a \ r =
        rf->v

    ///
    /// Updates `rf` to reference `x`.
    ///
    pub def put(x: a, rf: Ref[a, r]): Unit \ r =
        rf->v = x

    ///
    /// Updates `rf` to reference `f` applied to the value referenced by `rf`.
    ///
    /// I.e. if `rf` references `x` then `rf` is updated to reference `f(x)`.
    ///
    pub def transform(f: a -> a \ ef, rf: Ref[a, r]): Unit \ { r, ef } =
        put(f(rf->v), rf)

}
