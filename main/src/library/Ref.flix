/*
 *  Copyright 2023 Jakob Schneider Villumsen
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

struct Ref[a, r] {
    v: a
}

mod Ref {

    ///
    /// Returns a new reference to `x` in the region `rc`.
    ///
    pub def fresh(rc: Region[r], x: a): Ref[a, r] \ r =
        new Ref @ rc { v = x }

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
