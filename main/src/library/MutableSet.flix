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

///
/// The Set type.
///
/// A set is currently represented internally as a list.
///
enum MutableSet[t] {
    case MutableSet(Ref[Set[t]])
}

namespace MutableSet {

    ///
    /// Returns the size of `xs`.
    ///
    pub def size(xs: MutableSet[a]): Int32 & Impure =
        let MutableSet(s) = xs;
            Set.size(deref s)

    ///
    /// Returns a fresh empty set.
    ///
    pub def empty(): MutableSet[a] & Impure = MutableSet(ref Set.empty())

    ///
    /// Returns the singleton set containing `x`.
    ///
    pub def singleton(x: a): MutableSet[a] & Impure = MutableSet(ref Set.singleton(x))

    ///
    /// Adds `x` to `xs`.
    ///
    pub def insert(x: a, xs: mut MutableSet[a]): Unit & Impure =
        let MutableSet(s) = xs;
            s := Set.insert(x, deref s)

    ///
    /// Removes `x` from `xs`.
    ///
    pub def delete(x: a, xs: mut MutableSet[a]): Unit & Impure =
        let MutableSet(s) = xs;
            s := Set.delete(x, deref s)



/*

    ///
    /// Returns true if and only if `xs` is the empty set, i.e. `Set(Nil)`.
    ///
    pub def isEmpty(xs: Set[a]): Bool =
        let Set(s) = xs;
            List.isEmpty(s)
    ///
    /// Returns true if and only if `x` is a member of `xs`.
    ///
    pub def memberOf(x: a, xs: Set[a]): Bool =
        let Set(s) = xs;
            List.memberOf(x, s)
*/
}

