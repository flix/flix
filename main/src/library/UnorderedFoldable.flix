/*
 *  Copyright 2022 Jakob Schneider Villumsen
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
/// A type class for unordered data structures that can be folded.
///
pub class UnorderedFoldable[t : Type -> Type] with ForEach[t] {

    ///
    /// Unordered fold of `t`.
    /// Applies `f` to all elements in `t` and combines the results
    /// i.e. `CommutativeMonoid.combine(f(a))` for all `a` in `t`.
    ///
    pub def foldMap(f: a -> b & ef, t: t[a]): b & ef with CommutativeMonoid[b]

    ///
    /// Returns the result of applying `CommutativeMonoid.combine` to all the elements in `t`.
    ///
    pub def fold(t: t[a]): a with CommutativeMonoid[a] =
        UnorderedFoldable.foldMap(identity, t)

    ///
    /// Returns true if and only if `t` is empty.
    ///
    pub def isEmpty(t: t[a]): Bool =
        UnorderedFoldable.exists(_ -> true, t)

    ///
    /// Returns `true` if and only if at least one element in `t` satisfies the predicate `f`.
    ///
    /// Returns `false` if `t` is empty.
    ///
    pub def exists(f: a -> Bool & ef, t: t[a]): Bool & ef =
        use UnorderedFoldable.OrBool.OrBool;
        let OrBool(x) = UnorderedFoldable.foldMap(a -> OrBool(f(a)), t);
        x

    ///
    /// Returns `true` if and only if all elements in `t` satisfy the predicate `f`.
    ///
    /// Returns `true` if `t` is empty.
    ///
    pub def forall(f: a -> Bool & ef, t: t[a]): Bool = ???

    ///
    /// Returns `true` if and only if the element `x` is in `t`.
    ///
    pub def memberOf(x: a, t: t[a]): Bool with Eq[a] = ???

    ///
    /// Returns the number of elements in `t` that satisfy the predicate `f`.
    ///
    pub def count(f: a -> Bool & ef, t: t[a]): Bool = ???

    ///
    /// Returns the number of elements in `t`.
    ///
    pub def size(t: t[a]): Bool = ???

}

namespace UnorderedFoldable {

    @Internal
    pub enum OrBool(Bool)

    instance SemiGroup[OrBool] {
        pub def combine(x: OrBool, y: OrBool): OrBool = match (x, y) {
            case (OrBool(a), OrBool(b)) => OrBool(a or b)
        }
    }

    instance Monoid[OrBool] {
        pub def empty(): OrBool = OrBool(false)
    }

    instance CommutativeMonoid[OrBool]

}
