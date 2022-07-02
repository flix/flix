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

}
