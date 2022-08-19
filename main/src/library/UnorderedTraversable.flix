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
/// A type class for unordered data structures that can be traversed with an applicative functor.
///
pub class UnorderedTraversable[t : Type -> Type] {

    ///
    /// Unordered traverse of `t`.
    ///
    /// Returns the result of applying the applicative mapping function `f` to all the elements of the
    /// data structure `t`.
    ///
    pub def traverse(f: a -> m[b] \ ef, t: t[a]): m[t[b]] \ ef with CommutativeApplicative[m]

    ///
    /// Unordered sequence of `t`.
    ///
    /// Returns the result of running all the actions in the data structure `t`.
    ///
    pub def sequence(t: t[m[a]]): m[t[a]] with CommutativeApplicative[m] =
        UnorderedTraversable.traverse(identity, t)

    ///
    /// Traversing with the identity function wrapped into an applicative preserves the container `t`.
    ///
    law identity: forall(t: t[a], f: a -> m[a]) with Eq[m[t[a]]], Applicative[m] . Traversable.traverse(f, t) == Applicative.point(t)

    ///
    /// sequence identity.
    ///
    law identity: forall(t: t[a], f: a -> m[a]) with Eq[m[t[a]]], Applicative[m] . Traversable.sequence(Functor.map(f, t)) == Applicative.point(t)

}
