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
/// A trait for data structures that can be traversed in left-to-right
/// order with an applicative partial functor.
///
pub trait Witherable[t : Type -> Type] with Traversable[t], Filterable[t] {

    ///
    /// Returns the result of applying the applicative partial mapping function `f` to all the elements of the
    /// data structure `t`.
    ///
    pub def wither(f: a -> m[Option[b]] \ ef, t: t[a]): m[t[b]] \ ef with Applicative[m] =
        use Functor.{<$>};
        use Filterable.filterMap;
        (identity |> filterMap) <$> Traversable.traverse(f, t)

    ///
    /// Returns the result of running all the actions in the data structure `t`.
    ///
    pub def sequenceWither(t: t[m[Option[a]]]): m[t[a]] with Applicative[m] =
        Witherable.wither(identity, t)

}
