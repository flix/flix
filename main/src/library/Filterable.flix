/*
 *  Copyright 2022 Magnus Madsen, Stephen Tetley
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
/// A trait for filtering container functors.
///
pub trait Filterable[m : Type -> Type] with Functor[m], Foldable[m] {

    ///
    /// Applies the partial function `f` to every element in `x` collecting the results.
    ///
    pub def filterMap(f: a -> Option[b] \ ef, t: m[a]): m[b] \ (ef + Foldable.Aef[m])

    ///
    /// Applies `f` to every element in `x`. Keeps every element that satisfies `f`.
    ///
    pub def filter(f: a -> Bool \ ef, t: m[a]): m[a] \ (ef + Foldable.Aef[m]) =
        Filterable.filterMap(x -> if (f(x)) Some(x) else None, t)

}
