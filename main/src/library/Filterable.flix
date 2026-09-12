/*
 *  Copyright 2022 Magnus Madsen, Stephen Tetley
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Filterable {

    ///
    /// A trait for filtering container functors.
    ///
    pub trait Filterable[m: Type -> Type] with Functor[m], Foldable[m] {

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

}
