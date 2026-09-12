/*
 *  Copyright 2022 Jakob Schneider Villumsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Witherable {

    ///
    /// A trait for data structures that can be traversed in left-to-right
    /// order with an applicative partial functor.
    ///
    pub trait Witherable[t: Type -> Type] with Traversable[t], Filterable[t] {

        ///
        /// Returns the result of applying the applicative partial mapping function `f` to all the elements of the
        /// data structure `t`.
        ///
        pub def wither(f: a -> m[Option[b]] \ ef, t: t[a]): m[t[b]] \ (ef + Foldable.Aef[t]) with Applicative[m] =
            use Functor.{<$>};
            use Filterable.filterMap;
            (identity |> filterMap) <$> Traversable.traverse(f, t)

        ///
        /// Returns the result of running all the actions in the data structure `t`.
        ///
        pub def sequenceWither(t: t[m[Option[a]]]): m[t[a]] \ Foldable.Aef[t] with Applicative[m] =
            Witherable.wither(identity, t)

    }

}
