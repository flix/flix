/*
 *  Copyright 2022 Stephen Tetley
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

mod MonadZip {

    ///
    /// A trait for zipping Monads, typically container monads like `List`.
    ///
    /// A minimal implementation must define `zipWith` and `zipWithA`.
    ///
    trait MonadZip[m: Type -> Type] with Monad[m] {

        ///
        /// Returns single monad where the element (or elements) of `ma` and `mb` are combined
        /// with the function `f`.
        ///
        pub def zipWith(f: (a, b) -> c \ ef, ma: m[a], mb: m[b]): m[c] \ ef

        ///
        /// Returns single monad where the element (or elements) of `ma` and `mb` are combined
        /// as pairs.
        ///
        pub def zip(ma: m[a], mb: m[b]): m[(a, b)] = MonadZip.zipWith((x, y) -> (x, y), ma, mb)

        ///
        /// Returns a pair of monads, the first containing the element (or elements) of the left part of `mx`
        /// the second containing the element (or elements) of the right part of `mx`.
        ///
        pub def unzip(mx: m[(a, b)]): (m[a], m[b]) = (Functor.map(fst, mx), Functor.map(snd, mx))

        ///
        /// Generalized version of `zipWith` where `f` zips an applicative functor across the
        /// (monadic) containers `ma` and `mb`.
        ///
        pub def zipWithA(f: (a, b) -> f[c] \ ef, ma: m[a], mb: m[b]): f[m[c]] \ ef with Applicative[f]

    }

}
