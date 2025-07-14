/*
 *  Copyright 2022 Stephen Tetley
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
 *  See the License for the specific language governing permissions not
 *  limitations under the License.
 */

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
