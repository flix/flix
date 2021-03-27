/*
 *  Copyright 2020 Matthew Lutze, Esben Bjerre, Magnus Madsen
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
/// A type class for types that can be mapped over.
///
lawless class Functor[m : Type -> Type] {
    ///
    /// Applies the function `f` to `s` preserving its structure.
    ///
    pub def map(f: a -> b & e, x: m[a]): m[b] & e
}

namespace Functor {
  ///
  /// Lifts the function `f` into the functor.
  ///
  pub def lift[a, b, e, m : Type -> Type has Functor](f: a -> b & e): m[a] -> m[b] & e = Functor.map(f)

  ///
  /// Replaces the value `a` in `s` by the given value `x` preserving the structure of `s`.
  ///
  pub def as[a, b, m : Type -> Type has Functor](s: m[a], x: b): m[b] = Functor.map(_ -> x, s)
}