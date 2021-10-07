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
class Functor[m : Type -> Type] {
    ///
    /// Applies the function `f` to `s` preserving its structure.
    ///
    pub def map(f: a -> b & ef, x: m[a]): m[b] & ef

    ///
    /// Mapping the identity function over a functor preserves the functor.
    ///
    law identity: forall(x: m[a]) with Eq[m[a]] . Functor.map(identity, x) == x

    ///
    /// Composing two functions and mapping the resulting function over a functor is the same as
    /// mapping the functions one after the other over the functor.
    ///
    law composition: forall(f: b -> c, g: a -> b, v: m[a]) with Eq[m[c]] . Functor.map(g >> f, v) == Functor.map(f, Functor.map(g, v))
}

namespace Functor {
  ///
  /// Lifts the function `f` into the functor.
  ///
  pub def lift[a: Type, b: Type, ef: Bool, m: Type -> Type](f: a -> b & ef): m[a] -> m[b] & ef with Functor[m] = Functor.map(f)

  ///
  /// Replaces the value `a` in `s` by the given value `x` preserving the structure of `s`.
  ///
  pub def as[a: Type, b: Type, m: Type -> Type](s: m[a], x: b): m[b] with Functor[m] = Functor.map(_ -> x, s)
}
