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
/// A trait for types that can be mapped over.
///
pub lawful trait Functor[m : Type -> Type] {
    ///
    /// Applies the function `f` to `x` preserving its structure.
    ///
    pub def map(f: a -> b \ ef, x: m[a]): m[b] \ ef

    ///
    /// Mapping the identity function over a functor preserves the functor.
    ///
    law identity: forall(x: m[a]) with Eq[m[a]] Functor.map(identity, x) == x

    ///
    /// Composing two functions and mapping the resulting function over a functor is the same as
    /// mapping the functions one after the other over the functor.
    ///
    law composition: forall(f: b -> c, g: a -> b, v: m[a]) with Eq[m[c]] Functor.map(g >> f, v) == Functor.map(f, Functor.map(g, v))
}

mod Functor {

    ///
    /// Replaces the value `a` in `s` by the given value `x` preserving the structure of `s`.
    ///
    pub def replace[a: Type, b: Type, m: Type -> Type](s: m[a], x: b): m[b] with Functor[m] = Functor.map(_ -> x, s)

    ///
    /// Replaces the value `a` in `s` with `Unit` preserving the structure of `s`.
    ///
    /// This function is typically used to discard the return value of computing `s`.
    ///
    pub def ignore(s: m[a]): m[Unit] with Functor[m] = s `Functor.replace` ()

    ///
    /// `<$>` is an operator alias for `map`.
    ///
    pub def <$>(f: a -> b \ ef, x: m[a]): m[b] \ ef with Functor[m]= Functor.map(f, x)

    ///
    /// `<$$>` is the operator `<$>` with its arguments flipped.
    ///
    pub def <$$>(x: m[a], f: a -> b \ ef): m[b] \ ef with Functor[m]= Functor.map(f, x)

    ///
    /// `<$` is the operator `$>` with its arguments flipped.
    ///
    pub def <$(x: a, ma: f[b]): f[a] with Functor[f] = Functor.replace(ma, x)

    ///
    /// `$>` is an operator alias for `replace`.
    ///
    pub def $>(ma: f[a], x: b): f[b] with Functor[f] = Functor.replace(ma, x)

}
