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
/// A type class for types that can be reduced to a summary value.
///
/// `Reducible` is like a non-empty `Foldable`.
///
/// This type class is heavily inspired by the Cats implementation of Reducible:
/// https://github.com/typelevel/cats/blob/main/core/src/main/scala/cats/Reducible.scala
///
pub lawless class Reducible[t: Type -> Type] with Foldable[t] {

    ///
    /// Left-associative reduction of a structure.
    /// Applies `g` to the initial element of `t` and combines it
    /// with the remainder of `t` using `f` going from left to right.
    ///
    pub def reduceLeftTo(f: (b, a) -> b & ef, g: a -> b, t: t[a]): b & ef

    ///
    /// Right-associative reduction of a structure.
    /// Applies `g` to the initial element of `t` and combines it
    /// with the remainder of `t` using `f` going from right to left.
    ///
    pub def reduceRightTo(f: (a, b) -> b & ef, g: a -> b, t: t[a]): b & ef

    ///
    /// Left-associative fold of a structure.
    /// Applies `f` to a start value `s` and all elements in `t` going from left to right.
    ///
    pub def foldLeft(f: (b, a) -> b & ef, s: b, t: t[a]): b & ef =
        Foldable.foldLeft(f, s, t)

    ///
    /// Right-associative fold of a structure.
    /// Applies `f` to a start value `s` and all elements in `t` going from right to left.
    ///
    pub def foldRight(f: (a, b) -> b & ef, s: b, t: t[a]): b & ef =
        Foldable.foldRight(f, s, t)

}
