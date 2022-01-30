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
/// `Reducible` is like a non-empty `Foldable` and may only be implemented on non-empty data structures.
///
/// This type class is heavily inspired by the Cats implementation of Reducible:
/// https://github.com/typelevel/cats/blob/main/core/src/main/scala/cats/Reducible.scala
///
pub lawless class Reducible[t: Type -> Type] {

    ///
    /// Left-associative reduction of a structure.
    /// Applies `g` to the initial element of `t` and combines it
    /// with the remainder of `t` using `f` going from left to right.
    ///
    pub def reduceLeftTo(f: (b, a) -> b & ef1, g: a -> b & ef2, t: t[a]): b & (ef1 and ef2)

    ///
    /// Right-associative reduction of a structure.
    /// Applies `g` to the initial element of `t` and combines it
    /// with the remainder of `t` using `f` going from right to left.
    ///
    pub def reduceRightTo(f: (a, b) -> b & ef1, g: a -> b & ef2, t: t[a]): b & (ef1 and ef2)

    ///
    /// Left-associative reduction on `t` using `f`.
    ///
    pub def reduceLeft(f: (a, a) -> a & ef, t: t[a]): a & ef =
        Reducible.reduceLeftTo(f, identity, t)

    ///
    /// Right-associative reduction on `t` using `f`.
    ///
    pub def reduceRight(f: (a, a) -> a & ef, t: t[a]): a & ef =
        Reducible.reduceRightTo(f, identity, t)

    ///
    /// Reduce `t` using the derived `SemiGroup` instance.
    ///
    pub def reduce(t: t[a]): a with SemiGroup[a] =
        Reducible.reduceLeft((acc, a) -> SemiGroup.combine(acc, a), t)

    ///
    /// Applies `f` to each element of `t` and combines them using the derived `SemiGroup` instance.
    ///
    pub def reduceMap(f: a -> b & ef, t: t[a]): b & ef with SemiGroup[b] =
        Reducible.reduceLeftTo((b, a) -> SemiGroup.combine(b, f(a)), f, t)

    ///
    /// Left-associative fold of a structure.
    /// Applies `f` to a start value `s` and all elements in `t` going from left to right.
    ///
    pub def foldLeft(f: (b, a) -> b & ef, s: b, t: t[a]): b & ef =
        Reducible.reduceLeftTo(f, a -> f(s, a), t)

    ///
    /// Right-associative fold of a structure.
    /// Applies `f` to a start value `s` and all elements in `t` going from right to left.
    ///
    pub def foldRight(f: (a, b) -> b & ef, s: b, t: t[a]): b & ef =
        Reducible.reduceRightTo(f, a -> f(a, s), t)

    ///
    /// Returns the first element of `t`.
    ///
    pub def head(t: t[a]): a =
        Reducible.reduceLeft((acc, _) -> acc, t)

    ///
    /// Returns the last element of `t`.
    ///
    pub def last(t: t[a]): a =
        Reducible.reduceRight((_, acc) -> acc, t) // Could use reduceLeft w. lambda (_, a) -> a

    ///
    /// Returns the number of elements in `t` that satisfy the predicate `f`.
    ///
    pub def count(f: a -> Bool, t: t[a]): Int32 =
        Reducible.reduceMap(a -> if (f(a)) 1 else 0, t)

    ///
    /// Returns the number of elements in `t`.
    ///
    pub def length(t: t[a]): Int32 =
        Reducible.reduceMap(_ -> 1, t)

    ///
    /// Returns the sum of all elements in `t`.
    ///
    pub def sum(t: t[Int32]): Int32 = ???

    ///
    /// Returns the sum of all elements in `t` according to the function `f`.
    ///
    pub def sumWith(f: a -> Int32 & ef, t: t[a]): Int32 = ???

    ///
    /// Returns the product of all elements in `t`.
    ///
    pub def product(t: t[Int32]): Int32 = ???

    ///
    /// Returns the product of all elements in `t` according to the function `f`.
    ///
    pub def productWith(f: a -> Int32 & ef, t: t[a]): Int32 = ???

    ///
    /// Returns `true` if and only if at least one element in `t` satisfies the predicate `f`.
    ///
    pub def exists(f: a -> Bool & ef, t: t[a]): Bool = ???

    ///
    /// Returns `true` if and only if all elements in `t` satisfy the predicate `f`.
    ///
    pub def forall(f: a -> Bool & ef, t: t[a]): Bool = ???

    ///
    /// Optionally returns the first element of `t` that satisfies the predicate `f` when searching from left to right.
    ///
    pub def findLeft(f: a -> Bool & ef, t: t[a]): Option[a] = ???

    ///
    /// Optionally returns the first element of `t` that satisfies the predicate `f` when searching from right to left.
    ///
    pub def findRight(f: a -> Bool & ef, t: t[a]): Option[a] = ???

    ///
    /// Returns `true` if and only if the element `x` is in `t`.
    ///
    pub def memberOf(x: a, t: t[a]): Bool with Eq[a] = ???

    ///
    /// Finds the smallest element of `t` according to the `Order` on `a`.
    ///
    pub def minimum(t: t[a]): a with Order[a] = ???

    ///
    /// Finds the smallest element of `t` according to the given comparator `cmp`.
    ///
    pub def minimumBy(cmp: (a, a) -> Comparison & ef, t: t[a]): a = ???

    ///
    /// Finds the largest element of `t` according to the `Order` on `a`.
    ///
    pub def maximum(t: t[a]): a with Order[a] = ???

    ///
    /// Finds the largest element of `t` according to the given comparator `cmp`.
    ///
    pub def maximumBy(cmp: (a, a) -> Comparison & ef, t: t[a]): a = ???

    ///
    /// Returns `t` as a non-empty list.
    ///
    pub def toNel(t: t[a]): Nel[a] = ???

    ///
    /// Returns `t` as a list.
    ///
    pub def toList(t: t[a]): List[a] = ???

    ///
    /// Returns `t` as a set.
    ///
    pub def toSet(t: t[a]): Set[a] with Order[a] = ???

}
