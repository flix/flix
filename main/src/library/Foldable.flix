/*
 *  Copyright 2021 Esben Bjerre
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
/// A type class for data structures that can be folded.
///
pub class Foldable[t :# Type -> Type] {

    ///
    /// Left-associative fold of a structure.
    /// Applies `f` to a start value `s` and all elements in `t` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, t1), t2)..., tn)`.
    ///
    pub def foldLeft(f: (b, a) -> b & e, s: b, t: t[a]): b & e

    ///
    /// Right-associative fold of a structure.
    /// Applies `f` to a start value `s` and all elements in `t` going from right to left.
    ///
    /// That is, the result is of the form: `f(t1, ...f(tn-1, f(tn, s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b & e, s: b, t: t[a]): b & e

}

namespace Foldable {

    ///
    /// Returns the result of applying `combine` to all the elements `t`,
    /// using `empty` as the initial value.
    ///
    pub def combineAll[t :# Type -> Type : Foldable, a : Monoid](t: t[a]): a =
        Foldable.foldLeft((acc, x) -> Monoid.combine(acc, x), Monoid.empty(), t)

    ///
    /// Returns the number of elements in `t` that satisfy the predicate `f`.
    ///
    pub def count[t :# Type -> Type : Foldable, a](f: a -> Bool, t: t[a]): Int32 =
        Foldable.foldLeft((acc, x) -> if (f(x)) acc + 1 else acc, 0, t)

    ///
    /// Returns `true` if and only if at least one element in `t` satisfies the predicate `f`.
    ///
    /// Returns `false` if `t` is empty.
    ///
    pub def exists[t :# Type -> Type : Foldable, a](f: a -> Bool, t: t[a]): Bool =
        Foldable.foldLeft((acc, x) -> acc or f(x), false, t)

    ///
    /// Returns `true` if and only if all elements in `t` satisfy the predicate `f`.
    ///
    /// Returns `true` if `t` is empty.
    ///
    pub def forall[t :# Type -> Type : Foldable, a](f: a -> Bool, t: t[a]): Bool =
        Foldable.foldLeft((acc, x) -> acc and f(x), true, t)

    ///
    /// Optionally returns the first element of `t` that satisfies the predicate `f` when searching from left to right.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def findLeft[t :# Type -> Type : Foldable, a](f: a -> Bool, t: t[a]): Option[a] =
        Foldable.foldLeft((acc, x) -> Option.withDefault(acc, if (f(x)) Some(x) else None), None, t)

    ///
    /// Optionally returns the first element of `t` that satisfies the predicate `f` when searching from right to left.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def findRight[t :# Type -> Type : Foldable, a](f: a -> Bool, t: t[a]): Option[a] =
        Foldable.foldRight((x, acc) -> Option.withDefault(acc, if (f(x)) Some(x) else None), None, t)

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap[t :# Type -> Type : Foldable, a, b : Monoid](f: a -> b, t: t[a]): b =
        Foldable.foldLeft((acc, x) -> Monoid.combine(f(x), acc), Monoid.empty(), t)

    ///
    /// Returns true if and only if `t` is empty.
    ///
    pub def isEmpty[t :# Type -> Type : Foldable, a](t: t[a]): Bool =
        Foldable.foldLeft((_, _) -> false, true, t)

    ///
    /// Returns `true` if and only if the element `x` is in `t`.
    ///
    pub def memberOf[t :# Type -> Type : Foldable, a: Eq](x: a, t: t[a]): Bool =
        Foldable.foldLeft((acc, y) -> acc or x == y, false, t)

    ///
    /// Optionally finds the smallest element of `t` according to the `Order` on `a`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def minimum[t :# Type -> Type : Foldable, a: Order](t: t[a]): Option[a] =
        Foldable.foldLeft((acc, x) -> Some(Option.foldLeft((z, v) -> Order.min(z, v), x, acc)), None, t)

    ///
    /// Optionally finds the smallest element of `t` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def minimumBy[t :# Type -> Type : Foldable, a](cmp: (a, a) -> Comparison, t: t[a]): Option[a] =
        Foldable.foldLeft((acc, x) -> Some(Option.foldLeft((z, v) -> Order.minBy(cmp, z, v), x, acc)), None, t)

    ///
    /// Optionally finds the largest element of `t` according to the `Order` on `a`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def maximum[t :# Type -> Type : Foldable, a: Order](t: t[a]): Option[a] =
        Foldable.foldLeft((acc, x) -> Some(Option.foldLeft((z, v) -> Order.max(z, v), x, acc)), None, t)

    ///
    /// Optionally finds the largest element of `t` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def maximumBy[t :# Type -> Type : Foldable, a](cmp: (a, a) -> Comparison, t: t[a]): Option[a] =
        Foldable.foldLeft((acc, x) -> Some(Option.foldLeft((z, v) -> Order.maxBy(cmp, z, v), x, acc)), None, t)

    ///
    /// Optionally applies `f` to all elements in `t` going from left to right until a single value is obtained.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(x1, x2), x3)..., xn))`
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def reduceLeft[t :# Type -> Type : Foldable, a, e](f: (a, a) -> a & e, t: t[a]): Option[a] & e =
        foldLeft((acc, x) -> Some(Option.foldLeft((z, v) -> f(v, z), x, acc)), None, t)

    ///
    /// Optionally applies `f` to all elements in `t` going from right to left until a single value is obtained.
    ///
    /// That is, the result is of the form: `Some(f(x1, ...f(xn-2, f(xn-1, xn))...))`
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def reduceRight[t :# Type -> Type : Foldable, a, e](f: (a, a) -> a & e, t: t[a]): Option[a] & e =
        foldRight((x, acc) -> Some(Option.foldLeft((v, z) -> f(v, z), x, acc)), None, t)

    ///
    /// Returns `t` as an immutable list.
    ///
    /// That is, the result is of the form: `t1 :: t2 :: ... :: tn :: Nil`.
    ///
    pub def toList[t :# Type -> Type : Foldable, a](t: t[a]): List[a] =
        List.reverse(foldLeft((acc, x) -> x :: acc, Nil, t))

}