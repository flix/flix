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
    pub def foldLeft(f: b -> a -> b & e, s: b, t: t[a]): b & e

    ///
    /// Right-associative fold of a structure.
    /// Applies `f` to a start value `s` and all elements in `t` going from right to left.
    ///
    /// That is, the result is of the form: `f(t1, ...f(tn-1, f(tn, s))...)`.
    ///
    pub def foldRight(f: a -> b -> b & e, s: b, t: t[a]): b & e

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
    pub def find[t :# Type -> Type : Foldable, a](f: a -> Bool, t: t[a]): Option[a] =
        Foldable.foldLeft((acc, x) -> match acc {
            case None => if (f(x)) Some(x) else None
            case Some(y) => Some(y)
        }, None, t)

    pub def foldMap[t :# Type -> Type : Foldable, a, b : Monoid](f: a -> b, t: t[a]): b =
        Foldable.foldLeft((acc, x) -> Monoid.combine(f(x), acc), Monoid.empty(), t)

    //// def intercalate[f: Foldable, a: Monoid](fa: f[a], a: a): f[a]
    /// TODO: Fix intercalate and remove^
    ///
    /// Returns the concatenation of the elements in `t` with `a` inserted between every two adjacent elements.
    ///
    /// That is, returns `t1 :: a :: ... :: tn-1 :: a :: tn :: Nil`.
    ///
///    pub def intercalate[t :# Type -> Type : Foldable : Monoid, a](t: t[a], a: a): t[a] =
///        Foldable.foldLeft((acc, x) -> Monoid.combine(acc, Monoid.combine(x, a)), Monoid.empty(), t)

    ///
    /// Returns true if and only if `t` is empty.
    ///
    pub def isEmpty[t :# Type -> Type : Foldable, a](t: t[a]): Bool =
        Foldable.foldLeft((_, _) -> false, true, t)

    ///
    /// Optionally finds the smallest element of `t` according to the `Order` on `a`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def minimum[t :# Type -> Type : Foldable, a: Order](t: t[a]): Option[a] =
        Foldable.foldLeft((acc, x) -> match acc {
            case None => Some(x)
            case Some(y) => Some(Order.min(x, y))
        }, None, t)

    ///
    /// Optionally finds the smallest element of `t` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def minimumBy[t :# Type -> Type : Foldable, a](cmp: (a, a) -> Comparison, t: t[a]): Option[a] =
        Foldable.foldLeft((acc, x) -> match acc {
            case None => Some(x)
            case Some(y) => Some(Order.minBy(cmp, x, y))
        }, None, t)

    ///
    /// Optionally finds the largest element of `t` according to the `Order` on `a`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def maximum[t :# Type -> Type : Foldable, a: Order](t: t[a]): Option[a] =
        Foldable.foldLeft((acc, x) -> match acc {
            case None => Some(x)
            case Some(y) => Some(Order.max(x, y))
        }, None, t)

    ///
    /// Optionally finds the largest element of `t` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def maximumBy[t :# Type -> Type : Foldable, a](cmp: (a, a) -> Comparison, t: t[a]): Option[a] =
        Foldable.foldLeft((acc, x) -> match acc {
            case None => Some(x)
            case Some(y) => Some(Order.maxBy(cmp, x, y))
        }, None, t)

}