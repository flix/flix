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
pub lawless class Foldable[t : Type -> Type] {

    ///
    /// Left-associative fold of a structure.
    /// Applies `f` to a start value `s` and all elements in `t` going from left to right.
    ///
    pub def foldLeft(f: (b, a) -> b & e, s: b, t: t[a]): b & e

    ///
    /// Right-associative fold of a structure.
    /// Applies `f` to a start value `s` and all elements in `t` going from right to left.
    ///
    pub def foldRight(f: (a, b) -> b & e, s: b, t: t[a]): b & e

}

namespace Foldable {

    ///
    /// Returns the result of applying `combine` to all the elements in `t`, using `empty` as the initial value.
    ///
    pub def fold[t : Type -> Type, a](t: t[a]): a with Foldable[t], Monoid[a] =
        Foldable.foldLeft((acc, x) -> Monoid.combine(acc, x), Monoid.empty(), t)

    ///
    /// Returns the number of elements in `t` that satisfy the predicate `f`.
    ///
    pub def count[t : Type -> Type, a](f: a -> Bool, t: t[a]): Int32 with Foldable[t] =
        Foldable.foldLeft((acc, x) -> if (f(x)) acc + 1 else acc, 0, t)

    ///
    /// Returns `true` if and only if at least one element in `t` satisfies the predicate `f`.
    ///
    /// Returns `false` if `t` is empty.
    ///
    pub def exists[t : Type -> Type, a](f: a -> Bool, t: t[a]): Bool with Foldable[t] =
        Foldable.foldLeft((acc, x) -> acc or f(x), false, t)

    ///
    /// Returns `true` if and only if all elements in `t` satisfy the predicate `f`.
    ///
    /// Returns `true` if `t` is empty.
    ///
    pub def forall[t : Type -> Type, a](f: a -> Bool, t: t[a]): Bool with Foldable[t] =
        Foldable.foldLeft((acc, x) -> acc and f(x), true, t)

    ///
    /// Optionally returns the first element of `t` that satisfies the predicate `f` when searching from left to right.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def findLeft[t : Type -> Type, a](f: a -> Bool, t: t[a]): Option[a] with Foldable[t] =
        Foldable.foldLeft((acc, x) -> Option.withDefault(acc, if (f(x)) Some(x) else None), None, t)

    ///
    /// Optionally returns the first element of `t` that satisfies the predicate `f` when searching from right to left.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def findRight[t : Type -> Type, a](f: a -> Bool, t: t[a]): Option[a] with Foldable[t] =
        Foldable.foldRight((x, acc) -> Option.withDefault(acc, if (f(x)) Some(x) else None), None, t)

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap[t : Type -> Type, a, b](f: a -> b, t: t[a]): b with Foldable[t], Monoid[b] =
        Foldable.foldLeft((acc, x) -> Monoid.combine(f(x), acc), Monoid.empty(), t)

    ///
    /// Returns true if and only if `t` is empty.
    ///
    pub def isEmpty[t : Type -> Type, a](t: t[a]): Bool with Foldable[t] =
        Foldable.foldLeft((_, _) -> false, true, t)

    ///
    /// Returns `true` if and only if the element `x` is in `t`.
    ///
    pub def memberOf[t : Type -> Type, a](x: a, t: t[a]): Bool with Foldable[t], Eq[a] =
        Foldable.foldLeft((acc, y) -> acc or x == y, false, t)

    ///
    /// Optionally finds the smallest element of `t` according to the `Order` on `a`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def minimum[t : Type -> Type, a](t: t[a]): Option[a] with Foldable[t], Order[a] =
        reduceLeft(Order.min, t)

    ///
    /// Optionally finds the smallest element of `t` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def minimumBy[t : Type -> Type, a](cmp: (a, a) -> Comparison, t: t[a]): Option[a] with Foldable[t] =
        reduceLeft(Order.minBy(cmp), t)

    ///
    /// Optionally finds the largest element of `t` according to the `Order` on `a`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def maximum[t : Type -> Type, a](t: t[a]): Option[a] with Foldable[t], Order[a] =
        reduceLeft(Order.max, t)

    ///
    /// Optionally finds the largest element of `t` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def maximumBy[t : Type -> Type, a](cmp: (a, a) -> Comparison, t: t[a]): Option[a] with Foldable[t] =
        reduceLeft(Order.maxBy(cmp), t)

    ///
    /// Optionally applies `f` to all elements in `t` going from left to right until a single value is obtained.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def reduceLeft[t : Type -> Type, a, e](f: (a, a) -> a & e, t: t[a]): Option[a] & e with Foldable[t] =
        foldLeft((acc, x) -> Some(Option.foldLeft((z, v) -> f(v, z), x, acc)), None, t)

    ///
    /// Optionally applies `f` to all elements in `t` going from right to left until a single value is obtained.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def reduceRight[t : Type -> Type, a, e](f: (a, a) -> a & e, t: t[a]): Option[a] & e with Foldable[t] =
        foldRight((x, acc) -> Some(Option.foldLeft((v, z) -> f(v, z), x, acc)), None, t)

    ///
    /// Returns `t` as an immutable list.
    ///
    pub def toList[t : Type -> Type, a](t: t[a]): List[a] with Foldable[t] =
        foldRight((x, acc) -> x :: acc, Nil, t)

    ///
    /// Returns `t` as an array.
    ///
    pub def toArray[t : Type -> Type, a](t: t[a]): Array[a] & Impure with Foldable[t] =
        let v = MutList.new();
        foldRight((x, _) -> MutList.push!(x, v), (), t);
        MutList.toArray(v)

    ///
    /// Returns `t` as a mutable list.
    ///
    pub def toMutList[t : Type -> Type, a](t: t[a]): MutList[a] & Impure with Foldable[t] =
        let v = MutList.new();
        foldRight((x, _) -> MutList.push!(x, v), (), t);
        v

    ///
    /// Returns `t` as a set.
    ///
    pub def toSet[t : Type -> Type, a](t: t[a]): Set[a] with Foldable[t], Order[a] =
        foldRight((x, acc) -> Set.insert(x, acc), Set.empty(), t)

    ///
    /// Returns `t` as a map.
    ///
    pub def toMap[t : Type -> Type, k, v](t: t[(k, v)]): Map[k, v] with Foldable[t], Order[k] =
        foldRight((x, acc) -> let (k, v) = x; Map.insert(k, v, acc), Map.empty(), t)

    ///
    /// Returns `t` without the longest prefix that satisfies the predicate `f`.
    ///
    /// Returns an immutable list.
    ///
    pub def dropWhile[t : Type -> Type, a](f: a -> Bool, t: t[a]): List[a] with Foldable[t] =
        foldLeft((acc, x) -> {
                let (c, tail) = acc;
                if (c and f(x)) (true, tail)
                else (false, x :: tail)
            }, (true, Nil), t) |> snd |> List.reverse

    ///
    /// Returns the longest prefix of `t` that satisfies the predicate `f`.
    ///
    /// Returns an immutable list.
    ///
    pub def takeWhile[t : Type -> Type, a](f: a -> Bool, t: t[a]): List[a] with Foldable[t] =
        foldRight((x, acc) -> if (f(x)) x :: acc else Nil, Nil, t)

    ///
    /// Returns an immutable list of all the elements in `t` that satisfy the predicate `f`.
    ///
    pub def filter[t : Type -> Type, a](f: a -> Bool, t: t[a]): List[a] with Foldable[t] =
        foldRight((x, acc) -> if (f(x)) x :: acc else acc, Nil, t)

}
