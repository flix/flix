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
/// A trait for types that can be reduced to a summary value.
///
/// `Reducible` is like a non-empty `Foldable` and may only be implemented on non-empty data structures.
///
pub trait Reducible[t: Type -> Type] {

    ///
    /// Left-associative reduction of a structure.
    /// Applies `g` to the initial element of `t` and combines it
    /// with the remainder of `t` using `f` going from left to right.
    ///
    pub def reduceLeftTo(f: (b, a) -> b \ ef1, g: a -> b \ ef2, t: t[a]): b \ { ef1, ef2 }

    ///
    /// Right-associative reduction of a structure.
    /// Applies `g` to the initial element of `t` and combines it
    /// with the remainder of `t` using `f` going from right to left.
    ///
    pub def reduceRightTo(f: (a, b) -> b \ ef1, g: a -> b \ ef2, t: t[a]): b \ { ef1, ef2 }

    ///
    /// Left-associative reduction on `t` using `f`.
    ///
    pub def reduceLeft(f: (a, a) -> a \ ef, t: t[a]): a \ ef =
        Reducible.reduceLeftTo(f, identity, t)

    ///
    /// Right-associative reduction on `t` using `f`.
    ///
    pub def reduceRight(f: (a, a) -> a \ ef, t: t[a]): a \ ef =
        Reducible.reduceRightTo(f, identity, t)

    ///
    /// Reduce `t` using the derived `SemiGroup` instance.
    ///
    pub def reduce(t: t[a]): a with SemiGroup[a] =
        Reducible.reduceLeft(SemiGroup.combine, t)

    ///
    /// Applies `f` to each element of `t` and combines them using the derived `SemiGroup` instance.
    ///
    pub def reduceMap(f: a -> b \ ef, t: t[a]): b \ ef with SemiGroup[b] =
        Reducible.reduceLeftTo((b, a) -> SemiGroup.combine(b, f(a)), f, t)

    ///
    /// Left-associative fold of a structure.
    /// Applies `f` to a start value `s` and all elements in `t` going from left to right.
    ///
    pub def foldLeft(f: (b, a) -> b \ ef, s: b, t: t[a]): b \ ef =
        Reducible.reduceLeftTo(f, a -> f(s, a), t)

    ///
    /// Right-associative fold of a structure.
    /// Applies `f` to a start value `s` and all elements in `t` going from right to left.
    ///
    pub def foldRight(f: (a, b) -> b \ ef, s: b, t: t[a]): b \ ef =
        Reducible.reduceRightTo(f, a -> f(a, s), t)

    ///
    /// Right-associative fold of a structure.
    /// Applies `f` to a start value `s` and all elements in `t` going from right to left.
    ///
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, z: b, t: t[a]): b \ ef =
        let f1 = (a, b) -> f(a, _ -> checked_ecast(b));
        Reducible.reduceRightTo(f1, a -> f(a, _ -> checked_ecast(z)), t)

    ///
    /// Alias for `reduce`.
    ///
    /// Reduce `t` using the derived `SemiGroup` instance.
    ///
    pub def fold(t: t[a]): a with SemiGroup[a] =
        Reducible.reduceLeft(SemiGroup.combine, t)

    ///
    /// Returns the first element of `t`.
    ///
    pub def head(t: t[a]): a =
        Reducible.reduceLeft((acc, _) -> acc, t)

    ///
    /// Returns the last element of `t`.
    ///
    pub def last(t: t[a]): a =
        Reducible.reduceLeft((_, a) -> a, t)

    ///
    /// Returns `t` as a list without the last element.
    ///
    pub def init(t: t[a]): List[a] =
        Reducible.reduceRightTo((a, acc) -> a :: acc, _ -> Nil, t)

    ///
    /// Returns the tail of `t` as a list.
    ///
    pub def tail(t: t[a]): List[a] =
        Reducible.reduceLeftTo((k, a) -> ks -> k(a :: ks),  _ -> identity, t)(Nil) // NB: Builds a continuation function without the first
                                                                                   // element that is immediately invoked to avoid `foldRight`.
    ///
    /// Returns the reverse of `t` as a list.
    ///
    pub def reverse(t: t[a]): List[a] =
        Reducible.foldLeft((acc, a) -> a :: acc, Nil, t)

    ///
    /// Returns the number of elements in `t` that satisfy the predicate `f`.
    ///
    pub def count(f: a -> Bool \ ef, t: t[a]): Int32 \ ef =
        Reducible.foldLeft((acc, a) -> if (f(a)) 1 + acc else acc, 0, t)

    ///
    /// Returns the number of elements in `t`.
    ///
    pub def length(t: t[a]): Int32 =
        Reducible.foldLeft((acc, _) -> 1 + acc, 0, t)

    ///
    /// Returns the sum of all elements in `t`.
    ///
    pub def sum(t: t[Int32]): Int32 =
        Reducible.foldLeft((acc, a) -> a + acc, 0, t)

    ///
    /// Returns the sum of all elements in `t` according to the function `f`.
    ///
    pub def sumWith(f: a -> Int32 \ ef, t: t[a]): Int32 \ ef =
        Reducible.foldLeft((acc, a) -> f(a) + acc, 0, t)

    ///
    /// Returns `true` if and only if at least one element in `t` satisfies the predicate `f`.
    ///
    pub def exists(f: a -> Bool \ ef, t: t[a]): Bool \ ef =
        Reducible.foldLeft((acc, a) -> f(a) or acc, false, t)

    ///
    /// Returns `true` if and only if all elements in `t` satisfy the predicate `f`.
    ///
    pub def forAll(f: a -> Bool \ ef, t: t[a]): Bool \ ef =
        Reducible.foldLeft((acc, a) -> f(a) and acc, true, t)

    ///
    /// Applies `f` to each element in `t`.
    ///
    pub def forEach(f: a -> Unit \ ef, t: t[a]): Unit \ ef =
        Reducible.foldLeft((_, a) -> f(a), (), t)

    ///
    /// Alias for `findLeft`.
    ///
    /// Optionally returns the first element of `t` that satisfies the predicate `f` when searching from left to right.
    ///
    pub def find(f: a -> Bool \ ef, t: t[a]): Option[a] \ ef =
        Reducible.findLeft(f, t)

    ///
    /// Optionally returns the first element of `t` that satisfies the predicate `f` when searching from left to right.
    ///
    pub def findLeft(f: a -> Bool \ ef, t: t[a]): Option[a] \ ef =
        Reducible.foldLeft((acc, a) -> Option.withDefault(default = if (f(a)) Some(a) else None, acc), None, t)

    ///
    /// Optionally returns the first element of `t` that satisfies the predicate `f` when searching from right to left.
    ///
    pub def findRight(f: a -> Bool \ ef, t: t[a]): Option[a] \ ef =
        Reducible.foldRight((a, acc) -> Option.withDefault(default = if (f(a)) Some(a) else None, acc), None, t)

    ///
    /// Returns `true` if and only if the element `a` is in `t`.
    ///
    pub def memberOf(a: a, t: t[a]): Bool with Eq[a] =
        Reducible.foldLeft((acc, x) -> a == x or acc, false, t)

    ///
    /// Finds the smallest element of `t` according to the `Order` on `a`.
    ///
    pub def minimum(t: t[a]): a with Order[a] =
        Reducible.reduceLeft(Order.min, t)

    ///
    /// Finds the smallest element of `t` according to the given comparator `cmp`.
    ///
    pub def minimumBy(cmp: (a, a) -> Comparison, t: t[a]): a =
        Reducible.reduceLeft(Order.minBy(cmp), t)

    ///
    /// Finds the largest element of `t` according to the `Order` on `a`.
    ///
    pub def maximum(t: t[a]): a with Order[a] =
        Reducible.reduceLeft(Order.max, t)

    ///
    /// Finds the largest element of `t` according to the given comparator `cmp`.
    ///
    pub def maximumBy(cmp: (a, a) -> Comparison, t: t[a]): a =
        Reducible.reduceLeft(Order.maxBy(cmp), t)

    ///
    /// Returns `t` as a list without the longest prefix that satisfies the predicate `f`.
    ///
    pub def dropWhile(f: a -> Bool \ ef, t: t[a]): List[a] \ ef =
        (Reducible.foldLeft((acc, a) -> {
            let (c, k) = acc;
            if (c and f(a))
                (true, k)
            else
                (false, ks -> k(a :: ks))
        }, (true, identity), t) |> snd)(Nil)

    ///
    /// Returns the longest prefix of `t` as a list that satisfies the predicate `f`.
    ///
    pub def takeWhile(f: a -> Bool \ ef, t: t[a]): List[a] \ ef =
        (Reducible.foldLeft((acc, a) -> {
            let (c, k) = acc;
            if (c and f(a))
                (true, ks -> k(a :: ks))
            else
                (false, k)
        }, (true, identity), t) |> snd)(Nil)

    ///
    /// Returns `t` as a list with `a` inserted between every two adjacent elements.
    ///
    pub def intersperse(a: a, t: t[a]): List[a] =
        Reducible.reduceRightTo((x, acc) -> x :: a :: acc, x -> x :: Nil, t)

    ///
    /// Returns `t` as an array.
    ///
    pub def toArray(rc: Region[r], t: t[a]): Array[a, r] \ r =
        let v = MutList.empty(rc);
        Reducible.foldLeft((_, a) -> MutList.push!(a, v), (), t);
        MutList.toArray(rc, v)

    ///
    /// Returns `t` as a vector.
    ///
    pub def toVector(t: t[a]): Vector[a] = region rc {
        let l = MutList.empty(rc);
        Reducible.foldLeft((_, a) -> MutList.push!(a, l), (), t);
        MutList.toVector(l)
    }

    ///
    /// Returns `t` as a list.
    ///
    pub def toList(t: t[a]): List[a] =
        Reducible.foldRight((a, acc) -> a :: acc, Nil, t)

    ///
    /// Returns `t` as a map.
    ///
    pub def toMap(t: t[(k, v)]): Map[k, v] with Order[k] =
        Reducible.foldLeft((acc, a) -> Map.insert(fst(a), snd(a), acc), Map.empty(), t)

    ///
    /// Returns `t` as a non-empty list.
    ///
    pub def toNel(t: t[a]): Nel[a] =
        Reducible.reduceLeftTo((acc, a) -> Nel.append(acc, Nel.Nel(a, Nil)), a -> Nel.Nel(a, Nil), t)

    ///
    /// Returns `t` as a set.
    ///
    pub def toSet(t: t[a]): Set[a] with Order[a] =
        Reducible.foldLeft((acc, a) -> Set.insert(a, acc), Set.empty(), t)

}

mod Reducible {

    ///
    /// Returns the number of elements in `t`.
    ///
    pub def size(t: t[a]): Int32 with Reducible[t] = Reducible.length(t)

}
