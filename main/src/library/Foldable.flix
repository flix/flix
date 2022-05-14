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
pub class Foldable[t : Type -> Type] {

    ///
    /// Left-associative fold of a structure.
    /// Applies `f` to a start value `s` and all elements in `t` going from left to right.
    ///
    pub def foldLeft(f: (b, a) -> b & ef, s: b, t: t[a]): b & ef

    ///
    /// Right-associative fold of a structure.
    /// Applies `f` to a start value `s` and all elements in `t` going from right to left.
    ///
    pub def foldRight(f: (a, b) -> b & ef, s: b, t: t[a]): b & ef

    ///
    /// Right-associative fold of a structure.
    /// Applies `f` to a start value `s` and all elements in `t` going from right to left.
    ///
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (a, Unit -> b & ef) -> b & ef, s: b, t: t[a]): b & ef

    ///
    /// Returns the number of elements in `t` that satisfy the predicate `f`.
    ///
    pub def count(f: a -> Bool, t: t[a]): Int32 =
        Foldable.foldLeft((acc, x) -> if (f(x)) acc + 1 else acc, 0, t)

    ///
    /// Returns the sum of all elements in `t`.
    ///
    pub def sum(t: t[Int32]): Int32 =
        Foldable.foldLeft((acc, x) -> acc + x, 0, t)

    ///
    /// Returns the sum of all elements in `t` according to the function `f`.
    ///
    pub def sumWith(f: a -> Int32 & ef, t: t[a]): Int32 & ef =
        Foldable.foldLeft((acc, x) -> acc + f(x), 0, t)

    ///
    /// Returns the product of all elements in `t`.
    ///
    pub def product(t: t[Int32]): Int32 =
        if (Foldable.isEmpty(t))
            0
        else
            Foldable.foldLeft((acc, x) -> acc * x, 1, t)

    ///
    /// Returns the product of all elements in `t` according to the function `f`.
    ///
    pub def productWith(f: a -> Int32 & ef, t: t[a]): Int32 & ef =
        if (Foldable.isEmpty(t))
            0
        else
            Foldable.foldLeft((acc, x) -> acc * f(x), 1, t)

    ///
    /// Returns `true` if and only if at least one element in `t` satisfies the predicate `f`.
    ///
    /// Returns `false` if `t` is empty.
    ///
    pub def exists(f: a -> Bool & ef, t: t[a]): Bool & ef =
        Foldable.foldRightWithCont((x, k) -> f(x) or k(), false, t)

    ///
    /// Returns `true` if and only if all elements in `t` satisfy the predicate `f`.
    ///
    /// Returns `true` if `t` is empty.
    ///
    pub def forall(f: a -> Bool & ef, t: t[a]): Bool & ef =
        Foldable.foldRightWithCont((x, k) -> f(x) and k(), true, t)

    ///
    /// Optionally returns the first element of `t`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def head(t: t[a]): Option[a] =
        Foldable.foldRightWithCont((x, _) -> Some(x), None, t)

    ///
    /// Optionally returns the last element of `t`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def last(t: t[a]): Option[a] =
        Foldable.foldRightWithCont((x, k) -> Option.withDefault(default = Some(x), k()), None, t)

    ///
    /// Optionally returns the first element of `t` that satisfies the predicate `f` when searching from left to right.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def findLeft(f: a -> Bool & ef, t: t[a]): Option[a] & ef =
        Foldable.foldRightWithCont((x, k) -> if (f(x)) Some(x) else k(), None, t)

    ///
    /// Optionally returns the first element of `t` that satisfies the predicate `f` when searching from right to left.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def findRight(f: a -> Bool, t: t[a]): Option[a] =
        Foldable.foldRight((x, acc) -> Option.withDefault(default = if (f(x)) Some(x) else None, acc), None, t)

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap(f: a -> b & ef, t: t[a]): b & ef with Monoid[b] =
        Foldable.foldLeft((acc, x) -> Monoid.combine(f(x), acc), Monoid.empty(), t)

    ///
    /// Returns true if and only if `t` is empty.
    ///
    pub def isEmpty(t: t[a]): Bool =
      Foldable.foldRightWithCont((_, _) -> false, true, t)

    ///
    /// Returns `true` if and only if the element `x` is in `t`.
    ///
    pub def memberOf(x: a, t: t[a]): Bool with Eq[a] =
        Foldable.foldRightWithCont((y, k) -> x == y or k(), false, t)

    ///
    /// Optionally finds the smallest element of `t` according to the `Order` on `a`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def minimum(t: t[a]): Option[a] with Order[a] =
        Foldable.reduceLeft(Order.min, t)

    ///
    /// Optionally finds the smallest element of `t` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def minimumBy(cmp: (a, a) -> Comparison, t: t[a]): Option[a] =
        Foldable.reduceLeft(Order.minBy(cmp), t)

    ///
    /// Optionally finds the largest element of `t` according to the `Order` on `a`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def maximum(t: t[a]): Option[a] with Order[a] =
        Foldable.reduceLeft(Order.max, t)

    ///
    /// Optionally finds the largest element of `t` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def maximumBy(cmp: (a, a) -> Comparison, t: t[a]): Option[a] =
        Foldable.reduceLeft(Order.maxBy(cmp), t)

    ///
    /// Optionally applies `f` to all elements in `t` going from left to right until a single value is obtained.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def reduceLeft(f: (a, a) -> a & ef, t: t[a]): Option[a] & ef =
        Foldable.foldLeft((acc, x) -> Some(Option.foldLeft((z, v) -> f(v, z), x, acc)), None, t)

    ///
    /// Optionally applies `f` to all elements in `t` going from right to left until a single value is obtained.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def reduceRight(f: (a, a) -> a & ef, t: t[a]): Option[a] & ef =
        Foldable.foldRight((x, acc) -> Some(Option.foldLeft((v, z) -> f(v, z), x, acc)), None, t)

    ///
    /// Returns `t` as an immutable list.
    ///
    pub def toList(t: t[a]): List[a] =
        Foldable.foldRight((x, acc) -> x :: acc, Nil, t)

    ///
    /// Returns `t` as an array.
    ///
    pub def toArray(t: t[a]): Array[a] & Impure =
        let v = new MutList(static);
        Foldable.foreach(a -> MutList.push!(a, v), t);
        MutList.toArray(v)

    ///
    /// Returns an iterator over `t`.
    ///
    pub def toIterator(t: t[a]): Iterator[a] & Impure =
        let arr = Foldable.toArray(t);
        let len = Array.length(arr);
        let i = ref 0;
        let done = () -> (deref i) >= len;
        let next = () -> {
            let j = deref i;
            let x = arr[j];
            i := j + 1;
            x
        };
        Iterator(done, next)

    ///
    /// Returns `t` as a mutable list.
    ///
    pub def toMutList(t: t[a]): MutList[a, Impure] & Impure =
        let v = new MutList(static);
        Foldable.foldRight((x, _) -> MutList.push!(x, v), (), t);
        v

    ///
    /// Returns `t` as a set.
    ///
    pub def toSet(t: t[a]): Set[a] with Order[a] =
        Foldable.foldRight((x, acc) -> Set.insert(x, acc), Set.empty(), t)

    ///
    /// Returns `t` as a map.
    ///
    pub def toMap(t: t[(k, v)]): Map[k, v] with Order[k] =
        Foldable.foldRight((x, acc) -> let (k, v) = x; Map.insert(k, v, acc), Map.empty(), t)

    ///
    /// Returns a map with elements of `s` as keys and `f` applied as values.
    ///
    pub def toMapWith(f: a -> b, s: t[a]): Map[a, b] with Order[a] =
            Foldable.foldRight((x, acc) -> Map.insert(x, f(x), acc), Map.empty(), s)

    ///
    /// Returns `t` without the longest prefix that satisfies the predicate `f`.
    ///
    /// Returns an immutable list.
    ///
    pub def dropWhile(f: a -> Bool, t: t[a]): List[a] =
        Foldable.foldLeft((acc, x) -> {
                let (c, tail) = acc;
                if (c and f(x)) (true, tail)
                else (false, x :: tail)
            }, (true, Nil), t) |> snd |> List.reverse

    ///
    /// Returns the longest prefix of `t` that satisfies the predicate `f`.
    ///
    /// Returns an immutable list.
    ///
    pub def takeWhile(f: a -> Bool, t: t[a]): List[a] =
        Foldable.foldRight((x, acc) -> if (f(x)) x :: acc else Nil, Nil, t)

    ///
    /// Returns an immutable list of all the elements in `t` that satisfy the predicate `f`.
    ///
    pub def filter(f: a -> Bool, t: t[a]): List[a] =
        Foldable.foldRight((x, acc) -> if (f(x)) x :: acc else acc, Nil, t)

    ///
    /// Applies `f` to each element in `t`.
    ///
    pub def foreach(f: a -> Unit & ef, t: t[a]): Unit & ef =
        Foldable.foldLeft((_, x) -> f(x), (), t)


    // Acknowledgement: `foldLeftM` and `foldRightM` are derived from Haskell's Data.Foldable.
    // Because they use CPS, the implementations work for both strict and lazy languages.
    // Counterintuitively `foldLeftM` uses a right-fold and `foldRighM` uses a left-fold
    // but the "direction of travel" is observably correct.

    ///
    /// A monadic version of `foldLeft`.
    ///
    /// Applies the monadic `f` to a start value `s` and all elements in `t` going
    /// from left to right.
    ///
    pub def foldLeftM(f: (b, a) -> m[b] & ef, s: b, t: t[a]): m[b] & ef with Monad[m] =
        let f1 = (x, acc) -> z -> Monad.flatMap(acc, f(z, x));
        s |> Foldable.foldRight(f1, x1 -> Applicative.point(x1) as & ef, t)

    ///
    /// A monadic version of `foldRight`.
    ///
    /// Applies the monadic `f` to a start value `s` and all elements in `t` going
    /// from right to left.
    ///
    pub def foldRightM(f: (a, b) -> m[b] & ef, s: b, t: t[a]): m[b] & ef with Monad[m] =
        let f1 = (k, x) -> z -> Monad.flatMap(k, f(x, z));
        s |> Foldable.foldLeft(f1, x1 -> Applicative.point(x1) as & ef, t)

    ///
    /// A monadic version of `foreach`.
    ///
    /// Apply `f` to every value in `t`. `f` is applied for its monadic effect,
    /// the answer it produces is discarded.
    ///
    pub def foreachM(f: a -> m[b] & ef, t: t[a]): m[Unit] & ef with Monad[m] =
        use Applicative.{point, *>};
        Foldable.foldLeftM((acc, a) -> f(a) *> point(acc), (), t)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `t` with `sep` inserted between each element.
    ///
    pub def join(sep: String, t: t[a]): String with ToString[a] = {
        use StringBuilder.append!;
        let lastSep = String.length(sep);
        let sb = StringBuilder.new();
        t |> Foldable.foreach(x -> { append!(ToString.toString(x), sb); append!(sep, sb) });
        StringBuilder.toString(sb) |> String.dropRight(lastSep)
    } as & Pure

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `t` according to `f` with `sep` inserted between each element.
    ///
    pub def joinWith(f: a -> String & ef, sep: String, t: t[a]): String & ef = {
        use StringBuilder.append!;
        let lastSep = String.length(sep);
        let sb = StringBuilder.new();
        t |> Foldable.foreach(x -> { append!(f(x), sb); append!(sep, sb) });
        StringBuilder.toString(sb) |> String.dropRight(lastSep)
    } as & ef

}

namespace Foldable {

    use Applicative.{*>};

    ///
    /// Returns the result of applying `combine` to all the elements in `t`, using `empty` as the initial value.
    ///
    pub def fold[t: Type -> Type, a: Type](t: t[a]): a with Foldable[t], Monoid[a] =
        Foldable.foldLeft((acc, x) -> Monoid.combine(acc, x), Monoid.empty(), t)

}
