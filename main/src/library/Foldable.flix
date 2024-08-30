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
/// A trait for data structures that can be folded.
///
pub trait Foldable[t : Type -> Type] {
    ///
    /// The associated effect of the Foldable which represents the effect of accessing its elements.
    ///
    type Aef[t]: Eff = {}

    ///
    /// Left-associative fold of a structure.
    /// Applies `f` to a start value `s` and all elements in `t` going from left to right.
    ///
    pub def foldLeft(f: (b, a) -> b \ ef, s: b, t: t[a]): b \ (ef + Foldable.Aef[t])

    ///
    /// Right-associative fold of a structure.
    /// Applies `f` to a start value `s` and all elements in `t` going from right to left.
    ///
    pub def foldRight(f: (a, b) -> b \ ef, s: b, t: t[a]): b \ (ef + Foldable.Aef[t])

    ///
    /// Right-associative fold of a structure.
    /// Applies `f` to a start value `s` and all elements in `t` going from right to left.
    ///
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, s: b, t: t[a]): b \ (ef + Foldable.Aef[t])

    ///
    /// Returns the number of elements in `t`.
    ///
    pub def length(t: t[a]): Int32 \ Foldable.Aef[t] =
        Foldable.foldLeft((acc, _) -> acc + 1, 0, t)

    ///
    /// Returns the number of elements in `t` that satisfy the predicate `f`.
    ///
    pub def count(f: a -> Bool \ ef, t: t[a]): Int32 \ (ef + Foldable.Aef[t]) =
        Foldable.foldLeft((acc, x) -> if (f(x)) acc + 1 else acc, 0, t)

    ///
    /// Returns the sum of all elements in `t`.
    ///
    pub def sum(t: t[Int32]): Int32 \ Foldable.Aef[t] =
        Foldable.foldLeft((acc, x) -> acc + x, 0, t)

    ///
    /// Returns the sum of all elements in `t` according to the function `f`.
    ///
    pub def sumWith(f: a -> Int32 \ ef, t: t[a]): Int32 \ (ef + Foldable.Aef[t]) =
        Foldable.foldLeft((acc, x) -> acc + f(x), 0, t)

    ///
    /// Returns `true` if and only if at least one element in `t` satisfies the predicate `f`.
    ///
    /// Returns `false` if `t` is empty.
    ///
    pub def exists(f: a -> Bool \ ef, t: t[a]): Bool \ (ef + Foldable.Aef[t]) =
        Foldable.foldRightWithCont((x, k) -> f(x) or k(), false, t)

    ///
    /// Returns `true` if and only if all elements in `t` satisfy the predicate `f`.
    ///
    /// Returns `true` if `t` is empty.
    ///
    pub def forAll(f: a -> Bool \ ef, t: t[a]): Bool \ (ef + Foldable.Aef[t]) =
        Foldable.foldRightWithCont((x, k) -> f(x) and k(), true, t)

    ///
    /// Optionally returns the first element of `t`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def head(t: t[a]): Option[a] \ Foldable.Aef[t] =
        Foldable.foldRightWithCont((x, _) -> Some(x), None, t)

    ///
    /// Optionally returns the last element of `t`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def last(t: t[a]): Option[a] \ Foldable.Aef[t] =
        Foldable.foldRightWithCont((x, k) -> Option.withDefault(default = Some(x), k()), None, t)

    ///
    /// Optionally returns the first element of `t` that satisfies the predicate `f` when searching from left to right.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def findLeft(f: a -> Bool \ ef, t: t[a]): Option[a] \ (ef + Foldable.Aef[t]) =
        Foldable.foldRightWithCont((x, k) -> if (f(x)) Some(x) else k(), None, t)

    ///
    /// Optionally returns the first element of `t` that satisfies the predicate `f` when searching from right to left.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def findRight(f: a -> Bool, t: t[a]): Option[a] \ Foldable.Aef[t] =
        Foldable.foldRight((x, acc) -> Option.withDefault(default = if (f(x)) Some(x) else None, acc), None, t)

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap(f: a -> b \ ef, t: t[a]): b \ (ef + Foldable.Aef[t]) with Monoid[b] =
        Foldable.foldLeft((acc, x) -> Monoid.combine(acc, f(x)), Monoid.empty(), t)

    ///
    /// Returns true if and only if `t` is empty.
    ///
    pub def isEmpty(t: t[a]): Bool \ Foldable.Aef[t] =
      Foldable.foldRightWithCont((_, _) -> false, true, t)

    ///
    /// Returns true if and only if `t` is non-empty.
    ///
    pub def nonEmpty(t: t[a]): Bool \ Foldable.Aef[t] = not Foldable.isEmpty(t)

    ///
    /// Returns `true` if and only if the element `x` is in `t`.
    ///
    pub def memberOf(x: a, t: t[a]): Bool \ Foldable.Aef[t] with Eq[a] =
        Foldable.foldRightWithCont((y, k) -> x == y or k(), false, t)

    ///
    /// Optionally finds the smallest element of `t` according to the `Order` on `a`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def minimum(t: t[a]): Option[a] \ Foldable.Aef[t] with Order[a] =
        Foldable.reduceLeft(Order.min, t)

    ///
    /// Optionally finds the smallest element of `t` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def minimumBy(cmp: (a, a) -> Comparison, t: t[a]): Option[a] \ Foldable.Aef[t] =
        Foldable.reduceLeft(Order.minBy(cmp), t)

    ///
    /// Optionally finds the largest element of `t` according to the `Order` on `a`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def maximum(t: t[a]): Option[a] \ Foldable.Aef[t] with Order[a] =
        Foldable.reduceLeft(Order.max, t)

    ///
    /// Optionally finds the largest element of `t` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def maximumBy(cmp: (a, a) -> Comparison, t: t[a]): Option[a] \ Foldable.Aef[t] =
        Foldable.reduceLeft(Order.maxBy(cmp), t)

    ///
    /// Optionally applies `f` to all elements in `t` going from left to right until a single value is obtained.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def reduceLeft(f: (a, a) -> a \ ef, t: t[a]): Option[a] \ (ef + Foldable.Aef[t]) =
        Foldable.foldLeft((acc, x) -> Some(Option.foldLeft((z, v) -> f(v, z), x, acc)), None, t)

    ///
    /// Optionally applies `f` to all elements in `t` going from right to left until a single value is obtained.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def reduceRight(f: (a, a) -> a \ ef, t: t[a]): Option[a] \ (ef + Foldable.Aef[t]) =
        Foldable.foldRight((x, acc) -> Some(Option.foldLeft((v, z) -> f(v, z), x, acc)), None, t)

    ///
    /// Returns `t` as a chain.
    ///
    pub def toChain(t: t[a]): Chain[a] \ Foldable.Aef[t] =
        Foldable.foldRight((x, acc) -> Chain.cons(x, acc), Chain.empty(), t)

    ///
    /// Returns `t` as a `DelayList`.
    ///
    pub def toDelayList(t: t[a]): DelayList[a] \ Foldable.Aef[t] =
        Foldable.foldRight((x, acc) -> DelayList.ECons(x, acc), DelayList.ENil, t)

    ///
    /// Returns `t` as an immutable list.
    ///
    pub def toList(t: t[a]): List[a] \ Foldable.Aef[t] =
        Foldable.foldRight((x, acc) -> x :: acc, Nil, t)

    ///
    /// Returns `t` as an array.
    ///
    pub def toArray(rc: Region[r], t: t[a]): Array[a, r] \ (r + Foldable.Aef[t]) = region rc2 {
        let l = MutList.empty(rc2);
        Foldable.forEach(a -> MutList.push!(a, l), t);
        MutList.toArray(rc, l)
    }

    ///
    /// Returns `t` as a vector.
    ///
    pub def toVector(t: t[a]): Vector[a] \ Foldable.Aef[t] = region rc {
        let l = MutList.empty(rc);
        Foldable.forEach(a -> MutList.push!(a, l), t);
        MutList.toVector(l)
    }

    ///
    /// Returns `t` as a `MutDeque`.
    ///
    pub def toMutDeque(rc: Region[r], t: t[a]): MutDeque[a, r] \ (r + Foldable.Aef[t]) =
        let d = MutDeque.empty(rc);
        Foldable.forEach(x -> MutDeque.pushBack(x, d), t);
        d

    ///
    /// Returns `t` as a mutable list.
    ///
    pub def toMutList(rc: Region[r], t: t[a]): MutList[a, r] \ (r + Foldable.Aef[t]) = region rc2 {
        Array.toMutList(rc, Foldable.toArray(rc2, t))
    }

    ///
    /// Returns `t` as a set.
    ///
    pub def toSet(t: t[a]): Set[a] \ Foldable.Aef[t] with Order[a] =
        Foldable.foldRight((x, acc) -> Set.insert(x, acc), Set.empty(), t)

    ///
    /// Returns the set `s` as a `MutSet`.
    ///
    pub def toMutSet(rc: Region[r], t: t[a]): MutSet[a, r] \ (r + Foldable.Aef[t]) with Order[a] =
        let s = MutSet.empty(rc);
        Foldable.forEach(x -> MutSet.add!(x, s), t);
        s

    ///
    /// Returns `t` as a map.
    ///
    pub def toMap(t: t[(k, v)]): Map[k, v] \ Foldable.Aef[t] with Order[k] =
        Foldable.foldRight((x, acc) -> let (k, v) = x; Map.insert(k, v, acc), Map.empty(), t)

    ///
    /// Returns a map with elements of `s` as keys and `f` applied as values.
    ///
    pub def toMapWith(f: a -> b, s: t[a]): Map[a, b] \ Foldable.Aef[t] with Order[a] =
        Foldable.foldRight((x, acc) -> Map.insert(x, f(x), acc), Map.empty(), s)

    ///
    /// Returns `t` as a `DelayMap`
    ///
    pub def toDelayMap(t: t[(k, v)]): DelayMap[k, v] \ Foldable.Aef[t] with Order[k] =
        Foldable.foldRight((x, acc) -> let (k, v) = x; DelayMap.insert(k, v, acc), DelayMap.empty(), t)

    ///
    /// Returns `t` as a `MutMap`.
    ///
    pub def toMutMap(rc: Region[r], t: t[(k, v)]): MutMap[k, v, r] \ (r + Foldable.Aef[t]) with Order[k] =
        let m = MutMap.empty(rc);
        Foldable.forEach(x -> let (k, v) = x; MutMap.put!(k, v, m), t);
        m

    ///
    /// Optionally returns `t` as a non empty chain.
    ///
    pub def toNec(t: t[a]): Option[Nec[a]] \ Foldable.Aef[t] =
        Foldable.toChain(t) |> Chain.toNec

    ///
    /// Optionally returns `t` as a non empty list.
    ///
    pub def toNel(t: t[a]): Option[Nel[a]] \ Foldable.Aef[t] =
        Foldable.toList(t) |> List.toNel

    ///
    /// Returns `t` without the longest prefix that satisfies the predicate `f`.
    ///
    /// Returns an immutable list.
    ///
    pub def dropWhile(f: a -> Bool, t: t[a]): List[a] \ Foldable.Aef[t] =
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
    pub def takeWhile(f: a -> Bool, t: t[a]): List[a] \ Foldable.Aef[t] =
        Foldable.foldRight((x, acc) -> if (f(x)) x :: acc else Nil, Nil, t)

    ///
    /// Returns an immutable list of all the elements in `t` that satisfy the predicate `f`.
    ///
    pub def filter(f: a -> Bool, t: t[a]): List[a] \ Foldable.Aef[t] =
        Foldable.foldRight((x, acc) -> if (f(x)) x :: acc else acc, Nil, t)

    ///
    /// Applies `f` to each element in `t`.
    ///
    pub def forEach(f: a -> Unit \ ef, t: t[a]): Unit \ (ef + Foldable.Aef[t]) =
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
    pub def foldLeftM(f: (b, a) -> m[b] \ ef, s: b, t: t[a]): m[b] \ (ef + Foldable.Aef[t]) with Monad[m] =
        let f1 = (x, acc) -> z -> Monad.flatMap(acc, f(z, x));
        s |> Foldable.foldRight(f1, x1 -> checked_ecast(Applicative.point(x1)), t)

    ///
    /// A monadic version of `foldRight`.
    ///
    /// Applies the monadic `f` to a start value `s` and all elements in `t` going
    /// from right to left.
    ///
    pub def foldRightM(f: (a, b) -> m[b] \ ef, s: b, t: t[a]): m[b] \ (ef + Foldable.Aef[t]) with Monad[m] =
        let f1 = (k, x) -> z -> Monad.flatMap(k, f(x, z));
        s |> Foldable.foldLeft(f1, x1 -> checked_ecast(Applicative.point(x1)), t)

    ///
    /// A monadic version of `forEach`.
    ///
    /// Apply `f` to every value in `t`. `f` is applied for its monadic effect,
    /// the answer it produces is discarded.
    ///
    pub def forEachM(f: a -> m[b] \ ef, t: t[a]): m[Unit] \ (ef + Foldable.Aef[t]) with Monad[m] =
        use Applicative.{point, *>};
        Foldable.foldLeftM((acc, a) -> f(a) *> point(acc), (), t)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `t` with `sep` inserted between each element.
    ///
    pub def join(sep: String, t: t[a]): String \ Foldable.Aef[t] with ToString[a] =
        Foldable.joinWith(ToString.toString, sep, t)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `t` according to `f` with `sep` inserted between each element.
    ///
    pub def joinWith(f: a -> String \ ef, sep: String, t: t[a]): String \ (ef + Foldable.Aef[t]) = region rc {
        use StringBuilder.append!;
        let lastSep = String.length(sep);
        let sb = StringBuilder.empty(rc);
        t |> Foldable.forEach(x -> { append!(f(x), sb); append!(sep, sb) });
        StringBuilder.toString(sb) |> String.dropRight(lastSep)
    }

}

mod Foldable {

    use Applicative.{*>}

    ///
    /// Returns the result of applying `combine` to all the elements in `t`, using `empty` as the initial value.
    ///
    pub def fold(t: t[a]): a \ Foldable.Aef[t] with Foldable[t], Monoid[a] =
        Foldable.foldLeft((acc, x) -> Monoid.combine(acc, x), Monoid.empty(), t)

    ///
    /// Returns the number of elements in `t`.
    ///
    pub def size(t: t[a]): Int32 \ Foldable.Aef[t] with Foldable[t] = Foldable.length(t)

}
