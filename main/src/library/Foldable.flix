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
pub trait Foldable[t : Type] {
    type Elm: Type
    type Aef: Eff

    ///
    /// Left-associative fold of a structure.
    /// Applies `f` to a start value `s` and all elements in `t` going from left to right.
    ///
    pub def foldLeft(f: (b, Foldable.Elm[t]) -> b \ ef, s: b, t: t): b \ Foldable.Aef[t] + ef

    ///
    /// Right-associative fold of a structure.
    /// Applies `f` to a start value `s` and all elements in `t` going from right to left.
    ///
    pub def foldRight(f: (Foldable.Elm[t], b) -> b \ ef, s: b, t: t): b \ Foldable.Aef[t] + ef

    ///
    /// Right-associative fold of a structure.
    /// Applies `f` to a start value `s` and all elements in `t` going from right to left.
    ///
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (Foldable.Elm[t], Unit -> b \ Foldable.Aef[t] + ef) -> b \ Foldable.Aef[t] + ef, s: b, t: t): b \ Foldable.Aef[t] + ef

    ///
    /// Returns the length of `t`, i.e. the number of times it can be folded over.
    ///
    pub def length(t: t): Int32 \ Foldable.Aef[t] =
        Foldable.foldLeft((acc, _) -> acc + 1, 0, t)

    ///
    /// Returns the number of elements in `t` that satisfy the predicate `f`.
    ///
    pub def count(f: Foldable.Elm[t] -> Bool \ ef, t: t): Int32 \ Foldable.Aef[t] + ef =
        Foldable.foldLeft((acc, x) -> if (f(x)) acc + 1 else acc, 0, t)

    ///
    /// Returns the sum of all elements in `t`.
    ///
    pub def sum(t: t): Int32 \ Foldable.Aef[t] where Foldable.Elm[t] ~ Int32 =
        Foldable.foldLeft((acc, x) -> acc + x, 0, t)

    ///
    /// Returns the sum of all elements in `t` according to the function `f`.
    ///
    pub def sumWith(f: Foldable.Elm[t] -> Int32 \ ef, t: t): Int32 \ Foldable.Aef[t] + ef =
        Foldable.foldLeft((acc, x) -> acc + f(x), 0, t)

    ///
    /// Returns `true` if and only if at least one element in `t` satisfies the predicate `f`.
    ///
    /// Returns `false` if `t` is empty.
    ///
    pub def exists(f: Foldable.Elm[t] -> Bool \ ef, t: t): Bool \ Foldable.Aef[t] + ef =
        Foldable.foldRightWithCont((x, k) -> f(x) or k(), false, t)

    ///
    /// Returns `true` if and only if all elements in `t` satisfy the predicate `f`.
    ///
    /// Returns `true` if `t` is empty.
    ///
    pub def forAll(f: Foldable.Elm[t] -> Bool \ ef, t: t): Bool \ Foldable.Aef[t] + ef =
        Foldable.foldRightWithCont((x, k) -> f(x) and k(), true, t)

    ///
    /// Optionally returns the first element of `t`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def head(t: t): Option[Foldable.Elm[t]] \ Foldable.Aef[t] =
        Foldable.foldRightWithCont((x, _) -> checked_ecast(Some(x)), None, t)

    ///
    /// Optionally returns the last element of `t`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def last(t: t): Option[Foldable.Elm[t]] \ Foldable.Aef[t] =
        Foldable.foldRightWithCont((x, k) -> Option.withDefault(default = Some(x), k()), None, t)

    ///
    /// Optionally returns the first element of `t` that satisfies the predicate `f` when searching from left to right.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def findLeft(f: Foldable.Elm[t] -> Bool \ ef, t: t): Option[Foldable.Elm[t]] \ Foldable.Aef[t] + ef =
        Foldable.foldRightWithCont((x, k) -> if (f(x)) Some(x) else k(), None, t)

    ///
    /// Optionally returns the first element of `t` that satisfies the predicate `f` when searching from right to left.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def findRight(f: Foldable.Elm[t] -> Bool, t: t): Option[Foldable.Elm[t]] \ Foldable.Aef[t] =
        Foldable.foldRight((x, acc) -> Option.withDefault(default = if (f(x)) Some(x) else None, acc), None, t)

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap(f: Foldable.Elm[t] -> b \ ef, t: t): b \ Foldable.Aef[t] + ef with Monoid[b] =
        Foldable.foldLeft((acc, x) -> Monoid.combine(acc, f(x)), Monoid.empty(), t)

    ///
    /// Returns true if and only if `t` is empty.
    ///
    pub def isEmpty(t: t): Bool \ Foldable.Aef[t] =
      Foldable.foldRightWithCont((_, _) -> checked_ecast(false), true, t)

    ///
    /// Returns `true` if and only if the element `x` is in `t`.
    ///
    pub def memberOf(x: Foldable.Elm[t], t: t): Bool \ Foldable.Aef[t] with Eq[a] where Foldable.Elm[t] ~ a =
        Foldable.foldRightWithCont((y, k) -> x == y or k(), false, t)

    ///
    /// Optionally finds the smallest element of `t` according to the `Order` on `a`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def minimum(t: t): Option[Foldable.Elm[t]] \ Foldable.Aef[t] with Order[a] where Foldable.Elm[t] ~ a =
        Foldable.reduceLeft(Order.min, t)

    ///
    /// Optionally finds the smallest element of `t` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def minimumBy(cmp: (Foldable.Elm[t], Foldable.Elm[t]) -> Comparison, t: t): Option[Foldable.Elm[t]] \ Foldable.Aef[t] =
        Foldable.reduceLeft(Order.minBy(cmp), t)

    ///
    /// Optionally finds the largest element of `t` according to the `Order` on `a`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def maximum(t: t): Option[Foldable.Elm[t]] \ Foldable.Aef[t] with Order[a] where Foldable.Elm[t] ~ a =
        Foldable.reduceLeft(Order.max, t)

    ///
    /// Optionally finds the largest element of `t` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def maximumBy(cmp: (Foldable.Elm[t], Foldable.Elm[t]) -> Comparison, t: t): Option[Foldable.Elm[t]] \ Foldable.Aef[t] =
        Foldable.reduceLeft(Order.maxBy(cmp), t)

    ///
    /// Optionally applies `f` to all elements in `t` going from left to right until a single value is obtained.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def reduceLeft(f: (Foldable.Elm[t], Foldable.Elm[t]) -> Foldable.Elm[t] \ ef, t: t): Option[Foldable.Elm[t]] \ Foldable.Aef[t] + ef =
        Foldable.foldLeft((acc, x) -> Some(Option.foldLeft((z, v) -> f(v, z), x, acc)), None, t)

    ///
    /// Optionally applies `f` to all elements in `t` going from right to left until a single value is obtained.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def reduceRight(f: (Foldable.Elm[t], Foldable.Elm[t]) -> Foldable.Elm[t] \ ef, t: t): Option[Foldable.Elm[t]] \ Foldable.Aef[t] + ef =
        Foldable.foldRight((x, acc) -> Some(Option.foldLeft((v, z) -> f(v, z), x, acc)), None, t)

    ///
    /// Returns `t` as a chain.
    ///
    pub def toChain(t: t): Chain[Foldable.Elm[t]] \ Foldable.Aef[t] =
        Foldable.foldRight((x, acc) -> Chain.cons(x, acc), Chain.empty(), t)

    ///
    /// Returns `t` as a `DelayList`.
    ///
    pub def toDelayList(t: t): DelayList[Foldable.Elm[t]] \ Foldable.Aef[t] =
        Foldable.foldRight((x, acc) -> DelayList.ECons(x, acc), DelayList.ENil, t)

    ///
    /// Returns `t` as an immutable list.
    ///
    pub def toList(t: t): List[Foldable.Elm[t]] \ Foldable.Aef[t] =
        Foldable.foldRight((x, acc) -> x :: acc, Nil, t)

    ///
    /// Returns `t` as an array.
    ///
    pub def toArray(rc: Region[r], t: t): Array[Foldable.Elm[t], r] \ Foldable.Aef[t] + r = region rc2 {
        let l = MutList.new(rc2);
        Foldable.forEach(a -> MutList.push!(a, l), t);
        MutList.toArray(rc, l)
    }

    ///
    /// Returns `t` as a vector.
    ///
    pub def toVector(t: t): Vector[Foldable.Elm[t]] \ Foldable.Aef[t] = region rc {
        let l = MutList.new(rc);
        Foldable.forEach(a -> MutList.push!(a, l), t);
        MutList.toVector(l)
    }

    ///
    /// Returns `t` as a `MutDeque`.
    ///
    pub def toMutDeque(rc: Region[r], t: t): MutDeque[Foldable.Elm[t], r] \ Foldable.Aef[t] + r =
        let d = MutDeque.new(rc);
        Foldable.forEach(x -> MutDeque.pushBack(x, d), t);
        d

    ///
    /// Returns `t` as a mutable list.
    ///
    pub def toMutList(rc: Region[r], t: t): MutList[Foldable.Elm[t], r] \ Foldable.Aef[t] + r = region rc2 {
        Array.toMutList(rc, Foldable.toArray(rc2, t))
    }

    ///
    /// Returns `t` as a set.
    ///
    pub def toSet(t: t): Set[Foldable.Elm[t]] \ Foldable.Aef[t] with Order[a] where Foldable.Elm[t] ~ a =
        Foldable.foldRight((x, acc) -> Set.insert(x, acc), Set.empty(), t)

    ///
    /// Returns the set `s` as a `MutSet`.
    ///
    pub def toMutSet(rc: Region[r], t: t): MutSet[Foldable.Elm[t], r] \ Foldable.Aef[t] + r with Order[a] where Foldable.Elm[t] ~ a =
        let s = MutSet.new(rc);
        Foldable.forEach(x -> MutSet.add!(x, s), t);
        s

    ///
    /// Returns `t` as a map.
    ///
    pub def toMap(t: t): Map[k, v] \ Foldable.Aef[t] with Order[k] where Foldable.Elm[t] ~ (k, v) =
        Foldable.foldRight((x, acc) -> let (k, v) = x; Map.insert(k, v, acc), Map.empty(), t)

    ///
    /// Returns a map with elements of `s` as keys and `f` applied as values.
    ///
    pub def toMapWith(f: Foldable.Elm[t] -> b, s: t): Map[Foldable.Elm[t], b] \ Foldable.Aef[t] with Order[a] where Foldable.Elm[t] ~ a =
        Foldable.foldRight((x, acc) -> Map.insert(x, f(x), acc), Map.empty(), s)

    ///
    /// Returns `t` as a `DelayMap`
    ///
    pub def toDelayMap(t: t): DelayMap[k, v] \ Foldable.Aef[t] with Order[k] where Foldable.Elm[t] ~ (k, v) =
        Foldable.foldRight((x, acc) -> let (k, v) = x; DelayMap.insert(k, v, acc), DelayMap.empty(), t)

    ///
    /// Returns `t` as a `MutMap`.
    ///
    pub def toMutMap(rc: Region[r], t: t): MutMap[k, v, r] \ Foldable.Aef[t] + r with Order[k] where Foldable.Elm[t] ~ (k, v) =
        let m = MutMap.new(rc);
        Foldable.forEach(x -> let (k, v) = x; MutMap.put!(k, v, m), t);
        m

    ///
    /// Optionally returns `t` as a non empty chain.
    ///
    pub def toNec(t: t): Option[Nec[Foldable.Elm[t]]] \ Foldable.Aef[t] =
        Foldable.toChain(t) |> Chain.toNec

    ///
    /// Optionally returns `t` as a non empty list.
    ///
    pub def toNel(t: t): Option[Nel[Foldable.Elm[t]]] \ Foldable.Aef[t] =
        Foldable.toList(t) |> List.toNel

    ///
    /// Returns `t` without the longest prefix that satisfies the predicate `f`.
    ///
    /// Returns an immutable list.
    ///
    pub def dropWhile(f: Foldable.Elm[t] -> Bool, t: t): List[Foldable.Elm[t]] \ Foldable.Aef[t] =
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
    pub def takeWhile(f: Foldable.Elm[t] -> Bool, t: t): List[Foldable.Elm[t]] \ Foldable.Aef[t] =
        Foldable.foldRight((x, acc) -> if (f(x)) x :: acc else Nil, Nil, t)

    ///
    /// Returns an immutable list of all the elements in `t` that satisfy the predicate `f`.
    ///
    pub def filter(f: Foldable.Elm[t] -> Bool, t: t): List[Foldable.Elm[t]] \ Foldable.Aef[t] =
        Foldable.foldRight((x, acc) -> if (f(x)) x :: acc else acc, Nil, t)

    ///
    /// Applies `f` to each element in `t`.
    ///
    pub def forEach(f: Foldable.Elm[t] -> Unit \ ef, t: t): Unit \ Foldable.Aef[t] + ef =
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
    pub def foldLeftM(f: (b, Foldable.Elm[t]) -> m[b] \ ef, s: b, t: t): m[b] \ Foldable.Aef[t] + ef with Monad[m] =
        let f1 = (x, acc) -> z -> Monad.flatMap(acc, f(z, x));
        s |> Foldable.foldRight(f1, x1 -> checked_ecast(Applicative.point(x1)), t)

    ///
    /// A monadic version of `foldRight`.
    ///
    /// Applies the monadic `f` to a start value `s` and all elements in `t` going
    /// from right to left.
    ///
    pub def foldRightM(f: (Foldable.Elm[t], b) -> m[b] \ ef, s: b, t: t): m[b] \ Foldable.Aef[t] + ef with Monad[m] =
        let f1 = (k, x) -> z -> Monad.flatMap(k, f(x, z));
        s |> Foldable.foldLeft(f1, x1 -> checked_ecast(Applicative.point(x1)), t)

    ///
    /// A monadic version of `forEach`.
    ///
    /// Apply `f` to every value in `t`. `f` is applied for its monadic effect,
    /// the answer it produces is discarded.
    ///
    pub def forEachM(f: Foldable.Elm[t] -> m[b] \ ef, t: t): m[Unit] \ Foldable.Aef[t] + ef with Monad[m] =
        use Applicative.{point, *>};
        Foldable.foldLeftM((acc, a) -> f(a) *> point(acc), (), t)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `t` with `sep` inserted between each element.
    ///
    pub def join[a: Type](sep: String, t: t): String \ Foldable.Aef[t] with ToString[a] where Foldable.Elm[t] ~ a =
        Foldable.joinWith(ToString.toString, sep, t)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `t` according to `f` with `sep` inserted between each element.
    ///
    pub def joinWith(f: Foldable.Elm[t] -> String \ ef, sep: String, t: t): String \ Foldable.Aef[t] + ef = region rc {
        use StringBuilder.append!;
        let lastSep = String.length(sep);
        let sb = StringBuilder.new(rc);
        t |> Foldable.forEach(x -> { append!(f(x), sb); append!(sep, sb) });
        StringBuilder.toString(sb) |> String.dropRight(lastSep)
    }

}

mod Foldable {

    use Applicative.{*>}

    ///
    /// Returns the result of applying `combine` to all the elements in `t`, using `empty` as the initial value.
    ///
    pub def fold(t: t): Foldable.Elm[t] \ Foldable.Aef[t] with Foldable[t], Monoid[a] where Foldable.Elm[t] ~ a =
        Foldable.foldLeft((acc, x) -> Monoid.combine(acc, x), Monoid.empty(), t)

}

instance Foldable[Array[a, r]] {
    type Elm = a
    type Aef = r

    pub def foldLeft(f: (b, a) -> b \ ef, s: b, l: Array[a, r]): b \ r + ef = Array.foldLeft(f, s, l)
    pub def foldRight(f: (a, b) -> b \ ef, s: b, l: Array[a, r]): b \ r + ef = Array.foldRight(f, s, l)
    pub def foldRightWithCont(f: (a, Unit -> b \ r + ef) -> b \ r + ef, s: b, l: Array[a, r]): b \ r + ef = Array.foldRightWithCont(f, s, l)
}

instance Foldable[MutDeque[a, r]] {
    type Elm = a
    type Aef = r

    pub def foldLeft(f: (b, a) -> b \ ef, s: b, l: MutDeque[a, r]): b \ r + ef = MutDeque.foldLeft(f, s, l)
    pub def foldRight(f: (a, b) -> b \ ef, s: b, l: MutDeque[a, r]): b \ r + ef = MutDeque.foldRight(f, s, l)
    pub def foldRightWithCont(f: (a, Unit -> b \ r + ef) -> b \ r + ef, s: b, l: MutDeque[a, r]): b \ r + ef = MutDeque.foldRightWithCont(f, s, l)
}

instance Foldable[MutList[a, r]] {
    type Elm = a
    type Aef = r

    pub def foldLeft(f: (b, a) -> b \ ef, s: b, l: MutList[a, r]): b \ r + ef = MutList.foldLeft(f, s, l)
    pub def foldRight(f: (a, b) -> b \ ef, s: b, l: MutList[a, r]): b \ r + ef = MutList.foldRight(f, s, l)
    pub def foldRightWithCont(f: (a, Unit -> b \ r + ef) -> b \ r + ef, s: b, l: MutList[a, r]): b \ r + ef = MutList.foldRightWithCont(f, s, l)
}

instance Foldable[MutMap[k, v, r]] {
    type Elm = v
    type Aef = r

    pub def foldLeft(f: (b, v) -> b \ ef, s: b, l: MutMap[k, v, r]): b \ r + ef = MutMap.foldLeft(f, s, l)
    pub def foldRight(f: (v, b) -> b \ ef, s: b, l: MutMap[k, v, r]): b \ r + ef = MutMap.foldRight(f, s, l)
    pub def foldRightWithCont(f: (v, Unit -> b \ r + ef) -> b \ r + ef, s: b, l: MutMap[k, v, r]): b \ r + ef = MutMap.foldRightWithCont(f, s, l)
}

instance Foldable[MutSet[a, r]] {
    type Elm = a
    type Aef = r

    pub def foldLeft(f: (b, a) -> b \ ef, s: b, l: MutSet[a, r]): b \ r + ef = MutSet.foldLeft(f, s, l)
    pub def foldRight(f: (a, b) -> b \ ef, s: b, l: MutSet[a, r]): b \ r + ef = MutSet.foldRight(f, s, l)
    pub def foldRightWithCont(f: (a, Unit -> b \ r + ef) -> b \ r + ef, s: b, l: MutSet[a, r]): b \ r + ef = MutSet.foldRightWithCont(f, s, l)
}

instance Foldable[MutQueue[a, r]] with Order[a] {
    type Elm = a
    type Aef = r

    pub def foldLeft(f: (b, a) -> b \ ef, s: b, l: MutQueue[a, r]): b \ r + ef = {
        l |> MutQueue.toList |> List.foldLeft(f, s)
    }
    pub def foldRight(f: (a, b) -> b \ ef, s: b, l: MutQueue[a, r]): b \ r + ef = {
        l |> MutQueue.toList |> List.foldRight(f, s)
    }
    pub def foldRightWithCont(f: (a, Unit -> b \ r + ef) -> b \ r + ef, s: b, l: MutQueue[a, r]): b \ r + ef = {
        l |> MutQueue.toList |> List.foldRightWithCont(f, s)
    }
}
