/*
 * Copyright 2020 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

///
/// The Mutable Set type.
///
enum MutSet[t] {
    case MutSet(Ref[Set[t]])
}

namespace MutSet {

    ///
    /// Returns a fresh empty set.
    ///
    pub def empty(): MutSet[a] & Impure =
        MutSet(ref Set.empty())

    ///
    /// Returns the singleton set containing `x`.
    ///
    pub def singleton(x: a): MutSet[a] & Impure =
        MutSet(ref Set.singleton(x))

    ///
    /// Adds `x` to `xs`.
    ///
    pub def add(x: a, xs: mut MutSet[a]): Unit & Impure =
        let MutSet(ms) = xs;
        ms := Set.insert(x, deref ms)

    ///
    /// Adds all elements in `ys` to `xs`.
    ///
    pub def addAll(ys: Set[a], xs: mut MutSet[a]): Unit & Impure =
        let MutSet(ms) = xs;
        ms := Set.union(deref ms, ys)

    ///
    /// Removes all elements from `xs`.
    ///
    pub def clear(xs: mut MutSet[a]): Unit & Impure =
        let MutSet(ms) = xs;
        ms := Set.empty()

    ///
    /// Returns a shallow copy of `xs`.
    ///
    pub def copy(xs: mut MutSet[a]): MutSet[a] & Impure =
        let MutSet(ms) = xs;
        MutSet(ref (deref ms))

    ///
    /// Removes `x` from `xs`.
    ///
    pub def remove(x: a, xs: mut MutSet[a]): Unit & Impure =
        let MutSet(ms) = xs;
        ms := Set.delete(x, deref ms)

    ///
    /// Removes all `ys` from `xs`.
    ///
    pub def removeAll(ys: Set[a], xs: mut MutSet[a]): Unit & Impure =
        let MutSet(ms) = xs;
        ms := Set.difference(deref ms, ys)

    ///
    /// Removes all elements from `xs` that are not in `ys`.
    ///
    pub def retainAll(ys: Set[a], xs: mut MutSet[a]): Unit & Impure =
        let MutSet(ms) = xs;
        ms := Set.intersection(deref ms, ys)

    ///
    /// Removes all elements from `xs` that do not satisfy the given boolean function `f`.
    ///
    pub def refine(f: a -> Bool, xs: mut MutSet[a]): Unit & Impure =
        let MutSet(ms) = xs;
        ms := Set.filter(f, deref ms)

    ///
    /// Returns true if and only if `xs` is the empty set, i.e. `Set(Nil)`.
    ///
    pub def isEmpty(xs: MutSet[a]): Bool & Impure =
        let MutSet(ms) = xs;
        Set.isEmpty(deref ms)

    ///
    /// Returns true if and only if `x` is a member of `xs`.
    ///
    pub def memberOf(x: a, xs: MutSet[a]): Bool & Impure =
        let MutSet(ms) = xs;
        Set.memberOf(x, deref ms)

    ///
    /// Returns the size of `xs`.
    ///
    pub def size(xs: MutSet[a]): Int32 & Impure =
        let MutSet(ms) = xs;
        Set.size(deref ms)

    ///
    /// Returns true if and only if every element in `xs` appears in `ys`.
    ///
    pub def isSubsetOf(xs: MutSet[a], ys: MutSet[a]): Bool & Impure =
        let MutSet(s1) = xs;
        let MutSet(s2) = ys;
        Set.isSubsetOf(deref s1, deref s2)

    ///
    /// Returns true if and only if every element in `xs` appears in `ys` and `xs != ys`.
    ///
    pub def isProperSubsetOf(xs: MutSet[a], ys: MutSet[a]): Bool & Impure =
        let MutSet(s1) = xs;
        let MutSet(s2) = ys;
        Set.isProperSubsetOf(deref s1, deref s2)

    ///
    /// Alias for `findLeft`.
    ///
    /// The function `f` must be pure.
    ///
    pub def find(f: a -> Bool, xs: MutSet[a]): Option[a] & Impure = findLeft(f, xs)

    ///
    /// Optionally returns the first element of `xs` that satisfies the predicate `f` when searching from left to right.
    ///
    /// The function `f` must be pure.
    ///
    pub def findLeft(f: a -> Bool, xs: MutSet[a]): Option[a] & Impure =
        let MutSet(ms) = xs;
            Set.findLeft(f, deref ms)

    ///
    /// Optionally returns the first element of `xs` that satisfies the predicate `f` when searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    pub def findRight(f: a -> Bool, xs: MutSet[a]): Option[a] & Impure =
        let MutSet(ms) = xs;
            Set.findRight(f, deref ms)

    ///
    /// Alias for `foldLeft`.
    ///
    pub def fold(f: (b, a) -> b & e, s: b, xs: MutSet[a]): b & Impure =
        foldLeft(f, s, xs)

    ///
    /// Applies `f` to a start value `s` and all elements in `xs` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b & e, s: b, xs: MutSet[a]): b & Impure =
        let MutSet(ms) = xs;
        Set.foldLeft(f, s, deref ms)

    ///
    /// Applies `f` to a start value `s` and all elements in `xs` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b & e, s: b, xs: MutSet[a]): b & Impure =
        let MutSet(ms) = xs;
        Set.foldRight(f, s, deref ms)

    ///
    /// Alias for `reduceLeftOpt`.
    ///
    pub def reduce(f: (a, a) -> a & e, xs: MutSet[a]): Option[a] & Impure =
        reduceLeft(f, xs)

    ///
    /// Applies `f` to all elements in `xs` going from left to right until a single value `v` is obtained. Returns `Some(v)`.

    /// That is, the result is of the form: `Some(f(...f(f(x1, x2), x3)..., xn))`

    /// Returns `None` if `xs` is the empty set.
    ///
    pub def reduceLeft(f: (a, a) -> a & e, xs: MutSet[a]): Option[a] & Impure =
        let MutSet(ms) = xs;
        Set.reduceLeft(f, deref ms)

    ///
    /// Applies `f` to all elements in `xs` going from right to left until a single value `v` is obtained.  Returns `Some(v)`.

    /// That is, the result is of the form: `Some(f(x1, ...f(xn-2, f(xn-1, xn))...))`

    /// Returns `None` if `xs` is the empty set.
    ///
    pub def reduceRight(f: (a, a) -> a & e, xs: MutSet[a]): Option[a] & Impure =
        let MutSet(ms) = xs;
        Set.reduceRight(f, deref ms)

    ///
    /// Returns the number of elements in `xs` that satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def count(f: a -> Bool, xs: MutSet[a]): Int32 & Impure =
        let MutSet(ms) = xs;
        Set.count(f, deref ms)

    ///
    /// Returns `true` if and only if at least one element in `xs` satisfies the predicate `f`.
    ///
    /// Returns `false` if `xs` is the empty set.
    ///
    /// The function `f` must be pure.
    ///
    pub def exists(f: a -> Bool, xs: MutSet[a]): Bool & Impure =
        let MutSet(ms) = xs;
            Set.exists(f, deref ms)

    ///
    /// Returns `true` if and only if all elements in `xs` satisfy the predicate `f`.
    ///
    /// Returns `true` if `xs` is the empty set.
    ///
    /// The function `f` must be pure.
    ///
    pub def forall(f: a -> Bool, xs: MutSet[a]): Bool & Impure =
        let MutSet(ms) = xs;
            Set.forall(f, deref ms)

/*

// TODO
    ///
    /// Returns the intersection of `xs` and `ys`.
    ///
    pub def intersection(xs: Set[a], ys: Set[a]): Set[a] =
        let Set(s) = xs;
            List.foldRight((x, y) -> if (memberOf(x, ys)) insert(x, y) else y, empty(), s)

// TODO
    ///
    /// Returns the difference of `xs` and `ys`, i.e. `xs - ys`.
    ///
    pub def difference(xs: Set[a], ys: Set[a]): Set[a] =
        let Set(s) = xs;
            List.foldRight((x, y) -> if (memberOf(x, ys)) y else insert(x, y), empty(), s)

// TODO
    ///
    /// Returns the set of all elements of `xs` that satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def filter(f: a -> Bool, xs: Set[a]): Set[a] =
        let Set(s) = xs;
            Set(List.filter(f, s))

*/

    ///
    /// Applies the function `f` to every element in `xs`.
    ///
    pub def transform(f: a -> a & e, xs: mut MutSet[a]): Unit & Impure =
        let MutSet(ms) = xs;
        ms := Set.map(f, deref ms)

    ///
    /// Applies the function `f` to every element in `xs` merging the results into `xs`.
    ///
    pub def flatTransform(f: a -> Set[a] & e, xs: mut MutSet[a]): Unit & Impure =
        let MutSet(ms) = xs;
        ms := Set.flatMap(f, deref ms)

    ///
    /// Replaces the element `x` with `y` if `x` is in `xs`. Otherwise, returns `xs`.
    ///
    pub def replace(x: a, y: a, xs: mut MutSet[a]): Unit & Impure =
        let MutSet(ms) = xs;
        ms := Set.replace(x, y, deref ms)

    ///
    /// Returns a pair of sets `(ys, zs)`.
    //
    /// `ys` contains all elements of `xs` that satisfy the predicate `f`.
    /// `zs` contains all elements of `xs` that do not satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def partition(f: a -> Bool, xs: MutSet[a]): (MutSet[a], MutSet[a]) & Impure =
        let MutSet(ms) = xs;
        let (ys, zs) = Set.partition(f, deref ms);
            (MutSet(ref ys), MutSet(ref zs))

    ///
    /// Returns the set `xs` as an immutable set.
    ///
    pub def toImmutable(xs: MutSet[a]): Set[a] & Impure =
        let MutSet(ms) = xs;
        deref ms

    ///
    /// Returns the set `xs` as a list.
    ///
    pub def toList(xs: MutSet[a]): List[a] & Impure =
        let MutSet(ms) = xs;
        Set.toList(deref ms)

    ///
    /// Returns the association set `xs` as a map.
    ///
    /// If `xs` contains multiple mappings with the same key, `toMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    pub def toMap(xs: MutSet[(a, b)]): Map[a, b] & Impure =
        let MutSet(ms) = xs;
        Set.toMap(deref ms)

    ///
    /// Applies `f` to every element of `xs`.
    ///
    /// The function `f` must be impure.
    ///
    pub def foreach(f: a ~> Unit, xs: MutSet[a]): Unit & Impure =
        let MutSet(ms) = xs;
        Set.foreach(f, deref ms)

    ///
    /// Returns `true` if and only if `xs` and `ys` and equal, i.e. they have the same elements.
    ///
    pub def __eq(xs: MutSet[a], ys: MutSet[a]): Bool & Impure =
        isSubsetOf(xs, ys) && size(xs) == size(ys)

    /////////////////////////////////////////////////////////////////////////////
    // Lints: Simplifications                                                  //
    /////////////////////////////////////////////////////////////////////////////


    /////////////////////////////////////////////////////////////////////////////
    // Lints: Use Cases                                                        //
    /////////////////////////////////////////////////////////////////////////////


}

