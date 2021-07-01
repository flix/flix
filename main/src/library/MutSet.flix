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
opaque type MutSet[t] = Ref[Set[t]]

instance ToString[MutSet[a]] with ToString[a] {
    pub def toString(xs: MutSet[a]): String =
        let s = MutSet.foldRight((x, acc) -> "${x}, ${acc}", "", xs) as & Pure;
        "MutSet#{${String.dropRight(2, s)}}"
}

instance Foldable[MutSet] {
    pub def foldLeft(f: (b, a) -> b & e, i: b, s: MutSet[a]): b & e = MutSet.foldLeft(f, i, s) as & e
    pub def foldRight(f: (a, b) -> b & e, i: b, s: MutSet[a]): b & e = MutSet.foldRight(f, i, s) as & e
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
    pub def singleton(x: a): MutSet[a] & Impure with Order[a] =
        MutSet(ref Set.singleton(x))

    ///
    /// Adds the element `x` to the mutable set `s`.
    ///
    pub def add!(x: a, s: mut MutSet[a]): Unit & Impure with Order[a] =
        let MutSet(ms) = s;
        ms := Set.insert(x, deref ms)

    ///
    /// Adds all elements in the mutable set `s1` to the mutable set `s2`.
    ///
    pub def addAll!(s1: MutSet[a], s2: mut MutSet[a]): Unit & Impure with Order[a] =
        let MutSet(ms1) = s1;
        let MutSet(ms2) = s2;
        ms2 := Set.union(deref ms1, deref ms2)

    ///
    /// Removes all elements from the mutable set `s`.
    ///
    pub def clear!(s: mut MutSet[a]): Unit & Impure =
        let MutSet(ms) = s;
        ms := Set.empty()

    ///
    /// Removes the element `x` from the mutable set `s`.
    ///
    pub def remove!(x: a, s: mut MutSet[a]): Unit & Impure with Order[a] =
        let MutSet(ms) = s;
        ms := Set.delete(x, deref ms)

    ///
    /// Removes all elements in the mutable set `s1` from the mutable set `s2`.
    ///
    pub def removeAll!(s1: MutSet[a], s2: mut MutSet[a]): Unit & Impure with Order[a] =
        let MutSet(ms1) = s1;
        let MutSet(ms2) = s2;
        ms2 := Set.difference(deref ms2, deref ms1)

    ///
    /// Removes all elements from the mutable set `s2` that are not in the mutable set `s1`.
    ///
    pub def retainAll!(s1: MutSet[a], s2: mut MutSet[a]): Unit & Impure with Order[a] =
        let MutSet(ms1) = s1;
        let MutSet(ms2) = s2;
        ms2 := Set.intersection(deref ms1, deref ms2)

    ///
    /// Removes all elements from the mutable set `s` that do not satisfy the predicate function `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def refine!(f: a -> Bool, s: mut MutSet[a]): Unit & Impure with Order[a] =
        let MutSet(ms) = s;
        ms := Set.filter(f, deref ms)

    ///
    /// Replaces the element `x` with the element `y` if `x` is in the mutable set `s`.
    ///
    /// The mutable set `s` is unchanged if the element `x` is not in it.
    ///
    pub def replace!(x: a, y: a, s: mut MutSet[a]): Unit & Impure with Order[a] =
        let MutSet(ms) = s;
        ms := Set.replace(x, y, deref ms)

    ///
    /// Applies the function `f` to every element in the mutable set `s`.
    ///
    pub def transform!(f: a -> a & e, s: mut MutSet[a]): Unit & Impure with Order[a] =
        let MutSet(ms) = s;
        ms := Set.map(f, deref ms)

    ///
    /// Returns true if and only if `s` is the empty set.
    ///
    pub def isEmpty(s: MutSet[a]): Bool & Impure =
        let MutSet(ms) = s;
        Set.isEmpty(deref ms)

    ///
    /// Returns true if and only if `x` is a member of the mutable set `s`.
    ///
    pub def memberOf(x: a, s: MutSet[a]): Bool & Impure with Order[a] =
        let MutSet(ms) = s;
        Set.memberOf(x, deref ms)

    ///
    /// Optionally finds the smallest element of `s` according to the `Order` on `a`.
    ///
    /// Returns `None` if `s` is empty.
    ///
    pub def minimum(s: MutSet[a]): Option[a] & Impure =
        let MutSet(ms) = s;
        Set.minimum(deref ms)

    ///
    /// Optionally finds the smallest element of `s` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `s` is empty.
    ///
    pub def minimumBy(cmp: (a, a) -> Comparison, s: MutSet[a]): Option[a] & Impure =
        let MutSet(ms) = s;
        Set.minimumBy(cmp, deref ms)

    ///
    /// Optionally finds the largest element of `s` according to the `Order` on `a`.
    ///
    /// Returns `None` if `s` is empty.
    ///
    pub def maximum(s: MutSet[a]): Option[a] & Impure =
        let MutSet(ms) = s;
        Set.maximum(deref ms)

    ///
    /// Optionally finds the largest element of `s` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `s` is empty.
    ///
    pub def maximumBy(cmp: (a, a) -> Comparison, s: MutSet[a]): Option[a] & Impure =
        let MutSet(ms) = s;
        Set.maximumBy(cmp, deref ms)

    ///
    /// Returns the size of the mutable set `s`.
    ///
    pub def size(s: MutSet[a]): Int32 & Impure =
        let MutSet(ms) = s;
        Set.size(deref ms)

    ///
    /// Returns true if and only if every element in the mutable set `s1` appears in the mutable set `s2`.
    ///
    pub def isSubsetOf(s1: MutSet[a], s2: MutSet[a]): Bool & Impure with Order[a] =
        let MutSet(ms1) = s1;
        let MutSet(ms2) = s2;
        Set.isSubsetOf(deref ms1, deref ms2)

    ///
    /// Returns true if and only if every element in the mutable set `s1` appears in the mutable set `s2` and `s1 != s2`.
    ///
    pub def isProperSubsetOf(s1: MutSet[a], s2: MutSet[a]): Bool & Impure with Order[a] =
        let MutSet(ms1) = s1;
        let MutSet(ms2) = s2;
        Set.isProperSubsetOf(deref ms1, deref ms2)

    ///
    /// Alias for `findLeft`.
    ///
    /// The function `f` must be pure.
    ///
    pub def find(f: a -> Bool, s: MutSet[a]): Option[a] & Impure = findLeft(f, s)

    ///
    /// Optionally returns the first element of the mutable set `s` that satisfies the predicate function `f` when searching from left to right.
    ///
    /// The function `f` must be pure.
    ///
    pub def findLeft(f: a -> Bool, s: MutSet[a]): Option[a] & Impure =
        let MutSet(ms) = s;
            Set.findLeft(f, deref ms)

    ///
    /// Optionally returns the first element of the mutable set `s` that satisfies the predicate function `f` when searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    pub def findRight(f: a -> Bool, s: MutSet[a]): Option[a] & Impure =
        let MutSet(ms) = s;
            Set.findRight(f, deref ms)

    ///
    /// Returns the result of applying `combine` to all the elements in `s`, using `empty` as the initial value.
    ///
    pub def fold(s: MutSet[a]): a with Monoid[a] = Foldable.fold(s)

    ///
    /// Applies `f` to a start value `i` and all elements in the mutable set `s` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(i, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b & e, i: b, s: MutSet[a]): b & Impure =
        let MutSet(ms) = s;
        Set.foldLeft(f, i, deref ms)

    ///
    /// Applies `f` to a start value `i` and all elements in the mutable set `s` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, i))...)`.
    ///
    pub def foldRight(f: (a, b) -> b & e, i: b, s: MutSet[a]): b & Impure =
        let MutSet(ms) = s;
        Set.foldRight(f, i, deref ms)

    ///
    /// Applies `f` to all elements in the mutable set `s` going from left to right until a single value `v` is obtained. Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(x1, x2), x3)..., xn))`
    ///
    /// Returns `None` if `s` is the empty set.
    ///
    pub def reduceLeft(f: (a, a) -> a & e, s: MutSet[a]): Option[a] & Impure =
        let MutSet(ms) = s;
        Set.reduceLeft(f, deref ms)

    ///
    /// Applies `f` to all elements in the mutable set `s` going from right to left until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(x1, ...f(xn-2, f(xn-1, xn))...))`
    ///
    /// Returns `None` if `s` is the empty set.
    ///
    pub def reduceRight(f: (a, a) -> a & e, s: MutSet[a]): Option[a] & Impure =
        let MutSet(ms) = s;
        Set.reduceRight(f, deref ms)

    ///
    /// Returns the number of elements in the mutable set `s` that satisfy the predicate function `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def count(f: a -> Bool, s: MutSet[a]): Int32 & Impure =
        let MutSet(ms) = s;
        Set.count(f, deref ms)

    ///
    /// Returns `true` if and only if at least one element in the mutable set `s` satisfies the predicate function `f`.
    ///
    /// Returns `false` if `s` is the empty set.
    ///
    /// The function `f` must be pure.
    ///
    pub def exists(f: a -> Bool, s: MutSet[a]): Bool & Impure =
        let MutSet(ms) = s;
            Set.exists(f, deref ms)

    ///
    /// Returns `true` if and only if all elements in the mutable set `s` satisfy the predicate function `f`.
    ///
    /// Returns `true` if `s` is the empty set.
    ///
    /// The function `f` must be pure.
    ///
    pub def forall(f: a -> Bool, s: MutSet[a]): Bool & Impure =
        let MutSet(ms) = s;
            Set.forall(f, deref ms)

    ///
    /// Returns a shallow copy of the mutable set `s`.
    ///
    pub def copy(s: MutSet[a]): MutSet[a] & Impure =
        let MutSet(ms) = s;
        MutSet(ref (deref ms))

    ///
    /// Returns a pair of mutable sets `(s1, s2)` such that:
    //
    /// `s1` contains all elements of the mutable set `s` that satisfy the predicate function `f`.
    /// `s2` contains all elements of teh mutable set `s` that do not satisfy the predicate function `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def partition(f: a -> Bool, s: MutSet[a]): (MutSet[a], MutSet[a]) & Impure with Order[a] =
        let MutSet(ms) = s;
        let (ys, zs) = Set.partition(f, deref ms);
            (MutSet(ref ys), MutSet(ref zs))

    ///
    /// Returns the mutable set `s` as an immutable set.
    ///
    pub def toImm(s: MutSet[a]): Set[a] & Impure =
        let MutSet(ms) = s;
        deref ms

    ///
    /// Returns the mutable set `s` as a list.
    ///
    pub def toList(s: MutSet[a]): List[a] & Impure =
        let MutSet(ms) = s;
        Set.toList(deref ms)

    ///
    /// Returns the association set `s` as a map.
    ///
    /// If `s` contains multiple mappings with the same key, `toMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    pub def toMap(s: MutSet[(a, b)]): Map[a, b] & Impure with Order[a] =
        let MutSet(ms) = s;
        Set.toMap(deref ms)

    ///
    /// Applies `f` to every element of the mutable set `s`.
    ///
    /// The function `f` must be impure.
    ///
    pub def foreach(f: a ~> Unit, s: MutSet[a]): Unit & Impure =
        let MutSet(ms) = s;
        Set.foreach(f, deref ms)

    ///
    /// Extracts a range of elements from the set `s`.
    ///
    /// That is, the result is a list of all elements `x` where `p(x)` returns `Equal`.
    ///
    pub def query(p: a -> Comparison & e, s: MutSet[a]): List[a] & Impure =
        let MutSet(ms) = s;
        Set.query(p, deref ms)

    ///
    /// Applies `f` to all elements from the set `s` where `p(x)` returns `EqualTo`.
    ///
    /// The function `f` must be impure.
    ///
    pub def queryWith(p: a -> Comparison & e, f: a ~> Unit, s: MutSet[a]): Unit & Impure =
        let MutSet(ms) = s;
        Set.queryWith(p, f, deref ms)

}
