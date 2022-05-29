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
opaque type MutSet[t: Type, r: Region] = ScopedRef[Set[t], r]

instance Newable[MutSet[t]] {
    pub def new(r: Region[r]): MutSet[t, r] \ { Write(r) } = MutSet.new(r)
}

instance Scoped[MutSet[t]] {
    pub def regionOf(_: MutSet[t, r]): Region[r] = () as Region[r]
}

namespace MutSet {

    ///
    /// Returns a fresh empty set.
    ///
    pub def new(r: Region[r]): MutSet[a, r] \ { Write(r) } =
        MutSet(ref Set.empty() @ r)

    ///
    /// Returns the singleton set containing `x`.
    ///
    pub def singleton(x: a, r: Region[r]): MutSet[a, r] \ Write(r) with Order[a] =
        MutSet(ref Set.singleton(x) @ r)

    ///
    /// Adds the element `x` to the mutable set `s`.
    ///
    pub def add!(x: a, s: MutSet[a, r]): Unit \ { Read(r), Write(r) } with Order[a] =
        let MutSet(ms) = s;
        ms := Set.insert(x, deref ms)

    ///
    /// Adds all elements in the collection `m` to the mutable set `s`.
    ///
    pub def addAll!(m: m[a], s: MutSet[a, r]): Unit \ { Read(r), Write(r) } with Order[a], Foldable[m] =
        Foldable.foreach(x -> add!(x, s), m)

    ///
    /// Removes all elements from the mutable set `s`.
    ///
    pub def clear!(s: MutSet[a, r]): Unit \ { Write(r) } =
        let MutSet(ms) = s;
        ms := Set.empty()

    ///
    /// Removes the element `x` from the mutable set `s`.
    ///
    pub def remove!(x: a, s: MutSet[a, r]): Unit \ { Read(r), Write(r) } with Order[a] =
        let MutSet(ms) = s;
        ms := Set.remove(x, deref ms)

    ///
    /// Removes all elements in the mutable set `s1` from the mutable set `s2`.
    ///
    pub def removeAll!(s1: MutSet[a, r1], s2: MutSet[a, r2]): Unit \ { Read(r1, r2), Write(r2) } with Order[a] =
        let MutSet(ms1) = s1;
        let MutSet(ms2) = s2;
        ms2 := Set.difference(deref ms2, deref ms1)

    ///
    /// Removes all elements from the mutable set `s2` that are not in the mutable set `s1`.
    ///
    pub def retainAll!(s1: MutSet[a, r1], s2: MutSet[a, r2]): Unit \ { Read(r1, r2), Write(r2) } with Order[a] =
        let MutSet(ms1) = s1;
        let MutSet(ms2) = s2;
        ms2 := Set.intersection(deref ms1, deref ms2)

    ///
    /// Removes all elements from the mutable set `s` that do not satisfy the predicate function `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def refine!(f: a -> Bool, s: MutSet[a, r]): Unit \ { Read(r), Write(r) } with Order[a] =
        let MutSet(ms) = s;
        ms := Set.filter(f, deref ms)

    ///
    /// Replaces the element `from` with the element `to` if `from` is in the mutable set `s`.
    ///
    /// The mutable set `s` is unchanged if the element `from` is not in it.
    ///
    pub def replace!(from: {from :: a}, to: {to :: a}, s: MutSet[a, r]): Unit \ { Read(r), Write(r) } with Order[a] =
        let MutSet(ms) = s;
        ms := Set.replace(from = from.from, to = to.to, deref ms)

    ///
    /// Applies the function `f` to every element in the mutable set `s`.
    ///
    pub def transform!(f: a -> a & ef, s: MutSet[a, r]): Unit \ { ef, Read(r), Write(r) } with Order[a] =
        let MutSet(ms) = s;
        ms := Set.map(f, deref ms)

    ///
    /// Returns true if and only if `s` is the empty set.
    ///
    pub def isEmpty(s: MutSet[a, r]): Bool \ { Read(r) } =
        let MutSet(ms) = s;
        Set.isEmpty(deref ms)

    ///
    /// Returns true if and only if `x` is a member of the mutable set `s`.
    ///
    pub def memberOf(x: a, s: MutSet[a, r]): Bool \ { Read(r) } with Order[a] =
        let MutSet(ms) = s;
        Set.memberOf(x, deref ms)

    ///
    /// Optionally finds the smallest element of `s` according to the `Order` on `a`.
    ///
    /// Returns `None` if `s` is empty.
    ///
    pub def minimum(s: MutSet[a, r]): Option[a] \ { Read(r) } =
        let MutSet(ms) = s;
        Set.minimum(deref ms)

    ///
    /// Optionally finds the smallest element of `s` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `s` is empty.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def minimumBy(cmp: (a, a) -> Comparison & ef, s: MutSet[a, r]): Option[a] \ { ef, Read(r) } =
        let MutSet(ms) = s;
        Set.minimumBy(cmp, deref ms)

    ///
    /// Optionally finds the largest element of `s` according to the `Order` on `a`.
    ///
    /// Returns `None` if `s` is empty.
    ///
    pub def maximum(s: MutSet[a, r]): Option[a] \ { Read(r) } =
        let MutSet(ms) = s;
        Set.maximum(deref ms)

    ///
    /// Optionally finds the largest element of `s` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `s` is empty.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def maximumBy(cmp: (a, a) -> Comparison & ef, s: MutSet[a, r]): Option[a] \ { ef, Read(r) } =
        let MutSet(ms) = s;
        Set.maximumBy(cmp, deref ms)

    ///
    /// Returns the size of the mutable set `s`.
    ///
    pub def size(s: MutSet[a, r]): Int32 \ { Read(r) } =
        let MutSet(ms) = s;
        Set.size(deref ms)

    ///
    /// Returns true if and only if every element in the mutable set `s1` appears in the mutable set `s2`.
    ///
    pub def isSubsetOf(s1: MutSet[a, r1], s2: MutSet[a, r2]): Bool \ { Read(r1, r2) } with Order[a] =
        let MutSet(ms1) = s1;
        let MutSet(ms2) = s2;
        Set.isSubsetOf(deref ms1, deref ms2)

    ///
    /// Returns true if and only if every element in the mutable set `s1` appears in the mutable set `s2` and `s1 != s2`.
    ///
    pub def isProperSubsetOf(s1: MutSet[a, r1], s2: MutSet[a, r2]): Bool \ { Read(r1, r2) } with Order[a] =
        let MutSet(ms1) = s1;
        let MutSet(ms2) = s2;
        Set.isProperSubsetOf(deref ms1, deref ms2)

    ///
    /// Alias for `findLeft`.
    ///
    /// The function `f` must be pure.
    ///
    pub def find(f: a -> Bool, s: MutSet[a, r]): Option[a] \ { Read(r) } =
        findLeft(f, s)

    ///
    /// Optionally returns the first element of the mutable set `s` that satisfies the predicate function `f` when searching from left to right.
    ///
    /// The function `f` must be pure.
    ///
    pub def findLeft(f: a -> Bool, s: MutSet[a, r]): Option[a] \ { Read(r) } =
        let MutSet(ms) = s;
        Set.findLeft(f, deref ms)

    ///
    /// Optionally returns the first element of the mutable set `s` that satisfies the predicate function `f` when searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    pub def findRight(f: a -> Bool, s: MutSet[a, r]): Option[a] \ { Read(r) } =
        let MutSet(ms) = s;
        Set.findRight(f, deref ms)

    ///
    /// Applies `f` to a start value `i` and all elements in the mutable set `s` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(i, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b & ef, i: b, s: MutSet[a, r]): b \ { ef, Read(r) } =
        let MutSet(ms) = s;
        Set.foldLeft(f, i, deref ms)

    ///
    /// Applies `f` to a start value `z` and all elements in the mutable set `s` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, z))...)`.
    ///
    pub def foldRight(f: (a, b) -> b & ef, z: b, s: MutSet[a, r]): b \ { ef, Read(r) } =
        let MutSet(ms) = s;
        Set.foldRight(f, z, deref ms)

    ///
    /// Applies `f` to a start value `z` and all elements in the mutable set `s` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, z))...)`.
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (a, Unit -> b & ef) -> b & ef, z: b, s: MutSet[a, r]): b \ { ef, Read(r) } =
        let MutSet(ms) = s;
        Set.foldRightWithCont(f, z, deref ms)

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap(f: a -> b & ef, s: MutSet[a, r]): b \ {ef, Read(r)} with Monoid[b] =
        foldLeft((acc, x) -> Monoid.combine(acc, f(x)), Monoid.empty(), s)

    ///
    /// Applies `f` to all elements in the mutable set `s` going from left to right until a single value `v` is obtained. Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(x1, x2), x3)..., xn))`
    ///
    /// Returns `None` if `s` is the empty set.
    ///
    pub def reduceLeft(f: (a, a) -> a & ef, s: MutSet[a, r]): Option[a] \ { ef, Read(r) } =
        let MutSet(ms) = s;
        Set.reduceLeft(f, deref ms)

    ///
    /// Applies `f` to all elements in the mutable set `s` going from right to left until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(x1, ...f(xn-2, f(xn-1, xn))...))`
    ///
    /// Returns `None` if `s` is the empty set.
    ///
    pub def reduceRight(f: (a, a) -> a & ef, s: MutSet[a, r]): Option[a] \ { ef, Read(r) } =
        let MutSet(ms) = s;
        Set.reduceRight(f, deref ms)

    ///
    /// Returns the number of elements in the mutable set `s` that satisfy the predicate function `f`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def count(f: a -> Bool & ef, s: MutSet[a, r]): Int32 \ { ef, Read(r) } =
        let MutSet(ms) = s;
        Set.count(f, deref ms)

    ///
    /// Returns the sum of all elements in the mutable set `s`.
    ///
    pub def sum(s: MutSet[Int32, r]): Int32 \ { Read(r) } =
        let MutSet(ss) = s;
        Set.sum(deref ss)

    ///
    /// Returns the sum of all elements in the mutable set `s` according to the function `f`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def sumWith(f: a -> Int32 & ef, s: MutSet[a, r]): Int32 \ { ef, Read(r) } =
        let MutSet(ss) = s;
        Set.sumWith(f, deref ss)

    ///
    /// Returns the product of all elements in the set `s`.
    ///
    pub def product(s: MutSet[Int32, r]): Int32 \ { Read(r) } =
        let MutSet(ss) = s;
        Set.product(deref ss)

    ///
    /// Returns the product of all elements in the set `s` according to the function `f`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def productWith(f: a -> Int32 & ef, s: MutSet[a, r]): Int32 \ { ef, Read(r) } =
        let MutSet(ss) = s;
        Set.productWith(f, deref ss)

    ///
    /// Returns `true` if and only if at least one element in the mutable set `s` satisfies the predicate function `f`.
    ///
    /// Returns `false` if `s` is the empty set.
    ///
    /// The function `f` must be pure.
    ///
    pub def exists(f: a -> Bool, s: MutSet[a, r]): Bool \ { Read(r) } =
        let MutSet(ms) = s;
        Set.exists(f, deref ms)

    ///
    /// Returns `true` if and only if all elements in the mutable set `s` satisfy the predicate function `f`.
    ///
    /// Returns `true` if `s` is the empty set.
    ///
    /// The function `f` must be pure.
    ///
    pub def forall(f: a -> Bool, s: MutSet[a, r]): Bool \ { Read(r) } =
        let MutSet(ms) = s;
        Set.forall(f, deref ms)

    ///
    /// Returns a shallow copy of the mutable set `s`.
    ///
    pub def copy(s: MutSet[a, r]): MutSet[a, r] \ { Read(r), Write(r) } =
        let MutSet(ms) = s;
        MutSet(ref (deref ms) @ Scoped.regionOf(s))

    ///
    /// Returns a pair of mutable sets `(s1, s2)` such that:
    //
    /// `s1` contains all elements of the mutable set `s` that satisfy the predicate function `f`.
    /// `s2` contains all elements of teh mutable set `s` that do not satisfy the predicate function `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def partition(f: a -> Bool, s: MutSet[a, r]): (MutSet[a, r], MutSet[a, r]) \ { Read(r), Write(r) } with Order[a] =
        let MutSet(ms) = s;
        let (ys, zs) = Set.partition(f, deref ms);
        (MutSet(ref ys @ Scoped.regionOf(s)), MutSet(ref zs @ Scoped.regionOf(s)))

    ///
    /// Returns the mutable set `s` as an immutable set.
    ///
    pub def toSet(s: MutSet[a, r]): Set[a] \ { Read(r) } =
        let MutSet(ms) = s;
        deref ms

    ///
    /// Returns the mutable set `s` as a list.
    ///
    pub def toList(s: MutSet[a, r]): List[a] \ { Read(r) } =
        let MutSet(ms) = s;
        Set.toList(deref ms)

    ///
    /// Returns the association set `s` as a map.
    ///
    /// If `s` contains multiple mappings with the same key, `toMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    pub def toMap(s: MutSet[(a, b), r]): Map[a, b] \ { Read(r) } with Order[a] =
        let MutSet(ms) = s;
        Set.toMap(deref ms)

    ///
    /// Applies `f` to every element of the mutable set `s`.
    ///
    pub def foreach(f: a -> Unit & ef, s: MutSet[a, r]): Unit \ { ef, Read(r) } =
        let MutSet(ms) = s;
        Set.foreach(f, deref ms)

    ///
    /// Returns `true` if MutSets `a` and `b` have the same elements, i.e. are structurally equal.
    ///
    pub def sameElements(a: MutSet[a, r], b: MutSet[a, r]): Bool \ { Read(r) } with Order[a] =
        MutSet.isSubsetOf(a, b) and MutSet.isSubsetOf(b, a)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `s` with `sep` inserted between each element.
    ///
    pub def join(sep: String, s: MutSet[a, r]): String \ { Read(r) } with ToString[a] =
        let MutSet(ms) = s;
        Set.join(sep, deref ms)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `s` according to `f` with `sep` inserted between each element.
    ///
    pub def joinWith(f: a -> String & ef, sep: String, s: MutSet[a, r]): String \ { ef, Read(r) } =
        let MutSet(ms) = s;
        Set.joinWith(f, sep, deref ms)

}
