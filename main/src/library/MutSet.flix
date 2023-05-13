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
pub enum MutSet[t: Type, r: Region] {
    case MutSet(Region[r], Ref[Set[t], r])
}

mod MutSet {

    ///
    /// Returns a string representation of the given mutable set `s`.
    ///
    pub def toString(s: MutSet[a, r]): String \r with ToString[a] = region rc {
        let sb = StringBuilder.new(rc);
        StringBuilder.appendString!("MutSet#{", sb);
        forEachWithIndex((i, x) -> {
            if (i == 0)
                StringBuilder.appendString!("${x}", sb)
            else
                StringBuilder.appendString!(", ${x}", sb)
        }, s);
        StringBuilder.appendString!("}", sb);
        StringBuilder.toString(sb)
    }

    ///
    /// Returns a fresh empty set.
    ///
    pub def new(rc: Region[r]): MutSet[a, r] \ r =
        MutSet(rc, ref Set.empty() @ rc)

    ///
    /// Returns the singleton set containing `x`.
    ///
    pub def singleton(rc: Region[r], x: a): MutSet[a, r] \ r with Order[a] =
        MutSet(rc, ref Set.singleton(x) @ rc)

    ///
    /// Adds the element `x` to the mutable set `s`.
    ///
    pub def add!(x: a, s: MutSet[a, r]): Unit \ r with Order[a] =
        let MutSet(_, ms) = s;
        ms := Set.insert(x, deref ms)

    ///
    /// Adds all elements in the collection `m` to the mutable set `s`.
    ///
    pub def addAll!(m: m[a], s: MutSet[a, r]): Unit \ r with Order[a], Foldable[m] =
        Foldable.forEach(x -> add!(x, s), m)

    ///
    /// Removes all elements from the mutable set `s`.
    ///
    pub def clear!(s: MutSet[a, r]): Unit \ r =
        let MutSet(_, ms) = s;
        ms := Set.empty()

    ///
    /// Removes the element `x` from the mutable set `s`.
    ///
    pub def remove!(x: a, s: MutSet[a, r]): Unit \ r with Order[a] =
        let MutSet(_, ms) = s;
        ms := Set.remove(x, deref ms)

    ///
    /// Removes all elements in the collection `m` from the mutable set `s`.
    ///
    pub def removeAll!(m: m[a], s: MutSet[a, r]): Unit \ r with Order[a], Foldable[m] =
        let MutSet(_, ms) = s;
        let s2 = Foldable.toSet(m);
        ms := Set.difference(deref ms, s2)

    ///
    /// Removes all elements from the mutable set `s` that are not in collection `m`.
    ///
    pub def retainAll!(m: m[a], s: MutSet[a, r]): Unit \ r with Order[a], Foldable[m] =
        let MutSet(_, ms) = s;
        let s2 = Foldable.toSet(m);
        ms := Set.intersection(s2, deref ms)

    ///
    /// Removes all elements from the mutable set `s` that do not satisfy the predicate function `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def refine!(f: a -> Bool, s: MutSet[a, r]): Unit \ r with Order[a] =
        let MutSet(_, ms) = s;
        ms := Set.filter(f, deref ms)

    ///
    /// Replaces the element `from` with the element `to` if `from` is in the mutable set `s`.
    ///
    /// The mutable set `s` is unchanged if the element `from` is not in it.
    ///
    pub def replace!(from: {from = a}, to: {to = a}, s: MutSet[a, r]): Unit \ r with Order[a] =
        let MutSet(_, ms) = s;
        ms := Set.replace(from = from.from, to = to.to, deref ms)

    ///
    /// Applies the function `f` to every element in the mutable set `s`.
    ///
    pub def transform!(f: a -> a \ ef, s: MutSet[a, r]): Unit \ { ef, r } with Order[a] =
        let MutSet(_, ms) = s;
        ms := Set.map(f, deref ms)

    ///
    /// Returns true if and only if `s` is the empty set.
    ///
    pub def isEmpty(s: MutSet[a, r]): Bool \ r =
        let MutSet(_, ms) = s;
        Set.isEmpty(deref ms)

    ///
    /// Returns true if and only if `x` is a member of the mutable set `s`.
    ///
    pub def memberOf(x: a, s: MutSet[a, r]): Bool \ r with Order[a] =
        let MutSet(_, ms) = s;
        Set.memberOf(x, deref ms)

    ///
    /// Optionally finds the smallest element of `s` according to the `Order` on `a`.
    ///
    /// Returns `None` if `s` is empty.
    ///
    pub def minimum(s: MutSet[a, r]): Option[a] \ r =
        let MutSet(_, ms) = s;
        Set.minimum(deref ms)

    ///
    /// Optionally finds the smallest element of `s` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `s` is empty.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    pub def minimumBy(cmp: (a, a) -> Comparison \ ef, s: MutSet[a, r]): Option[a] \ { ef, r } =
        let MutSet(_, ms) = s;
        Set.minimumBy(cmp, deref ms)

    ///
    /// Optionally finds the largest element of `s` according to the `Order` on `a`.
    ///
    /// Returns `None` if `s` is empty.
    ///
    pub def maximum(s: MutSet[a, r]): Option[a] \ r =
        let MutSet(_, ms) = s;
        Set.maximum(deref ms)

    ///
    /// Optionally finds the largest element of `s` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `s` is empty.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    pub def maximumBy(cmp: (a, a) -> Comparison \ ef, s: MutSet[a, r]): Option[a] \ { ef, r } =
        let MutSet(_, ms) = s;
        Set.maximumBy(cmp, deref ms)

    ///
    /// Returns the size of the mutable set `s`.
    ///
    pub def size(s: MutSet[a, r]): Int32 \ r =
        let MutSet(_, ms) = s;
        Set.size(deref ms)

    ///
    /// Returns true if and only if every element in the mutable set `s1` appears in the mutable set `s2`.
    ///
    pub def isSubsetOf(s1: MutSet[a, r1], s2: MutSet[a, r2]): Bool \ { r1, r2 } with Order[a] =
        let MutSet(_, ms1) = s1;
        let MutSet(_, ms2) = s2;
        Set.isSubsetOf(deref ms1, deref ms2)

    ///
    /// Returns true if and only if every element in the mutable set `s1` appears in the mutable set `s2` and `s1 != s2`.
    ///
    pub def isProperSubsetOf(s1: MutSet[a, r1], s2: MutSet[a, r2]): Bool \ { r1, r2 } with Order[a] =
        let MutSet(_, ms1) = s1;
        let MutSet(_, ms2) = s2;
        Set.isProperSubsetOf(deref ms1, deref ms2)

    ///
    /// Alias for `findLeft`.
    ///
    /// The function `f` must be pure.
    ///
    pub def find(f: a -> Bool, s: MutSet[a, r]): Option[a] \ r =
        findLeft(f, s)

    ///
    /// Optionally returns the first element of the mutable set `s` that satisfies the predicate function `f` when searching from left to right.
    ///
    /// The function `f` must be pure.
    ///
    pub def findLeft(f: a -> Bool, s: MutSet[a, r]): Option[a] \ r =
        let MutSet(_, ms) = s;
        Set.findLeft(f, deref ms)

    ///
    /// Optionally returns the first element of the mutable set `s` that satisfies the predicate function `f` when searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    pub def findRight(f: a -> Bool, s: MutSet[a, r]): Option[a] \ r =
        let MutSet(_, ms) = s;
        Set.findRight(f, deref ms)

    ///
    /// Applies `f` to a start value `i` and all elements in the mutable set `s` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(i, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b \ ef, i: b, s: MutSet[a, r]): b \ { ef, r } =
        let MutSet(_, ms) = s;
        Set.foldLeft(f, i, deref ms)

    ///
    /// Applies `f` to a start value `z` and all elements in the mutable set `s` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, z))...)`.
    ///
    pub def foldRight(f: (a, b) -> b \ ef, z: b, s: MutSet[a, r]): b \ { ef, r } =
        let MutSet(_, ms) = s;
        Set.foldRight(f, z, deref ms)

    ///
    /// Applies `f` to a start value `z` and all elements in the mutable set `s` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, z))...)`.
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, z: b, s: MutSet[a, r]): b \ { ef, r } =
        let MutSet(_, ms) = s;
        Set.foldRightWithCont(f, z, deref ms)

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap(f: a -> b \ ef, s: MutSet[a, r]): b \ {ef, r} with Monoid[b] =
        foldLeft((acc, x) -> Monoid.combine(acc, f(x)), Monoid.empty(), s)

    ///
    /// Applies `f` to all elements in the mutable set `s` going from left to right until a single value `v` is obtained. Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(x1, x2), x3)..., xn))`
    ///
    /// Returns `None` if `s` is the empty set.
    ///
    pub def reduceLeft(f: (a, a) -> a \ ef, s: MutSet[a, r]): Option[a] \ { ef, r } =
        let MutSet(_, ms) = s;
        Set.reduceLeft(f, deref ms)

    ///
    /// Applies `f` to all elements in the mutable set `s` going from right to left until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(x1, ...f(xn-2, f(xn-1, xn))...))`
    ///
    /// Returns `None` if `s` is the empty set.
    ///
    pub def reduceRight(f: (a, a) -> a \ ef, s: MutSet[a, r]): Option[a] \ { ef, r } =
        let MutSet(_, ms) = s;
        Set.reduceRight(f, deref ms)

    ///
    /// Returns the number of elements in the mutable set `s` that satisfy the predicate function `f`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    pub def count(f: a -> Bool \ ef, s: MutSet[a, r]): Int32 \ { ef, r } =
        let MutSet(_, ms) = s;
        Set.count(f, deref ms)

    ///
    /// Returns the sum of all elements in the mutable set `s`.
    ///
    pub def sum(s: MutSet[Int32, r]): Int32 \ r =
        let MutSet(_, ms) = s;
        Set.sum(deref ms)

    ///
    /// Returns the sum of all elements in the mutable set `s` according to the function `f`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    pub def sumWith(f: a -> Int32 \ ef, s: MutSet[a, r]): Int32 \ { ef, r } =
        let MutSet(_, ms) = s;
        Set.sumWith(f, deref ms)

    ///
    /// Returns `true` if and only if at least one element in the mutable set `s` satisfies the predicate function `f`.
    ///
    /// Returns `false` if `s` is the empty set.
    ///
    pub def exists(f: a -> Bool \ ef, s: MutSet[a, r]): Bool \ { ef, r } =
        let MutSet(_, ms) = s;
        Set.exists(f, deref ms)

    ///
    /// Returns `true` if and only if all elements in the mutable set `s` satisfy the predicate function `f`.
    ///
    /// Returns `true` if `s` is the empty set.
    ///
    pub def forAll(f: a -> Bool \ ef, s: MutSet[a, r]): Bool \ { ef, r } =
        let MutSet(_, ms) = s;
        Set.forAll(f, deref ms)

    ///
    /// Returns a shallow copy of the mutable set `s`.
    ///
    pub def copy(r1: Region[r1], s: MutSet[a, r]): MutSet[a, r1] \ { r, r1 } =
        let MutSet(_, ms) = s;
        MutSet(r1, ref (deref ms) @ r1)

    ///
    /// Returns a pair of mutable sets `(s1, s2)` such that:
    //
    /// `s1` contains all elements of the mutable set `s` that satisfy the predicate function `f`.
    /// `s2` contains all elements of the mutable set `s` that do not satisfy the predicate function `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def partition(r1: Region[r1], r2: Region[r2], f: a -> Bool, s: MutSet[a, r]): (MutSet[a, r1], MutSet[a, r2]) \ { r, r1, r2 } with Order[a] =
        let MutSet(_, ms) = s;
        let (ys, zs) = Set.partition(f, deref ms);
        (MutSet(r1, ref ys @ r1), MutSet(r2, ref zs @ r2))

    ///
    /// Returns the mutable set `s` as an immutable set.
    ///
    pub def toSet(s: MutSet[a, r]): Set[a] \ r =
        let MutSet(_, ms) = s;
        deref ms

    ///
    /// Returns the mutable set `s` as a list.
    ///
    pub def toList(s: MutSet[a, r]): List[a] \ r =
        let MutSet(_, ms) = s;
        Set.toList(deref ms)

    ///
    /// Returns the association set `s` as a map.
    ///
    /// If `s` contains multiple mappings with the same key, `toMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    pub def toMap(s: MutSet[(a, b), r]): Map[a, b] \ r with Order[a] =
        let MutSet(_, ms) = s;
        Set.toMap(deref ms)

    ///
    /// Returns the mutable set `s` as a MutDeque.
    ///
    pub def toMutDeque(r1: Region[r1], s: MutSet[a, r2]): MutDeque[a, r1] \ { r2, r1 } =
        let MutSet(_, ms) = s;
        Set.toMutDeque(r1, deref ms)

    ///
    /// Applies `f` to every element of the mutable set `s`.
    ///
    pub def forEach(f: a -> Unit \ ef, s: MutSet[a, r]): Unit \ { ef, r } =
        let MutSet(_, ms) = s;
        Set.forEach(f, deref ms)

    ///
    /// Applies `f` to every element of the mutable set `s` along with that element's index.
    ///
    pub def forEachWithIndex(f: (Int32, a) -> Unit \ ef, s: MutSet[a, r]): Unit \ { ef, r } = region rc {
        let ix = ref 0 @ rc;
        forEach(x -> {let i = deref ix; f(i, x); ix := i+1}, s)
    }

    ///
    /// Returns an iterator over `s`.
    ///
    pub def iterator(rc: Region[r1], s: MutSet[a, r2]): Iterator[a, r1 and r2, r1] \ { r1, r2 } =
        let MutSet(_, ms) = s;
        Set.iterator(rc, deref ms) |> Iterator.map(x -> {discard deref ms; x})

    ///
    /// Returns an enumerator over `s`.
    ///
    pub def enumerator(rc: Region[r1], s: MutSet[a, r2]): Iterator[(Int32, a), r1 and r2, r1] \ { r1, r2 } =
        iterator(rc, s) |> Iterator.zipWithIndex

    ///
    /// Returns `true` if MutSets `a` and `b` have the same elements, i.e. are structurally equal.
    ///
    pub def sameElements(a: MutSet[a, r], b: MutSet[a, r]): Bool \ r with Order[a] =
        MutSet.isSubsetOf(a, b) and MutSet.isSubsetOf(b, a)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `s` with `sep` inserted between each element.
    ///
    pub def join(sep: String, s: MutSet[a, r]): String \ r with ToString[a] =
        let MutSet(_, ms) = s;
        Set.join(sep, deref ms)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `s` according to `f` with `sep` inserted between each element.
    ///
    pub def joinWith(f: a -> String \ ef, sep: String, s: MutSet[a, r]): String \ { ef, r } =
        let MutSet(_, ms) = s;
        Set.joinWith(f, sep, deref ms)

}
