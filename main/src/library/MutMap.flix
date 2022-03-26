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
/// The Mutable Map type.
///
opaque type MutMap[k, v] = Ref[Map[k, v]]

namespace MutMap {

    ///
    /// Returns `true` if and only if `m` is the empty map.
    ///
    pub def isEmpty(m: MutMap[k, v]): Bool & Impure =
        let MutMap(mm) = m;
        Map.isEmpty(deref mm)

    ///
    /// Returns a fresh empty mutable map.
    ///
    pub def empty(): MutMap[k, v] & Impure =
        MutMap(ref Map.empty())

    ///
    /// Returns the singleton map where the key `k` is mapped to value `v`.
    ///
    pub def singleton(k: k, v: v): MutMap[k, v] & Impure with Order[k] =
        MutMap(ref Map.singleton(k, v))

    ///
    /// Returns `true` if and only if the mutable map `m` contains the key `k`.
    ///
    pub def memberOf(k: k, m: MutMap[k, v]): Bool & Impure with Order[k] =
        let MutMap(mm) = m;
        Map.memberOf(k, deref mm)

    ///
    /// Optionally finds `k -> v` where `k` is the smallest key according to the `Order` instance of `k`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    pub def minimumKey(m: MutMap[k, v]): Option[(k, v)] & Impure =
        let MutMap(mm) = m;
        Map.minimumKey(deref mm)

    ///
    /// Optionally finds `k => v` where `k` is the smallest key according to the given comparator `cmp`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `cmp`.
    ///
    @ParallelWhenPure
    pub def minimumKeyBy(cmp: (k, k) -> Comparison & ef, m: MutMap[k, v]): Option[(k, v)] & Impure =
        let MutMap(mm) = m;
        Map.minimumKeyBy(cmp, deref mm)

    ///
    /// Optionally finds `k => v` where `v` is the smallest value.
    ///
    /// Returns `None` if `m` is empty.
    ///
    @Parallel
    pub def minimumValue(m: MutMap[k, v]): Option[(k, v)] & Impure with Order[v] =
        let MutMap(mm) = m;
        Map.minimumValue(deref mm)

    ///
    /// Optionally finds `k => v` where `v` is the smallest value according to the given comparator `cmp`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `cmp`.
    ///
    @ParallelWhenPure
    pub def minimumValueBy(cmp: (v, v) -> Comparison & ef, m: MutMap[k, v]): Option[(k, v)] & Impure =
        let MutMap(mm) = m;
        Map.minimumValueBy(cmp, deref mm)

    ///
    /// Optionally finds `k => v` where `k` is the largest key according to the `Order` instance of `k`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    pub def maximumKey(m: MutMap[k, v]): Option[(k, v)] & Impure =
        let MutMap(mm) = m;
        Map.maximumKey(deref mm)

    ///
    /// Optionally finds `k => v` where `k` is the largest key according to the given comparator `cmp`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def maximumKeyBy(cmp: (k, k) -> Comparison & ef, m: MutMap[k, v]): Option[(k, v)] & Impure =
        let MutMap(mm) = m;
        Map.maximumKeyBy(cmp, deref mm)

    ///
    /// Optionally finds `k => v` where `v` is the largest value.
    ///
    /// Returns `None` if `m` is empty.
    ///
    @Parallel
    pub def maximumValue(m: MutMap[k, v]): Option[(k, v)] & Impure with Order[v] =
        let MutMap(mm) = m;
        Map.maximumValue(deref mm)

    ///
    /// Optionally finds `k => v` where `v` is the largest value according to the given comparator `cmp`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `cmp`.
    ///
    @ParallelWhenPure
    pub def maximumValueBy(cmp: (v, v) -> Comparison & ef, m: MutMap[k, v]): Option[(k, v)] & Impure =
        let MutMap(mm) = m;
        Map.maximumValueBy(cmp, deref mm)

    ///
    /// Returns the keys of the mutable map `m`.
    ///
    pub def keysOf(m: MutMap[k, v]): Set[k] & Impure with Order[k] =
        let MutMap(mm) = m;
        Map.keysOf(deref mm)

    ///
    /// Returns the values of the mutable map `m`.
    ///
    pub def valuesOf(m: MutMap[k, v]): List[v] & Impure =
        let MutMap(mm) = m;
        Map.valuesOf(deref mm)

    ///
    /// Returns the size of the mutable map `m`.
    ///
    pub def size(m: MutMap[k, v]): Int32 & Impure =
        let MutMap(mm) = m;
        Map.size(deref mm)

    ///
    /// Returns `Some(v)` if `k -> v` is in the mutable map `m`.
    ///
    /// Otherwise returns `None`.
    ///
    pub def get(k: k, m: MutMap[k, v]): Option[v] & Impure with Order[k] =
        let MutMap(mm) = m;
        Map.get(k, deref mm)

    ///
    /// Returns `v` if `k -> v` is in the mutable map `m`.
    ///
    /// Otherwise returns `d`.
    ///
    pub def getWithDefault(k: k, d: v, m: MutMap[k, v]): v & Impure with Order[k] =
        let MutMap(mm) = m;
        Map.getWithDefault(k, d, deref mm)

    ///
    /// Updates the mutable map `m` with the binding `k -> v`. Replaces any existing binding.
    ///
    pub def put!(k: k, v: v, m: MutMap[k, v]): Unit & Impure with Order[k] =
        let MutMap(mm) = m;
        mm := Map.insert(k, v, deref mm)

    ///
    /// Updates the mutable map `m` with the binding `k -> f(v, v1)` if `k -> v1` is in `m`.
    ///
    /// Otherwise updates the mutable map `m` with the binding `k -> v`.
    ///
    pub def putWith!(f: (v, v) -> v & ef, k: k, v: v, m: MutMap[k, v]): Unit & Impure with Order[k] =
        let MutMap(mm) = m;
        mm := Map.insertWith(f, k, v, deref mm)

    ///
    /// Updates the mutable map `m` with `k -> f(v)` if `k -> v` is in `m`.
    ///
    /// Otherwise leaves the map is unchanged.
    ///
    pub def adjust!(f: v -> v & ef, k: k, m: MutMap[k, v]): Unit & Impure with Order[k] =
        adjustWithKey!((_, v1) -> f(v1), k, m)

    ///
    /// Updates the mutable map `m` with `k -> f(k, v)` if `k -> v` is in `m`.
    ///
    /// Otherwise leaves the map is unchanged.
    ///
    pub def adjustWithKey!(f: (k, v) -> v & ef, k: k, m: MutMap[k, v]): Unit & Impure with Order[k] =
        let MutMap(mm) = m;
        mm := Map.adjustWithKey(f, k, deref mm)

    ///
    /// Removes all mappings from the mutable map `m`.
    ///
    pub def clear!(m: MutMap[k, v]): Unit & Impure =
        let MutMap(mm) = m;
        mm := Map.empty()

    ///
    /// Returns `v` if `k -> v` is in the mutable map `m`.
    ///
    /// Otherwise updates the mutable map `m` with a new mapping `k -> d` and returns d.
    ///
    pub def getOrElsePut!(k: k, d: v, m: MutMap[k, v]): v & Impure with Order[k] =
        let MutMap(mm) = m;
        match Map.get(k, deref mm) {
            case None    => mm := Map.insert(k, d, deref mm); d
            case Some(v) => v
        }

    ///
    /// Merges the mutable map `m1` into the mutable map `m2` in a left-biased manner.
    ///
    /// That is, key collisions are resolved by taking the mapping from `m1`.
    ///
    pub def merge!(m1: MutMap[k, v], m2: MutMap[k, v]): Unit & Impure with Order[k] =
        mergeWithKey!((_, v1, _) -> v1, m1, m2)

    ///
    /// Merges the mutable map `m1` into the mutable map `m2` where key collisions are resolved with the merge function `f`.
    ///
    pub def mergeWith!(f: (v, v) -> v & ef, m1: MutMap[k, v], m2: MutMap[k, v]): Unit & Impure with Order[k] =
        mergeWithKey!((_, v1, v2) -> f(v1, v2), m1, m2)

    ///
    /// Merges the mutable map `m1` into the mutable map `m2` where key collisions are resolved with the merge function `f`, taking both the key and values.
    ///
    pub def mergeWithKey!(f: (k, v, v) -> v & ef, m1: MutMap[k, v], m2: MutMap[k, v]): Unit & Impure with Order[k] =
        let MutMap(mm1) = m1;
        let MutMap(mm2) = m2;
        mm2 := Map.unionWithKey(f, deref mm1, deref mm2)

    ///
    /// Removes all mappings `k -> v` from the mutable map `m` where `v` does not satisfy the predicate function `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def refine!(f: v -> Bool, m: MutMap[k, v]): Unit & Impure with Order[k] =
        refineWithKey!((_, v) -> f(v), m)

    ///
    /// Removes all mappings `k -> v` from the mutable map `m` where `(k, v)` does not satisfy the predicate function `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def refineWithKey!(f: (k, v) -> Bool, m: MutMap[k, v]): Unit & Impure with Order[k] =
        let MutMap(mm) = m;
        mm := Map.filterWithKey(f, deref mm)

    ///
    /// Removes the mapping `k` from the mutable map `m`.
    ///
    /// Leaves the map unchanged if the mutable map `m` does not contain any mapping for `k`.
    ///
    pub def remove!(k: k, m: MutMap[k, v]): Unit & Impure with Order[k] =
        let MutMap(mm) = m;
        mm := Map.remove(k, deref mm)

    ///
    /// Applies the function `f` to every value in the mutable map `m`.
    ///
    pub def transform!(f: v -> v & ef, m: MutMap[k, v]): Unit & Impure with Order[k] =
        let MutMap(mm) = m;
        mm := Map.map(f, deref mm)

    ///
    /// Applies the function `f` to every value in the mutable map `m`.
    ///
    pub def transformWithKey!(f: (k, v) -> v & ef, m: MutMap[k, v]): Unit & Impure with Order[k] =
        let MutMap(mm) = m;
        mm := Map.mapWithKey(f, deref mm)

    ///
    /// Returns `true` if and only if all mappings in the mutable map `m1` occur in the mutable map `m2`.
    ///
    pub def isSubmapOf(m1: MutMap[k, v], m2: MutMap[k, v]): Bool & Impure with Order[k], Eq[v] =
        let MutMap(mm1) = m1;
        let MutMap(mm2) = m2;
        Map.isSubmapOf(deref mm1, deref mm2)

    ///
    /// Returns `true` if and only if all mappings in the mutable map `m1` occur in the mutable map `m2` and `m1 != m2`.
    ///
    pub def isProperSubmapOf(m1: MutMap[k, v], m2: MutMap[k, v]): Bool & Impure with Order[k], Eq[v] =
        let MutMap(mm1) = m1;
        let MutMap(mm2) = m2;
        Map.isProperSubmapOf(deref mm1, deref mm2)

    ///
    /// Alias for `findLeft`.
    ///
    /// The function `f` must be pure.
    ///
    pub def find(f: (k, v) -> Bool, m: MutMap[k, v]): Option[(k, v)] & Impure =
        findLeft(f, m)

    ///
    /// Optionally returns the first mapping of the mutable map `m` that satisfies the predicate function `f` when searching from left to right.
    ///
    /// The function `f` must be pure.
    ///
    pub def findLeft(f: (k, v) -> Bool, m: MutMap[k, v]): Option[(k, v)] & Impure =
        let MutMap(mm) = m;
        Map.findLeft(f, deref mm)

    ///
    /// Optionally returns the first mapping of the mutable map `m` that satisfies the predicate function `f` when searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    pub def findRight(f: (k, v) -> Bool, m: MutMap[k, v]): Option[(k, v)] & Impure =
        let MutMap(mm) = m;
        Map.findRight(f, deref mm)

    ///
    /// Returns a map with mappings `k => f(v)` for every `k => v` in `m`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def map(f: v1 -> v2 & ef, m: MutMap[k, v1]): MutMap[k, v2] & Impure = mapWithKey((_, v) -> f(v), m)

    ///
    /// Returns a map with mappings `k => f(k, v)` for every `k => v` in `m`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def mapWithKey(f: (k, v1) -> v2 & ef, m: MutMap[k, v1]): MutMap[k, v2] & Impure =
        let MutMap(mm) = m;
        let mms = Map.mapWithKey(f, deref mm);
        MutMap(ref mms)


    ///
    /// Alias for `foldLeftWithKey`.
    ///
    pub def foldWithKey(f: (b, k, v) -> b & ef, i: b, m: MutMap[k, v]): b & Impure  =
        foldLeftWithKey(f, i, m)

    ///
    /// Applies `f` to a start value `i` and all values in the mutable map `m` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(i, v1), v2)..., vn)`.
    ///
    pub def foldLeft(f: (b, v) -> b & ef, i: b, m: MutMap[k, v]): b & Impure =
        let MutMap(mm) = m;
        Map.foldLeft(f, i, deref mm)

    ///
    /// Applies `f` to a start value `i` and all key-value pairs in the mutable map `m` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(k2, f(k1, i, v1), v2)..., vn)`.
    ///
    pub def foldLeftWithKey(f: (b, k, v) -> b & ef, i: b, m: MutMap[k, v]): b & Impure =
        let MutMap(mm) = m;
        Map.foldLeftWithKey(f, i, deref mm)

    ///
    /// Applies `f` to a start value `i` and all values in the mutable map `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(v1, ...f(vn-1, f(vn, i)))`.
    ///
    pub def foldRight(f: (v, b) -> b & ef, i: b, m: MutMap[k, v]): b & Impure =
        foldRightWithKey((_, v, b) -> f(v, b), i, m)

    ///
    /// Applies `f` to a start value `i` and all key-value pairs in the mutable map `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(k1, v1, ...f(kn-1, vn-1, f(kn, vn, i)))`.
    ///
    pub def foldRightWithKey(f: (k, v, b) -> b & ef, i: b, m: MutMap[k, v]): b & Impure =
        let MutMap(mm) = m;
        Map.foldRightWithKey(f, i, deref mm)

    ///
    /// Lazily applies `f` to a start value `s` and all elements in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...lazy f(xn-1, lazy f(xn, s))...)`.
    /// The foldRightLazy function exists to allow early termination of a fold.
    ///
    pub def foldRightLazy(f: (v, Lazy[b]) -> b, i: b, m: MutMap[k, v]): b & Impure =
        let MutMap(mm) = m;
        Map.foldRightLazy(f, i, deref mm)

    ///
    /// Applies `f` to all values in the mutable map `m` going from left to right until a single value `v` is obtained. Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(v1, v2), v3)..., vn))`
    ///
    /// Returns `None` if `m` is the empty map.
    ///
    pub def reduceLeft(f: (v, v) -> v & ef, m: MutMap[k, v]): Option[v] & Impure =
        let MutMap(mm) = m;
        Map.reduceLeft(f, deref mm)

    ///
    /// Applies `f` to all mappings in the mutable map `m` going from left to right until a single mapping `(k, v)` is obtained. Returns `Some((k, v))`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(k1, v1, k2, v2), k3, v3)..., kn, vn))`
    ///
    /// Returns `None` if `m` is the empty map.
    ///
    pub def reduceLeftWithKey(f: (k, v, k, v) -> (k, v) & ef, m: MutMap[k, v]): Option[(k, v)] & Impure =
        let MutMap(mm) = m;
        Map.reduceLeftWithKey(f, deref mm)

    ///
    /// Applies `f` to all values in the mutable map `m` going from right to left until a single value `v` is obtained. Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(v1, ...f(vn-2, f(vn-1, vn))...))`
    ///
    /// Returns `None` if `m` is the empty map.
    ///
    pub def reduceRight(f: (v, v) -> v & ef, m: MutMap[k, v]): Option[v] & Impure =
        let MutMap(mm) = m;
        Map.reduceRight(f, deref mm)

    ///
    /// Applies `f` to all mappings in the mutable map `m` going from right to left until a single mapping `(k, v)` is obtained. Returns `Some((k, v))`.
    ///
    /// That is, the result is of the form: `Some(f(k1, v1, ...f(kn-2, vn-2, f(kn-1, vn-1, kn, vn))...))`
    ///
    /// Returns `None` if `m` is the empty map.
    ///
    pub def reduceRightWithKey(f: (k, v, k, v) -> (k, v) & ef, m: MutMap[k, v]): Option[(k, v)] & Impure =
        let MutMap(mm) = m;
        Map.reduceRightWithKey(f, deref mm)

    ///
    /// Returns the number of mappings in the mutable map `m` that satisfy the predicate function `f`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def count(f: (k, v) -> Bool & ef, m: MutMap[k, v]): Int32 & Impure =
        let MutMap(mm) = m;
        Map.count(f, deref mm)

    ///
    /// Returns the sum of all keys in the map `m`.
    ///
    pub def sumKeys(m: MutMap[Int32, v]): Int32 & Impure =
        let MutMap(mm) = m;
        Map.sumKeys(deref mm)

    ///
    /// Returns the sum of all values in the map `m`.
    ///
    pub def sumValues(m: MutMap[k, Int32]): Int32 & Impure =
        let MutMap(mm) = m;
        Map.sumValues(deref mm)

    ///
    /// Returns the sum of all key-value pairs `k => v` in the map `m` according to the function `f`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def sumWith(f: (k, v) -> Int32 & ef, m: MutMap[k, v]): Int32 & Impure =
        let MutMap(mm) = m;
        Map.sumWith(f, deref mm)

    ///
    /// Returns the product of all keys in the mutable map `m`.
    ///
    pub def productKeys(m: MutMap[Int32, v]): Int32 & Impure =
        let MutMap(mm) = m;
        Map.productKeys(deref mm)

    ///
    /// Returns the product of all values in the mutable map `m`.
    ///
    pub def productValues(m: MutMap[k, Int32]): Int32 & Impure =
        let MutMap(mm) = m;
        Map.productValues(deref mm)

    ///
    /// Returns the product of all key-value pairs `k => v` in the mutable map `m` according to the function `f`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def productWith(f: (k, v) -> Int32 & ef, m: MutMap[k, v]): Int32 & Impure =
        let MutMap(mm) = m;
        Map.productWith(f, deref mm)

    ///
    /// Returns `true` if and only if at least one mapping in the mutable map `m` satisfies the predicate function `f`.
    ///
    /// Returns `false` if `m` is the empty map.
    ///
    /// The function `f` must be pure.
    ///
    pub def exists(f: (k, v) -> Bool, m: MutMap[k, v]): Bool & Impure =
        let MutMap(mm) = m;
        Map.exists(f, deref mm)

    ///
    /// Returns `true` if and only if all mappings in the mutable map `m` satisfy the predicate function `f`.
    ///
    /// Returns `true` if `m` is the empty map.
    ///
    /// The function `f` must be pure.
    ///
    pub def forall(f: (k, v) -> Bool, m: MutMap[k, v]): Bool & Impure =
        let MutMap(mm) = m;
        Map.forall(f, deref mm)

    ///
    /// Returns a shallow copy of the mutable map `m`.
    ///
    pub def copy(m: MutMap[k, v]): MutMap[k, v] & Impure =
        let MutMap(mm) = m;
        MutMap(ref (deref mm))

    ///
    /// Returns the mutable map `m` as an immutable map.
    ///
    pub def toMap(m: MutMap[k, v]): Map[k, v] & Impure =
        let MutMap(mm) = m;
        deref mm

    ///
    /// Returns the mutable map `m` as a list of key-value pairs.
    ///
    pub def toList(m: MutMap[k, v]): List[(k, v)] & Impure =
        let MutMap(mm) = m;
        Map.toList(deref mm)

    ///
    /// Returns the mutable map `m` as a set of key-value pairs.
    ///
    pub def toSet(m: MutMap[k, v]): Set[(k, v)] & Impure with Order[k], Order[v] =
        let MutMap(mm) = m;
        Map.toSet(deref mm)

    ///
    /// Applies `f` to every element in the mutable map `m`.
    ///
    pub def foreach(f: (k, v) -> Unit & ef, m: MutMap[k, v]): Unit & Impure =
        let MutMap(mm) = m;
        Map.foreach(f, deref mm)

    ///
    /// Extracts a range of key-value pairs from the mutable map `m`.
    ///
    /// That is, the result is a list of all pairs `(k, v)` where `p(k)` returns `Equal`.
    ///
    pub def query(p: k -> Comparison, m: MutMap[k, v]): List[(k, v)] & Impure =
        let MutMap(mm) = m;
        Map.query(p, deref mm)

    ///
    /// Applies `f` to all key-value pairs `(k, v)` in the mutable map `m` where `p(k)` returns `EqualTo`.
    ///
    /// The function `f` must be impure.
    ///
    pub def queryWith(p: k -> Comparison, f: (k, v) -> Unit & ef, m: MutMap[k, v]): Unit & Impure =
        let MutMap(mm) = m;
        Map.queryWith(p, f, deref mm)

    ///
    /// Returns `true` if MutMaps `a` and `b` have the same elements, i.e. are structurally equal.
    ///
    pub def sameElements(a: MutMap[k, v], b: MutMap[k, v]): Bool & Impure with Order[k], Eq[v] =
        MutMap.isSubmapOf(a, b) and MutMap.isSubmapOf(b, a)

    ///
    /// Returns the concatenation of the string representation of each key `k`
    /// in `m` with `sep` inserted between each element.
    ///
    pub def joinKeys(sep: String, m: MutMap[k, v]): String & Impure with ToString[k] =
        let MutMap(mm) = m;
        Map.joinKeys(sep, deref mm)

    ///
    /// Returns the concatenation of the string representation of each value `v`
    /// in `m` with `sep` inserted between each element.
    ///
    pub def joinValues(sep: String, m: MutMap[k, v]): String & Impure with ToString[v] =
        let MutMap(mm) = m;
        Map.joinValues(sep, deref mm)

    ///
    /// Returns the concatenation of the string representation of each key-value pair
    /// `k => v` in `m` according to `f` with `sep` inserted between each element.
    ///
    pub def joinWith(f: (k, v) -> String & ef, sep: String, m: MutMap[k, v]): String & Impure =
        let MutMap(mm) = m;
        Map.joinWith(f, sep, deref mm)

}
