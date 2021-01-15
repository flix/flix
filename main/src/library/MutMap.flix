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

instance ToString[MutMap[k, v]] with [k : ToString, v : ToString] {
    def toString(m: MutMap[k, v]): String =
        let s = MutMap.foldRightWithKey((k, v, acc) -> "${k} -> ${v}, ${acc}", "", m) as & Pure;
        "MutMap#{${String.dropRight(2, s)}}"
}

namespace MutMap {

    ///
    /// Returns `true` if and only if `m` is the empty map.
    ///
    pub def isEmpty[k : Eq, v](m: MutMap[k, v]): Bool & Impure =
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
    pub def singleton[k : Order, v](k: k, v: v): MutMap[k, v] & Impure =
        MutMap(ref Map.singleton(k, v))

    ///
    /// Returns `true` if and only if the mutable map `m` contains the key `k`.
    ///
    pub def memberOf[k : Order, v](k: k, m: MutMap[k, v]): Bool & Impure =
        let MutMap(mm) = m;
        Map.memberOf(k, deref mm)

    ///
    /// Optionally finds `k -> v` where `k` is the smallest key according to the `Order` instance of `k`.
    ///
    pub def minimum(m: MutMap[k, v]): Option[(k, v)] & Impure =
        let MutMap(mm) = m;
        Map.minimum(deref mm)

    ///
    /// Optionally finds `k -> v` where `k` is the smallest key according to the given comparator `cmp`.
    ///
    pub def minimumBy(cmp: (k, k) -> Comparison, m: MutMap[k, v]): Option[(k, v)] & Impure =
        let MutMap(mm) = m;
        Map.minimumBy(cmp, deref mm)

    ///
    /// Optionally finds `k -> v` where `k` is the largest key according to the `Order` instance of `k`.
    ///
    pub def maximum(m: MutMap[k, v]): Option[(k, v)] & Impure =
        let MutMap(mm) = m;
        Map.maximum(deref mm)

    ///
    /// Optionally finds `k -> v` where `k` is the largest key according to the given comparator `cmp`.
    ///
    pub def maximumBy(cmp: (k, k) -> Comparison, m: MutMap[k, v]): Option[(k, v)] & Impure =
        let MutMap(mm) = m;
        Map.maximumBy(cmp, deref mm)

    ///
    /// Returns the keys of the mutable map `m`.
    ///
    pub def keysOf[k : Order, v](m: MutMap[k, v]): Set[k] & Impure =
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
    pub def get[k : Order, v](k: k, m: MutMap[k, v]): Option[v] & Impure =
        let MutMap(mm) = m;
        Map.get(k, deref mm)

    ///
    /// Returns `v` if `k -> v` is in the mutable map `m`.
    ///
    /// Otherwise returns `d`.
    ///
    pub def getWithDefault[k : Order, v](k: k, d: v, m: MutMap[k, v]): v & Impure =
        let MutMap(mm) = m;
        Map.getWithDefault(k, d, deref mm)

    ///
    /// Updates the mutable map `m` with the binding `k -> v`. Replaces any existing binding.
    ///
    pub def put![k : Order, v](k: k, v: v, m: mut MutMap[k, v]): Unit & Impure =
        let MutMap(mm) = m;
        mm := Map.insert(k, v, deref mm)

    ///
    /// Updates the mutable map `m` with `k -> f(v)` if `k -> v` is in `m`.
    ///
    /// Otherwise leaves the map is unchanged.
    ///
    pub def adjust![k : Order, v, e](f: v -> v & e, k: k, m: mut MutMap[k, v]): Unit & Impure =
        adjustWithKey!((_, v1) -> f(v1), k, m)

    ///
    /// Updates the mutable map `m` with `k -> f(k, v)` if `k -> v` is in `m`.
    ///
    /// Otherwise leaves the map is unchanged.
    ///
    pub def adjustWithKey![k : Order, v, e](f: (k, v) -> v & e, k: k, m: mut MutMap[k, v]): Unit & Impure =
        let MutMap(mm) = m;
        mm := Map.adjustWithKey(f, k, deref mm)

    ///
    /// Removes all mappings from the mutable map `m`.
    ///
    pub def clear!(m: mut MutMap[k, v]): Unit & Impure =
        let MutMap(mm) = m;
        mm := Map.empty()

    ///
    /// Returns `v` if `k -> v` is in the mutable map `m`.
    ///
    /// Otherwise updates the mutable map `m` with a new mapping `k -> d` and returns d.
    ///
    pub def getOrElsePut![k : Order, v](k: k, d: v, m: mut MutMap[k, v]): v & Impure =
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
    pub def merge![k : Order, v](m1: MutMap[k, v], m2: mut MutMap[k, v]): Unit & Impure =
        mergeWithKey!((_, v1, _) -> v1, m1, m2)

    ///
    /// Merges the mutable map `m1` into the mutable map `m2` where key collisions are resolved with the merge function `f`.
    ///
    pub def mergeWith![k : Order, v, e](f: (v, v) -> v & e, m1: MutMap[k, v], m2: mut MutMap[k, v]): Unit & Impure =
        mergeWithKey!((_, v1, v2) -> f(v1, v2), m1, m2)

    ///
    /// Merges the mutable map `m1` into the mutable map `m2` where key collisions are resolved with the merge function `f`, taking both the key and values.
    ///
    pub def mergeWithKey![k : Order, v, e](f: (k, v, v) -> v & e, m1: MutMap[k, v], m2: mut MutMap[k, v]): Unit & Impure =
        let MutMap(mm1) = m1;
        let MutMap(mm2) = m2;
        mm2 := Map.unionWithKey(f, deref mm1, deref mm2)

    ///
    /// Removes all mappings `k -> v` from the mutable map `m` where `v` does not satisfy the predicate function `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def refine![k : Order, v](f: v -> Bool, m: mut MutMap[k, v]): Unit & Impure =
        refineWithKey!((_, v) -> f(v), m)

    ///
    /// Removes all mappings `k -> v` from the mutable map `m` where `(k, v)` does not satisfy the predicate function `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def refineWithKey![k : Order, v](f: (k, v) -> Bool, m: mut MutMap[k, v]): Unit & Impure =
        let MutMap(mm) = m;
        mm := Map.filterWithKey(f, deref mm)

    ///
    /// Removes the mapping `k` from the mutable map `m`.
    ///
    /// Leaves the map unchanged if the mutable map `m` does not contain any mapping for `k`.
    ///
    pub def remove![k : Order, v](k: k, m: mut MutMap[k, v]): Unit & Impure =
        let MutMap(mm) = m;
        mm := Map.delete(k, deref mm)

    ///
    /// Applies the function `f` to every value in the mutable map `m`.
    ///
    pub def transform![k : Order, v, e](f: v -> v & e, m: mut MutMap[k, v]): Unit & Impure =
        let MutMap(mm) = m;
        mm := Map.map(f, deref mm)

    ///
    /// Applies the function `f` to every value in the mutable map `m`.
    ///
    pub def transformWithKey![k : Order, v, e](f: (k, v) -> v & e, m: mut MutMap[k, v]): Unit & Impure =
        let MutMap(mm) = m;
        mm := Map.mapWithKey(f, deref mm)

    ///
    /// Returns `true` if and only if all mappings in the mutable map `m1` occur in the mutable map `m2`.
    ///
    pub def isSubmapOf[k : Order, v : Eq](m1: MutMap[k, v], m2: MutMap[k, v]): Bool & Impure =
        let MutMap(mm1) = m1;
        let MutMap(mm2) = m2;
        Map.isSubmapOf(deref mm1, deref mm2)

    ///
    /// Returns `true` if and only if all mappings in the mutable map `m1` occur in the mutable map `m2` and `m1 != m2`.
    ///
    pub def isProperSubmapOf[k : Order, v : Eq](m1: MutMap[k, v], m2: MutMap[k, v]): Bool & Impure =
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
    /// Alias for `foldLeft`.
    ///
    pub def fold(f: (b, v) -> b & e, i: b, m: MutMap[k, v]): b & Impure =
        foldLeft(f, i, m)

    ///
    /// Alias for `foldLeftWithKey`.
    ///
    pub def foldWithKey(f: (b, k, v) -> b & e, i: b, m: MutMap[k, v]): b & Impure  =
        foldLeftWithKey(f, i, m)

    ///
    /// Applies `f` to a start value `i` and all values in the mutable map `m` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(i, v1), v2)..., vn)`.
    ///
    pub def foldLeft(f: (b, v) -> b & e, i: b, m: MutMap[k, v]): b & Impure =
        let MutMap(mm) = m;
        Map.foldLeft(f, i, deref mm)

    ///
    /// Applies `f` to a start value `i` and all key-value pairs in the mutable map `m` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(k2, f(k1, i, v1), v2)..., vn)`.
    ///
    pub def foldLeftWithKey(f: (b, k, v) -> b & e, i: b, m: MutMap[k, v]): b & Impure =
        let MutMap(mm) = m;
        Map.foldLeftWithKey(f, i, deref mm)

    ///
    /// Applies `f` to a start value `i` and all values in the mutable map `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(v1, ...f(vn-1, f(vn, i)))`.
    ///
    pub def foldRight(f: (v, b) -> b & e, i: b, m: MutMap[k, v]): b & Impure =
        foldRightWithKey((_, v, b) -> f(v, b), i, m)

    ///
    /// Applies `f` to a start value `i` and all key-value pairs in the mutable map `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(k1, v1, ...f(kn-1, vn-1, f(kn, vn, i)))`.
    ///
    pub def foldRightWithKey(f: (k, v, b) -> b & e, i: b, m: MutMap[k, v]): b & Impure =
        let MutMap(mm) = m;
        Map.foldRightWithKey(f, i, deref mm)

    ///
    /// Alias for `reduceLeft`.
    ///
    pub def reduce(f: (v, v) -> v & e, m: MutMap[k, v]): Option[v] & Impure =
        reduceLeft(f, m)

    ///
    /// Alias for `reduceLeftWithKey`.
    ///
    pub def reduceWithKey(f: (k, v, k, v) -> (k, v) & e, m: MutMap[k, v]): Option[(k, v)] & Impure =
        reduceLeftWithKey(f, m)

    ///
    /// Applies `f` to all values in the mutable map `m` going from left to right until a single value `v` is obtained. Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(v1, v2), v3)..., vn))`
    ///
    /// Returns `None` if `m` is the empty map.
    ///
    pub def reduceLeft(f: (v, v) -> v & e, m: MutMap[k, v]): Option[v] & Impure =
        let MutMap(mm) = m;
        Map.reduceLeft(f, deref mm)

    ///
    /// Applies `f` to all mappings in the mutable map `m` going from left to right until a single mapping `(k, v)` is obtained. Returns `Some((k, v))`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(k1, v1, k2, v2), k3, v3)..., kn, vn))`
    ///
    /// Returns `None` if `m` is the empty map.
    ///
    pub def reduceLeftWithKey(f: (k, v, k, v) -> (k, v) & e, m: MutMap[k, v]): Option[(k, v)] & Impure =
        let MutMap(mm) = m;
        Map.reduceLeftWithKey(f, deref mm)

    ///
    /// Applies `f` to all values in the mutable map `m` going from right to left until a single value `v` is obtained. Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(v1, ...f(vn-2, f(vn-1, vn))...))`
    ///
    /// Returns `None` if `m` is the empty map.
    ///
    pub def reduceRight(f: (v, v) -> v & e, m: MutMap[k, v]): Option[v] & Impure =
        let MutMap(mm) = m;
        Map.reduceRight(f, deref mm)

    ///
    /// Applies `f` to all mappings in the mutable map `m` going from right to left until a single mapping `(k, v)` is obtained. Returns `Some((k, v))`.
    ///
    /// That is, the result is of the form: `Some(f(k1, v1, ...f(kn-2, vn-2, f(kn-1, vn-1, kn, vn))...))`
    ///
    /// Returns `None` if `m` is the empty map.
    ///
    pub def reduceRightWithKey(f: (k, v, k, v) -> (k, v) & e, m: MutMap[k, v]): Option[(k, v)] & Impure =
        let MutMap(mm) = m;
        Map.reduceRightWithKey(f, deref mm)

    ///
    /// Returns the number of mappings in the mutable map `m` that satisfy the predicate function `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def count(f: (k, v) -> Bool, m: MutMap[k, v]): Int32 & Impure =
        let MutMap(mm) = m;
        Map.count(f, deref mm)

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
    pub def toImmutable(m: MutMap[k, v]): Map[k, v] & Impure =
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
    pub def toSet[k : Order, v : Order](m: MutMap[k, v]): Set[(k, v)] & Impure =
        let MutMap(mm) = m;
        Map.toSet(deref mm)

    ///
    /// Applies `f` to every element in the mutable map `m`.
    ///
    /// The function `f` must be impure.
    ///
    pub def foreach(f: (k, v) ~> Unit, m: MutMap[k, v]): Unit & Impure =
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

}
