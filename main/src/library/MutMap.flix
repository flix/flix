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

    // TODO: Add vs. insert. vs. put.

    // TODO: intersect, and difference.

    // TODO: More operations?

    ///
    /// Returns `true` if and only if `m` is the empty map.
    ///
    pub def isEmpty(m: MutMap[k, v]): Bool & Impure =
        let MutMap(mm) = m;
        Map.isEmpty(deref mm)

    ///
    /// Returns a fresh empty map.
    ///
    pub def empty(): MutMap[k, v] & Impure =
        MutMap(ref Map.empty())

    ///
    /// Returns the singleton map where key `k` is mapped to value `v`.
    ///
    pub def singleton(k: k, v: v): MutMap[k, v] & Impure =
        MutMap(ref Map.singleton(k, v))

    /// Returns `true` if and only if `m` contains the key `k`.
    ///
    pub def memberOf(k: k, m: MutMap[k, v]): Bool & Impure =
        let MutMap(mm) = m;
        Map.memberOf(k, deref mm)

    ///
    /// Returns the keys of `m`.
    ///
    pub def keysOf(m: MutMap[k, v]): Set[k] & Impure =
        let MutMap(mm) = m;
        Map.keysOf(deref mm)

    ///
    /// Returns the values of `m`.
    ///
    pub def valuesOf(m: MutMap[k, v]): List[v] & Impure =
        let MutMap(mm) = m;
        Map.valuesOf(deref mm)

    ///
    /// Returns the size of `m`.
    ///
    pub def size(m: MutMap[k, v]): Int32 & Impure =
        let MutMap(mm) = m;
        Map.size(deref mm)

    ///
    /// Returns `Some(v)` if `k -> v` is in `m`.
    ///
    /// Otherwise returns `None`.
    ///
    pub def get(k: k, m: MutMap[k, v]): Option[v] & Impure =
        let MutMap(mm) = m;
        Map.get(k, deref mm)

    ///
    /// Returns `v` if `k -> v` is in `m`.
    ///
    /// Otherwise, returns `d`.
    ///
    pub def getWithDefault(k: k, d: v, m: MutMap[k, v]): v & Impure =
        let MutMap(mm) = m;
        Map.getWithDefault(k, d, deref mm)

    ///
    /// Returns `v` if `k -> v` is in `m`.
    ///
    /// Otherwise updates the map `m` with a new mapping `k -> d` and returns d.
    ///
    pub def getOrElseUpdate(k: k, d: v, m: mut MutMap[k, v]): v & impure =
        let MutMap(mm) = m;
        match Map.get(k, deref mm) {
            case None    => mm := Map.insert(k, d, deref mm); d
            case Some(v) => v
        }


    ///
    /// Updates `m` with `k -> v`.
    ///
    pub def add(k: k, v: v, m: mut MutMap[k, v]): Unit & Impure =
        let MutMap(mm) = m;
        mm := Map.insert(k, v, deref mm)

    ///
    /// Updates `m` with `k -> f(v, v1)` if `k -> v1` is in `m`.
    ///
    /// Otherwise, updates `m` with `k -> v`.
    ///
    pub def addWith(f: (v, v) -> v & e, k: k, v: v, m: mut MutMap[k, v]): Unit & Impure =
        let MutMap(mm) = m;
        mm := Map.insertWith(f, k, v, deref mm)

    ///
    /// Updates `m` with `k -> f(k, v, v1)` if `k -> v1` is in `m`.
    ///
    /// Otherwise, updates `m` with `k -> v`.
    ///
    pub def addWithKey(f: (k, v, v) -> v & e, k: k, v: v, m: mut MutMap[k, v]): Unit & Impure =
        let MutMap(mm) = m;
        mm := Map.insertWithKey(f, k, v, deref mm)

    ///
    /// Removes all mappings from `m`.
    ///
    pub def clear(m: mut MutMap[k, v]): Unit & Impure =
        let MutMap(mm) = m;
        mm := Map.empty()

    ///
    /// Returns a shallow copy of `m`.
    ///
    pub def copy(m: mut MutMap[k, v]): MutMap[k, v] & Impure =
        let MutMap(mm) = m;
        MutMap(ref (deref mm))

    ///
    /// Updates `m` with `k -> f(v)` if `k -> v` is in `m`.
    ///
    /// Otherwise, the map is unchanged.
    ///
    pub def adjust(f: v -> v & e, k: k, m: mut MutMap[k, v]): Unit & Impure =
        adjustWithKey((_, v1) -> f(v1), k, m)

    ///
    /// Updates `m` with `k -> f(k, v)` if `k -> v` is in `m`.
    ///
    /// Otherwise, the map is unchanged.
    ///
    pub def adjustWithKey(f: (k, v) -> v & e, k: k, m: mut MutMap[k, v]): Unit & Impure =
        let MutMap(mm) = m;
        mm := Map.adjustWithKey(f, k, deref mm)

    ///
    /// Updates `m` with `k -> v1` if `k -> v` is in `m` and `f(v) = Some(v1)`.
    ///
    /// Otherwise, the map is unchanged.
    ///
    pub def update(f: v -> Option[v] & e, k: k, m: mut MutMap[k, v]): Unit & Impure =
        updateWithKey((_, v1) -> f(v1), k, m)

    ///
    /// Updates `m` with `k -> v1` if `k -> v` is in `m` and `f(k, v) = Some(v1)`.
    ///
    /// Otherwise, the map is unchanged.
    ///
    pub def updateWithKey(f: (k, v) -> Option[v] & e, k: k, m: mut MutMap[k, v]): Unit & Impure =
        let MutMap(mm) = m;
        mm := Map.updateWithKey(f, k, deref mm)

    ///
    /// Replaces the mapping `fst(x) -> snd(x)` with `fst(y) -> snd(y)` if `fst(x) -> snd(x)` is in `m`.
    ///
    /// Otherwise, the map is unchanged.
    ///
    pub def replace(x: (k, v), y: (k, v), m: mut MutMap[k, v]): Unit & Impure =
        let MutMap(mm) = m;
        mm := Map.replace(x, y, deref mm)

    ///
    /// Removes the mapping `k` from the map `m`.
    ///
    pub def delete(k: k, m: MutMap[k, v]): Unit & Impure =
        let MutMap(mm) = m;
        mm := Map.delete(k, deref mm)

    ///
    /// Removes all mappings `k -> v` from `m` where `v` does not satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def refine(f: v -> Bool, m: mut MutMap[k, v]): Unit & Impure =
        refineWithKey((_, v) -> f(v), m)

    ///
    /// Removes all mappings `k -> v` from `m` where `(k, v)` does not satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def refineWithKey(f: (k, v) -> Bool, m: mut MutMap[k, v]): Unit & Impure =
        let MutMap(mm) = m;
        mm := Map.filterWithKey(f, deref mm)

    ///
    /// Merges the map `m1` into the map `m2` in a right-biased manner.
    ///
    /// That is, key collisions are resolved by taking the mapping from `m1`.
    ///
    pub def merge(m1: Map[k, v], m2: mut MutMap[k, v]): Unit & Impure =
        mergeWithKey((_, v1, _) -> v1, m1, m2)

    ///
    /// Merges the map `m1` into the map `m2` where key collisions are resolved with the merge function `f`.
    ///
    pub def mergeWith(f: (v, v) -> v & e, m1: Map[k, v], m2: mut MutMap[k, v]): Unit & Impure =
        mergeWithKey((_, v1, v2) -> f(v1, v2), m1, m2)

    ///
    /// Merges the map `m1` into the map `m2` where key collisions are resolved with the merge function `f`, taking both the key and values.
    ///
    pub def mergeWithKey(f: (k, v, v) -> v & e, m1: Map[k, v], m2: mut MutMap[k, v]): Unit & Impure =
        let MutMap(mm2) = m2;
        mm2 := Map.unionWithKey(f, m1, deref mm2)

/*
    ///
    /// Returns the left-biased intersection of `m1` and `m2`.
    ///
    /// That is, key collisions are resolved by taking the mapping from `m1`.
    ///
    /// TODO
    pub def intersection(m1: Map[k, v], m2: Map[k, v]): Map[k, v] = intersectionWithKey((_, v1, _) -> v1, m1, m2)

    ///
    /// Returns the intersection of `m1` and `m2` where key collisions are resolved with the merge function `f`.
    ///
    /// TODO
    pub def intersectionWith(f: (v, v) -> v & e, m1: Map[k, v], m2: Map[k, v]): Map[k, v] & e = intersectionWithKey((_, v1, v2) -> f(v1, v2), m1, m2)

    ///
    /// Returns the intersection of `m1` and `m2` where key collisions are resolved with the merge function `f`, taking both the key and values.
    ///
    /// TODO
    pub def intersectionWithKey(f: (k, v, v) -> v & e, m1: Map[k, v], m2: Map[k, v]): Map[k, v] & e =
        let intersect = filterWithKey((k, _) -> memberOf(k, m1), m2);
        foldRightWithKey((k, v, m) -> adjustWithKey((key, v1) -> f(key, v, v1), k, m), intersect, m1)

    ///
    /// Returns the difference of `m1` and `m2`, i.e. `m1 - m2`.
    ///
    /// That is, returns the map `m1` with the keys removed that are in `m2`.
    ///
    /// TODO
    pub def difference(m1: Map[k, v], m2: Map[k, v]): Map[k, v] = differenceWithKey((_, _, _) -> None, m1, m2)

    ///
    /// Returns the difference of `m1` and `m2`, i.e. `m1 - m2`.
    ///
    /// When a key `k` is in both `m1` and `m2`, the associated values are passed to the merge function `f`.
    /// If `f` returns `None` the mapping with `k` is thrown away (proper set difference).
    /// If `f` returns `Some(v)` the mapping `k -> v` is included in the result.
    ///
    /// TODO
    pub def differenceWith(f: (v, v) -> Option[v] & e, m1: Map[k, v], m2: Map[k, v]): Map[k, v] & e = differenceWithKey((_, v1, v2) -> f(v1, v2), m1, m2)

    ///
    /// Returns the difference of `m1` and `m2`, i.e. `m1 - m2`.
    ///
    /// When a key `k` is in both `m1` and `m2`, `k` and the associated values are passed to the merge function `f`.
    /// If `f` returns `None` the mapping with `k` is thrown away (proper set difference).
    /// If `f` returns `Some(v)` the mapping `k -> v` is included in the result.
    ///
    /// TODO
    pub def differenceWithKey(f: (k, v, v) -> Option[v] & e, m1: Map[k, v], m2: Map[k, v]): Map[k, v] & e =
        let diff = filterWithKey((k, _) -> !memberOf(k, m2), m1);
        let g = (k, v, m) -> if (memberOf(k, m1))
                                match get(k, m1) {
                                    case Some(v1) => match f(k, v1, v) {
                                        case None => m
                                        case Some(w) => insert(k, w, m)
                                    }
                                    case None => panic "unreachable"
                                }
                             else m;
        foldRightWithKey(g, diff, m2)
*/

    ///
    /// Returns `true` if and only if all mappings in `m1` occur in `m2`.
    ///
    pub def isSubmapOf(m1: MutMap[k, v], m2: MutMap[k, v]): Bool & Impure =
        let MutMap(mm1) = m1;
        let MutMap(mm2) = m2;
        Map.isSubmapOf(deref mm1, deref mm2)

    ///
    /// Returns `true` if and only if all mappings in `m1` occur in `m2` and `m1 != m2`.
    ///
    pub def isProperSubmapOf(m1: MutMap[k, v], m2: MutMap[k, v]): Bool & Impure =
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
    /// Optionally returns the first mapping of `m` that satisfies the predicate `f` when searching from left to right.
    ///
    /// The function `f` must be pure.
    ///
    pub def findLeft(f: (k, v) -> Bool, m: MutMap[k, v]): Option[(k, v)] & Impure =
        let MutMap(mm) = m;
        Map.findLeft(f, deref mm)

    ///
    /// Optionally returns the first mapping of `m` that satisfies the predicate `f` when searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    pub def findRight(f: (k, v) -> Bool, m: MutMap[k, v]): Option[(k, v)] & Impure =
        let MutMap(mm) = m;
        Map.findRight(f, deref mm)

    ///
    /// Applies the function `f` to every value in the map `m`.
    ///
    pub def transform(f: v1 -> v2 & e, m: mut MutMap[k, v1]): Unit & Impure =
        let MutMap(mm) = m;
        mm := Map.map(f, deref mm)

    ///
    /// Applies the function `f` to every value in the map `m`.
    ///
    pub def transformWithKey(f: (k, v1) -> v2 & e, m: mut MutMap[k, v1]): Unit & Impure =
        let MutMap(mm) = m;
        mm := Map.mapWithKey(f, deref mm)

    ///
    /// Alias for `foldLeft`.
    ///
    pub def fold(f: (b, v) -> b & e, s: b, m: MutMap[k, v]): b & Impure =
        foldLeft(f, s, m)

    ///
    /// Alias for `foldLeftWithKey`.
    ///
    pub def foldWithKey(f: (k, b, v) -> b & e, s: b, m: MutMap[k, v]): b & Impure  =
        foldLeftWithKey(f, s, m)

    ///
    /// Applies `f` to a start value `s` and all values in `m` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, v1), v2)..., vn)`.
    ///
    pub def foldLeft(f: (b, v) -> b & e, s: b, m: MutMap[k, v]): b & Impure =
        let MutMap(mm) = m;
        Map.foldLeft(f, s, deref mm)

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `m` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(k2, f(k1, s, v1), v2)..., vn)`.
    ///
    pub def foldLeftWithKey(f: (k, b, v) -> b & e, s: b, m: MutMap[k, v]): b & Impure =
        let MutMap(mm) = m;
        Map.foldLeftWithKey(f, s, deref mm)

    ///
    /// Applies `f` to a start value `s` and all values in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(v1, ...f(vn-1, f(vn, s)))`.
    ///
    pub def foldRight(f: (v, b) -> b & e, s: b, m: MutMap[k, v]): b & Impure =
        foldRightWithKey((_, v, b) -> f(v, b), s, m)

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(k1, v1, ...f(kn-1, vn-1, f(kn, vn, s)))`.
    ///
    pub def foldRightWithKey(f: (k, v, b) -> b & e, s: b, m: MutMap[k, v]): b & Impure =
        let MutMap(mm) = m;
        Map.foldRightWithKey(f, s, deref mm)

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
    /// Applies `f` to all values in `m` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(v1, v2), v3)..., vn))`
    ///
    /// Returns `None` if `m` is the empty map.
    ///
    pub def reduceLeft(f: (v, v) -> v & e, m: MutMap[k, v]): Option[v] & Impure =
        let MutMap(mm) = m;
        Map.reduceLeft(f, deref mm)

    ///
    /// Applies `f` to all mappings in `m` going from left to right until a single mapping `(k, v)` is obtained.  Returns `Some((k, v))`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(k1, v1, k2, v2), k3, v3)..., kn, vn))`
    ///
    /// Returns `None` if `m` is the empty map.
    ///
    pub def reduceLeftWithKey(f: (k, v, k, v) -> (k, v) & e, m: MutMap[k, v]): Option[(k, v)] & Impure =
        let MutMap(mm) = m;
        Map.reduceLeftWithKey(f, deref mm)

    ///
    /// Applies `f` to all values in `m` going from right to left until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(v1, ...f(vn-2, f(vn-1, vn))...))`
    ///
    /// Returns `None` if `m` is the empty map.
    ///
    pub def reduceRight(f: (v, v) -> v & e, m: MutMap[k, v]): Option[v] & Impure =
        let MutMap(mm) = m;
        Map.reduceRight(f, deref mm)

    ///
    /// Applies `f` to all mappings in `m` going from right to left until a single mapping `(k, v)` is obtained.  Returns `Some((k, v))`.
    ///
    /// That is, the result is of the form: `Some(f(k1, v1, ...f(kn-2, vn-2, f(kn-1, vn-1, kn, vn))...))`
    ///
    /// Returns `None` if `m` is the empty map.
    ///
    pub def reduceRightWithKey(f: (k, v, k, v) -> (k, v) & e, m: MutMap[k, v]): Option[(k, v)] & Impure =
        let MutMap(mm) = m;
        Map.reduceRightWithKey(f, deref mm)

    ///
    /// Returns the number of mappings in `m` that satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def count(f: (k, v) -> Bool, m: MutMap[k, v]): Int32 & Impure =
        let MutMap(mm) = m;
        Map.count(f, deref mm)

    ///
    /// Returns `true` if and only if at least one mapping in `m` satisfies the predicate `f`.
    ///
    /// Returns `false` if `m` is the empty map.
    ///
    /// The function `f` must be pure.
    ///
    pub def exists(f: (k, v) -> Bool, m: MutMap[k, v]): Bool & Impure =
        let MutMap(mm) = m;
        Map.exists(f, deref mm)

    ///
    /// Returns `true` if and only if all mappings in `m` satisfy the predicate `f`.
    ///
    /// Returns `true` if `m` is the empty map.
    ///
    /// The function `f` must be pure.
    ///
    pub def forall(f: (k, v) -> Bool, m: MutMap[k, v]): Bool & Impure =
        let MutMap(mm) = m;
        Map.forall(f, deref mm)


    ///
    /// Returns the map `m` as a list of key-value pairs.
    ///
    pub def toList(m: MutMap[k, v]): List[(k, v)] & Impure =
        let MutMap(mm) = m;
        Map.toList(deref mm)

    ///
    /// Returns the map `m` as a set of key-value pairs.
    ///
    pub def toSet(m: MutMap[k, v]): Set[(k, v)] & Impure =
        let MutMap(mm) = m;
        Map.toSet(deref mm)

    ///
    /// Applies `f` to every element of `xs`.
    ///
    /// The function `f` must be impure.
    ///
    pub def foreach(f: (k, v) ~> Unit, m: MutMap[k, v]): Unit & Impure =
        let MutMap(mm) = m;
        Map.foreach(f, deref mm)

    ///
    /// Returns `true` if and only if `m1` and `m2` and equal, i.e. they have the same mappings.
    ///
    pub def __eq(m1: MutMap[k, v], m2: MutMap[k, v]): Bool =
        (isSubmapOf(m1, m2) && isSubmapOf(m2, m1)) as & Pure

}