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

    // TODO: Extra operations: clear.

    ///
    /// Returns a fresh empty map.
    ///
    pub def empty(): MutMap[k, v] & Impure =
        MutMap(ref Map.empty())

    ///
    /// Returns the size of `m`.
    ///
    pub def size(m: MutMap[k, v]): Int32 & Impure =
        let MutMap(mm) = m;
        Map.size(deref mm)

    ///
    /// Returns the singleton map where key `k` is mapped to value `v`.
    ///
    pub def singleton(k: k, v: v): MutMap[k, v] & Impure =
        MutMap(ref Map.singleton(k, v))

    ///
    /// Returns `true` if and only if `m` is the empty map.
    ///
    pub def isEmpty(m: MutMap[k, v]): Bool & Impure =
        let MutMap(mm) = m;
        Map.isEmpty(deref mm)

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

/*

    ///
    /// Updates `m` with `k -> v`.
    ///
    /// TODO
    pub def insert(k: k, v: v, m: Map[k, v]): Map[k, v] =
        insertWithKey((_, v1, _) -> v1, k, v, m)

    ///
    /// Updates `m` with `k -> f(v, v1)` if `k -> v1` is in `m`.
    ///
    /// Otherwise, updates `m` with `k -> v`.
    ///
    /// TODO
    pub def insertWith(f: (v, v) -> v & e, k: k, v: v, m: Map[k, v]): Map[k, v] & e =
        insertWithKey((_, v1, v2) -> f(v1, v2), k, v, m)

    ///
    /// Updates `m` with `k -> f(k, v, v1)` if `k -> v1` is in `m`.
    ///
    /// Otherwise, updates `m` with `k -> v`.
    ///
    /// TODO
    pub def insertWithKey(f: (k, v, v) -> v & e, k: k, v: v, m: Map[k, v]): Map[k, v] & e =
        if (memberOf(k, m)) adjustWithKey((a, b) -> f(a, v, b), k, m)
        else
            let Map(xs) = m;
                Map((k, v) :: xs)






    ///
    /// Updates `m` with `k -> f(v)` if `k -> v` is in `m`.
    ///
    /// Otherwise, returns `m`.
    ///
    /// TODO
    pub def adjust(f: v -> v & e, k: k, m: Map[k, v]): Map[k, v] & e = adjustWithKey((_, v1) -> f(v1), k, m)

    ///
    /// Updates `m` with `k -> f(k, v)` if `k -> v` is in `m`. Otherwise, returns `m`.
    ///
    /// TODO
    pub def adjustWithKey(f: (k, v) -> v & e, k: k, m: Map[k, v]): Map[k, v] & e =
        let Map(xs) = m;
            Map(List.map(match (key, v) -> if (key == k) (k, f(k, v)) else (key, v), xs))

    ///
    /// Updates `m` with `k -> v1` if `k -> v` is in `m` and `f(v) = Some(v1)`. Otherwise, returns `m`.
    ///
    /// TODO
    pub def update(f: v -> Option[v] & e, k: k, m: Map[k, v]): Map[k, v] & e = updateWithKey((_, v1) -> f(v1), k, m)

    ///
    /// Updates `m` with `k -> v1` if `k -> v` is in `m` and `f(k, v) = Some(v1)`. Otherwise, returns `m`.
    ///
    /// TODO
    pub def updateWithKey(f: (k, v) -> Option[v] & e, k: k, m: Map[k, v]): Map[k, v] & e =
        let Map(xs) = m;
            Map(List.map(match (key, v) -> if (key == k) (k, Option.getWithDefault(f(k, v), v)) else (key, v), xs))

    ///
    /// Replaces the mapping `fst(x) -> snd(x)` with `fst(y) -> snd(y)` if `fst(x) -> snd(x)` is in `m`. Otherwise, returns `m`.
    ///
    /// Note: The returned map may be smaller than `m`.
    ///
    /// TODO
    pub def replace(x: (k, v), y: (k, v), m: Map[k, v]): Map[k, v] =
        let (k, v) = x;
            match get(k, m) {
                case None => m
                case Some(w) => if (v == w) insert(fst(y), snd(y), delete(k, m)) else m
            }






    ///
    /// Removes the mapping `k` from the map `m`.
    ///
    /// TODO
    pub def delete(k: k, m: Map[k, v]): Map[k, v] =
        let Map(xs) = m;
            Map(List.filter(match (key, _) -> key != k, xs))


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

  /*
    ///
    /// Returns a map of all mappings `k -> v` in `m` where `v` satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    /// TODO
    pub def filter(f: v -> Bool, m: Map[k, v]): Map[k, v] = filterWithKey((_, v) -> f(v), m)

    ///
    /// Returns a map of all mappings `k -> v` in `m` where `(k, v)` satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    /// TODO
    pub def filterWithKey(f: (k, v) -> Bool, m: Map[k, v]): Map[k, v] =
        let Map(xs) = m;
            Map(List.filter(x -> f(fst(x), snd(x)), xs))

    ///
    /// Returns a map with mappings `k -> f(v)` for every `k -> v` in `m`.
    ///
    /// TODO
    pub def map(f: v1 -> v2 & e, m: Map[k, v1]): Map[k, v2] & e = mapWithKey((_, v) -> f(v), m)

    ///
    /// Returns a map with mappings `k -> f(k, v)` for every `k -> v` in `m`.
    ///
    /// TODO
    pub def mapWithKey(f: (k, v1) -> v2 & e, m: Map[k, v1]): Map[k, v2] & e =
        let Map(xs) = m;
            Map(List.map(match (k, v) -> (k, f(k, v)), xs))






    ///
    /// Alias for `foldLeft`.
    ///
    /// TODO
    pub def fold(f: (b, v) -> b & e, s: b, m: Map[k, v]): b & e = foldLeft(f, s, m)

    ///
    /// Alias for `foldLeftWithKey`.
    ///
    /// TODO
    pub def foldWithKey(f: (k, b, v) -> b & e, s: b, m: Map[k, v]): b & e = foldLeftWithKey(f, s, m)

    ///
    /// Applies `f` to a start value `s` and all values in `m` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, v1), v2)..., vn)`.
    ///
    /// TODO
    pub def foldLeft(f: (b, v) -> b & e, s: b, m: Map[k, v]): b & e = foldLeftWithKey((_, b, v) -> f(b, v), s, m)

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `m` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(k2, f(k1, s, v1), v2)..., vn)`.
    ///
    /// TODO
    pub def foldLeftWithKey(f: (k, b, v) -> b & e, s: b, m: Map[k, v]): b & e =
        let Map(xs) = m;
            List.foldLeft((y, kv) -> f(fst(kv), y, snd(kv)), s, xs)

    ///
    /// Applies `f` to a start value `s` and all values in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(v1, ...f(vn-1, f(vn, s)))`.
    ///
    /// TODO
    pub def foldRight(f: (v, b) -> b & e, s: b, m: Map[k, v]): b & e =
        foldRightWithKey((_, v, b) -> f(v, b), s, m)

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(k1, v1, ...f(kn-1, vn-1, f(kn, vn, s)))`.
    ///
    /// TODO
    pub def foldRightWithKey(f: (k, v, b) -> b & e, s: b, m: Map[k, v]): b & e =
        let Map(xs) = m;
            List.foldRight((kv, y) -> f(fst(kv), snd(kv), y), s, xs)

    ///
    /// Alias for `reduceLeft`.
    ///
    /// TODO
    pub def reduce(f: (v, v) -> v & e, m: Map[k, v]): Option[v] & e = reduceLeft(f, m)

    ///
    /// Alias for `reduceLeftWithKey`.
    ///
    /// TODO
    pub def reduceWithKey(f: (k, v, k, v) -> (k, v) & e, m: Map[k, v]): Option[(k, v)] = reduceLeftWithKey(f, m)

    ///
    /// Applies `f` to all values in `m` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(v1, v2), v3)..., vn))`
    ///
    /// Returns `None` if `m` is the empty map.
    ///
    /// TODO
    pub def reduceLeft(f: (v, v) -> v & e, m: Map[k, v]): Option[v] & e = List.reduceLeft(f, m.valuesOf())

    ///
    /// Applies `f` to all mappings in `m` going from left to right until a single mapping `(k, v)` is obtained.  Returns `Some((k, v))`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(k1, v1, k2, v2), k3, v3)..., kn, vn))`
    ///
    /// Returns `None` if `m` is the empty map.
    ///
    /// TODO
    pub def reduceLeftWithKey(f: (k, v, k, v) -> (k, v) & e, m: Map[k, v]): Option[(k, v)] =
        let Map(xs) = m;
            List.reduceLeft((kv1, kv2) -> f(fst(kv1), snd(kv1), fst(kv2), snd(kv2)), xs)

    ///
    /// Applies `f` to all values in `m` going from right to left until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(v1, ...f(vn-2, f(vn-1, vn))...))`
    ///
    /// Returns `None` if `m` is the empty map.
    ///
    /// TODO
    pub def reduceRight(f: (v, v) -> v & e, m: Map[k, v]): Option[v] & e =
        let Map(xs) = m;
            List.reduceRight(f, List.map(kv -> snd(kv), xs))

    ///
    /// Applies `f` to all mappings in `m` going from right to left until a single mapping `(k, v)` is obtained.  Returns `Some((k, v))`.
    ///
    /// That is, the result is of the form: `Some(f(k1, v1, ...f(kn-2, vn-2, f(kn-1, vn-1, kn, vn))...))`
    ///
    /// Returns `None` if `m` is the empty map.
    ///
    /// TODO
    pub def reduceRightWithKey(f: (k, v, k, v) -> (k, v) & e, m: Map[k, v]): Option[(k, v)] & e =
        let Map(xs) = m;
            List.reduceRight((kv1, kv2) -> f(fst(kv1), snd(kv1), fst(kv2), snd(kv2)), xs)
*/


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



/*
    ///
    /// Returns `true` if and only if all mappings in `m` satisfy the predicate `f`.
    ///
    /// Returns `true` if `m` is the empty map.
    ///
    /// The function `f` must be pure.
    ///
    /// TODO
    pub def forall(f: (k, v) -> Bool, m: Map[k, v]): Bool =
        let Map(xs) = m;
            List.forall(x -> f(fst(x), snd(x)), xs)






    ///
    /// Returns the left-biased union of `m1` and `m2`.
    ///
    /// That is, key collisions are resolved by taking the mapping from `m1`.
    ///
    /// TODO
    pub def union(m1: Map[k, v], m2: Map[k, v]): Map[k, v] = unionWithKey((_, v1, _) -> v1, m1, m2)

    ///
    /// Returns the union of `m1` and `m2` where key collisions are resolved with the merge function `f`.
    ///
    /// TODO
    pub def unionWith(f: (v, v) -> v & e, m1: Map[k, v], m2: Map[k, v]): Map[k, v] & e = unionWithKey((_, v1, v2) -> f(v1, v2), m1, m2)

    ///
    /// Returns the union of `m1` and `m2` where key collisions are resolved with the merge function `f`, taking both the key and values.
    ///
    /// TODO
    pub def unionWithKey(f: (k, v, v) -> v & e, m1: Map[k, v], m2: Map[k, v]): Map[k, v] & e =
        foldRightWithKey((k, v, m) -> insertWithKey((key, v1, v2) -> f(key, v1, v2), k, v, m) , m2, m1)

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






    ///
    /// Returns the map `m` as a list of key-value pairs.
    ///
    /// TODO
    pub def toList(m: Map[k, v]): List[(k, v)] =
        let Map(xs) = m;
            xs

    ///
    /// Returns the map `m` as a set of key-value pairs.
    ///
    /// TODO
    pub def toSet(m: Map[k, v]): Set[(k, v)] =
        let Map(xs) = m;
            List.toSet(xs)
*/

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