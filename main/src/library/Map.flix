/*
 * Copyright 2017 Liam Palmer
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
/// The Map type.
///
/// A map is currently represented internally as a list.
///
enum Map[k, v] {
    case Map(List[(k, v)])
}

namespace Map {

    ///
    /// Returns the size of `m`.
    ///
    pub def size[k, v](m: Map[k, v]): Int32 =
        let Map(xs) = m;
            List.length(xs)






    ///
    /// Returns the empty map.
    /// `@{}` is syntactic sugar for `empty` (`@{} = empty()`).
    ///
    pub def empty[k, v](): Map[k, v] = Map(Nil)

    ///
    /// Returns the singleton map where key `k` is mapped to value `v`.
    /// `@{k -> v}` is syntactic sugar for `singleton` (`@{k -> v} = singleton(k, v)`).
    ///
    pub def singleton[k, v](k: k, v: v): Map[k, v] = Map((k, v) :: Nil)






    ///
    /// Returns `true` if and only if `m` is the empty map, i.e. `Map(Nil)`.
    ///
    pub def isEmpty[k, v](m: Map[k, v]): Bool =
        let Map(xs) = m;
            List.isEmpty(xs)

    ///
    /// Returns `Some(v)` if `k -> v` is in `m`.
    /// Otherwise returns `None`.
    ///
    pub def get[k, v](k: k, m: Map[k, v]): Option[v] =
        let Map(xs) = m;
            List.findMap(match (key, v) -> if (key != k) None else Some(v), xs)

    ///
    /// Returns `v` if `k -> v` is in `m`.
    /// Otherwise, returns `d`.
    ///
    pub def getWithDefault[k, v](k: k, d: v, m: Map[k, v]): v = Option.getWithDefault(get(k, m), d)

    ///
    /// Returns `true` if and only if `m` contains the key `k`.
    ///
    pub def memberOf[k, v](k: k, m: Map[k, v]): Bool = match m with {
        case Map(Nil) => false
        case Map((a, _) :: rs) => if (a == k) true else memberOf(k, Map(rs))
    }

    ///
    /// Returns the keys of `m`.
    ///
    pub def keysOf[k, v](m: Map[k, v]): Set[k] =
        let Map(xs) = m;
            List.toSet(fst(List.unzip(xs)))

    ///
    /// Returns the values of `m`.
    ///
    pub def valuesOf[k, v](m: Map[k, v]): List[v] =
        let Map(xs) = m;
            snd(List.unzip(xs))






    ///
    /// Updates `m` with `k -> v`.
    ///
    pub def insert[k, v](k: k, v: v, m: Map[k, v]): Map[k, v] =
        insertWithKey((_, v1, _) -> v1, k, v, m)

    ///
    /// Updates `m` with `k -> f(v, v1)` if `k -> v1` is in `m`.
    /// Otherwise, updates `m` with `k -> v`.
    ///
    pub def insertWith[k, v](f: (v, v) -> v, k: k, v: v, m: Map[k, v]): Map[k, v] =
        insertWithKey((_, v1, v2) -> f(v1, v2), k, v, m)

    ///
    /// Updates `m` with `k -> f(k, v, v1)` if `k -> v1` is in `m`.
    /// Otherwise, updates `m` with `k -> v`.
    ///
    pub def insertWithKey[k, v](f: (k, v, v) -> v, k: k, v: v, m: Map[k, v]): Map[k, v] =
        if (memberOf(k, m)) adjustWithKey((a, b) -> f(a, v, b), k, m)
        else
            let Map(xs) = m;
                Map((k, v) :: xs)






    ///
    /// Updates `m` with `k -> f(v)` if `k -> v` is in `m`.
    /// Otherwise, returns `m`.
    ///
    pub def adjust[k, v](f: v -> v, k: k, m: Map[k, v]): Map[k, v] = adjustWithKey((_, v1) -> f(v1), k, m)

    ///
    /// Updates `m` with `k -> f(k, v)` if `k -> v` is in `m`.
    /// Otherwise, returns `m`.
    ///
    pub def adjustWithKey[k, v](f: (k, v) -> v, k: k, m: Map[k, v]): Map[k, v] =
        let Map(xs) = m;
            Map(List.map(match (key, v) -> if (key == k) (k, f(k, v)) else (key, v), xs))

    ///
    /// Updates `m` with `k -> v1` if `k -> v` is in `m` and `f(v) = Some(v1).
    /// Otherwise, returns `m`.
    ///
    pub def update[k, v](f: v -> Option[v], k: k, m: Map[k, v]): Map[k, v] = updateWithKey((_, v1) -> f(v1), k, m)

    ///
    /// Updates `m` with `k -> v1` if `k -> v` is in `m` and `f(k, v) = Some(v1).
    /// Otherwise, returns `m`.
    ///
    pub def updateWithKey[k, v](f: (k, v) -> Option[v], k: k, m: Map[k, v]): Map[k, v] =
        let Map(xs) = m;
            Map(List.map(match (key, v) -> if (key == k) (k, Option.getWithDefault(f(k, v), v)) else (key, v), xs))

    ///
    /// Replaces the mapping `fst(x) -> snd(x)` with `fst(y) -> snd(y)` if `fst(x) -> snd(x)` is in `m`.
    /// Otherwise, returns `m`.
    /// Note: The returned map may be smaller than `m`.
    ///
    pub def replace[k, v](x: (k, v), y: (k, v), m: Map[k, v]): Map[k, v] =
        let (k, v) = x;
            match get(k, m) with {
                case None => m
                case Some(w) => if (v == w) insert(fst(y), snd(y), delete(k, m)) else m
            }






    ///
    ///
    ///
    pub def delete[k, v](k: k, m: Map[k, v]): Map[k, v] =
        let Map(xs) = m;
            Map(List.filter(match (key, _) -> key != k, xs))






    ///
    /// Returns `true` if and only if all mappings in `m1` occur in `m2`.
    ///
    pub def isSubmapOf[k, v](m1: Map[k, v], m2: Map[k, v]): Bool =
        let Map(xs) = m1;
        let Map(ys) = m2;
            List.forall(kv -> List.memberOf(kv, ys), xs)

    ///
    /// Returns `true` if and only if all mappings in `m1` occur in `m2` and `m1 != m2`.
    ///
    pub def isProperSubmapOf[k, v](m1: Map[k, v], m2: Map[k, v]): Bool = isSubmapOf(m1, m2) && size(m1) != size(m2)

    ///
    /// Alias for `findLeft`.
    ///
    pub def find[k, v](f: (k, v) -> Bool, m: Map[k, v]): Option[(k, v)] = findLeft(f, m)

    ///
    /// Optionally returns the first mapping of `m` that satisfies the predicate `f` when searching from left to right.
    ///
    pub def findLeft[k, v](f: (k, v) -> Bool, m: Map[k, v]): Option[(k, v)] =
        let Map(xs) = m;
            List.findLeft(x -> f(fst(x), snd(x)), xs)

    ///
    /// Optionally returns the first mapping of `m` that satisfies the predicate `f` when searching from right to left.
    ///
    pub def findRight[k, v](f: (k, v) -> Bool, m: Map[k, v]): Option[(k, v)] =
        let Map(xs) = m;
            List.findRight(x -> f(fst(x), snd(x)), xs)






    ///
    /// Returns a map of all mappings `k -> v` in `m` where `v` satisfies the predicate `f`.
    ///
    pub def filter[k, v](f: v -> Bool, m: Map[k, v]): Map[k, v] = filterWithKey((_, v) -> f(v), m)

    ///
    /// Returns a map of all mappings `k -> v` in `m` where `(k, v)` satisfies the predicate `f`.
    ///
    pub def filterWithKey[k, v](f: (k, v) -> Bool, m: Map[k, v]): Map[k, v] =
        let Map(xs) = m;
            Map(List.filter(x -> f(fst(x), snd(x)), xs))

    ///
    /// Returns a map with mappings `k -> f(v)` for every `k -> v` in `m`.
    ///
    pub def map[k, v1, v2](f: v1 -> v2, m: Map[k, v1]): Map[k, v2] = mapWithKey((_, v) -> f(v), m)

    ///
    /// Returns a map with mappings `k -> f(k, v)` for every `k -> v` in `m`.
    ///
    pub def mapWithKey[k, v1, v2](f: (k, v1) -> v2, m: Map[k, v1]): Map[k, v2] =
        let Map(xs) = m;
            Map(List.map(match (k, v) -> (k, f(k, v)), xs))






    ///
    /// Alias for `foldLeft`.
    ///
    pub def fold[k, v, b](f: (b, v) -> b, s: b, m: Map[k, v]): b = foldLeft(f, s, m)

    ///
    /// Alias for `foldLeftWithKey`.
    ///
    pub def foldWithKey[k, v, b](f: (k, b, v) -> b, s: b, m: Map[k, v]): b = foldLeftWithKey(f, s, m)

    ///
    /// Applies `f` to a start value `s` and all values in `m` going from left to right.
    /// That is, the result is of the form: `f(...f(f(s, v1), v2)..., vn)`.
    ///
    pub def foldLeft[k, v, b](f: (b, v) -> b, s: b, m: Map[k, v]): b = foldLeftWithKey((_, b, v) -> f(b, v), s, m)

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `m` going from left to right.
    /// That is, the result is of the form: `f(...f(k2, f(k1, s, v1), v2)..., vn)`.
    ///
    pub def foldLeftWithKey[k, v, b](f: (k, b, v) -> b, s: b, m: Map[k, v]): b =
        let Map(xs) = m;
            List.foldLeft((y, kv) -> f(fst(kv), y, snd(kv)), s, xs)

    ///
    /// Applies `f` to a start value `s` and all values in `m` going from right to left.
    /// That is, the result is of the form: `f(v1, ...f(vn-1, f(vn, s)))`.
    ///
    pub def foldRight[k, v, b](f: (v, b) -> b, s: b, m: Map[k, v]): b =
        foldRightWithKey((_, v, b) -> f(v, b), s, m)

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `m` going from right to left.
    /// That is, the result is of the form: `f(k1, v1, ...f(kn-1, vn-1, f(kn, vn, s)))`.
    ///
    pub def foldRightWithKey[k, v, b](f: (k, v, b) -> b, s: b, m: Map[k, v]): b =
        let Map(xs) = m;
            List.foldRight((kv, y) -> f(fst(kv), snd(kv), y), s, xs)

    ///
    /// Alias for `reduceLeft`.
    ///
    pub def reduce[k, v](f: (v, v) -> v, m: Map[k, v]): Option[v] = reduceLeft(f, m)

    ///
    /// Alias for `reduceLeftWithKey`.
    ///
    pub def reduceWithKey[k, v](f: (k, v, k, v) -> (k, v), m: Map[k, v]): Option[(k, v)] = reduceLeftWithKey(f, m)

    ///
    /// Applies `f` to all values in `m` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    /// That is, the result is of the form: `Some(f(...f(f(v1, v2), v3)..., vn))`
    /// Returns `None` if `m` is the empty map.
    ///
    pub def reduceLeft[k, v](f: (v, v) -> v, m: Map[k, v]): Option[v] = List.reduceLeft(f, m.valuesOf())

    ///
    /// Applies `f` to all mappings in `m` going from left to right until a single mapping `(k, v)` is obtained.  Returns `Some((k, v))`.
    /// That is, the result is of the form: `Some(f(...f(f(k1, v1, k2, v2), k3, v3)..., kn, vn))`
    /// Returns `None` if `m` is the empty map.
    ///
    pub def reduceLeftWithKey[k, v](f: (k, v, k, v) -> (k, v), m: Map[k, v]): Option[(k, v)] =
        let Map(xs) = m;
            List.reduceLeft((kv1, kv2) -> f(fst(kv1), snd(kv1), fst(kv2), snd(kv2)), xs)

    ///
    /// Applies `f` to all values in `m` going from right to left until a single value `v` is obtained.  Returns `Some(v)`.
    /// That is, the result is of the form: `Some(f(v1, ...f(vn-2, f(vn-1, vn))...))`
    /// Returns `None` if `m` is the empty map.
    ///
    pub def reduceRight[k, v](f: (v, v) -> v, m: Map[k, v]): Option[v] =
        let Map(xs) = m;
            List.reduceRight(f, List.map(kv -> snd(kv), xs))

    ///
    /// Applies `f` to all mappings in `m` going from right to left until a single mapping `(k, v)` is obtained.  Returns `Some((k, v))`.
    /// That is, the result is of the form: `Some(f(k1, v1, ...f(kn-2, vn-2, f(kn-1, vn-1, kn, vn))...))`
    /// Returns `None` if `m` is the empty map.
    ///
    pub def reduceRightWithKey[k, v](f: (k, v, k, v) -> (k, v), m: Map[k, v]): Option[(k, v)] =
        let Map(xs) = m;
            List.reduceRight((kv1, kv2) -> f(fst(kv1), snd(kv1), fst(kv2), snd(kv2)), xs)






    ///
    /// Returns the number of mappings in `m` that satisfy the predicate `f`.
    ///
    pub def count[k, v](f: (k, v) -> Bool, m: Map[k, v]): Int32 =
        let Map(xs) = m;
            List.count(x -> f(fst(x), snd(x)), xs)

    ///
    /// Returns `true` if and only if at least one mapping in `m` satisfies the predicate `f`.
    /// Returns `false` if `m` is the empty map.
    ///
    pub def exists[k, v](f: (k, v) -> Bool, m: Map[k, v]): Bool =
        let Map(xs) = m;
            List.exists(x -> f(fst(x), snd(x)), xs)

    ///
    /// Returns `true` if and only if all mappings in `m` satisfy the predicate `f`.
    /// Returns `true` if `m` is the empty map.
    ///
    pub def forall[k, v](f: (k, v) -> Bool, m: Map[k, v]): Bool =
        let Map(xs) = m;
            List.forall(x -> f(fst(x), snd(x)), xs)






    ///
    /// Returns the left-biased union of `m1` and `m2`.
    /// That is, key collisions are resolved by taking the mapping from `m1`.
    ///
    pub def union[k, v](m1: Map[k, v], m2: Map[k, v]): Map[k, v] = unionWithKey((_, v1, _) -> v1, m1, m2)

    ///
    /// Returns the union of `m1` and `m2` where key collisions are resolved with the merge function `f`.
    ///
    pub def unionWith[k, v](f: (v, v) -> v, m1: Map[k, v], m2: Map[k, v]): Map[k, v] = unionWithKey((_, v1, v2) -> f(v1, v2), m1, m2)

    ///
    /// Returns the union of `m1` and `m2` where key collisions are resolved with the merge function `f`, taking both the key and values.
    ///
    pub def unionWithKey[k, v](f: (k, v, v) -> v, m1: Map[k, v], m2: Map[k, v]): Map[k, v] =
        foldRightWithKey((k, v, m) -> insertWithKey((key, v1, v2) -> f(key, v1, v2), k, v, m) , m2, m1)

    ///
    /// Returns the left-biased intersection of `m1` and `m2`.
    /// That is, key collisions are resolved by taking the mapping from `m1`.
    ///
    pub def intersection[k, v](m1: Map[k, v], m2: Map[k, v]): Map[k, v] = intersectionWithKey((_, v1, _) -> v1, m1, m2)

    ///
    /// Returns the intersection of `m1` and `m2` where key collisions are resolved with the merge function `f`.
    ///
    pub def intersectionWith[k, v](f: (v, v) -> v, m1: Map[k, v], m2: Map[k, v]): Map[k, v] = intersectionWithKey((_, v1, v2) -> f(v1, v2), m1, m2)

    ///
    /// Returns the intersection of `m1` and `m2` where key collisions are resolved with the merge function `f`, taking both the key and values.
    ///
    pub def intersectionWithKey[k, v](f: (k, v, v) -> v, m1: Map[k, v], m2: Map[k, v]): Map[k, v] =
        let intersect = filterWithKey((k, _) -> memberOf(k, m1), m2);
        foldRightWithKey((k, v, m) -> adjustWithKey((key, v1) -> f(key, v, v1), k, m), intersect, m1)

    ///
    /// Returns the difference of `m1` and `m2`, i.e. `m1 - m2`.
    /// That is, returns the map `m1` with the keys removed that are in `m2`.
    ///
    pub def difference[k, v](m1: Map[k, v], m2: Map[k, v]): Map[k, v] = differenceWithKey((_, _, _) -> None, m1, m2)

    ///
    /// Returns the difference of `m1` and `m2`, i.e. `m1 - m2`.
    /// When a key `k` is in both `m1` and `m2`, the associated values are passed to the merge function `f`.
    /// If `f` returns `None` the mapping with `k` is thrown away (proper set difference).
    /// If `f` returns `Some(v)` the mapping `k -> v` is included in the result.
    ///
    pub def differenceWith[k, v](f: (v, v) -> Option[v], m1: Map[k, v], m2: Map[k, v]): Map[k, v] = differenceWithKey((_, v1, v2) -> f(v1, v2), m1, m2)

    ///
    /// Returns the difference of `m1` and `m2`, i.e. `m1 - m2`.
    /// When a key `k` is in both `m1` and `m2`, `k` and the associated values are passed to the merge function `f`.
    /// If `f` returns `None` the mapping with `k` is thrown away (proper set difference).
    /// If `f` returns `Some(v)` the mapping `k -> v` is included in the result.
    ///
    pub def differenceWithKey[k, v](f: (k, v, v) -> Option[v], m1: Map[k, v], m2: Map[k, v]): Map[k, v] =
        let diff = filterWithKey((k, _) -> !memberOf(k, m2), m1);
        let g = (k, v, m) -> if (memberOf(k, m1))
                                match get(k, m1) with {
                                    case Some(v1) => match f(k, v1, v) with {
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
    pub def toList[k, v](m: Map[k, v]): List[(k, v)] =
        let Map(xs) = m;
            xs

    ///
    /// Returns the map `m` as a set of key-value pairs.
    ///
    pub def toSet[k, v](m: Map[k, v]): Set[(k, v)] =
        let Map(xs) = m;
            List.toSet(xs)






    ///
    /// Returns `true` if and only if `m1` and `m2` and equal, i.e. they have the same mappings.
    ///
    pub def eq[k, v](m1: Map[k, v], m2: Map[k, v]): Bool = isSubmapOf(m1, m2) && size(m1) == size(m2)
}
