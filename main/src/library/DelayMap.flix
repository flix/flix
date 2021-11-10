/*
 * Copyright 2021 Jakob Schneider Villumsen
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

use RedBlackTree.RedBlackTree;

enum DelayMap[k, v] {
    case DMap(RedBlackTree[k, Lazy[v]])
}

namespace DelayMap {

    ///
    /// Returns the empty map.
    ///
    pub def empty(): DelayMap[k, v] =
        DMap(RedBlackTree.empty())

    ///
    /// Returns the singleton map where key `k` is mapped to value `v`.
    ///
    pub def singleton(k: k, v: v): DelayMap[k, v] with Order[k] =
        insert(k, v, empty())

    ///
    /// Returns `true` if and only if `m` is the empty map, i.e. `Map(Nil)`.
    ///
    pub def isEmpty(m: DelayMap[k, v]): Bool =
        let DMap(t) = m;
        RedBlackTree.isEmpty(t)

    ///
    /// Returns an updated map with `k -> v`.
    ///
    pub def insert(k: k, v: v, m: DelayMap[k, v]): DelayMap[k, v] with Order[k] =
        insertLazy(k, lazy v, m)

    ///
    /// Helper function that inserts `k -> v` into `m`, where `v` is required to be lazy.
    ///
    def insertLazy(k: k, v: Lazy[v], m: DelayMap[k, v]): DelayMap[k, v] with Order[k] =
        let DMap(t) = m;
        DMap(RedBlackTree.insert(k, v, t))

    ///
    /// Returns `Some(v)` if `k -> v` is in `m`.
    ///
    /// Otherwise returns `None`.
    ///
    pub def get(k: k, m: DelayMap[k, v]): Option[v] with Order[k] =
        let DMap(t) = m;
        match RedBlackTree.get(k, t) {
            case None    => None
            case Some(x) => Some(force x)
        }

    ///
    /// Updates `m` with `k -> f(v, v1)` if `k -> v1` is in `m`.
    ///
    /// Otherwise, updates `m` with `k -> v`.
    ///
    pub def insertWith(f: (v, v) -> v & ef, k: k, v: v, m: DelayMap[k, v]): DelayMap[k, v] & ef with Order[k] =
        insertWithKey((_, v1, v2) -> f(v1, v2), k, v, m)

    ///
    /// Updates `m` with `k -> f(k, v, v1)` if `k -> v1` is in `m`.
    ///
    /// Otherwise, updates `m` with `k -> v`.
    ///
    pub def insertWithKey(f: (k, v, v) -> v & ef, k: k, v: v, m: DelayMap[k, v]): DelayMap[k, v] & ef with Order[k] =
        insertWithKeyLazy(f, k, lazy v, m)

    ///
    /// Helper function for `insertWithKey`.
    ///
    def insertWithKeyLazy(f: (k, v, v) -> v & ef, k: k, v: Lazy[v], m: DelayMap[k, v]): DelayMap[k, v] & ef with Order[k] =
        let DMap(t) = m;
        if (reify ef)
            let f1 = (key, newVal, oldVal) -> lazy (f(key, force newVal, force oldVal) as & Pure);
            DMap(RedBlackTree.insertWith(f1, k, v, t))
        else
            let f1 = (key, newVal, oldVal) -> {
                let x = f(key, force newVal, force oldVal);
                lazy x
            };
            DMap(RedBlackTree.insertWith(f1, k, v, t)) as & ef

    ///
    /// Returns a map with mappings `k -> f(v)` for every `k -> v` in `m`.
    ///
    pub def map(f: v1 -> v2 & ef, m: DelayMap[k, v1]): DelayMap[k, v2] & ef with Order[k] =
        mapWithKey((_, v) -> f(v), m)

    ///
    /// Returns a map with mappings `k -> f(k, v)` for every `k -> v` in `m`.
    ///
    pub def mapWithKey(f: (k, v1) -> v2 & ef, m: DelayMap[k, v1]): DelayMap[k, v2] & ef =
        let DMap(tree) = m;
        if (reify ef)
            let f1 = (k, v) -> lazy (f(k, force v) as & Pure);
            DMap(RedBlackTree.mapWithKey(f1, tree))
        else
            let f1 = (k, v) -> {
                let x1 = f(k, force v);
                lazy x1
            };
            DMap(RedBlackTree.mapWithKey(f1, tree)) as & ef

    ///
    /// Returns the left-biased union of `m1` and `m2`.
    ///
    /// That is, key collisions are resolved by taking the mapping from `m1`.
    ///
    pub def union(m1: DelayMap[k, v], m2: DelayMap[k, v]): DelayMap[k, v] with Order[k] =
        unionWithKey((_, v1, _) -> v1, m1, m2)

    ///
    /// Returns the union of `m1` and `m2` where key collisions are resolved with the merge function `f`.
    ///
    pub def unionWith(f: (v, v) -> v & ef, m1: DelayMap[k, v], m2: DelayMap[k, v]): DelayMap[k, v] & ef with Order[k] =
        unionWithKey((_, v1, v2) -> f(v1, v2), m1, m2)

    ///
    /// Returns the union of `m1` and `m2` where key collisions are resolved with the merge function `f`, taking both the key and values.
    ///
    pub def unionWithKey(f: (k, v, v) -> v & ef, m1: DelayMap[k, v], m2: DelayMap[k, v]): DelayMap[k, v] & ef with Order[k] =
        if (reify ef)
            unionWithKeyHelper((k, v1, v2) -> lazy (f(k, force v1, force v2) as & Pure), m1, m2)
        else
            let f1 = (k, v1, v2) -> {
                let x = f(k, force v1, force v2);
                lazy x
            };
            unionWithKeyHelper(f1, m1, m2) as & ef

    ///
    /// Helper function for `unionWithKey`.
    ///
    def unionWithKeyHelper(f: (k, Lazy[v], Lazy[v]) -> Lazy[v] & ef, m1: DelayMap[k, v], m2: DelayMap[k, v]): DelayMap[k, v] & ef with Order[k] =
        use RedBlackTree.{blackHeight, foldRight, insertWith};
        let DMap(xs) = m1;
        let DMap(ys) = m2;
        if (blackHeight(xs) < blackHeight(ys))
            DMap(foldRight((k, v) -> insertWith(f, k, v), ys, xs))
        else
            DMap(foldRight((k, v) -> insertWith((_, v1, v2) -> f(k, v2, v1), k, v), xs, ys))

    ///
    /// Returns the map `m` as a list of key-value pairs.
    ///
    pub def toList(m: DelayMap[k, v]): List[(k, v)] =
        foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil, m)

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `m` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, k1, v1), k2, v2)..., vn)`.
    ///
    pub def foldLeftWithKey(f: (b, k, v) -> b & ef, s: b, m: DelayMap[k, v]): b & ef =
        let f1 = (b, k, v) -> f(b, k, force v);
        foldLeftWithKeyLazy(f1, s, m)

    ///
    /// Helper function for `foldLeftWithKey`.
    ///
    def foldLeftWithKeyLazy(f: (b, k, Lazy[v]) -> b & ef, s: b, m: DelayMap[k, v]): b & ef =
        let DMap(t) = m;
        RedBlackTree.foldLeft(f, s, t)

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(k1, v1, ...f(kn-1, vn-1, f(kn, vn, s)))`.
    ///
    pub def foldRightWithKey(f: (k, v, b) -> b & ef, s: b, m: DelayMap[k, v]): b & ef =
        let DMap(t) = m;
        let f1 = (k1, v1, b1) -> f(k1, force v1, b1);
        RedBlackTree.foldRight(f1, s, t)

    ///
    /// Returns `m` as a Map, i.e. every value is forced.
    ///
    pub def toMap(m: DelayMap[k, v]): Map[k, v] =
        let DMap(t) = m;
        Map(RedBlackTree.mapWithKey((_, v) -> force v, t))

    ///
    /// Updates `m` with `k -> f(v)` if `k -> v` is in `m`.
    ///
    /// Otherwise, returns `m`.
    ///
    pub def adjust(f: v -> v & ef, k: k, m: DelayMap[k, v]): DelayMap[k, v] & ef with Order[k] =
        adjustWithKey((_, v1) -> f(v1), k, m)

    ///
    /// Updates `m` with `k -> f(k, v)` if `k -> v` is in `m`. Otherwise, returns `m`.
    ///
    pub def adjustWithKey(f: (k, v) -> v & ef, k: k, m: DelayMap[k, v]): DelayMap[k, v] & ef with Order[k] =
        updateWithKey((k1, v) -> Some(f(k1, v)), k, m)

    ///
    /// Updates `m` with `k -> v1` if `k -> v` is in `m` and `f(v) = Some(v1)`. Otherwise, returns `m`.
    ///
    pub def update(f: v -> Option[v] & ef, k: k, m: DelayMap[k, v]): DelayMap[k, v] & ef with Order[k] =
        updateWithKey((_, v1) -> f(v1), k, m)

    ///
    /// Updates `m` with `k -> v1` if `k -> v` is in `m` and `f(k, v) = Some(v1)`. Otherwise, returns `m`.
    ///
    pub def updateWithKey(f: (k, v) -> Option[v] & ef, k: k, m: DelayMap[k, v]): DelayMap[k, v] & ef with Order[k] =
        let f1 = (t) -> f(fst(t), snd(t));
        reifyEff(f1) {
            case Pure(g) => updateWithKeyL((k1, v) -> g((k1, v)), k, m)
            case _       => updateWithKeyE(f, k, m)
        }

    ///
    /// Helper function for `updateWithKey`. Does not force the `v`.
    ///
    def updateWithKeyL(f: (k, v) -> Option[v], k: k, m: DelayMap[k, v]): DelayMap[k, v] with Order[k] =
        let DMap(t) = m;
        let f1 = (k, v) -> {
            let res = lazy match f(k, force v) {
                case Some(v1) => v1
                case None     => force v
            };
            Some(res)
        };
        DMap(RedBlackTree.updateWith(f1, k, t))

    ///
    /// Helper function for `updateWithKey`. Forces `v`.
    ///
    def updateWithKeyE(f: (k, v) -> Option[v] & ef, k: k, m: DelayMap[k, v]): DelayMap[k, v] & ef with Order[k] =
        let DMap(t) = m;
        let f1 = (k, v) -> {
            let res = f(k, force v);
            match res {
                case Some(v1) => Some(lazy v1)
                case None     => None
            }
        };
        DMap(RedBlackTree.updateWith(f1, k, t))
}
