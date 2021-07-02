/*
 * Copyright 2017 Liam Palmer, Esben Bjerre
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

///
/// The Map type.
///
/// A map is currently represented internally as a red-black tree.
///
enum Map[k, v] {
    case Map(RedBlackTree[k, v])
}

instance Boxable[Map[k, v]] with Order[k], ToString[k], Order[v], ToString[v] {
    pub def box(x: Map[k, v]): Boxed = {
        let value = x as ##java.lang.Object;
        let compare = (o1, o2) -> Order.compare(o1 as Map[k, v], o2 as Map[k, v]);
        let toString = o -> ToString.toString(o as Map[k, v]);
        BoxedObject(value, compare, toString)
    }
    pub def unbox(x: Boxed): Map[k, v] = match x {
        case BoxedObject(v, _, _) => v as Map[k, v]
        case _                    => ?bug
    }
}

instance ToString[Map[k, v]] with ToString[k], ToString[v] {
    pub def toString(m: Map[k, v]): String =
        let s = Map.foldRightWithKey((k, v, acc) -> "${k} -> ${v}, ${acc}", "", m);
        "Map#{${String.dropRight(2, s)}}"
}

instance Hash[Map[k, v]] with Hash[k], Hash[v] {
    pub def hash(m: Map[k, v]): Int =
        Map.foldLeftWithKey((acc, k, v) -> acc * 17 + Hash.hash(k) + 19 * Hash.hash(v), 7937, m)
}

instance Eq[Map[k, v]] with Order[k], Eq[v] {
    pub def eq(m1: Map[k, v], m2: Map[k, v]): Bool =
        Map.isSubmapOf(m1, m2) and Map.isSubmapOf(m2, m1)

}

instance Order[Map[k, v]] with Order[k], Order[v] {

    pub def compare(x: Map[k, v], y: Map[k, v]): Comparison =
        Map.toList(x) <=> Map.toList(y)

}

namespace Map {

    ///
    /// Returns the size of `m`.
    ///
    @Time(size(m)) @Space(Int32.log2(size(m)))
    pub def size(m: Map[k, v]): Int32 =
        let Map(xs) = m;
        RedBlackTree.size(xs)






    ///
    /// Returns the empty map.
    ///
    /// `Map#{}` is syntactic sugar for `empty` (`Map#{} = empty()`).
    ///
    @Time(1) @Space(1)
    pub def empty(): Map[k, v] = Map(RedBlackTree.empty())

    ///
    /// Returns the singleton map where key `k` is mapped to value `v`.
    ///
    /// `Map#{k -> v}` is syntactic sugar for `singleton` (`Map#{k -> v} = singleton(k, v)`).
    ///
    @Time(1) @Space(1)
    pub def singleton(k: k, v: v): Map[k, v] with Order[k] = insert(k, v, empty())






    ///
    /// Returns `true` if and only if `m` is the empty map, i.e. `Map(Nil)`.
    ///
    @Time(1) @Space(1)
    pub def isEmpty(m: Map[k, v]): Bool =
        let Map(xs) = m;
        RedBlackTree.isEmpty(xs)

    ///
    /// Returns `Some(v)` if `k -> v` is in `m`.
    ///
    /// Otherwise returns `None`.
    ///
    @Time(Int32.log2(size(m))) @Space(1)
    pub def get(k: k, m: Map[k, v]): Option[v] with Order[k] =
        let Map(xs) = m;
        RedBlackTree.get(k, xs)

    ///
    /// Returns `v` if `k -> v` is in `m`.
    ///
    /// Otherwise, returns `d`.
    ///
    @Time(Int32.log2(size(m))) @Space(1)
    pub def getWithDefault(k: k, d: v, m: Map[k, v]): v with Order[k] = Option.getWithDefault(get(k, m), d)

    ///
    /// Returns `true` if and only if `m` contains the key `k`.
    ///
    @Time(Int32.log2(size(m))) @Space(1)
    pub def memberOf(k: k, m: Map[k, v]): Bool with Order[k] =
        let Map(xs) = m;
        RedBlackTree.memberOf(k, xs)

    ///
    /// Optionally finds `k -> v` where `k` is the smallest key according to the `Order` instance of `k`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    pub def minimum(m: Map[k, v]): Option[(k, v)] =
        let Map(xs) = m;
        RedBlackTree.minimum(xs)

    ///
    /// Optionally finds `k -> v` where `k` is the smallest key according to the given comparator `cmp`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    pub def minimumBy(cmp: (k, v, k, v) -> Comparison, m: Map[k, v]): Option[(k, v)] =
        reduceLeftWithKey((k1, v1, k2, v2) -> if (cmp(k1, v1, k2, v2) == LessThan) (k1, v1) else (k2, v2), m)

    ///
    /// Optionally finds `k -> v` where `k` is the largest key according to the `Order` instance of `k`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    pub def maximum(m: Map[k, v]): Option[(k, v)] =
        let Map(xs) = m;
        RedBlackTree.maximum(xs)

    ///
    /// Optionally finds `k -> v` where `k` is the largest key according to the given comparator `cmp`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    pub def maximumBy(cmp: (k, v, k, v) -> Comparison, m: Map[k, v]): Option[(k, v)] =
        reduceLeftWithKey((k1, v1, k2, v2) -> if (cmp(k1, v1, k2, v2) == GreaterThan) (k1, v1) else (k2, v2), m)

    ///
    /// Returns the keys of `m`.
    ///
    @Time(size(m)) @Space(size(m) )
    pub def keysOf(m: Map[k, v]): Set[k] with Order[k] =
        foldLeftWithKey((acc, k, _) -> Set.insert(k, acc), Set.empty(), m)

    ///
    /// Returns the values of `m`.
    ///
    @Time(size(m)) @Space(size(m))
    pub def valuesOf(m: Map[k, v]): List[v] =
        foldRight((v, acc) -> v :: acc, Nil, m)






    ///
    /// Updates `m` with `k -> v`.
    ///
    @Time(Int32.log2(size(m))) @Space(Int32.log2(size(m)))
    pub def insert(k: k, v: v, m: Map[k, v]): Map[k, v] with Order[k] =
        let Map(xs) = m;
        Map(RedBlackTree.insert(k, v, xs))

    ///
    /// Updates `m` with `k -> f(v, v1)` if `k -> v1` is in `m`.
    ///
    /// Otherwise, updates `m` with `k -> v`.
    ///
    @Time(time(f) + Int32.log2(size(m))) @Space(space(f) + Int32.log2(size(m)))
    pub def insertWith(f: (v, v) -> v & e, k: k, v: v, m: Map[k, v]): Map[k, v] & e with Order[k] =
        insertWithKey((_, v1, v2) -> f(v1, v2), k, v, m)

    ///
    /// Updates `m` with `k -> f(k, v, v1)` if `k -> v1` is in `m`.
    ///
    /// Otherwise, updates `m` with `k -> v`.
    ///
    @Time(time(f) + Int32.log2(size(m))) @Space(space(f) + Int32.log2(size(m)))
    pub def insertWithKey(f: (k, v, v) -> v & e, k: k, v: v, m: Map[k, v]): Map[k, v] & e with Order[k] =
        let Map(xs) = m;
        Map(RedBlackTree.insertWith(f, k, v, xs))





    ///
    /// Updates `m` with `k -> f(v)` if `k -> v` is in `m`.
    ///
    /// Otherwise, returns `m`.
    ///
    @Time(time(f) + Int32.log2(size(m))) @Space(space(f) + Int32.log2(size(m)))
    pub def adjust(f: v -> v & e, k: k, m: Map[k, v]): Map[k, v] & e with Order[k] =
        adjustWithKey((_, v1) -> f(v1), k, m)

    ///
    /// Updates `m` with `k -> f(k, v)` if `k -> v` is in `m`. Otherwise, returns `m`.
    ///
    @Time(time(f) + Int32.log2(size(m))) @Space(space(f) + Int32.log2(size(m)))
    pub def adjustWithKey(f: (k, v) -> v & e, k: k, m: Map[k, v]): Map[k, v] & e with Order[k] =
        updateWithKey((k1, v) -> Some(f(k1, v)), k, m)

    ///
    /// Updates `m` with `k -> v1` if `k -> v` is in `m` and `f(v) = Some(v1)`. Otherwise, returns `m`.
    ///
    @Time(time(f) + Int32.log2(size(m))) @Space(space(f) + Int32.log2(size(m)))
    pub def update(f: v -> Option[v] & e, k: k, m: Map[k, v]): Map[k, v] & e with Order[k] =
        updateWithKey((_, v1) -> f(v1), k, m)

    ///
    /// Updates `m` with `k -> v1` if `k -> v` is in `m` and `f(k, v) = Some(v1)`. Otherwise, returns `m`.
    ///
    @Time(time(f) + Int32.log2(size(m))) @Space(space(f) + Int32.log2(size(m)))
    pub def updateWithKey(f: (k, v) -> Option[v] & e, k: k, m: Map[k, v]): Map[k, v] & e with Order[k] =
        let Map(xs) = m;
        Map(RedBlackTree.updateWith(f, k, xs))





    ///
    /// Removes the mapping `k` from the map `m`.
    ///
    @Time(Int32.log2(size(m))) @Space(Int32.log2(size(m)))
    pub def delete(k: k, m: Map[k, v]): Map[k, v] with Order[k] =
        let Map(xs) = m;
        Map(RedBlackTree.delete(k, xs))






    ///
    /// Returns `true` if and only if all mappings in `m1` occur in `m2`.
    ///
    @Time(size(m1)) @Space(size(m1) * Int32.log2(size(m2)))
    pub def isSubmapOf(m1: Map[k, v], m2: Map[k, v]): Bool with Order[k], Eq[v] = forall((k, v) -> get(k, m2) == Some(v), m1)

    ///
    /// Returns `true` if and only if all mappings in `m1` occur in `m2` and `m1 != m2`.
    ///
    @Time(size(m1) + size(m2)) @Space(size(m1) * Int32.log2(size(m2)))
    pub def isProperSubmapOf(m1: Map[k, v], m2: Map[k, v]): Bool with Order[k], Eq[v] =
        size(m1) != size(m2) and isSubmapOf(m1, m2)

    ///
    /// Alias for `findLeft`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * size(m)) @Space(space(f) * Int32.log2(size(m)))
    pub def find(f: (k, v) -> Bool, m: Map[k, v]): Option[(k, v)] = findLeft(f, m)

    ///
    /// Optionally returns the first mapping of `m` that satisfies the predicate `f` when searching from left to right.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * size(m)) @Space(space(f) * Int32.log2(size(m)))
    pub def findLeft(f: (k, v) -> Bool, m: Map[k, v]): Option[(k, v)] =
        let Map(xs) = m;
        RedBlackTree.findLeft(f, xs)

    ///
    /// Optionally returns the first mapping of `m` that satisfies the predicate `f` when searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * size(m)) @Space(space(f) * Int32.log2(size(m)))
    pub def findRight(f: (k, v) -> Bool, m: Map[k, v]): Option[(k, v)] =
        let Map(xs) = m;
        RedBlackTree.findRight(f, xs)






    ///
    /// Returns a map of all mappings `k -> v` in `m` where `v` satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * size(m)) @Space(space(f) * size(m))
    pub def filter(f: v -> Bool, m: Map[k, v]): Map[k, v] with Order[k] = filterWithKey((_, v) -> f(v), m)

    ///
    /// Returns a map of all mappings `k -> v` in `m` where `(k, v)` satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * size(m)) @Space(space(f) * size(m))
    pub def filterWithKey(f: (k, v) -> Bool, m: Map[k, v]): Map[k, v] with Order[k] =
        foldLeftWithKey((acc, k, v) -> if (f(k, v)) insert(k, v, acc) else acc, empty(), m)

    ///
    /// Returns a map with mappings `k -> f(v)` for every `k -> v` in `m`.
    ///
    @Time(time(f) * size(m)) @Space(space(f) * size(m))
    pub def map(f: v1 -> v2 & e, m: Map[k, v1]): Map[k, v2] & e with Order[k] = mapWithKey((_, v) -> f(v), m)

    ///
    /// Returns a map with mappings `k -> f(k, v)` for every `k -> v` in `m`.
    ///
    @Time(time(f) * size(m)) @Space(space(f) * size(m))
    pub def mapWithKey(f: (k, v1) -> v2 & e, m: Map[k, v1]): Map[k, v2] & e with Order[k] =
        foldLeftWithKey((acc, k, v) -> insert(k, f(k, v), acc), empty(), m)

    ///
    /// Alias for `foldLeftWithKey`.
    ///
    pub def foldWithKey(f: (b, k, v) -> b & e, s: b, m: Map[k, v]): b & e = foldLeftWithKey(f, s, m)

    ///
    /// Applies `f` to a start value `s` and all values in `m` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, v1), v2)..., vn)`.
    ///
    pub def foldLeft(f: (b, v) -> b & e, s: b, m: Map[k, v]): b & e =
        foldLeftWithKey((acc, _, v) -> f(acc, v), s, m)

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `m` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, k1, v1), k2, v2)..., vn)`.
    ///
    pub def foldLeftWithKey(f: (b, k, v) -> b & e, s: b, m: Map[k, v]): b & e =
        let Map(xs) = m;
        RedBlackTree.foldLeft(f, s, xs)

    ///
    /// Applies `f` to a start value `s` and all values in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(v1, ...f(vn-1, f(vn, s)))`.
    ///
    pub def foldRight(f: (v, b) -> b & e, s: b, m: Map[k, v]): b & e =
        foldRightWithKey((_, v, acc) -> f(v, acc), s, m)

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(k1, v1, ...f(kn-1, vn-1, f(kn, vn, s)))`.
    ///
    pub def foldRightWithKey(f: (k, v, b) -> b & e, s: b, m: Map[k, v]): b & e =
        let Map(xs) = m;
        RedBlackTree.foldRight(f, s, xs)

    ///
    /// Applies `f` to all values in `m` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(v1, v2), v3)..., vn))`
    ///
    /// Returns `None` if `m` is the empty map.
    ///
    pub def reduceLeft(f: (v, v) -> v & e, m: Map[k, v]): Option[v] & e =
        reduceLeftWithKey((k, v1, _, v2) -> (k, f(v1, v2)), m) |> Option.map(snd)

    ///
    /// Applies `f` to all mappings in `m` going from left to right until a single mapping `(k, v)` is obtained.  Returns `Some((k, v))`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(k1, v1, k2, v2), k3, v3)..., kn, vn))`
    ///
    /// Returns `None` if `m` is the empty map.
    ///
    pub def reduceLeftWithKey(f: (k, v, k, v) -> (k, v) & e, m: Map[k, v]): Option[(k, v)] & e =
        let Map(xs) = m;
        RedBlackTree.reduceLeft(f, xs)

    ///
    /// Applies `f` to all values in `m` going from right to left until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(v1, ...f(vn-2, f(vn-1, vn))...))`
    ///
    /// Returns `None` if `m` is the empty map.
    ///
    pub def reduceRight(f: (v, v) -> v & e, m: Map[k, v]): Option[v] & e =
        reduceRightWithKey((k, v1, _, v2) -> (k, f(v1, v2)), m) |> Option.map(snd)

    ///
    /// Applies `f` to all mappings in `m` going from right to left until a single mapping `(k, v)` is obtained.  Returns `Some((k, v))`.
    ///
    /// That is, the result is of the form: `Some(f(k1, v1, ...f(kn-2, vn-2, f(kn-1, vn-1, kn, vn))...))`
    ///
    /// Returns `None` if `m` is the empty map.
    ///
    pub def reduceRightWithKey(f: (k, v, k, v) -> (k, v) & e, m: Map[k, v]): Option[(k, v)] & e =
        let Map(xs) = m;
        RedBlackTree.reduceRight(f, xs)






    ///
    /// Returns the number of mappings in `m` that satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * size(m)) @Space(space(f) * Int32.log2(size(m)))
    pub def count(f: (k, v) -> Bool, m: Map[k, v]): Int32 =
        foldLeftWithKey((acc, k, v) -> if (f(k, v)) 1 + acc else acc, 0, m)

    ///
    /// Returns `true` if and only if at least one mapping in `m` satisfies the predicate `f`.
    ///
    /// Returns `false` if `m` is the empty map.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * size(m)) @Space(space(f) * Int32.log2(size(m)))
    pub def exists(f: (k, v) -> Bool, m: Map[k, v]): Bool =
        let Map(xs) = m;
        RedBlackTree.exists(f, xs)

    ///
    /// Returns `true` if and only if all mappings in `m` satisfy the predicate `f`.
    ///
    /// Returns `true` if `m` is the empty map.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * size(m)) @Space(space(f) * Int32.log2(size(m)))
    pub def forall(f: (k, v) -> Bool, m: Map[k, v]): Bool =
        let Map(xs) = m;
        RedBlackTree.forall(f, xs)






    ///
    /// Returns the left-biased union of `m1` and `m2`.
    ///
    /// That is, key collisions are resolved by taking the mapping from `m1`.
    ///
    @Time(size(m1) * Int32.log2(size(m2))) @Space(size(m1) * Int32.log2(size(m2)))
    pub def union(m1: Map[k, v], m2: Map[k, v]): Map[k, v] with Order[k] =
        unionWithKey((_, v1, _) -> v1, m1, m2)

    ///
    /// Returns the union of `m1` and `m2` where key collisions are resolved with the merge function `f`.
    ///
    @Time(time(f) * size(m1) * Int32.log2(size(m2))) @Space(space(f) * size(m1) *  Int32.log2(size(m2)))
    pub def unionWith(f: (v, v) -> v & e, m1: Map[k, v], m2: Map[k, v]): Map[k, v] & e with Order[k] =
        unionWithKey((_, v1, v2) -> f(v1, v2), m1, m2)

    ///
    /// Returns the union of `m1` and `m2` where key collisions are resolved with the merge function `f`, taking both the key and values.
    ///
    @Time(time(f) * size(m1) * Int32.log2(size(m2))) @Space(space(f) * size(m1) *  Int32.log2(size(m2)))
    pub def unionWithKey(f: (k, v, v) -> v & e, m1: Map[k, v], m2: Map[k, v]): Map[k, v] & e with Order[k] =
        use RedBlackTree.{blackHeight, foldRight, insertWith};
        let Map(xs) = m1;
        let Map(ys) = m2;
        if (blackHeight(xs) < blackHeight(ys))
            Map(foldRight((k, v) -> insertWith(f, k, v), ys, xs))
        else
            Map(foldRight((k, v) -> insertWith((_, v1, v2) -> f(k, v2, v1), k, v), xs, ys))

    ///
    /// Returns the left-biased intersection of `m1` and `m2`.
    ///
    /// That is, key collisions are resolved by taking the mapping from `m1`.
    ///
    @Time(size(m1) * Int32.log2(size(m2))) @Space(size(m1) * Int32.log2(size(m2)))
    pub def intersection(m1: Map[k, v], m2: Map[k, v]): Map[k, v] with Order[k] =
        intersectionWithKey((_, v1, _) -> v1, m1, m2)

    ///
    /// Returns the intersection of `m1` and `m2` where key collisions are resolved with the merge function `f`.
    ///
    @Time(time(f) * size(m1) * Int32.log2(size(m2))) @Space(space(f) * size(m1) *  Int32.log2(size(m2)))
    pub def intersectionWith(f: (v, v) -> v & e, m1: Map[k, v], m2: Map[k, v]): Map[k, v] & e with Order[k] =
        intersectionWithKey((_, v1, v2) -> f(v1, v2), m1, m2)

    ///
    /// Returns the intersection of `m1` and `m2` where key collisions are resolved with the merge function `f`, taking both the key and values.
    ///
    @Time(time(f) * size(m1) * Int32.log2(size(m2))) @Space(space(f) * size(m1) *  Int32.log2(size(m2)))
    pub def intersectionWithKey(f: (k, v, v) -> v & e, m1: Map[k, v], m2: Map[k, v]): Map[k, v] & e with Order[k] =
        let intersect = filterWithKey((k, _) -> memberOf(k, m1), m2);
        foldRightWithKey((k, v, m) -> adjustWithKey((key, v1) -> f(key, v, v1), k, m), intersect, m1)

    ///
    /// Returns the difference of `m1` and `m2`, i.e. `m1 - m2`.
    ///
    /// That is, returns the map `m1` with the keys removed that are in `m2`.
    ///
    @Time(size(m2) * Int32.log2(size(m1))) @Space(size(m2) * Int32.log2(size(m1)))
    pub def difference(m1: Map[k, v], m2: Map[k, v]): Map[k, v] with Order[k] =
        differenceWithKey((_, _, _) -> None, m1, m2)

    ///
    /// Returns the difference of `m1` and `m2`, i.e. `m1 - m2`.
    ///
    /// When a key `k` is in both `m1` and `m2`, the associated values are passed to the merge function `f`.
    /// If `f` returns `None` the mapping with `k` is thrown away (proper set difference).
    /// If `f` returns `Some(v)` the mapping `k -> v` is included in the result.
    ///
    @Time(size(f) * size(m2) * Int32.log2(size(m1))) @Space(space(f) * size(m2) * Int32.log2(size(m1)))
    pub def differenceWith(f: (v, v) -> Option[v] & e, m1: Map[k, v], m2: Map[k, v]): Map[k, v] & e with Order[k] =
        differenceWithKey((_, v1, v2) -> f(v1, v2), m1, m2)

    ///
    /// Returns the difference of `m1` and `m2`, i.e. `m1 - m2`.
    ///
    /// When a key `k` is in both `m1` and `m2`, `k` and the associated values are passed to the merge function `f`.
    /// If `f` returns `None` the mapping with `k` is thrown away (proper set difference).
    /// If `f` returns `Some(v)` the mapping `k -> v` is included in the result.
    ///
    @Time(size(f) * size(m2) * Int32.log2(size(m1))) @Space(space(f) * size(m2) * Int32.log2(size(m1)))
    pub def differenceWithKey(f: (k, v, v) -> Option[v] & e, m1: Map[k, v], m2: Map[k, v]): Map[k, v] & e with Order[k] =
        let diff = filterWithKey((k, _) -> not memberOf(k, m2), m1);
        let g = (k, v, m) -> if (memberOf(k, m1))
                                match get(k, m1) {
                                    case Some(v1) => match f(k, v1, v) {
                                        case None => m
                                        case Some(w) => insert(k, w, m)
                                    }
                                    case None => unreachable!()
                                }
                             else m;
        foldRightWithKey(g, diff, m2)

    ///
    /// Returns the inverse map of `m`.
    ///
    /// That is, given a `Map[k, v]` returns a map `Map[v, Set[k]]`
    /// where every value is mapped to its key(s) in the original map.
    ///
    pub def invert(m: Map[k, v]): Map[v, Set[k]] with Order[k], Order[v] =
        let f = (acc, k, v) -> Map.insertWith(Set.union, v, Set#{k}, acc);
        Map.foldLeftWithKey(f, Map#{}, m)

    ///
    /// Returns `m` as a mutable set.
    ///
    @Time(1) @Space(1)
    pub def toMut(m: Map[k, v]): MutMap[k, v] & Impure =
        MutMap(ref m)

    ///
    /// Returns the map `m` as a list of key-value pairs.
    ///
    @Time(size(m)) @Space(size(m))
    pub def toList(m: Map[k, v]): List[(k, v)] =
        foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil, m)

    ///
    /// Returns the map `m` as a set of key-value pairs.
    ///
    @Time(size(m)) @Space(size(m))
    pub def toSet(m: Map[k, v]): Set[(k, v)] with Order[k], Order[v] =
        foldLeftWithKey((acc, k, v) -> Set.insert((k, v), acc), Set.empty(), m)

    ///
    /// Applies `f` to every element of `xs`.
    ///
    /// The function `f` must be impure.
    ///
    @Time(size(f) * size(m)) @Space(space(f) * Int32.log2(size(m)))
    pub def foreach(f: (k, v) -> Unit & ef, m: Map[k, v]): Unit & ef =
        let Map(xs) = m;
        RedBlackTree.foreach(f, xs)

    ///
    /// Build a map by applying `f` to the seed value `st`.
    ///
    /// `f` should return `Some(k,v,st1)` to signal a new key-value pair `k` and `v` and a new seed value `st1`.
    ///
    /// `f` should return `None` to signal the end of building the map.
    ///
    pub def unfold(f: s -> Option[(k, v, s)] & e, st: s): Map[k,v] & e with Order[k] = unfoldHelper(f, st, empty())

    ///
    /// Helper function for `unfold`.
    ///
    def unfoldHelper(f: s -> Option[(k, v, s)] & e, st: s, ac: Map[k,v]): Map[k,v] & e with Order[k] = match f(st) {
        case None => ac
        case Some((k, v, st1)) => unfoldHelper(f, st1, insert(k, v, ac))
    }

    ///
    /// Build a map by applying the function `next` to `()`. `next` is expected to encapsulate
    /// a stateful resource such as a file handle that can be iterated.
    ///
    /// `next` should return `Some(k,v)` to signal a new key-value pair `k` and `v`.
    ///
    /// `next` should return `None` to signal the end of building the map.
    ///
    pub def unfoldWithIter(next: () -> Option[(k, v)] & ef): Map[k, v] & ef with Order[k] = unfoldWithIterHelper(next, empty())

    ///
    /// Helper function for `unfoldWithIter`.
    ///
    def unfoldWithIterHelper(next: () -> Option[(k, v)] & ef, ac: Map[k, v]): Map[k, v] & ef with Order[k] = match next() {
        case None => ac
        case Some(k, v) => unfoldWithIterHelper(next, insert(k, v, ac))
    }

    ///
    /// Extracts a range of key-value pairs from the map `m`.
    ///
    /// That is, the result is a list of all pairs `(k, v)` where `p(k)` returns `Equal`.
    ///
    pub def query(p: k -> Comparison & e, m: Map[k, v]): List[(k, v)] & e =
        let Map(xs) = m;
        RedBlackTree.query(p, (k, v) -> (k, v), xs)

    ///
    /// Applies `f` to all key-value pairs `(k, v)` from the map `m` where `p(k)` returns `EqualTo`.
    ///
    /// The function `f` must be impure.
    ///
    pub def queryWith(p: k -> Comparison & ef1, f: (k, v) -> Unit & ef2, m: Map[k, v]): Unit & (ef1 and ef2) =
        let Map(xs) = m;
        RedBlackTree.queryWith(p, f, xs)

    /////////////////////////////////////////////////////////////////////////////
    // Lints: Use Cases                                                        //
    /////////////////////////////////////////////////////////////////////////////

///    @lint
///    def useGetWithDefault(): Bool =
///        forall [k, v] (k: k, d: v, m: Map[k, v]).
///            Option.getWithDefault(Map.get(k, m), d) === Map.getWithDefault(k, d, m)
///
///    @lint
///    def useIsSubmapOf(): Bool =
///        forall [k, v] (m1: Map[k, v], m2: Map[k, v]).
///            Map.forall((k, v) -> Map.get(k, m1) == Some(v), m2) === Map.isSubmapOf(m1, m2)


}
