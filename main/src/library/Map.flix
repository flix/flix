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

instance ToString[Map[k, v]] with [k : ToString, v : ToString] {
    def toString(m: Map[k, v]): String =
        let s = Map.foldRightWithKey((k, v, acc) -> "${k} -> ${v}, ${acc}", "", m);
        "Map#{${String.dropRight(2, s)}}"
}

instance Hash[Map[k, v]] with [k : Hash, v : Hash] {
    def hash(m: Map[k, v]): Int =
        Map.foldLeftWithKey((acc, k, v) -> acc * 17 + Hash.hash(k) + 19 * Hash.hash(v), 7937, m)
}

instance Eq[Map[k, v]] with[k : Eq, k : Order, v : Eq] {

    def eq(m1: Map[k, v], m2: Map[k, v]): Bool =
        Map.isSubmapOf(m1, m2) and Map.isSubmapOf(m2, m1)

    def neq(m1: Map[k, v], m2: Map[k, v]): Bool = not (m1 == m2)

}

instance Order[Map[k, v]] with [k : Order, v : Eq, v : Order] {

    def less(x: Map[k, v], y: Map[k, v]): Bool = match (x <=> y) {
        case LessThan => true
        case _ => false
    }
    def lessEqual(x: Map[k, v], y: Map[k, v]): Bool = match (x <=> y) {
        case LessThan => true
        case EqualTo => true
        case _ => false
    }
    def greater(x: Map[k, v], y: Map[k, v]): Bool = match (x <=> y) {
        case GreaterThan => true
        case _ => false
    }
    def greaterEqual(x: Map[k, v], y: Map[k, v]): Bool = match (x <=> y) {
        case GreaterThan => true
        case EqualTo => true
        case _ => false
    }
    pub def compare(x: Map[k, v], y: Map[k, v]): Comparison = match (Map.isSubmapOf(x, y), Map.isSubmapOf(y, x)) {
        case (true, true) => EqualTo
        case (false, true) => LessThan
        case _ => GreaterThan
    }

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
    pub def singleton[k : Order, v](k: k, v: v): Map[k, v] = insert(k, v, empty())






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
    pub def get[k : Order, v](k: k, m: Map[k, v]): Option[v] =
        let Map(xs) = m;
        RedBlackTree.get(k, xs)

    ///
    /// Returns `v` if `k -> v` is in `m`.
    ///
    /// Otherwise, returns `d`.
    ///
    @Time(Int32.log2(size(m))) @Space(1)
    pub def getWithDefault[k : Order, v](k: k, d: v, m: Map[k, v]): v = Option.getWithDefault(get(k, m), d)

    ///
    /// Returns `true` if and only if `m` contains the key `k`.
    ///
    @Time(Int32.log2(size(m))) @Space(1)
    pub def memberOf[k : Order, v](k: k, m: Map[k, v]): Bool =
        let Map(xs) = m;
        RedBlackTree.memberOf(k, xs)

    ///
    /// Returns the keys of `m`.
    ///
    @Time(size(m)) @Space(size(m) )
    pub def keysOf[k : Order, v](m: Map[k, v]): Set[k] =
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
    pub def insert[k : Order, v](k: k, v: v, m: Map[k, v]): Map[k, v] =
        let Map(xs) = m;
        Map(RedBlackTree.insert(k, v, xs))

    ///
    /// Updates `m` with `k -> f(v, v1)` if `k -> v1` is in `m`.
    ///
    /// Otherwise, updates `m` with `k -> v`.
    ///
    @Time(time(f) + Int32.log2(size(m))) @Space(space(f) + Int32.log2(size(m)))
    pub def insertWith[k : Order, v, e](f: (v, v) -> v & e, k: k, v: v, m: Map[k, v]): Map[k, v] & e =
        insertWithKey((_, v1, v2) -> f(v1, v2), k, v, m)

    ///
    /// Updates `m` with `k -> f(k, v, v1)` if `k -> v1` is in `m`.
    ///
    /// Otherwise, updates `m` with `k -> v`.
    ///
    @Time(time(f) + Int32.log2(size(m))) @Space(space(f) + Int32.log2(size(m)))
    pub def insertWithKey[k : Order, v, e](f: (k, v, v) -> v & e, k: k, v: v, m: Map[k, v]): Map[k, v] & e =
        let Map(xs) = m;
        Map(RedBlackTree.insertWith(f, k, v, xs))





    ///
    /// Updates `m` with `k -> f(v)` if `k -> v` is in `m`.
    ///
    /// Otherwise, returns `m`.
    ///
    @Time(time(f) + Int32.log2(size(m))) @Space(space(f) + Int32.log2(size(m)))
    pub def adjust[k : Order, v, e](f: v -> v & e, k: k, m: Map[k, v]): Map[k, v] & e =
        adjustWithKey((_, v1) -> f(v1), k, m)

    ///
    /// Updates `m` with `k -> f(k, v)` if `k -> v` is in `m`. Otherwise, returns `m`.
    ///
    @Time(time(f) + Int32.log2(size(m))) @Space(space(f) + Int32.log2(size(m)))
    pub def adjustWithKey[k : Order, v, e](f: (k, v) -> v & e, k: k, m: Map[k, v]): Map[k, v] & e =
        updateWithKey((k1, v) -> Some(f(k1, v)), k, m)

    ///
    /// Updates `m` with `k -> v1` if `k -> v` is in `m` and `f(v) = Some(v1)`. Otherwise, returns `m`.
    ///
    @Time(time(f) + Int32.log2(size(m))) @Space(space(f) + Int32.log2(size(m)))
    pub def update[k : Order, v, e](f: v -> Option[v] & e, k: k, m: Map[k, v]): Map[k, v] & e =
        updateWithKey((_, v1) -> f(v1), k, m)

    ///
    /// Updates `m` with `k -> v1` if `k -> v` is in `m` and `f(k, v) = Some(v1)`. Otherwise, returns `m`.
    ///
    @Time(time(f) + Int32.log2(size(m))) @Space(space(f) + Int32.log2(size(m)))
    pub def updateWithKey[k : Order, v, e](f: (k, v) -> Option[v] & e, k: k, m: Map[k, v]): Map[k, v] & e =
        let Map(xs) = m;
        Map(RedBlackTree.updateWith(f, k, xs))





    ///
    /// Removes the mapping `k` from the map `m`.
    ///
    @Time(Int32.log2(size(m))) @Space(Int32.log2(size(m)))
    pub def delete[k : Order, v](k: k, m: Map[k, v]): Map[k, v] =
        let Map(xs) = m;
        Map(RedBlackTree.delete(k, xs))






    ///
    /// Returns `true` if and only if all mappings in `m1` occur in `m2`.
    ///
    @Time(size(m1)) @Space(size(m1) * Int32.log2(size(m2)))
    pub def isSubmapOf[k : Order, v : Eq](m1: Map[k, v], m2: Map[k, v]): Bool = forall((k, v) -> get(k, m2) == Some(v), m1)

    ///
    /// Returns `true` if and only if all mappings in `m1` occur in `m2` and `m1 != m2`.
    ///
    @Time(size(m1) + size(m2)) @Space(size(m1) * Int32.log2(size(m2)))
    pub def isProperSubmapOf[k : Order, v : Eq](m1: Map[k, v], m2: Map[k, v]): Bool =
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
    pub def filter[k : Order, v](f: v -> Bool, m: Map[k, v]): Map[k, v] = filterWithKey((_, v) -> f(v), m)

    ///
    /// Returns a map of all mappings `k -> v` in `m` where `(k, v)` satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * size(m)) @Space(space(f) * size(m))
    pub def filterWithKey[k : Order, v](f: (k, v) -> Bool, m: Map[k, v]): Map[k, v] =
        foldLeftWithKey((acc, k, v) -> if (f(k, v)) insert(k, v, acc) else acc, empty(), m)

    ///
    /// Returns a map with mappings `k -> f(v)` for every `k -> v` in `m`.
    ///
    @Time(time(f) * size(m)) @Space(space(f) * size(m))
    pub def map[k : Order, v1, v2, e](f: v1 -> v2 & e, m: Map[k, v1]): Map[k, v2] & e = mapWithKey((_, v) -> f(v), m)

    ///
    /// Returns a map with mappings `k -> f(k, v)` for every `k -> v` in `m`.
    ///
    @Time(time(f) * size(m)) @Space(space(f) * size(m))
    pub def mapWithKey[k : Order, v1, v2, e](f: (k, v1) -> v2 & e, m: Map[k, v1]): Map[k, v2] & e =
        foldLeftWithKey((acc, k, v) -> insert(k, f(k, v), acc), empty(), m)





    ///
    /// Alias for `foldLeft`.
    ///
    pub def fold(f: (b, v) -> b & e, s: b, m: Map[k, v]): b & e = foldLeft(f, s, m)

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
    /// Alias for `reduceLeft`.
    ///
    pub def reduce(f: (v, v) -> v & e, m: Map[k, v]): Option[v] & e = reduceLeft(f, m)

    ///
    /// Alias for `reduceLeftWithKey`.
    ///
    pub def reduceWithKey(f: (k, v, k, v) -> (k, v) & e, m: Map[k, v]): Option[(k, v)] & e = reduceLeftWithKey(f, m)

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
    pub def union[k : Order, v](m1: Map[k, v], m2: Map[k, v]): Map[k, v] =
        unionWithKey((_, v1, _) -> v1, m1, m2)

    ///
    /// Returns the union of `m1` and `m2` where key collisions are resolved with the merge function `f`.
    ///
    @Time(time(f) * size(m1) * Int32.log2(size(m2))) @Space(space(f) * size(m1) *  Int32.log2(size(m2)))
    pub def unionWith[k : Order, v, e](f: (v, v) -> v & e, m1: Map[k, v], m2: Map[k, v]): Map[k, v] & e =
        unionWithKey((_, v1, v2) -> f(v1, v2), m1, m2)

    ///
    /// Returns the union of `m1` and `m2` where key collisions are resolved with the merge function `f`, taking both the key and values.
    ///
    @Time(time(f) * size(m1) * Int32.log2(size(m2))) @Space(space(f) * size(m1) *  Int32.log2(size(m2)))
    pub def unionWithKey[k : Order, v, e](f: (k, v, v) -> v & e, m1: Map[k, v], m2: Map[k, v]): Map[k, v] & e =
        foldRightWithKey((k, v, m) -> insertWithKey((key, v1, v2) -> f(key, v1, v2), k, v, m) , m2, m1)

    ///
    /// Returns the left-biased intersection of `m1` and `m2`.
    ///
    /// That is, key collisions are resolved by taking the mapping from `m1`.
    ///
    @Time(size(m1) * Int32.log2(size(m2))) @Space(size(m1) * Int32.log2(size(m2)))
    pub def intersection[k : Order, v](m1: Map[k, v], m2: Map[k, v]): Map[k, v] =
        intersectionWithKey((_, v1, _) -> v1, m1, m2)

    ///
    /// Returns the intersection of `m1` and `m2` where key collisions are resolved with the merge function `f`.
    ///
    @Time(time(f) * size(m1) * Int32.log2(size(m2))) @Space(space(f) * size(m1) *  Int32.log2(size(m2)))
    pub def intersectionWith[k : Order, v, e](f: (v, v) -> v & e, m1: Map[k, v], m2: Map[k, v]): Map[k, v] & e =
        intersectionWithKey((_, v1, v2) -> f(v1, v2), m1, m2)

    ///
    /// Returns the intersection of `m1` and `m2` where key collisions are resolved with the merge function `f`, taking both the key and values.
    ///
    @Time(time(f) * size(m1) * Int32.log2(size(m2))) @Space(space(f) * size(m1) *  Int32.log2(size(m2)))
    pub def intersectionWithKey[k : Order, v, e](f: (k, v, v) -> v & e, m1: Map[k, v], m2: Map[k, v]): Map[k, v] & e =
        let intersect = filterWithKey((k, _) -> memberOf(k, m1), m2);
        foldRightWithKey((k, v, m) -> adjustWithKey((key, v1) -> f(key, v, v1), k, m), intersect, m1)

    ///
    /// Returns the difference of `m1` and `m2`, i.e. `m1 - m2`.
    ///
    /// That is, returns the map `m1` with the keys removed that are in `m2`.
    ///
    @Time(size(m2) * Int32.log2(size(m1))) @Space(size(m2) * Int32.log2(size(m1)))
    pub def difference[k : Order, v](m1: Map[k, v], m2: Map[k, v]): Map[k, v] =
        differenceWithKey((_, _, _) -> None, m1, m2)

    ///
    /// Returns the difference of `m1` and `m2`, i.e. `m1 - m2`.
    ///
    /// When a key `k` is in both `m1` and `m2`, the associated values are passed to the merge function `f`.
    /// If `f` returns `None` the mapping with `k` is thrown away (proper set difference).
    /// If `f` returns `Some(v)` the mapping `k -> v` is included in the result.
    ///
    @Time(size(f) * size(m2) * Int32.log2(size(m1))) @Space(space(f) * size(m2) * Int32.log2(size(m1)))
    pub def differenceWith[k : Order, v, e](f: (v, v) -> Option[v] & e, m1: Map[k, v], m2: Map[k, v]): Map[k, v] & e =
        differenceWithKey((_, v1, v2) -> f(v1, v2), m1, m2)

    ///
    /// Returns the difference of `m1` and `m2`, i.e. `m1 - m2`.
    ///
    /// When a key `k` is in both `m1` and `m2`, `k` and the associated values are passed to the merge function `f`.
    /// If `f` returns `None` the mapping with `k` is thrown away (proper set difference).
    /// If `f` returns `Some(v)` the mapping `k -> v` is included in the result.
    ///
    @Time(size(f) * size(m2) * Int32.log2(size(m1))) @Space(space(f) * size(m2) * Int32.log2(size(m1)))
    pub def differenceWithKey[k : Order, v, e](f: (k, v, v) -> Option[v] & e, m1: Map[k, v], m2: Map[k, v]): Map[k, v] & e =
        let diff = filterWithKey((k, _) -> not memberOf(k, m2), m1);
        let g = (k, v, m) -> if (memberOf(k, m1))
                                match get(k, m1) {
                                    case Some(v1) => match f(k, v1, v) {
                                        case None => m
                                        case Some(w) => insert(k, w, m)
                                    }
                                    case None => panic("unreachable")
                                }
                             else m;
        foldRightWithKey(g, diff, m2)





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
    pub def toSet[k : Order, v : Order](m: Map[k, v]): Set[(k, v)] =
        foldLeftWithKey((acc, k, v) -> Set.insert((k, v), acc), Set.empty(), m)

    ///
    /// Applies `f` to every element of `xs`.
    ///
    /// The function `f` must be impure.
    ///
    @Time(size(f) * size(m)) @Space(space(f) * Int32.log2(size(m)))
    pub def foreach(f: (k, v) ~> Unit, m: Map[k, v]): Unit & Impure =
        let Map(xs) = m;
        RedBlackTree.foreach(f, xs)

    ///
    /// Build a map by applying `f` to the seed value `st`.
    ///
    /// `f` should return `Some(k,v,st1)` to signal a new key-value pair `k` and `v` and a new seed value `st1`.
    ///
    /// `f` should return `None` to signal the end of building the map.
    ///
    pub def unfold[k : Order, v, s, e](f: s -> Option[(k, v, s)] & e, st: s): Map[k,v] & e = unfoldHelper(f, st, empty())

    ///
    /// Helper function for `unfold`.
    ///
    def unfoldHelper[k : Order, v, s, e](f: s -> Option[(k, v, s)] & e, st: s, ac: Map[k,v]): Map[k,v] & e = match f(st) {
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
    pub def unfoldWithIter[k : Order, v](next: () ~> Option[(k, v)]): Map[k, v] & Impure = unfoldWithIterHelper(next, empty())

    ///
    /// Helper function for `unfoldWithIter`.
    ///
    def unfoldWithIterHelper[k : Order, v](next: () ~> Option[(k, v)], ac: Map[k, v]): Map[k, v] & Impure = match next() {
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
        RedBlackTree.queryWith(p, (k, v) -> (k, v), xs)

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