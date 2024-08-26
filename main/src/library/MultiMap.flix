/*
 * Copyright 2022 Stephen Tetley
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
/// The MultiMap type.
///
/// A MultiMap is a Map that allows multiple values for a key.
/// Multiple values are stored in a Set so duplicates are not allowed.
///
/// The key and value types must implement the Eq and Order traits.
///
pub enum MultiMap[k, v] with Sendable {
    case MultiMap(Map[k, Set[v]])
}

instance ToString[MultiMap[k, v]] with ToString[k], ToString[v] {
    pub def toString(m: MultiMap[k, v]): String = MultiMap.toString(m)
}

instance Hash[MultiMap[k, v]] with Hash[k], Hash[v] {
    pub def hash(m: MultiMap[k, v]): Int32 =
        let MultiMap.MultiMap(m1) = m;
        Map.foldLeftWithKey((acc, k, v) -> acc `Hash.combine` Hash.hash(k) `Hash.combine` Hash.hash(v), Hash.magic(), m1)
}


instance Eq[MultiMap[k, v]] with Eq[k], Eq[v] {
    pub def eq(m1: MultiMap[k, v], m2: MultiMap[k, v]): Bool =
        MultiMap.toList(m1) == MultiMap.toList(m2)
}

instance Order[MultiMap[k, v]] with Order[k], Order[v] {
    pub def compare(x: MultiMap[k, v], y: MultiMap[k, v]): Comparison =
        MultiMap.toList(x) <=> MultiMap.toList(y)
}

instance Foldable[MultiMap[k]] {
    pub def foldLeft(f: (b, v) -> b \ ef, s: b, m: MultiMap[k, v]): b \ ef = MultiMap.foldLeft((acc, v) -> f(acc, v), s, m)
    pub def foldRight(f: (v, b) -> b \ ef, s: b, m: MultiMap[k, v]): b \ ef = MultiMap.foldRight(f, s, m)
    pub def foldRightWithCont(f: (v, Unit -> b \ ef) -> b \ ef, s: b, m: MultiMap[k, v]): b \ ef = MultiMap.foldRightWithCont(f, s, m)
}

instance UnorderedFoldable[MultiMap[k]] {
    pub def foldMap(f: v -> b \ ef, m: MultiMap[k, v]): b \ ef with CommutativeMonoid[b] = MultiMap.foldMap(f, m)
    pub redef isEmpty(m: MultiMap[k, v]): Bool = MultiMap.isEmpty(m)
    pub redef exists(f: v -> Bool \ ef, m: MultiMap[k, v]): Bool \ ef = MultiMap.exists(_ -> f, m)
    pub redef forAll(f: v -> Bool \ ef, m: MultiMap[k, v]): Bool \ ef = MultiMap.forAll(_ -> f, m)
}

instance SemiGroup[MultiMap[k,v]] with Order[k], Order[v] {
    pub def combine(x: MultiMap[k,v], y: MultiMap[k,v]): MultiMap[k,v] = MultiMap.union(x, y)
}

instance CommutativeSemiGroup[MultiMap[k,v]] with Order[k], Order[v]

instance Monoid[MultiMap[k, v]] with Order[k], Order[v] {
    pub def empty(): MultiMap[k, v] = MultiMap.empty()
}

instance CommutativeMonoid[MultiMap[k, v]] with Order[k], Order[v]

mod MultiMap {

    ///
    /// Returns a string representation of `m`.
    ///
    pub def toString(m: MultiMap[k, v]): String with ToString[k], ToString[v] = region rc {
        let sb = StringBuilder.empty(rc);
        let MultiMap(m1) = m;
        StringBuilder.appendString!("MultiMap#{", sb);
        Map.forEachWithIndex((i, k, v) -> {
            if (i < 1)
                StringBuilder.appendString!("${k} => ${v}", sb)
            else
                StringBuilder.appendString!(", ${k} => ${v}", sb)
            }, m1);
        StringBuilder.appendString!("}", sb);
        StringBuilder.toString(sb)
    }

    ///
    /// Returns the empty MultiMap.
    ///
    pub def empty(): MultiMap[k, v] = MultiMap(Map.empty())

    ///
    /// Returns the singleton MultiMap where key `k` is mapped to value `v`.
    ///
    pub def singleton(k: k, v: v): MultiMap[k, v] with Order[k], Order[v] = MultiMap(Map.singleton(k, Set.singleton(v)))

    ///
    /// Returns `true` if and only if `m` is the empty MultiMap.
    ///
    pub def isEmpty(m: MultiMap[k, v]): Bool =
        let MultiMap(m1) = m;
        Map.isEmpty(m1)

    ///
    /// Returns `true` if and only if `m` is a non-empty MultiMap.
    ///
    pub def nonEmpty(m: MultiMap[k, v]): Bool = not isEmpty(m)

    ///
    /// Returns `Some(vs)` if `k => vs` is in `m`.
    ///
    /// Otherwise returns `Nil`.
    ///
    pub def get(k: k, m: MultiMap[k, v]): Set[v] with Order[k] =
        let MultiMap(t) = m;
        Map.get(k, t) |> Option.getWithDefault(Set.empty())

    ///
    /// Returns `true` if and only if `m` contains the key `k`.
    ///
    pub def memberOf(k: k, m: MultiMap[k, v]): Bool with Order[k] =
        let MultiMap(m1) = m;
        Map.memberOf(k, m1)

    ///
    /// Returns the keys of `m`.
    ///
    pub def keysOf(m: MultiMap[k, v]): Set[k] with Order[k] =
        let MultiMap(m1) = m;
        Map.keysOf(m1)

    ///
    /// Returns the values of `m`.
    ///
    /// Answer may contain duplicates where values were ascribed to multiple keys.
    ///
    pub def valuesOf(m: MultiMap[k, v]): List[v] with Order[k] =
        let MultiMap(m1) = m;
        Map.valuesOf(m1) |> List.map(Set.toList) |> List.flatten

    ///
    /// Updates `m` with `k => v`.
    ///
    pub def insert(k: k, v: v, m: MultiMap[k, v]): MultiMap[k, v] with Order[k], Order[v] =
        let MultiMap(m1) = m;
        MultiMap(Map.insertWith((v1, v2) -> Set.union(v1, v2), k, Set.singleton(v), m1))

    ///
    /// Updates `m` with `k => vs`.
    ///
    /// Where `vs` is any foldable container. If `vs` is empty nothing is inserted.
    ///
    pub def insertAll(k: k, vs: t[v], m: MultiMap[k, v]): MultiMap[k, v] \ Foldable.Aef[t] with Order[k], Order[v], Foldable[t] =
        match Foldable.toSet(vs) {
            case s1 if Set.isEmpty(s1) => m
            case s1                    => {
                let MultiMap(m1) = m;
                MultiMap(Map.insertWith((v1, v2) -> Set.union(v1, v2), k, s1, m1))
            }
        }

    ///
    /// Updates `m` with `k => map(f, vs)` if `k => vs` is in `m`.
    ///
    /// Otherwise, returns `m`.
    ///
    pub def adjust(f: v -> v \ ef, k: k, m: MultiMap[k, v]): MultiMap[k, v] \ ef with Order[k], Order[v] =
        adjustWithKey((_, v1) -> f(v1), k, m)

    ///
    /// Updates `m` with `k => map(f(k), vs)` if `k => vs` is in `m`. Otherwise, returns `m`.
    ///
    pub def adjustWithKey(f: (k, v) -> v \ ef, k: k, m: MultiMap[k, v]): MultiMap[k, v] \ ef with Order[k], Order[v] =
        updateWithKey((k1, v) -> Some(f(k1, v)), k, m)

    ///
    /// Updates `m` with `k => v1` if `k => v` is in `m` and `f(v) = Some(v1)`. Otherwise, returns `m`.
    ///
    pub def update(f: v -> Option[v] \ ef, k: k, m: MultiMap[k, v]): MultiMap[k, v] \ ef with Order[k], Order[v] =
        updateWithKey((_, v1) -> f(v1), k, m)

    ///
    /// Updates `m` with `k => v1` if `k => v` is in `m` and `f(k, v) = Some(v1)`. Otherwise, returns `m`.
    ///
    pub def updateWithKey(f: (k, v) -> Option[v] \ ef, k: k, m: MultiMap[k, v]): MultiMap[k, v] \ ef with Order[k], Order[v] =
        let MultiMap(m1) = m;
        let outer = (k1, s) -> if (k1 == k) Set.map(v1 -> Option.getWithDefault(v1, f(k1, v1)), s) else s;
        MultiMap(Map.mapWithKey(outer, m1))

    ///
    /// Removes the mapping `k` from the MultiMap `m`.
    ///
    pub def remove(k: k, m: MultiMap[k, v]): MultiMap[k, v] with Order[k] =
        let MultiMap(m1) = m;
        MultiMap(Map.remove(k, m1))

    ///
    /// Removes the mapping of `(k, v)`  from the MultiMap `m` it it exists.
    ///
    pub def removeWithValue(k: k, v: v, m: MultiMap[k, v]): MultiMap[k, v] with Order[k], Order[v] =
        let MultiMap(m1) = m;
        match Map.get(k, m1) {
            case None                               => m
            case Some(s) if not Set.memberOf(v, s)  => m
            case Some(s) if s == Set#{v}            => remove(k, m)
            case Some(_)                            => MultiMap(Map.adjust(s1 -> Set.remove(v, s1), k, m1))
        }

    ///
    /// Alias for `findLeft`.
    ///
    pub def find(f: (k, v) -> Bool \ ef, m: MultiMap[k, v]): Option[(k, v)] \ ef = findLeft(f, m)

    ///
    /// Optionally returns the first mapping of `m` that satisfies the predicate `f` when searching from left to right.
    ///
    pub def findLeft(f: (k, v) -> Bool \ ef, m: MultiMap[k, v]): Option[(k, v)] \ ef =
        let MultiMap(m1) = m;
        let findGroup = (k, s) -> Set.exists(f(k), s);
        let findItem = (k, s) -> Set.findLeft(f(k), s) |> Option.map(v1 -> (k, v1));
        match Map.findLeft(findGroup, m1) {
            case None          => None
            case Some((k, vs)) => findItem(k, vs)
        }

    ///
    /// Optionally returns the first mapping of `m` that satisfies the predicate `f` when searching from right to left.
    ///
    pub def findRight(f: (k, v) -> Bool \ ef, m: MultiMap[k, v]): Option[(k, v)] \ ef =
        let MultiMap(m1) = m;
        let findGroup = (k, s) -> Set.exists(f(k), s);
        let findItem = (k, s) -> Set.findRight(f(k), s) |> Option.map(v1 -> (k, v1));
        match Map.findRight(findGroup, m1) {
            case None          => None
            case Some((k, vs)) => findItem(k, vs)
        }

    ///
    /// Returns a MultiMap of all mappings `k => v` in `m` where `v` satisfies the predicate `f`.
    ///
    pub def filter(f: v -> Bool \ ef, m: MultiMap[k, v]): MultiMap[k, v] \ ef with Order[k], Order[v] =
        filterWithKey((_, v) -> f(v), m)

    ///
    /// Returns a MultiMap of all mappings `k => v` in `m` where `(k, v)` satisfies the predicate `f`.
    ///
    pub def filterWithKey(f: (k, v) -> Bool \ ef, m: MultiMap[k, v]): MultiMap[k, v] \ ef with Order[k], Order[v] =
        foldLeftWithKey((acc, k, v) -> if (f(k, v)) insert(k, v, acc) else acc, empty(), m)

    ///
    /// Returns a MultiMap with mappings `k => f(v)` for every `k => v` in `m`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def map(f: v1 -> v2 \ ef, m: MultiMap[k, v1]): MultiMap[k, v2] \ ef with Order[v2] =
        mapWithKey((_, v) -> f(v), m)

    ///
    /// Returns a MultiMap with mappings `k => f(k, v)` for every `k => v` in `m`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def mapWithKey(f: (k, v1) -> v2 \ ef, m: MultiMap[k, v1]): MultiMap[k, v2] \ ef with Order[v2] =
        let MultiMap(m1) = m;
        let outer = (k, vs) -> Set.map(f(k), vs);
        MultiMap(Map.mapWithKey(outer, m1))

    ///
    /// Alias for `foldLeftWithKey`.
    ///
    pub def foldWithKey(f: (b, k, v) -> b \ ef, s: b, m: MultiMap[k, v]): b \ ef = foldLeftWithKey(f, s, m)

    ///
    /// Applies `f` to a start value `s` and all values in `m` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, v1), v2)..., vn)`.
    ///
    pub def foldLeft(f: (b, v) -> b \ ef, s: b, m: MultiMap[k, v]): b \ ef =
        foldLeftWithKey((acc, _, v) -> f(acc, v), s, m)

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `m` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, k1, v1), k2, v2)..., vn)`.
    ///
    pub def foldLeftWithKey(f: (b, k, v) -> b \ ef, s: b, m: MultiMap[k, v]): b \ ef =
        let MultiMap(m1) = m;
        let outer = (acc, k, vs) -> Set.foldLeft((acc1, v1) -> f(acc1, k, v1), acc, vs);
        Map.foldLeftWithKey(outer, s, m1)

    ///
    /// Applies `f` to a start value `s` and all values in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(v1, ...f(vn-1, f(vn, s)))`.
    ///
    pub def foldRight(f: (v, b) -> b \ ef, s: b, m: MultiMap[k, v]): b \ ef =
        foldRightWithKey((_, v, acc) -> f(v, acc), s, m)

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(k1, v1, ...f(kn-1, vn-1, f(kn, vn, s)))`.
    ///
    pub def foldRightWithKey(f: (k, v, b) -> b \ ef, s: b, m: MultiMap[k, v]): b \ ef =
        let MultiMap(m1) = m;
        let outer = (k, vs, acc) -> Set.foldRight((v1, acc1) -> f(k, v1, acc1), acc, vs);
        Map.foldRightWithKey(outer, s, m1)

    ///
    /// Applies `f` to a start value `z` and all values in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(v1, ...f(vn-1, f(vn, z)))`.
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (v, Unit -> b \ ef) -> b \ ef, z: b, m: MultiMap[k, v]): b \ ef =
        foldRightWithKeyCont((_, v, c) -> f(v, c), z, m)

    ///
    /// Applies `f` to a start value `z` and all key-value pairs in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(k1, v1, ...f(kn-1, vn-1, f(kn, vn, z)))`.
    /// A `foldRightWithKeyCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithKeyCont(f: (k, v, Unit -> b \ ef) -> b \ ef, z: b, m: MultiMap[k, v]): b \ ef =
        let MultiMap(m1) = m;
        Map.foldRightWithKeyCont((k, vs, b1) -> Set.foldRightWithCont((v1, fac) -> f(k, v1, fac), b1(), vs), z, m1)

    ///
    /// Returns the result of mapping each key-value pair and combining the results.
    ///
    pub def foldMapWithKey(f: (k, v) -> b \ ef, m: MultiMap[k, v]): b \ ef with Monoid[b] =
        foldLeftWithKey((acc, k, v) -> Monoid.combine(acc, f(k, v)), Monoid.empty(), m)

    ///
    /// Returns the result of mapping each value and combining the results.
    ///
    pub def foldMap(f: v -> b \ ef, m: MultiMap[k, v]): b \ ef with Monoid[b] =
        foldMapWithKey(_ -> f, m)

    ///
    /// Applies `f` to all values in `m` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(v1, v2), v3)..., vn))`
    ///
    /// Returns `None` if `m` is the empty MultiMap.
    ///
    pub def reduceLeft(f: (v, v) -> v \ ef, m: MultiMap[k, v]): Option[v] \ ef =
        reduceLeftWithKey((k, v1, _, v2) -> (k, f(v1, v2)), m) |> Option.map(snd)

    ///
    /// Applies `f` to all mappings in `m` going from left to right until a single mapping `(k, v)` is obtained.  Returns `Some((k, v))`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(k1, v1, k2, v2), k3, v3)..., kn, vn))`
    ///
    /// Returns `None` if `m` is the empty MultiMap.
    ///
    pub def reduceLeftWithKey(f: (k, v, k, v) -> (k, v) \ ef, m: MultiMap[k, v]): Option[(k, v)] \ ef =
        let MultiMap(m1) = m;
        let inner = (k, acc, v) -> match acc {
            case None               => Some((k, v))
            case Some((kacc, vacc)) => Some(f(kacc, vacc, k, v))
        };
        let outer = (acc, k, vs) -> Set.foldLeft(inner(k), acc, vs);
        Map.foldLeftWithKey(outer, None, m1)

    ///
    /// Applies `f` to all values in `m` going from right to left until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(v1, ...f(vn-2, f(vn-1, vn))...))`
    ///
    /// Returns `None` if `m` is the empty MultiMap.
    ///
    pub def reduceRight(f: (v, v) -> v \ ef, m: MultiMap[k, v]): Option[v] \ ef =
        reduceRightWithKey((k, v1, _, v2) -> (k, f(v1, v2)), m) |> Option.map(snd)

    ///
    /// Applies `f` to all mappings in `m` going from right to left until a single mapping `(k, v)` is obtained.  Returns `Some((k, v))`.
    ///
    /// That is, the result is of the form: `Some(f(k1, v1, ...f(kn-2, vn-2, f(kn-1, vn-1, kn, vn))...))`
    ///
    /// Returns `None` if `m` is the empty MultiMap.
    ///
    pub def reduceRightWithKey(f: (k, v, k, v) -> (k, v) \ ef, m: MultiMap[k, v]): Option[(k, v)] \ ef =
        let MultiMap(m1) = m;
        let inner = (k, v, acc) -> match acc {
            case None               => Some((k, v))
            case Some((kacc, vacc)) => Some(f(k, v, kacc, vacc))
        };
        let outer = (k, vs, acc) -> Set.foldRight(inner(k), acc, vs);
        Map.foldRightWithKey(outer, None, m1)

    ///
    /// Returns the number of mappings `k => v` in `m` that satisfy the predicate `f`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def count(f: (k, v) -> Bool \ ef, m: MultiMap[k, v]): Int32 \ ef =
        sumWith((k, v) -> if (f(k,v)) 1 else 0, m)

    ///
    /// Returns the sum of all keys in the MultiMap `m`.
    ///
    pub def sumKeys(m: MultiMap[Int32, v]): Int32 =
        sumWith((k, _) -> k, m)

    ///
    /// Returns the sum of all values in the MultiMap `m`.
    ///
    pub def sumValues(m: MultiMap[k, Int32]): Int32 =
        sumWith((_, v) -> v, m)

    ///
    /// Returns the sum of all key-value pairs `k => v` in the MultiMap `m` according to the function `f`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def sumWith(f: (k, v) -> Int32 \ ef, m: MultiMap[k, v]): Int32 \ ef =
        let MultiMap(m1) = m;
        let outer = (k, vs) -> Set.sumWith(v -> f(k, v), vs);
        Map.sumWith(outer, m1)

    ///
    /// Returns `true` if and only if at least one mapping in `m` satisfies the predicate `f`.
    ///
    /// Returns `false` if `m` is the empty MultiMap.
    ///
    pub def exists(f: (k, v) -> Bool \ ef, m: MultiMap[k, v]): Bool \ ef =
        let MultiMap(m1) = m;
        let outer = (k, vs) -> Set.exists(v -> f(k, v), vs);
        Map.exists(outer, m1)

    ///
    /// Returns `true` if and only if all mappings in `m` satisfy the predicate `f`.
    ///
    /// Returns `true` if `m` is the empty MultiMap.
    ///
    pub def forAll(f: (k, v) -> Bool \ ef, m: MultiMap[k, v]): Bool \ ef =
        let MultiMap(m1) = m;
        let outer = (k, vs) -> Set.forAll(v -> f(k, v), vs);
        Map.forAll(outer, m1)

    ///
    /// Returns the union of `m1` and `m2`.
    ///
    pub def union(m1: MultiMap[k, v], m2: MultiMap[k, v]): MultiMap[k, v] with Order[k], Order[v] =
        let MultiMap(mm1) = m1;
        let MultiMap(mm2) = m2;
        MultiMap(Map.unionWith((s1, s2) -> Set.union(s1, s2), mm1, mm2))

    ///
    /// Returns the intersection of `m1` and `m2`.
    ///
    pub def intersection(m1: MultiMap[k, v], m2: MultiMap[k, v]): MultiMap[k, v] with Order[k], Order[v] =
        let MultiMap(mm1) = m1;
        let MultiMap(mm2) = m2;
        let step = (acc, k, s) -> match Map.get(k, mm2) {
            case None => acc
            case Some(s1) => match Set.intersection(s, s1) {
                case s2 if Set.isEmpty(s2) => acc
                case s2                    => Map.insert(k, s2, acc)
            }
        };
        MultiMap(Map.foldLeftWithKey(step, Map.empty(), mm1))

    ///
    /// Returns the difference of `m1` and `m2` i.e `m1 - m2` (left biased).
    ///
    pub def difference(m1: MultiMap[k, v], m2: MultiMap[k, v]): MultiMap[k, v] with Order[k], Order[v] =
        let MultiMap(mm1) = m1;
        let MultiMap(mm2) = m2;
        MultiMap(Map.differenceWith(setDifference1, mm1, mm2))

    ///
    /// Helper for `difference`.
    ///
    /// Note - returns `None` when the set difference is empty.
    ///
    def setDifference1(s1: Set[v], s2: Set[v]): Option[Set[v]] with Order[v] =
        match Set.difference(s1, s2) {
            case s if Set.isEmpty(s) => None
            case s                   => Some(s)
        }

    ///
    /// Returns the MultiMap `m` as a list of singleton key-value pairs.
    ///
    pub def toList(m: MultiMap[k, v]): List[(k, v)] = toAscList(m)

    ///
    /// Returns the MultiMap `m` as a list of singleton key-value pairs in ascending order.
    ///
    pub def toAscList(m: MultiMap[k, v]): List[(k, v)] =
        foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil, m)

    ///
    /// Returns the MultiMap `m` as a list of singleton key-value pairs in descending order.
    ///
    pub def toDescList(m: MultiMap[k, v]): List[(k, v)] =
        foldLeftWithKey((acc, k, v) -> (k, v) :: acc, Nil, m)

    ///
    /// Returns the MultiMap `m` as a list of `(key, Set[value])` pairs in ascending order.
    ///
    pub def toAssocList(m: MultiMap[k, v]): List[(k, Set[v])] =
        let MultiMap(m1) = m;
        Map.foldRightWithKey((k, vs, acc) -> (k, vs) :: acc, Nil, m1)

    ///
    /// Returns the MultiMap `m` as a list of singleton key-value pairs in ascending order.
    ///
    pub def toMap(m: MultiMap[k, v]): Map[k, Set[v]] =
        let MultiMap(m1) = m;
        m1

    ///
    /// Returns the MultiMap `m` as a list of singleton key-value pairs in ascending order.
    ///
    pub def toMutDeque(rc: Region[r], m: MultiMap[k, v]): MutDeque[(k, Set[v]), r] \ r =
        let MultiMap(m1) = m;
        Map.toMutDeque(rc, m1)

    // No traversable functions due to Order constraint that is outside the `Traversable` trait.

    ///
    /// Applies `f` to every `(key, value)` of MultiMap `m`.
    ///
    pub def forEach(f: (k, v) -> Unit \ ef, m: MultiMap[k, v]): Unit \ ef =
        let MultiMap(m1) = m;
        Map.forEach((k, s) -> Set.forEach(f(k), s), m1)

    ///
    /// Applies `f` to tuple `(index, key, value)` formed of the keys and values of
    /// MultiMap `m` and the index of the traversal.
    ///
    pub def forEachWithIndex(f: (Int32, k, v) -> Unit \ ef, m: MultiMap[k, v]): Unit \ ef = region rc {
        let ix = Ref.fresh(rc, 0);
        let f1 = (k , v) -> { let i = Ref.get(ix); f(i, k, v); Ref.put(i + 1, ix) };
        forEach(f1, m)
    }

    ///
    /// Returns an iterator over all key-value pairs in `m` i.e. `k => Set#{v_1, ..., v_n}`.
    ///
    pub def iterator(rc: Region[r], m: MultiMap[k, v]): Iterator[(k, Set[v]), r, r] \ r =
        let MultiMap(m1) = m;
        Map.iterator(rc, m1)

}
