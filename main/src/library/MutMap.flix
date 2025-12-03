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
pub struct MutMap[k: Type, v: Type, r: Region] {
    rc: Region[r],
    mut inner: Map[k, v]
}

instance Indexable[MutMap[k, v, r]] with Order[k] {
    type Idx = k
    type Elm = v
    type Aef = r + KeyNotFound

    pub def get(t: MutMap[k, v, r], i: k): v \ r + KeyNotFound = {
        match MutMap.get(i, t) {
            case Some(v) => v
            case None => KeyNotFound.keyNotFound("key not found")
        }
    }
}

instance IndexableMut[MutMap[k, v, r]] with Order[k] {
    type Aef = r

    pub def put(t: MutMap[k, v, r], i: k, v: v): Unit \ r = MutMap.put(i, v, t)
}

instance Iterable[MutMap[k, v, r]] {
    type Elm = (k, v)
    type Aef = r
    pub def iterator(rc: Region[r1], m: MutMap[k, v, r]): Iterator[(k, v), r + r1, r1] \ (r + r1) =
        MutMap.iterator(rc, m)
}

instance ForEach[MutMap[k, v, r]] {
    type Elm = (k, v)
    type Aef = r
    pub def forEach(f: ((k, v)) -> Unit \ ef, m: MutMap[k, v, r]): Unit \ ef + r = MutMap.forEach(k -> v -> f((k, v)), m)
}

instance Formattable[MutMap[k, v, r]] with Formattable[k], Formattable[v] {
    type Aef = Formattable.Aef[k] + Formattable.Aef[v] + r

    pub def format(x: MutMap[k, v, r]): RichString \ (Formattable.Aef[k] + Formattable.Aef[v] + r) =
        use RichString.{fromString, joinWith};
        let kvs = MutMap.toList(x) |> List.map(match (k, v) -> Formattable.format(k) + fromString(" => ") + Formattable.format(v));
        fromString("MutMap#{") + joinWith(identity, fromString(", "), kvs) + fromString("}")
}

mod MutMap {

    ///
    /// Returns a string representation of the given MutMap `m`.
    ///
    pub def toString(m: MutMap[k, v, r]): String \ r with ToString[k], ToString[v] = region rc {
        "MutMap#{" + (MutMap.iterator(rc, m) |> Iterator.joinWith(match (k, v) -> "${k} => ${v}", ", ")) + "}"
    }

    ///
    /// Returns a fresh empty mutable map.
    ///
    pub def empty(rc: Region[r]): MutMap[k, v, r] \ r =
        new MutMap @ rc { rc = rc, inner = Map.empty() }

    ///
    /// Returns the singleton map where key `k` is mapped to value `v`.
    ///
    pub def singleton(rc: Region[r], k: k, v: v): MutMap[k, v, r] \ r with Order[k] =
        new MutMap @ rc { rc = rc, inner = Map.singleton(k, v) }

    ///
    /// Returns `true` if and only if the mutable map `m` contains the key `k`.
    ///
    pub def memberOf(k: k, m: MutMap[k, v, r]): Bool \ r with Order[k] =
        Map.memberOf(k, m->inner)

    ///
    /// Returns `true` if and only if `m` is the empty map.
    ///
    pub def isEmpty(m: MutMap[k, v, r]): Bool \ r =
        Map.isEmpty(m->inner)

    ///
    /// Returns `true` if and only if `m` is a non-empty map.
    ///
    pub def nonEmpty(m: MutMap[k, v, r]): Bool \ r = not isEmpty(m)

    ///
    /// Optionally finds `k -> v` where `k` is the smallest key according to the `Order` instance of `k`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    pub def minimumKey(m: MutMap[k, v, r]): Option[(k, v)] \ r =
        Map.minimumKey(m->inner)

    ///
    /// Optionally finds `k => v` where `k` is the smallest key according to the given comparator `cmp`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `cmp`.
    ///
    pub def minimumKeyBy(cmp: (k, k) -> Comparison \ ef, m: MutMap[k, v, r]): Option[(k, v)] \ { ef, r } =
        Map.minimumKeyBy(cmp, m->inner)

    ///
    /// Optionally finds `k => v` where `v` is the smallest value.
    ///
    /// Returns `None` if `m` is empty.
    ///
    @Parallel
    pub def minimumValue(m: MutMap[k, v, r]): Option[(k, v)] \ r with Order[v] =
        Map.minimumValue(m->inner)

    ///
    /// Optionally finds `k => v` where `v` is the smallest value according to the given comparator `cmp`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `cmp`.
    ///
    pub def minimumValueBy(cmp: (v, v) -> Comparison \ ef, m: MutMap[k, v, r]): Option[(k, v)] \ { ef, r } =
        Map.minimumValueBy(cmp, m->inner)

    ///
    /// Optionally finds `k => v` where `k` is the largest key according to the `Order` instance of `k`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    pub def maximumKey(m: MutMap[k, v, r]): Option[(k, v)] \ r =
        Map.maximumKey(m->inner)

    ///
    /// Optionally finds `k => v` where `k` is the largest key according to the given comparator `cmp`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    pub def maximumKeyBy(cmp: (k, k) -> Comparison \ ef, m: MutMap[k, v, r]): Option[(k, v)] \ { ef, r } =
        Map.maximumKeyBy(cmp, m->inner)

    ///
    /// Optionally finds `k => v` where `v` is the largest value.
    ///
    /// Returns `None` if `m` is empty.
    ///
    @Parallel
    pub def maximumValue(m: MutMap[k, v, r]): Option[(k, v)] \ r with Order[v] =
        Map.maximumValue(m->inner)

    ///
    /// Optionally finds `k => v` where `v` is the largest value according to the given comparator `cmp`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `cmp`.
    ///
    pub def maximumValueBy(cmp: (v, v) -> Comparison \ ef, m: MutMap[k, v, r]): Option[(k, v)] \ { ef, r } =
        Map.maximumValueBy(cmp, m->inner)

    ///
    /// Returns the keys of the mutable map `m`.
    ///
    pub def keysOf(m: MutMap[k, v, r]): Set[k] \ r with Order[k] =
        Map.keysOf(m->inner)

    ///
    /// Returns the values of the mutable map `m`.
    ///
    pub def valuesOf(m: MutMap[k, v, r]): List[v] \ r =
        Map.valuesOf(m->inner)

    ///
    /// Returns the number of keys in the mutable map `m`.
    ///
    pub def size(m: MutMap[k, v, r]): Int32 \ r =
        Map.size(m->inner)

    ///
    /// Returns `Some(v)` if `k -> v` is in the mutable map `m`.
    ///
    /// Otherwise returns `None`.
    ///
    pub def get(k: k, m: MutMap[k, v, r]): Option[v] \ r with Order[k] =
        Map.get(k, m->inner)

    ///
    /// Returns `v` if `k -> v` is in the mutable map `m`.
    ///
    /// Otherwise returns `d`.
    ///
    pub def getWithDefault(k: k, d: v, m: MutMap[k, v, r]): v \ r with Order[k] =
        Map.getWithDefault(k, d, m->inner)

    ///
    /// Updates the mutable map `m` with the binding `k -> v`. Replaces any existing binding.
    ///
    pub def put(k: k, v: v, m: MutMap[k, v, r]): Unit \ r with Order[k] =
        m->inner = Map.insert(k, v, m->inner)

    ///
    /// Adds all key-value pairs in the collection `kvs` to the mutable map `m`.
    ///
    pub def putAll(kvs: f[(k, v)], m: MutMap[k, v, r]): Unit \ (r + Foldable.Aef[f]) with Order[k], Foldable[f] =
        Foldable.forEach(match (k, v) -> put(k, v, m), kvs)

    ///
    /// Updates the mutable map `m` with the binding `k -> f(v, v1)` if `k -> v1` is in `m`.
    ///
    /// Otherwise updates the mutable map `m` with the binding `k -> v`.
    ///
    pub def putWith(f: (v, v) -> v \ ef, k: k, v: v, m: MutMap[k, v, r]): Unit \ { ef, r } with Order[k] =
        putWithKey((_, v1, v2) -> f(v1, v2), k, v, m)

    ///
    /// Updates the mutable map `m` with the binding `k -> f(k, v, v1)` if `k -> v1` is in `m`.
    ///
    /// Otherwise updates the mutable map `m` with the binding `k -> v`.
    ///
    pub def putWithKey(f: (k, v, v) -> v \ ef, k: k, v: v, m: MutMap[k, v, r]): Unit \ { ef, r } with Order[k] =
        m->inner = Map.insertWithKey(f, k, v, m->inner)

    ///
    /// Updates the mutable map `m` with `k -> f(v)` if `k -> v` is in `m`.
    ///
    /// Otherwise leaves the map is unchanged.
    ///
    pub def adjust(f: v -> v \ ef, k: k, m: MutMap[k, v, r]): Unit \ { ef, r } with Order[k] =
        adjustWithKey((_, v1) -> f(v1), k, m)

    ///
    /// Updates the mutable map `m` with `k -> f(k, v)` if `k -> v` is in `m`.
    ///
    /// Otherwise leaves the map is unchanged.
    ///
    pub def adjustWithKey(f: (k, v) -> v \ ef, k: k, m: MutMap[k, v, r]): Unit \ { ef, r } with Order[k] =
        m->inner = Map.adjustWithKey(f, k, m->inner)

    ///
    /// Removes all mappings from the mutable map `m`.
    ///
    pub def clear(m: MutMap[k, v, r]): Unit \ r =
        m->inner = Map.empty()

    ///
    /// Returns `v` if `k -> v` is in the mutable map `m`.
    ///
    /// Otherwise updates the mutable map `m` with a new mapping `k -> d` and returns d.
    ///
    pub def getOrElsePut(k: k, d: v, m: MutMap[k, v, r]): v \ r with Order[k] =
        match Map.get(k, m->inner) {
            case None    => m->inner = Map.insert(k, d, m->inner); d
            case Some(v) => v
        }

    ///
    /// Merges the mutable map `m1` into the mutable map `m2` in a left-biased manner.
    ///
    /// That is, key collisions are resolved by taking the mapping from `m1`.
    ///
    pub def merge(m1: MutMap[k, v, r1], m2: MutMap[k, v, r2]): Unit \ { r1, r2 } with Order[k] =
        mergeWithKey((_, v1, _) -> v1, m1, m2)

    ///
    /// Merges the mutable map `m1` into the mutable map `m2` where key collisions are resolved with the merge function `f`.
    ///
    pub def mergeWith(f: (v, v) -> v \ ef, m1: MutMap[k, v, r1], m2: MutMap[k, v, r2]): Unit \ { ef, r1, r2 } with Order[k] =
        mergeWithKey(_ -> f, m1, m2)

    ///
    /// Merges the mutable map `m1` into the mutable map `m2` where key collisions are resolved with the merge function `f`, taking both the key and values.
    ///
    pub def mergeWithKey(f: (k, v, v) -> v \ ef, m1: MutMap[k, v, r1], m2: MutMap[k, v, r2]): Unit \ { ef, r1, r2 } with Order[k] =
        m2->inner = Map.unionWithKey(f, m1->inner, m2->inner)

    ///
    /// Removes all mappings `k -> v` from the mutable map `m` where `v` does not satisfy the predicate function `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def refine(f: v -> Bool, m: MutMap[k, v, r]): Unit \ r with Order[k] =
        refineWithKey(_ -> f, m)

    ///
    /// Removes all mappings `k -> v` from the mutable map `m` where `(k, v)` does not satisfy the predicate function `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def refineWithKey(f: (k, v) -> Bool, m: MutMap[k, v, r]): Unit \ r with Order[k] =
        m->inner = Map.filterWithKey(f, m->inner)

    ///
    /// Removes the mapping `k` from the mutable map `m`.
    ///
    /// Leaves the map unchanged if the mutable map `m` does not contain any mapping for `k`.
    ///
    pub def remove(k: k, m: MutMap[k, v, r]): Unit \ r with Order[k] =
        m->inner = Map.remove(k, m->inner)

    ///
    /// Applies the function `f` to every value in the mutable map `m`.
    ///
    pub def transform(f: v -> v \ ef, m: MutMap[k, v, r]): Unit \ { ef, r } with Order[k] =
        m->inner = Map.map(f, m->inner)

    ///
    /// Applies the function `f` to every value in the mutable map `m`.
    ///
    pub def transformWithKey(f: (k, v) -> v \ ef, m: MutMap[k, v, r]): Unit \ { ef, r } with Order[k] =
        m->inner = Map.mapWithKey(f, m->inner)

    ///
    /// Returns `true` if and only if all mappings in the mutable map `m1` occur in the mutable map `m2`.
    ///
    pub def isSubmapOf(m1: MutMap[k, v, r1], m2: MutMap[k, v, r2]): Bool \ { r1, r2 } with Order[k], Eq[v] =
        Map.isSubmapOf(m1->inner, m2->inner)

    ///
    /// Returns `true` if and only if all mappings in the mutable map `m1` occur in the mutable map `m2` and `m1 != m2`.
    ///
    pub def isProperSubmapOf(m1: MutMap[k, v, r1], m2: MutMap[k, v, r2]): Bool \ { r1, r2 } with Order[k], Eq[v] =
        Map.isProperSubmapOf(m1->inner, m2->inner)

    ///
    /// Alias for `findLeft`.
    ///
    /// The function `f` must be pure.
    ///
    pub def find(f: (k, v) -> Bool, m: MutMap[k, v, r]): Option[(k, v)] \ r =
        findLeft(f, m)

    ///
    /// Optionally returns the first mapping of the mutable map `m` that satisfies the predicate function `f` when searching from left to right.
    ///
    /// The function `f` must be pure.
    ///
    pub def findLeft(f: (k, v) -> Bool, m: MutMap[k, v, r]): Option[(k, v)] \ r =
        Map.findLeft(f, m->inner)

    ///
    /// Optionally returns the first mapping of the mutable map `m` that satisfies the predicate function `f` when searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    pub def findRight(f: (k, v) -> Bool, m: MutMap[k, v, r]): Option[(k, v)] \ r =
        Map.findRight(f, m->inner)

    ///
    /// Returns a map with mappings `k => f(v)` for every `k => v` in `m`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    pub def map(rc1: Region[r1], f: v1 -> v2 \ ef, m: MutMap[k, v1, r]): MutMap[k, v2, r1] \ { ef, r, r1 } =
        mapWithKey(rc1, _ -> f, m)

    ///
    /// Returns a map with mappings `k => f(k, v)` for every `k => v` in `m`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    pub def mapWithKey(rc: Region[r1], f: (k, v1) -> v2 \ ef, m: MutMap[k, v1, r]): MutMap[k, v2, r1] \ { ef, r, r1 } =
        new MutMap @ rc { rc = rc, inner = Map.mapWithKey(f, m->inner) }

    ///
    /// Alias for `foldLeftWithKey`.
    ///
    pub def foldWithKey(f: (b, k, v) -> b \ ef, i: b, m: MutMap[k, v, r]): b \ { ef, r } =
        foldLeftWithKey(f, i, m)

    ///
    /// Applies `f` to a start value `i` and all values in the mutable map `m` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(i, v1), v2)..., vn)`.
    ///
    pub def foldLeft(f: (b, v) -> b \ ef, i: b, m: MutMap[k, v, r]): b \ { ef, r } =
        Map.foldLeft(f, i, m->inner)

    ///
    /// Applies `f` to a start value `i` and all key-value pairs in the mutable map `m` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(k2, f(k1, i, v1), v2)..., vn)`.
    ///
    pub def foldLeftWithKey(f: (b, k, v) -> b \ ef, i: b, m: MutMap[k, v, r]): b \ { ef, r } =
        Map.foldLeftWithKey(f, i, m->inner)

    ///
    /// Applies `f` to a start value `s` and all values in the mutable map `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(v1, ...f(vn-1, f(vn, s)))`.
    ///
    pub def foldRight(f: (v, b) -> b \ ef, s: b, m: MutMap[k, v, r]): b \ { ef, r } =
        foldRightWithKey((_, v, b) -> f(v, b), s, m)

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in the mutable map `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(k1, v1, ...f(kn-1, vn-1, f(kn, vn, s)))`.
    ///
    pub def foldRightWithKey(f: (k, v, b) -> b \ ef, s: b, m: MutMap[k, v, r]): b \ { ef, r } =
        Map.foldRightWithKey(f, s, m->inner)

    ///
    /// Applies `f` to all values in the mutable map `m` going from left to right until a single value `v` is obtained. Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(v1, v2), v3)..., vn))`
    ///
    /// Returns `None` if `m` is the empty map.
    ///
    pub def reduceLeft(f: (v, v) -> v \ ef, m: MutMap[k, v, r]): Option[v] \ { ef, r } =
        Map.reduceLeft(f, m->inner)

    ///
    /// Applies `f` to all mappings in the mutable map `m` going from left to right until a single mapping `(k, v)` is obtained. Returns `Some((k, v))`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(k1, v1, k2, v2), k3, v3)..., kn, vn))`
    ///
    /// Returns `None` if `m` is the empty map.
    ///
    pub def reduceLeftWithKey(f: (k, v, k, v) -> (k, v) \ ef, m: MutMap[k, v, r]): Option[(k, v)] \ { ef, r } =
        Map.reduceLeftWithKey(f, m->inner)

    ///
    /// Applies `f` to all values in the mutable map `m` going from right to left until a single value `v` is obtained. Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(v1, ...f(vn-2, f(vn-1, vn))...))`
    ///
    /// Returns `None` if `m` is the empty map.
    ///
    pub def reduceRight(f: (v, v) -> v \ ef, m: MutMap[k, v, r]): Option[v] \ { ef, r } =
        Map.reduceRight(f, m->inner)

    ///
    /// Applies `f` to all mappings in the mutable map `m` going from right to left until a single mapping `(k, v)` is obtained. Returns `Some((k, v))`.
    ///
    /// That is, the result is of the form: `Some(f(k1, v1, ...f(kn-2, vn-2, f(kn-1, vn-1, kn, vn))...))`
    ///
    /// Returns `None` if `m` is the empty map.
    ///
    pub def reduceRightWithKey(f: (k, v, k, v) -> (k, v) \ ef, m: MutMap[k, v, r]): Option[(k, v)] \ { ef, r } =
        Map.reduceRightWithKey(f, m->inner)

    ///
    /// Returns the number of mappings in the mutable map `m` that satisfy the predicate function `f`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    pub def count(f: (k, v) -> Bool \ ef, m: MutMap[k, v, r]): Int32 \ { ef, r } =
        Map.count(f, m->inner)

    ///
    /// Returns `true` if and only if at least one mapping in the mutable map `m` satisfies the predicate function `f`.
    ///
    /// Returns `false` if `m` is the empty map.
    ///
    pub def exists(f: (k, v) -> Bool \ ef, m: MutMap[k, v, r]): Bool \ { ef, r } =
        Map.exists(f, m->inner)

    ///
    /// Returns `true` if and only if all mappings in the mutable map `m` satisfy the predicate function `f`.
    ///
    /// Returns `true` if `m` is the empty map.
    ///
    pub def forAll(f: (k, v) -> Bool \ ef, m: MutMap[k, v, r]): Bool \ { ef, r } =
        Map.forAll(f, m->inner)

    ///
    /// Returns a shallow copy of the mutable map `m`.
    ///
    pub def copy(rc: Region[r1], m: MutMap[k, v, r]): MutMap[k, v, r1] \ { r, r1 } =
        new MutMap @ rc { rc = rc, inner = m->inner }

    ///
    /// Returns the mutable map `m` as an immutable map.
    ///
    pub def toMap(m: MutMap[k, v, r]): Map[k, v] \ r =
        m->inner

    ///
    /// Returns the mutable map `m` as a list of key-value pairs.
    ///
    pub def toList(m: MutMap[k, v, r]): List[(k, v)] \ r =
        Map.toList(m->inner)

    ///
    /// Returns the mutable map `m` as a vector of key-value pairs.
    ///
    pub def toVector(m: MutMap[k, v, r]): Vector[(k, v)] \ r =
        Map.toVector(m->inner)

    ///
    /// Returns the mutable map `m` as an array of key-value pairs.
    ///
    pub def toArray(rc1: Region[r1], m: MutMap[k, v, r]): Array[(k, v), r1] \ {r, r1} =
        Map.toArray(rc1, m->inner)

    ///
    /// Returns the mutable map `m` as a set of key-value pairs.
    ///
    pub def toSet(m: MutMap[k, v, r]): Set[(k, v)] \ r with Order[k], Order[v] =
        Map.toSet(m->inner)

    ///
    /// Applies `f` to all the `(key, value)` pairs in the mutable map `m`.
    ///
    pub def forEach(f: (k, v) -> Unit \ ef, m: MutMap[k, v, r]): Unit \ { ef, r } =
        Map.forEach(f, m->inner)

    ///
    /// Apply the effectful function `f` to all the `(key, value)` pairs in the mutable map `m`
    /// along with that element's index.
    ///
    pub def forEachWithIndex(f: (Int32, k, v) -> Unit \ ef, m: MutMap[k, v, r]): Unit \ { ef, r } = region rc {
        let ix = Ref.fresh(rc, 0);
        forEach((k, v) -> { let i = Ref.get(ix); f(i, k, v); Ref.put(i + 1, ix) }, m)
    }

    ///
    /// Returns an iterator over all key-value pairs in `m`.
    ///
    pub def iterator(rc: Region[r1], m: MutMap[k, v, r2]): Iterator[(k, v), r1 + r2, r1] \ { r1, r2 } =
        Map.iterator(rc, m->inner) |> Iterator.map(x -> checked_ecast(x))

    ///
    /// Returns an iterator over keys in `m`.
    ///
    pub def iteratorKeys(rc: Region[r1], m: MutMap[k, v, r2]): Iterator[k, r1 + r2, r1] \ { r1, r2 } =
        Map.iteratorKeys(rc, m->inner) |> Iterator.map(x -> checked_ecast(x))

    ///
    /// Returns an iterator over values in `m`.
    ///
    pub def iteratorValues(rc: Region[r1], m: MutMap[k, v, r2]): Iterator[v, r1 + r2, r1] \ { r1, r2 } =
        Map.iteratorValues(rc, m->inner) |> Iterator.map(x -> checked_ecast(x))

    ///
    /// Extracts a range of key-value pairs from the mutable map `m`.
    ///
    /// That is, the result is a list of all pairs `(k, v)` where `p(k)` returns `Equal`.
    ///
    pub def rangeQuery(p: k -> Comparison \ ef, m: MutMap[k, v, r]): List[(k, v)] \ { ef, r } =
        Map.rangeQuery(p, m->inner)

    ///
    /// Applies `f` to all key-value pairs `(k, v)` in the mutable map `m` where `p(k)` returns `EqualTo`.
    ///
    pub def rangeQueryWith(p: k -> Comparison \ ef1, f: (k, v) -> Unit \ ef2, m: MutMap[k, v, r]): Unit \ { ef1, ef2, r } =
        Map.rangeQueryWith(p, f, m->inner)

    ///
    /// Returns `true` if MutMaps `a` and `b` have the same elements, i.e. are structurally equal.
    ///
    pub def sameElements(a: MutMap[k, v, r1], b: MutMap[k, v, r2]): Bool \ { r1, r2 } with Order[k], Eq[v] =
        (MutMap.isSubmapOf(a, b) and MutMap.isSubmapOf(b, a))

    ///
    /// Returns the concatenation of the string representation of each key `k`
    /// in `m` with `sep` inserted between each element.
    ///
    pub def joinKeys(sep: String, m: MutMap[k, v, r]): String \ r with ToString[k] =
        Map.joinKeys(sep, m->inner)

    ///
    /// Returns the concatenation of the string representation of each value `v`
    /// in `m` with `sep` inserted between each element.
    ///
    pub def joinValues(sep: String, m: MutMap[k, v, r]): String \ r with ToString[v] =
        Map.joinValues(sep, m->inner)

    ///
    /// Returns the concatenation of the string representation of each key-value pair
    /// `k => v` in `m` according to `f` with `sep` inserted between each element.
    ///
    pub def joinWith(f: (k, v) -> String \ ef, sep: String, m: MutMap[k, v, r]): String \ { ef, r } =
        Map.joinWith(f, sep, m->inner)

}
