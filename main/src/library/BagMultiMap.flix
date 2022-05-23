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
/// The BagMultiMap type.
///
/// A BagMultiMap is a Map that allows multiple values for a key.
/// Mulitple values re stored in an ordered Bag so duplicates are allowed.
///
pub enum BagMultiMap[k, v] {
    case BagMultiMap(Map[k, Nec[v]])
}

instance Boxable[BagMultiMap[k, v]] with Boxable[k], Boxable[v]

instance ToString[BagMultiMap[k, v]] with ToString[k], ToString[v] {
    pub def toString(m: BagMultiMap[k, v]): String =
        let BagMultiMap(m1) = m;
        let s = Map.foldRightWithKey((k, v, acc) -> "${k} => ${v}, ${acc}", "", m1);
        "BagMultiMap#{${String.dropRight(2, s)}}"
}

instance Hash[BagMultiMap[k, v]] with Hash[k], Hash[v] {
    pub def hash(m: BagMultiMap[k, v]): Int32 =
        let BagMultiMap(m1) = m;
        Map.foldLeftWithKey((acc, k, v) -> acc * 17 + Hash.hash(k) + 19 * Hash.hash(v), 7937, m1)
}

instance Eq[BagMultiMap[k, v]] with Eq[k], Eq[v] {
    pub def eq(m1: BagMultiMap[k, v], m2: BagMultiMap[k, v]): Bool =
        BagMultiMap.toList(m1) == BagMultiMap.toList(m2)

}

instance Order[BagMultiMap[k, v]] with Order[k], Order[v] {

    pub def compare(x: BagMultiMap[k, v], y: BagMultiMap[k, v]): Comparison =
        BagMultiMap.toList(x) <=> BagMultiMap.toList(y)

}

instance Functor[BagMultiMap[k]] {
    pub def map(f: v1 -> v2 & ef, m: BagMultiMap[k, v1]): BagMultiMap[k, v2] & ef = BagMultiMap.map(f, m)
}

instance Foldable[BagMultiMap[k]] {
    pub def foldLeft(f: (b, v) -> b & ef, s: b, m: BagMultiMap[k, v]): b & ef = BagMultiMap.foldLeft((acc, v) -> f(acc, v), s, m)
    pub def foldRight(f: (v, b) -> b & ef, s: b, m: BagMultiMap[k, v]): b & ef = BagMultiMap.foldRight(f, s, m)
    pub def foldRightWithCont(f: (v, Unit -> b & ef) -> b & ef, s: b, m: BagMultiMap[k, v]): b & ef = BagMultiMap.foldRightWithCont(f, s, m)
}

instance Traversable[BagMultiMap[k]] {
    pub def traverse(f: a -> m[b] & ef, t: BagMultiMap[k, a]): m[BagMultiMap[k, b]] & ef with Applicative[m] = BagMultiMap.traverse(f, t)
    pub override def sequence(t: BagMultiMap[k, m[a]]): m[BagMultiMap[k, a]] with Applicative[m] = BagMultiMap.sequence(t)
}

instance SemiGroup[BagMultiMap[k,v]] with Order[k], SemiGroup[v] {
    pub def combine(x: BagMultiMap[k,v], y: BagMultiMap[k,v]): BagMultiMap[k,v] = BagMultiMap.union(x, y)
}

instance CommutativeSemiGroup[BagMultiMap[k,v]] with Order[k], CommutativeSemiGroup[v]

instance Monoid[BagMultiMap[k, v]] with Order[k], Monoid[v] {
    pub def empty(): BagMultiMap[k, v] = BagMultiMap.empty()
}

instance CommutativeMonoid[BagMultiMap[k, v]] with Order[k], CommutativeMonoid[v]


namespace BagMultiMap {

    ///
    /// Returns the empty BagMultiMap.
    ///
    pub def empty(): BagMultiMap[k, v] = BagMultiMap(Map.empty())

    ///
    /// Returns the singleton BagMultiMap where key `k` is mapped to value `v`.
    ///
    pub def singleton(k: k, v: v): BagMultiMap[k, v] with Order[k] = BagMultiMap(Map.singleton(k, Nec.singleton(v)))

    ///
    /// Returns a BagMultiMap where key `k` is mapped to the singleton bag containing `v`.
    ///
    pub def fromMap(m: Map[k, v]): BagMultiMap[k, v] with Order[k] = BagMultiMap(Map.map(Nec.singleton, m))

    ///
    /// Returns `true` if and only if `m` is the empty BagMultiMap.
    ///
    pub def isEmpty(m: BagMultiMap[k, v]): Bool =
        let BagMultiMap(m1) = m;
        Map.isEmpty(m1)

    ///
    /// Returns the list `vs` if `k => vs` is in `m`.
    ///
    /// Otherwise returns `Nil`.
    ///
    pub def get(k: k, m: BagMultiMap[k, v]): List[v] with Order[k] =
        let BagMultiMap(t) = m;
        Map.get(k, t) |> Option.map(Nec.toList) |> Option.getWithDefault(Nil)

    ///
    /// Returns `true` if and only if `m` contains the key `k`.
    ///
    pub def memberOf(k: k, m: BagMultiMap[k, v]): Bool with Order[k] =
        let BagMultiMap(m1) = m;
        Map.memberOf(k, m1)

    ///
    /// Returns the keys of `m`.
    ///
    pub def keysOf(m: BagMultiMap[k, v]): Set[k] with Order[k] =
        let BagMultiMap(m1) = m;
        Map.keysOf(m1)

    ///
    /// Returns the values of `m`.
    ///
    pub def valuesOf(m: BagMultiMap[k, v]): List[v] with Order[k] =
        let BagMultiMap(m1) = m;
        match Map.valuesOf(m1) {
            case Nil     => Nil
            case x :: rs => List.foldLeft(Nec.append, x, rs) |> Nec.toList
        }

    ///
    /// Updates `m` with `k => v`.
    ///
    /// Operationally, if one or more values already exists for `k` then `v` is added to the
    /// front of the bag of values.
    ///
    pub def insert(k: k, v: v, m: BagMultiMap[k, v]): BagMultiMap[k, v] with Order[k] =
        let BagMultiMap(m1) = m;
        BagMultiMap(Map.insertWith((_, vs) -> Nec.cons(v, vs), k, Nec.singleton(v), m1))

    ///
    /// Updates `m` with `k => vs`.
    ///
    /// Where `vs` is any foldable container. If `vs` is empty then nothing is inserted.
    ///
    pub def inserts(k: k, vs: t[v], m: BagMultiMap[k, v]): BagMultiMap[k, v] with Order[k], Foldable[t] =
        match Foldable.toList(vs) |> List.toNec {
            case None => m
            case Some(front) => {
                let BagMultiMap(m1) = m;
                BagMultiMap(Map.insertWith((_, vs) -> Nec.append(front, vs), k, front, m1))
            }
        }

    ///
    /// Updates `m` with `k => map(f, vs)` if `k => vs` is in `m`.
    ///
    /// Otherwise, returns `m`.
    ///
    pub def adjust(f: v -> v & ef, k: k, m: BagMultiMap[k, v]): BagMultiMap[k, v] & ef with Order[k] =
        adjustWithKey((_, v1) -> f(v1), k, m)

    ///
    /// Updates `m` with `k => map(f(k), vs)` if `k => vs` is in `m`. Otherwise, returns `m`.
    ///
    pub def adjustWithKey(f: (k, v) -> v & ef, k: k, m: BagMultiMap[k, v]): BagMultiMap[k, v] & ef with Order[k] =
        updateWithKey((k1, v) -> Some(f(k1, v)), k, m)


    ///
    /// Updates `m` with `k => v1` if `k => v` is in `m` and `f(v) = Some(v1)`. Otherwise, returns `m`.
    ///
    pub def update(f: v -> Option[v] & ef, k: k, m: BagMultiMap[k, v]): BagMultiMap[k, v] & ef with Order[k] =
        updateWithKey((_, v1) -> f(v1), k, m)

    ///
    /// Updates `m` with `k => v1` if `k => v` is in `m` and `f(k, v) = Some(v1)`. Otherwise, returns `m`.
    ///
    pub def updateWithKey(f: (k, v) -> Option[v] & ef, k: k, m: BagMultiMap[k, v]): BagMultiMap[k, v] & ef with Order[k] =
        let BagMultiMap(m1) = m;
        let outer = (k1, vs) -> if (k1 == k) Nec.map(v1 -> Option.getWithDefault(v1, f(k1, v1)), vs) else vs;
        BagMultiMap(Map.mapWithKey(outer, m1))

    ///
    /// Removes the mapping `k` from the BagMultiMap `m`.
    ///
    pub def remove(k: k, m: BagMultiMap[k, v]): BagMultiMap[k, v] with Order[k] =
        let BagMultiMap(m1) = m;
        BagMultiMap(Map.remove(k, m1))

    ///
    /// Alias for `findLeft`.
    ///
    /// The function `f` must be pure.
    ///
    pub def find(f: (k, v) -> Bool, m: BagMultiMap[k, v]): Option[(k, v)] = findLeft(f, m)

    ///
    /// Optionally returns the first mapping of `m` that satisfies the predicate `f` when searching from left to right.
    ///
    /// The function `f` must be pure.
    ///
    pub def findLeft(f: (k, v) -> Bool, m: BagMultiMap[k, v]): Option[(k, v)] =
        let BagMultiMap(m1) = m;
        let findGroup = (k, vs) -> Nec.exists(f(k), vs);
        let findItem = (k, vs) -> Nec.findLeft(f(k), vs) |> Option.map(v1 -> (k, v1));
        match Map.findLeft(findGroup, m1) {
            case None          => None
            case Some((k, vs)) => findItem(k, vs)
        }

    ///
    /// Optionally returns the first mapping of `m` that satisfies the predicate `f` when searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    pub def findRight(f: (k, v) -> Bool, m: BagMultiMap[k, v]): Option[(k, v)] =
        let BagMultiMap(m1) = m;
        let findGroup = (k, vs: Nec[v]) -> Nec.exists(f(k), vs);
        let findItem = (k, vs: Nec[v]) -> Nec.findRight(f(k), vs) |> Option.map(v1 -> (k, v1));
        match Map.findRight(findGroup, m1) {
            case None          => None
            case Some((k, vs)) => findItem(k, vs)
        }

    ///
    /// Returns a map of all mappings `k => v` in `m` where `v` satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def filter(f: v -> Bool, m: BagMultiMap[k, v]): BagMultiMap[k, v] with Order[k] = filterWithKey((_, v) -> f(v), m)

    ///
    /// Returns a map of all mappings `k => v` in `m` where `(k, v)` satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def filterWithKey(f: (k, v) -> Bool, m: BagMultiMap[k, v]): BagMultiMap[k, v] with Order[k] =
        foldLeftWithKey((acc, k, v) -> if (f(k, v)) insert(k, v, acc) else acc, empty(), m)

    ///
    /// Returns a map with mappings `k => f(v)` for every `k => v` in `m`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    pub def map(f: v1 -> v2 & ef, m: BagMultiMap[k, v1]): BagMultiMap[k, v2] & ef = mapWithKey((_, v) -> f(v), m)

    ///
    /// Returns a map with mappings `k => f(k, v)` for every `k => v` in `m`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    pub def mapWithKey(f: (k, v1) -> v2 & ef, m: BagMultiMap[k, v1]): BagMultiMap[k, v2] & ef =
        let BagMultiMap(m1) = m;
        let outer = (k, vs) -> Nec.map(f(k), vs);
        BagMultiMap(Map.mapWithKey(outer, m1))


    ///
    /// Alias for `foldLeftWithKey`.
    ///
    pub def foldWithKey(f: (b, k, v) -> b & ef, s: b, m: BagMultiMap[k, v]): b & ef = foldLeftWithKey(f, s, m)

    ///
    /// Applies `f` to a start value `s` and all values in `m` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, v1), v2)..., vn)`.
    ///
    pub def foldLeft(f: (b, v) -> b & ef, s: b, m: BagMultiMap[k, v]): b & ef =
        foldLeftWithKey((acc, _, v) -> f(acc, v), s, m)

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `m` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, k1, v1), k2, v2)..., vn)`.
    ///
    pub def foldLeftWithKey(f: (b, k, v) -> b & ef, s: b, m: BagMultiMap[k, v]): b & ef =
        let BagMultiMap(m1) = m;
        let outer = (acc, k, vs) -> Nec.foldLeft((acc1, v1) -> f(acc1, k, v1), acc, vs);
        Map.foldLeftWithKey(outer, s, m1)

    ///
    /// Applies `f` to a start value `s` and all values in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(v1, ...f(vn-1, f(vn, s)))`.
    ///
    pub def foldRight(f: (v, b) -> b & ef, s: b, m: BagMultiMap[k, v]): b & ef =
        foldRightWithKey((_, v, acc) -> f(v, acc), s, m)

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(k1, v1, ...f(kn-1, vn-1, f(kn, vn, s)))`.
    ///
    pub def foldRightWithKey(f: (k, v, b) -> b & ef, s: b, m: BagMultiMap[k, v]): b & ef =
        let BagMultiMap(m1) = m;
        let outer = (k, vs, acc) -> Nec.foldRight((v1, acc1) -> f(k, v1, acc1), acc, vs);
        Map.foldRightWithKey(outer, s, m1)

    ///
    /// Applies `f` to a start value `z` and all values in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(v1, ...f(vn-1, f(vn, z)))`.
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (v, Unit -> b & ef) -> b & ef, z: b, m: BagMultiMap[k, v]): b & ef =
        foldRightWithKeyCont((_, v, c) -> f(v, c), z, m)

    ///
    /// Applies `f` to a start value `z` and all key-value pairs in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(k1, v1, ...f(kn-1, vn-1, f(kn, vn, z)))`.
    /// A `foldRightWithKeyCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithKeyCont(f: (k, v, Unit -> b & ef) -> b & ef, z: b, m: BagMultiMap[k, v]): b & ef =
        let BagMultiMap(m1) = m;
        Map.foldRightWithKeyCont((k, vs, b1) -> Nec.foldRightWithCont((v1, fac) -> f(k, v1, fac), b1(), vs), z, m1)

        /// ??? We call the outer continuation `b1` - is this correct?

    ///
    /// Applies `f` to all values in `m` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(v1, v2), v3)..., vn))`
    ///
    /// Returns `None` if `m` is the empty BagMultiMap.
    ///
    pub def reduceLeft(f: (v, v) -> v & ef, m: BagMultiMap[k, v]): Option[v] & ef =
        reduceLeftWithKey((_, v1, k, v2) -> (k, f(v1, v2)), m) |> Option.map(snd)

    ///
    /// Applies `f` to all mappings in `m` going from left to right until a single mapping `(k, v)` is obtained.  Returns `Some((k, v))`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(k1, v1, k2, v2), k3, v3)..., kn, vn))`
    ///
    /// Returns `None` if `m` is the empty BagMultiMap.
    ///
    pub def reduceLeftWithKey(f: (k, v, k, v) -> (k, v) & ef, m: BagMultiMap[k, v]): Option[(k, v)] & ef =
        let BagMultiMap(m1) = m;
        let inner = (k, acc, v) -> match acc {
            case None               => Some((k, v))
            case Some((kacc, vacc)) => Some(f(kacc, vacc, k, v))
        };
        let outer = (acc, k, vs) -> Nec.foldLeft(inner(k), acc, vs);
        Map.foldLeftWithKey(outer, None, m1)

    ///
    /// Applies `f` to all values in `m` going from right to left until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(v1, ...f(vn-2, f(vn-1, vn))...))`
    ///
    /// Returns `None` if `m` is the empty BagMultiMap.
    ///
    pub def reduceRight(f: (v, v) -> v & ef, m: BagMultiMap[k, v]): Option[v] & ef =
        reduceRightWithKey((k, v1, _, v2) -> (k, f(v1, v2)), m) |> Option.map(snd)

    ///
    /// Applies `f` to all mappings in `m` going from right to left until a single mapping `(k, v)` is obtained.  Returns `Some((k, v))`.
    ///
    /// That is, the result is of the form: `Some(f(k1, v1, ...f(kn-2, vn-2, f(kn-1, vn-1, kn, vn))...))`
    ///
    /// Returns `None` if `m` is the empty BagMultiMap.
    ///
    pub def reduceRightWithKey(f: (k, v, k, v) -> (k, v) & ef, m: BagMultiMap[k, v]): Option[(k, v)] & ef =
        let BagMultiMap(m1) = m;
        let inner = (k, v, acc) -> match acc {
            case None               => Some((k, v))
            case Some((kacc, vacc)) => Some(f(k, v, kacc, vacc))
        };
        let outer = (k, vs, acc) -> Nec.foldRight(inner(k), acc, vs);
        Map.foldRightWithKey(outer, None, m1)

    ///
    /// Returns the number of mappings `k => v` in `m` that satisfy the predicate `f`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def count(f: (k, v) -> Bool & ef, m: BagMultiMap[k, v]): Int32 & ef =
        sumWith((k, v) -> if (f(k,v)) 1 else 0, m)

    ///
    /// Returns the sum of all keys in the BagMultiMap `m`.
    ///
    /// A key is counted foreach value it points to.
    ///
    pub def sumKeys(m: BagMultiMap[Int32, v]): Int32 =
        sumWith((k, _) -> k, m)

    ///
    /// Returns the sum of all values in the BagMultiMap `m`.
    ///
    pub def sumValues(m: BagMultiMap[k, Int32]): Int32 =
        sumWith((_, v) -> v, m)

    ///
    /// Returns the sum of all key-value pairs `k => v` in the BagMultiMap `m` according to the function `f`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def sumWith(f: (k, v) -> Int32 & ef, m: BagMultiMap[k, v]): Int32 & ef =
        let BagMultiMap(m1) = m;
        let outer = (k, vs) -> Nec.sumWith(v -> f(k, v), vs);
        Map.sumWith(outer, m1)

    ///
    /// Returns the product of all keys in the BagMultiMap `m`.
    ///
    pub def productKeys(m: BagMultiMap[Int32, v]): Int32 =
        productWith((k, _) -> k, m)

    ///
    /// Returns the product of all values in the BagMultiMap `m`.
    ///
    pub def productValues(m: BagMultiMap[k, Int32]): Int32 =
        productWith((_, v) -> v, m)

    ///
    /// Returns the product of all key-value pairs `k => v` in the BagMultiMap `m` according to the function `f`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def productWith(f: (k, v) -> Int32 & ef, m: BagMultiMap[k, v]): Int32 & ef =
        let BagMultiMap(m1) = m;
        let outer = (k, vs) -> Nec.productWith(v -> f(k, v), vs);
        Map.productWith(outer, m1)

    ///
    /// Returns `true` if and only if at least one mapping in `m` satisfies the predicate `f`.
    ///
    /// Returns `false` if `m` is the empty BagMultiMap.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def exists(f: (k, v) -> Bool & ef, m: BagMultiMap[k, v]): Bool & ef =
        let BagMultiMap(m1) = m;
        let outer = (k, vs) -> Nec.exists(v -> f(k, v), vs);
        Map.exists(outer, m1)

    ///
    /// Returns `true` if and only if all mappings in `m` satisfy the predicate `f`.
    ///
    /// Returns `true` if `m` is the empty BagMultiMap.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def forall(f: (k, v) -> Bool & ef, m: BagMultiMap[k, v]): Bool & ef =
        let BagMultiMap(m1) = m;
        let outer = (k, vs) -> Nec.forall(v -> f(k, v), vs);
        Map.forall(outer, m1)

    ///
    /// Returns the union of `m1` and `m2`.
    ///
    /// If a key is common to both BagMultiMaps then the left and right bags are concatenated.
    ///
    pub def union(m1: BagMultiMap[k, v], m2: BagMultiMap[k, v]): BagMultiMap[k, v] with Order[k] =
        let BagMultiMap(mm1) = m1;
        let BagMultiMap(mm2) = m2;
        BagMultiMap(Map.unionWith((n1, n2) -> Nec.append(n1, n2), mm1, mm2))

    ///
    /// Returns the intersection of `m1` and `m2`.
    ///
    pub def intersection(m1: BagMultiMap[k, v], m2: BagMultiMap[k, v]): BagMultiMap[k, v] with Order[k], Eq[v] =
        let BagMultiMap(mm1) = m1;
        let BagMultiMap(mm2) = m2;
        let step = (acc, k, vs) -> match Map.get(k, mm2) {
            case None => acc
            case Some(vs1) => match intersectionNec(vs, vs1) {
                case None      => acc
                case Some(vs2) => Map.insert(k, vs2, acc)
            }
        };
        BagMultiMap(Map.foldLeftWithKey(step, Map.empty(), mm1))

    ///
    /// Helper function for `intersection`.
    ///
    def intersectionNec(n1: Nec[v], n2: Nec[v]): Option[Nec[v]] with Eq[v] =
        Nec.foldLeft((acc, v) -> if (Nec.memberOf(v, n2) and not Chain.memberOf(v, acc)) Chain.snoc(acc, v) else acc, Chain.empty(), n1)
            |> Chain.toNec

    ///
    /// Returns the difference of `m1` and `m2` i.e `m1 - m2` (left biased).
    ///
    pub def difference(m1: BagMultiMap[k, v], m2: BagMultiMap[k, v]): BagMultiMap[k, v] with Order[k], Eq[v] =
        let BagMultiMap(mm1) = m1;
        let BagMultiMap(mm2) = m2;
        BagMultiMap(Map.differenceWith(differenceNec, mm1, mm2))

    ///
    /// Returns the difference of `n1` and `n2` i.e `n1 - n2` (left biased).
    ///
    def differenceNec(n1: Nec[v], n2: Nec[v]): Option[Nec[v]] with Eq[v] =
        necRemoveLefts(Nec.toList(n2), n1)

    ///
    /// Helper for `differenceNec`.
    ///
    def necRemoveLefts(xs: List[a], nec: Nec[a]): Option[Nec[a]] with Eq[a] =
        def loop(acc, ds) = match (acc, ds) {
            case (None, _)            => None
            case (Some(n1), Nil)      => Some(n1)
            case (Some(n1), d1 :: rs) => let n2 = necRemoveLeft(d1, n1); loop(n2, rs)
        };
        loop(Some(nec), xs)

    ///
    /// Helper for `differenceNec`.
    ///
    def necRemoveLeft(x: a, nec: Nec[a]): Option[Nec[a]] with Eq[a] =
        use Nec.ViewLeft.{OneLeft, SomeLeft};
        let snoc1 = (acc, b) -> match acc {
            case None             => Some(Nec.singleton(b))
            case Some(acc1)       => Some(Nec.snoc(acc1, b))
        };
        let append1 = (acc, bs) -> match acc {
            case None             => Some(bs)
            case Some(acc1)       => Some(Nec.append(acc1, bs))
        };
        def loop(acc, c) = match Nec.viewLeft(c) {
            case OneLeft(b) if b == x      => acc
            case OneLeft(b)                => snoc1(acc, b)
            case SomeLeft(b, rs) if b == x => append1(acc, rs)
            case SomeLeft(b, rs)           => loop(snoc1(acc, b), rs)
        };
        loop(None, nec)



    ///
    /// Returns the BagMultiMap `m` as a list of singleton key-value pairs.
    ///
    pub def toList(m: BagMultiMap[k, v]): List[(k, v)] = toAscList(m)

    ///
    /// Returns the BagMultiMap `m` as a list of singleton key-value pairs in ascending order.
    ///
    pub def toAscList(m: BagMultiMap[k, v]): List[(k, v)] =
        foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil, m)


    ///
    /// Returns the BagMultiMap `m` as a list of singleton key-value pairs in descending order.
    ///
    pub def toDescList(m: BagMultiMap[k, v]): List[(k, v)] =
        foldLeftWithKey((acc, k, v) -> (k, v) :: acc, Nil, m)


    ///
    /// Returns the BagMultiMap `m` as a list of key * List[value] pairs in ascending order.
    ///
    pub def toAssocList(m: BagMultiMap[k, v]): List[(k, List[v])] =
        let BagMultiMap(m1) = m;
        Map.foldRightWithKey((k, vs, acc) -> (k, Nec.toList(vs)) :: acc, Nil, m1)

    ///
    /// Returns the result of running all the actions in the BagMultiMap `m`.
    ///
    pub def sequence(m: BagMultiMap[k, m[v]]): m[BagMultiMap[k, v]] with Applicative[m] =
        traverse(identity, m)

    ///
    /// Returns the result of applying the applicative mapping function `f` to all the values of the
    /// BagMultiMap `m`.
    ///
    pub def traverse(f: v1 -> m[v2] & ef, m: BagMultiMap[k, v1]): m[BagMultiMap[k, v2]] & ef with Applicative[m] =
        let BagMultiMap(m1) = m;
        Functor.map(BagMultiMap, Map.traverse(Traversable.traverse(f), m1))

    ///
    /// Returns the result of applying the applicative mapping function `f` to all the key-value pairs
    /// of the BagMultiMap `m`.
    ///
    pub def traverseWithKey(f: (k, v1) -> m[v2] & ef, m: BagMultiMap[k, v1]): m[BagMultiMap[k, v2]] & ef with Applicative[m] =
        let BagMultiMap(m1) = m;
        Functor.map(BagMultiMap, Map.traverseWithKey((k, vs) -> Traversable.traverse(f(k), vs), m1))


}
