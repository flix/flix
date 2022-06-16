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
pub enum Map[k, v] {
    case Map(RedBlackTree[k, v])
}

instance Boxable[Map[k, v]] with Boxable[k], Boxable[v]

instance ToString[Map[k, v]] with ToString[k], ToString[v] {
    pub def toString(m: Map[k, v]): String = Map.toString(m)
}

instance Hash[Map[k, v]] with Hash[k], Hash[v] {
    pub def hash(m: Map[k, v]): Int32 =
        Map.foldLeftWithKey((acc, k, v) -> acc * 17 + Hash.hash(k) + 19 * Hash.hash(v), 7937, m)
}

instance Eq[Map[k, v]] with Eq[k], Eq[v] {
    pub def eq(m1: Map[k, v], m2: Map[k, v]): Bool =
        Map.toList(m1) == Map.toList(m2)

}

instance Order[Map[k, v]] with Order[k], Order[v] {

    pub def compare(x: Map[k, v], y: Map[k, v]): Comparison =
        Map.toList(x) <=> Map.toList(y)

}

instance Functor[Map[k]] {
    pub def map(f: v1 -> v2 & ef, m: Map[k, v1]): Map[k, v2] & ef = Map.map(f, m)
}

instance ForEach[Map[k]] {
    pub def foreach(f: a -> Unit & ef, m: Map[k, a]): Unit & ef = Map.foreach((_, v) -> f(v), m)
}

instance Foldable[Map[k]] {
    pub def foldLeft(f: (b, v) -> b & ef, s: b, m: Map[k, v]): b & ef = Map.foldLeft((acc, v) -> f(acc, v), s, m)
    pub def foldRight(f: (v, b) -> b & ef, s: b, m: Map[k, v]): b & ef = Map.foldRight(f, s, m)
    pub def foldRightWithCont(f: (v, Unit -> b & ef) -> b & ef, s: b, m: Map[k, v]): b & ef = Map.foldRightWithCont(f, s, m)
}

instance Traversable[Map[k]] {
    pub def traverse(f: a -> m[b] & ef, t: Map[k, a]): m[Map[k, b]] & ef with Applicative[m] = Map.traverse(f, t)
    pub override def sequence(t: Map[k, m[a]]): m[Map[k, a]] with Applicative[m] = Map.sequence(t)
}

instance SemiGroup[Map[k,v]] with Order[k], SemiGroup[v] {
    pub def combine(x: Map[k,v], y: Map[k,v]): Map[k,v] = Map.unionWith(SemiGroup.combine, x, y)
}

instance CommutativeSemiGroup[Map[k,v]] with Order[k], CommutativeSemiGroup[v]

instance Monoid[Map[k, v]] with Order[k], Monoid[v] {
    pub def empty(): Map[k, v] = Map.empty()
}

instance CommutativeMonoid[Map[k, v]] with Order[k], CommutativeMonoid[v]

instance LowerBound[Map[k, v]] {
    pub def minValue(): Map[k, v] = Map.empty()
}

instance PartialOrder[Map[k, v]] with Order[k], Eq[v] {
    pub def lessEqual(m1: Map[k, v], m2: Map[k, v]): Bool = m1 `Map.isSubmapOf` m2
}

instance JoinLattice[Map[k, v]] with Order[k], Eq[v], JoinLattice[v] {
    pub def leastUpperBound(m1: Map[k, v], m2: Map[k, v]): Map[k, v] =
        Map.unionWith(JoinLattice.leastUpperBound, m1, m2)
}

instance MeetLattice[Map[k, v]] with Order[k], Eq[v], MeetLattice[v] {
    pub def greatestLowerBound(m1: Map[k, v], m2: Map[k, v]): Map[k, v] =
        Map.intersectionWith(MeetLattice.greatestLowerBound, m1, m2)
}

namespace Map {

    ///
    /// Returns a string representation of the given map `m`.
    ///
    pub def toString(m: Map[k, v]): String with ToString[k], ToString[v] = region r {
        let sb = new StringBuilder(r);
        let first = ref true;

        StringBuilder.appendString!("Map#{", sb);

        let f = (k, v) -> {
            if (not deref first) StringBuilder.appendString!(", ", sb) else first := false;
            StringBuilder.appendString!("${k} => ${v}", sb)
        };
        Map.foreach(f, m);

        StringBuilder.appendString!("}", sb);
        StringBuilder.toString(sb)
    }

    ///
    /// Returns the number of threads to use for parallel evaluation.
    ///
    def threads(): Int32 =
        // Note: We use a multiple of the number of physical cores for better performance.
        let multiplier = 4;
        multiplier * Environment.getVirtualProcessors()

    ///
    /// Determines whether to use parallel evaluation.
    ///
    /// By default we only enable parallel evaluation if the map has a certain size.
    ///
    def useParallelEvaluation(m: Map[k, v]): Bool =
        let Map(t) = m;
        let minSize = 2 ** RedBlackTree.blackHeight(t);
        minSize >= 1024

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
    /// `Map#{k => v}` is syntactic sugar for `singleton` (`Map#{k => v} = singleton(k, v)`).
    ///
    @Time(1) @Space(1)
    pub def singleton(k: k, v: v): Map[k, v] with Order[k] = insert(k, v, empty())

    ///
    /// Returns `true` if and only if `m` is the empty map, i.e. `Map(Nil)`.
    ///
    @Time(1) @Space(1)
    pub def isEmpty(m: Map[k, v]): Bool =
        let Map(t) = m;
        RedBlackTree.isEmpty(t)

    ///
    /// Returns `Some(v)` if `k => v` is in `m`.
    ///
    /// Otherwise returns `None`.
    ///
    @Time(Int32.log2(size(m))) @Space(1)
    pub def get(k: k, m: Map[k, v]): Option[v] with Order[k] =
        let Map(t) = m;
        RedBlackTree.get(k, t)

    ///
    /// Returns `v` if `k => v` is in `m`.
    ///
    /// Otherwise, returns `d`.
    ///
    @Time(Int32.log2(size(m))) @Space(1)
    pub def getWithDefault(k: k, d: v, m: Map[k, v]): v with Order[k] = Option.getWithDefault(d, get(k, m))

    ///
    /// Returns `true` if and only if `m` contains the key `k`.
    ///
    @Time(Int32.log2(size(m))) @Space(1)
    pub def memberOf(k: k, m: Map[k, v]): Bool with Order[k] =
        let Map(t) = m;
        RedBlackTree.memberOf(k, t)

    ///
    /// Optionally finds `k => v` where `k` is the smallest key according to the `Order` instance of `k`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    pub def minimumKey(m: Map[k, v]): Option[(k, v)] =
        let Map(t) = m;
        RedBlackTree.minimumKey(t)

    ///
    /// Optionally finds `k => v` where `k` is the smallest key according to the given comparator `cmp`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `cmp`.
    ///
    @ParallelWhenPure
    pub def minimumKeyBy(cmp: (k, k) -> Comparison & ef, m: Map[k, v]): Option[(k, v)] & ef =
        def min() = reduceLeftWithKey((kl, vl, kr, vr) -> if (cmp(kl, kr) == LessThan) (kl, vl) else (kr, vr), m);
        let f = r -> cmp(r.kl, r.kr);
        reifyEff(f) {
            case Pure(g) =>
                if (useParallelEvaluation(m))
                    let h = (kl, _, kr, _) -> g({kl = kl, kr = kr});
                    let Map(t) = m;
                    RedBlackTree.parMinimumBy(threads() - 1, h, t) as & Pure
                else
                    min()
            case _ => min()
        }

    ///
    /// Optionally finds `k => v` where `v` is the smallest value.
    ///
    /// Returns `None` if `m` is empty.
    ///
    @Parallel
    pub def minimumValue(m: Map[k, v]): Option[(k, v)] with Order[v] =
        minimumValueBy((x, y) -> x <=> y, m)

    ///
    /// Optionally finds `k => v` where `v` is the smallest value according to the given comparator `cmp`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `cmp`.
    ///
    @ParallelWhenPure
    pub def minimumValueBy(cmp: (v, v) -> Comparison & ef, m: Map[k, v]): Option[(k, v)] & ef =
        def min() = reduceLeftWithKey((kl, vl, kr, vr) -> if (cmp(vl, vr) == LessThan) (kl, vl) else (kr, vr), m);
        let f = r -> cmp(r.vl, r.vr);
        reifyEff(f) {
            case Pure(g) =>
                if (useParallelEvaluation(m))
                    let h = (_, vl, _, vr) -> g({vl = vl, vr = vr});
                    let Map(t) = m;
                    RedBlackTree.parMinimumBy(threads() - 1, h, t) as & Pure
                else
                    min()
            case _ => min()
        }

    ///
    /// Optionally finds `k => v` where `k` is the largest key according to the `Order` instance of `k`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    pub def maximumKey(m: Map[k, v]): Option[(k, v)] =
        let Map(t) = m;
        RedBlackTree.maximumKey(t)

    ///
    /// Optionally finds `k => v` where `k` is the largest key according to the given comparator `cmp`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `cmp`.
    ///
    @ParallelWhenPure
    pub def maximumKeyBy(cmp: (k, k) -> Comparison & ef, m: Map[k, v]): Option[(k, v)] & ef =
        def max() = reduceLeftWithKey((kl, vl, kr, vr) -> if (cmp(kl, kr) == GreaterThan) (kl, vl) else (kr, vr), m);
        let f = r -> cmp(r.kl, r.kr);
        reifyEff(f) {
            case Pure(g) =>
                if (useParallelEvaluation(m))
                    let h = (kl, _, kr, _) -> g({kl = kl, kr = kr});
                    let Map(t) = m;
                    RedBlackTree.parMaximumBy(threads() - 1, h, t) as & Pure
                else
                    max()
            case _ => max()
        }

    ///
    /// Optionally finds `k => v` where `v` is the largest value.
    ///
    /// Returns `None` if `m` is empty.
    ///
    @Parallel
    pub def maximumValue(m: Map[k, v]): Option[(k, v)] with Order[v] =
        maximumValueBy((x, y) -> x <=> y, m)

    ///
    /// Optionally finds `k => v` where `v` is the largest value according to the given comparator `cmp`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `cmp`.
    ///
    @ParallelWhenPure
    pub def maximumValueBy(cmp: (v, v) -> Comparison & ef, m: Map[k, v]): Option[(k, v)] & ef =
        def max() = reduceLeftWithKey((kl, vl, kr, vr) -> if (cmp(vl, vr) == GreaterThan) (kl, vl) else (kr, vr), m);
        let f = r -> cmp(r.vl, r.vr);
        reifyEff(f) {
            case Pure(g) =>
                if (useParallelEvaluation(m))
                    let h = (_, vl, _, vr) -> g({vl = vl, vr = vr});
                    let Map(t) = m;
                    RedBlackTree.parMaximumBy(threads() - 1, h, t) as & Pure
                else
                    max()
            case _ => max()
        }

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
    /// Updates `m` with `k => v`.
    ///
    @Time(Int32.log2(size(m))) @Space(Int32.log2(size(m)))
    pub def insert(k: k, v: v, m: Map[k, v]): Map[k, v] with Order[k] =
        let Map(t) = m;
        Map(RedBlackTree.insert(k, v, t))

    ///
    /// Updates `m` with `k => f(v, v1)` if `k => v1` is in `m`.
    ///
    /// Otherwise, updates `m` with `k => v`.
    ///
    @Time(time(f) + Int32.log2(size(m))) @Space(space(f) + Int32.log2(size(m)))
    pub def insertWith(f: (v, v) -> v & ef, k: k, v: v, m: Map[k, v]): Map[k, v] & ef with Order[k] =
        insertWithKey((_, v1, v2) -> f(v1, v2), k, v, m)

    ///
    /// Updates `m` with `k => f(k, v, v1)` if `k => v1` is in `m`.
    ///
    /// Otherwise, updates `m` with `k => v`.
    ///
    @Time(time(f) + Int32.log2(size(m))) @Space(space(f) + Int32.log2(size(m)))
    pub def insertWithKey(f: (k, v, v) -> v & ef, k: k, v: v, m: Map[k, v]): Map[k, v] & ef with Order[k] =
        let Map(t) = m;
        Map(RedBlackTree.insertWith(f, k, v, t))

    ///
    /// Updates `m` with `k => f(v)` if `k => v` is in `m`.
    ///
    /// Otherwise, returns `m`.
    ///
    @Time(time(f) + Int32.log2(size(m))) @Space(space(f) + Int32.log2(size(m)))
    pub def adjust(f: v -> v & ef, k: k, m: Map[k, v]): Map[k, v] & ef with Order[k] =
        adjustWithKey((_, v1) -> f(v1), k, m)

    ///
    /// Updates `m` with `k => f(k, v)` if `k => v` is in `m`. Otherwise, returns `m`.
    ///
    @Time(time(f) + Int32.log2(size(m))) @Space(space(f) + Int32.log2(size(m)))
    pub def adjustWithKey(f: (k, v) -> v & ef, k: k, m: Map[k, v]): Map[k, v] & ef with Order[k] =
        updateWithKey((k1, v) -> Some(f(k1, v)), k, m)

    ///
    /// Updates `m` with `k => v1` if `k => v` is in `m` and `f(v) = Some(v1)`. Otherwise, returns `m`.
    ///
    @Time(time(f) + Int32.log2(size(m))) @Space(space(f) + Int32.log2(size(m)))
    pub def update(f: v -> Option[v] & ef, k: k, m: Map[k, v]): Map[k, v] & ef with Order[k] =
        updateWithKey((_, v1) -> f(v1), k, m)

    ///
    /// Updates `m` with `k => v1` if `k => v` is in `m` and `f(k, v) = Some(v1)`. Otherwise, returns `m`.
    ///
    @Time(time(f) + Int32.log2(size(m))) @Space(space(f) + Int32.log2(size(m)))
    pub def updateWithKey(f: (k, v) -> Option[v] & ef, k: k, m: Map[k, v]): Map[k, v] & ef with Order[k] =
        let Map(t) = m;
        Map(RedBlackTree.updateWith(f, k, t))

    ///
    /// Removes the mapping `k` from the map `m`.
    ///
    @Time(Int32.log2(size(m))) @Space(Int32.log2(size(m)))
    pub def remove(k: k, m: Map[k, v]): Map[k, v] with Order[k] =
        let Map(t) = m;
        Map(RedBlackTree.remove(k, t))

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
    @Time(time(f) * size(m)) @Space(space(f) * Int32.log2(size(m)))
    pub def find(f: (k, v) -> Bool & ef, m: Map[k, v]): Option[(k, v)] & ef = findLeft(f, m)

    ///
    /// Optionally returns the first mapping of `m` that satisfies the predicate `f` when searching from left to right.
    ///
    @Time(time(f) * size(m)) @Space(space(f) * Int32.log2(size(m)))
    pub def findLeft(f: (k, v) -> Bool & ef, m: Map[k, v]): Option[(k, v)] & ef =
        let Map(t) = m;
        RedBlackTree.findLeft(f, t)

    ///
    /// Optionally returns the first mapping of `m` that satisfies the predicate `f` when searching from right to left.
    ///
    @Time(time(f) * size(m)) @Space(space(f) * Int32.log2(size(m)))
    pub def findRight(f: (k, v) -> Bool & ef, m: Map[k, v]): Option[(k, v)] & ef =
        let Map(t) = m;
        RedBlackTree.findRight(f, t)

    ///
    /// Returns a map of all mappings `k => v` in `m` where `v` satisfies the predicate `f`.
    ///
    @Time(time(f) * size(m)) @Space(space(f) * size(m))
    pub def filter(f: v -> Bool & ef, m: Map[k, v]): Map[k, v] & ef with Order[k] = filterWithKey((_, v) -> f(v), m)

    ///
    /// Returns a map of all mappings `k => v` in `m` where `(k, v)` satisfies the predicate `f`.
    ///
    @Time(time(f) * size(m)) @Space(space(f) * size(m))
    pub def filterWithKey(f: (k, v) -> Bool & ef, m: Map[k, v]): Map[k, v] & ef with Order[k] =
        foldLeftWithKey((acc, k, v) -> if (f(k, v)) insert(k, v, acc) else acc, empty(), m)

    ///
    /// Returns a map with mappings `k => f(v)` for every `k => v` in `m`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @Time(time(f) * size(m)) @Space(space(f) * size(m))
    @ParallelWhenPure
    pub def map(f: v1 -> v2 & ef, m: Map[k, v1]): Map[k, v2] & ef = mapWithKey((_, v) -> f(v), m)

    ///
    /// Returns a map with mappings `k => f(k, v)` for every `k => v` in `m`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @Time(time(f) * size(m)) @Space(space(f) * size(m))
    @ParallelWhenPure
    pub def mapWithKey(f: (k, v1) -> v2 & ef, m: Map[k, v1]): Map[k, v2] & ef =
        let Map(t) = m;
        Map(RedBlackTree.mapWithKey(f, t))

    ///
    /// Alias for `foldLeftWithKey`.
    ///
    pub def foldWithKey(f: (b, k, v) -> b & ef, s: b, m: Map[k, v]): b & ef = foldLeftWithKey(f, s, m)

    ///
    /// Applies `f` to a start value `s` and all values in `m` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, v1), v2)..., vn)`.
    ///
    pub def foldLeft(f: (b, v) -> b & ef, s: b, m: Map[k, v]): b & ef =
        foldLeftWithKey((acc, _, v) -> f(acc, v), s, m)

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `m` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, k1, v1), k2, v2)..., vn)`.
    ///
    pub def foldLeftWithKey(f: (b, k, v) -> b & ef, s: b, m: Map[k, v]): b & ef =
        let Map(xs) = m;
        RedBlackTree.foldLeft(f, s, xs)

    ///
    /// Applies `f` to a start value `s` and all values in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(v1, ...f(vn-1, f(vn, s)))`.
    ///
    pub def foldRight(f: (v, b) -> b & ef, s: b, m: Map[k, v]): b & ef =
        foldRightWithKey((_, v, acc) -> f(v, acc), s, m)

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(k1, v1, ...f(kn-1, vn-1, f(kn, vn, s)))`.
    ///
    pub def foldRightWithKey(f: (k, v, b) -> b & ef, s: b, m: Map[k, v]): b & ef =
        let Map(t) = m;
        RedBlackTree.foldRight(f, s, t)

    ///
    /// Applies `f` to a start value `z` and all values in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(v1, ...f(vn-1, f(vn, z)))`.
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (v, Unit -> b & ef) -> b & ef, z: b, m: Map[k, v]): b & ef =
        foldRightWithKeyCont((_, v, c) -> f(v, c), z, m)

    ///
    /// Applies `f` to a start value `z` and all key-value pairs in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(k1, v1, ...f(kn-1, vn-1, f(kn, vn, z)))`.
    /// A `foldRightWithKeyCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithKeyCont(f: (k, v, Unit -> b & ef) -> b & ef, z: b, m: Map[k, v]): b & ef =
        let Map(t) = m;
        RedBlackTree.foldRightWithCont(f, z, t)

    ///
    /// Returns the result of mapping each key-value pair and combining the results.
    ///
    pub def foldMapWithKey(f: (k, v) -> b & ef, m: Map[k, v]): b & ef with Monoid[b] =
        foldLeftWithKey((acc, k, v) -> Monoid.combine(acc, f(k, v)), Monoid.empty(), m)

    ///
    /// Applies `f` to all values in `m` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(v1, v2), v3)..., vn))`
    ///
    /// Returns `None` if `m` is the empty map.
    ///
    pub def reduceLeft(f: (v, v) -> v & ef, m: Map[k, v]): Option[v] & ef =
        reduceLeftWithKey((k, v1, _, v2) -> (k, f(v1, v2)), m) |> Option.map(snd)

    ///
    /// Applies `f` to all mappings in `m` going from left to right until a single mapping `(k, v)` is obtained.  Returns `Some((k, v))`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(k1, v1, k2, v2), k3, v3)..., kn, vn))`
    ///
    /// Returns `None` if `m` is the empty map.
    ///
    pub def reduceLeftWithKey(f: (k, v, k, v) -> (k, v) & ef, m: Map[k, v]): Option[(k, v)] & ef =
        let Map(t) = m;
        RedBlackTree.reduceLeft(f, t)

    ///
    /// Applies `f` to all values in `m` going from right to left until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(v1, ...f(vn-2, f(vn-1, vn))...))`
    ///
    /// Returns `None` if `m` is the empty map.
    ///
    pub def reduceRight(f: (v, v) -> v & ef, m: Map[k, v]): Option[v] & ef =
        reduceRightWithKey((k, v1, _, v2) -> (k, f(v1, v2)), m) |> Option.map(snd)

    ///
    /// Applies `f` to all mappings in `m` going from right to left until a single mapping `(k, v)` is obtained.  Returns `Some((k, v))`.
    ///
    /// That is, the result is of the form: `Some(f(k1, v1, ...f(kn-2, vn-2, f(kn-1, vn-1, kn, vn))...))`
    ///
    /// Returns `None` if `m` is the empty map.
    ///
    pub def reduceRightWithKey(f: (k, v, k, v) -> (k, v) & ef, m: Map[k, v]): Option[(k, v)] & ef =
        let Map(t) = m;
        RedBlackTree.reduceRight(f, t)

    ///
    /// Returns the number of mappings `k => v` in `m` that satisfy the predicate `f`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def count(f: (k, v) -> Bool & ef, m: Map[k, v]): Int32 & ef =
        def c() = foldLeftWithKey((b, k, v) -> if (f(k, v)) b + 1 else b, 0, m);
        let f1 = r -> f(r.k, r.v);
        reifyEff(f1) {
            case Pure(g) =>
                if (useParallelEvaluation(m))
                    let h = (k, v) -> g({k = k, v = v});
                    let Map(t) = m;
                    RedBlackTree.parCount(threads() - 1, h, t) as & Pure
                else
                    c()
            case _  => c()
        }

    ///
    /// Returns the sum of all keys in the map `m`.
    ///
    pub def sumKeys(m: Map[Int32, v]): Int32 =
        sumWith((k, _) -> k, m)

    ///
    /// Returns the sum of all values in the map `m`.
    ///
    pub def sumValues(m: Map[k, Int32]): Int32 =
        sumWith((_, v) -> v, m)

    ///
    /// Returns the sum of all key-value pairs `k => v` in the map `m` according to the function `f`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def sumWith(f: (k, v) -> Int32 & ef, m: Map[k, v]): Int32 & ef =
        let f1 = match (k, v) -> f(k, v);
        let Map(t) = m;
        def sw() = RedBlackTree.sumWith(f, t);
        reifyEff(f1) {
            case Pure(g) =>
                if (useParallelEvaluation(m))
                    RedBlackTree.parSumWith(threads() - 1, (k, v) -> g((k, v)), t) as & Pure
                else
                    sw()
            case _ => sw()
        }

    ///
    /// Returns the product of all keys in the map `m`.
    ///
    pub def productKeys(m: Map[Int32, v]): Int32 =
        let Map(t) = m;
        RedBlackTree.productKeys(t)

    ///
    /// Returns the product of all values in the map `m`.
    ///
    pub def productValues(m: Map[k, Int32]): Int32 =
        let Map(t) = m;
        RedBlackTree.productValues(t)

    ///
    /// Returns the product of all key-value pairs `k => v` in the map `m` according to the function `f`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def productWith(f: (k, v) -> Int32 & ef, m: Map[k, v]): Int32 & ef =
        let f1 = match (k, v) -> f(k, v);
        let Map(t) = m;
        def pw() = RedBlackTree.productWith(f, t);
        reifyEff(f1) {
            case Pure(g) =>
                if (useParallelEvaluation(m))
                    RedBlackTree.parProductWith(threads() - 1, (k, v) -> g((k, v)), t) as & Pure
                else
                    pw()
            case _ => pw()
        }

    ///
    /// Returns `true` if and only if at least one mapping in `m` satisfies the predicate `f`.
    ///
    /// Returns `false` if `m` is the empty map.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure @Time(time(f) * size(m)) @Space(space(f) * Int32.log2(size(m)))
    pub def exists(f: (k, v) -> Bool & ef, m: Map[k, v]): Bool & ef =
        let Map(t) = m;
        let f1 = match (k, v) -> f(k, v);
        def e() = RedBlackTree.exists(f, t);
        reifyEff(f1) {
            case Pure(g) =>
                if (useParallelEvaluation(m))
                    RedBlackTree.parExists(threads() - 1, (k, v) -> g((k, v)), t) as & Pure
                else
                    e()
            case _ => e()
        }

    ///
    /// Returns `true` if and only if all mappings in `m` satisfy the predicate `f`.
    ///
    /// Returns `true` if `m` is the empty map.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure @Time(time(f) * size(m)) @Space(space(f) * Int32.log2(size(m)))
    pub def forall(f: (k, v) -> Bool & ef, m: Map[k, v]): Bool & ef =
        let Map(t) = m;
        let f1 = match (k, v) -> f(k, v);
        def fa() = RedBlackTree.forall(f, t);
        reifyEff(f1) {
            case Pure(g) =>
                if (useParallelEvaluation(m))
                    RedBlackTree.parForall(threads() - 1, (k, v) -> g((k, v)), t) as & Pure
                else
                    fa()
            case _ => fa()
        }

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
    pub def unionWith(f: (v, v) -> v & ef, m1: Map[k, v], m2: Map[k, v]): Map[k, v] & ef with Order[k] =
        unionWithKey((_, v1, v2) -> f(v1, v2), m1, m2)

    ///
    /// Returns the union of `m1` and `m2` where key collisions are resolved with the merge function `f`, taking both the key and values.
    ///
    @Time(time(f) * size(m1) * Int32.log2(size(m2))) @Space(space(f) * size(m1) *  Int32.log2(size(m2)))
    pub def unionWithKey(f: (k, v, v) -> v & ef, m1: Map[k, v], m2: Map[k, v]): Map[k, v] & ef with Order[k] =
        use RedBlackTree.{blackHeight, foldRight, insertWith};
        let Map(t1) = m1;
        let Map(t2) = m2;
        if (blackHeight(t1) < blackHeight(t2))
            Map(foldRight((k, v, acc) -> insertWith(f, k, v, acc), t2, t1))
        else
            Map(foldRight((k, v, acc) -> insertWith((_, v1, v2) -> f(k, v2, v1), k, v, acc), t1, t2))

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
    pub def intersectionWith(f: (v, v) -> v & ef, m1: Map[k, v], m2: Map[k, v]): Map[k, v] & ef with Order[k] =
        intersectionWithKey((_, v1, v2) -> f(v1, v2), m1, m2)

    ///
    /// Returns the intersection of `m1` and `m2` where key collisions are resolved with the merge function `f`, taking both the key and values.
    ///
    @Time(time(f) * size(m1) * Int32.log2(size(m2))) @Space(space(f) * size(m1) *  Int32.log2(size(m2)))
    pub def intersectionWithKey(f: (k, v, v) -> v & ef, m1: Map[k, v], m2: Map[k, v]): Map[k, v] & ef with Order[k] =
        let intersect = filterWithKey((k, _) -> memberOf(k, m1), m2);
        foldRightWithKey((k, v, acc) -> adjustWithKey((key, v1) -> f(key, v, v1), k, acc), intersect, m1)

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
    /// If `f` returns `Some(v)` the mapping `k => v` is included in the result.
    ///
    @Time(size(f) * size(m2) * Int32.log2(size(m1))) @Space(space(f) * size(m2) * Int32.log2(size(m1)))
    pub def differenceWith(f: (v, v) -> Option[v] & ef, m1: Map[k, v], m2: Map[k, v]): Map[k, v] & ef with Order[k] =
        differenceWithKey((_, v1, v2) -> f(v1, v2), m1, m2)

    ///
    /// Returns the difference of `m1` and `m2`, i.e. `m1 - m2`.
    ///
    /// When a key `k` is in both `m1` and `m2`, `k` and the associated values are passed to the merge function `f`.
    /// If `f` returns `None` the mapping with `k` is thrown away (proper set difference).
    /// If `f` returns `Some(v)` the mapping `k => v` is included in the result.
    ///
    @Time(size(f) * size(m2) * Int32.log2(size(m1))) @Space(space(f) * size(m2) * Int32.log2(size(m1)))
    pub def differenceWithKey(f: (k, v, v) -> Option[v] & ef, m1: Map[k, v], m2: Map[k, v]): Map[k, v] & ef with Order[k] =
        let diff = filterWithKey((k, _) -> not memberOf(k, m2), m1);
        let g = (k, v, acc) -> if (memberOf(k, m1))
                                match get(k, m1) {
                                    case Some(v1) => match f(k, v1, v) {
                                        case None => acc
                                        case Some(w) => insert(k, w, acc)
                                    }
                                    case None => unreachable!()
                                }
                             else acc;
        foldRightWithKey(g, diff, m2)

    ///
    /// Returns the inverse map of `m`.
    ///
    /// That is, given a `Map[k, v]` returns a map `Map[v, Set[k]]`
    /// where every value is mapped to its key(s) in the original map.
    ///
    pub def invert(m: Map[k, v]): Map[v, Set[k]] with Order[k], Order[v] =
        let f = (acc, k, v) -> Map.insertWith(Set.union, v, Set#{k}, acc);
        Map.foldLeftWithKey(f, empty(), m)

    ///
    /// Returns `m` as a mutable set.
    ///
    @Time(1) @Space(1)
    pub def toMutMap(m: Map[k, v], r: Region[r]): MutMap[k, v, r] \ Write(r) =
        MutMap(ref m @ r)

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
    /// Returns the map `m` as a `DelayList` of key-value pairs.
    ///
    pub def toDelayList(m: Map[k, v]): DelayList[(k, v)] =
        foldRightWithKey((k, v, acc) -> ECons((k, v), acc), ENil, m)

    ///
    /// Returns the map `m` as a chain of key-value pairs.
    ///
    pub def toChain(m: Map[a, b]): Chain[(a, b)] with Order[a] =
        foldLeftWithKey((acc, k, v) -> Chain.snoc(acc, (k,v)), Chain.empty(), m)

    ///
    /// Returns a MultiMap where key `k` is mapped to the singleton set containing `v`.
    ///
    pub def toMultiMap(m: Map[k, v]): MultiMap[k, v] with Order[k], Order[v] =
        MultiMap(Map.map(Set.singleton, m))

    ///
    /// Returns `m` as a MutDeque.
    ///
    pub def toMutDeque(m: Map[k, v], r: Region[r]): MutDeque[(k, v), r] \ Write(r) =
        let d = new MutDeque(r);
        let f = (k, v) -> { let x = (k, v); MutDeque.pushBack(x, d) };
        Map.foreach(f, m);
        d

    ///
    /// Applies `f` to every element of `xs`.
    ///
    @Time(size(f) * size(m)) @Space(space(f) * Int32.log2(size(m)))
    pub def foreach(f: (k, v) -> Unit & ef, m: Map[k, v]): Unit & ef =
        let Map(t) = m;
        RedBlackTree.foreach(f, t)

    ///
    /// Build a map by applying `f` to the seed value `st`.
    ///
    /// `f` should return `Some(k,v,st1)` to signal a new key-value pair `k` and `v` and a new seed value `st1`.
    ///
    /// `f` should return `None` to signal the end of building the map.
    ///
    pub def unfold(f: s -> Option[(k, v, s)] & ef, st: s): Map[k,v] & ef with Order[k] =
        def loop(sst, m) = match f(sst) {
            case None            => m
            case Some(k, v, st1) => loop(st1, insert(k, v, m))
        };
        loop(st, empty())

    ///
    /// Build a map by applying the function `next` to `()`. `next` is expected to encapsulate
    /// a stateful resource such as a file handle that can be iterated.
    ///
    /// `next` should return `Some(k,v)` to signal a new key-value pair `k` and `v`.
    ///
    /// `next` should return `None` to signal the end of building the map.
    ///
    pub def unfoldWithIter(next: Unit -> Option[(k, v)] & ef): Map[k, v] & ef with Order[k] =
        def loop(m) = match next() {
            case None       => m
            case Some(k, v) => loop(insert(k, v, m))
        };
        loop(empty())

    ///
    /// Returns the set of tuples `(k, v)` where `v` is a value in `t` and `k => t`.
    ///
    pub def explode(m: Map[k, t[v]]): Set[(k, v)] with Foldable[t], Order[k], Order[v] =
        foldLeftWithKey((acc, k, t) -> Foldable.toSet(t) |> Set.map(e -> (k, e)) |> Set.union(acc), Set.empty(), m)

    ///
    /// Extracts a range of key-value pairs from the map `m`.
    ///
    /// That is, the result is a list of all pairs `(k, v)` where `p(k)` returns `Equal`.
    ///
    pub def query(p: k -> Comparison, m: Map[k, v]): List[(k, v)] =
        let Map(t) = m;
        RedBlackTree.query(p, (k, v) -> (k, v), t)

    ///
    /// Applies `f` to all key-value pairs `(k, v)` from the map `m` where `p(k)` returns `EqualTo`.
    ///
    pub def queryWith(p: k -> Comparison, f: (k, v) -> Unit & ef, m: Map[k, v]): Unit & ef =
        let Map(t) = m;
        RedBlackTree.queryWith(p, f, t)

    ///
    /// Returns an iterator over all key-value pairs in `m`.
    ///
    pub def toIterator(m: Map[a, b]): Iterator[(a, b)] & Impure =
        List.toIterator(Map.query(_ -> EqualTo, m))

    ///
    /// Returns an iterator over all key-value pairs in `m`.
    ///
    pub def toRegionIterator(r: Region[r], m: Map[k, v]): RegionIterator[(k, v), r] \ Write(r) =
        List.toRegionIterator(r, Map.query(_ -> EqualTo, m))

    ///
    /// Returns `m` as a `DelayMap`.
    ///
    pub def toDelayMap(m: Map[k, v]): DelayMap[k, v] = {
        let Map(t) = m;
        let f = (_, v) -> lazy v;
        DMap(RedBlackTree.mapWithKey(f, t))
    }

    ///
    /// Returns the result of running all the actions in the map `m`.
    ///
    pub def sequence(m: Map[k, m[v]]): m[Map[k, v]] with Applicative[m] =
        let Map(t) = m;
        Functor.map(Map, Traversable.sequence(t))

    ///
    /// Returns the result of applying the applicative mapping function `f` to all the values of the
    /// map `m`.
    ///
    pub def traverse(f: v1 -> m[v2] & ef, m: Map[k, v1]): m[Map[k, v2]] & ef with Applicative[m] =
        let Map(t) = m;
        Functor.map(Map, Traversable.traverse(f, t))

    ///
    /// Returns the result of applying the applicative mapping function `f` to all the key-value pairs
    /// of the map `m`.
    ///
    pub def traverseWithKey(f: (k, v1) -> m[v2] & ef, m: Map[k, v1]): m[Map[k, v2]] & ef with Applicative[m] =
        let Map(t) = m;
        Functor.map(Map, RedBlackTree.mapAWithKey(f, t))

    ///
    /// Returns the concatenation of the string representation of each key `k`
    /// in `m` with `sep` inserted between each element.
    ///
    pub def joinKeys(sep: String, m: Map[k, v]): String with ToString[k] =
        let Map(t) = m;
        RedBlackTree.joinKeys(sep, t)

    ///
    /// Returns the concatenation of the string representation of each value `v`
    /// in `m` with `sep` inserted between each element.
    ///
    pub def joinValues(sep: String, m: Map[k, v]): String with ToString[v] =
        let Map(t) = m;
        RedBlackTree.joinValues(sep, t)

    ///
    /// Returns the concatenation of the string representation of each key-value pair
    /// `k => v` in `m` according to `f` with `sep` inserted between each element.
    ///
    pub def joinWith(f: (k, v) -> String & ef, sep: String, m: Map[k, v]): String & ef =
        let Map(t) = m;
        RedBlackTree.joinWith(f, sep, t)

}
