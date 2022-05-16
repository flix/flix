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

pub enum DelayMap[k, v] {
    case DMap(RedBlackTree[k, Lazy[v]])
}

namespace DelayMap {

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
    def useParallelEvaluation(m: DelayMap[k, v]): Bool =
        let DMap(t) = m;
        let minSize = 2 ** RedBlackTree.blackHeight(t);
        minSize >= 1024

    ///
    /// Returns the empty map.
    ///
    @Experimental
    pub def empty(): DelayMap[k, v] =
        DMap(RedBlackTree.empty())

    ///
    /// Returns the singleton map where key `k` is mapped to value `v`.
    ///
    @Experimental
    pub def singleton(k: k, v: v): DelayMap[k, v] with Order[k] =
        insert(k, v, empty())

    ///
    /// Returns the number of keys in `m`.
    ///
    @Experimental
    pub def size(m: DelayMap[k, v]): Int32 =
        let DMap(t) = m;
        RedBlackTree.size(t)

    ///
    /// Returns `true` if and only if `m` is the empty map, i.e. `Map(Nil)`.
    ///
    @Experimental
    pub def isEmpty(m: DelayMap[k, v]): Bool =
        let DMap(t) = m;
        RedBlackTree.isEmpty(t)

    ///
    /// Returns `m` with `k => v`.
    ///
    @Experimental
    pub def insert(k: k, v: v, m: DelayMap[k, v]): DelayMap[k, v] with Order[k] =
        let DMap(t) = m;
        DMap(RedBlackTree.insert(k, lazy v, t))

    ///
    /// Returns `Some(v)` if `k => v` is in `m`.
    ///
    /// Otherwise returns `None`.
    ///
    @Experimental
    pub def get(k: k, m: DelayMap[k, v]): Option[v] with Order[k] =
        let DMap(t) = m;
        match RedBlackTree.get(k, t) {
            case None    => None
            case Some(x) => Some(force x)
        }

    ///
    /// Returns `v` if `k => v` is in `m`.
    ///
    /// Otherwise, returns `d`.
    ///
    @Experimental
    pub def getWithDefault(k: k, d: v, m: DelayMap[k, v]): v with Order[k] =
        Option.getWithDefault(d, get(k, m))

    ///
    /// Returns the number of mappings `k => v` in `m` that satisfy the predicate `f`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @Experimental @ParallelWhenPure
    pub def count(f: (k, v) -> Bool & ef, m: DelayMap[k, v]): Int32 & ef =
        let f1 = match (k, v) -> f(k, v);
        def c() = foldLeftWithKey((b, k, v) -> if (f(k, v)) b + 1 else b, 0, m);
        reifyEff(f1) {
            case Pure(g) =>
                if (useParallelEvaluation(m))
                    let h = (k, v) -> g((k, force v));
                    let DMap(t) = m;
                    RedBlackTree.parCount(threads() - 1, h, t) as & Pure
                else
                    c()
            case _  => c()
        }

    ///
    /// Returns `true` if and only if `m` contains the key `k`.
    ///
    @Experimental
    pub def memberOf(k: k, m: DelayMap[k, v]): Bool with Order[k] =
        let DMap(t) = m;
        RedBlackTree.memberOf(k, t)

    ///
    /// Optionally finds `k => v` where `k` is the smallest key according to the `Order` instance of `k`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    @Experimental
    pub def minimumKey(m: DelayMap[k, v]): Option[(k, v)] =
        let DMap(t) = m;
        match RedBlackTree.minimumKey(t) {
            case None       => None
            case Some(k, v) => Some(k, force v)
        }

    ///
    /// Optionally finds `k => v` where `k` is the smallest key according to the given comparator `cmp`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `cmp`.
    ///
    @Experimental @ParallelWhenPure
    pub def minimumKeyBy(cmp: (k, k) -> Comparison & ef, m: DelayMap[k, v]): Option[(k, v)] & ef =
        def min() = reduceLeftWithKey((kl, vl, kr, vr) -> if (cmp(kl, kr) == LessThan) (kl, vl) else (kr, vr), m);
        let f = r -> cmp(r.kl, r.kr);
        reifyEff(f) {
            case Pure(g) =>
                if (useParallelEvaluation(m))
                    let h = (kl, _, kr, _) -> g({kl = kl, kr = kr});
                    let DMap(t) = m;
                    let res = RedBlackTree.parMinimumBy(threads() - 1, h, t) as & Pure;
                    match res {
                        case None       => None
                        case Some(k, v) => Some(k, force v)
                    }
                else
                    min()
            case _ => min()
        }
    ///
    /// Optionally finds `k => v` where `v` is the smallest value.
    ///
    /// Returns `None` if `m` is empty.
    ///
    @Experimental @Parallel
    pub def minimumValue(m: DelayMap[k, v]): Option[(k, v)] with Order[v] =
        minimumValueBy((x, y) -> x <=> y, m)

    ///
    /// Optionally finds `k => v` where `k` is the smallest value according to the given comparator `cmp`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `cmp`.
    ///
    @Experimental @ParallelWhenPure
    pub def minimumValueBy(cmp: (v, v) -> Comparison & ef, m: DelayMap[k, v]): Option[(k, v)] & ef =
        def min() = reduceLeftWithKey((kl, vl, kr, vr) -> if (cmp(vl, vr) == LessThan) (kl, vl) else (kr, vr), m);
        let f = r -> cmp(r.vl, r.vr);
        reifyEff(f) {
            case Pure(g) =>
                if (useParallelEvaluation(m))
                    let h = (_, vl, _, vr) -> g({vl = force vl, vr = force vr});
                    let DMap(t) = m;
                    let res = RedBlackTree.parMinimumBy(threads() - 1, h, t) as & Pure;
                    match res {
                        case None       => None
                        case Some(k, v) => Some(k, force v)
                    }
                else
                    min()
            case _ => min()
        }

    ///
    /// Optionally finds `k => v` where `k` is the largest key according to the `Order` instance of `k`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    @Experimental
    pub def maximumKey(m: DelayMap[k, v]): Option[(k, v)] =
        let DMap(t) = m;
        match RedBlackTree.maximumKey(t) {
            case None       => None
            case Some(k, v) => Some(k, force v)
        }

    ///
    /// Optionally finds `k => v` where `k` is the largest key according to the given comparator `cmp`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `cmp`.
    ///
    @Experimental @ParallelWhenPure
    pub def maximumKeyBy(cmp: (k, k) -> Comparison & ef, m: DelayMap[k, v]): Option[(k, v)] & ef =
        def max() = reduceLeftWithKey((kl, vl, kr, vr) -> if (cmp(kl, kr) == GreaterThan) (kl, vl) else (kr, vr), m);
        let f = r -> cmp(r.kl, r.kr);
        reifyEff(f) {
            case Pure(g) =>
                if (useParallelEvaluation(m))
                    let h = (kl, _, kr, _) -> g({kl = kl, kr = kr});
                    let DMap(t) = m;
                    let res = RedBlackTree.parMaximumBy(threads() - 1, h, t) as & Pure;
                    match res {
                        case None       => None
                        case Some(k, v) => Some(k, force v)
                    }
                else
                    max()
            case _ => max()
        }

    ///
    /// Optionally finds `k => v` where `v` is the largest value.
    ///
    /// Returns `None` if `m` is empty.
    ///
    @Experimental @Parallel
    pub def maximumValue(m: DelayMap[k, v]): Option[(k, v)] with Order[v] =
        maximumValueBy((x, y) -> x <=> y, m)

    ///
    /// Optionally finds `k => v` where `k` is the largest value according to the given comparator `cmp`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `cmp`.
    ///
    @Experimental @ParallelWhenPure
    pub def maximumValueBy(cmp: (v, v) -> Comparison & ef, m: DelayMap[k, v]): Option[(k, v)] & ef =
        def max() = reduceLeftWithKey((kl, vl, kr, vr) -> if (cmp(vl, vr) == GreaterThan) (kl, vl) else (kr, vr), m);
        let f = r -> cmp(r.vl, r.vr);
        reifyEff(f) {
            case Pure(g) =>
                if (useParallelEvaluation(m))
                    let h = (_, vl, _, vr) -> g({vl = force vl, vr = force vr});
                    let DMap(t) = m;
                    let res = RedBlackTree.parMaximumBy(threads() - 1, h, t) as & Pure;
                    match res {
                        case None       => None
                        case Some(k, v) => Some(k, force v)
                    }
                else
                    max()
            case _ => max()
        }

    ///
    /// Returns the keys of `m`.
    ///
    @Experimental
    pub def keysOf(m: DelayMap[k, v]): Set[k] with Order[k] =
        foldLeftWithKey((acc, k, _) -> Set.insert(k, acc), Set.empty(), m)

    ///
    /// Returns the values of `m`.
    ///
    @Experimental
    pub def valuesOf(m: DelayMap[k, v]): List[v] =
        foldRight((v, acc) -> v :: acc, Nil, m)

    ///
    /// Removes the mapping `k` from the map `m`.
    ///
    @Experimental
    pub def remove(k: k, m: DelayMap[k, v]): DelayMap[k, v] with Order[k] =
        let DMap(t) = m;
        DMap(RedBlackTree.remove(k, t))

    ///
    /// Updates `m` with `k => f(v, v1)` if `k => v1` is in `m`.
    ///
    /// Otherwise, updates `m` with `k => v`.
    ///
    @Experimental @LazyWhenPure
    pub def insertWith(f: (v, v) -> v & ef, k: k, v: v, m: DelayMap[k, v]): DelayMap[k, v] & ef with Order[k] =
        insertWithKey((_, v1, v2) -> f(v1, v2), k, v, m)

    ///
    /// Updates `m` with `k => f(k, v, v1)` if `k => v1` is in `m`.
    ///
    /// Otherwise, updates `m` with `k => v`.
    ///
    @Experimental @LazyWhenPure
    pub def insertWithKey(f: (k, v, v) -> v & ef, k: k, v: v, m: DelayMap[k, v]): DelayMap[k, v] & ef with Order[k] =
        let f1 = (args) -> f(args.k, args.v1, args.v2);
        reifyEff(f1) {
            case Pure(g) => insertWithKeyL((k1, v1, v2) -> g({k = k1, v1 = v1, v2 = v2}), k, v, m)
            case _       => insertWithKeyE(f, k, v, m)
        }

    ///
    /// Helper function for `insertWithKey`. Applies `f` lazily.
    ///
    @Lazy
    def insertWithKeyL(f: (k, v, v) -> v, k: k, v: v, m: DelayMap[k, v]): DelayMap[k, v] with Order[k] =
        let DMap(t) = m;
        let f1 = (k1, v1, v2) -> lazy f(k1, force v1, force v2);
        DMap(RedBlackTree.insertWith(f1, k, lazy v, t))

    ///
    /// Helper function for `insertWithKey`. Applies `f` eagerly.
    ///
    def insertWithKeyE(f: (k, v, v) -> v & ef, k: k, v: v, m: DelayMap[k, v]): DelayMap[k, v] & ef with Order[k] =
        let DMap(t) = m;
        let f1 = (k1, v1, v2) -> {
                let x = f(k1, force v1, force v2);
                lazy x
            };
        DMap(RedBlackTree.insertWith(f1, k, lazy v, t))

    ///
    /// Returns a map with mappings `k => f(v)` for every `k => v` in `m`.
    ///
    /// Purity reflective:
    ///  - Runs in parallel when given a pure function `f`.
    ///  - Applies `f` lazily if `f` is pure.
    ///
    @Experimental @ParallelWhenPure @LazyWhenPure
    pub def map(f: v1 -> v2 & ef, m: DelayMap[k, v1]): DelayMap[k, v2] & ef with Order[k] =
        mapWithKey((_, v) -> f(v), m)

    ///
    /// Returns a map with mappings `k => f(k, v)` for every `k => v` in `m`.
    ///
    /// Purity reflective:
    ///  - Runs in parallel when given a pure function `f`.
    ///  - Applies `f` lazily if `f` is pure.
    ///
    @Experimental @ParallelWhenPure @LazyWhenPure
    pub def mapWithKey(f: (k, v1) -> v2 & ef, m: DelayMap[k, v1]): DelayMap[k, v2] & ef =
        let f1 = (args) -> f(fst(args), snd(args));
        reifyEff(f1) {
            case Pure(g) => mapWithKeyL((k, v) -> g((k, v)), m)
            case _       => mapWithKeyE(f, m)
        }

    ///
    /// Helper function for `mapWithKey`. Applies `f` lazily.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure @Lazy
    def mapWithKeyL(f: (k, v1) -> v2, m: DelayMap[k, v1]): DelayMap[k, v2] =
        let DMap(t) = m;
        let g = (k, v) -> lazy f(k, force v);
        DMap(RedBlackTree.mapWithKey(g, t))

    ///
    /// Helper function for `mapWithKey`. Applies `f` eagerly.
    ///
    def mapWithKeyE(f: (k, v1) -> v2 & ef, m: DelayMap[k, v1]): DelayMap[k, v2] & ef =
        let g = (k, v) -> {
                let x1 = f(k, force v);
                lazy x1
            };
        let _ = parallelForce(m);
        let DMap(t) = m;
        DMap(RedBlackTree.mapWithKey(g, t))

    ///
    /// Forces `m` in parallel if it is big, otherwise returns `m`.
    ///
    @Parallel
    def parallelForce(m: DelayMap[k, v]): Unit =
        if (useParallelEvaluation(m))
            forceAll(m)
        else
            ()

    ///
    /// Forces **all values** in `m`.
    ///
    @Experimental @Parallel
    pub def forceAll(m: DelayMap[k, v]): Unit =
        use RedBlackTree.RedBlackTree.Node;
        def seqLoop(tt) = match tt {
            case Node(_, a, _, v, b) =>
                let _ = seqLoop(a);
                let _ = force v;
                seqLoop(b)
            case _ => ()
        };
        def parLoop(n, tt) = {
            if (n <= 1)
                seqLoop(tt)
            else
                match tt {
                    case Node(_, a, _, v, b) =>
                        let chanL = chan Unit 1;
                        let chanR = chan Unit 1;
                        spawn (chanL <- parLoop((n - 2) / 2, a)); // We divide the rest of the threads as follows:
                        spawn (chanR <- parLoop((n - 2) / 2, b)); // We spawn two new threads leaving us with n - 2
                        let _ = force v;                          // that we distribute over the two spanned threads.
                        let _ = <- chanL;
                        <- chanR
                    case _ => ()
                }
        };
        let DMap(t) = m;
        if (useParallelEvaluation(m))
            parLoop(threads() - 1, t) as & Pure
        else
            seqLoop(t)

    ///
    /// Returns a map of all mappings `k => v` in `m` where `v` satisfies the predicate `f`.
    ///
    @Experimental
    pub def filter(f: v -> Bool & ef, m: DelayMap[k, v]): DelayMap[k, v] & ef with Order[k] =
        filterWithKey((_, v) -> f(v), m)

    ///
    /// Returns a map of all mappings `k => v` in `m` where `(k, v)` satisfies the predicate `f`.
    ///
    @Experimental
    pub def filterWithKey(f: (k, v) -> Bool & ef, m: DelayMap[k, v]): DelayMap[k, v] & ef with Order[k] =
        foldLeftWithKey((acc, k, v) -> if (f(k, v)) insert(k, v, acc) else acc, empty(), m)

    ///
    /// Returns the left-biased union of `m1` and `m2`.
    ///
    /// That is, key collisions are resolved by taking the mapping from `m1`.
    ///
    @Experimental @Lazy
    pub def union(m1: DelayMap[k, v], m2: DelayMap[k, v]): DelayMap[k, v] with Order[k] =
        unionWithKey((_, v1, _) -> v1, m1, m2)

    ///
    /// Returns the union of `m1` and `m2` where key collisions are resolved with the merge function `f`.
    ///
    /// Purity reflective: Applies `f` lazily if `f` is pure.
    ///
    @Experimental @LazyWhenPure
    pub def unionWith(f: (v, v) -> v & ef, m1: DelayMap[k, v], m2: DelayMap[k, v]): DelayMap[k, v] & ef with Order[k] =
        unionWithKey((_, v1, v2) -> f(v1, v2), m1, m2)

    ///
    /// Returns the union of `m1` and `m2` where key collisions are resolved with the merge function `f`, taking both the key and values.
    ///
    /// Purity reflective: Applies `f` lazily if `f` is pure.
    ///
    @Experimental @LazyWhenPure
    pub def unionWithKey(f: (k, v, v) -> v & ef, m1: DelayMap[k, v], m2: DelayMap[k, v]): DelayMap[k, v] & ef with Order[k] =
        let f1 = (args) -> f(args.k, args.v1, args.v2);
        reifyEff(f1) {
            case Pure(g) => unionWithKeyL((k, v1, v2) -> g({k = k, v1 = v1, v2 = v2}), m1, m2)
            case _       => unionWithKeyE(f, m1, m2)
        }

    ///
    /// Helper function for `unionWithKey`. Applies `f` lazily.
    ///
    @Lazy
    def unionWithKeyL(f: (k, v, v) -> v, m1: DelayMap[k, v], m2: DelayMap[k, v]): DelayMap[k, v] with Order[k] =
        use RedBlackTree.{blackHeight, foldRight, insertWith};
        let DMap(xs) = m1;
        let DMap(ys) = m2;
        let f1 = (k, v1, v2) -> lazy (f(k, force v1, force v2));
        if (blackHeight(xs) < blackHeight(ys))
            DMap(foldRight((k, v, acc) -> insertWith(f1, k, v, acc), ys, xs))
        else
            DMap(foldRight((k, v, acc) -> insertWith((_, v1, v2) -> f1(k, v2, v1), k, v, acc), xs, ys))

    ///
    /// Helper function for `unionWithKey`. Applies `f` eagerly.
    ///
    def unionWithKeyE(f: (k, v, v) -> v & ef, m1: DelayMap[k, v], m2: DelayMap[k, v]): DelayMap[k, v] & ef with Order[k] =
        use RedBlackTree.{blackHeight, foldRight, insertWith};
        let DMap(xs) = m1;
        let DMap(ys) = m2;
        let f1 = (k, v1, v2) -> {
                let x = f(k, force v1, force v2);
                lazy x
            };
        if (blackHeight(xs) < blackHeight(ys))
            let _ = parallelForce(m1);
            DMap(foldRight((k, v, acc) -> insertWith(f1, k, v, acc), ys, xs))
        else
            let _ = parallelForce(m2);
            DMap(foldRight((k, v, acc) -> insertWith((_, v1, v2) -> f1(k, v2, v1), k, v, acc), xs, ys))

    ///
    /// Returns the map `m` as a list of key-value pairs.
    ///
    @Experimental
    pub def toList(m: DelayMap[k, v]): List[(k, v)] =
        foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil, m)

    ///
    /// Applies `f` to a start value `s` and all values in `m` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, v1), v2)..., vn)`.
    ///
    @Experimental
    pub def foldLeft(f: (b, v) -> b & ef, s: b, m: DelayMap[k, v]): b & ef =
        foldLeftWithKey((acc, _, v) -> f(acc, v), s, m)

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `m` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, k1, v1), k2, v2)..., vn)`.
    ///
    @Experimental
    pub def foldLeftWithKey(f: (b, k, v) -> b & ef, s: b, m: DelayMap[k, v]): b & ef =
        let _ = parallelForce(m);
        let DMap(t) = m;
        let f1 = (b, k, v) -> f(b, k, force v);
        RedBlackTree.foldLeft(f1, s, t)

    ///
    /// Applies `f` to a start value `s` and all values in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(v1, ...f(vn-1, f(vn, s)))`.
    ///
    @Experimental
    pub def foldRight(f: (v, b) -> b & ef, s: b, m: DelayMap[k, v]): b & ef =
        foldRightWithKey((_, v, acc) -> f(v, acc), s, m)

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(k1, v1, ...f(kn-1, vn-1, f(kn, vn, s)))`.
    ///
    @Experimental
    pub def foldRightWithKey(f: (k, v, b) -> b & ef, s: b, m: DelayMap[k, v]): b & ef =
        let _ = parallelForce(m);
        let DMap(t) = m;
        let f1 = (k1, v1, b1) -> f(k1, force v1, b1);
        RedBlackTree.foldRight(f1, s, t)

    ///
    /// Applies `f` to a start value `z` and all values in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(v1, ...f(vn-1, f(vn, z)))`.
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    @Experimental
    pub def foldRightWithCont(f: (v, Unit -> b & ef) -> b & ef, z: b, m: DelayMap[k, v]): b & ef =
        foldRightWithKeyCont((_, v, c) -> f(v, c), z, m)

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(k1, v1, ...f(kn-1, vn-1, f(kn, vn, s)))`.
    /// A `foldRightWithKeyCont` allows early termination by not calling the continuation.
    ///
    @Experimental
    pub def foldRightWithKeyCont(f: (k, v, Unit -> b & ef) -> b & ef, s: b, m: DelayMap[k, v]): b & ef =
        let _ = parallelForce(m);
        let DMap(t) = m;
        let f1 = (k1, v1, c) -> f(k1, force v1, c);
        RedBlackTree.foldRightWithCont(f1, s, t)

    ///
    /// Applies `f` to all values in `m` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(v1, v2), v3)..., vn))`
    ///
    /// Returns `None` if `m` is the empty map.
    ///
    @Experimental
    pub def reduceLeft(f: (v, v) -> v & ef, m: DelayMap[k, v]): Option[v] & ef =
        reduceLeftWithKey((k, v1, _, v2) -> (k, f(v1, v2)), m) |> Option.map(snd)

    ///
    /// Applies `f` to all mappings in `m` going from left to right until a single mapping `(k, v)` is obtained.  Returns `Some((k, v))`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(k1, v1, k2, v2), k3, v3)..., kn, vn))`
    ///
    /// Returns `None` if `m` is the empty map.
    ///
    @Experimental
    pub def reduceLeftWithKey(f: (k, v, k, v) -> (k, v) & ef, m: DelayMap[k, v]): Option[(k, v)] & ef =
        let _ = parallelForce(m);
        let DMap(t) = m;
        let f1 = (k1, v1, k2, v2) -> {
            let (k, v) = f(k1, force v1, k2, force v2);
            (k, lazy v)
        };
        match RedBlackTree.reduceLeft(f1, t) {
            case Some((k, v)) => Some((k, force v))
            case None         => None
        }

    ///
    /// Applies `f` to all values in `m` going from right to left until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(v1, ...f(vn-2, f(vn-1, vn))...))`
    ///
    /// Returns `None` if `m` is the empty DelayMap.
    ///
    @Experimental
    pub def reduceRight(f: (v, v) -> v & ef, m: DelayMap[k, v]): Option[v] & ef =
        reduceRightWithKey((k, v1, _, v2) -> (k, f(v1, v2)), m) |> Option.map(snd)

    ///
    /// Applies `f` to all mappings in `m` going from right to left until a single mapping `(k, v)` is obtained.  Returns `Some((k, v))`.
    ///
    /// That is, the result is of the form: `Some(f(k1, v1, ...f(kn-2, vn-2, f(kn-1, vn-1, kn, vn))...))`
    ///
    /// Returns `None` if `m` is the empty DelayMap.
    ///
    @Experimental
    pub def reduceRightWithKey(f: (k, v, k, v) -> (k, v) & ef, m: DelayMap[k, v]): Option[(k, v)] & ef =
        let _ = parallelForce(m);
        let DMap(t) = m;
        let f1 = (k1, v1, k2, v2) -> {
            let (k, v) = f(k1, force v1, k2, force v2);
            (k, lazy v)
        };
        match RedBlackTree.reduceRight(f1, t) {
            case Some((k, v)) => Some((k, force v))
            case None         => None
        }

    ///
    /// Updates `m` with `k => f(v)` if `k => v` is in `m`. Otherwise, returns `m`.
    ///
    /// Purity reflective: Applies `f` lazily if `f` is pure.
    ///
    @Experimental @LazyWhenPure
    pub def adjust(f: v -> v & ef, k: k, m: DelayMap[k, v]): DelayMap[k, v] & ef with Order[k] =
        adjustWithKey((_, v1) -> f(v1), k, m)

    ///
    /// Updates `m` with `k => f(k, v)` if `k => v` is in `m`. Otherwise, returns `m`.
    ///
    /// Purity reflective: Applies `f` lazily if `f` is pure.
    ///
    @Experimental @LazyWhenPure
    pub def adjustWithKey(f: (k, v) -> v & ef, k: k, m: DelayMap[k, v]): DelayMap[k, v] & ef with Order[k] =
        updateWithKey((k1, v) -> Some(f(k1, v)), k, m)

    ///
    /// Updates `m` with `k => v1` if `k => v` is in `m` and `f(v) = Some(v1)`. Otherwise, returns `m`.
    ///
    /// Purity reflective: Applies `f` lazily if `f` is pure.
    ///
    @Experimental @LazyWhenPure
    pub def update(f: v -> Option[v] & ef, k: k, m: DelayMap[k, v]): DelayMap[k, v] & ef with Order[k] =
        updateWithKey((_, v1) -> f(v1), k, m)

    ///
    /// Updates `m` with `k => v1` if `k => v` is in `m` and `f(k, v) = Some(v1)`. Otherwise, returns `m`.
    ///
    /// Purity reflective: Applies `f` lazily if `f` is pure.
    ///
    @Experimental @LazyWhenPure
    pub def updateWithKey(f: (k, v) -> Option[v] & ef, k: k, m: DelayMap[k, v]): DelayMap[k, v] & ef with Order[k] =
        let f1 = (t) -> f(fst(t), snd(t));
        reifyEff(f1) {
            case Pure(g) => updateWithKeyL((k1, v) -> g((k1, v)), k, m)
            case _       => updateWithKeyE(f, k, m)
        }

    ///
    /// Helper function for `updateWithKey`. Does not force the `v`.
    ///
    @Lazy
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

    ///
    /// Returns `m` as a Map, i.e. every value is forced.
    ///
    @Experimental @Parallel
    pub def toMap(m: DelayMap[k, v]): Map[k, v] =
        let _ = parallelForce(m);
        let DMap(t) = m;
        Map(RedBlackTree.mapWithKey((_, v) -> force v, t))

    ///
    /// Returns `m` as a mutable set.
    ///
    @Experimental @Parallel
    pub def toMutMap(m: DelayMap[k, v]): MutMap[k, v, Impure] & Impure =
        MutMap(ref toMap(m)) as MutMap[k, v, Impure]

    ///
    /// Returns `m` as a mutable set.
    ///
    @Experimental @Parallel
    pub def toMutMapRegion(m: DelayMap[k, v], r: Region[r]): MutMap[k, v, r] \ Write(r) =
        MutMap(ref toMap(m) @ r)

    ///
    /// Returns the map `m` as a set of key-value pairs.
    ///
    @Experimental
    pub def toSet(m: DelayMap[k, v]): Set[(k, v)] with Order[k], Order[v] =
        foldLeftWithKey((acc, k, v) -> Set.insert((k, v), acc), Set.empty(), m)

    ///
    /// Returns the map `m` as a `DelayList` of key-value pairs.
    ///
    @Experimental
    pub def toDelayList(m: DelayMap[k, v]): DelayList[(k, v)] =
        foldRightWithKey((k, v, acc) -> ECons((k, v), acc), ENil, m)

    ///
    /// Returns an iterator over all key-value pairs in `m`.
    ///
    @Experimental
    pub def toIterator(m: DelayMap[a, b]): Iterator[(a, b)] & Impure with Order[a] =
        let keys = keysOf(m) |> Set.toList;
        let cursor = ref keys;
        let done = () -> match deref cursor {
            case Nil => true
            case _   => false
        };
        let next = () -> match deref cursor {
            case Nil     => bug!("Empty Iterator!")
            case x :: xs =>
                cursor := xs;
                match DelayMap.get(x, m) {
                    case None    => bug!("Empty Iterator!")
                    case Some(v) => (x, v)
                }
        };
        Iterator(done, next)

    ///
    /// Applies `f` to every element of `xs`.
    ///
    @Experimental
    pub def foreach(f: (k, v) -> Unit & ef, m: DelayMap[k, v]): Unit & ef =
        let _ = parallelForce(m);
        let DMap(t) = m;
        let f1 = (k, v) -> f(k, force v);
        RedBlackTree.foreach(f1 as & Impure, t) as & ef

    ///
    /// Returns the sum of all values in `m`.
    ///
    @Experimental @Parallel
    pub def sumKeys(m: DelayMap[Int32, v]): Int32 =
        sumWith((k, _) -> k, m)

    ///
    /// Returns the sum of all values in `m`.
    ///
    @Experimental @Parallel
    pub def sumValues(m: DelayMap[k, Int32]): Int32 =
        sumWith((_, v) -> v, m)

    ///
    /// Returns the sum of all key-value pairs `k => v` in `m`
    /// according to the function `f`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @Experimental @ParallelWhenPure
    pub def sumWith(f: (k, v) -> Int32 & ef, m: DelayMap[k, v]): Int32 & ef =
        let f1 = match (k, v) -> f(k, v);
        let DMap(t) = m;
        def sw() = {
            let _ = parallelForce(m);
            RedBlackTree.sumWith((k, v) -> f(k, force v), t)
        };
        reifyEff(f1) {
            case Pure(g) =>
                if (useParallelEvaluation(m))
                    let h = (k, v) -> g((k, force v));
                    RedBlackTree.parSumWith(threads() - 1, h, t) as & Pure
                else
                    sw()
            case _ => sw()
        }

    ///
    /// Returns the product of all keys in the DelayMap `m`.
    ///
    @Experimental @Parallel
    pub def productKeys(m: DelayMap[Int32, v]): Int32 =
        productWith((k, _) -> k, m)

    ///
    /// Returns the product of all values in the DelayMap `m`.
    ///
    @Experimental @Parallel
    pub def productValues(m: DelayMap[k, Int32]): Int32 =
        productWith((_, v) -> v, m)

    ///
    /// Returns the product of all key-value pairs `k => v` in the DelayMap `m`
    /// according to the function `f`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @Experimental @ParallelWhenPure
    pub def productWith(f: (k, v) -> Int32 & ef, m: DelayMap[k, v]): Int32 & ef =
        let f1 = match (k, v) -> f(k, v);
        let DMap(t) = m;
        def pw() = {
            let _ = parallelForce(m);
            RedBlackTree.productWith((k, v) -> f(k, force v), t)
        };
        reifyEff(f1) {
            case Pure(g) =>
                if (useParallelEvaluation(m))
                    let h = (k, v) -> g((k, force v));
                    RedBlackTree.parProductWith(threads() - 1, h, t) as & Pure
                else
                    pw()
            case _ => pw()
        }

    ///
    /// Returns the concatenation of the string representation of each key `k`
    /// in `m` with `sep` inserted between each element.
    ///
    @Experimental
    pub def joinKeys(sep: String, m: DelayMap[k, v]): String with ToString[k] =
        let DMap(t) = m;
        RedBlackTree.joinKeys(sep, t)

    ///
    /// Returns the concatenation of the string representation of each value `v`
    /// in `m` with `sep` inserted between each element.
    ///
    @Experimental
    pub def joinValues(sep: String, m: DelayMap[k, v]): String with ToString[v] =
        joinWith((_, v) -> ToString.toString(v), sep, m)

    ///
    /// Returns the concatenation of the string representation of each key-value pair
    /// `k => v` in `m` according to `f` with `sep` inserted between each element.
    ///
    @Experimental
    pub def joinWith(f: (k, v) -> String & ef, sep: String, m: DelayMap[k, v]): String & ef =
        let _ = parallelForce(m);
        let DMap(t) = m;
        RedBlackTree.joinWith((k, v) -> f(k, force v), sep, t)

}
