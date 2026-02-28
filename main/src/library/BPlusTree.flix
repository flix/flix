/*
 * Copyright 2025 Casper Dalgaard Nielsen
 *                Adam Yasser Tallouzi
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
/// A mutable concurrent B+ Tree implementation with keys of type `k` and values of type `v`.
///
/// A B+ Tree is an m-ary self-balancing search tree where each node is either an internal
/// node or a leaf node. In contrast to binary search trees, a B+ Tree of arity `m` only
/// allows nodes with at most `m` children and `m-1` keys. A B+ Tree stores its keys in
/// a sorted order in its internal nodes, which are used to guide searches to leaf nodes
/// that store the keys' corresponding values.
///
/// See below for more details:
///
/// - The Ubiquitous B-Tree (1979) by Douglas Comer
///   (https://dl.acm.org/doi/10.1145/356770.356776)
///
/// A BPlusTree satisfies the following.
///
/// `root` is the root node of the tree, which upon creation of the tree is a leaf node.
/// Through insertion into the tree, a new node may be created and propagated up to become
/// the new root of the tree.
///
/// `arity` or `m` is the arity of the tree which determines the maximum number of children
/// a node may have. The arity must be at least 3.
///
/// `rootLock` is a lock on the `root` pointer, which a thread must first acquire before
/// accessing the root node. It must also be acquired before changing `root`. The node
/// `root` has a separate lock concerning the contents of the node. The lock of `root`
/// must first be acquired after successfully acquiring `rootLock`.
///
/// `rc` is the region for the tree.
///
/// `search` is only used internally within the Fixpoint module where keys are of type
/// `Vector[Int64]`. A `search` describes the order in which elements should be
/// compared. E.g. [0, 1, 2, ..., n] is the natural order. A `search` may only be used
/// when using `Vector[Int64]` as keys.
///
/// `counter` is used for keeping track of the number of inserted elements.
///
/// For all other usages `search` will be a vector of size 0.
///
pub struct BPlusTree[k, v, r] {
    mut root: BPlusTree.Node[k, v, r],
    arity:    Int32,
    rootLock: BPlusTree.Lock[r],
    rc:       Region[r],
    counter:  BPlusTree.AtomicCounter[r],
    search:   BPlusTree.Search
}

instance ForEach[BPlusTree[k, v, r]] {
    type Elm = (k, v)
    type Aef = r
    pub def forEach(f: ((k, v)) -> Unit \ ef, m: BPlusTree[k, v, r]): Unit \ ef + r = BPlusTree.forEach(k -> v -> f((k, v)), m)
}

pub mod BPlusTree {
    import java.lang.Runtime
    import java.util.concurrent.atomic.{LongAdder => JLongAdder}
    import java.util.concurrent.locks.{StampedLock => JStampedLock}

    pub type alias Search = Vector[Int32]

    ///
    /// A thread-safe counter producing `Int32`.
    ///
    /// A `LongAdder` is used as `AtomicInteger`/`AtomicLong` appeared to perform
    /// badly under high contention.
    ///
    pub enum AtomicCounter[_: Region](JLongAdder)

    ///
    /// The purpose of Lock is to provide a thin wrapper around Java's StampedLock without
    /// the IO effect. The lock is optimistic in the sense that reads are performed by
    /// reading the values and only afterwards verifying that no other thread has changed
    /// the values. Only annotating a region effect to StampedLock is safe, since it can
    /// only non-deterministically block a thread for some ( potentially infinite) amount
    /// of time. This is allowed under non-effectful behavior.
    ///
    ///
    /// One possible danger, introduced by StampedLock, is handled by a null-check in
    /// binarySearch. Consider the following example where everything is initialized to 0:
    /// Thread 1 executes
    /// {a = 2; b = 3}
    /// Thread 2 executes
    /// {c = a; d = b}
    /// The question is what can `c` and `d` be? It depends on the JVM, but can be any of:
    /// {c = 2, d = 3}, {c = 0, d = 0}, {c = 2, d = 0}, {c = 0, d = 3}
    /// The last follows from the fact that the JVM can reorder statements under some
    /// requirements. The requirements are roughly that the new 'code' should be
    /// equivalent on a single-threaded machine and there are no happens-before relations
    /// which link to other threads preventing the reordering. If either variable were
    /// volatile we might avoid this, but volatile is expensive (and not in Flix).
    ///
    /// For us, this means that {array[size] = 5; size++} can be reordered to
    /// {oldSize = size++; array[oldSize] = 5}.
    /// Another thread reading may then see the increase of size and try to access that
    /// position getting null. This has not happened in our experiments, but we wish to
    /// handle the case. The risk is in binarySearch and we therefore check whether any
    /// value read is null and if so 'aborts'.
    ///
    /// For Java's memory model see: https://docs.oracle.com/javase/specs/jls/se8/html/jls-17.html#jls-17.4
    ///
    pub enum Lock[_: Region](JStampedLock)

    ///
    /// Updates `t` with `k => f(v)` if `k => v` is in `t`.
    ///
    /// Otherwise, `t` is unchanged.
    ///
    /// Thread-safe.
    ///
    pub def adjust(f: v -> v \ ef, k: k, t: BPlusTree[k, v, r]): Unit \ ef + r with Order[k] =
        adjustWithKey(_ -> v -> f(v), k, t)

    ///
    /// Updates `t` with `k => f(k, v)` if `k => v` is in `t`.
    ///
    /// Otherwise, `t` is unchanged.
    ///
    /// Thread-safe.
    ///
    pub def adjustWithKey(f: k -> v -> v \ ef, k: k, t: BPlusTree[k, v, r]): Unit \ ef + r with Order[k] =
        let (node, stamp) = findLeaf(k, t);
        Node.adjustWithKey(f, k, stamp, t, node)

    ///
    /// Returns true if and only if `t` is a structurally correct tree.
    ///
    /// Only used for internal testing.
    ///
    pub def assertTreeInvariant(t: BPlusTree[k, v, r]): Bool \ r with Order[k] =
        let root = t->root;
        let arity = BPlusTree.arity(t);
        let nodeWorks = Node.assertNodeInvariant(root, arity, true, None, t->search);
        let rootIsUnlocked = not BPlusTree.Lock.isLocked(t->rootLock);
        let rootHasNoParent = Option.isEmpty(Node.parent(root));
        nodeWorks and rootHasNoParent and rootIsUnlocked

    ///
    /// Returns the number of mappings in `t` that satisfy the predicate function `f`.
    ///
    /// This method unsafely removes the effect `r` when spawning threads.
    ///
    /// Not thread-safe.
    ///
    @Parallel
    pub def count(f: k -> v -> Bool, t: BPlusTree[k, v, r]): Int32 \ r with Order[k] = {
        let c = AtomicCounter.mkCounter(t->rc, 0);
        parForEach(k -> v -> if (f(k, v)) AtomicCounter.increment(c) else (), t);
        AtomicCounter.get(c)
    }

    ///
    /// Alias for `foldLeftWithKey`
    ///
    pub def foldWithKey(f: b -> k -> v -> b \ ef, i: b, t: BPlusTree[k, v, r]): b \ r + ef with Order[k] =
        foldLeftWithKey(f, i, t)

    ///
    /// Returns `v` if `k => v` is in `t`.
    ///
    /// Otherwise updates `t` with a new mapping `k => d` and returns `d`.
    ///
    /// Atomic and thread-safe
    ///
    pub def getOrElsePut(k: k, d: v, t: BPlusTree[k, v, r]): v \ r with Order[k] =
        computeIfAbsent(_ -> d, k, t)

    ///
    /// Returns `true` if `t1` and `t2` contain the same keys and values, regardless of
    /// each individual tree's internal structure.
    ///
    pub def sameElements(t1: BPlusTree[k, v, r1], t2: BPlusTree[k, v, r2]): Bool \ r1 + r2 with Order[k], Eq[v] =
        if (size(t1) != size(t2))
            false
        else
            Node.sameElementsFrom(0, Node.leftMostChild(t1->root), 0, Node.leftMostChild(t2->root))

    ///
    /// Applies the function `f` to every value in `t`.
    ///
    pub def transform(f: v -> v \ ef, t: BPlusTree[k, v, r]): Unit \ r + ef with Order[k] =
        transformWithKey(_ -> v -> f(v), t)

    ///
    /// Applies the function `f` to every value in `t`.
    ///
    pub def transformWithKey(f: k -> v -> v \ ef, t: BPlusTree[k, v, r]): Unit \ r + ef with Order[k] =
        Node.transformWithKey(f, 0, Node.leftMostChild(t->root))

    ///
    /// Returns `v` if `k => v` is in `t`. Otherwise, computes `v = f()`, inserts `k => v`
    /// and returns `v`. That is, `f` is only evaluated if `k` is not in `t`.
    ///
    /// This operation is atomic.
    ///
    /// Thread-safe.
    ///
    pub def computeIfAbsent(f: Unit -> v \ ef, k: k, t: BPlusTree[k, v, r]): v \ ef + r with Order[k] =
        let (leaf, stamp) = findLeaf(k, t);
        match Node.get(k, stamp, leaf, t) {
            case Some(v) => v
            case None =>
                let lock = Node.getLock(leaf);
                let writeStamp = Lock.tryConvertToWrite(stamp, lock);
                if (not Lock.valid(writeStamp, lock)) {
                    computeIfAbsent(f, k, t)
                } else {
                    let v = f();
                    fixRoot(Node.insertIntoLeaf(k, v, leaf, writeStamp, t), t);
                    v
                }
        }

    ///
    /// Returns `true` if and only if for all `k => v` in `t`, `f(k, v) == true`.
    ///
    /// Not thread-safe.
    ///
    pub def forAll(f: k -> v -> Bool \ ef, t: BPlusTree[k, v, r]): Bool \ ef + r =
        not exists(k -> v -> not f(k, v), t)

    ///
    /// Returns `true` if and only if there exists `k => v` in `t` such that `f(k, v) == true`.
    ///
    /// Not thread-safe.
    ///
    pub def exists(f: k -> v -> Bool \ ef, t: BPlusTree[k, v, r]): Bool \ ef + r =
        Node.exists(f, 0, Node.leftMostChild(t->root))

    ///
    /// Alias for `findLeft`.
    ///
    /// Not thread-safe.
    ///
    pub def find(f: k -> v -> Bool \ ef, t: BPlusTree[k, v, r]): Option[(k, v)] \ ef + r =
        findLeft(f, t)

    ///
    /// Optionally returns the first mapping in `t` satisfying the function `f`.
    ///
    /// Not thread-safe.
    ///
    pub def findLeft(f: k -> v -> Bool \ ef, t: BPlusTree[k, v, r]): Option[(k, v)] \ ef + r =
        Node.findLeft(f, 0, Node.leftMostChild(t->root))

    ///
    /// Increments the stored size of `t`.
    ///
    /// Thread-safe.
    ///
    def incrementSize(t: BPlusTree[k, v, r]): Unit \ r = AtomicCounter.increment(t->counter)

    ///
    /// Returns `true` if and only if `t` contains the mapping `k => v`.
    ///
    /// Thread-safe.
    ///
    pub def memberOfPair(k: k, v: v, t: BPlusTree[k, v, r]): Bool \ r with Order[k], Eq[v] =
        let (leaf, stamp) = BPlusTree.findLeaf(k, t);
        let res = Node.leafMemberOfPair(k, v, t->search, leaf);
        if (Lock.valid(stamp, Node.getLock(leaf))) {
            res
        } else {
            Lock.yieldBasedOn(Node.getLock(leaf));
            memberOfPair(k, v, t)
        }

    ///
    /// Returns a `BPlusTree` with mappings `k => f(v)` for every `k => v` in `t`.
    ///
    /// Not thread-safe.
    ///
    pub def map(rc1: Region[r1], f: v1 -> v2 \ ef, t: BPlusTree[k, v1, r]): BPlusTree[k, v2, r1] \ ef + r + r1 with Order[k] =
        mapWithKey(rc1, _ -> v -> f(v), t)

    ///
    /// Returns a `BPlusTree` with mappings `k => f(k, v)` for every `k => v` in `t`.
    ///
    /// Not thread-safe.
    ///
    pub def mapWithKey(rc1: Region[r1], f: k -> v1 -> v2 \ ef, t: BPlusTree[k, v1, r]): BPlusTree[k, v2, r1] \ ef + r + r1 with Order[k] = {
        let t1 = BPlusTree.emptyWithArity(rc1, BPlusTree.arity(t));
        foreach ((k, v) <- t) {
            BPlusTree.put(k, f(k, v), t1)
        };
        t1
    }

    ///
    /// Returns an iterator over keys in `t`.
    ///
    pub def iteratorWithKeys(rc1: Region[r1], t: BPlusTree[k, v, r2]): Iterator[k, r1 + r2, r1] \ r1 + r2 = 
        Iterator.map(match (k, _) -> k, iterator(rc1, t))
    
    ///
    /// Returns an iterator over values in `t`.
    ///
    pub def iteratorValues(rc1: Region[r1], t: BPlusTree[k, v, r2]): Iterator[v, r1 + r2, r1] \ r1 + r2 = 
        Iterator.map(match (_, v) -> v, iterator(rc1, t))

    ///
    /// Returns an iterator over all key-value pairs in `t`,
    ///
    /// Not thread-safe.
    ///
    pub def iterator(rc1: Region[r1], t: BPlusTree[k, v, r]): Iterator[(k, v), r + r1, r1] \ r + r1 = {
        let nodeRef = Ref.fresh(rc1, Node.leftMostChild(t->root));
        let indexRef = Ref.fresh(rc1, 0);
        def next() = {
            let node = Ref.get(nodeRef);
            let index = Ref.get(indexRef);
            if (index < Node.size(node)) {
                Ref.put(index + 1, indexRef);
                Some(Node.getKeyAndValueAt(index, node))
            } else match Node.next(node) {
                case None => None
                case Some(nextNode) => {
                    Ref.put(nextNode, nodeRef);
                    Ref.put(0, indexRef);
                    next()
                }
            }
        };
        Iterator.iterate(rc1, next)
    }

    ///
    /// Returns `t` as an immutable map.
    ///
    /// Not thread-safe.
    ///
    pub def toMap(t: BPlusTree[k, v, r]): Map[k, v] \ r with Order[k] =
        (Map.empty(), t) ||> foldLeftWithKey(acc -> k -> v -> Map.insert(k, v, acc))

    ///
    /// Returns `t` as a set of key-value pairs.
    ///
    /// Not thread-safe.
    ///
    pub def toSet(t: BPlusTree[k, v, r]): Set[(k, v)] \ r with Order[k], Order[v] =
        (Set.empty(), t) ||> foldLeftWithKey(acc -> k -> v -> Set.insert((k, v), acc))

    ///
    /// Returns `t` as a vector of key-value pairs.
    ///
    /// Not thread-safe.
    ///
    pub def toVector(t: BPlusTree[k, v, r]): Vector[(k, v)] \ r = region rc {
        let mm = MutList.empty(rc);
        foreach ((k, v) <- t) {
            MutList.push((k, v), mm)
        };
        MutList.toVector(mm)
    }

    ///
    /// Returns a fresh empty BPlusTree tree of standard arity.
    ///
    pub def empty(rc: Region[r]): BPlusTree[k, v, r] \ r =
        emptyWithSearch(rc, unchecked_cast(null as Vector[Int32]))

    ///
    /// Returns a fresh empty BPlusTree tree with arity `m` (the maximum keys/values per node).
    /// The arity must be at least 3.
    ///
    pub def emptyWithArity(rc: Region[r], m: Int32): BPlusTree[k, v, r] \ r =
        emptyWithArityAndSearch(rc, m `Int32.max` 3, unchecked_cast(null as Vector[Int32]))

    ///
    /// Returns a fresh empty BPlusTree tree of arity `m` (the maximum keys/values per node).
    /// The arity must be at least 3.
    ///
    /// `search` decided the comparison order for the stored vectors.
    ///
    pub def emptyWithArityAndSearch(rc: Region[r], m: Int32, search: Search): BPlusTree[k, v, r] \ r =
        let node = Node.mkLeaf(rc, Array.empty(rc, m), Array.empty(rc, m), 0, None, None);
        new BPlusTree @ rc {
            root = node,
            arity = m,
            rootLock = Lock.mkLock(rc),
            rc = rc,
            search = search,
            counter = AtomicCounter.mkCounter(rc, 0)
        }

    ///
    /// Returns a fresh empty BPlusTree of standard arity.
    ///
    /// `search` decided the comparison order for the stored vectors.
    ///
    pub def emptyWithSearch(rc: Region[r], search: Search): BPlusTree[k, v, r] \ r =
        emptyWithArityAndSearch(rc, 64, search)

    ///
    /// Returns the leaf in `t` containing key `k` and leaves it locked. If no node contains
    /// `k`, the rightmost node with values strictly less than `k` is returned.
    ///
    def findLeaf(k: k, t: BPlusTree[k, v, r]): (Node[k, v, r], Int64) \ r with Order[k] =
        let (node, stamp) = getLockedRoot(t);
        Node.traverseDown(k, node, stamp, t)

    ///
    /// Updates the root of `t` if `newRoot` is `Some(...)`. Unlocks `t->rootLock` using the stamp.
    ///
    def fixRoot(newRoot: Option[(Node[k, v, r], Int64)], t: BPlusTree[k, v, r]): Unit \ r = match newRoot {
        case Some((root, rootStamp)) =>
            t->root = root;
            Lock.unlockWrite(rootStamp, t->rootLock);
            ()
        case None => ()
    }

    ///
    /// Applies `f` to a start value `i` and all values in `t` going from left to right.
    ///
    /// Not thread-safe.
    ///
    pub def foldLeft(f: b -> v -> b \ ef, i: b, t: BPlusTree[k, v, r]): b \ ef + r with Order[k] =
        List.foldLeft(f, i, valuesOf(t))

    ///
    /// Applies `f` to a start value `i` and all mappings `k => v` in `t` going from left to right.
    ///
    /// Not thread-safe.
    ///
    pub def foldLeftWithKey(f: b -> k -> v -> b \ ef, i: b, t: BPlusTree[k, v, r]): b \ ef + r with Order[k] =
        List.foldLeft(b -> match (k, v) -> f(b, k, v), i, toList(t))

    ///
    /// Applies `f` to all mappings `k => v` in `t`.
    ///
    /// Not thread-safe.
    ///
    pub def forEach(f: k -> v -> Unit \ ef, t: BPlusTree[k, v, r]): Unit \ ef + r =
        let minLeaf = Node.leftMostChild(t->root);
        Node.traverseRightUnconditional(f, 0, minLeaf)

    ///
    /// Returns the root of the `t` and a stamp on the lock of the returned node.
    ///
    def getLockedRoot(t: BPlusTree[k, v, r]): (Node[k, v, r], Int64) \ r =
        let rootPointerStamp = Lock.tryReadLock(t->rootLock);
        let rootNode = t->root;
        let nodeStamp = Lock.tryReadLock(Node.getLock(rootNode));
        if (Lock.valid(rootPointerStamp, t->rootLock)) {
            (rootNode, nodeStamp)
        } else {
            Lock.yieldBasedOn(t->rootLock);
            getLockedRoot(t)
        }

    ///
    /// Returns `Some(v)` if `k => v` is in `t`. Otherwise, returns `None`.
    ///
    /// Thread-safe.
    ///
    pub def get(k: k, t: BPlusTree[k, v, r]): Option[v] \ r with Order[k] =
        let (leaf, stamp) = BPlusTree.findLeaf(k, t);
        Node.get(k, stamp, leaf, t)

    ///
    /// Returns `Some((k, v))` if `k => v` is in `t`. Otherwise, returns `None`.
    ///
    /// Thread-safe.
    ///
    pub def getKeyAndValue(k: k, t: BPlusTree[k, v, r]): Option[(k, v)] \ r with Order[k] = {
        let (leaf, stamp) = BPlusTree.findLeaf(k, t);
        Node.getKeyAndValue(k, stamp, leaf, t)
    }

    ///
    /// Returns `v` if `k => v` is in `t`. Otherwise, returns `d`.
    ///
    /// Thread-safe.
    ///
    pub def getWithDefault(k: k, d: v, t: BPlusTree[k, v, r]): v \ r with Order[k] =
        let (leaf, stamp) = BPlusTree.findLeaf(k, t);
        Node.getWithDefault(k, stamp, d, leaf, t)


    ///
    /// Returns the set of keys in `t`.
    ///
    /// Not thread-safe.
    ///
    pub def keysOf(t: BPlusTree[k, v, r]): Set[k] \ r with Order[k] = region rc {
        let set = MutSet.empty(rc);
        forEach(k -> _ -> MutSet.add(k, set), t);
        MutSet.toSet(set)
    }

    ///
    /// Returns the list of values in `t`.
    ///
    /// Not thread-safe.
    ///
    pub def valuesOf(t: BPlusTree[k, v, r]): List[v] \ r with Order[k] = region rc {
        let list = MutList.empty(rc);
        forEach(_ -> v -> MutList.push(v, list), t);
        MutList.toList(list)
    }

    ///
    /// Returns a string containing all values in `t`, in key order,
    /// with each value formatted by its `ToString` instance and separated by `sep`.
    ///
    /// Not thread-safe.
    ///
    pub def joinValues(sep: String, t: BPlusTree[k, v, r]): String \ r with ToString[v] =
        joinWith(_ -> v -> ToString.toString(v), sep, t)

    ///
    /// Returns a string containing all keys in `t`, in key order,
    /// with each key formatted by its `ToString` instance and separated by `sep`.
    ///
    /// Not thread-safe.
    ///
    pub def joinKeys(sep: String, t: BPlusTree[k, v, r]): String \ r with ToString[k] =
        joinWith(k -> _ -> ToString.toString(k), sep, t)

    ///
    /// Applies `f` to each key-value pair in `t`, in key order, to obtain a
    /// string and returns all results joined together using `sep`.
    ///
    /// Not thread-safe.
    ///
    pub def joinWith(f: k -> v -> String \ ef, sep: String, t: BPlusTree[k, v, r]): String \ r + ef = region rc {
        let sb = StringBuilder.empty(rc);
        foreach ((k, v) <- t) {
            StringBuilder.append(f(k, v), sb);
            StringBuilder.append(sep, sb)
        };
        let str = StringBuilder.toString(sb);
        String.dropRight(String.length(sep), str)
    }

    ///
    /// Applies `f` to all mappings `k => v` in `t` in parallel.
    /// `f` must not modify `t`.
    ///
    /// This method unsafe removes the `r` effect when spawning threads.
    ///
    /// Not thread-safe.
    ///
    @Parallel
    pub def parForEach(f: k -> v -> Unit \ r, t: BPlusTree[k, v, r]): Unit \ r = region rc {
        // `threadNum` is the number of spawned threads.
        let threadNum = if (Node.computeHeight(t->root) >= 2) {
            threads()
        } else {
            Int32.min(threads(), Node.size(t->root))
        };
        if (threadNum <= 1) {
            forEach(f, t)
        } else {
            let minLeaf = Node.leftMostChild(t->root);
            unsafe IO as r {
                Vector.range(0, threadNum) |>
                Vector.forEach(i -> {
                    let _: Unit = spawn unsafe r as IO {
                        // Flix does not allow region effect in spawns. Remove the effect.
                        Node.traverseRightUnconditionalInc(f, i, threadNum, minLeaf)
                    } @ rc;
                    ()
                })
            }
        }
    }

    ///
    /// Applies `f1` to all mappings `k => v` in `t` sequentially if `t` contains strictly less than
    /// `parLimit` elements. If `t` contains `parLimit` or more elements `f2` is applied in parallel
    /// to all mappings.
    ///
    /// `f1` and `f2` must not modify `t`.
    ///
    /// This method unsafe removes the `r` effect when spawning threads in case 2.
    ///
    /// Not thread-safe.
    ///
    @Parallel
    pub def parForEachWhen(f1: k -> v -> Unit \ r, f2: k -> v -> Unit \ r, parLimit: Int32, t: BPlusTree[k, v, r]): Unit \ r =
        if (size(t) < parLimit)
            forEach(f1, t)
        else
            parForEach(f2, t)

    ///
    /// Updates the tree `t` with the mapping `k => v`. Replaces any
    /// existing mapping.
    ///
    /// Updates both the key and value of the mapping.
    ///
    /// Thread-safe.
    ///
    pub def put(k: k, v: v, t: BPlusTree[k, v, r]): Unit \ r with Order[k] =
        fixRoot(putInternal(k, v, t), t)

    ///
    /// Updates the tree `t` with the mapping `k => v`. Replaces any
    /// existing mapping, `k2 => v2`, if `decider(k, v, k2, v2) == true`.
    ///
    /// Thread-safe and atomic.
    ///
    pub def putIf(decider: k -> v -> k -> v -> Bool, k: k, v: v, t: BPlusTree[k, v, r]): Unit \ r with Order[k] =
        fixRoot(putIfInternal(decider, k, v, t), t)

    ///
    /// Internal method for `putIf`.
    ///
    /// Called when restarting.
    ///
    /// Returns the `Some(newRoot, stamp)` if the root was split where `stamp` is a write-stamp
    /// on `rootLock`.
    ///
    def putIfInternal(decider: k -> v -> k -> v -> Bool, k: k, v: v, t: BPlusTree[k, v, r]): Option[(Node[k, v, r], Int64)] \ r with Order[k] =
        let (leaf, stamp) = BPlusTree.findLeaf(k, t);
        Node.insertIntoLeafIf(decider, k, v, leaf, stamp, t)

    ///
    /// Internal method for `put`.
    ///
    /// Called when restarting.
    ///
    /// Returns the `Some(newRoot, stamp)` if the root was split where `stamp` is a write-stamp
    /// on `rootLock`.
    ///
    def putInternal(k: k, v: v, t: BPlusTree[k, v, r]): Option[(Node[k, v, r], Int64)] \ r with Order[k] =
        let (leaf, stamp) = BPlusTree.findLeaf(k, t);
        Node.insertIntoLeaf(k, v, leaf, stamp, t)

    ///
    /// Returns `true` if and only if `t` is the empty tree.
    ///
    /// Thread-safe.
    ///
    pub def isEmpty(t: BPlusTree[k, v, r]): Bool \ r =
        let (root, stamp) = BPlusTree.getLockedRoot(t);
        let res = Node.isEmpty(root);
        if (Lock.valid(stamp, Node.getLock(root))) {
            res
        } else {
            // Restart
            Lock.yieldBasedOn(Node.getLock(root));
            isEmpty(t)
        }

    ///
    /// Returns `true` if and only if `t` contains at least one mapping.
    ///
    /// Thread-safe.
    ///
    pub def nonEmpty(t: BPlusTree[k, v, r]): Bool \ r =
        not BPlusTree.isEmpty(t)

    ///
    /// Returns `true` if and only if `t` contains the key `k`.
    ///
    /// Thread-safe.
    ///
    pub def memberOf(k: k, t: BPlusTree[k, v, r]): Bool \ r with Order[k] =
        let (leaf, stamp) = BPlusTree.findLeaf(k, t);
        let res = Node.leafMemberOf(k, t->search, leaf);
        if (not Lock.valid(stamp, Node.getLock(leaf))) {
            Lock.yieldBasedOn(Node.getLock(leaf));
            memberOf(k, t)
        } else {
            res
        }

    ///
    /// Merge `src` into `dst` modyfing `dst` in a left-biased manner.
    ///
    /// That is, key collisions are resolved by taking the mapping from `src`.
    ///
    /// Not thread-safe.
    ///
    pub def merge(src: BPlusTree[k, v, r], dst: BPlusTree[k, v, r]): Unit \ r with Order[k] =
        BPlusTree.forEach(k -> v -> BPlusTree.put(k, v, dst), src)

    ///
    /// Merges `src` into `dst` modyfing `dst`. If `k => v1` is in `src` and `k => v2` is
    /// in `dst`, updates `dst` to with `k => f(v1, v2)`.
    ///
    /// Not thread-safe.
    ///
    pub def mergeWith(f: v -> v -> v \ ef, src: BPlusTree[k, v, r], dst: BPlusTree[k, v, r]): Unit \ r + ef with Order[k] =
        BPlusTree.forEach(k -> v -> BPlusTree.putWith(f, k, v, dst), src)

    ///
    /// Optionally returns `k => v` where `k` is the minimum key.
    ///
    /// Not thread-safe.
    ///
    pub def minimumKey(t: BPlusTree[k, v, r]): Option[(k, v)] \ r =
        Node.minimumKey(t->root)

    ///
    /// Returns the arity of `t`.
    ///
    pub def arity(t: BPlusTree[k, v, r]): Int32 = t->arity

    ///
    /// Updates `t` with `k => f(k, v, v1)` if `k => v1` is in `t`.
    ///
    /// Otherwise, updates `t` with `k => v`.
    ///
    /// Thread-safe.
    ///
    pub def putWith(f: v -> v -> v \ ef, k: k, v: v, t: BPlusTree[k, v, r]): Unit \ ef + r with Order[k] =
        fixRoot(putWithInternal(f, k, v, t), t)

    ///
    /// Internal method to update the mapping as described by `putWith`.
    ///
    /// Called when restarting.
    ///
    /// Thread-safe.
    ///
    def putWithInternal(f: v -> v -> v \ ef, k: k, v: v, t: BPlusTree[k, v, r]): Option[(Node[k, v, r], Int64)] \ ef + r with Order[k] =
        let (leaf, stamp) = findLeaf(k, t);
        Node.putWith(f, k, v, t, leaf, stamp)

    ///
    /// Returns the list of mappings `k => v` in `t` where `min <= k <= max` according to 
    /// the `Order` instance on `k`.
    ///
    pub def rangeQuery(min: k, max: k, t: BPlusTree[k, v, r]): List[(k, v)] \ r with Order[k] = region rc {
        let l = MutList.empty(rc);
        rangeQueryWith(k -> v -> MutList.push((k, v), l), min, max, t);
        MutList.toList(l)
    }

    ///
    /// Applies `f` in ascending order to all mappings `k => v` in `t` where
    /// `min <= k <= max`.
    ///
    /// Not thread-safe.
    ///
    pub def rangeQueryWith(f: k -> v -> Unit \ ef, min: k, max: k, t: BPlusTree[k, v, r]): Unit \ r + ef with Order[k] =
        let (root, stamp) = getLockedRoot(t);
        Node.rangeQueryWith(f, min, max, stamp, root, t)

    ///
    /// Returns the region of `t`.
    ///
    pub def rc(t: BPlusTree[k, v, r]): Region[r] = t->rc

    ///
    /// Returns the search of `t`.
    ///
    pub def search(t: BPlusTree[k, v, r]): Search = t->search

    ///
    /// Returns the size of `t`. This operation is `O(1)`.
    ///
    /// Not atomic.
    ///
    /// Thread-safe.
    ///
    pub def size(t: BPlusTree[k, v, r]): Int32 \ r = AtomicCounter.get(t->counter)

    ///
    /// Returns the number of threads to use for parallel evaluation.
    ///
    /// # SAFETY:
    /// This accesses the runtime environment, which is an effect.
    /// It is assumed that this function is only used in contexts
    /// where this effect is not observable outside of the RedBlackTree module.
    ///
    def threads(): Int32 = {
        // Note: We use a multiple of the number of physical cores for better performance.
        let multiplier = 4;
        multiplier * Runtime.getRuntime().availableProcessors()
    }

    ///
    /// Returns a list of the key-value pairs in `t`. Elements are ordered from smallest
    /// (left) to largest (right).
    ///
    /// Not thread-safe.
    ///
    pub def toList(t: BPlusTree[k, v, r]): List[(k, v)] \ r = region rc {
        let list = MutList.empty(rc);
        forEach(k -> v -> MutList.push((k, v), list), t);
        MutList.toList(list)
    }

    ///
    /// Returns a string representation of `t`.
    ///
    /// Not thread-safe.
    ///
    pub def toString(t: BPlusTree[k, v, r]): String \ r with ToString[k], ToString[v] =
        Node.toString(0, t->rc, t->root)

    ///
    /// Write lock `t->rootLock` and return the stamp.
    ///
    def writeLockRootPointer(t: BPlusTree[k, v, r]): Int64 \ r =
        Lock.writeLock(t->rootLock)

    ///
    /// Write lock `t->rootLock` and return the stamp.
    ///
    def unlockRootPointer(stamp: Int64, t: BPlusTree[k, v, r]): Unit \ r =
        Lock.unlockWrite(stamp, t->rootLock)

    ///
    /// The Node struct defines the tree, having either children or values depending on
    /// whether the node is internal or a leaf, respectively. The following invariants
    /// hold for Node: `parent` is `None` for the root and `Some(p)` for all other nodes
    /// where `p` is the parent node. `next` is `None` for internal nodes and the
    /// rightmost leaf node and `Some(p)` for all leaf nodes where `p` is the node
    /// directly to the right. `isLeaf` is true if the node is a leaf and false
    /// otherwise.
    ///
    /// If `isLeaf == true`
    ///
    ///     `children` is of size 0
    ///
    ///     `keys` and `values` have the same size (which is the `arity` of the
    ///     tree)
    ///
    ///     `keys[i]` should store the key for the value stored at `values[i]`
    ///
    ///     `size` is the number of keys/values currently stored in the leaf
    ///
    /// If `isLeaf == false`
    ///
    ///      `values` is of size 0
    ///
    ///      `children` is of size `arity` and `keys` have size `arity - 1`
    ///
    ///      `keys[i]` stores the smallest key in the subtree `children[i+1]`
    ///
    ///      `size` is the number of children currently stored in the leaf
    ///
    /// Lock invariants:
    ///
    ///     `lock` is the lock associated with the node. When it is held, no other thread may
    ///     modify the node's contents. The only exception to the previous rule is that the
    ///     `parent` pointer is owned by the `lock` on the parent For the root node with
    ///     parent None the parent pointer is owned by the `rootLock` on the tree. When going
    ///     up the tree locks are forcefully acquired. When going down the tree if a lock
    ///     cannot be acquired immediately all current locks will be released and the attempt
    ///     restarted after the wanted lock has been released by the holder.
    ///
    pub struct Node[k, v, r] {
        keys:       Array[k, r],
        children:   Array[Node[k, v, r], r],
        values:     Array[v, r],
        lock:       Lock[r],
        mut size:   Int32,
        mut parent: Option[Node[k, v, r]],
        mut next:   Option[Node[k, v, r]],
        isLeaf:     Bool
    }

}
