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

mod BPlusTree {
    import java.lang.Runtime
    import java.util.concurrent.atomic.{LongAdder => JLongAdder}
    import java.util.concurrent.locks.{StampedLock => JStampedLock}

    @Internal
    pub type alias Search = Vector[Int32]

    ///
    /// A thread-safe counter producing `Int32`.
    ///
    /// A `LongAdder` is used as `AtomicInteger`/`AtomicLong` appeared to perform
    /// badly under high contention.
    ///
    @Internal
    pub enum AtomicCounter[_: Region](JLongAdder)

    mod AtomicCounter {
        import java.util.concurrent.atomic.{LongAdder => JLongAdder}

        ///
        /// Returns a fresh counter with initialized to `initialVal`.
        ///
        @Internal
        pub def mkCounter(_: Region[r], initialValue: Int32): AtomicCounter[r] \ r =
            let adder = unchecked_cast((new JLongAdder(): _ \ IO) as _ \ r);
            unchecked_cast((adder.add(Int32.toInt64(initialValue)): _ \ IO) as _ \ r);
            AtomicCounter(adder)


        ///
        /// Increments `counter` by `1`.
        ///
        @Internal
        pub def increment(counter: AtomicCounter[r]): Unit \ r =
            let AtomicCounter(c) = counter;
            unchecked_cast((c.increment(): _ \ IO) as _ \ r);
            ()

        ///
        /// Returns the current state of `counter`.
        ///
        @Internal
        pub def get(counter: AtomicCounter[r]): Int32 \ r =
            let AtomicCounter(c) = counter;
            unchecked_cast((c.intValue(): _ \ IO) as _ \ r)
    }


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
    @Internal
    pub enum Lock[_: Region](JStampedLock)

    mod Lock {
        import java.util.concurrent.locks.{StampedLock => JStampedLock}
        use BPlusTree.Lock

        // Unchecked casts are here used to remove the IO effect. It is replaced with an
        // effect to a region to ensure that the code is not optimized away.

        ///
        /// Wait for `lock` to be unlocked by the current holder of the write-lock on it.
        ///
        /// Why:
        ///
        /// Virtual threads can exhibit weird behaviour when using all physical threads
        /// and using locking. Without this the threads using optimistic reading will
        /// never be removed from the physical thread. Meanwhile a thread that holds the
        /// lock will never get to run and we have a deadlock. Progress can happen, but it
        /// appears to be extremely unlikely. To fix this call a blocking operation which
        /// makes the JVM remove the virtual threads from the physical threads allowing
        /// the lock-holder to unlock.
        ///
        /// For a better explanation see Netflix's encounter of a similar problem:
        /// https://netflixtechblog.com/java-21-virtual-threads-dude-wheres-my-lock-3052540e231d?gi=128dba0ad426
        ///
        @Internal
        pub def yieldBasedOn(lock: Lock[r]): Unit \ r =
            let Lock(l) = lock;
            let stamp = unchecked_cast((l.readLock(): _ \ IO) as _ \ r);
            unchecked_cast((l.unlockRead(stamp): _ \ IO) as _ \ r)

        ///
        /// Returns `true` if `lock` is currently held by a thread.
        ///
        /// Only supposed to be used for testing.
        ///
        @Internal
        pub def isLocked(lock: Lock[r]): Bool \ r =
            let Lock(l) = lock;
            unchecked_cast((l.isWriteLocked() or l.isReadLocked(): _ \ IO) as _ \ r)

        ///
        /// Returns a fresh lock.
        ///
        @Internal
        pub def mkLock(_: Region[r]): Lock[r] \ r =
            Lock(unchecked_cast((new JStampedLock(): _ \ IO) as _ \ r))

        /// Attempt to upgrade `stamp` on `lock` to a write-lock on `lock`.
        /// The returned stamp cannot be invalidated if it was valid when issued.
        ///
        /// This operation is non-blocking.
        ///
        @Internal
        pub def tryConvertToWrite(stamp: Int64, lock: Lock[r]): Int64 \ r =
            let Lock(l) = lock;
            unchecked_cast((l.tryConvertToWriteLock(stamp): _ \ IO) as _ \ r)

        ///
        /// Return an optimistic stamp on `lock` which can be used in `valid` to
        /// assert that no thread has taken a write lock since `stamp` was issued.
        ///
        /// This operation is non-blocking.
        ///
        @Internal
        pub def tryReadLock(lock: Lock[r]): Int64 \ r =
            let Lock(l) = lock;
            unchecked_cast((l.tryOptimisticRead(): _ \ IO) as _ \ r)

        ///
        /// Unlock `lock` with the issued write-stamp, `stamp`.
        ///
        /// This operation is non-blocking.
        ///
        @Internal
        pub def unlockWrite(stamp: Int64, lock: Lock[r]): Unit \ r =
            let Lock(l) = lock;
            unchecked_cast((l.unlockWrite(stamp): _ \ IO) as _ \ r)

        ///
        /// Returns true if `stamp` is still valid on `lock`, meaning no thread
        /// has acquired a write-lock on `lock` since `stamp` was issued.
        ///
        /// This operation is non-blocking.
        ///
        @Internal
        pub def valid(stamp: Int64, lock: Lock[r]): Bool \ r =
            let Lock(l) = lock;
            unchecked_cast((l.validate(stamp): _ \ IO) as _ \ r)

        ///
        /// Lock `lock` and returns the stamp.
        ///
        /// This operation is blocking.
        ///
        @Internal
        pub def writeLock(lock: Lock[r]): Int64 \ r =
            let Lock(l) = lock;
            unchecked_cast((l.writeLock(): _ \ IO) as _ \ r)
    }

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
    @Internal
    pub def assertTreeInvariant(t: BPlusTree[k, v, r]): Bool \ r with Order[k] =
        let root = t->root;
        let arity = BPlusTree.arity(t);
        let nodeWorks = Node.assertNodeInvariant(root, arity, true, None, t->search);
        let rootIsUnlocked = not BPlusTree.Lock.isLocked(t->rootLock);
        let rootHasNoParent = Option.isEmpty(Node.parent(root));
        nodeWorks and rootHasNoParent and rootIsUnlocked


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
        emptyWithSearch(rc, Vector#{})

    ///
    /// Returns a fresh empty BPlusTree tree with arity `m` (the maximum keys/values per node).
    /// The arity must be at least 3.
    ///
    pub def emptyWithArity(rc: Region[r], m: Int32): BPlusTree[k, v, r] \ r =
        emptyWithArityAndSearch(rc, m `Int32.max` 3, Vector#{})

    ///
    /// Returns a fresh empty BPlusTree tree of arity `m` (the maximum keys/values per node).
    /// The arity must be at least 3.
    ///
    /// `search` decided the comparison order for the stored vectors.
    ///
    @Internal
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
    @Internal
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
    /// This method unsafely removes the `r` effect when spawning threads.
    ///
    /// Not thread-safe.
    ///
    @Internal
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
            unchecked_cast(Vector.range(0, threadNum) |>
            (Vector.forEach(i -> {
                let _: Unit = spawn unchecked_cast(
                    // Flix does not allow region effect in spawns. Remove the effect.
                    (Node.traverseRightUnconditionalInc(f, i, threadNum, minLeaf): _ \ r) as _ \ IO
                ) @ rc;
                ()
            }): _ \ {}) as _ \ r)
        }
    }

    ///
    /// Applies `f1` to all mappings `k => v` in `t` sequentially if `t` contains strictly less than
    /// `parLimit` elements. If `t` contains `parLimit` or more elements `f2` is applied in parallel
    /// to all mappings.
    ///
    /// `f1` and `f2` must not modify `t`.
    ///
    /// This method unsafely removes the `r` effect when spawning threads in case 2.
    ///
    /// Not thread-safe.
    ///
    @Internal
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
    @Internal
    pub def rc(t: BPlusTree[k, v, r]): Region[r] = t->rc

    ///
    /// Returns the search of `t`.
    ///
    @Internal
    pub def search(t: BPlusTree[k, v, r]): Search = t->search

    ///
    /// Returns the size of `t`. This operation is `O(1)`.
    ///
    /// Not atomic.
    ///
    /// Thread-safe.
    ///
    @Internal
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
        "Search: ${Vector.join(" < ", t->search)}\nTree:\n${Node.toString(0, t->rc, t->root)}"

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
    @Internal
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

    mod Node {
        import java.lang.invoke.{VarHandle => JVarHandle}
        import java.lang.{Object => JObject}
        import java.util.{Objects => JObjects}
        use BPlusTree.Lock
        use BPlusTree.Search

        ///
        /// Internal method to insert `key` and `val` in `node` at `insertionPoint`.
        /// Increments the `size` of `node`.
        ///
        /// Lock invariant:
        ///
        /// - On invocation, the thread has a lock on `node`.
        ///
        def actualInsertLeaf(
            key: k,
            val: v,
            insertionPoint: Int32,
            node: Node[k, v, r]
        ): Unit \ r =
            let size = node->size;
            arrayInsert(val, insertionPoint, size, node->values);
            arrayInsert(key, insertionPoint, size, node->keys);
            node->size = size + 1

        ///
        /// Internal method to update the leaf `node` with `key => f(key, val)` if `key => val` is in `node`.
        ///
        /// Otherwise, `node` is unchanged.
        ///
        /// Thread-safe.
        ///
        /// Lock invariant:
        ///
        /// - On invocation, the thread has a lock on `node`.
        ///
        @Internal
        pub def adjustWithKey(f: k -> v -> v \ ef, key: k, stamp: Int64, t: BPlusTree[k, v, r], node: Node[k, v, r]): Unit \ ef + r with Order[k] = {
            let index = binarySearch(key, BPlusTree.search(t), node);
            if (index >= 0) {
                let writeStamp = Lock.tryConvertToWrite(stamp, node->lock);
                if (not Lock.valid(writeStamp, node->lock)) {
                    Lock.yieldBasedOn(node->lock);
                    BPlusTree.adjustWithKey(f, key, t)
                } else {
                    let val = Array.get(index, node->values);
                    Array.put(f(key, val), index, node->values);
                    Lock.unlockWrite(writeStamp, node->lock)
                }
            } else if (not Lock.valid(stamp, node->lock)) {
                BPlusTree.adjustWithKey(f, key, t)
            }
        }

        ///
        /// Applies the predicate `f` to all pairs of direcly subsequent elements of `v` and returns the and.
        /// E.g. f(v[0], v[1]) and f(v[1], v[2]) and ... and f(v[n-1], v[n])
        /// Only used by the testing functions
        ///
        def applyPredicate(v: Vector[k], f: k -> k -> Bool \ r): Bool \ r =
            let zipped = Vector.map(x -> (x, true), v);
            let reduced = Vector.reduceLeft(old -> cur ->
                let (oldNum, oldBool) = old;
                let (curNum, _) = cur;
                (curNum, oldBool and f(oldNum, curNum))
            , zipped);
            match reduced {
                case None => true
                case Some((_, bool)) => bool
            }

        ///
        /// Insert `x` at position `i` in array `arr` as though it currently has `len`
        /// elements. Shifts elements at position `[i, .., len]` to `[i+1, .., len+1]`.
        ///
        def arrayInsert(x: a, i: Int32, len: Int32, arr: Array[a, r]): Unit \ r =
            if (len == i)
                Array.put(x, i, arr)
            else {
                Array.copyInto(srcPos = i, dstPos = i + 1, len = len - i, src = arr, arr);
                Array.put(x, i, arr)
            }

        ///
        /// Returns `true` if and only if `node` is structurally correct.
        ///
        /// Only used for internal testing.
        ///
        @Internal
        pub def assertNodeInvariant(
            node: Node[k, v, r],
            arity: Int32,
            isRightMost: Bool,
            shouldBeBiggerThan: Option[k],
            search: Search
        ): Bool \ r with Order[k] =
            def unwrapNode(opt: Option[Node[k, v, r]]): Node[k, v, r] = match opt {
                case Some(val) => val
                case None => bug!("")
            };
            match node->isLeaf {
                case false =>
                    let c = node->children;
                    let size = node->size;
                    let keys = toLength(Array.toVector(node->keys), size - 1);
                    let children = toLength(Array.toVector(c), size);
                    let keysInOrder = isSorted(keys, search);
                    let firstChildWorks = assertNodeInvariant(Vector.get(0, children), arity, false, shouldBeBiggerThan, search);
                    let lastChildWorks = assertNodeInvariant(unwrapNode(Vector.last(children)), arity, isRightMost, if (size == 0) shouldBeBiggerThan else Vector.last(keys), search);
                    let zippedWithoutFirst = Vector.filter(match (index, _) -> index != 0, Vector.mapWithIndex(i -> x -> (i, x), toLength(children, size - 1)));
                    let middleChildrenWorks = Vector.forAll(pair ->
                        let (i, p) = pair;
                        assertNodeInvariant(p, arity, false, Some(Vector.get(i - 1, keys)), search), zippedWithoutFirst);
                    let childrenWorks = firstChildWorks and middleChildrenWorks and lastChildWorks;
                    let parentPointerCorrect = Vector.foldLeft(
                        workingSoFar -> cur -> workingSoFar and equalByReference(node, unwrapNode(cur->parent)),
                        true, children);
                    let lastHasNoNext = not isRightMost or Option.isEmpty(unwrapNode(Vector.last(children))->next);
                    let noLeaves = Array.size(node->values) == 0;
                    let nodeIsUnlocked = not BPlusTree.Lock.isLocked(node->lock);
                    let isBiggerThanPrev = match shouldBeBiggerThan {
                        case None => true
                        case Some(v) => comparison(Vector.get(0, keys), v, search) == 1
                    };
                    parentPointerCorrect and
                        lastHasNoNext and noLeaves and keysInOrder and
                        childrenWorks and nodeIsUnlocked and isBiggerThanPrev
                case true =>
                    let size = node->size;
                    let keys = toLength(Array.toVector(node->keys), size);
                    let keysInOrder = isSorted(toLength(keys, size), search);
                    let sizeInRange = size <= arity and 0 <= size;
                    let noChildren = Array.size(node->children) == 0;
                    let leafIsUnlocked = not BPlusTree.Lock.isLocked(node->lock);
                    sizeInRange and keysInOrder and leafIsUnlocked and noChildren
        }

        ///
        /// Perform a binary search and return the index of `key` in `node->keys` if it exists.
        /// Otherwise, return `(-insertionPoint - 1)` where `insertionPoint` is the index
        /// where `key` would be inserted.
        ///
        /// `search` defines the order in which elements of the vectors are compared.
        ///
        def binarySearch(key: k, search: Search, node: Node[k, v, r]): Int32 \ r with Order[k] =
            let length = if (not node->isLeaf) node->size - 1 else node->size;
            let arr = node->keys;
            def f(l: Int32, r: Int32): Int32 \ r = {
                if (l <= r) {
                    let m = (l + r) `Int32.rightShift` 1;
                    let elem = Array.get(m, arr);
                    if (Object.isNull(elem)) {
                        // We read null. Return -1. For all callers this would mean that the element was not inserted
                        // This can only happen in a multi-threaded environment and all functions should in this
                        // case attempt to validate its stamp, which will fail so the result will not be used.
                        -1
                    } else {
                        let comp = comparison(key, elem, search);
                        if (comp == 1) f(m+1, r)
                        else if (comp == -1) f(l, m-1)
                        else m
                    }
                } else -l-1
            };
            f(0, length - 1)

        ///
        /// Compare `val1` to `val2`. If `search` has length zero then the order on `k` is used for the comparison.
        ///
        /// If `search` has non-zero length `val1` and `val2` is assumed to be of type `Vector[Int64]`.
        /// The elements will be compared in the order given by `search`.
        ///
        def comparison(val1: k, val2: k, search: Search): Int32 with Order[k] = {
            let len = Vector.length(search);
            // If the values are of type Vector[Int64] check whether a specific search is given.
            // If so use the custom compare function for that search.
            if (len == 0) {
                let comp = val1 <=> val2;
                if (comp == Comparison.GreaterThan) 1
                else if (comp == Comparison.LessThan) -1
                else 0
            } else {
                vectorComp(unchecked_cast(val1 as Vector[Int64]), unchecked_cast(val2 as Vector[Int64]), len, 0, search)
            }
        }

        ///
        /// Compare `vec1` to `vec2` from `i` (inclusive) to `max` (exlusive) in the order defined by `search`.
        ///
        def vectorComp(vec1: Vector[Int64], vec2: Vector[Int64], max: Int32, i: Int32, search: Vector[Int32]): Int32 = {
                if (i >= max) {
                    0
                } else {
                    let s = Vector.get(i, search);
                    let vec1P = Vector.get(s, vec1);
                    let vec2P = Vector.get(s, vec2);
                    if (vec1P < vec2P) {
                        -1
                    } else if (vec1P > vec2P) {
                        1
                    } else {
                        vectorComp(vec1, vec2, max, i + 1, search)
                    }
                }
            }

        ///
        /// Compute the height of the tree rooted in `node`.
        ///
        /// Not thread-safe.
        ///
        @Internal
        pub def computeHeight(node: Node[k, v, r]): Int32 \ r =
            // Every leaf is at the same height, so just use the left most path.
            def loop(count, n) = {
                match n->isLeaf {
                    case true => count
                    case false =>
                        if (n->size == 0) {
                            count
                        } else {
                            loop(count + 1, Array.get(0, n->children))
                        }
                }
            };
            loop(1, node)

        ///
        /// Return the mapping `k => v` at position `index` in `node`. Crashes if 
        /// `index >= node->size`.
        ///
        @Internal
        pub def getKeyAndValueAt(index: Int32, node: Node[k, v, r]): (k, v) \ r = {
            let k = Array.get(index, node->keys);
            let v = Array.get(index, node->values);
            (k, v)
        }

        ///
        /// Optionally return the node to the right of `node`.
        ///
        @Internal
        pub def next(node: Node[k, v, r]): Option[Node[k, v, r]] \ r = node->next

        ///
        /// Returns `true` if the pointer of `o1` is equal to the pointer of `o2`.
        ///
        def equalByReference(o1: Node[k, v, r], o2: Node[k, v, r]): Bool =
            // Unchecked cast is here used to transform the values to Object.
            // The purpose is to be able to give them to a Java class which performs pointer comparison.
            let obj1 = unchecked_cast(o1 as JObject);
            let obj2 = unchecked_cast(o2 as JObject);
            // Unchecked cast is here used to remove the IO effect from interacting with Java.
            unchecked_cast((JObjects.equals(obj1, obj2): _ \ IO) as _ \ {})

        ///
        /// Optionally returns the first mapping satisfying the function `f` in `node` and
        /// to the right of `node`.
        ///
        /// Not thread-safe.
        ///
        @Internal
        pub def findLeft(
            f: k -> v -> Bool \ ef,
            index: Int32,
            node: Node[k, v, r]
        ): Option[(k, v)] \ ef + r =
            if (index < node->size) {
                let k = Array.get(index, node->keys);
                let v = Array.get(index, node->values);
                if (f(k, v))
                    Some((k, v))
                else
                    findLeft(f, index + 1, node)
            } else match node->next {
                case None => None
                case Some(next) => findLeft(f, 0, next)
            }

        ///
        /// Returns `true` if and only if there exists mappings in `node` or to the right of `node`
        /// satisfying `f`.
        ///
        /// Not thread-safe.
        ///
        @Internal
        pub def exists(
            f: k -> v -> Bool \ ef,
            index: Int32,
            node: Node[k, v, r]
        ): Bool \ ef + r =
            if (index < node->size) {
                let k = Array.get(index, node->keys);
                let v = Array.get(index, node->values);
                if (f(k, v))
                    true
                else
                    exists(f, index + 1, node)
            } else match node->next {
                case None => false
                case Some(next) => exists(f, 0, next)
            }

        ///
        /// Provides access to the `lock` field of `node`.
        ///
        @Internal
        pub def getLock(node: Node[k, v, r]): Lock[r] = node->lock

        ///
        /// Returns `Some(val)` if `key => val` is in `tree`. Otherwise return `None`.
        ///
        @Internal
        pub def get(key: k, stamp: Int64, node: Node[k, v, r], tree: BPlusTree[k, v, r]): Option[v] \ r with Order[k] =
            let index = binarySearch(key, BPlusTree.search(tree), node);
            let result = if (index < 0)
                None
            else
                Some(Array.get(index, node->values));
            if (not Lock.valid(stamp, node->lock)) {
                // Restart
                Lock.yieldBasedOn(node->lock);
                BPlusTree.get(key, tree)
            } else {
                result
            }

        ///
        /// Returns `Some((key, value))` if `key => val` is in `tree`. Otherwise return `None`.
        ///
        @Internal
        pub def getKeyAndValue(key: k, stamp: Int64, node: Node[k, v, r], tree: BPlusTree[k, v, r]): Option[(k, v)] \ r with Order[k] =
            let index = binarySearch(key, BPlusTree.search(tree), node);
            let result = if (index < 0)
                None
            else
                Some((Array.get(index, node->keys), Array.get(index, node->values)));
            if (not Lock.valid(stamp, node->lock)) {
                // Restart
                Lock.yieldBasedOn(node->lock);
                BPlusTree.getKeyAndValue(key, tree)
            } else {
                result
            }

        ///
        /// Returns `val` if `key => val` is in `tree`. Otherwise return `d`.
        ///
        @Internal
        pub def getWithDefault(key: k, stamp: Int64, d: v, node: Node[k, v, r], tree: BPlusTree[k, v, r]): v \ r with Order[k] =
            let index = binarySearch(key, BPlusTree.search(tree), node);
            let result = if (index < 0)
                d
            else
                Array.get(index, node->values);
            if (not Lock.valid(stamp, node->lock)) {
                // Restart
                Lock.yieldBasedOn(node->lock);
                BPlusTree.getWithDefault(key, d, tree)
            } else {
                result
            }

        ///
        /// Inserts the mapping `key => val` into the leaf `node`, potentially causing a
        /// split and returning a new root.
        ///
        /// Lock invariants:
        ///
        /// - On invocation, `stamp` is an optimistic stamp on `node->lock`.
        ///
        /// - On return, the thread has no lock on the tree if the returned value is None.
        ///   Otherwise it has a write-lock on `tree->rootLock` and `p` where `Some(p)` is returned
        ///
        @Internal
        pub def insertIntoLeaf(
            key: k,
            val: v,
            node: Node[k, v, r],
            stamp: Int64,
            tree: BPlusTree[k, v, r]
        ): Option[(Node[k, v, r], Int64)] \ r with Order[k] = {
            let search = BPlusTree.search(tree);
            let index = binarySearch(key, search, node);
            let writeStamp = Lock.tryConvertToWrite(stamp, node->lock);
            if (not Lock.valid(writeStamp, node->lock)) {
                // Restart
                Lock.yieldBasedOn(node->lock);
                BPlusTree.putInternal(key, val, tree)
            } else {
                insertIntoLeafPropagate(key, val, index, node, writeStamp, tree)
            }
        }

        ///
        /// Inserts the mapping `key => val` into the leaf `node` if it is not already present
        /// Otherwise if the mapping `key2 => val2` is presents overwrite the mapping by `key => val`,
        /// if `decider(key, val, key2, val2) == true`. If a split is caused return the new root.
        ///
        /// Lock invariants:
        ///
        /// - On invocation, `stamp` is an optimistic stamp on `node->lock`.
        ///
        /// - On return, the thread has no lock on the tree if the returned value is None.
        ///   Otherwise it has a write-lock on `tree->rootLock` and `p` where `Some(p)` is returned
        ///
        @Internal
        pub def insertIntoLeafIf(
            decider: k -> v -> k -> v -> Bool,
            key: k,
            val: v,
            node: Node[k, v, r],
            stamp: Int64,
            tree: BPlusTree[k, v, r]
        ): Option[(Node[k, v, r], Int64)] \ r with Order[k] = {
            let search = BPlusTree.search(tree);
            let index = binarySearch(key, search, node);
            if (index < 0) {
                // `key` is not currently in tree. Attempt to gain the write lock and add it.
                insertIntoLeafForceIf(decider, key, val, node, stamp, index, tree)
            } else {
                let oldKey = Array.get(index, node->keys);
                let oldVal = Array.get(index, node->values);
                // Verify that the pair is legal/not corrupted.
                if (not Lock.valid(stamp, node->lock)) {
                    // Restart
                    Lock.yieldBasedOn(node->lock);
                    BPlusTree.putIfInternal(decider, key, val, tree)
                } else {
                    if (decider(key, val, oldKey, oldVal)) {
                        insertIntoLeafForceIf(decider, key, val, node, stamp, index, tree)
                    } else {
                        // At some point `oldKey => oldVal` was in the tree. We observed it and
                        // decided to not insert `key => val`, by `decider`.
                        None
                    }
                }
            }
        }

        ///
        /// Attempts to grab the lock of `node` and insert the `key => val` mapping. Restarts with
        /// `putIfInternal` if this is not successfull.
        ///
        /// Lock invariants:
        ///
        /// - On invocation, `stamp` is an optimistic stamp on `node->lock`.
        ///
        /// - On return, the thread has no lock on the tree if the returned value is None.
        ///   Otherwise it has a write-lock on `tree->rootLock` and `p` where `Some(p)` is returned
        ///
        def insertIntoLeafForceIf(
            decider: k -> v -> k -> v -> Bool,
            key: k,
            val: v,
            node: Node[k, v, r],
            stamp: Int64,
            index: Int32,
            tree: BPlusTree[k, v, r]
        ): Option[(Node[k, v, r], Int64)] \ r with Order[k] = {
            let writeStamp = Lock.tryConvertToWrite(stamp, node->lock);
            if (not Lock.valid(writeStamp, node->lock)) {
                // Restart
                Lock.yieldBasedOn(node->lock);
                BPlusTree.putIfInternal(decider, key, val, tree)
            } else {
                insertIntoLeafPropagate(key, val, index, node, writeStamp, tree)
            }
        }

        ///
        /// Inserts the mapping `key => val` into the leaf `node`, potentially causing a
        /// split and returning a new root.
        ///
        /// Lock invariants:
        ///
        /// - On invocation, `writeStamp` is a write stamp on `node->lock`.
        ///
        /// - On return, the thread has no lock on the tree if the returned value is None.
        ///   Otherwise it has a write-lock on `tree->rootLock` and `p` where `Some(p)` is returned
        ///
        def insertIntoLeafPropagate(
            key: k,
            val: v,
            index: Int32,
            node: Node[k, v, r],
            writeStamp: Int64,
            tree: BPlusTree[k, v, r]
        ): Option[(Node[k, v, r], Int64)] \ r with Order[k] = {
            let arity = BPlusTree.arity(tree);
            let size = node->size;
            if (index >= 0) {
                // We now have a write lock and can insert before releasing lock
                Array.put(key, index, node->keys);
                Array.put(val, index, node->values);
                Lock.unlockWrite(writeStamp, node->lock);
                None
            } else {
                BPlusTree.incrementSize(tree);
                // We now have a write lock and can insert and propagate.
                let insertionPoint = toInsertionPoint(index);
                if (size < arity) {
                    // Just insert and after which we are done, so release lock.
                    actualInsertLeaf(key, val, insertionPoint, node);
                    Lock.unlockWrite(writeStamp, node->lock);
                    None
                } else {
                    // We need to insert and split, locking up through the tree.
                    splitLeaf(node, writeStamp, key, val, insertionPoint, tree)
                }
            }
        }

        ///
        /// Returns `true` if `node` is empty.
        ///
        /// Not thread-safe.
        ///
        @Internal
        pub def isEmpty(node: Node[k, v, r]): Bool \ r = node->size == 0

        ///
        /// Returns true if and only if the elements of `v` are sorted.
        /// Only used for internal testing.
        ///
        def isSorted(v: Vector[k], search: Search): Bool with Order[k] =
            applyPredicate(v, x -> y -> comparison(x, y, search) == -1)

        ///
        /// Returns `true` if `key` is contained in the leaf `node`.
        ///
        @Internal
        pub def leafMemberOf(key: k, search: Search, node: Node[k, v, r]): Bool \ r with Order[k] =
            let index = binarySearch(key, search, node);
            if (index < 0)
                false
            else
                true

        ///
        /// Returns `true` if the mapping `key => val` is contained in the leaf `node`.
        ///
        @Internal
        pub def leafMemberOfPair(key: k, val: v, search: Search, node: Node[k, v, r]): Bool \ r with Order[k], Eq[v] =
            let index1 = binarySearch(key, search, node);
            if (index1 < 0)
                false
            else
                Array.get(index1, node->values) == val

        ///
        /// Returns the left-most leaf node rooted in `node`.
        ///
        /// Not thread-safe.
        ///
        @Internal
        pub def leftMostChild(node: Node[k, v, r]): Node[k, v, r] \ r =
            match node->isLeaf {
                case false =>
                    leftMostChild(Array.get(0, node->children))
                case true => node
            }

        ///
        /// Attempt to lock the parent of `node`. Returns the stamp associated with the parent.
        /// If the parent is None return a lock on the `tree->rootLock`.
        ///
        /// Lock invariants:
        ///
        /// - On invocation, the thread has a write-lock on `node`.
        ///
        /// - On return, the thread holds a write-lock on `node` and on `parent` given by
        ///   `node->Some(parent)`
        ///
        def lockParent(node: Node[k, v, r], tree: BPlusTree[k, v, r]): Int64 \ r = match node->parent {
            case Some(parent) =>
                let parentLock = parent->lock;
                let writeStamp = Lock.writeLock(parentLock);
                // If some thread had a lock on your parent and changed your parent pointer retry.
                let possiblyNewParent = match node->parent {
                    case Some(v) => v
                    case None => bug!("Cannot happen (when there are no deletions). Parent pointer replaced by None")
                };
                let isSame = equalByReference(possiblyNewParent, parent);
                if (not isSame) {
                    Lock.unlockWrite(writeStamp, parentLock);
                    lockParent(node, tree)
                } else writeStamp
            // `node` was root node. We need to lock the root instead, as it protects the parent pointer of `node`.
            case None =>
                let stamp = BPlusTree.writeLockRootPointer(tree);
                let parentHasChanged = match node->parent {
                    case Some(_) => true
                    case None => false
                };
                if (parentHasChanged) {
                    BPlusTree.unlockRootPointer(stamp, tree);
                    lockParent(node, tree)
                } else {
                    stamp
                }
        }

        ///
        /// Optionally returns `k => v` where `k` is the minimum key.
        ///
        /// Not thread-safe.
        ///
        @Internal
        pub def minimumKey(t: Node[k, v, r]): Option[(k, v)] \ r =
            let leftMost = leftMostChild(t);
            if (size(leftMost) == 0) {
                None
            } else {
                Some((Array.get(0, leftMost->keys), Array.get(0, leftMost->values)))
            }

        ///
        /// Create a new leaf node.
        ///
        @Internal
        pub def mkLeaf(
            rc: Region[r],
            keys: Array[k, r],
            values: Array[v, r],
            size: Int32,
            parent: Option[Node[k, v, r]],
            next: Option[Node[k, v, r]]
        ): Node[k, v, r] \ r =
            new Node @ rc {
                keys = keys,
                children = Array.empty(rc, 0),
                values = values,
                lock = Lock.mkLock(rc),
                size = size,
                parent = parent,
                next = next,
                isLeaf = true
            }

        ///
        /// Create a new internal node.
        ///
        def mkInternal(
            rc: Region[r],
            keys: Array[k, r],
            children: Array[Node[k, v, r], r],
            size: Int32,
            parent: Option[Node[k, v, r]]
        ): Node[k, v, r] \ r =
            new Node @ rc {
                keys = keys,
                children = children,
                values = Array.empty(rc, 0),
                lock = Lock.mkLock(rc),
                size = size,
                parent = parent,
                next = None,
                isLeaf = false
            }

        ///
        /// Provides access to the `parent` field of `node`.
        ///
        @Internal
        pub def parent(node: Node[k, v, r]): Option[Node[k, v, r]] \ r = node->parent

        ///
        /// Push the key `newKey` to `parent`. If `parent` is `Some(node)`, insert
        /// `newKey` into `node` if `size + 1 <= arity`. Otherwise split `node` and return
        /// a new root.
        ///
        /// If `parent` is `None`, create a new root node, insert `newKey` into it and
        /// return it.
        ///
        /// Lock invariants:
        ///
        /// - On invocation, the thread has a write-lock on `parent`, represented by `parentStamp`.
        ///
        /// - On return, the thread only holds a lock if the return value is Some. In
        /// that case it holds a write-lock on `tree->rootLock`.
        def pushKeyToParent(
            parent: Option[Node[k, v, r]],
            leftChild: Node[k, v, r],
            rightChild: Node[k, v, r],
            parentStamp: Int64,
            newKey: k,
            tree: BPlusTree[k, v, r]
        ): Option[(Node[k, v, r], Int64)] \ r with Order[k] =
        match parent {
            case Some(node) =>
                let arity = BPlusTree.arity(tree);
                let search = BPlusTree.search(tree);
                let children = node->children;
                let insertionPoint = toInsertionPoint(binarySearch(newKey, search, node));
                let size = node->size;
                if (size < arity) {
                    arrayInsert(newKey, insertionPoint, size - 1, node->keys);
                    arrayInsert(rightChild, insertionPoint + 1, size, children);
                    node->size = size + 1;
                    Lock.unlockWrite(parentStamp, node->lock);
                    None
                } else {
                    splitInternal(node, parentStamp, rightChild, insertionPoint, newKey, tree)
                }
            case None =>
                // In this case, leftChild is the root of the tree
                // and we must construct a new root node
                let arity = BPlusTree.arity(tree);
                let rc = BPlusTree.rc(tree);
                let children = Array.empty(rc, arity);
                let keys = Array.empty(rc, arity - 1);
                Array.put(leftChild, 0, children);
                Array.put(rightChild, 1, children);
                Array.put(newKey, 0, keys);
                let newRoot = mkInternal(rc, keys, children, 2, None);
                leftChild->parent = Some(newRoot);
                rightChild->parent = Some(newRoot);
                Some((newRoot, parentStamp))
        }

        ///
        /// Inserts the mapping `key => f(val, val1)` if `key => val1` is in `node`.
        /// Otherwise, inserts `key => val` into `node`.
        ///
        /// Returns `Some(newRoot, stamp)` if the root was split.
        ///
        /// Thread-safe.
        ///
        @Internal
        pub def putWith(f: v -> v -> v \ ef, key: k, val: v, tree: BPlusTree[k, v, r], node: Node[k, v, r], stamp: Int64): Option[(Node[k, v, r], Int64)] \ r + ef with Order[k] =
            let writeStamp = Lock.tryConvertToWrite(stamp, node->lock);
            if (not Lock.valid(writeStamp, node->lock)) {
                // Restart
                Lock.yieldBasedOn(node->lock);
                BPlusTree.putWithInternal(f, key, val, tree)
            } else {
                let index = binarySearch(key, BPlusTree.search(tree), node);
                if (index < 0) {
                    insertIntoLeafPropagate(key, val, index, node, writeStamp, tree)
                } else {
                    let v1 = Array.get(index, node->values);
                    Array.put(f(val, v1), index, node->values);
                    Lock.unlockWrite(writeStamp, node->lock);
                    None
                }
            }

        ///
        /// Applies `f` in ascending order to all mappings `k => v` in the tree
        /// rooted in `node` where `min <= k <= max`.
        ///
        /// Not thread-safe.
        ///
        @Internal
        pub def rangeQueryWith(
            f: k -> v -> Unit \ r0,
            min: k,
            max: k,
            stamp: Int64,
            node: Node[k, v, r],
            tree: BPlusTree[k, v, r]
        ): Unit \ r0 + r with Order[k] =
            let search = BPlusTree.search(tree);
            let (minLeaf, _) = traverseDown(min, node, stamp, tree);
            let index = binarySearch(min, search, minLeaf);
            if (index < 0) {
                let insertionPoint = toInsertionPoint(index);
                if (insertionPoint <= minLeaf->size) {
                    traverseRight(f, insertionPoint, max, search, minLeaf)
                }
            } else
                traverseRight(f, index, max, search, minLeaf)

        ///
        /// Returns the size of `node`.
        ///
        @Internal
        pub def size(node: Node[k, v, r]): Int32 \ r = node->size

        ///
        /// Split `leftLeaf` into two leaves and push the middle key into the parent,
        /// potentially causing the parent to split and return a new root. Should only
        /// be called when `leftLeaf->size == arity` and
        /// `0 <= insertionPoint <= arity + 1`.
        ///
        /// Lock invariants:
        ///
        /// - On invocation, the thread has a write-lock on `leftNode`.
        ///
        /// - On return, the thread only holds a lock if the return value is Some. In
        /// that case it holds a write-lock on `tree->rootLock`.
        def splitLeaf(
            leftLeaf: Node[k, v, r],
            leftStamp: Int64,
            key: k,
            val: v,
            insertionPoint: Int32,
            tree: BPlusTree[k, v, r]
        ): Option[(Node[k, v, r], Int64)] \ r with Order[k] =
            let arity = BPlusTree.arity(tree);
            let rc = BPlusTree.rc(tree);
            let prev_mid = arity / 2;
            let (mid, insertInLeft) = if (insertionPoint <= prev_mid) (prev_mid, true) else (prev_mid + 1, false);
            let (rightKeys, rightValues) = splitToRightLeaf(rc, mid, arity, leftLeaf);
            // Create a new leaf to store the right half
            let rightLeaf = mkLeaf(rc, rightKeys, rightValues, leftLeaf->size - mid, leftLeaf->parent, leftLeaf->next);
            // Update the old leaf's fields
            leftLeaf->next = Some(rightLeaf);
            leftLeaf->size = mid;
            // Finally insert the new key-value pair into the correct leaf
            if (insertInLeft) actualInsertLeaf(key, val, insertionPoint, leftLeaf)
            else actualInsertLeaf(key, val, insertionPoint - mid, rightLeaf);
            // Push rightLeaf's smallest key up to parent
            // We do not need the lock on the left leaf any longer.
            let parentStamp = lockParent(leftLeaf, tree);
            rightLeaf->parent = leftLeaf->parent;
            Lock.unlockWrite(leftStamp, leftLeaf->lock);
            pushKeyToParent(leftLeaf->parent, leftLeaf, rightLeaf, parentStamp, Array.get(0, rightKeys), tree)

        ///
        /// Split `leftNode` into two nodes with `newKey` inserted into the appropriate
        /// node depending on `insertionPoint`.
        ///
        /// Lock invariants:
        ///
        /// - On invocation, the thread has a lock on `leftNode`.
        ///
        /// - On return, the thread only holds a lock if the return value is Some. In
        /// that case it holds a write-lock on `tree->rootLock`.
        def splitInternal(
            leftNode: Node[k, v, r],
            leftNodeStamp: Int64,
            insertedChild: Node[k, v, r],
            insertionPoint: Int32,
            newKey: k,
            tree: BPlusTree[k, v, r]
        ): Option[(Node[k, v, r], Int64)] \ r with Order[k] =
            let arity = BPlusTree.arity(tree);
            let rc = BPlusTree.rc(tree);
            let prev_mid = arity / 2;
            // Note: The current implementation requires that we do not insert `insertedChild` as
            // the last element in the left child. If this is done the inserted key will be incorrect
            let (mid, insertInLeft, rightSize) = if (insertionPoint >= prev_mid) (prev_mid + 1, false, arity - prev_mid) else (prev_mid, true, arity - prev_mid);
            leftNode->size = mid;
            let (keyOfRight, rightKeys, rightChildren) = if (insertInLeft) {
                splitInternalToLeftChild(rc, insertedChild, insertionPoint, newKey, mid, arity, rightSize, leftNode)
            } else {
                splitInternalToRightChild(rc, insertedChild, insertionPoint, newKey, mid, arity, rightSize, leftNode)
            };
            let rightNode = mkInternal(rc, rightKeys, rightChildren, rightSize, leftNode->parent);
            // Lock `rightNode` before updating child-pointers.
            // This prevents threads working on children from grabbing the lock of our
            // parent pointer before we lock it.
            let rightNodeStamp = Lock.writeLock(rightNode->lock);
            let parentStamp = lockParent(leftNode, tree);
            rightNode->parent = leftNode->parent;
            Array.forEachWithIndex(i -> child ->
                if (i < rightSize) {
                    child->parent = Some(rightNode)
                }, rightChildren);
            Lock.unlockWrite(leftNodeStamp, leftNode->lock);
            Lock.unlockWrite(rightNodeStamp, rightNode->lock);
            pushKeyToParent(leftNode->parent, leftNode, rightNode, parentStamp, keyOfRight, tree)

        ///
        /// Moves elements of `leftLeaf->keys` and `leftLeaf->values` from `mid` (inclusive) to `arity` (exclusive)
        /// to the returned arrays, respectively.
        ///
        def splitToRightLeaf(rc: Region[r], mid: Int32, arity: Int32, leftLeaf: Node[k, v, r]): (Array[k, r], Array[v, r]) \ r = {
            let rightKeys = Array.empty(rc, arity);
            let rightValues = Array.empty(rc, arity);
            Array.copyInto(srcPos = mid, dstPos = 0, len = arity - mid, src = leftLeaf->keys, rightKeys);
            Array.copyInto(srcPos = mid, dstPos = 0, len = arity - mid, src = leftLeaf->values, rightValues);
            (rightKeys, rightValues)
        }

        ///
        /// Moves elements of `leftNode->keys` and `leftNode->children`
        /// from `mid` (inclusive) to `arity` (exclusive) to the returned
        /// arrays, respectively. Inserts `insertedChild` at `insertionPoint`
        /// in `leftNode`.
        ///
        /// Note: This function assumes that `insertionPoint < mid - 1`.
        /// In other words `insertedChild` is not the rightMost child.
        ///
        /// Returns `(key, rightKeys, rightChildren)` where `key` is the largest value in the left subtree.
        ///
        def splitInternalToLeftChild(
            rc: Region[r],
            insertedChild: Node[k, v, r],
            insertionPoint: Int32,
            newKey: k,
            mid: Int32,
            arity: Int32,
            rightSize: Int32,
            leftNode: Node[k, v, r]
        ): (k, Array[k, r], Array[Node[k, v, r], r]) \ r with Order[k] = {
            let keyToPass = Array.get(mid - 1, leftNode->keys);
            let leftChildren = leftNode->children;
            let leftKeys = leftNode->keys;
            let rightKeys = Array.empty(rc, arity - 1);
            let rightChildren = Array.empty(rc, arity);
            Array.copyInto(srcPos = mid, dstPos = 0, len = rightSize - 1, src = leftKeys, rightKeys);
            Array.copyInto(srcPos = mid, dstPos = 0, len = rightSize, src = leftChildren, rightChildren);
            arrayInsert(insertedChild, insertionPoint + 1, mid, leftChildren);
            arrayInsert(newKey, insertionPoint, mid, leftKeys);
            leftNode->size = mid + 1;
            (keyToPass, rightKeys, rightChildren)
        }

        ///
        /// Moves elements of `leftNode->keys` and `leftNode->children` from
        /// `mid` (inclusive) to `arity` (exclusive) to the returned arrays, respectively.
        /// Inserts `insertedChild` at `insertionPoint` in the right node.
        ///
        /// Returns `(key, rightKeys, rightChildren)` where `key` is the largest value in the left subtree.
        ///
        def splitInternalToRightChild(
            rc: Region[r],
            insertedChild: Node[k, v, r],
            insertionPoint: Int32,
            newKey: k,
            mid: Int32,
            arity: Int32,
            rightSize: Int32,
            leftNode: Node[k, v, r]
        ): (k, Array[k, r], Array[Node[k, v, r], r]) \ r = {
            // Insert the new node in the new right node and copy the relevant elements of the leftNode (index>=mid).
            let rightKeys = Array.empty(rc, arity - 1);
            let rightChildren = Array.empty(rc, arity);
            let rightSizeWithoutNew = rightSize - 1;
            if (insertionPoint + 1 == mid) {
                // If insertionPoint is the first element copy all old keys and old elements and insert new child at 0.
                Array.copyInto(srcPos = mid - 1, dstPos = 0, len = rightSizeWithoutNew, src = leftNode->keys, rightKeys);
                Array.copyInto(srcPos = mid, dstPos = 1, len = rightSizeWithoutNew, src = leftNode->children, rightChildren);
                Array.put(insertedChild, 0, rightChildren);
                (newKey, rightKeys, rightChildren)
            } else {
                let childIndexInRight = insertionPoint + 1 - mid;
                Array.copyInto(srcPos = mid, dstPos = 0, len = childIndexInRight - 1, src = leftNode->keys, rightKeys);
                Array.copyInto(srcPos = mid, dstPos = 0, len = childIndexInRight, src = leftNode->children, rightChildren);
                Array.put(insertedChild, childIndexInRight, rightChildren);
                Array.put(newKey, childIndexInRight - 1, rightKeys);
                // If insertionPoint is the last index we are done, otherwise insert rest of list.
                if (childIndexInRight != rightSizeWithoutNew) {
                    Array.copyInto(srcPos = mid + childIndexInRight - 1, dstPos = childIndexInRight, len = rightSizeWithoutNew - childIndexInRight, src = leftNode->keys, rightKeys);
                    Array.copyInto(srcPos = mid + childIndexInRight, dstPos = childIndexInRight + 1, len = rightSizeWithoutNew - childIndexInRight, src = leftNode->children, rightChildren)
                };
                (Array.get(mid - 1, leftNode->keys), rightKeys, rightChildren)
            }
        }

        ///
        /// Returns the real insertion point of the result of binarySearch, assuming that
        /// the result of binarySearch was negative.
        ///
        def toInsertionPoint(i: Int32): Int32 = -i-1

        ///
        /// Slices `v` to the elements from 0 (inclusive) to `length` (exclusive).
        /// Only used for internal testing.
        ///
        def toLength(v: Vector[v], length: Int32): Vector[v] =
            Vector.slice(start = 0, end = length, v)

        ///
        /// Returns a string representation of the given Node `node` starting with
        /// `indent` spaces.
        ///
        /// Not thread-safe.
        ///
        @Internal
        pub def toString(indent: Int32, rc: Region[r], node: Node[k, v, r]): String \ r with ToString[k], ToString[v] =
            let indentString = String.repeat(indent, " ");
            match node->isLeaf {
                case false =>
                    let children = node->children;
                    let keysString =
                        Array.copyOfRange(rc, 0, node->size - 1, node->keys) |>
                        // Array.map(rc, tupleToString) |>
                        Array.join(",");
                    let childrenArr = Array.copyOfRange(rc, 0, node->size, children);
                    let childrenString = Array.foldLeft(a -> x -> "${a}${toString(indent+2, rc, x)}", "", childrenArr);
                    "${indentString}Internal(${keysString})\n${childrenString}"
                case true =>
                    let values = Array.copyOfRange(rc, 0, node->size, node->values);
                    let keys = Array.copyOfRange(rc, 0, node->size, node->keys);
                    let keysStrings = Array.mapWithIndex(rc, i -> key -> "${key}: ${Array.get(i, values)}", keys);
                    let keysString = Array.join(",\n${indentString} ", keysStrings);
                    "${indentString}Leaf(\n${indentString} ${keysString}\n${indentString})\n"
            }

        ///
        /// Traverse down through the tree while keeping the invariant that there is a
        /// `lock` on `cur`. If acquiring a lock at any point fails release the locks you
        /// hold and retry from the root.
        ///
        @Internal
        pub def traverseDown(key: k, cur: Node[k, v, r], stamp: Int64, tree: BPlusTree[k, v, r]): (Node[k, v, r], Int64) \ r with Order[k] =
            match cur->isLeaf {
                case false =>
                    let children = cur->children;
                    let index = binarySearch(key, BPlusTree.search(tree), cur);
                    let childToVisit = if (index < 0)
                        Array.get(toInsertionPoint(index), children)
                    else
                        Array.get(index + 1, children);
                    if (not Lock.valid(stamp, cur->lock)) {
                        // Give up and restart the attempt.
                        Lock.yieldBasedOn(cur->lock);
                        BPlusTree.findLeaf(key, tree)
                    } else {
                        let childStamp = Lock.tryReadLock(childToVisit->lock);
                        if (not Lock.valid(stamp, cur->lock)) {
                        Lock.yieldBasedOn(cur->lock);
                            BPlusTree.findLeaf(key, tree)
                        } else {
                            traverseDown(key, childToVisit, childStamp, tree)
                        }
                    }
                case true => (cur, stamp)
            }

        ///
        /// Applies `f` to mappings `k => v` starting at `node->keys[index]` until
        /// `node->keys[index] > max`. Uses the `next` pointer to traverse to the next node.
        ///
        /// Not thread-safe.
        ///
        def traverseRight(
            f: k -> v -> Unit \ ef,
            index: Int32,
            max: k,
            search: Search,
            node: Node[k, v, r]
        ): Unit \ ef + r with Order[k] =
            if (index < node->size) {
                let k = Array.get(index, node->keys);
                if (comparison(k, max, search) != 1) {
                    f(k, Array.get(index, node->values));
                    traverseRight(f, index + 1, max, search, node)
                }
            } else match node->next {
                case Some(next) => traverseRight(f, 0, max, search, next)
                case None => ()
            }

        ///
        /// Apply `f` to all mappings in `node` and all nodes to the right of `node`.
        ///
        /// Not thread-safe.
        ///
        @Internal
        pub def traverseRightUnconditional(
            f: k -> v -> Unit \ ef,
            index: Int32,
            node: Node[k, v, r]
        ): Unit \ ef + r =
            if (index < node->size) {
                let k = Array.get(index, node->keys);
                f(k, Array.get(index, node->values));
                traverseRightUnconditional(f, index + 1, node)
            } else match node->next {
                case Some(next) => traverseRightUnconditional(f, 0, next)
                case None => ()
            }
        ///
        /// Apply `f` to every `stepSize` mapping in `node` and continues in right of `node`.
        ///
        /// Not thread-safe.
        ///
        @Internal
        pub def traverseRightUnconditionalInc(
            f: k -> v -> Unit \ ef,
            index: Int32,
            stepSize: Int32,
            node: Node[k, v, r]
        ): Unit \ ef + r =
            if (index < node->size) {
                let k = Array.get(index, node->keys);
                f(k, Array.get(index, node->values));
                traverseRightUnconditionalInc(f, index + stepSize, stepSize, node)
            } else match node->next {
                case Some(next) => traverseRightUnconditionalInc(f, index - node->size, stepSize, next)
                case None => ()
            }
    }
}
