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
/// node or a leaf node. In contrast to binary search trees, a B+ Tree of order `m` only
/// allows nodes with at most `m` children and `m-1` keys. A B+ Tree maintains the sorted
/// order of its keys by only storing keys in internal nodes and values in leaf nodes.
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
/// `order` is the order of the tree which determines the maximum number of children a
/// node may have. The order must be at least 3.
///
/// `rootLock` is a lock on the `root` pointer, which a thread must first acquire before
/// accessing the root node. It must also be acquired before changing `root`. The node
/// `root` has a separate lock concerning the contents of the node. The lock of `root`
/// must first be acquired after successfully acquiring `rootLock`.
///
/// `rc` is the region for the tree.
///
/// `search` is a result of the primary usage being for vectors. A `Search` describes the
/// order in which elements should be compared. E.g. [0, 1, 2, ..., n] is the natural order.
///
/// For all other usages `search` will be a vector of size 0.

type alias Search = Vector[Int32]

pub struct BPlusTree[k, v, r] {
    mut root: BPlusTree.Node[k, v, r],
    order:    Int32,
    rootLock: BPlusTree.Lock[r],
    rc:       Region[r],
    search:   Search
}

mod BPlusTree {
    import java.util.concurrent.locks.{StampedLock => JStampedLock}

    ///
    /// The purpose of Lock is to provide a thin wrapper around Java's StampedLock
    /// without the IO effect. The lock is optimistic in the sense that reads are
    /// performed by reading the values and only afterwards verifying that no other
    /// thread has changed the values. Only annotating a region effect to StampedLock
    /// is safe, since it can only non-deterministically block a thread for some (
    /// potentially infinite) amount of time. This is allowed under non-effectful behavior.
    ///
    ///
    /// One possible danger, introduced by StampedLock, is handled by a null-check in binarySearch.
    /// Consider the following example where everything is initialized to 0:
    /// Thread 1 executes
    /// {a = 2; b = 3}
    /// Thread 2 executes
    /// {c = a; d = b}
    /// The question is what can `c` and `d` be? It depends on the JVM, but can be any of:
    /// {c = 2, d = 3}, {c = 0, d = 0}, {c = 2, d = 0}, {c = 0, d = 3}
    /// The last follows from the fact that the JVM can reorder statements under some requirements.
    /// The requirements are roughly that the new 'code' should be equivalent on a single-threaded machine
    /// and there are no happens-before relations which link to other threads preventing the reordering.
    /// If either variable were volatile we might avoid this, but volatile is expensive (and not in Flix).
    ///
    /// For us this means that {array[size] = 5; size++} can be reordered to {oldSize = size++; array[oldSize] = 5}.
    /// Another thread reading may then see the increase of size and try to access that position getting null.
    /// This has not happened in our experiments, but we wish to handle the case. The risk is in binarySearch and we
    /// therefore check whether any value read is null and if so 'aborts'.
    ///
    /// For Java's memory model see: https://docs.oracle.com/javase/specs/jls/se8/html/jls-17.html#jls-17.4
    ///

    @Internal
    pub enum Lock[_: Region](JStampedLock)

    mod Lock {
        import java.util.concurrent.locks.{StampedLock => JStampedLock}
        use BPlusTree.Lock

        /// Unchecked casts are here used to remove the IO effect. It is replaced with an
        /// effect to a region to ensure that the code is not optimized away.

        ///
        /// Wait for `lock` to be unlocked by the current holder of the write-lock on it.
        ///
        /// Why:
        ///
        /// Virtual threads can show weird behaviour when using all physical threads
        /// and using locking. Without this the threads using optimistic reading will never be
        /// removed from the physical thread. Meanwhile a thread that holds the lock will never
        /// get to run and we have a deadlock. Progress can happen, but it appears to be
        /// extremely unlikely. To fix this call a blocking operation which makes the JVM remove
        /// the virtual threads from the physical threads allowing the lock-holder to unlock.
        ///
        /// For a better explanation see Netflix's encounter of a similar problem:
        /// https://netflixtechblog.com/java-21-virtual-threads-dude-wheres-my-lock-3052540e231d?gi=128dba0ad426
        pub def dumbWait(lock: Lock[r]): Unit \ r =
            let Lock(l) = lock;
            let stamp = unchecked_cast((l.readLock(): _ \ IO) as _ \ r);
            unchecked_cast((l.unlockRead(stamp): _ \ IO) as _ \ r)

        ///
        /// Returns `true` if `lock` is currently held by a thread.
        /// Only supposed to be used for testing.
        ///
        @Internal
        pub def isLocked(lock: Lock[r]): Bool \ r =
            let Lock(l) = lock;
            unchecked_cast((l.isWriteLocked() or l.isReadLocked(): _ \ IO) as _ \ r)

        ///
        /// Returns a fresh lock.
        ///
        pub def mkLock(_: Region[r]): Lock[r] \ r =
            Lock(unchecked_cast((new JStampedLock(): _ \ IO) as _ \ r))


        /// Attmept to upgrade `stamp` on `lock` to a write-lock on `lock`.
        /// The returned stamp cannot be invalidated if it was valid when issued.
        ///
        /// This operation is non-blocking.
        ///
        pub def tryConvertToWrite(stamp: Int64, lock: Lock[r]): Int64 \ r =
            let Lock(l) = lock;
            unchecked_cast((l.tryConvertToWriteLock(stamp): _ \ IO) as _ \ r)

        ///
        /// Return an optimistic stamp on `lock` which can be used in `valid` to
        /// assert that no thread has taken a write lock since `stamp` was issued.
        ///
        /// This operation is non-blocking.
        ///
        pub def tryReadLock(lock: Lock[r]): Int64 \ r =
            let Lock(l) = lock;
            unchecked_cast((l.tryOptimisticRead(): _ \ IO) as _ \ r)

        ///
        /// Unlock `lock` with the issued write-stamp, `stamp`.
        ///
        /// This operation is non-blocking.
        ///
        pub def unlockWrite(stamp: Int64, lock: Lock[r]): Unit \ r =
            let Lock(l) = lock;
            unchecked_cast((l.unlockWrite(stamp): _ \ IO) as _ \ r)

        ///
        /// Returns true if `stamp` is still valid on `lock`, meaning no thread
        /// has acquired a write-lock on `lock` since `stamp` was issued.
        ///
        /// This operation is non-blocking.
        ///
        pub def valid(stamp: Int64, lock: Lock[r]): Bool \ r =
            let Lock(l) = lock;
            unchecked_cast((l.validate(stamp): _ \ IO) as _ \ r)

        ///
        /// Lock `lock` and returns the stamp.
        ///
        /// This operation is blocking.
        ///
        pub def writeLock(lock: Lock[r]): Int64 \ r =
            let Lock(l) = lock;
            unchecked_cast((l.writeLock(): _ \ IO) as _ \ r)
    }

    ///
    /// Returns true if and only if `tree` is a structurally correct tree.
    /// Only used for internal testing
    ///
    @Internal
    pub def assertTreeInvariant(tree: BPlusTree[k, v, r]): Bool \ r with Order[k] =
        let root = tree->root;
        let order = BPlusTree.order(tree);
        let nodeWorks = Node.assertNodeInvariant(root, order, true, None, tree->search);
        let rootIsUnlocked = not BPlusTree.Lock.isLocked(tree->rootLock);
        let rootHasNoParent = Option.isEmpty(Node.parent(root));
        nodeWorks and rootHasNoParent and rootIsUnlocked


    ///
    /// Returns `true` if and only if `tree` contains the key-value pair `(key, val)`.
    ///
    /// Thread-safe.
    ///
    pub def containsPair(key: k, val: v, tree: BPlusTree[k, v, r]): Bool \ r with Order[k], Eq[v] =
        let (leaf, stamp) = BPlusTree.findLeaf(key, tree);
        let res = Node.leafContainsPair(key, val, tree->search, leaf);
        if(Lock.valid(stamp, Node.getLock(leaf))) {
            res
        } else {
            Lock.dumbWait(Node.getLock(leaf));
            containsPair(key, val, tree)
        }

    ///
    /// Returns a fresh empty BPlusTree tree of standard order wrapping values of `k`.
    ///
    pub def empty(rc: Region[r]): BPlusTree[k, v, r] \ r =
        emptyWithSearch(rc, Vector#{})

    ///
    /// Returns a fresh empty BPlusTree tree of `order` (the maximum keys/values per node).
    /// The order of the tree must be at least 3.
    ///
    @Internal
    pub def emptyWithOrder(rc: Region[r], order: Int32): BPlusTree[k, v, r] \ r =
        emptyWithOrderAndSearch(rc, order, Vector#{})

    ///
    /// Returns a fresh empty BPlusTree tree of `order` (the maximum keys/values per node).
    /// The order of the tree must be at least 3.
    ///
    /// `search` decided the comparison order for the stored vectors.
    ///
    @Internal
    pub def emptyWithOrderAndSearch(rc: Region[r], order: Int32, search: Search): BPlusTree[k, v, r] \ r =
        let node = Node.mkLeaf(rc, Array.empty(rc, order), Array.empty(rc, order), 0, None, None);
        new BPlusTree @ rc {
            root = node,
            order = order,
            rootLock = Lock.mkLock(rc),
            rc = rc,
            search = search
        }

    ///
    /// Returns a fresh empty BPlusTree tree of standard order.
    ///
    /// `search` decided the comparison order for the stored vectors.
    ///
    @Internal
    pub def emptyWithSearch(rc: Region[r], search: Search): BPlusTree[k, v, r] \ r =
        emptyWithOrderAndSearch(rc, 64, search)

    ///
    /// Returns the leaf containing `key` and leaves it locked. If no node contains `key` the rightmost
    /// node with values strictly less than `key` is returned.
    ///
    def findLeaf(key: k, tree: BPlusTree[k, v, r]): (Node[k, v, r], Int64) \ r with Order[k] =
        let (node, stamp) = getLockedRoot(tree);
        Node.traverseDown(key, node, stamp, tree)

    ///
    /// Updates the root of `tree` if `newRoot` is some. Unlocks `tree->rootLock` using the stamp.
    ///
    def fixRoot(newRoot: Option[(Node[k, v, r], Int64)], tree: BPlusTree[k, v, r]): Unit \ r = match newRoot {
        case Some((root, rootStamp)) =>
            tree->root = root;
            Lock.unlockWrite(rootStamp, tree->rootLock);
            ()
        case None => ()
    }

    ///
    /// Apply `f` to all mapping `(key, val)` in `tree`
    ///
    /// Not thread-safe
    ///
    pub def forEach(f: k -> v -> Unit \ ef, tree: BPlusTree[k, v, r]): Unit \ ef + r with Order[k] =
        let minLeaf = Node.leftMostChild(tree->root);
        Node.traverseRightUnconditional(f, 0, minLeaf)

    ///
    /// Return the root of `tree` and a stamp on the lock of the returned node.
    ///
    def getLockedRoot(tree: BPlusTree[k, v, r]): (Node[k, v, r], Int64) \ r =
        let rootPointerStamp = Lock.tryReadLock(tree->rootLock);
        let rootNode = tree->root;
        let nodeStamp = Lock.tryReadLock(Node.getLock(rootNode));
        if(Lock.valid(rootPointerStamp, tree->rootLock)){
            (rootNode, nodeStamp)
        } else {
            Lock.dumbWait(tree->rootLock);
            getLockedRoot(tree)
        }

    ///
    /// Returns `v` if `(key, v)` is in `tree`. Otherwise return `d`.
    ///
    pub def getWithDefault(key: k, d: v, tree: BPlusTree[k, v, r]): v \ r with Order[k] =
        let (leaf, stamp) = BPlusTree.findLeaf(key, tree);
        let res = Node.getWithDefault(key, stamp, d, leaf, tree);
        res

    ///
    /// Updates the BPlusTree `tree` with the key-value pair `(key, val)`. Replaces any
    /// existing key-value pair.
    ///
    /// Thread-safe.
    ///
    pub def insert(key: k, val: v, tree: BPlusTree[k, v, r]): Unit \ r with Order[k], Eq[v] =
        let newRoot = insertInternal(key, val, tree);
        fixRoot(newRoot, tree)

    ///
    /// Updates the BPlusTree `tree` with the key-value pair `(key, val)`. Replaces any
    /// existing key-value pair.
    ///
    /// Internal method for insert. Called when restarting.
    ///
    /// Returns the `Some(newRoot, stamp)` if the root was split where `stamp` is a write-stamp
    /// on `rootLock`.
    ///
    def insertInternal(key: k, val: v, tree: BPlusTree[k, v, r]): Option[(Node[k, v, r], Int64)] \ r with Order[k], Eq[v] =
        let (leaf, stamp) = BPlusTree.findLeaf(key, tree);
        Node.insertIntoLeaf(key, val, leaf, stamp, tree)

    ///
    /// Returns `true` if and only if `tree` is the empty tree.
    ///
    /// Thread-safe.
    ///
    pub def isEmpty(tree: BPlusTree[k, v, r]): Bool \ r =
        let (root, stamp) = BPlusTree.getLockedRoot(tree);
        let res = Node.isEmpty(root);
        if(Lock.valid(stamp, Node.getLock(root))) {
            res
        } else {
            // Restart
            Lock.dumbWait(Node.getLock(root));
            isEmpty(tree)
        }

    ///
    /// Returns `true` if and only if `tree` contains the key `k`.
    ///
    /// Thread-safe.
    ///
    pub def memberOf(key: k, tree: BPlusTree[k, v, r]): Bool \ r with Order[k] =
        let (leaf, stamp) = BPlusTree.findLeaf(key, tree);
        let res = Node.leafContains(key, tree->search, leaf);
        if(not Lock.valid(stamp, Node.getLock(leaf))) {
            Lock.dumbWait(Node.getLock(leaf));
            memberOf(key, tree)
        } else {
            res
        }

    ///
    /// Merge `src` into `dst` modyfing `dst` in a left-biased manner.
    ///
    /// That is, key collisions are resolved by taking the mapping from m1.
    ///
    /// Not thread-safe
    ///
    pub def merge(src: BPlusTree[k, v, r], dst: BPlusTree[k, v, r]): Unit \ r with Order[k], Eq[v] = {
        BPlusTree.forEach(k -> v -> BPlusTree.insert(k, v, dst), src)
    }

    ///
    /// Merge `src` into `dst` modyfing `dst`. If `(key, val1)` is in `src` and `(key, val2)` is in `dst`
    /// update `dst` to contain `(key, f(val1, val2))`.
    ///
    /// Not thread-safe
    ///
    pub def mergeWith(f: v -> v -> v \ ef, src: BPlusTree[k, v, r], dst: BPlusTree[k, v, r]): Unit \ r + ef with Order[k], Eq[v] = {
        BPlusTree.forEach(k -> v -> BPlusTree.putWith(f, k, v, dst), src)
    }

    ///
    /// Returns the order of `tree`.
    ///
    @Internal
    pub def order(tree: BPlusTree[k, v, r]): Int32 = tree->order

    ///
    /// Updated `tree` with the mapping `(key, val)` if `key` is not inserted.
    /// Otherwise if the mapping `(key, oldVal)` is in `tree` update the mapping
    /// to `key->f(val, oldVal)`.
    ///
    /// Thread-safe
    ///
    pub def putWith(f: v -> v -> v \ ef, key: k, val: v, tree: BPlusTree[k, v, r]): Unit \ ef + r with Order[k], Eq[v] =
        let optNewRoot = putWithInternal(f, key, val, tree);
        fixRoot(optNewRoot, tree)

    ///
    /// Internal method to update the mapping as described by `putWith`.
    ///
    /// Called when restarting.
    ///
    /// Thread-safe
    ///
    def putWithInternal(f: v -> v -> v \ ef, k: k, val: v, tree: BPlusTree[k, v, r]): Option[(Node[k, v, r], Int64)] \ ef + r with Order[k], Eq[v] =
        let (leaf, stamp) = findLeaf(k, tree);
        Node.putWith(f, k, val, tree, leaf, stamp)

    ///
    /// Applies `f` in ascending order to all key-value pairs `(k, v)` in `tree` where
    /// `min <= k <= max`.
    ///
    /// Not thread-safe.
    ///
    pub def rangeQueryWith(f: k -> v -> Unit \ ef, min: k, max: k, tree: BPlusTree[k, v, r]): Unit \ r + ef with Order[k] =
        let (root, stamp) = getLockedRoot(tree);
        Node.rangeQueryWith(f, min, max, stamp, root, tree)

    ///
    /// Returns the region of `tree`.
    ///
    @Internal
    pub def rc(tree: BPlusTree[k, v, r]): Region[r] = tree->rc

    ///
    /// Returns the search of `tree`.
    ///
    @Internal
    pub def search(tree: BPlusTree[k, v, r]): Search = tree->search

    ///
    /// Returns a string representation of BPlusTree `tree`.
    ///
    /// Not thread-safe
    ///
    pub def toString(tree: BPlusTree[k, v, r]): String \ r with ToString[k], ToString[v] =
        "Search: ${Vector.join(" < ", tree->search)}\nTree:\n${Node.toString(0, tree->rc, tree->root)}"

    ///
    /// Write lock `tree->rootLock` and return the stamp.
    ///
    def writeLockRootPointer(tree: BPlusTree[k, v, r]): Int64 \ r =
        let res = Lock.writeLock(tree->rootLock);
        res

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
    ///     `keys` and `values` have the same size (which is the `order` of the
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
    ///      `children` is of size `order` and `keys` have size `order - 1`
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

    mod Node {
        import java.lang.invoke.{VarHandle => JVarHandle}
        import java.lang.{Object => JObject}
        import java.util.{Objects => JObjects}
        use BPlusTree.Lock

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
        /// Applies the predicate `f` to all pairs of direcly subsequent elements of `v` and returns the and.
        /// E.g. f(v[0], v[1]) and f(v[1], v[2]) and ... and f(v[n-1], v[n])
        /// Only used by the testing functions
        ///
        def applyPredicate(v: Vector[k], f: k -> k -> Bool \ r): Bool \ r =
            let zipped = Vector.map(x -> (x, true), v);
            let reduced =  Vector.reduceLeft(old -> cur ->
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
        /// Returns true if and only if the node is structurally correct.
        /// Only used for internal testing.
        ///
        @Internal
        pub def assertNodeInvariant(
            node: Node[k, v, r],
            order: Int32,
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
                    let firstChildWorks = assertNodeInvariant(Vector.get(0, children), order, false, shouldBeBiggerThan, search);
                    let lastChildWorks = assertNodeInvariant(unwrapNode(Vector.last(children)), order, isRightMost, if(size == 0) shouldBeBiggerThan else Vector.last(keys), search);
                    let zippedWithoutFirst = Vector.filter(match (index, _) -> index != 0, Vector.mapWithIndex(i -> x -> (i, x), toLength(children, size - 1)));
                    let middleChildrenWorks = Vector.forAll(pair ->
                        let (i, p) = pair;
                        assertNodeInvariant(p, order, false, Some(Vector.get(i - 1, keys)), search), zippedWithoutFirst);
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
                    let sizeInRange = size <= order and 0 <= size;
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
            let length = if(not node->isLeaf) node->size - 1 else node->size;
            let arr = node->keys;
            def f(l: Int32, r: Int32): Int32 \ r = {
                if (l <= r) {
                    let m = (l + r) `Int32.rightShift` 1;
                    let elem = Array.get(m, arr);
                    if(Object.isNull(elem)) {
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
        /// Compare `val1` to `val2`. If `search` has non-zero length and `val1` and `val2` are of type `Vector[Int64]`
        /// compare the lements in the order defined by `search`.
        ///
        def comparison(val1: k, val2: k, search: Search): Int32 with Order[k] = {
            let len = Vector.length(search);
            // If the values are of type Vector[Int64] check whether a specific search is given.
            // If so use the custom compare function for that search.
            typematch val1 {
                case val_1: Vector[Int64] =>
                    if(len == 0) {
                        let comp = val1 <=> val2;
                        if (comp == Comparison.GreaterThan) 1
                        else if (comp == Comparison.LessThan) -1
                        else 0
                    } else {
                        typematch val2 {
                            case val_2: Vector[Int64] => vectorComp(val_1, val_2, len, 0, search)
                            case _: _ => unreachable!()
                        }
                    }
                case _: _ =>
                    let comp = val1 <=> val2;
                    if (comp == Comparison.GreaterThan) 1
                    else if (comp == Comparison.LessThan) -1
                    else 0
            }
        }

        ///
        /// Compare `vec1` to `vec2` from `i` (inclusive) to `max` (exlusive) in the order defined by `search`.
        ///
        def vectorComp(vec1: Vector[Int64], vec2: Vector[Int64], max: Int32, i: Int32, search: Vector[Int32]): Int32 = {
                if(i >= max) {
                    0
                } else {
                    let s = Vector.get(i, search);
                    let vec1P = Vector.get(s, vec1);
                    let vec2P = Vector.get(s, vec2);
                    if(vec1P < vec2P) {
                        -1
                    } else if (vec1P > vec2P) {
                        1
                    } else {
                        vectorComp(vec1, vec2, max, i + 1, search)
                    }
                }
            }


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
        /// Provides access to the `lock` field of `node`.
        ///
        pub def getLock(node: Node[k, v, r]): Lock[r] = node->lock

        ///
        /// Returns `v` if `(key, v)` is in `tree`. Otherwise return `d`.
        ///
        pub def getWithDefault(key: k, stamp: Int64, d: v, node: Node[k, v, r], tree: BPlusTree[k, v, r]): v \ r with Order[k] =
            let index = binarySearch(key, BPlusTree.search(tree), node);
            let result = if (index < 0)
                d
            else
                Array.get(index, node->values);
            if(not Lock.valid(stamp, node->lock)) {
                // Restart
                Lock.dumbWait(node->lock);
                BPlusTree.getWithDefault(key, d, tree)
            } else {
                result
            }

        ///
        /// Insert key-value pair `(key, val)` into the leaf `node`, potentially causing a
        /// split and returning a new root.
        ///
        /// Lock invariants:
        ///
        /// - On invocation, `stamp` is an optimistic stamp on `node->lock`.
        ///
        /// - On return, the thread has no lock on the tree if the returned value is None
        ///     Otherwise it has a write-lock on `tree->rootLock` and `p` where `Some(p)` is returned
        ///
        pub def insertIntoLeaf(
            key: k,
            val: v,
            node: Node[k, v, r],
            stamp: Int64,
            tree: BPlusTree[k, v, r]
        ): Option[(Node[k, v, r], Int64)] \ r with Order[k], Eq[v] = {
            let search = BPlusTree.search(tree);
            let index = binarySearch(key, search, node);
            insertIntoLeafPropagate(key, val, index, node, stamp, tree)
        }

        def insertIntoLeafPropagate(
                key: k,
                val: v,
                index: Int32,
                node: Node[k, v, r],
                stamp: Int64,
                tree: BPlusTree[k, v, r]
        ): Option[(Node[k, v, r], Int64)] \ r with Order[k], Eq[v] = {
            let order = BPlusTree.order(tree);
            let size = node->size;
            if (index >= 0) {
                // This can be optimized out for non-lattice parts. It will always be true for them.
                if(val == Array.get(index, node->values)) {
                    if(not Lock.valid(stamp, node->lock)) {
                        // Restart
                        Lock.dumbWait(node->lock);
                        BPlusTree.insertInternal(key, val, tree)
                    } else {
                        // Value was in tree and noone has taken a write-lock.
                        None
                    }
                } else {
                    let writeStamp = Lock.tryConvertToWrite(stamp, node->lock);
                    if(not Lock.valid(writeStamp, node->lock)) {
                        // Restart
                        Lock.dumbWait(node->lock);
                        BPlusTree.insertInternal(key, val, tree)
                    } else {
                        // We now have a write lock and can insert before releasing lock
                        Array.put(val, index, node->values);
                        Lock.unlockWrite(writeStamp, node->lock);
                        None
                    }
                }
            } else {
                let writeStamp = Lock.tryConvertToWrite(stamp, node->lock);
                if(not Lock.valid(writeStamp, node->lock)) {
                    // Restart
                    Lock.dumbWait(node->lock);
                    BPlusTree.insertInternal(key, val, tree)
                } else {
                    // We now have a write lock and can insert and propagate.
                    let insertionPoint = toInsertionPoint(index);
                    if (size < order) {
                        // Just insert and after which we are done, so release lock.
                        actualInsertLeaf(key, val, insertionPoint, node);
                        Lock.unlockWrite(writeStamp, node->lock);
                        None
                    } else{
                        // We need to insert and split, locking up through the tree.
                        splitLeaf(node, writeStamp, key, val, insertionPoint, tree)
                    }}
            }
        }

        ///
        /// Returns `true` if `node` is empty.
        ///
        /// Not thread-safe.
        ///
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
        pub def leafContains(key: k, search: Search, node: Node[k, v, r]): Bool \ r with Order[k] =
            let index = binarySearch(key, search, node);
            if (index < 0)
                false
            else
                true

        ///
        /// Returns `true` if the mapping `(key, val)` is contained in the leaf `node`.
        ///
        pub def leafContainsPair(key: k, val: v, search: Search, node: Node[k, v, r]): Bool \ r with Order[k], Eq[v] =
            let index1 = binarySearch(key, search, node);
            if (index1 < 0)
                false
            else
                Array.get(index1, node->values) == val

        ///
        /// Returns the left-most leaf tree rooted in `node`.
        ///
        /// Not thread-safe.
        ///
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
            case None => BPlusTree.writeLockRootPointer(tree)
        }

        ///
        /// Create a new leaf node.
        ///
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
        pub def parent(node: Node[k, v, r]): Option[Node[k, v, r]] \ r = node->parent

        ///
        /// Push the key `newKey` to `parent`. If `parent` is `Some(node)`, insert
        /// `newKey` into `node` if `size + 1 <= order`. Otherwise split `node` and return
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
                let order = BPlusTree.order(tree);
                let search = BPlusTree.search(tree);
                let children = node->children;
                let insertionPoint = toInsertionPoint(binarySearch(newKey, search, node));
                let size = node->size;
                if(size < order) {
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
                let order = BPlusTree.order(tree);
                let rc = BPlusTree.rc(tree);
                let children = Array.empty(rc, order);
                let keys = Array.empty(rc, order - 1);
                Array.put(leftChild, 0, children);
                Array.put(rightChild, 1, children);
                Array.put(newKey, 0, keys);
                let newRoot = mkInternal(rc, keys, children, 2, None);
                leftChild->parent = Some(newRoot);
                rightChild->parent = Some(newRoot);
                Some((newRoot, parentStamp))
        }

        ///
        /// Updated `node` with the mapping `(key, val)` if `key` is not inserted.
        /// Otherwise if the mapping `(key, oldVal)` is in `node` update the mapping
        /// to `key->f(val, oldVal)`. Returns `Some(newRoot, stamp)` if the root was split.
        ///
        /// Thread-safe
        ///
        pub def putWith(f: v -> v -> v \ ef, key: k, val: v, tree: BPlusTree[k, v, r], node: Node[k, v, r], stamp: Int64): Option[(Node[k, v, r], Int64)] \ r + ef with Order[k], Eq[v] =
            let index = binarySearch(key, BPlusTree.search(tree), node);
            if (index < 0) {
                insertIntoLeafPropagate(key, val, index, node, stamp, tree)
            } else {
                let writeStamp = Lock.tryConvertToWrite(stamp, node->lock);
                if(not Lock.valid(writeStamp, node->lock)) {
                    // Restart
                    Lock.dumbWait(node->lock);
                    BPlusTree.putWithInternal(f, key, val, tree)
                } else {
                    let v1 = Array.get(index, node->values);
                    Array.put(f(val, v1), index, node->values);
                    Lock.unlockWrite(writeStamp, node->lock);
                    None
                }
            }

        ///
        /// Applies `f` in ascending order to all key-value pairs `(k, v)` in the tree
        /// rooted in `node` where `min <= k <= max`.
        ///
        /// Not thread-safe.
        ///
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
            if(index < 0) {
                let insertionPoint = toInsertionPoint(index);
                if (insertionPoint <= minLeaf->size)
                    traverseRight(f, insertionPoint, max, search, minLeaf)
                else ()
            } else
                traverseRight(f, index, max, search, minLeaf)

        ///
        /// Split `leftLeaf` into two leaves and push the middle key into the parent,
        /// potentially causing the parent to split and return a new root. Should only
        /// be called when `leftLeaf->size == order` and
        /// `0 <= insertionPoint <= order + 1`.
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
            let order = BPlusTree.order(tree);
            let rc = BPlusTree.rc(tree);
            let prev_mid = order / 2;
            let (mid, insertInLeft) = if (insertionPoint <= prev_mid) (prev_mid, true) else (prev_mid + 1, false);
            let (rightKeys, rightValues) = splitToRightLeaf(rc, mid, order, leftLeaf);
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
            let order = BPlusTree.order(tree);
            let rc = BPlusTree.rc(tree);
            let prev_mid = order / 2;
            // Note: The current implementation requires that we do not insert `insertedChild` as
            // the last element in the left child. If this is done the inserted key will be incorrect
            let (mid, insertInLeft, rightSize) = if (insertionPoint >= prev_mid) (prev_mid + 1, false, order - prev_mid) else (prev_mid, true, order - prev_mid);
            leftNode->size = mid;
            let (keyOfRight, rightKeys, rightChildren) = if (insertInLeft) {
                splitInternalToLeftChild(rc, insertedChild, insertionPoint, newKey, mid, order, rightSize, leftNode)
            } else {
                splitInternalToRightChild(rc, insertedChild, insertionPoint, newKey, mid, order, rightSize, leftNode)
            };
            let rightNode = mkInternal(rc, rightKeys, rightChildren, rightSize, leftNode->parent);
            Array.forEachWithIndex(i -> child ->
                if (i < rightSize)
                    child->parent = Some(rightNode)
                else (), rightChildren);
            let parentStamp = lockParent(leftNode, tree);
            rightNode->parent = leftNode->parent;
            Lock.unlockWrite(leftNodeStamp, leftNode->lock);
            pushKeyToParent(leftNode->parent, leftNode, rightNode, parentStamp, keyOfRight, tree)

        ///
        /// Moves elements of `leftLeaf->keys` and `leftLeaf->values` from `mid` (inclusive) to `order` (exclusive)
        /// to the returned arrays, respectively.
        ///
        def splitToRightLeaf(rc: Region[r], mid: Int32, order: Int32, leftLeaf: Node[k, v, r]): (Array[k, r], Array[v, r]) \ r with Order[k] = {
            let rightKeys = Array.empty(rc, order);
            let rightValues = Array.empty(rc, order);
            Array.copyInto(srcPos = mid, dstPos = 0, len = order - mid, src = leftLeaf->keys, rightKeys);
            Array.copyInto(srcPos = mid, dstPos = 0, len = order - mid, src = leftLeaf->values, rightValues);
            (rightKeys, rightValues)
        }

        ///
        /// Moves elements of `leftNode->keys` and `leftNode->children`
        /// from `mid` (inclusive) to `order` (exclusive) to the returned
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
            order: Int32,
            rightSize: Int32,
            leftNode: Node[k, v, r]
        ): (k, Array[k, r], Array[Node[k, v, r], r]) \ r with Order[k] = {
            let keyToPass = Array.get(mid - 1, leftNode->keys);
            let leftChildren = leftNode->children;
            let leftKeys = leftNode->keys;
            let rightKeys = Array.empty(rc, order - 1);
            let rightChildren = Array.empty(rc, order);
            Array.copyInto(srcPos = mid, dstPos = 0, len = rightSize - 1, src = leftKeys, rightKeys);
            Array.copyInto(srcPos = mid, dstPos = 0, len = rightSize, src = leftChildren, rightChildren);
            arrayInsert(insertedChild, insertionPoint + 1, mid, leftChildren);
            arrayInsert(newKey, insertionPoint, mid, leftKeys);
            leftNode->size = mid + 1;
            (keyToPass, rightKeys, rightChildren)
        }

        ///
        /// Moves elements of `leftNode->keys` and `leftNode->children` from
        /// `mid` (inclusive) to `order` (exclusive) to the returned arrays, respectively.
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
            order: Int32,
            rightSize: Int32,
            leftNode: Node[k, v, r]
        ): (k, Array[k, r], Array[Node[k, v, r], r]) \ r = {
            // Insert the new node in the new right node and copy the relevant elements of the leftNode (index>=mid).
            let rightKeys = Array.empty(rc, order - 1);
            let rightChildren = Array.empty(rc, order);
            let rightSizeWithoutNew = rightSize - 1;
            if(insertionPoint + 1 == mid) {
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
                if(childIndexInRight != rightSizeWithoutNew) {
                    Array.copyInto(srcPos = mid + childIndexInRight - 1, dstPos = childIndexInRight, len = rightSizeWithoutNew - childIndexInRight, src = leftNode->keys, rightKeys);
                    Array.copyInto(srcPos = mid + childIndexInRight, dstPos = childIndexInRight + 1, len = rightSizeWithoutNew - childIndexInRight, src = leftNode->children, rightChildren)
                } else ();
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
                    let keysString = Array.join(",\n${indentString}  ", keysStrings);
                    "${indentString}Leaf(\n${indentString}  ${keysString}\n${indentString})\n"
            }

        ///
        /// Traverse down through the tree while keeping the invariant that there is a
        /// `lock` on `cur`. If acquiring a lock at any point fails release the locks you
        /// hold and retry from the root.
        ///
        pub def traverseDown(key: k, cur: Node[k, v, r], stamp: Int64, tree: BPlusTree[k, v, r]): (Node[k, v, r], Int64) \ r with Order[k] =
            match cur->isLeaf {
                case false =>
                    let children = cur->children;
                    let index = binarySearch(key, BPlusTree.search(tree), cur);
                    let childToVisit = if (index < 0)
                        Array.get(toInsertionPoint(index), children)
                    else
                        Array.get(index + 1, children);
                    if(not Lock.valid(stamp, cur->lock)) {
                        // Give up and restart the attempt.
                        Lock.dumbWait(cur->lock);
                        BPlusTree.findLeaf(key, tree)
                    } else {
                        let childStamp = Lock.tryReadLock(childToVisit->lock);
                        if(not Lock.valid(stamp, cur->lock)) {
                        Lock.dumbWait(cur->lock);
                            BPlusTree.findLeaf(key, tree)
                        } else {
                            traverseDown(key, childToVisit, childStamp, tree)
                        }
                    }
                case true => (cur, stamp)
            }

        ///
        /// Applies `f` to key-value pairs `(k, v)` starting at
        /// `node->keys[index]` until `node->keys[index] > max`. Uses the `next` pointer
        /// to traverse to the next node.
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
            if(index < node->size) {
                let k = Array.get(index, node->keys);
                if(comparison(k, max, search) != 1) {
                    f(k, Array.get(index, node->values));
                    traverseRight(f, index + 1, max, search, node)
                } else ()
            } else match node->next {
                case Some(next) => traverseRight(f, 0, max, search, next)
                case None => ()
            }

        ///
        /// Apply `f` to all mappings in `node` and all nodes to the right of `node`
        ///
        /// Not thread-safe
        ///
        pub def traverseRightUnconditional(
            f: k -> v -> Unit \ ef,
            index: Int32,
            node: Node[k, v, r]
        ): Unit \ ef + r with Order[k] =
            if(index < node->size) {
                let k = Array.get(index, node->keys);
                f(k, Array.get(index, node->values));
                traverseRightUnconditional(f, index + 1, node)
            } else match node->next {
                case Some(next) => traverseRightUnconditional(f, 0, next)
                case None => ()
            }
    }
}
