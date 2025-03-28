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
pub struct BPlusTree[k, v, r] {
    mut root: BPlusTree.Node[k, v, r],
    order:    Int32,
    rootLock: BPlusTree.Lock[r],
    rc:       Region[r]
}

mod BPlusTree {
    import java.util.concurrent.locks.{ReentrantLock => JReentrantLock}

    ///
    /// The purpose of Lock is to provide a thin wrapper around Java's ReentrantLock
    /// without the IO effect. Only one thread can hold a lock at any given time, but
    /// calling lock on the same lock multiple times only means that the holder must call
    /// unlock the same number of times before other threads can acquire the lock. Only
    /// annotating a region effect to ReentrantLock is safe, since it can only
    /// non-deterministically block a thread for some (potentially infinite) amount of
    /// time. This is allowed under non-effectful behavior.
    ///
    @Internal
    pub enum Lock[_: Region](JReentrantLock)

    mod Lock {
        import java.util.concurrent.locks.{ReentrantLock => JReentrantLock}
        use BPlusTree.Lock

        /// Unchecked casts are here used to remove the IO effect. It is replaced with an
        /// effect to a region to ensure that the code is not optimized away.

        ///
        /// Returns a fresh lock.
        ///
        pub def mkLock(_: Region[r]): Lock[r] \ r =
            Lock(unchecked_cast((new JReentrantLock(): _ \ IO) as _ \ r))

        ///
        /// Acquires `lock` only if it is not currently held by another thread.
        ///
        pub def tryLock(lock: Lock[r]): Bool \ r =
            let Lock(l) = lock;
            unchecked_cast((l.tryLock(): _ \ IO) as _ \ r)

        ///
        /// Acquire the lock `lock`.
        ///
        /// This operation is blocking.
        ///
        pub def lock(lock: Lock[r]): Unit \ r =
            let Lock(l) = lock;
            unchecked_cast((l.lock(): _ \ IO) as _ \ r)

        ///
        /// Attempts to release `lock`.
        ///
        pub def unlock(lock: Lock[r]): Unit \ r =
            let Lock(l) = lock;
            unchecked_cast((l.unlock(): _ \ IO) as _ \ r)

        ///
        /// Returns `true` if `lock` is currently held by a thread.
        /// Only supposed to be used for testing.
        ///
        @Internal
        pub def isLocked(lock: Lock[r]): Bool \ r =
            let Lock(l) = lock;
            unchecked_cast((l.isLocked(): _ \ IO) as _ \ r)
    }

    ///
    /// Returns `true` if and only if `tree` contains the key-value pair `(key, val)`.
    ///
    /// Thread-safe.
    ///
    pub def containsPair(key: k, val: v, tree: BPlusTree[k, v, r]): Bool \ r with Order[k], Eq[v] =
        let leaf = BPlusTree.findLeaf(key, tree);
        let res = Node.leafContainsPair(key, val, leaf);
        Lock.unlock(Node.getLock(leaf));
        res

    ///
    /// Returns `true` if and only if `tree` contains the key-value pair `(key, val)`.
    ///
    /// Not thread-safe.
    ///
    pub def containsPairThreadUnsafe(key: k, val: v, tree: BPlusTree[k, v, r]): Bool \ r with Order[k], Eq[v] =
        let leaf = BPlusTree.findLeafThreadUnsafe(key, tree);
        Node.leafContainsPair(key, val, leaf)

    ///
    /// Returns a fresh empty BPlusTree tree of `order` (the maximum keys/values per node).
    /// The order of the tree must be at least 3.
    ///
    pub def empty(rc: Region[r], order: Int32): BPlusTree[k, v, r] \ r with Order[k] =
        if (order < 3)
            bug!("BPlusTree.empty requires an order more than 2, found '${order}'.")
        else {
            let node = Node.mkLeaf(rc, Array.empty(rc, order), Array.empty(rc, order), 0, None, None);
            new BPlusTree @ rc {
                root = node,
                order = order,
                rootLock = Lock.mkLock(rc),
                rc = rc
            }
        }


    ///
    /// Returns the order of `tree`.
    ///
    pub def order(tree: BPlusTree[k, v, r]): Int32 = tree->order

    ///
    /// Returns the leaf containing `key` and leaves it locked. If no node contains `key` the rightmost
    /// node with values strictly less than `key` is returned.
    ///
    def findLeaf(key: k, tree: BPlusTree[k, v, r]): Node[k, v, r] \ r with Order[k] =
        Node.traverseDown(key, getLockedRoot(tree), tree)

    ///
    /// Returns the leaf containing `key`. If no node contains `key` the rightmost
    /// node with values strictly less than `key` is returned.
    ///
    /// Not thread-safe
    ///
    def findLeafThreadUnsafe(key: k, tree: BPlusTree[k, v, r]): Node[k, v, r] \ r with Order[k] =
        Node.traverseDownThreadUnsafe(key, tree->root)

    ///
    /// Return the root of `tree` and leaves the node locked.
    ///
    @Internal
    def getLockedRoot(tree: BPlusTree[k, v, r]): Node[k, v, r] \ r =
        Lock.lock(tree->rootLock);
        let rootNode = tree->root;
        let success = Lock.tryLock(Node.getLock(rootNode));
        Lock.unlock(tree->rootLock);
        if(success)
            rootNode
        else {
            // Wait for the thread that was operating on the root to finish
            Lock.lock(Node.getLock(rootNode));
            Lock.unlock(Node.getLock(rootNode));
            getLockedRoot(tree)
        }

    ///
    /// Updates the BPlusTree `tree` with the key-value pair `(key, val)`. Replaces any
    /// existing key-value pair.
    ///
    /// Thread-safe.
    ///
    pub def insert(key: k, val: v, tree: BPlusTree[k, v, r]): Unit \ r with Order[k] =
        let leaf = BPlusTree.findLeaf(key, tree);
        let newRoot = Node.insertIntoLeaf(key, val, tree->order, tree->rc, leaf);
        match newRoot {
            case Some(root) =>
                // While this may seem unsafe the splits so far should have ensured that
                // at this point we have a lock on the old root which is still the root
                Lock.lock(tree->rootLock);
                let oldRoot = tree->root;
                tree->root = root;
                Lock.unlock(tree->rootLock);
                Lock.unlock(Node.getLock(oldRoot));
                Lock.unlock(Node.getLock(root));
                ()
            case None => ()
        }

    ///
    /// Returns `true` if and only if `tree` is the empty tree.
    ///
    /// Thread-safe.
    ///
    pub def isEmpty(tree: BPlusTree[k, v, r]): Bool \ r =
        let root = BPlusTree.getLockedRoot(tree);
        let res = Node.isEmpty(root);
        Lock.unlock(Node.getLock(root));
        res

    ///
    /// Returns `true` if and only if `tree` is the empty tree.
    ///
    /// Not thread-safe.
    ///
    pub def isEmptyThreadUnsafe(tree: BPlusTree[k, v, r]): Bool \ r = Node.isEmpty(tree->root)

    ///
    /// Returns `true` if and only if `tree` contains the key `k`.
    ///
    /// Thread-safe.
    ///
    pub def memberOf(key: k, tree: BPlusTree[k, v, r]): Bool \ r with Order[k] =
        let leaf = BPlusTree.findLeaf(key, tree);
        let res = Node.leafContains(key, leaf);
        Lock.unlock(Node.getLock(leaf));
        res

    ///
    /// Returns `true` if and only if `tree` contains the key `k`.
    ///
    /// Not thread-safe.
    ///
    pub def memberOfThreadUnsafe(key: k, tree: BPlusTree[k, v, r]): Bool \ r with Order[k] =
        let leaf = BPlusTree.findLeafThreadUnsafe(key, tree);
        Node.leafContains(key, leaf)

    ///
    /// Applies `f` in ascending order to all key-value pairs `(k, v)` in `tree` where
    /// `min <= k <= max`.
    ///
    /// Not thread-safe.
    ///
    pub def rangeQueryWith(f: k -> v -> Unit \ ef, min: k, max: k, tree: BPlusTree[k, v, r]): Unit \ {r, ef} with Order[k] =
        Node.rangeQueryWith(f, min, max, tree->root)


    ///
    /// Returns true if and only if `tree` is a structurally correct tree.
    /// Only used for internal testing
    ///
    @Internal
    pub def assertTreeInvariant(tree: BPlusTree[k, v, r]): Bool \ r with Order[k] =
        let root = tree->root;
        let order = BPlusTree.order(tree);
        let nodeWorks = Node.assertNodeInvariant(root, order, true, None);
        let rootIsUnlocked = not BPlusTree.Lock.isLocked(tree->rootLock);
        let rootHasNoParent = Option.isEmpty(Node.parent(root));
        nodeWorks and rootHasNoParent and rootIsUnlocked


    ///
    /// Returns a string representation of BPlusTree `tree`.
    ///
    /// Not thread-safe
    ///
    pub def toString(tree: BPlusTree[k, v, r]): String \ r with ToString[k], ToString[v] =
        Node.toString(0, tree->rc, tree->root)

    ///
    /// The Node struct defines the tree, having either children or leaves depending on
    /// whether the node is a leaf or internal. The following invariants hold for Node:
    /// `parent` is `None` for the root and `Some(p)` for all other nodes where `p` is the
    /// parent node. `next` is `None` for internal nodes and the rightmost leaf node and
    /// `Some(p)` for all leaf-nodes where `p` is the node directly to the right. `isLeaf`
    /// describes whether a `Node` is a leaf or an internal node.
    ///
    /// If `isLeaf == true`
    ///
    ///     `children` is of size 0
    ///
    ///     `keys` and `values` have the same size (which should be the `order` of
    ///     the tree)
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
    ///      `keys[i]` store the smallest key in the subtree `children[i+1]`
    ///
    ///      `size` is the number of children currently stored in the leaf
    ///
    /// Lock invariants:
    ///
    ///     `lock` is the lock associated with the node. When it is held no other thread
    ///     may modify the contents The only exception to the previous rule is that the
    ///     `parent` pointer is owned by the `lock` on the parent For the root node with
    ///     parent None the parent pointer is owned by the `rootLock` on the tree. When
    ///     going up the tree locks are forcefully acquired. When going down the tree if
    ///     a lock cannot be acquired immediately all current locks will be released and
    ///     the attempt restarted after the wanted lock has been released by the holder.
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
        use BPlusTree.Node.traverseDownThreadUnsafe
        import java.lang.Object
        import java.util.Objects
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
                node: Node[k, v, r]): Unit \ r with Order[k] =
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
        /// Returns true if and only if the elements of `v` are sorted.
        /// Only used for internal testing.
        ///
        def isSorted(v: Vector[k]): Bool with Order[k] =
            applyPredicate(v, x -> y -> x <= y)

        ///
        /// Slices `v` to the elements from 0 (inclusive) to `length` (exclusive).
        /// Only used for internal testing.
        ///
        def toLength(v: Vector[v], length: Int32): Vector[v] =
            Vector.slice(start = 0, end = length, v)

        ///
        /// Returns true if and only if the node is structurally correct.
        /// Only used for internal testing.
        ///
        @Internal
        pub def assertNodeInvariant(node: Node[k, v, r], order: Int32, isRightMost: Bool, shouldBeBiggerThan: Option[k]): Bool \ r with Order[k] =
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
                    let keysInOrder = isSorted(keys);
                    let firstChildWorks = assertNodeInvariant(Vector.get(0, children), order, false, shouldBeBiggerThan);
                    let lastChildWorks = assertNodeInvariant(unwrapNode(Vector.last(children)), order, isRightMost, if(size == 0) shouldBeBiggerThan else Vector.last(keys));
                    let zippedWithoutFirst = Vector.filter(match (index, _) -> index != 0, Vector.mapWithIndex(i -> x -> (i, x), toLength(children, size - 1)));
                    let middleChildrenWorks = Vector.forAll(pair ->
                        let (i, p) = pair;
                        assertNodeInvariant(p, order, false, Some(Vector.get(i - 1, keys))), zippedWithoutFirst);
                    let childrenWorks = firstChildWorks and middleChildrenWorks and lastChildWorks;
                    let parentPointerCorrect = Vector.foldLeft(
                        workingSoFar -> cur -> workingSoFar and equalByReference(node, unwrapNode(cur->parent)),
                        true, children);
                    let nextPointerCorrect = not unwrapNode(Vector.head(children))->isLeaf or applyPredicate(children, prev -> cur -> equalByReference(unwrapNode(prev->next), cur));
                    let lastHasNoNext = not isRightMost or Option.isEmpty(unwrapNode(Vector.last(children))->next);
                    let noLeaves = Array.size(node->values) == 0;
                    let nodeIsUnlocked = not BPlusTree.Lock.isLocked(node->lock);
                    let isBiggerThanPrev = match shouldBeBiggerThan {
                        case None => true
                        case Some(v) => Vector.get(0, keys) > v
                    };
                    parentPointerCorrect and nextPointerCorrect and
                        lastHasNoNext and noLeaves and keysInOrder and
                        childrenWorks and nodeIsUnlocked and isBiggerThanPrev
                case true =>
                    let size = node->size;
                    let keys = toLength(Array.toVector(node->keys), size);
                    let keysInOrder = isSorted(toLength(keys, size));
                    let sizeInRange = size <= order and 0 <= size;
                    let noChildren = Array.size(node->children) == 0;
                    let leafIsUnlocked = not BPlusTree.Lock.isLocked(node->lock);
                    sizeInRange and keysInOrder and leafIsUnlocked and noChildren
        }

        /// Provides access to the `lock` field of `node`.
        pub def getLock(node: Node[k, v, r]): Lock[r] = node->lock

        ///
        /// Returns `true` if `node` is empty.
        ///
        /// Thread safe if and only if the caller has a lock on `node`.
        ///
        pub def isEmpty(node: Node[k, v, r]): Bool \ r = node->size == 0

        ///
        /// Insert key-value pair `(key, val)` into the leaf `node`, potentially causing a
        /// split and returning a new root.
        ///
        /// Lock invariants:
        ///
        /// - On invocation, the thread has a lock on `node`.
        ///
        /// - On return, the thread has no lock on the tree if the returned value is None
        ///     Otherwise it has a lock on the old root and `p` where `Some(p)` is returned
        ///
        pub def insertIntoLeaf(
                key: k,
                val: v,
                order: Int32,
                rc: Region[r],
                node: Node[k, v, r]): Option[Node[k, v, r]] \ r with Order[k] = {
            let size = node->size;
            let index = binarySearch(key, size, node->keys);
            if (index >= 0) {
                Array.put(val, index, node->values);
                Lock.unlock(node->lock);
                None
            } else
                let insertionPoint = toInsertionPoint(index);
                if (size < order) {
                    actualInsertLeaf(key, val, insertionPoint, node);
                    Lock.unlock(node->lock);
                    None
                } else
                    splitLeaf(node, key, val, insertionPoint, order, rc)
        }

        ///
        /// Returns `true` if the mapping `key` is contained in the leaf `node`.
        ///
        pub def leafContains(key: k, node: Node[k, v, r]): Bool \ r with Order[k] =
            let index = binarySearch(key, node->size, node->keys);
            if (index < 0)
                false
            else
                true

        ///
        /// Returns `true` if the mapping `(key, val)` is contained in the leaf `node`.
        ///
        pub def leafContainsPair(key: k, val: v, node: Node[k, v, r]): Bool \ r with Order[k], Eq[v] =
            let index1 = binarySearch(key, node->size, node->keys);
            if (index1 < 0)
                false
            else
                Array.get(index1, node->values) == val

        ///
        /// Attempt to lock the parent of `node`.
        ///
        /// Lock invariants:
        ///
        /// - On invocation, the thread has a lock on `node`.
        ///
        /// - On return, the thread holds a lock on `node` and on `parent` given by
        ///   `node->Some(parent)`
        ///
        def lockParent(node: Node[k, v, r]): Unit \ r = match node->parent {
            // The access `node->parent` on the previous line is not safe with respect to concurrent editing.
            // There are 2 cases:
            // `node->parent` is none in which case the node is the root. Since the root's
            // parent is only edited while holding the lock on the root we must have a happens before relation
            // with any other change to the roots parent pointer.
            // `node->parent` is Some(parent) in which case we might have read a stale value. The thread that
            // Updated `node->parent` held a lock on both the old and new parent. We therefore grab a lock on
            // parent which forces `node->parent` to then be synchronized with all threads that could have
            // changed `node->parent`. Afterwards we check whether the pointer has been changed.
            case Some(parent) =>
                let parentLock = parent->lock;
                // After acquiring the lock on the parent in the following step we should
                // be synchronized with the last thread that held the lock. We should
                // therefore now be able to see every change they have made before
                // releasing the lock. Reading the parent-pointer afterwards is therefore
                // safe with respect to changes to the old pointer.
                Lock.lock(parentLock);
                // If some thread had a lock on your parent and changed your parent pointer retry.
                let possiblyNewParent = match node->parent {
                    case Some(v) => v
                    case None => bug!("Cannot happen (when there are no deletions). Parent pointer replaced by None")
                };
                let isSame = equalByReference(possiblyNewParent, parent);
                if (not isSame) {
                    Lock.unlock(parentLock);
                    lockParent(node)
                } else ()
            case None => ()
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
                next: Option[Node[k, v, r]]): Node[k, v, r] \ r =
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
                parent: Option[Node[k, v, r]]): Node[k, v, r] \ r =
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
        /// - On invocation, the thread has a lock on `parent`, `leftChild` and `rightChild`.
        ///
        /// - On return, the thread only holds a lock if the return value is Some. In
        /// that case it holds a lock on the new root and the old root.
        def pushKeyToParent(
                parent: Option[Node[k, v, r]],
                leftChild: Node[k, v, r],
                rightChild: Node[k, v, r],
                newKey: k,
                order: Int32,
                rc: Region[r]): Option[Node[k, v, r]] \ r with Order[k] = match parent {
            case Some(node) =>
                let children = node->children;
                let insertionPoint = toInsertionPoint(binarySearch(newKey, node->size - 1, node->keys));
                let size = node->size;
                if(size < order) {
                    arrayInsert(newKey, insertionPoint, size - 1, node->keys);
                    arrayInsert(rightChild, insertionPoint + 1, size, children);
                    node->size = size + 1;
                    Lock.unlock(leftChild->lock);
                    Lock.unlock(rightChild->lock);
                    Lock.unlock(node->lock);
                    None
                } else {
                    Lock.unlock(leftChild->lock);
                    splitInternal(node, rightChild, insertionPoint, newKey, order, rc)
                }
            case None =>
                // In this case, leftChild is the root of the tree
                // and we must construct a new root node
                let children = Array.empty(rc, order);
                let keys = Array.empty(rc, order - 1);
                Array.put(leftChild, 0, children);
                Array.put(rightChild, 1, children);
                Array.put(newKey, 0, keys);
                let newRoot = mkInternal(rc, keys, children, 2, None);
                Lock.lock(newRoot->lock);
                leftChild->parent = Some(newRoot);
                rightChild->parent = Some(newRoot);
                Lock.unlock(rightChild->lock);
                Some(newRoot)
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
                node: Node[k, v, r1]): Unit \ r0 + r1 with Order[k] =
            let minLeaf = traverseDownThreadUnsafe(min, node);
            let index = binarySearch(min, minLeaf->size, minLeaf->keys);
            if(index < 0) {
                let insertionPoint = toInsertionPoint(index);
                if (insertionPoint <= minLeaf->size)
                    traverseRight(f, insertionPoint, max, minLeaf)
                    else ()
            } else
                traverseRight(f, index, max, minLeaf)

        ///
        /// Split `leftLeaf` into two leaves and push the middle key into the parent,
        /// potentially causing the parent to split and return a new root. Should only
        /// be called when `leftLeaf->size == order` and
        /// `0 <= insertionPoint <= order + 1`.
        ///
        /// Lock invariants:
        ///
        /// - On invocation, the thread has a lock on `leftNode`.
        ///
        /// - On return, the thread only holds a lock if the return value is Some. In
        /// that case it holds a lock on the new root and the old root.
        def splitLeaf(
                leftLeaf: Node[k, v, r],
                key: k,
                val: v,
                insertionPoint: Int32,
                order: Int32,
                rc: Region[r]): Option[Node[k, v, r]] \ r with Order[k] =
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
            Lock.lock(rightLeaf->lock);
            lockParent(leftLeaf);
            rightLeaf->parent = leftLeaf->parent;
            pushKeyToParent(leftLeaf->parent, leftLeaf, rightLeaf, Array.get(0, rightKeys), order, rc)


        ///
        /// Split `leftNode` into two nodes with `newKey` inserted into the appropriate
        /// node depending on `insertionPoint`.
        ///
        /// Lock invariants:
        ///
        /// - On invocation, the thread has a lock on `leftNode`.
        ///
        /// - On return, the thread only holds a lock if the return value is Some. In
        /// that case it holds a lock on the new root and the old root.
        def splitInternal(
                leftNode: Node[k, v, r],
                insertedChild: Node[k, v, r],
                insertionPoint: Int32,
                newKey: k,
                order: Int32,
                rc: Region[r]): Option[Node[k, v, r]] \ r with Order[k] =
            Lock.unlock(insertedChild->lock);
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
            Lock.lock(rightNode->lock); // pushKeyToParent expects a lock on the parent
            lockParent(leftNode);
            rightNode->parent = leftNode->parent;
            pushKeyToParent(leftNode->parent, leftNode, rightNode, keyOfRight, order, rc)

        ///
        /// Moves elements of `leftLeaf->keys` and `leftLeaf->values` from `mid` (inclusive) to `order` (exclusive)
        /// to the returned arrays, respectively.
        ///
        def splitToRightLeaf(rc: Region[r], mid: Int32, order: Int32, leftLeaf: Node[k, v, r]): (Array[k, r], Array[v, r]) \ r = {
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
                                    leftNode: Node[k, v, r]): (k, Array[k, r], Array[Node[k, v, r], r]) \ r = {
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
                                    leftNode: Node[k, v, r]): (k, Array[k, r], Array[Node[k, v, r], r]) \ r = {
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
                    let keysString = Array.join(",", Array.copyOfRange(rc, 0, node->size - 1, node->keys));
                    let childrenArr = Array.copyOfRange(rc, 0, node->size, children);
                    let childrenString = Array.foldLeft(a -> x -> "${a}${toString(indent+2, rc, x)}", "", childrenArr);
                    "${indentString}Internal(${keysString})\n${childrenString}"
                case true =>
                    let values = Array.copyOfRange(rc, 0, node->size, node->values);
                    let keys = Array.copyOfRange(rc, 0, node->size, node->keys);
                    let keysStrings = Array.mapWithIndex(rc, i -> key -> "${key}: ${Array.get(i, values)}", keys);
                    let keysString = Array.join("${indentString}  ,\n", keysStrings);
                    "${indentString}Leaf(\n${indentString}  ${keysString}\n\${indentString})\n"
            }

        ///
        /// Traverse down through the tree while keeping the invariant that there is a
        /// `lock` on `cur`. If acquiring a lock at any point fails release the locks you
        /// hold and retry from the root.
        ///
        pub def traverseDown(key: k, cur: Node[k, v, r], tree: BPlusTree[k, v, r]): Node[k, v, r] \ r with Order[k] =
            match cur->isLeaf {
                case false =>
                    let children = cur->children;
                    let index = binarySearch(key, cur->size - 1, cur->keys);
                    let childToVisit = if (index < 0)
                        Array.get(toInsertionPoint(index), children)
                    else
                        Array.get(index + 1, children);
                    // Try to get a read-lock on the next child
                    let childLock = childToVisit->lock;
                    let success = Lock.tryLock(childLock);
                    Lock.unlock(cur->lock);
                    if(success)
                        // Unlock the current node i.e. the parent of `childToVisit`
                        traverseDown(key, childToVisit, tree)
                    else {
                        // Give up and restart the attempt.
                        // To allow for the potential writer we encountered to grab the lock
                        // on the node lock the child node as a wait operation.
                        Lock.lock(childLock);
                        Lock.unlock(childLock);
                        BPlusTree.findLeaf(key, tree)
                    }
                case true => cur
            }

        ///
        /// Returns the leaf containing `key`. If no node contains `key` the rightmost
        /// node with values strictly less than `key` is returned.
        ///
        /// Equivalent to `traverseDown`, but is not thread-safe.
        ///
        pub def traverseDownThreadUnsafe(key: k, node: Node[k, v, r]): Node[k, v, r] \ r with Order[k] =
            match node->isLeaf {
                case false =>
                    let children = node->children;
                    let index = binarySearch(key, node->size - 1, node->keys);
                    let childToVisit = if (index < 0)
                        Array.get(toInsertionPoint(index), children)
                    else
                        Array.get(index + 1, children);
                    traverseDownThreadUnsafe(key, childToVisit)
                case true => node
            }

        ///
        /// Applies `f` to key-value pairs `(k, v)` starting at
        /// `node->keys[index]` until `node->keys[index] > max`. Uses the `next` pointer
        /// to traverse to the next node.
        ///
        /// Not thread-safe.
        ///
        def traverseRight(
                f: k -> v -> Unit \ r0,
                index: Int32,
                max: k,
                node: Node[k, v, r1]): Unit \ r0 + r1 with Order[k] =
            if(index < node->size) {
                let k = Array.get(index, node->keys);
                if(k <= max) {
                    f(k, Array.get(index, node->values));
                    traverseRight(f, index + 1, max, node)
                } else ()
            } else match node->next {
                case Some(next) => traverseRight(f, 0, max, next)
                case None => ()
            }

        ///
        /// Insert `x` at position `i` in array `arr` as though it currently has `len`
        /// elements. Shifts elements at position `[i, .., len]` to `[i+1, .., len+1]`.
        ///
        def arrayInsert(x: a, i: Int32, len: Int32, arr: Array[a, r]): Unit \ r =
            if (len == i)
                Array.put(x, i, arr)
            else
                Array.copyInto(srcPos = i, dstPos = i + 1, len = len - i, src = arr, arr);
                Array.put(x, i, arr)

        ///
        /// Perform a binary search and return the index of `key` in `arr` if it exists.
        /// Otherwise, return `(-insertionPoint - 1)` where `insertionPoint` is the index
        /// where `key` would be inserted.
        ///
        def binarySearch(key: v, length: Int32, arr: Array[v, r]): Int32 \ r with Order[v] =
            if (length == 0)
                -1
            else
                def f(l: Int32, r: Int32): Int32 \ r = {
                    if (l <= r - 1) {
                        let m = (l + r) / 2;
                        match Array.get(m, arr) <=> key {
                            case Comparison.LessThan => f(m + 1, r)
                            case Comparison.GreaterThan => f(l, m)
                            case Comparison.EqualTo => m
                        }
                    } else -l-1
                };
                f(0, length)

        ///
        /// Returns `true` if the pointer of `o1` is equal to the pointer of `o2`.
        ///
        def equalByReference(o1: v, o2: v): Bool =
            // Unchecked cast is here used to transform the values to Object.
            // The purpose is to be able to give them to a Java class which performs pointer comparison.
            let obj1 = unchecked_cast(o1 as Object);
            let obj2 = unchecked_cast(o2 as Object);
            // Unchecked cast is here used to remove the IO effect from interacting with Java.
            unchecked_cast((Objects.equals(obj1, obj2): _ \ IO) as _ \ {})

        ///
        /// Returns the real insertion point of the result of binarySearch, assuming that
        /// the result of binarySearch was negative.
        ///
        def toInsertionPoint(i: Int32): Int32 = -i-1
    }
}