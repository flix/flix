///
/// An immutable concurrent B+ Tree implementation with
/// keys of type `k` and values of type `v`.
///
/// A B+ Tree is an m-ary self-balancing search tree where each node
/// is either an internal node or a leaf node. In contrast to binary
/// search trees, a B+ Tree of order `m` allows for nodes with at most
/// `m` children and `m-1` keys. A B+ Tree maintains the sorted order
/// of its keys by storing only keys in internal nodes and additionally
/// values in leaf nodes. This implementation varies from the standard
/// definition by also linking internal nodes. This is due to a use-case
/// within Flix.
///
/// See below for more details:
/// - The Ubiquitous B-Tree (1979) by Douglas Comer
///   (https://dl.acm.org/doi/10.1145/356770.356776)
///
///
/// A BPlusTree satisfies the following.
///
/// `root` is the root node of the tree, which upon creation of the
/// tree is a leaf node. Through insertion into the tree, a new node may
/// be created and propogated up to become the new root of the tree.
///
/// `order` is the order of the tree which determines the maximum
/// number of children a node may have.
///
/// `rootLock` is a lock on the `root` node which a thread must first
/// acquire in order to insert into the tree. As the thread descends into
/// the tree, `rootLock` is released to allow for other threads to insert.
///
/// `rc` is the region for the tree which is required for a mutable object.
///
pub struct BPlusTree[k, v, r] {
    mut root: BPlusTree.Node[k, v, r],
    order:    Int32, // The maximum number of children a node can have
    rootLock: BPlusTree.Lock[r],
    rc:       Region[r]
}

mod BPlusTree {
    import java.util.concurrent.locks.{ReentrantLock => JReentrantLock}

    ///
    /// The purpose of Lock is to provide a thin wrapper around Java's ReentrantLock without the IO effect
    /// Only one thread can hold a lock at any given time, but calling lock on the same lock multiple times
    /// only means that the holder must call unlock the same number of times before other threads can acquire the lock
    ///
    @Internal
    pub enum Lock[_: Region](JReentrantLock)

    mod Lock {
        import java.util.concurrent.locks.{ReentrantLock => JReentrantLock}
        use BPlusTree.Lock

        // We use unchecked_casts here to remove the IO effect. It is replaced
        // with an effect to a region to ensure that the code is not optimized away.

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
        ///
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
        let getRoot = () -> root(tree);
        let leaf = Node.findLeaf(key, getRoot);
        let res = Node.leafContainsPair(key, val, leaf);
        Lock.unlock(Node.lock(leaf));
        res

    ///
    /// Returns `true` if and only if `tree` contains the key-value pair `(key, val)`.
    ///
    /// Not thread-safe.
    ///
    pub def containsPairUnsafe(key: k, val: v, tree: BPlusTree[k, v, r]): Bool \ r with Order[k], Eq[v] =
        let leaf = Node.findLeafUnsafe(key, tree->root);
        Node.leafContainsPair(key, val, leaf)

    ///
    /// Returns a fresh empty B+ tree of with nodes of size `order`.
    /// The order of the tree must be at least 3.
    ///
    pub def empty(rc: Region[r], order: Int32): BPlusTree[k, v, r] \ r with Order[k] =
        if (order < 3)
            bug!("order of tree must be at least 3")
        else
            let node = new Node @ rc {
                keys = Array.empty(rc, order),
                children = Array.empty(rc, 0),
                values = Array.empty(rc, order),
                lock = Lock.mkLock(rc),
                size = 0,
                parent = None,
                next = None,
                isLeaf = true
            };
            new BPlusTree @ rc {
                root = node,
                order = order,
                rootLock = Lock.mkLock(rc),
                rc = rc
            }

    ///
    /// Returns the lock of the root pointer of `tree`.
    ///
    pub def lock(tree: BPlusTree[k, v, r]): Lock[r] = tree->rootLock

    ///
    /// Returns the order of `tree`.
    ///
    pub def order(tree: BPlusTree[k, v, r]): Int32 = tree->order

    ///
    /// Return the root of `tree` by acquiring the root lock.
    ///
    def root(tree: BPlusTree[k, v, r]): Node[k, v, r] \ r =
        Lock.lock(tree->rootLock);
        let rootNode = tree->root;
        let success = Lock.tryLock(Node.lock(rootNode));
        Lock.unlock(tree->rootLock);
        if(success)
            rootNode
        else {
            // Wait for the thread that was operating on the root to finish
            Lock.lock(Node.lock(rootNode));
            Lock.unlock(Node.lock(rootNode));
            root(tree)
        }

    ///
    /// Returns the root of `tree`.
    ///
    /// Not thread-safe.
    ///
    pub def getRootUnsafe(tree: BPlusTree[k, v, r]): Node[k, v, r] \ r =  tree->root

    ///
    /// Updates the B+ tree `tree` with the key-value pair `(key, val)`. Replaces any existing key-value pair.
    ///
    /// Thread-safe.
    ///
    pub def insert(key: k, val: v, tree: BPlusTree[k, v, r]): Unit \ r with Order[k] =
        let getRoot = () -> root(tree);
        let leaf = Node.findLeaf(key, getRoot);
        let newRoot = Node.insertIntoLeaf(key, val, tree->order, tree->rc, leaf);
        match newRoot {
            case Some(root) =>
                // While this may seem unsafe the splits so far should have ensured that
                // at this point we have a lock on the old root which is still the root
                Lock.lock(tree->rootLock);
                let oldRoot = tree->root;
                tree->root = root;
                Lock.unlock(tree->rootLock);
                Lock.unlock(Node.lock(oldRoot));
                Lock.unlock(Node.lock(root));
                ()
            case None => ()
        }

    ///
    /// Returns `true` if and only if `tree` is the empty tree.
    ///
    /// Thread-safe.
    ///
    pub def isEmpty(tree: BPlusTree[k, v, r]): Bool \ r = match Node.isLeaf(tree->root) {
        case false => false
        case true => let root =
            BPlusTree.root(tree);
            let res = Node.isEmpty(root);
            Lock.unlock(Node.lock(root));
            res
    }

    ///
    /// Returns `true` if and only if `tree` is the empty tree.
    ///
    /// Not thread-safe.
    ///
    pub def isEmptyUnsafe(tree: BPlusTree[k, v, r]): Bool \ r = match Node.isLeaf(tree->root) {
        case false => false
        case true => Node.isEmpty(tree->root)
    }

    ///
    /// Returns `true` if and only if `tree` contains the key `k`.
    ///
    /// Thread-safe.
    ///
    pub def memberOf(key: k, tree: BPlusTree[k, v, r]): Bool \ r with Order[k] =
        let getRoot = () -> root(tree);
        let leaf = Node.findLeaf(key, getRoot);
        let res = Node.leafContains(key, leaf);
        Lock.unlock(Node.lock(leaf));
        res

    ///
    /// Returns `true` if and only if `tree` contains the key `k`.
    ///
    /// Not thread-safe.
    ///
    pub def memberOfUnsafe(key: k, tree: BPlusTree[k, v, r]): Bool \ r with Order[k] =
        let leaf = Node.findLeafUnsafe(key, tree->root);
        Node.leafContains(key, leaf)

    ///
    /// Applies `f` to all key-value pairs from `tree` with keys between `min` and `max`.
    ///
    pub def rangeQueryWith(f: k -> v -> Unit \ ef, min: k, max: k, tree: BPlusTree[k, v, r]): Unit \ {r, ef} with Order[k] =
        Node.rangeQueryWith(f, min, max, tree->root)

    ///
    /// Returns region associated with `tree`.
    ///
    pub def rc(tree: BPlusTree[k, v, r]): Region[r] =  tree->rc

    ///
    /// Returns a string representation of BPlusTree `tree`.
    ///
    pub def toString(tree: BPlusTree[k, v, r]): String \ r with ToString[k], ToString[v] =
        Node.toString(0, tree->rc, tree->root)

    ///
    /// The Node struct defines the tree, having either children or leaves depending on whether the node is a leaf or internal.
    /// The following invariants hold for Node:
    /// `parent` is `None` for the root and `Some(p)` for all other nodes where `p` is the parent node.
    /// `next` is `None` for the rightmost nodes of the tree and `Some(p)` for all other nodes where `p` is the node directly to the right.
    /// `isLeaf` describes whether a `Node` is a leaf or an internal node.
    ///     If `isLeaf == true`
    ///         `children` is of size 0
    ///         `keys` and `values` have the same size (which should be the `order` of the tree)
    ///         `keys[i]` should store the key for the value stored at `values[i]`
    ///         `size` is the number of keys/values currently stored in the leaf
    ///     If `isLeaf == false`
    ///         `values` is of size 0
    ///         `children` is of size `order` and `keys` have size `order - 1`
    ///         `keys[i]` store the smallest value in the subtree store in `children[i+1]`
    ///         `size` is the number of children currently stored in the leaf
    /// Lock invariants:
    ///     `lock` is the lock associated with the node. When it is held no other thread may modify the contents
    ///     The only exception to the previous rule is that the `parent` pointer is owned by the `lock` on the parent

    pub struct Node[k, v, r] {
        keys:       Array[k, r], // Acts as keys for internal nodes and values for leaves
        children:   Array[Node[k, v, r], r],
        values:     Array[v, r],
        lock:       Lock[r],
        mut size:   Int32,      // Number of children / values
        mut parent: Option[Node[k, v, r]],
        mut next:   Option[Node[k, v, r]],
        isLeaf:     Bool
    }

    mod Node {
        import java.lang.Object
        import java.util.Objects
        use BPlusTree.Lock

        // Accessor methods
        /// Provides access to the `children` field of `node`
        pub def children(node: Node[k, v, r]): Array[Node[k, v, r], r]  = node->children
        /// Provides access to the `isLeaf` field of `node`
        pub def isLeaf(node: Node[k, v, r]): Bool  = node->isLeaf
        /// Provides access to the `keys` field of `node`
        pub def keys(node: Node[k, v, r]): Array[k, r] = node->keys
        /// Provides access to the `lock` field of `node`
        pub def lock(node: Node[k, v, r]): Lock[r] = node->lock
        /// Provides access to the `next` field of `node`
        pub def next(node: Node[k, v, r]): Option[Node[k, v, r]] \ r = node->next
        /// Provides access to the `parent` field of `node`
        pub def parent(node: Node[k, v, r]): Option[Node[k, v, r]] \ r = node->parent
        /// Provides access to the `size` field of `node`
        pub def size(node: Node[k, v, r]): Int32 \ r  = node->size
        /// Provides access to the `values` field of `node`
        pub def values(node: Node[k, v, r]): Array[v, r]  = node->values

        ///
        /// Returns a string representation of the given Node `node` starting with `indent` spaces.
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
        /// Returns the leaf containing `key`. If no node contains `key` the first node with values less than `key` is returned.
        /// Equivalent to `findLeaf` but is not thread-safe.
        ///
        pub def findLeafUnsafe(key: k, node: Node[k, v, r]): Node[k, v, r] \ r with Order[k] =
            match node->isLeaf {
                case false =>
                    let children = node->children;
                    let index = binarySearch(key, node->size - 1, node->keys);
                    let childToVisit = if (index < 0)
                        Array.get(toInsertionPoint(index), children)
                    else
                        Array.get(index + 1, children);
                    findLeafUnsafe(key, childToVisit)
                case true => node
            }

        ///
        /// Returns the leaf containing `key`. If no node contains `key` the first node with values less than `key` is returned
        /// `getRoot` should provide a safe way to get the root of the tree such that the lock on the root is held when it returns
        /// The method is thread-safe and provides a lock on the returned value.
        ///
        pub def findLeaf(key: k, getRoot: Unit -> Node[k, v, r]  \ r): Node[k, v, r] \ r with Order[k] =
            /// Traverse the tree while keeping the invariant that there is a `lock` on `cur`.
            /// If aquiring a lock at any point fails release the locks you hold and retry from the top.
            def traverse(cur: Node[k, v, r]): Node[k, v, r] \ r =
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
                            traverse(childToVisit)
                        else {
                            // Give up and restart the attempt.
                            // To allow for the potential writer we encountered to grab the lock
                            // on the node lock the child node as a wait operation.
                            Lock.lock(childLock);
                            Lock.unlock(childLock);
                            traverse(getRoot())
                        }
                    case true => cur
                };
            traverse(getRoot())

        ///
        /// Returns `true` if `node` is empty.
        ///
        pub def isEmpty(node: Node[k, v, r]): Bool \ r = node->size == 0

        ///
        /// Returns `true` if the mapping `key` is contained in the leaf `node`.
        ///
        pub def leafContains(key: k, node: Node[k, v, r]): Bool \ r with Order[k] =
            match node->isLeaf {
                case true =>
                    let index = binarySearch(key, node->size, node->keys);
                    if (index < 0)
                        false
                    else
                        true
                case false => bug!("The contains method on nodes should only be used on leaves")
            }

        ///
        /// Returns `true` if the mapping `(key, val)` is contained in the leaf `node`.
        ///
        pub def leafContainsPair(key: k, val: v, node: Node[k, v, r]): Bool \ r with Order[k], Eq[v] =
            match node->isLeaf {
                case true =>
                    let index1 = binarySearch(key, node->size, node->keys);
                    if (index1 < 0)
                        false
                    else
                        Array.get(index1, node->values) == val
                case false => bug!("The contains method on nodes should only be used on leaves")
            }

        ///
        /// Internal method to insert `key` and `val` in `node` at `insertionPoint`.
        /// Increments the `size` of `node`.
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
        /// Insert key-value pair `(key, val)` into the leaf `node`,
        /// potentially causing a split and returning a new root
        ///
        /// Lock invariants:
        /// - On invocation, the thread has a lock on `node`.
        /// - On return, the thread only holds a lock on `Some(node)`
        ///
        pub def insertIntoLeaf(
                key: k,
                val: v,
                order: Int32,
                rc: Region[r],
                node: Node[k, v, r]): Option[Node[k, v, r]] \ r with Order[k] =
            let size = node->size;
            let index = binarySearch(key, size, node->keys);
            if (index >= 0)
                let _ = Array.put(val, index, node->values);
                Lock.unlock(node->lock);
                None
            else
                let insertionPoint = toInsertionPoint(index);
                if (size < order) {
                    actualInsertLeaf(key, val, insertionPoint, node);
                    Lock.unlock(node->lock);
                    None
                } else
                    splitLeaf(node, key, val, insertionPoint, order, rc)

        ///
        /// Attempt to lock the parent of `node`.
        ///
        /// Lock invariants:
        /// - On invocation, the thread has a lock on `node`.
        /// - On return, the thread holds a lock on `node` and on `parent` given by `node->Some(parent)`
        def lockParent(node: Node[k, v, r]): Unit \ r = match node->parent {
            case Some(parent) =>
                let parentLock = parent->lock;
                Lock.lock(parentLock);
                // If some thread had a lock on your parent and changed your parent pointer retry
                let possiblyNewParent = match node->parent {
                    case Some(v) => v
                    case None => bug!("Cannot happen (when there are no deletions). Parent pointer replaced by None")
                };
                let isSame = ptrEqual(possiblyNewParent, parent);
                if (not isSame) {
                    Lock.unlock(parentLock);
                    lockParent(node)
                } else ()
            case None => ()
        }

        ///
        /// Split `leftLeaf` into two leaves and push the middle key into the parent,
        /// potentially causing the parent to split and return a new root
        ///
        /// Lock invariants:
        /// - On invocation, the thread has a lock on `leftNode`.
        /// - On return, the thread only holds a lock if the return value is Some.
        ///   In that case it holds a lock on the new root and the old root.
        def splitLeaf(
                leftLeaf: Node[k, v, r],
                key: k,
                val: v,
                insertionPoint: Int32,
                order: Int32,
                rc: Region[r]): Option[Node[k, v, r]] \ r with Order[k] =
            let prev_mid = order / 2;
            let (mid, insertInLeft) = if (insertionPoint <= prev_mid) (prev_mid, true) else (prev_mid + 1, false);
            let rightKeys = Array.empty(rc, order);
            let rightValues = Array.empty(rc, order);
            Array.copyInto(srcPos = mid, dstPos = 0, len = order - mid, src = leftLeaf->keys, rightKeys);
            Array.copyInto(srcPos = mid, dstPos = 0, len = order - mid, src = leftLeaf->values, rightValues);
            // Create a new leaf to store the right half
            let rightLeaf = new Node @ rc {
                keys = rightKeys,
                children = Array.empty(rc, 0),
                values = rightValues,
                lock = Lock.mkLock(rc),
                size = leftLeaf->size - mid,
                parent = leftLeaf->parent,
                next = leftLeaf->next,
                isLeaf = true
            };
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
        /// Split `leftNode` into two nodes with `newKey` inserted into the appropriate node
        /// depending on `insertionPoint`.
        ///
        /// Lock invariants:
        /// - On invocation, the thread has a lock on `parent`.
        /// - On return, the thread only holds a lock if the return value is Some.
        ///   In that case it holds a lock on the new root and the old root.
        def splitInternal(
                leftNode: Node[k, v, r],
                insertedChild: Node[k, v, r],
                insertionPoint: Int32,
                newKey: k,
                order: Int32,
                rc: Region[r]): Option[Node[k, v, r]] \ r with Order[k] =
            Lock.unlock(insertedChild->lock);
            let prev_mid = order / 2;
            let (mid, insertInLeft, rightSize) = if (insertionPoint >= prev_mid) (prev_mid + 1, false, order - prev_mid) else (prev_mid, true, order - prev_mid);
            let rightKeys = Array.empty(rc, order - 1);
            let rightChildren = Array.empty(rc, order);
            leftNode->size = mid;
            let keyOfRight = if (insertInLeft) {
                let keyToPass_ = Array.get(mid - 1, leftNode->keys);
                let leftChildren = leftNode->children;
                let leftKeys = leftNode->keys;
                // No problems. Just copy everything from mid and forward. Finish by inserting the new element in old node.
                Array.copyInto(srcPos = mid, dstPos = 0, len = rightSize - 1, src = leftKeys, rightKeys);
                Array.copyInto(srcPos = mid, dstPos = 0, len = rightSize, src = leftChildren, rightChildren);
                arrayInsert(insertedChild, insertionPoint + 1, mid, leftChildren);
                arrayInsert(newKey, insertionPoint, mid, leftKeys);
                leftNode->size = mid + 1;
                keyToPass_
            } else {
                // Insert the new node in the new right node and copy the relevant elements of the leftNode (index>=mid).
                let rightSizeWithoutNew = rightSize - 1;
                if(insertionPoint + 1 == mid) {
                    // If insertionPoint is the first element copy all old keys and old elements and insert new child at 0.
                    Array.copyInto(srcPos = mid - 1, dstPos = 0, len = rightSizeWithoutNew, src = leftNode->keys, rightKeys);
                    Array.copyInto(srcPos = mid, dstPos = 1, len = rightSizeWithoutNew, src = leftNode->children, rightChildren);
                    Array.put(insertedChild, 0, rightChildren);
                    newKey
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
                    Array.get(mid - 1, leftNode->keys)
                }
            };
            let rightNode = new Node @ rc {
                keys = rightKeys,
                children = rightChildren,
                values = Array.empty(rc, 0),
                lock = Lock.mkLock(rc),
                size = rightSize,
                parent = leftNode->parent,
                next = leftNode->next,
                isLeaf = false
            };
            leftNode->next = Some(rightNode);
            Array.forEachWithIndex(i -> child ->
                if (i < rightSize)
                    child->parent = Some(rightNode)
                else (), rightChildren);
            Lock.lock(rightNode->lock); // pushKeyToParent expects a lock on the parent
            lockParent(leftNode);
            rightNode->parent = leftNode->parent;
            pushKeyToParent(leftNode->parent, leftNode, rightNode, keyOfRight, order, rc)

        ///
        /// Push the key `newKey` to `parent`.
        /// If `parent` is `Some(node)`, insert `newKey` into node if there is room. Otherwise split
        /// `node` and return a new root node.
        ///
        /// If `parent` is `None`, create a new root node, insert `newKey` into it and return it.
        ///
        /// Lock invariants:
        /// - On invocation, the thread has a lock on `parent`, `leftChild` and `rightChild`.
        /// - On return, the thread only holds a lock if the return value is Some.
        ///   In that case it holds a lock on the new root and the old root.
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
                let newRoot = new Node @ rc {
                    keys = keys,
                    children = children,
                    values = Array.empty(rc, 0),
                    lock = Lock.mkLock(rc),
                    size = 2,
                    parent = None,
                    next = None,
                    isLeaf = false
                };
                Lock.lock(newRoot->lock);
                leftChild->parent = Some(newRoot);
                rightChild->parent = Some(newRoot);
                Lock.unlock(rightChild->lock);
                Some(newRoot)
        }

        ///
        /// Applies `f` to all key-value pairs `(k, v)` in the tree rooted in `node` where `min <= k <= max`.
        ///
        /// Not thread-safe.
        ///
        pub def rangeQueryWith(
                f: k -> v -> Unit \ r0,
                min: k,
                max: k,
                node: Node[k, v, r1]): Unit \ r0 + r1 with Order[k] =
            let minLeaf = findLeafUnsafe(min, node);
            let index = binarySearch(min, minLeaf->size, minLeaf->keys);
            if(index < 0) {
                let insertionPoint = toInsertionPoint(index);
                if (insertionPoint <= minLeaf->size)
                    traverseRight(f, insertionPoint, max, minLeaf)
                    else ()
            } else
                traverseRight(f, index, max, minLeaf)

        ///
        /// Applies `f` to key-value pairs `(k, v)` starting at `node->keys[index]` until `node->keys[index] > max`.
        ///
        /// Uses the `next` pointer to traverse to the next node.
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

        /////////////////////////////////////////////////////////////////////////////
        // Helper methods                                                          //
        /////////////////////////////////////////////////////////////////////////////

        ///
        /// Insert `x` at position `i` in array `arr` as though it currently has
        /// `len` elements. Shifts elements at position `[i, .., len]` to `[i+1, .., len+1]`.
        ///
        def arrayInsert(x: a, i: Int32, len: Int32, arr: Array[a, r]): Unit \ r =
            if (len == i)
                Array.put(x, i, arr)
            else
                Array.copyInto(srcPos = i, dstPos = i + 1, len = len - i, src = arr, arr);
                Array.put(x, i, arr)

        ///
        /// Returns `true` if the pointer of `o1` is equal to the pointer of `o2`.
        ///
        def ptrEqual(o1: v, o2: v): Bool =
            // Unchecked_cast is here used to transform the values to Object.
            // The purpose is to be able to give them to a Java class which performs pointer comparison.
            let obj1 = unchecked_cast(o1 as Object);
            let obj2 = unchecked_cast(o2 as Object);
            // Unchecked_cast is here used to remove the IO effect from interacting with Java.
            unchecked_cast(Objects.equals(obj1, obj2) as _ \ {})

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
        /// Returns the real insertion point of the result of binarySearch, assuming
        /// that the result of binarySearch was negative.
        ///
        def toInsertionPoint(i: Int32): Int32 = -i-1

    }

}
