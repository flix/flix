// TODO: Possible optimzations: Instead of giving up as often as we do we could try test whether something changed

// Variation of concurrent B+ Tree with linked internal nodes

pub struct BPlusTree[k, v, r] {
    mut root: BPlusTree.Node[k, v, r],
    order:    Int32, // The maximum number of children a node can have
    rootLock: BPlusTree.Lock[r],
    rc:       Region[r]
}

mod BPlusTree {
    import java.lang.Object
    import java.lang.System
    import java.util.concurrent.locks.{ReentrantLock => JReentrantLock}
    import java.util.Objects

    @Internal
    pub enum Lock[_: Region](JReentrantLock)

    mod Lock {
        import java.util.concurrent.locks.{ReentrantLock => JReentrantLock}
        use BPlusTree.Lock

        pub def mkLock(_: Region[r]): Lock[r] \ r =
            Lock(unchecked_cast((new JReentrantLock(): _ \ IO) as _ \ r))

        pub def tryLock(lock: Lock[r]): Bool \ r =
            let Lock(l) = lock;
            unchecked_cast((l.tryLock(): _ \ IO) as _ \ r)

        pub def lock(lock: Lock[r]): Unit \ r =
            let Lock(l) = lock;
            unchecked_cast((l.lock(): _ \ IO) as _ \ r)

        pub def unlock(lock: Lock[r]): Unit \ r =
            let Lock(l) = lock;
            unchecked_cast((l.unlock(): _ \ IO) as _ \ r)

        pub def isLocked(lock: Lock[r]): Bool \ r =
            let Lock(l) = lock;
            unchecked_cast((l.isLocked(): _ \ IO) as _ \ r)
    }

    ///
    /// Returns `true` if and only if `m` contains the key-value `(k, v)`.
    /// Concurrency safe
    ///
    pub def containsPair(key: k, val: v, tree: BPlusTree[k, v, r]): Bool \ r with Order[k], Eq[v] =
        let getRoot = () -> root(tree);
        let leaf = Node.findLeaf(key, getRoot);
        let res = Node.leafContainsPair(key, val, leaf);
        Lock.unlock(Node.lock(leaf));
        res

    ///
    /// Returns `true` if and only if `m` contains the key-value pair `(k, v)`.
    /// Not concurrency safe
    ///
    pub def containsPairUnsafe(key: k, val: v, tree: BPlusTree[k, v, r]): Bool \ r with Order[k], Eq[v] =
        let leaf = Node.findLeafUnsafe(key, tree->root);
        Node.leafContainsPair(key, val, leaf)

    ///
    /// Returns a fresh empty B+ tree of with nodes of size `order`
    /// The order of the tree must be at least 3
    ///
    pub def empty(rc: Region[r], order: Int32): BPlusTree[k, v, r] \ r with Order[k] =
        if (order < 3)
            bug!("order of tree must be at least 3")
        else
            let node = new Node @ rc {
                keys = Array.empty(rc, order),
                children = None,
                values = Some(Array.empty(rc, order)),
                lock = Lock.mkLock(rc),
                size = 0,
                parent = None,
                next = None
            };
            new BPlusTree @ rc {
                root = node,
                order = order,
                rootLock = Lock.mkLock(rc),
                rc = rc
            }

    /// Returns the lock of the root pointer of `tree`
    pub def lock(tree: BPlusTree[k, v, r]): Lock[r] = tree->rootLock

    /// Returns the order of `tree`
    pub def order(tree: BPlusTree[k, v, r]): Int32 = tree->order

    // Locks the root and provides a lock on the root node.
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

    /// Returns the root pointer
    /// Not concurrency safe
    pub def getRootUnsafe(tree: BPlusTree[k, v, r]): Node[k, v, r] \ r =  tree->root

    ///
    /// Updates the B+ tree `tree` with the binding `k -> v`. Replaces any existing binding.
    ///
    pub def insert(key: k, val: v, tree: BPlusTree[k, v, r]): Unit \ r with Order[k] =
        let getRoot = () -> root(tree);
        let leaf = Node.findLeaf(key, getRoot);
        let newRoot = Node.insertIntoLeaf(key, val, tree->order, tree->rc, leaf);
        match newRoot {
            case Some(root) =>
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
    /// Concurrency safe
    ///
    pub def isEmpty(tree: BPlusTree[k, v, r]): Bool \ r = match Node.children(tree->root) {
        case Some(_) => false
        case None => Node.isEmpty(tree->root)
    }

    ///
    /// Returns `true` if and only if `tree` is the empty tree.
    /// Not concurrency safe
    ///
    pub def isEmptyUnsafe(tree: BPlusTree[k, v, r]): Bool \ r = match Node.children(tree->root) {
        case Some(_) => false
        case None => Node.isEmpty(tree->root)
    }

    ///
    /// Returns `true` if and only if `m` contains the key `k`.
    /// Concurrency safe
    ///
    pub def memberOf(key: k, tree: BPlusTree[k, v, r]): Bool \ r with Order[k] =
        let getRoot = () -> root(tree);
        let leaf = Node.findLeaf(key, getRoot);
        let res = Node.leafContains(key, leaf);
        Lock.unlock(Node.lock(leaf));
        res

    ///
    /// Returns `true` if and only if `m` contains the key `k`.
    /// Not concurrency safe
    ///
    pub def memberOfUnsafe(key: k, tree: BPlusTree[k, v, r]): Bool \ r with Order[k] =
        let leaf = Node.findLeafUnsafe(key, tree->root);
        Node.leafContains(key, leaf)

    ///
    /// Applies `f` to all key-value pairs from `tree` between `min` and `max`.
    ///
    pub def rangeQueryWith(f: k -> v -> Unit \ ef, min: k, max: k, tree: BPlusTree[k, v, r]): Unit \ {r, ef} with Order[k] =
        Node.rangeQueryWith(f, min, max, tree->root)

    pub def region_(tree: BPlusTree[k, v, r]): Region[r] =  tree->rc

    pub def toString(tree: BPlusTree[k, v, r]): String \ r with ToString[k], ToString[v] =
        Node.toString(0, tree->rc, tree->root)

    ///
    /// Helper methods
    ///
    pub def arrayInsert(x: a, i: Int32, len: Int32, v: Array[a, r]): Unit \ r =
        if (len == i)
            Array.put(x, i, v)
        else
            Array.copyInto(srcPos = i, dstPos = i + 1, len = len - i, src = v, v);
            Array.put(x, i, v)

    def ptrEqual(o1: v, o2: v): Bool \ r =
        let obj1 = unchecked_cast(o1 as Object \ r);
        let obj2 = unchecked_cast(o2 as Object \ r);
        unchecked_cast(Objects.equals(obj1, obj2) as _ \ r)

    def unwrap(x: Option[a]): a = match x {
        case Some(v) => v
        case None => bug!("Attempted unwrapping None")
    }

    /// Perform a binary search and return the index of `key` in `arr` if it exists.
    /// Otherwise, return (-insertionPoint - 1). See Java binary search documentation for arrays.
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

    def toInsertionPoint(i: Int32): Int32 = -i-1


    pub struct Node[k, v, r] {
        keys:       Array[k, r], // Acts as keys for internal nodes and values for leaves
        children:   Option[Array[Node[k, v, r], r]],
        values:     Option[Array[v, r]],
        lock:       Lock[r],
        mut size:   Int32,      // Number of children / values
        mut parent: Option[Node[k, v, r]],
        mut next:   Option[Node[k, v, r]]
    }

    mod Node {
        use BPlusTree.{unwrap, toInsertionPoint, binarySearch, ptrEqual}
        use BPlusTree.Lock

        // Accessor methods
        pub def children(node: Node[k, v, r]): Option[Array[Node[k, v, r], r]]  = node->children
        pub def keys(node: Node[k, v, r]): Array[k, r] = node->keys
        pub def lock(node: Node[k, v, r]): Lock[r] = node->lock
        pub def next(node: Node[k, v, r]): Option[Node[k, v, r]] \ r = node->next
        pub def parent(node: Node[k, v, r]): Option[Node[k, v, r]] \ r = node->parent
        pub def size(node: Node[k, v, r]): Int32 \ r  = node->size
        pub def values(node: Node[k, v, r]): Option[Array[v, r]]  = node->values

        pub def toString(indent: Int32, rc: Region[r], node: Node[k, v, r]): String \ r with ToString[k], ToString[v] =
            let indentString = String.repeat(indent, " ");
            match node->children {
                case Some(children) =>
                    let keysString = Array.join(",", Array.copyOfRange(rc, 0, node->size - 1, node->keys));
                    let childrenArr = Array.copyOfRange(rc, 0, node->size, children);
                    let childrenString = Array.foldLeft(a -> x -> "${a}${toString(indent+2, rc, x)}", "", childrenArr);
                    "${indentString}Internal(${keysString})\n${childrenString}"
                case None =>
                    let values = Array.copyOfRange(rc, 0, node->size, unwrap(node->values));
                    let keys = Array.copyOfRange(rc, 0, node->size, node->keys);
                    let keysStrings = Array.mapWithIndex(rc, i -> key -> "${key}: ${Array.get(i, values)}", keys);
                    let keysString = Array.join("${indentString}  ,\n", keysStrings);
                    "${indentString}Leaf(\n${indentString}  ${keysString}\n\${indentString})\n"
            }


        /// Like `findLeaf` but assumes no concurrency
        pub def findLeafUnsafe(key: k, node: Node[k, v, r]): Node[k, v, r] \ r with Order[k] =
            match node->children {
                case Some(children) =>
                    let index = binarySearch(key, node->size - 1, node->keys);
                    let childToVisit = if (index < 0)
                        Array.get(toInsertionPoint(index), children)
                    else
                        Array.get(index + 1, children);
                    findLeafUnsafe(key, childToVisit)
                case None => node
            }

        /// Traverse the tree until a leaf containing `val` is reached
        /// The function provides a single read lock on the returned node
        pub def findLeaf(key: k, getRoot: Unit -> Node[k, v, r]  \ r): Node[k, v, r] \ r with Order[k] =
            // Traverse the tree while keeping the invariant that there is a node on cur.
            // If aquiring a lock at any point fails release the locks you hold and retry from the top.
            def traverse(cur: Node[k, v, r]): Node[k, v, r] \ r =
                match cur->children {
                    case Some(children) =>
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
                            // To allow for the writer we encountered to grab the lock
                            // on the node readLock the child node as a wait operation.
                            Lock.lock(childLock);
                            Lock.unlock(childLock);
                            traverse(getRoot())
                        }
                    case None => cur
                };
            traverse(getRoot())

        pub def isEmpty(node: Node[k, v, r]): Bool \ r = node->size == 0

        // Returns true if the mapping `k` is contained in the leaf `node`
        pub def leafContains(key: k, node: Node[k, v, r]): Bool \ r with Order[k] =
            match node->values {
                case Some(_) =>
                    let index = binarySearch(key, node->size, node->keys);
                    if (index < 0)
                        false
                    else
                        true
                case None => bug!("The contains method on nodes should only be used on leaves")
            }

        // Returns true if the mapping `(k, v)` is contained in the leaf `node`
        pub def leafContainsPair(key: k, val: v, node: Node[k, v, r]): Bool \ r with Order[k], Eq[v] =
            match node->values {
                case Some(values) =>
                    let index1 = binarySearch(key, node->size, node->keys);
                    if (index1 < 0)
                        false
                    else
                        Array.get(index1, values) == val
                case None => bug!("The contains method on nodes should only be used on leaves")
            }

        def actualInsertLeaf(
                key: k,
                val: v,
                insertionPoint: Int32,
                node: Node[k, v, r]): Unit \ r with Order[k] =
            let size = node->size;
            BPlusTree.arrayInsert(val, insertionPoint, size, unwrap(node->values));
            BPlusTree.arrayInsert(key, insertionPoint, size, node->keys);
            node->size = size + 1

        /// Insert key-value pair `(key, val)` into the leaf `node`,
        /// potentially causing a split and returning a new root
        /// (Lock) Invariants:
        ///      On invocation, the thread has a lock on `node`.
        ///      On return, the thread only holds a lock on `Some(node)`
        pub def insertIntoLeaf(
                key: k,
                val: v,
                order: Int32,
                rc: Region[r],
                node: Node[k, v, r]): Option[Node[k, v, r]] \ r with Order[k] =
            let size = node->size;
            let index = binarySearch(key, size, node->keys);
            if (index >= 0)
                let _ = Array.put(val, index, unwrap(node->values));
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

        // (Lock) Invariants:
        //      On invocation, the thread has a lock on `node`.
        //      On return, the thread holds a lock on `node` and on `parent` given by `node->Some(parent)`
        // Try to lock the parent node.
        def lockParent(node: Node[k, v, r]): Unit \ r = match node->parent {
            case Some(parent) =>
                let parentLock = parent->lock;
                Lock.lock(parentLock);
                // If some thread had a lock on your parent and changed your parent pointer retry
                let isSame = ptrEqual(unwrap(node->parent), parent);
                if (not isSame) {
                    Lock.unlock(parentLock);
                    lockParent(node)
                } else ()
            case None => ()
        }

        // Split `leftLeaf` into two leaves and push the middle key into the parent,
        // potentially causing the parent to split and return a new root
        // (Lock) Invariants:
        //   On invocation, the thread has a lock on `leftNode`.
        //   On return, the thread only holds a lock if the return value is Some. In that case it holds a lock on the new root and the old root.
        def splitLeaf(
                leftLeaf: Node[k, v, r],
                key: k,
                val: v,
                insertionPoint: Int32,
                order: Int32,
                rc: Region[r]): Option[Node[k, v, r]] \ r with Order[k] =
            lockParent(leftLeaf); // TODO: For performance move this to be last and update rightLeaf as necessary.
            let prev_mid = order / 2;
            let (mid, insertInLeft) = if (insertionPoint <= prev_mid) (prev_mid, true) else (prev_mid + 1, false);
            let rightKeys = Array.empty(rc, order);
            let rightValues = Array.empty(rc, order);
            Array.copyInto(srcPos = mid, dstPos = 0, len = order - mid, src = leftLeaf->keys, rightKeys);
            Array.copyInto(srcPos = mid, dstPos = 0, len = order - mid, src = unwrap(leftLeaf->values), rightValues);
            // Create a new leaf to store the right half
            let rightLeaf = new Node @ rc {
                keys = rightKeys,
                children = None,
                values = Some(rightValues),
                lock = Lock.mkLock(rc),
                size = leftLeaf->size - mid,
                parent = leftLeaf->parent,
                next = leftLeaf->next
            };
            // Update the old leaf's fields
            leftLeaf->next = Some(rightLeaf);
            leftLeaf->size = mid;
            // Finally insert the new key-value pair into the correct leaf
            if (insertInLeft) actualInsertLeaf(key, val, insertionPoint, leftLeaf)
            else actualInsertLeaf(key, val, insertionPoint - mid, rightLeaf);
            // Push rightLeaf's smallest key up to parent
            Lock.lock(rightLeaf->lock);
            pushKeyToParent(leftLeaf->parent, leftLeaf, rightLeaf, Array.get(0, rightKeys), order, rc)


        //
        // Split `leftNode` into two nodes with `newKey` inserted into the appropriate node
        // depending on `insertionPoint`.
        //
        // Lock invariants:
        //   On invocation, the thread has a lock on `parent`.
        //   On return, the thread only holds a lock if the return value is Some.
        //   In that case it holds a lock on the new root and the old root.
        def splitInternal(
                leftNode: Node[k, v, r],
                insertedChild: Node[k, v, r],
                insertionPoint: Int32,
                newKey: k,
                order: Int32,
                rc: Region[r]): Option[Node[k, v, r]] \ r with Order[k] =
            lockParent(leftNode); // TODO: Move this to be last instruction and update new right as necessary.
            let prev_mid = order / 2;
            let (mid, insertInLeft, rightSize) = if (insertionPoint >= prev_mid) (prev_mid + 1, false, order - prev_mid) else (prev_mid, true, order - prev_mid);
            let rightKeys = Array.empty(rc, order - 1);
            let rightChildren = Array.empty(rc, order);
            leftNode->size = mid;
            let keyOfRight = if (insertInLeft) {
                let keyToPass_ = Array.get(mid - 1, leftNode->keys);
                let leftChildren = unwrap(leftNode->children);
                let leftKeys = leftNode->keys;
                // No problems. Just copy everything from mid and forward. Finish by inserting the new element in old node.
                Array.copyInto(srcPos = mid, dstPos = 0, len = rightSize - 1, src = leftKeys, rightKeys);
                Array.copyInto(srcPos = mid, dstPos = 0, len = rightSize, src = leftChildren, rightChildren);
                BPlusTree.arrayInsert(insertedChild, insertionPoint + 1, mid, leftChildren);
                BPlusTree.arrayInsert(newKey, insertionPoint, mid, leftKeys);
                leftNode->size = mid + 1;
                keyToPass_
            } else {
                // Insert the new node in the new right node and copy the relevant elements of the leftNode (index>=mid).
                let rightSizeWithoutNew = rightSize - 1;
                if(insertionPoint + 1 == mid) {
                    // If insertionPoint is the first element copy all old keys and old elements and insert new child at 0.
                    Array.copyInto(srcPos = mid - 1, dstPos = 0, len = rightSizeWithoutNew, src = leftNode->keys, rightKeys);
                    Array.copyInto(srcPos = mid, dstPos = 1, len = rightSizeWithoutNew, src = unwrap(leftNode->children), rightChildren);
                    Array.put(insertedChild, 0, rightChildren);
                    newKey
                } else {
                    let childIndexInRight = insertionPoint + 1 - mid;
                    Array.copyInto(srcPos = mid, dstPos = 0, len = childIndexInRight - 1, src = leftNode->keys, rightKeys);
                    Array.copyInto(srcPos = mid, dstPos = 0, len = childIndexInRight, src = unwrap(leftNode->children), rightChildren);
                    Array.put(insertedChild, childIndexInRight, rightChildren);
                    Array.put(newKey, childIndexInRight - 1, rightKeys);
                    // If insertionPoint is the last index we are done, otherwise insert rest of list.
                    if(childIndexInRight != rightSizeWithoutNew) {
                        Array.copyInto(srcPos = mid + childIndexInRight - 1, dstPos = childIndexInRight, len = rightSizeWithoutNew - childIndexInRight, src = leftNode->keys, rightKeys);
                        Array.copyInto(srcPos = mid + childIndexInRight, dstPos = childIndexInRight + 1, len = rightSizeWithoutNew - childIndexInRight, src = unwrap(leftNode->children), rightChildren)
                    } else ();
                    Array.get(mid - 1, leftNode->keys)
                }
            };
            let rightNode = new Node @ rc {
                keys = rightKeys,
                children = Some(rightChildren),
                values = None,
                lock = Lock.mkLock(rc),
                size = rightSize,
                parent = leftNode->parent,
                next = leftNode->next
            };
            leftNode->next = Some(rightNode);
            Array.forEachWithIndex(i -> child ->
                if (i < rightSize)
                    child->parent = Some(rightNode)
                else (), rightChildren);
            Lock.unlock(insertedChild->lock); // TODO: Could be done earlier?
            Lock.lock(rightNode->lock); // pushKeyToParent expects a lock on the parent
            pushKeyToParent(leftNode->parent, leftNode, rightNode, keyOfRight, order, rc)

        // (Lock) Invariants:
        //   On invocation, the thread has a lock on `parent`, `leftChild` and `rightChild`.
        //   On return, the thread only holds a lock if the return value is Some.
        //   In that case it holds a lock on the new root and the old root.
        def pushKeyToParent(
                parent: Option[Node[k, v, r]],
                leftChild: Node[k, v, r],
                rightChild: Node[k, v, r],
                newKey: k,
                order: Int32,
                rc: Region[r]): Option[Node[k, v, r]] \ r with Order[k] = match parent {
            case Some(node) =>
                let children = unwrap(node->children);
                let insertionPoint = toInsertionPoint(binarySearch(newKey, node->size - 1, node->keys));
                let size = node->size;
                if(size < order) {
                    BPlusTree.arrayInsert(newKey, insertionPoint, size - 1, node->keys);
                    BPlusTree.arrayInsert(rightChild, insertionPoint + 1, size, children);
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
                    children = Some(children),
                    values = None,
                    lock = Lock.mkLock(rc),
                    size = 2,
                    parent = None,
                    next = None
                };
                Lock.lock(newRoot->lock);
                leftChild->parent = Some(newRoot);
                rightChild->parent = Some(newRoot);
                Lock.unlock(rightChild->lock);
                Some(newRoot)
        }

        // No locks be careful
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

        def traverseRight(
                f: k -> v -> Unit \ r0,
                index: Int32,
                max: k,
                node: Node[k, v, r1]): Unit \ r0 + r1 with Order[k] =
            if(index < node->size) {
                let k = Array.get(index, node->keys);
                if(k <= max) {
                    f(k, Array.get(index, unwrap(node->values)));
                    traverseRight(f, index + 1, max, node)
                } else ()
            } else match node->next {
                case Some(next) => traverseRight(f, 0, max, next)
                case None => ()
            }
    }

}
