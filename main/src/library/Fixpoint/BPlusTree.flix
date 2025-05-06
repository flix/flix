// Temporary file until we sync BPlusTree

type alias Search = Vector[Int32]

pub struct BPlusTree[r] {
    mut root: BPlusTree.Node[r],
    order:    Int32,
    rootLock: BPlusTree.Lock[r],
    rc:       Region[r],
    search:   Search
}

mod BPlusTree {
    import java.util.concurrent.locks.{ReentrantLock => JReentrantLock}
    use Fixpoint.Boxed

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

        @Internal
        pub def isLocked(lock: Lock[r]): Bool \ r =
            let Lock(l) = lock;
            unchecked_cast((l.isLocked(): _ \ IO) as _ \ r)
    }

    pub def toString(tree: BPlusTree[r]): String \ r =
        "Search: ${Vector.join(" < ", tree->search)}\nTree:\n${Node.toString(0, tree->rc, tree->root)}"

    pub def forEach(f: Array[Int64, r] -> Boxed -> Unit \ ef, tree: BPlusTree[r]): Unit \ ef + r =
        let minLeaf = Node.leftMostChild(tree->search, tree->root);
        Node.traverseRightUnconditional(f, 0, tree->search, minLeaf)

    pub def containsPair(key: Array[Int64, r], val: Boxed, tree: BPlusTree[r]): Bool \ r =
        let leaf = BPlusTree.findLeaf(key, tree);
        let res = Node.leafContainsPair(key, val, tree->search, leaf);
        Lock.unlock(Node.getLock(leaf));
        res

    pub def containsPairThreadUnsafe(key: Array[Int64, r], val: Boxed, tree: BPlusTree[r]): Bool \ r =
        let leaf = BPlusTree.findLeafThreadUnsafe(key, tree);
        Node.leafContainsPair(key, val, tree->search, leaf)

    pub def getSearch(tree: BPlusTree[r]): Search = tree->search

    pub def merge(src: BPlusTree[r], dst: BPlusTree[r]): Unit \ r = {
        let len = Vector.length(src->search);
        let min = Array.repeat(src->rc, len, Int64.minValue());
        let max = Array.repeat(src->rc, len, Int64.maxValue());
        BPlusTree.rangeQueryWith(k -> v -> BPlusTree.insert(k, v, dst), min, max, src)
    }

    pub def mergeWith(f: Boxed -> Boxed -> Boxed \ ef, src: BPlusTree[r], dst: BPlusTree[r]): Unit \ r + ef = {
        let len = Vector.length(src->search);
        let min = Array.repeat(src->rc, len, Int64.minValue());
        let max = Array.repeat(src->rc, len, Int64.maxValue());
        BPlusTree.rangeQueryWith(k -> v -> BPlusTree.putWith(f, k, v, dst), min, max, src)
    }

    pub def emptyWithOrder(rc: Region[r], order: Int32, search: Search): BPlusTree[r] \ r = 
        let node = Node.mkLeaf(rc, Array.empty(rc, order), Array.empty(rc, order), 0, None, None);
        new BPlusTree @ rc {
            root = node,
            order = order,
            rootLock = Lock.mkLock(rc),
            rc = rc,
            search = search
        }

    pub def empty(rc: Region[r], search: Search): BPlusTree[r] \ r = 
        let order = 64;
        let node = Node.mkLeaf(rc, Array.empty(rc, order), Array.empty(rc, order), 0, None, None);
        new BPlusTree @ rc {
            root = node,
            order = order,
            rootLock = Lock.mkLock(rc),
            rc = rc,
            search = search
        }

    pub def emptyWithOder(rc: Region[r], order: Int32, search: Search): BPlusTree[r] \ r =
        if (order < 3)
            bug!("BPlusTree.empty requires an order more than 2, found '${order}'.")
        else {
            let node = Node.mkLeaf(rc, Array.empty(rc, order), Array.empty(rc, order), 0, None, None);
            new BPlusTree @ rc {
                root = node,
                order = order,
                rootLock = Lock.mkLock(rc),
                rc = rc,
                search = search
            }
        }

    pub def order(tree: BPlusTree[r]): Int32 = tree->order

    def findLeaf(key: Array[Int64, r], tree: BPlusTree[r]): Node[r] \ r =
        Node.traverseDown(key, getLockedRoot(tree), tree->search, tree)

    def findLeafThreadUnsafe(key: Array[Int64, r], tree: BPlusTree[r]): Node[r] \ r =
        Node.traverseDownThreadUnsafe(key, tree->search, tree->root)

    @Internal
    def getLockedRoot(tree: BPlusTree[r]): Node[r] \ r =
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

    pub def insert(key: Array[Int64, r], val: Boxed, tree: BPlusTree[r]): Unit \ r =
        let leaf = BPlusTree.findLeaf(key, tree);
        let newRoot = Node.insertIntoLeaf(key, val, tree->order, tree->rc, tree->search, leaf);
        fixRoot(newRoot, tree)
    
    def fixRoot(newRoot: Option[Node[r]], tree: BPlusTree[r]): Unit \ r = match newRoot {
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

    pub def getWithDefault(key: Array[Int64, r], default_: Boxed, tree: BPlusTree[r]): Boxed \ r =
        let leaf = BPlusTree.findLeafThreadUnsafe(key, tree);
        let res = Node.getWithDefault(key, tree->search, default_, leaf);
        res

    pub def isEmpty(tree: BPlusTree[r]): Bool \ r =
        let root = BPlusTree.getLockedRoot(tree);
        let res = Node.isEmpty(root);
        Lock.unlock(Node.getLock(root));
        res

    pub def isEmptyThreadUnsafe(tree: BPlusTree[r]): Bool \ r = Node.isEmpty(tree->root)

    pub def memberOf(key: Array[Int64, r], tree: BPlusTree[r]): Bool \ r =
        let leaf = BPlusTree.findLeaf(key, tree);
        let res = Node.leafContains(key, tree->search, leaf);
        Lock.unlock(Node.getLock(leaf));
        res

    pub def memberOfThreadUnsafe(key: Array[Int64, r], tree: BPlusTree[r]): Bool \ r =
        let leaf = BPlusTree.findLeafThreadUnsafe(key, tree);
        Node.leafContains(key, tree->search, leaf)

    pub def rangeQueryWith(f: Array[Int64, r] -> Boxed -> Unit \ ef, min: Array[Int64, r], max: Array[Int64, r], tree: BPlusTree[r]): Unit \ {r, ef} =
        Node.rangeQueryWith(f, min, max, tree->search, tree->root)

    pub def putWith(f: Boxed -> Boxed -> Boxed \ ef, k: Array[Int64, r], val: Boxed, tree: BPlusTree[r]): Unit \ ef + r =
        let leaf = findLeaf(k, tree);
        let optNewRoot = Node.putWith(f, k, val, tree->search, tree->order, tree->rc, leaf);
        fixRoot(optNewRoot, tree)

    @Internal
    pub def assertTreeInvariant(tree: BPlusTree[r]): Bool \ r =
        let root = tree->root;
        let order = BPlusTree.order(tree);
        let nodeWorks = Node.assertNodeInvariant(root, order, true, None, tree->search);
        let rootIsUnlocked = not BPlusTree.Lock.isLocked(tree->rootLock);
        let rootHasNoParent = Option.isEmpty(Node.parent(root));
        nodeWorks and rootHasNoParent and rootIsUnlocked

    pub struct Node[r] {
        keys:       Array[Array[Int64, r], r],
        children:   Array[Node[r], r],
        values:     Array[Boxed, r],
        lock:       Lock[r],
        mut size:   Int32,
        mut parent: Option[Node[r]],
        mut next:   Option[Node[r]],
        isLeaf:     Bool
    }

    mod Node {
        use BPlusTree.Node.traverseDownThreadUnsafe
        import java.lang.Object
        import java.util.Objects
        use BPlusTree.Lock
        use Fixpoint.Boxed

        pub def tupleToString(tuple: Array[Int64, r]): String \ r = 
            let s = Array.join(",", tuple); 
            "(${s})"

        pub def toString(indent: Int32, rc: Region[r], node: Node[r]): String \ r =
            let indentString = String.repeat(indent, " ");
            match node->isLeaf {
                case false =>
                    let children = node->children;
                    let keysString = 
                        Array.copyOfRange(rc, 0, node->size - 1, node->keys) |> 
                        Array.map(rc, tupleToString) |>
                        Array.join(",");
                    let childrenArr = Array.copyOfRange(rc, 0, node->size, children);
                    let childrenString = Array.foldLeft(a -> x -> "${a}${toString(indent+2, rc, x)}", "", childrenArr);
                    "T${indentString}Internal(${keysString})\n${childrenString}"
                case true =>
                    let values = Array.copyOfRange(rc, 0, node->size, node->values);
                    let keys = Array.copyOfRange(rc, 0, node->size, node->keys);
                    let keysStrings = Array.mapWithIndex(rc, i -> key -> "${tupleToString(key)}: ${Array.get(i, values)}", keys);
                    let keysString = Array.join(",\n${indentString}  ", keysStrings);
                    "${indentString}Leaf(\n${indentString}  ${keysString}\n${indentString})\n"
            }

        pub def putWith(f: Boxed -> Boxed -> Boxed \ ef, key: Array[Int64, r], val: Boxed, search: Search, order: Int32, rc: Region[r], node: Node[r]): Option[Node[r]] \ r + ef =
            let index = binarySearch(key, node->size, search, node->keys);
            if (index < 0) {
                insertIntoLeafPropagate(key, val, order, rc, search, index, node)
            } else{
                let v1 = Array.get(index, node->values);
                Array.put(f(val, v1), index, node->values);
                Lock.unlock(node->lock);
                None
            }

        pub def traverseRightUnconditional(
            f: Array[Int64, r] -> Boxed -> Unit \ ef,
            index: Int32,
            search: Search,
            node: Node[r]
        ): Unit \ ef + r =
            if(index < node->size) {
                let k = Array.get(index, node->keys);
                f(k, Array.get(index, node->values));
                traverseRightUnconditional(f, index + 1, search, node)
            } else match node->next {
                case Some(next) => traverseRightUnconditional(f, 0, search, next)
                case None => ()
            }

        pub def getWithDefault(key: Array[Int64, r], search: Search, default_: Boxed, node: Node[r]): Boxed \ r =
            let index = binarySearch(key, node->size, search, node->keys);
            if (index < 0)
                default_
            else
                Array.get(index, node->values)

        def actualInsertLeaf(
            key: Array[Int64, r],
            val: Boxed,
            insertionPoint: Int32,
            node: Node[r]
        ): Unit \ r =
            let size = node->size;
            arrayInsert(val, insertionPoint, size, node->values);
            arrayInsert(key, insertionPoint, size, node->keys);
            node->size = size + 1

        def applyPredicate(v: Vector[Array[Int64, r]], f: Array[Int64, r] -> Array[Int64, r] -> Bool \ r): Bool \ r =
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

        def isSorted(v: Vector[Array[Int64, r]], search: Search): Bool \ r =
            applyPredicate(v, x -> y -> comparison(x, y, 0, Vector.length(search), search) == -1)

        def toLength(v: Vector[v], length: Int32): Vector[v] =
            Vector.slice(start = 0, end = length, v)

        @Internal
        pub def assertNodeInvariant(
            node: Node[r], 
            order: Int32, 
            isRightMost: Bool, 
            shouldBeBiggerThan: Option[Array[Int64, r]],
            search: Search
        ): Bool \ r =
            def unwrapNode(opt: Option[Node[r]]): Node[r] = match opt {
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
                    // let nextPointerCorrect = not unwrapNode(Vector.head(children))->isLeaf or applyPredicate(children, prev -> cur -> equalByReference(unwrapNode(prev->next), cur));
                    let lastHasNoNext = not isRightMost or Option.isEmpty(unwrapNode(Vector.last(children))->next);
                    let noLeaves = Array.size(node->values) == 0;
                    let nodeIsUnlocked = not BPlusTree.Lock.isLocked(node->lock);
                    let isBiggerThanPrev = match shouldBeBiggerThan {
                        case None => true
                        case Some(v) => comparison(Vector.get(0, keys), v, 0, Vector.length(search), search) == 1
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

        pub def getLock(node: Node[r]): Lock[r] = node->lock

        pub def isEmpty(node: Node[r]): Bool \ r = node->size == 0

        pub def insertIntoLeaf(
            key: Array[Int64, r],
            val: Boxed,
            order: Int32,
            rc: Region[r],
            search: Search,
            node: Node[r]
        ): Option[Node[r]] \ r = {
            let size = node->size;
            let index = binarySearch(key, size, search, node->keys);
            insertIntoLeafPropagate(key, val, order, rc, search, index, node)
        }

        def insertIntoLeafPropagate(
                key: Array[Int64, r],
                val: Boxed,
                order: Int32,
                rc: Region[r],
                search: Search,
                index: Int32,
                node: Node[r]
        ): Option[Node[r]] \ r = {
            let size = node->size;
            if (index >= 0) {
                Array.put(val, index, node->values);
                Lock.unlock(node->lock);
                None
            } else {
                let insertionPoint = toInsertionPoint(index);
                if (size < order) {
                    actualInsertLeaf(key, val, insertionPoint, node);
                    Lock.unlock(node->lock);
                    None
                } else
                    splitLeaf(node, key, val, insertionPoint, order, rc, search)
        }}

        pub def leafContains(key: Array[Int64, r], search: Search, node: Node[r]): Bool \ r =
            let index = binarySearch(key, node->size, search, node->keys);
            if (index < 0)
                false
            else
                true

        pub def leafContainsPair(key: Array[Int64, r], val: Boxed, search: Search, node: Node[r]): Bool \ r =
            let index1 = binarySearch(key, node->size, search, node->keys);
            if (index1 < 0)
                false
            else
                Array.get(index1, node->values) == val

        def lockParent(node: Node[r]): Unit \ r = match node->parent {
            case Some(parent) =>
                let parentLock = parent->lock;
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

        pub def mkLeaf(
            rc: Region[r],
            keys: Array[Array[Int64, r], r],
            values: Array[Boxed, r],
            size: Int32,
            parent: Option[Node[r]],
            next: Option[Node[r]]
        ): Node[r] \ r =
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

        def mkInternal(
            rc: Region[r],
            keys: Array[Array[Int64, r], r],
            children: Array[Node[r], r],
            size: Int32,
            parent: Option[Node[r]]
        ): Node[r] \ r =
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

        pub def parent(node: Node[r]): Option[Node[r]] \ r = node->parent

        def pushKeyToParent(
            parent: Option[Node[r]],
            leftChild: Node[r],
            rightChild: Node[r],
            newKey: Array[Int64, r],
            order: Int32,
            rc: Region[r],
            search: Search
        ): Option[Node[r]] \ r = match parent {
            case Some(node) =>
                let children = node->children;
                let insertionPoint = toInsertionPoint(binarySearch(newKey, node->size - 1, search, node->keys));
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
                    splitInternal(node, rightChild, insertionPoint, newKey, order, rc, search)
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

        pub def rangeQueryWith(
            f: Array[Int64, r] -> Boxed -> Unit \ r0,
            min: Array[Int64, r],
            max: Array[Int64, r],
            search: Search,
            node: Node[r]
        ): Unit \ r0 + r =
            let minLeaf = traverseDownThreadUnsafe(min, search, node);
            let index = binarySearch(min, minLeaf->size, search, minLeaf->keys);
            if(index < 0) {
                let insertionPoint = toInsertionPoint(index);
                if (insertionPoint <= minLeaf->size)
                    traverseRight(f, insertionPoint, max, search, minLeaf)
                else ()
            } else
                traverseRight(f, index, max, search, minLeaf)

        pub def forEach(
            f: Array[Int64, r] -> Boxed -> Unit \ r0,
            search: Search,
            node: Node[r]
        ): Unit \ r0 + r =
            let minLeaf = leftMostChild(search, node);
            traverseRightUnconditional(f, 0, search, minLeaf)

        def splitLeaf(
            leftLeaf: Node[r],
            key: Array[Int64, r],
            val: Boxed,
            insertionPoint: Int32,
            order: Int32,
            rc: Region[r],
            search: Search
        ): Option[Node[r]] \ r =
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
            pushKeyToParent(leftLeaf->parent, leftLeaf, rightLeaf, Array.get(0, rightKeys), order, rc, search)

        def splitInternal(
            leftNode: Node[r],
            insertedChild: Node[r],
            insertionPoint: Int32,
            newKey: Array[Int64, r],
            order: Int32,
            rc: Region[r],
            search: Search
        ): Option[Node[r]] \ r =
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
            pushKeyToParent(leftNode->parent, leftNode, rightNode, keyOfRight, order, rc, search)

        def splitToRightLeaf(rc: Region[r], mid: Int32, order: Int32, leftLeaf: Node[r]): (Array[Array[Int64, r], r], Array[Boxed, r]) \ r = {
            let rightKeys = Array.empty(rc, order);
            let rightValues = Array.empty(rc, order);
            Array.copyInto(srcPos = mid, dstPos = 0, len = order - mid, src = leftLeaf->keys, rightKeys);
            Array.copyInto(srcPos = mid, dstPos = 0, len = order - mid, src = leftLeaf->values, rightValues);
            (rightKeys, rightValues)
        }

        def splitInternalToLeftChild(
            rc: Region[r],
            insertedChild: Node[r],
            insertionPoint: Int32,
            newKey: Array[Int64, r],
            mid: Int32,
            order: Int32,
            rightSize: Int32,
            leftNode: Node[r]
        ): (Array[Int64, r], Array[Array[Int64, r], r], Array[Node[r], r]) \ r = {
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

        def splitInternalToRightChild(
            rc: Region[r],
            insertedChild: Node[r],
            insertionPoint: Int32,
            newKey: Array[Int64, r],
            mid: Int32,
            order: Int32,
            rightSize: Int32,
            leftNode: Node[r]
        ): (Array[Int64, r], Array[Array[Int64, r], r], Array[Node[r], r]) \ r = {
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

        pub def traverseDown(key: Array[Int64, r], cur: Node[r], search: Search, tree: BPlusTree[r]): Node[r] \ r =
            match cur->isLeaf {
                case false =>
                    let children = cur->children;
                    let index = binarySearch(key, cur->size - 1, search, cur->keys);
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
                        traverseDown(key, childToVisit, search, tree)
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

        pub def traverseDownThreadUnsafe(key: Array[Int64, r], search: Search, node: Node[r]): Node[r] \ r =
            match node->isLeaf {
                case false =>
                    let children = node->children;
                    let index = binarySearch(key, node->size - 1, search, node->keys);
                    let childToVisit = if (index < 0)
                        Array.get(toInsertionPoint(index), children)
                    else
                        Array.get(index + 1, children);
                    traverseDownThreadUnsafe(key, search, childToVisit)
                case true => node
            }

        def traverseRight(
            f: Array[Int64, r] -> Boxed -> Unit \ r0,
            index: Int32,
            max: Array[Int64, r],
            search: Search,
            node: Node[r]
        ): Unit \ r0 + r =
            if(index < node->size) {
                let k = Array.get(index, node->keys);
                if(comparison(k, max, 0, Vector.length(search), search) != 1) {
                    f(k, Array.get(index, node->values));
                    traverseRight(f, index + 1, max, search, node)
                } else ()
            } else match node->next {
                case Some(next) => traverseRight(f, 0, max, search, next)
                case None => ()
            }

        pub def leftMostChild(search: Search, node: Node[r]): Node[r] \ r =
            match node->isLeaf {
                case false =>
                    leftMostChild(search, Array.get(0, node->children))
                case true => node
            }

        def arrayInsert(x: a, i: Int32, len: Int32, arr: Array[a, r]): Unit \ r =
            if (len == i)
                Array.put(x, i, arr)
            else {
                Array.copyInto(srcPos = i, dstPos = i + 1, len = len - i, src = arr, arr);
                Array.put(x, i, arr)
            }

        def binarySearch(key: Array[Int64, r], length: Int32, search: Search, arr: Array[Array[Int64, r], r]): Int32 \ r =
            let len = Vector.length(search);
            def f(l: Int32, r: Int32): Int32 \ r = {
                if (l <= r) {
                    let m = (l + r) `Int32.rightShift` 1;
                    let elem = Array.get(m, arr);
                    let comp = comparison(key, elem, 0, len, search);
                    if (comp == 1) f(m+1, r)
                    else if (comp == -1) f(l, m-1)
                    else m
                } else -l-1
            };
            f(0, length - 1)

        def comparison(arr1: Array[Int64, r], arr2: Array[Int64, r], i: Int32, max: Int32, search: Search): Int32 \ r = {
            if(i >= max) {
                0
            } else {
                let s = Vector.get(i, search);
                let arr1P = Array.get(s, arr1);
                let arr2P = Array.get(s, arr2);
                if(arr1P < arr2P) {
                    -1
                } else if (arr1P > arr2P) {
                    1
                } else {
                    comparison(arr1, arr2, i + 1, max, search)
                }
            }
        }

        def equalByReference(o1: Node[r], o2: Node[r]): Bool =
            // Unchecked cast is here used to transform the values to Object.
            // The purpose is to be able to give them to a Java class which performs pointer comparison.
            let obj1 = unchecked_cast(o1 as Object);
            let obj2 = unchecked_cast(o2 as Object);
            // Unchecked cast is here used to remove the IO effect from interacting with Java.
            unchecked_cast((Objects.equals(obj1, obj2): _ \ IO) as _ \ {})

        def toInsertionPoint(i: Int32): Int32 = -i-1
    }

}
