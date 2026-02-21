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

mod BPlusTree.Node {
    import java.lang.invoke.{VarHandle => JVarHandle}
    import java.lang.{Object => JObject}
    import java.util.{Objects => JObjects}
    use BPlusTree.Lock
    use BPlusTree.Search

    ///
    /// Returns the next in-bounds `(index, node)` while traversing right.
    ///
    def seek(index: Int32, node: Node[k, v, r]): Option[(Int32, Node[k, v, r])] \ r =
        if (index < node->size)
            Some((index, node))
        else match node->next {
            case None => None
            case Some(next) => seek(0, next)
        }

    ///
    /// Compares leaf entries from left to right in lockstep.
    ///
    pub def sameElementsFrom(
        i1: Int32,
        n1: Node[k, v, r1],
        i2: Int32,
        n2: Node[k, v, r2]
    ): Bool \ r1 + r2 with Order[k], Eq[v] = match (seek(i1, n1), seek(i2, n2)) {
        case (None, None) => true
        case (Some((j1, m1)), Some((j2, m2))) =>
            let (k1, v1) = getKeyAndValueAt(j1, m1);
            let (k2, v2) = getKeyAndValueAt(j2, m2);
            if (k1 == k2 and v1 == v2)
                sameElementsFrom(j1 + 1, m1, j2 + 1, m2)
            else
                false
        case _ => false
    }

    ///
    /// Applies the function `f` to every value in `n`.
    ///
    pub def transformWithKey(
        f: k -> v -> v \ ef,
        i: Int32,
        n: Node[k, v, r]
    ): Unit \ r + ef with Order[k] = match seek(i, n) {
        case None => ()
        case Some((index, node)) => {
            let v1 = getKeyAndValueAt(index, node) ||> f;
            Array.put(v1, index, node->values);
            transformWithKey(f, index + 1, node)
        }
    }

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
        // For the Datalog engine we support a custom order, but only for `Vector[Int64]`.
        // This is checked by testing whether `search` is `null`.
        if (Object.isNull(search)) {
            let comp = val1 <=> val2;
            if (comp == Comparison.GreaterThan) 1
            else if (comp == Comparison.LessThan) -1
            else 0
        } else {
            let len = Vector.length(search);
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
    pub def getKeyAndValueAt(index: Int32, node: Node[k, v, r]): (k, v) \ r = {
        let k = Array.get(index, node->keys);
        let v = Array.get(index, node->values);
        (k, v)
    }

    ///
    /// Optionally return the node to the right of `node`.
    ///
    pub def next(node: Node[k, v, r]): Option[Node[k, v, r]] \ r = node->next

    ///
    /// Returns `true` if the pointer of `o1` is equal to the pointer of `o2`.
    ///
    def equalByReference(o1: Node[k, v, r], o2: Node[k, v, r]): Bool =
        // Unchecked cast is here used to transform the values to Object.
        // The purpose is to be able to give them to a Java class which performs pointer comparison.
        let obj1 = unchecked_cast(o1 as JObject);
        let obj2 = unchecked_cast(o2 as JObject);
        unsafe IO { JObjects.equals(obj1, obj2) }

    ///
    /// Optionally returns the first mapping satisfying the function `f` in `node` and
    /// to the right of `node`.
    ///
    /// Not thread-safe.
    ///
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
    pub def getLock(node: Node[k, v, r]): Lock[r] = node->lock

    ///
    /// Returns `Some(val)` if `key => val` is in `tree`. Otherwise return `None`.
    ///
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
    /// `putIfInternal` if this is not successful.
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
    pub def leafMemberOf(key: k, search: Search, node: Node[k, v, r]): Bool \ r with Order[k] =
        let index = binarySearch(key, search, node);
        if (index < 0)
            false
        else
            true

    ///
    /// Returns `true` if the mapping `key => val` is contained in the leaf `node`.
    ///
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
