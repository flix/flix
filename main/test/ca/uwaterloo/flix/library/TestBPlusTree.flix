mod TestBPlusTree {
    import java.lang.Object
    import java.util.Objects
    use Array.sameElements
    use BPlusTree.Node

    /////////////////////////////////////////////////////////////////////////////
    // Test invariant about the BTree                                          //
    /////////////////////////////////////////////////////////////////////////////
    def testTreeInvariant(tree: BPlusTree[k, v, r]): Bool \ r with Order[k] =
        let root = BPlusTree.getRootTesting(tree);
        let order = BPlusTree.order(tree);
        let nodeWorks = testNodeInvariant(root, order, true);
        let rootIsUnlocked = not BPlusTree.Lock.isLocked(BPlusTree.getLock(tree));
        let rootHasNoParent = Option.isEmpty(Node.parent(root));
        nodeWorks and rootHasNoParent and rootIsUnlocked

    def testNodeInvariant(node: Node[k, v, r], order: Int32, isRightMost: Bool): Bool \ r with Order[k] =
        match Node.isLeaf(node) {
        case false =>
            let c = Node.children(node);
            let size = Node.size(node);
            let keys = toLength(Array.toVector(Node.keys(node)), size - 1);
            let children = toLength(Array.toVector(c), size);
            let keysInOrder = isInOrder(keys);
            let childrenWorks = Vector.forAll(
                x -> testNodeInvariant(x, order, false), toLength(children, size - 1)
            ) and testNodeInvariant(unwrap(Vector.last(children)), order, isRightMost);
            let parentPointerCorrect = Vector.foldLeft(
                workingSoFar -> cur -> workingSoFar and ptrEqual(node, unwrap(Node.parent(cur))),
                true, children);
            let nextPointerCorrect = not Node.isLeaf(unwrap(Vector.head(children))) or applyPredicate(children, prev -> cur -> ptrEqual(unwrap(Node.next(prev)), cur));
            let lastHasNoNext = not isRightMost or Option.isEmpty(Node.next(unwrap(Vector.last(children))));
            let noLeaves = Array.size(Node.values(node)) == 0;
            let nodeIsUnlocked = not BPlusTree.Lock.isLocked(Node.getLock(node));
            parentPointerCorrect and nextPointerCorrect and
                lastHasNoNext and noLeaves and keysInOrder and
                childrenWorks and nodeIsUnlocked
        case true =>
            let size = Node.size(node);
            let keys = toLength(Array.toVector(Node.keys(node)), size);
            let keysInOrder = isInOrder(toLength(keys, size));
            let sizeInRange = size <= order and 0 <= size;
            let noChildren = Array.size(Node.children(node)) == 0;
            let leafIsUnlocked = not BPlusTree.Lock.isLocked(Node.getLock(node));
            sizeInRange and keysInOrder and leafIsUnlocked and noChildren
    }

    def toLength(v: Vector[v], length: Int32): Vector[v] =
        Vector.slice(start = 0, end = length, v)

    def isInOrder(v: Vector[k]): Bool with Order[k] =
        applyPredicate(v, x -> y -> x <= y)

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

    def listToMap(l: List[(k, v)]): Map[k, v] with Order[k] =
        List.foldLeft(acc -> p -> let (x, y) = p; Map.insert(x, y, acc), Map#{}, l)

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isEmpty01(): Bool = region rc {
        let tree: BPlusTree[Int32, Int32, rc] = BPlusTree.empty(rc, 3);
        BPlusTree.isEmpty(tree)
    }

    @test
    def isEmpty02(): Bool = region rc {
        let tree: BPlusTree[Int32, Int32, rc] = BPlusTree.empty(rc, 3);
        BPlusTree.insert(1, 2, tree);
        not BPlusTree.isEmpty(tree)
    }

    @test
    def isEmpty03(): Bool = region rc {
        let tree: BPlusTree[Int32, Int32, rc] = BPlusTree.empty(rc, 3);
        let empty0 = BPlusTree.isEmpty(tree);
        BPlusTree.insert(1, 2, tree);
        let empty1 = BPlusTree.isEmpty(tree);
        BPlusTree.insert(2, 3, tree);
        let empty2 = BPlusTree.isEmpty(tree);
        BPlusTree.insert(3, 4, tree);
        let empty3 = BPlusTree.isEmpty(tree);
        BPlusTree.insert(4, 5, tree);
        let empty4 = BPlusTree.isEmpty(tree);
        empty0 and not empty1 and not empty2 and not empty3 and not empty4
    }

    /////////////////////////////////////////////////////////////////////////////
    // insert                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    def getRandomListOfPairs(n: Int32): List[(Int32, Int32)] \ Random =
        List.map(_ -> (Random.randomInt32(), Random.randomInt32()), List.range(0, n))

    @test
    def insert01(): Bool = region rc {
        let tree = BPlusTree.empty(rc, 3);
        BPlusTree.insert(4, 2, tree);
        BPlusTree.containsPair(4, 2 ,tree) and testTreeInvariant(tree)
    }

    @test
    def insert02(): Bool = region rc {
        let tree = BPlusTree.empty(rc, 3);
        BPlusTree.insert(1, 10, tree);
        BPlusTree.insert(2, 11, tree);
        BPlusTree.insert(3, 12, tree);
        BPlusTree.containsPair(1, 10, tree) and
            BPlusTree.containsPair(2, 11, tree) and
            BPlusTree.containsPair(3, 12, tree) and
            testTreeInvariant(tree)
    }

    @test
    def insert03(): Bool = region rc {
        let tree = BPlusTree.empty(rc, 3);
        BPlusTree.insert(6, 15, tree);
        BPlusTree.insert(5, 14, tree);
        BPlusTree.insert(4, 13, tree);
        BPlusTree.insert(3, 12, tree);
        BPlusTree.insert(2, 11, tree);
        BPlusTree.insert(1, 10, tree);
        BPlusTree.containsPair(1, 10, tree) and
            BPlusTree.containsPair(2, 11, tree) and
            BPlusTree.containsPair(3, 12, tree) and
            BPlusTree.containsPair(4, 13, tree) and
            BPlusTree.containsPair(5, 14, tree) and
            BPlusTree.containsPair(6, 15, tree) and
            testTreeInvariant(tree)
    }

    @test
    def insert04(): Bool = region rc {
        let tree = BPlusTree.empty(rc, 5);
        let pairs = List.map(p -> {
            let (x, y) = p;
            (Int32.modulo(x * 1000, 179), Int32.modulo(y * 1003, 109))
        }, List.zip(List.range(1, 10_000), List.reverse(List.range(1, 10_000))));
        List.forEach(p -> {
            let (x, y) = p;
            BPlusTree.insert(x, y, tree)
        }, pairs);
        Map.forAll(x -> y -> {
            BPlusTree.containsPair(x, y, tree)
        }, listToMap(pairs)) and testTreeInvariant(tree)
    }

    @test
    def insert05(): Bool = region rc {
        List.forAll(order -> {
            let tree = BPlusTree.empty(rc, order);
            let pairs = List.map(p -> {
                let (x, y) = p;
                (Int32.modulo(x * 1000, 179), Int32.modulo(y * 1003, 109))
            }, List.zip(List.range(1, 100_000), List.reverse(List.range(1, 100_000))));
            List.forEach(p -> {
                let (x, y) = p;
                BPlusTree.insert(x, y, tree)
            }, pairs);
            Map.forAll((x, y) -> {
                BPlusTree.containsPair(x, y, tree)
            }, listToMap(pairs)) and testTreeInvariant(tree)
        }, List.range(3, 10))
    }


    @test
    def insert06(): Bool = region rc {
        List.forAll(threads -> {
            let tree: BPlusTree[Int32, Int32, rc]  = BPlusTree.empty(rc, 10);
            let pairsList = Random.handleWithSeed(0i64, () -> Vector.map(_ -> getRandomListOfPairs(10_000 / threads), Vector.range(0, threads)))();
            let _: Unit = region rc2 {
                unchecked_cast(
                    Vector.forEach(list -> spawn unchecked_cast(() ->
                        List.forEach(p -> {
                            let (x, y) = p;
                            BPlusTree.insert(x, y, tree)
                        }, list) as Unit -> Unit \ IO)() @ rc2
                    , pairsList)
                as _ \ rc)
            };
            Vector.forAll(list ->
                List.forAll(p -> {
                    let (x, _) = p;
                    BPlusTree.memberOf(x, tree)
            }, list), pairsList) and testTreeInvariant(tree)
        }, List.range(1, 11))
    }

    /////////////////////////////////////////////////////////////////////////////
    // memberOfThreadUnsafe                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def memberOfUnsafe01(): Bool = region rc {
        let tree = BPlusTree.empty(rc, 3);
        BPlusTree.insert(2, 3, tree);
        BPlusTree.memberOfThreadUnsafe(2, tree)
    }

    @test
    def memberOfUnsafe02(): Bool = region rc {
        let tree = BPlusTree.empty(rc, 3);
        not BPlusTree.memberOfThreadUnsafe(0, tree) and
            not BPlusTree.memberOfThreadUnsafe(1, tree) and
            not BPlusTree.memberOfThreadUnsafe(-1, tree)
    }

    @test
    def memberOfUnsafe03(): Bool = region rc {
        let tree = BPlusTree.empty(rc, 3);
        BPlusTree.insert(1, 2, tree);
        BPlusTree.memberOfThreadUnsafe(1, tree) and
            not BPlusTree.memberOfThreadUnsafe(2, tree) and
            not BPlusTree.memberOfThreadUnsafe(0, tree)
    }

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def memberOf01(): Bool = region rc {
        let tree = BPlusTree.empty(rc, 3);
        BPlusTree.insert(2, 3, tree);
        BPlusTree.memberOf(2, tree)
    }


    @test
    def memberOf02(): Bool = region rc {
        let tree = BPlusTree.empty(rc, 3);
        not BPlusTree.memberOf(0, tree) and
            not BPlusTree.memberOf(1, tree) and
            not BPlusTree.memberOf(-1, tree)
    }

    @test
    def memberOf03(): Bool = region rc {
        let tree = BPlusTree.empty(rc, 3);
        BPlusTree.insert(1, 2, tree);
        BPlusTree.memberOf(1, tree) and
            not BPlusTree.memberOf(2, tree) and
            not BPlusTree.memberOf(0, tree)
    }

    /////////////////////////////////////////////////////////////////////////////
    // containsPair                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def containsPair01(): Bool = region rc {
        let tree = BPlusTree.empty(rc, 3);
        not BPlusTree.containsPair(0, 0, tree) and
            not BPlusTree.containsPair(0, 1, tree) and
            not BPlusTree.containsPair(1, 0, tree)
    }
    @test
    def containsPair04(): Bool = region rc {
        let tree = BPlusTree.empty(rc, 3);
        BPlusTree.insert(1, 2, tree);
        BPlusTree.containsPair(1, 2, tree) and
            not BPlusTree.containsPair(2, 1, tree) and
            not BPlusTree.containsPair(1, 3, tree)
    }

    /////////////////////////////////////////////////////////////////////////////
    // rangeQueryWith                                                          //
    /////////////////////////////////////////////////////////////////////////////
    def rangeQueryDefaultTree(rc: Region[r]): BPlusTree[Int32, Int32, r] \ r =
        let tree: BPlusTree[Int32, Int32, r] = BPlusTree.empty(rc, 3);
        BPlusTree.insert(0, 10, tree);
        BPlusTree.insert(1, 11, tree);
        BPlusTree.insert(2, 12, tree);
        BPlusTree.insert(3, 13, tree);
        BPlusTree.insert(4, 14, tree);
        BPlusTree.insert(5, 15, tree);
        tree

    def rangeQueryMultipleValues(rc: Region[r]): BPlusTree[Int32, Int32, r] \ r =
        let tree = BPlusTree.empty(rc, 3);
        BPlusTree.insert(0, 10, tree);
        BPlusTree.insert(0, 11, tree);
        BPlusTree.insert(0, 12, tree);
        BPlusTree.insert(1, 11, tree);
        BPlusTree.insert(1, 12, tree);
        BPlusTree.insert(2, 13, tree);
        BPlusTree.insert(3, 14, tree);
        tree

    @test
    def rangeQueryWith01(): Bool = region rc {
        let tree = rangeQueryDefaultTree(rc);
        let list = MutList.empty(rc);
        let f = k -> _ -> MutList.push(k, list);
        BPlusTree.rangeQueryWith(f, 0, 5, tree);
        let res = Vector#{0, 1, 2, 3, 4, 5};
        MutList.toVector(list) `Vector.equals` res
    }

    @test
    def rangeQueryWith02(): Bool = region rc {
        let tree = rangeQueryDefaultTree(rc);
        let list = MutList.empty(rc);
        let f = k -> _ -> MutList.push(k, list);
        BPlusTree.rangeQueryWith(f, -2, 5, tree);
        let res = Vector#{0, 1, 2, 3, 4, 5};
        MutList.toVector(list) `Vector.equals` res
    }

    @test
    def rangeQueryWith03(): Bool = region rc {
        let tree = rangeQueryDefaultTree(rc);
        let list = MutList.empty(rc);
        let f = k -> _ -> MutList.push(k, list);
        BPlusTree.rangeQueryWith(f, -2, 7, tree);
        let res = Vector#{0, 1, 2, 3, 4, 5};
        MutList.toVector(list) `Vector.equals` res
    }

    @test
    def rangeQueryWith04(): Bool = region rc {
        let tree = rangeQueryDefaultTree(rc);
        let list = MutList.empty(rc);
        let f = k -> _ -> MutList.push(k, list);
        BPlusTree.rangeQueryWith(f, 1, 1, tree);
        let res = Vector#{1};
        MutList.toVector(list) `Vector.equals` res
    }

    @test
    def rangeQueryWith05(): Bool = region rc {
        let tree = rangeQueryDefaultTree(rc);
        let list = MutList.empty(rc);
        let f = k -> _ -> MutList.push(k, list);
        BPlusTree.rangeQueryWith(f, 1, 3, tree);
        let res = Vector#{1, 2, 3};
        MutList.toVector(list) `Vector.equals` res
    }

    @test
    def rangeQueryWith06(): Bool = region rc {
        let tree = rangeQueryDefaultTree(rc);
        let list = MutList.empty(rc);
        let f = k -> _ -> MutList.push(k, list);
        BPlusTree.rangeQueryWith(f, -2, -2, tree);
        let res = Vector#{ };
        MutList.toVector(list) `Vector.equals` res
    }

    @test
    def rangeQueryWith07(): Bool = region rc {
        let tree = rangeQueryDefaultTree(rc);
        let list = MutList.empty(rc);
        let f = k -> _ -> MutList.push(k, list);
        BPlusTree.rangeQueryWith(f, 7, 7, tree);
        let res = Vector#{ };
        MutList.toVector(list) `Vector.equals` res
    }

    @test
    def rangeQueryWith08(): Bool = region rc {
        let tree = rangeQueryMultipleValues(rc);
        let list = MutList.empty(rc);
        let f = k -> v -> MutList.push((k, v), list);
        BPlusTree.rangeQueryWith(f, 0, 3, tree);
        let res = Vector#{(0, 12), (1, 12), (2, 13), (3, 14)};
        MutList.toVector(list) `Vector.equals` res
    }

    def ptrEqual(o1: v, o2: v): Bool \ r =
        let obj1 = unchecked_cast(o1 as Object \ r);
        let obj2 = unchecked_cast(o2 as Object \ r);
        unchecked_cast(Objects.equals(obj1, obj2) as _ \ r)

    def unwrap(x: Option[a]): a = match x {
        case Some(v) => v
        case None => bug!("Attempted unwrapping None")
    }
}
