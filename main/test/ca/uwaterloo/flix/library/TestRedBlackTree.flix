mod TestRedBlackTree {

    import java.util.Random

    use Assert.{assertEq, assertTrue, assertFalse}
    use RedBlackTree.{Node}
    use RedBlackTree.Color.{Red, Black}

    @Test
    def testSize01(): Unit \ Assert =
        assertEq(expected = 0, RedBlackTree.size(RedBlackTree.empty()))

    @Test
    def testSize02(): Unit \ Assert =
        assertEq(expected = 1, RedBlackTree.size(RedBlackTree.insert(1, (), RedBlackTree.empty())))

    @Test
    def testSize03(): Unit \ Assert =
        assertEq(expected = 3, RedBlackTree.size(toTree(List.range(0, 3))))

    @Test
    def testSize04(): Unit \ Assert =
        assertEq(expected = 7, RedBlackTree.size(toTree(List.range(0, 7))))

    @Test
    def testSize05(): Unit \ Assert =
        assertEq(expected = 100, RedBlackTree.size(toTree(List.range(0, 100))))

    @Test
    def testRemove01(): Unit \ Assert =
        assertFalse(RedBlackTree.memberOf(1, RedBlackTree.remove(1, toTree(List.range(0, 100)))))

    @Test
    def testRemove02(): Unit \ Assert =
        assertFalse(RedBlackTree.memberOf(50, RedBlackTree.remove(50, toTree(List.range(0, 100)))))

    @Test
    def testRemove03(): Unit \ Assert =
        assertFalse(RedBlackTree.memberOf(99, RedBlackTree.remove(99, toTree(List.range(0, 100)))))

    @Test
    def testEmptyAfterRemove01(): Unit \ Assert =
        assertEq(expected = Map.Map(RedBlackTree.empty()), Map.Map(RedBlackTree.remove(42, RedBlackTree.insert(42, (), RedBlackTree.empty()))))

    @Test
    def testEmptyAfterRemove02(): Unit \ Assert =
        assertEq(expected = Map.Map(RedBlackTree.empty()), Map.Map(RedBlackTree.remove(2, RedBlackTree.remove(1,
            RedBlackTree.remove(0, toTree(List.range(0, 3)))))))

    @Test
    def testEmptyAfterRemove03(): Unit \ Assert =
        let tree =
        List.foldLeft((acc, x) -> RedBlackTree.remove(x, acc), toTree(List.range(0, 20)), List.range(0, 20));
        assertEq(expected = Map.Map(RedBlackTree.empty()), Map.Map(tree))

    @Test
    def testBinarySearchTreeInvariant01(): Unit \ Assert =
        assertTrue(checkBinarySearchTreeInvariant(toTree(List.range(0, 10))))

    @Test
    def testBinarySearchTreeInvariant02(): Unit \ Assert =
        assertTrue(checkBinarySearchTreeInvariant(toTree(9 :: 8 :: 7 :: 6 :: 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil)))

    @Test
    def testBinarySearchTreeInvariant03(): Unit \ Assert =
        assertTrue(checkBinarySearchTreeInvariant(toTree(2 :: 7 :: 1 :: 8 :: 0 :: 3 :: 5 :: 4 :: 9 :: 6 :: Nil)))

    @Test
    def testBinarySearchTreeInvariant04(): Unit \ Assert =
        assertTrue(checkBinarySearchTreeInvariant(toTree(List.range(0, 100))))

    @Test
    def testBinarySearchTreeInvariant05(): Unit \ Assert =
        assertTrue(checkBinarySearchTreeInvariant(toTree(List.range(0, 1000))))

    @Test
    def testBinarySearchTreeInvariant06(): Unit \ Assert =
        assertTrue(checkBinarySearchTreeInvariant(RedBlackTree.remove(0, toTree(List.range(0, 100)))))

    @Test
    def testBinarySearchTreeInvariant07(): Unit \ Assert =
        assertTrue(checkBinarySearchTreeInvariant(RedBlackTree.remove(49, toTree(List.range(0, 100)))))

    @Test
    def testBinarySearchTreeInvariant08(): Unit \ Assert =
        assertTrue(checkBinarySearchTreeInvariant(RedBlackTree.remove(99, toTree(List.range(0, 100)))))

    @Test
    def testBinarySearchTreeInvariant09(): Unit \ Assert =
        let tree =
        List.foldLeft((acc, x) -> RedBlackTree.remove(x, acc), toTree(List.range(0, 20)),
            12 :: 11 :: 80 :: 93 :: 21 :: 71 :: 39 :: 32 :: 29 :: 95 :: Nil);
        assertTrue(checkBinarySearchTreeInvariant(tree))

    @Test
    def testBinarySearchTreeInvariant10(): Unit \ {Assert, IO} = assertTrue(checkBinarySearchTreeInvariant(random(10)))

    @Test
    def testBinarySearchTreeInvariant11(): Unit \ {Assert, IO} = assertTrue(checkBinarySearchTreeInvariant(random(100)))

    @Test
    def testBinarySearchTreeInvariant12(): Unit \ {Assert, IO} = assertTrue(checkBinarySearchTreeInvariant(random(1000)))

    @Test
    def testRedParentInvariant01(): Unit \ Assert =
        assertTrue(checkRedParentInvariant(toTree(List.range(0, 10))))

    @Test
    def testRedParentInvariant02(): Unit \ Assert =
        assertTrue(checkRedParentInvariant(toTree(9 :: 8 :: 7 :: 6 :: 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil)))

    @Test
    def testRedParentInvariant03(): Unit \ Assert =
        assertTrue(checkRedParentInvariant(toTree(2 :: 7 :: 1 :: 8 :: 0 :: 3 :: 5 :: 4 :: 9 :: 6 :: Nil)))

    @Test
    def testRedParentInvariant04(): Unit \ Assert =
        assertTrue(checkRedParentInvariant(toTree(List.range(0, 100))))

    @Test
    def testRedParentInvariant05(): Unit \ Assert =
        assertTrue(checkRedParentInvariant(toTree(List.range(0, 1000))))

    @Test
    def testRedParentInvariant06(): Unit \ Assert =
        assertTrue(checkRedParentInvariant(RedBlackTree.remove(0, toTree(List.range(0, 100)))))

    @Test
    def testRedParentInvariant07(): Unit \ Assert =
        assertTrue(checkRedParentInvariant(RedBlackTree.remove(49, toTree(List.range(0, 100)))))

    @Test
    def testRedParentInvariant08(): Unit \ Assert =
        assertTrue(checkRedParentInvariant(RedBlackTree.remove(99, toTree(List.range(0, 100)))))

    @Test
    def testRedParentInvariant09(): Unit \ Assert =
        let tree =
        List.foldLeft((acc, x) -> RedBlackTree.remove(x, acc), toTree(List.range(0, 20)),
            12 :: 11 :: 80 :: 93 :: 21 :: 71 :: 39 :: 32 :: 29 :: 95 :: Nil);
        assertTrue(checkRedParentInvariant(tree))

    @Test
    def testRedParentInvariant10(): Unit \ {Assert, IO} = assertTrue(checkRedParentInvariant(random(10)))

    @Test
    def testRedParentInvariant11(): Unit \ {Assert, IO} = assertTrue(checkRedParentInvariant(random(100)))

    @Test
    def testRedParentInvariant12(): Unit \ {Assert, IO} = assertTrue(checkRedParentInvariant(random(1000)))

    ///
    /// Returns `true` if and only if the binary search tree invariant holds for `tree`.
    ///
    def checkBinarySearchTreeInvariant(tree: RedBlackTree[k, v]): Bool with Order[k] = match tree {
        case Node(_, a, xK, _, b) =>
            if (RedBlackTree.exists((yK, _) -> (yK <=> xK) == Comparison.GreaterThan, a) or RedBlackTree.exists((zK, _) -> (zK <=> xK) == Comparison.LessThan, b))
                false
            else
                checkBinarySearchTreeInvariant(a) and checkBinarySearchTreeInvariant(b)
        case _ => true
    }

    ///
    /// Returns true if `c` is equal to `Red`.
    ///
    def isRed(c: RedBlackTree.Color): Bool = match c {
        case Red => true
        case _   => false
    }

    ///
    /// Returns `true` if and only if no red node in `tree` has a red parent.
    ///
    def checkRedParentInvariant(tree: RedBlackTree[k, v]): Bool =
       def loop(t, c) = match t {
           case Node(Red, a, _, _, b)   =>
                if (isRed(c))
                    false
                else
                    loop(a, Red) and loop(b, Red)
           case Node(Black, a, _, _, b) =>
                loop(a, Black) and loop(b, Black)
           case _                       => true
       };
       loop(tree, Black)

    ///
    /// Returns a tree with mappings `x -> ()` for all `x` in `xs`.
    ///
    def toTree(xs: List[Int32]): RedBlackTree[Int32, Unit]
        = List.foldLeft((acc, x) -> RedBlackTree.insert(x, (), acc), RedBlackTree.empty(), xs)
    ///
    /// Returns a tree with `n` nodes `x -> ()` where `x` is a random integer.
    ///
    def random(n: Int32): RedBlackTree[Int32, Unit] \ IO = {
        let rnd = new Random();
        List.foldLeft((acc, x) -> RedBlackTree.insert(x, (), acc), RedBlackTree.empty(),
            List.map(_ -> rnd.nextInt(), List.range(0, n)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // insert                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def insert01(): Unit \ Assert =
        let tree = (RedBlackTree.empty(): RedBlackTree[Int32, Int32]) |>
            RedBlackTree.insert(0, 1);
        assertEq(expected = Some(1), RedBlackTree.get(0, tree));
        assertEq(expected = None, RedBlackTree.get(1, tree))

    @Test
    def insert02(): Unit \ Assert =
        let tree = (RedBlackTree.empty(): RedBlackTree[Int32, Int32]) |>
            RedBlackTree.insert(0, 1) |>
            RedBlackTree.insert(1, 2);
        assertEq(expected = Some(1), RedBlackTree.get(0, tree));
        assertEq(expected = Some(2), RedBlackTree.get(1, tree));
        assertEq(expected = None, RedBlackTree.get(2, tree))

    @Test
    def insert03(): Unit \ Assert =
        let inserts = Vector#{(0, 1), (3, 4), (4, 5), (1, 2), (8, 9), (6, 7), (2, 3), (10, 11), (9, 10)};
        let tree = inserts |> Vector.foldRight(match (k, v) -> RedBlackTree.insert(k, v), RedBlackTree.empty());
        assertTrue(inserts |> Vector.forAll(match (k, v) -> RedBlackTree.get(k, tree) == Some(v)))

    /////////////////////////////////////////////////////////////////////////////
    // insertWith                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def insertWith01(): Unit \ Assert =
        let tree = (RedBlackTree.empty(): RedBlackTree[Int32, Int32]) |>
            RedBlackTree.insertWith(_k -> _v -> _v1 -> -1, 0, 1);
        assertEq(expected = Some(1), RedBlackTree.get(0, tree));
        assertEq(expected = None, RedBlackTree.get(1, tree))

    @Test
    def insertWith02(): Unit \ Assert =
        let tree = (RedBlackTree.empty(): RedBlackTree[Int32, Int32]) |>
            RedBlackTree.insert(1, 10) |>
            RedBlackTree.insertWith(k -> v -> v1 -> k + v + v1, 1, 100);
        assertEq(expected = Some(1 + 10 + 100), RedBlackTree.get(1, tree));
        assertEq(expected = None, RedBlackTree.get(0, tree))

    @Test
    def insertWith03(): Unit \ Assert =
        let inserts = Vector#{(0, 1), (3, 4), (4, 5), (1, 2), (8, 9), (6, 7), (2, 3), (10, 11), (9, 10)};
        let (kept, overWrite) = Vector.splitAt(Vector.length(inserts), inserts);
        let treeWithInserts = inserts |> Vector.foldRight(match (k, v) -> RedBlackTree.insert(k, v), RedBlackTree.empty());
        let treeWithUpdates = overWrite |> Vector.foldRight(match (k, _) -> RedBlackTree.insertWith(k_ -> v -> v1 -> k_ + v + v1, -1, k), treeWithInserts);
        assertTrue(kept |> Vector.forAll(match (k, v) -> RedBlackTree.get(k, treeWithUpdates) == Some(v)));
        assertTrue(overWrite |> Vector.forAll(match (k, v) -> RedBlackTree.get(k, treeWithUpdates) == Some(k + v - 1)))

    /////////////////////////////////////////////////////////////////////////////
    // updateWith                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def updateWith01(): Unit \ Assert =
        let tree = (RedBlackTree.empty(): RedBlackTree[Int32, Int32]) |>
            RedBlackTree.updateWith(_k -> _v -> Some(1), 0);
        assertEq(expected = None, RedBlackTree.get(0, tree));
        assertEq(expected = None, RedBlackTree.get(1, tree))

    @Test
    def updateWith02(): Unit \ Assert =
        let tree = (RedBlackTree.empty(): RedBlackTree[Int32, Int32]) |>
            RedBlackTree.insert(1, 10) |>
            RedBlackTree.insert(2, 11) |>
            RedBlackTree.updateWith(k -> v -> Some(k + v), 1) |>
            RedBlackTree.updateWith(k -> v -> Some(k + v), 3) |>
            RedBlackTree.updateWith(_ -> _ -> None, 1);
        assertEq(expected = Some(1 + 10), RedBlackTree.get(1, tree));
        assertEq(expected = Some(11), RedBlackTree.get(2, tree));
        assertEq(expected = None, RedBlackTree.get(3, tree));
        assertEq(expected = None, RedBlackTree.get(10, tree));
        assertEq(expected = None, RedBlackTree.get(11, tree))

    @Test
    def updateWith03(): Unit \ Assert =
        let inserts = Vector#{(0, 1), (3, 4), (4, 5), (1, 2), (8, 9), (6, 7), (2, 3), (10, 11), (9, 10)};
        let nonReplacingUpdates = Vector#{(11, 12), (12, 13), (13, 14), (-1, -2), (-2, -3)};
        let (kept, overWrite) = Vector.splitAt(Vector.length(inserts), inserts);
        let updates = Vector.append(nonReplacingUpdates, overWrite);
        let treeWithInserts = inserts |> Vector.foldRight(match (k, v) -> RedBlackTree.insert(k, v), RedBlackTree.empty());
        let treeWithUpdates = updates |> Vector.foldRight(match (k, _) -> RedBlackTree.updateWith(k_ -> v -> Some(k_ + v), k), treeWithInserts);
        assertTrue(kept |> Vector.forAll(match (k, v) -> RedBlackTree.get(k, treeWithUpdates) == Some(v)));
        assertTrue(overWrite |> Vector.forAll(match (k, v) -> RedBlackTree.get(k, treeWithUpdates) == Some(k + v)));
        assertTrue(nonReplacingUpdates |> Vector.forAll(match (k, _) -> RedBlackTree.get(k, treeWithUpdates) == None))

    /////////////////////////////////////////////////////////////////////////////
    // parMapWithKey                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapWithKey01(): Unit \ Assert =
        let Map.Map(t) = (Map#{}: Map[Unit, Unit]);
        let m = Map.Map(RedBlackTree.mapWithKey((_, v) -> v, t));
        assertEq(expected = Map#{}, m)

    @Test
    def mapWithKey02(): Unit \ Assert =
        let Map.Map(t) = Map#{1 => 4};
        let m = Map.Map(RedBlackTree.mapWithKey((k, v) -> k + v, t));
        assertEq(expected = Map#{1 => 5}, m)

    @Test
    def mapWithKey03(): Unit \ Assert =
        let Map.Map(t) = Map#{2 => -1, 0 => 0};
        let m = Map.Map(RedBlackTree.mapWithKey((k, v) -> k + v, t));
        assertEq(expected = Map#{2 => 1, 0 => 0}, m)

    @Test
    def mapWithKey04(): Unit \ Assert =
        let Map.Map(t) = Map#{2 => -1, 5 => 15, 11 => -9};
        let m = Map.Map(RedBlackTree.mapWithKey((k, v) -> k + v, t));
        assertEq(expected = Map#{2 => 1, 5 => 20, 11 => 2}, m)

    @Test
    def mapWithKey05(): Unit \ Assert =
        let Map.Map(t) = Map#{2 => -1, 5 => 15, 11 => -9, 8 => 8};
        let m = Map.Map(RedBlackTree.mapWithKey((k, v) -> k + v, t));
        assertEq(expected = Map#{2 => 1, 5 => 20, 11 => 2, 8 => 16}, m)

    @Test
    def mapWithKey06(): Unit \ Assert = region rc {
        let range = List.range(0, 2000);
        let m = List.zip(range, range) |> List.toMap;
        let a = MutList.empty(rc);
        discard Map.mapWithKey((k, v) -> { let b = MutList.empty(rc); MutList.push(k, b); MutList.append(MutList.toList(b), a); v }, m);
        assertEq(expected = range, MutList.toList(a))
    }

    @Test
    def mapWithKey07(): Unit \ Assert =
        let Map.Map(t) = List.zip(List.range(0, 2000), List.range(0, 2000)) |> List.toMap;
        let expected = List.zip(List.range(0, 2000), List.range(1, 2001)) |> List.toMap;
        assertEq(expected = expected, Map.Map(RedBlackTree.mapWithKey((_, v) -> v + 1, t)))

    @Test
    def mapWithKey08(): Unit \ Assert =
        let Map.Map(t) = List.zip(List.range(0, 2000), List.range(-1000, 1000)) |> List.toMap;
        let expected = List.zip(List.range(0, 2000), List.range(-999, 1001)) |> List.toMap;
        assertEq(expected = expected, Map.Map(RedBlackTree.mapWithKey((_, v) -> v + 1, t)))


    /////////////////////////////////////////////////////////////////////////////
    // parCount                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def parCount01(): Unit \ Assert =
        let Map.Map(t) = List.zip(List.range(0, 2000), List.range(0, 2000)) |> List.toMap;
        assertEq(expected = 2000, RedBlackTree.parCount((k, v) -> k == v, t))

    @Test
    def parCount02(): Unit \ Assert =
        let Map.Map(t) = List.zip(List.range(0, 2000), List.range(-1000, 1000)) |> List.toMap;
        assertEq(expected = 2000, RedBlackTree.parCount((k, v) -> k - 1000 == v, t))


    /////////////////////////////////////////////////////////////////////////////
    // parMinimumBy                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def parMinimumBy01(): Unit \ Assert =
        let range = List.range(0, 2000);
        let Map.Map(t) = List.zip(range, range) |> List.toMap;
        assertEq(expected = Some((0, 0)), RedBlackTree.parMinimumBy((_, x, _, y) -> x <=> y, t))

    @Test
    def parMinimumBy02(): Unit \ Assert =
        let range = List.range(0, 2000);
        let Map.Map(t) = List.zip(range, List.reverse(range)) |> List.toMap;
        assertEq(expected = Some((1999, 0)), RedBlackTree.parMinimumBy((_, x, _, y) -> x <=> y, t))


    /////////////////////////////////////////////////////////////////////////////
    // parMaximumBy                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def parMaximumBy01(): Unit \ Assert =
        let range = List.range(0, 2000);
        let Map.Map(t) = List.zip(range, range) |> List.toMap;
        assertEq(expected = Some((1999, 1999)), RedBlackTree.parMaximumBy((_, x, _, y) -> x <=> y, t))

    @Test
    def parMaximumBy02(): Unit \ Assert =
        let range = List.range(0, 2000);
        let Map.Map(t) = List.zip(range, List.reverse(range)) |> List.toMap;
        assertEq(expected = Some((0, 1999)), RedBlackTree.parMaximumBy((_, x, _, y) -> x <=> y, t))


    /////////////////////////////////////////////////////////////////////////////
    // eq                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    def toRedBlackTree(xs: List[(k, v)]): RedBlackTree[k, v] with Order[k] =
        List.foldLeft((acc, kv) -> {let (k,v) = kv; RedBlackTree.insert(k, v, acc)}, RedBlackTree.empty(), xs)

    @Test
    def eq01(): Unit \ Assert =
        assertTrue((RedBlackTree.empty(): RedBlackTree[Int32, Int32]) == (RedBlackTree.empty(): RedBlackTree[Int32, Int32]))

    @Test
    def eq02(): Unit \ Assert =
        assertTrue((toRedBlackTree(Nil): RedBlackTree[Int32, Int32]) == (toRedBlackTree(Nil): RedBlackTree[Int32, Int32]))

    @Test
    def eq03(): Unit \ Assert =
        assertTrue(toRedBlackTree((1, 1) :: Nil) == toRedBlackTree((1, 1) :: Nil))

    @Test
    def neq01(): Unit \ Assert =
        assertTrue(toRedBlackTree((1, 1) :: Nil) != RedBlackTree.empty())


    /////////////////////////////////////////////////////////////////////////////
    // parExists                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def parExists01(): Unit \ Assert =
        let l = List.range(1, 2001);
        let Map.Map(t) = List.zip(l, l) |> List.toMap;
        assertTrue(RedBlackTree.parExists((k, v) -> k == 1002 and v == 1002, t))

    @Test
    def parExists02(): Unit \ Assert =
        let l1 = List.range(1, 2001);
        let l2 = List.range(-2000, 0);
        let Map.Map(t) = List.zip(l1, l2) |> List.toMap;
        assertTrue(RedBlackTree.parExists((k, v) -> k == 1002 and v == -999, t))

    @Test
    def parExists03(): Unit \ Assert =
        let l = List.range(1, 2001);
        let Map.Map(t) = List.zip(l, l) |> List.toMap;
        assertFalse(RedBlackTree.parExists((k, v) -> k < 0 or v > 3000, t))


    /////////////////////////////////////////////////////////////////////////////
    // parForAll                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def parForAll01(): Unit \ Assert =
        let l = List.range(1, 2001);
        let Map.Map(t) = List.zip(l, l) |> List.toMap;
        assertTrue(RedBlackTree.parForAll((k, v) -> k == v, t))

    @Test
    def parForAll02(): Unit \ Assert =
        let l = List.range(1, 2001);
        let Map.Map(t) = List.zip(l, l) |> List.toMap;
        assertFalse(RedBlackTree.parForAll((k, _) -> k == 1, t))

    @Test
    def parForAll03(): Unit \ Assert =
        let l1 = List.range(1, 2001);
        let l2 = List.range(-2000, 0);
        let Map.Map(t) = List.zip(l1, l2) |> List.toMap;
        assertTrue(RedBlackTree.parForAll((k, v) -> k > v, t))

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 21);
        let Map.Map(t) = Map#{};
        RedBlackTree.forEach((k, _) -> Ref.put(k, x), t);
        assertEq(expected = 21, Ref.get(x))
    }

    @Test
    def forEach02(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 21);
        let Map.Map(t) = Map#{1 => "Hello World!"};
        RedBlackTree.forEach((k, _) -> Ref.put(k, x), t);
        assertEq(expected = 1, Ref.get(x))
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEachWithIndex01(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 21);
        let Map.Map(t) = Map#{};
        RedBlackTree.forEachWithIndex((i, _, _) -> Ref.put(i, x), t);
        assertEq(expected = 21, Ref.get(x))
    }

    @Test
    def forEachWithIndex02(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 21);
        let Map.Map(t) = Map#{1 => "Hello World!"};
        RedBlackTree.forEachWithIndex((i, _, _) -> Ref.put(i, x), t);
        assertEq(expected = 0, Ref.get(x))
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldMapWithKey                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldMap01(): Unit \ Assert =
        let Map.Map(t) = Map#{};
        assertEq(expected = 0, RedBlackTree.foldMap((k, v) -> k + v, t))

    @Test
    def foldMap02(): Unit \ Assert =
        let Map.Map(t) = Map#{1 => 2, 3 => 5};
        assertEq(expected = (1 + 2 + 3 + 5), RedBlackTree.foldMap((k, v) -> k + v, t))

    @Test
    def foldMap03(): Unit \ Assert =
        let Map.Map(t) = Map#{1 => 2, 3 => 5};
        assertEq(expected = (2 * (1 + 2) + 2 * (3 + 5)), RedBlackTree.foldMap((k, v) -> 2 * (k + v), t))

    @Test
    def foldMap04(): Unit \ Assert =
        let Map.Map(t) = Map#{1 => "a", 2 => "b"};
        assertEq(expected = "1a2b", RedBlackTree.foldMap((k, v) -> "${k}" + v, t))


    /////////////////////////////////////////////////////////////////////////////
    // sumValues                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumValues01(): Unit \ Assert =
        assertEq(expected = 0, RedBlackTree.empty() |> RedBlackTree.sumValues)

    @Test
    def sumValues02(): Unit \ Assert =
        let Map.Map(t) = Map#{1 => 1};
        assertEq(expected = 1, RedBlackTree.sumValues(t))

    @Test
    def sumValues03(): Unit \ Assert =
        let Map.Map(t) = Map#{1 => 1, 2 => 2, 3 => 3};
        assertEq(expected = 6, RedBlackTree.sumValues(t))

    @Test
    def sumValues04(): Unit \ Assert =
        let Map.Map(t) = Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3};
        assertEq(expected = 3, RedBlackTree.sumValues(t))

    @Test
    def sumValues05(): Unit \ Assert =
        let Map.Map(t) = Map#{-1 => -1, -2 => -2, -3 => -3, -4 => -4};
        assertEq(expected = -10, RedBlackTree.sumValues(t))

    @Test
    def sumValues06(): Unit \ Assert =
        let Map.Map(t) = Map#{10 => 10, -10 => -10};
        assertEq(expected = 0, RedBlackTree.sumValues(t))

    @Test
    def sumValues07(): Unit \ Assert =
        let Map.Map(t) = List.range(1, 101) |> List.zip(List.range(1, 101)) |> List.toMap;
        assertEq(expected = 5050, RedBlackTree.sumValues(t))


    /////////////////////////////////////////////////////////////////////////////
    // sumKeys                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumKeys01(): Unit \ Assert =
        assertEq(expected = 0, RedBlackTree.empty() |> RedBlackTree.sumKeys)

    @Test
    def sumKeys02(): Unit \ Assert =
        let Map.Map(t) = Map#{1 => 1};
        assertEq(expected = 1, RedBlackTree.sumKeys(t))

    @Test
    def sumKeys03(): Unit \ Assert =
        let Map.Map(t) = Map#{1 => 1, 2 => 2, 3 => 3};
        assertEq(expected = 6, RedBlackTree.sumKeys(t))

    @Test
    def sumKeys04(): Unit \ Assert =
        let Map.Map(t) = Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3};
        assertEq(expected = 3, RedBlackTree.sumKeys(t))

    @Test
    def sumKeys05(): Unit \ Assert =
        let Map.Map(t) = Map#{-1 => -1, -2 => -2, -5 => -3, -4 => -4};
        assertEq(expected = -12, RedBlackTree.sumKeys(t))

    @Test
    def sumKeys06(): Unit \ Assert =
        let Map.Map(t) = Map#{10 => 10, -10 => -10};
        assertEq(expected = 0, RedBlackTree.sumKeys(t))

    @Test
    def sumKeys07(): Unit \ Assert =
        let Map.Map(t) = List.range(1, 101) |> List.zip(List.range(1, 101)) |> List.toMap;
        assertEq(expected = 5050, RedBlackTree.sumKeys(t))


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Unit \ Assert =
        assertEq(expected = 0, RedBlackTree.empty() |> RedBlackTree.sumWith((k, v) -> k + v))

    @Test
    def sumWith02(): Unit \ Assert =
        let Map.Map(t) = Map#{1 => 1};
        assertEq(expected = 2, RedBlackTree.sumWith((k, v) -> k + v, t))

    @Test
    def sumWith03(): Unit \ Assert =
        let Map.Map(t) = Map#{1 => 1, 2 => 2, 3 => 3};
        assertEq(expected = 12, RedBlackTree.sumWith((k, v) -> k + v, t))

    @Test
    def sumWith04(): Unit \ Assert =
        let Map.Map(t) = Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3};
        assertEq(expected = 6, RedBlackTree.sumWith((k, v) -> k + v, t))

    @Test
    def sumWith05(): Unit \ Assert =
        let Map.Map(t) = Map#{-1 => -1, -2 => -2, -3 => -3, -4 => -4};
        assertEq(expected = -20, RedBlackTree.sumWith((k, v) -> k + v, t))

    @Test
    def sumWith06(): Unit \ Assert =
        let Map.Map(t) = Map#{10 => 10, -10 => -10};
        assertEq(expected = 0, RedBlackTree.sumWith((k, v) -> k + v, t))


    /////////////////////////////////////////////////////////////////////////////
    // parSumWith                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def parSumWith01(): Unit \ Assert =
        let Map.Map(t) = List.zip(List.range(0, 2000), List.range(0, 2000)) |> List.toMap;
        assertEq(expected = 3998000, RedBlackTree.parSumWith((k, v) -> k + v, t))

    @Test
    def parSumWith02(): Unit \ Assert =
        let Map.Map(t) = List.zip(List.range(0, 2000), List.range(-1000, 1000)) |> List.toMap;
        assertEq(expected = 1998000, RedBlackTree.parSumWith((k, v) -> k + v, t))


    /////////////////////////////////////////////////////////////////////////////
    // joinKeys                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinKeys01(): Unit \ Assert =
        assertEq(expected = "", (RedBlackTree.empty(): RedBlackTree[Int32, Int32]) |> RedBlackTree.joinKeys(","))

    @Test
    def joinKeys02(): Unit \ Assert =
        let Map.Map(t) = Map#{1 => 1};
        assertEq(expected = "1", RedBlackTree.joinKeys(",", t))

    @Test
    def joinKeys03(): Unit \ Assert =
        let Map.Map(t) = Map#{0 => 1, 1 => 2, 2 => 2};
        assertEq(expected = "0,1,2", RedBlackTree.joinKeys(",", t))

    @Test
    def joinKeys04(): Unit \ Assert =
        let Map.Map(t) = Map#{"0" => 1, "1" => 2, "2" => 2};
        assertEq(expected = "0,1,2", RedBlackTree.joinKeys(",", t))


    /////////////////////////////////////////////////////////////////////////////
    // joinValues                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinValues01(): Unit \ Assert =
        assertEq(expected = "", (RedBlackTree.empty(): RedBlackTree[Int32, Int32]) |> RedBlackTree.joinValues(","))

    @Test
    def joinValues02(): Unit \ Assert =
        let Map.Map(t) = Map#{1 => 1};
        assertEq(expected = "1", RedBlackTree.joinValues(",", t))

    @Test
    def joinValues03(): Unit \ Assert =
        let Map.Map(t) = Map#{0 => 1, 1 => 2, 2 => 2};
        assertEq(expected = "1,2,2", RedBlackTree.joinValues(",", t))

    @Test
    def joinValues04(): Unit \ Assert =
        let Map.Map(t) = Map#{0 => "1", 1 => "2", 2 => "2"};
        assertEq(expected = "1,2,2", RedBlackTree.joinValues(",", t))


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinWith01(): Unit \ Assert =
        assertEq(expected = "", (RedBlackTree.empty(): RedBlackTree[Int32, Int32]) |> RedBlackTree.joinWith((k, v) -> "${k} => ${v}", ","))

    @Test
    def joinWith02(): Unit \ Assert =
        let Map.Map(t) = Map#{1 => 1};
        assertEq(expected = "1 => 1", RedBlackTree.joinWith((k, v) -> "${k} => ${v}", ", ", t))

    @Test
    def joinWith03(): Unit \ Assert =
        let Map.Map(t) = Map#{0 => 1, 1 => 2, 2 => 2};
        assertEq(expected = "0 => 1, 1 => 2, 2 => 2", RedBlackTree.joinWith((k, v) -> "${k} => ${v}", ", ", t))

    @Test
    def joinWith04(): Unit \ Assert =
        let Map.Map(t) = Map#{0 => "1", 1 => "2", 2 => "2"};
        assertEq(expected = "0 => 1, 1 => 2, 2 => 2", RedBlackTree.joinWith((k, v) -> "${k} => ${v}", ", ", t))

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filter01(): Unit \ Assert =
        assertTrue(RedBlackTree.filter(x -> x > 1, (RedBlackTree.empty(): RedBlackTree[Int32, Int32])) == RedBlackTree.empty())

    @Test
    def filter02(): Unit \ Assert =
        assertTrue(RedBlackTree.filter(x -> x > 1, RedBlackTree.empty() |> RedBlackTree.insert(1, 1)) == RedBlackTree.empty())

    @Test
    def filter03(): Unit \ Assert =
        assertEq(expected = (2, 2) :: (3, 3) :: (4, 4) :: Nil, RedBlackTree.filter(x -> x > 1, RedBlackTree.empty() |> RedBlackTree.insert(1, 1) |> RedBlackTree.insert(2, 2) |> RedBlackTree.insert(3, 3) |> RedBlackTree.insert(4, 4))
            |> RedBlackTree.toList |> List.sort)

    @Test
    def filter04(): Unit \ Assert =
        assertEq(expected = (2, 2) :: (3, 3) :: (4, 4) :: Nil, Filterable.filter(x -> x > 1, RedBlackTree.empty() |> RedBlackTree.insert(1, 1) |> RedBlackTree.insert(2, 2) |> RedBlackTree.insert(3, 3) |> RedBlackTree.insert(4, 4))
                    |> RedBlackTree.toList |> List.sort)


    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMap01(): Unit \ Assert =
        assertTrue(RedBlackTree.filterMap(x -> if (x > 1) Some("${x}") else None, (RedBlackTree.empty(): RedBlackTree[Int32, Int32]))
            == RedBlackTree.empty())

    @Test
    def filterMap02(): Unit \ Assert =
        assertTrue(RedBlackTree.filterMap(x -> if (x > 1) Some("${x}") else None, RedBlackTree.empty() |> RedBlackTree.insert(1, 1))
            == RedBlackTree.empty())

    @Test
    def filterMap03(): Unit \ Assert =
        assertEq(expected = (2, "2") :: (3, "3") :: (4, "4") :: Nil, RedBlackTree.filterMap(x -> if (x > 1) Some("${x}") else None, RedBlackTree.empty() |> RedBlackTree.insert(1, 1) |> RedBlackTree.insert(2, 2) |> RedBlackTree.insert(3, 3) |> RedBlackTree.insert(4, 4))
            |> RedBlackTree.toList |> List.sort)

    @Test
    def filterMap04(): Unit \ Assert =
        assertEq(expected = (2, "2") :: (3, "3") :: (4, "4") :: Nil, Filterable.filterMap(x -> if (x > 1) Some("${x}") else None, RedBlackTree.empty() |> RedBlackTree.insert(1, 1) |> RedBlackTree.insert(2, 2) |> RedBlackTree.insert(3, 3) |> RedBlackTree.insert(4, 4))
            |> RedBlackTree.toList |> List.sort)


    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMutDeque01(): Unit \ Assert = region rc {
        let t = (RedBlackTree.empty(): RedBlackTree[Int32, Int32]);
        let d1 = RedBlackTree.toMutDeque(rc, t);

        let d2 = MutDeque.empty(rc);

        assertTrue(d1 `MutDeque.sameElements` d2)
    }

    @Test
    def toMutDeque02(): Unit \ Assert = region rc {
        let t = RedBlackTree.empty() |> RedBlackTree.insert(1, 1);
        let d1 = RedBlackTree.toMutDeque(rc, t);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushBack((1, 1), d2);

        assertTrue(d1 `MutDeque.sameElements` d2)
    }

    @Test
    def toMutDeque03(): Unit \ Assert = region rc {
        let t = RedBlackTree.empty() |> RedBlackTree.insert(1, 1) |> RedBlackTree.insert(2, 2)
                                     |> RedBlackTree.insert(3, 3) |> RedBlackTree.insert(4, 4);
        let d1 = RedBlackTree.toMutDeque(rc, t);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushBack((2, 2), d2);
        MutDeque.pushBack((3, 3), d2);
        MutDeque.pushBack((4, 4), d2);
        MutDeque.pushFront((1, 1), d2);

        assertTrue(d1 `MutDeque.sameElements` d2)
    }

    @Test
    def toMutDeque04(): Unit \ Assert = region rc {
        let t = RedBlackTree.empty() |> RedBlackTree.insert(1, 'a') |> RedBlackTree.insert(2, 'b')
                                     |> RedBlackTree.insert(3, 'c') |> RedBlackTree.insert(4, 'd');
        let d1 = RedBlackTree.toMutDeque(rc, t);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushFront((4, 'd'), d2);
        MutDeque.pushFront((3, 'c'), d2);
        MutDeque.pushFront((2, 'b'), d2);
        MutDeque.pushFront((1, 'a'), d2);

        assertTrue(d1 `MutDeque.sameElements` d2)
    }


    /////////////////////////////////////////////////////////////////////////////
    // mapAWithKey (aka traverseWithKey)                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapAWithKey01(): Unit \ Assert = region rc {
        let st = Ref.fresh(rc, '#');
        let t: RedBlackTree[Int32, Char] = RedBlackTree.empty();
        let ans = RedBlackTree.mapAWithKey((_, x) -> {Ref.put(x, st); Identity.Identity(x)}, t);
        assertTrue(ans == Identity.Identity(RedBlackTree.empty()));
        assertEq(expected = '#', Ref.get(st))
    }

    @Test
    def mapAWithKey02(): Unit \ Assert = region rc {
        let st = Ref.fresh(rc, '#');
        let t = RedBlackTree.empty() |> RedBlackTree.insert(1, 'a');
        let ans = RedBlackTree.mapAWithKey((_, x) -> {Ref.put(x, st); Identity.Identity(x)}, t);
        assertTrue(ans == Identity.Identity(RedBlackTree.empty() |> RedBlackTree.insert(1, 'a')));
        assertEq(expected = 'a', Ref.get(st))
    }

    @Test
    def mapAWithKey03(): Unit \ Assert = region rc {
        let st = Ref.fresh(rc, '#');
        let t = RedBlackTree.empty() |> RedBlackTree.insert(1, 'a') |> RedBlackTree.insert(2, 'b');
        let ans = RedBlackTree.mapAWithKey((_, x) -> {Ref.put(x, st); Identity.Identity(x)}, t);
        assertTrue(ans == Identity.Identity(RedBlackTree.empty() |> RedBlackTree.insert(1, 'a') |> RedBlackTree.insert(2, 'b')));
        assertEq(expected = 'b', Ref.get(st))
    }

    @Test
    def mapAWithKey04(): Unit \ Assert = region rc {
        let st = Ref.fresh(rc, '#');
        let t = RedBlackTree.empty() |> RedBlackTree.insert(1, 'a') |> RedBlackTree.insert(2, 'b') |> RedBlackTree.insert(3, 'c');
        let ans = RedBlackTree.mapAWithKey((_, x) -> {Ref.put(x, st); Identity.Identity(x)}, t);
        assertTrue(ans == Identity.Identity(RedBlackTree.empty()
                            |> RedBlackTree.insert(1, 'a')
                            |> RedBlackTree.insert(2, 'b') |> RedBlackTree.insert(3, 'c')));
        assertEq(expected = 'c', Ref.get(st))
    }

    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Unit \ Assert = region rc {
        assertEq(expected = Nil, (RedBlackTree.empty(): RedBlackTree[Int32, Int32]) |> RedBlackTree.iterator(rc) |> Iterator.toList)
    }

    @Test
    def iterator02(): Unit \ Assert = region rc {
        let t = RedBlackTree.empty() |> RedBlackTree.insert(1, 'a') |> RedBlackTree.insert(2, 'b')
                                     |> RedBlackTree.insert(3, 'c') |> RedBlackTree.insert(4, 'd');
        assertEq(expected = (1, 'a') :: (2, 'b') :: (3, 'c') :: (4, 'd') :: Nil, RedBlackTree.iterator(rc, t) |> Iterator.toList)
    }

    @Test
    def iterator03(): Unit \ Assert = region rc {
        let t = RedBlackTree.empty() |> RedBlackTree.insert(4, 'd') |> RedBlackTree.insert(3, 'c')
                                     |> RedBlackTree.insert(2, 'b') |> RedBlackTree.insert(1, 'a');
        assertEq(expected = (1, 'a') :: (2, 'b') :: (3, 'c') :: (4, 'd') :: Nil, RedBlackTree.iterator(rc, t) |> Iterator.toList)
    }

    @Test
    def iterator04(): Unit \ Assert = region rc {
        let l = List.range(1, 100);
        let t = List.foldLeft((ac, i) -> RedBlackTree.insert(i, i+100, ac), RedBlackTree.empty(), l);
        assertEq(expected = List.map(i -> (i, i+100), List.range(1, 100)), RedBlackTree.iterator(rc, t) |> Iterator.toList)
    }

}
