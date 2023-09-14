mod TestRedBlackTree {

    use RedBlackTree.{Node}
    use RedBlackTree.Color.{Red, Black}

    @Test
    def testSize01(): Bool =
        RedBlackTree.size(RedBlackTree.empty()) == 0

    @Test
    def testSize02(): Bool =
        RedBlackTree.size(RedBlackTree.insert(1, (), RedBlackTree.empty())) == 1

    @Test
    def testSize03(): Bool =
        RedBlackTree.size(toTree(List.range(0, 3))) == 3

    @Test
    def testSize04(): Bool =
        RedBlackTree.size(toTree(List.range(0, 7))) == 7

    @Test
    def testSize05(): Bool =
        RedBlackTree.size(toTree(List.range(0, 100))) == 100

    @Test
    def testRemove01(): Bool =
        not RedBlackTree.memberOf(1, RedBlackTree.remove(1, toTree(List.range(0, 100))))

    @Test
    def testRemove02(): Bool =
        not RedBlackTree.memberOf(50, RedBlackTree.remove(50, toTree(List.range(0, 100))))

    @Test
    def testRemove03(): Bool =
        not RedBlackTree.memberOf(99, RedBlackTree.remove(99, toTree(List.range(0, 100))))

    @Test
    def testEmptyAfterRemove01(): Bool =
        Map.Map(RedBlackTree.empty()) == Map.Map(RedBlackTree.remove(42, RedBlackTree.insert(42, (), RedBlackTree.empty())))

    @Test
    def testEmptyAfterRemove02(): Bool =
        Map.Map(RedBlackTree.empty()) == Map.Map(RedBlackTree.remove(2, RedBlackTree.remove(1,
            RedBlackTree.remove(0, toTree(List.range(0, 3))))))

    @Test
    def testEmptyAfterRemove03(): Bool =
        let tree =
        List.foldLeft((acc, x) -> RedBlackTree.remove(x, acc), toTree(List.range(0, 20)), List.range(0, 20));
        Map.Map(tree) == Map.Map(RedBlackTree.empty())

    @Test
    def testBinarySearchTreeInvariant01(): Bool =
        checkBinarySearchTreeInvariant(toTree(List.range(0, 10)))

    @Test
    def testBinarySearchTreeInvariant02(): Bool =
        checkBinarySearchTreeInvariant(toTree(9 :: 8 :: 7 :: 6 :: 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil))

    @Test
    def testBinarySearchTreeInvariant03(): Bool =
        checkBinarySearchTreeInvariant(toTree(2 :: 7 :: 1 :: 8 :: 0 :: 3 :: 5 :: 4 :: 9 :: 6 :: Nil))

    @Test
    def testBinarySearchTreeInvariant04(): Bool =
        checkBinarySearchTreeInvariant(toTree(List.range(0, 100)))

    @Test
    def testBinarySearchTreeInvariant05(): Bool =
        checkBinarySearchTreeInvariant(toTree(List.range(0, 1000)))

    @Test
    def testBinarySearchTreeInvariant06(): Bool =
        checkBinarySearchTreeInvariant(RedBlackTree.remove(0, toTree(List.range(0, 100))))

    @Test
    def testBinarySearchTreeInvariant07(): Bool =
        checkBinarySearchTreeInvariant(RedBlackTree.remove(49, toTree(List.range(0, 100))))

    @Test
    def testBinarySearchTreeInvariant08(): Bool =
        checkBinarySearchTreeInvariant(RedBlackTree.remove(99, toTree(List.range(0, 100))))

    @Test
    def testBinarySearchTreeInvariant09(): Bool =
        let tree =
        List.foldLeft((acc, x) -> RedBlackTree.remove(x, acc), toTree(List.range(0, 20)),
            12 :: 11 :: 80 :: 93 :: 21 :: 71 :: 39 :: 32 :: 29 :: 95 :: Nil);
        checkBinarySearchTreeInvariant(tree)

    @Test
    def testBinarySearchTreeInvariant10(): Bool \ NonDet = checkBinarySearchTreeInvariant(random(10))

    @Test
    def testBinarySearchTreeInvariant11(): Bool \ NonDet = checkBinarySearchTreeInvariant(random(100))

    @Test
    def testBinarySearchTreeInvariant12(): Bool \ NonDet = checkBinarySearchTreeInvariant(random(1000))

    @Test
    def testRedParentInvariant01(): Bool =
        checkRedParentInvariant(toTree(List.range(0, 10)))

    @Test
    def testRedParentInvariant02(): Bool =
        checkRedParentInvariant(toTree(9 :: 8 :: 7 :: 6 :: 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil))

    @Test
    def testRedParentInvariant03(): Bool =
        checkRedParentInvariant(toTree(2 :: 7 :: 1 :: 8 :: 0 :: 3 :: 5 :: 4 :: 9 :: 6 :: Nil))

    @Test
    def testRedParentInvariant04(): Bool =
        checkRedParentInvariant(toTree(List.range(0, 100)))

    @Test
    def testRedParentInvariant05(): Bool =
        checkRedParentInvariant(toTree(List.range(0, 1000)))

    @Test
    def testRedParentInvariant06(): Bool =
        checkRedParentInvariant(RedBlackTree.remove(0, toTree(List.range(0, 100))))

    @Test
    def testRedParentInvariant07(): Bool =
        checkRedParentInvariant(RedBlackTree.remove(49, toTree(List.range(0, 100))))

    @Test
    def testRedParentInvariant08(): Bool =
        checkRedParentInvariant(RedBlackTree.remove(99, toTree(List.range(0, 100))))

    @Test
    def testRedParentInvariant09(): Bool =
        let tree =
        List.foldLeft((acc, x) -> RedBlackTree.remove(x, acc), toTree(List.range(0, 20)),
            12 :: 11 :: 80 :: 93 :: 21 :: 71 :: 39 :: 32 :: 29 :: 95 :: Nil);
        checkRedParentInvariant(tree)

    @Test
    def testRedParentInvariant10(): Bool \ NonDet = checkRedParentInvariant(random(10))

    @Test
    def testRedParentInvariant11(): Bool \ NonDet = checkRedParentInvariant(random(100))

    @Test
    def testRedParentInvariant12(): Bool \ NonDet = checkRedParentInvariant(random(1000))

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
    def random(n: Int32): RedBlackTree[Int32, Unit] \ NonDet = {
        let seed = 42i64;
        let r = Random.newWithSeed(seed);
        List.foldLeft((acc, x) -> RedBlackTree.insert(x, (), acc), RedBlackTree.empty(),
            List.map(_ -> Random.nextInt32(r), List.range(0, n)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // Parallel Helper functions                                               //
    /////////////////////////////////////////////////////////////////////////////

    ///
    /// Returns the number of threads to use for parallel evaluation.
    ///
    def threads(): Int32 =
        // Note: We use a multiple of the number of physical cores for better performance.
        let multiplier = 4;
        multiplier * Environment.getVirtualProcessors()


    /////////////////////////////////////////////////////////////////////////////
    // parMapWithKey                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapWithKey01(): Bool =
        let Map.Map(t) = Map#{}: Map[Unit, Unit];
        let m = Map.Map(RedBlackTree.mapWithKey((_, v) -> v, t));
        m == Map#{}

    @Test
    def mapWithKey02(): Bool =
        let Map.Map(t) = Map#{1 => 4};
        let m = Map.Map(RedBlackTree.mapWithKey((k, v) -> k + v, t));
        m == Map#{1 => 5}

    @Test
    def mapWithKey03(): Bool =
        let Map.Map(t) = Map#{2 => -1, 0 => 0};
        let m = Map.Map(RedBlackTree.mapWithKey((k, v) -> k + v, t));
        m == Map#{2 => 1, 0 => 0}

    @Test
    def mapWithKey04(): Bool =
        let Map.Map(t) = Map#{2 => -1, 5 => 15, 11 => -9};
        let m = Map.Map(RedBlackTree.mapWithKey((k, v) -> k + v, t));
        m == Map#{2 => 1, 5 => 20, 11 => 2}

    @Test
    def mapWithKey05(): Bool =
        let Map.Map(t) = Map#{2 => -1, 5 => 15, 11 => -9, 8 => 8};
        let m = Map.Map(RedBlackTree.mapWithKey((k, v) -> k + v, t));
        m == Map#{2 => 1, 5 => 20, 11 => 2, 8 => 16}

    @Test
    def mapWithKey06(): Bool = region rc {
        let range = List.range(0, 2000);
        let m = List.zip(range, range) |> List.toMap;
        let a = MutList.new(rc);
        discard Map.mapWithKey((k, v) -> { let b = MutList.new(rc); MutList.push!(k, b); MutList.append!(MutList.toList(b), a); v }, m);
        MutList.toList(a) == range
    }

    @Test
    def mapWithKey07(): Bool =
        let Map.Map(t) = List.zip(List.range(0, 2000), List.range(0, 2000)) |> List.toMap;
        let expected = List.zip(List.range(0, 2000), List.range(1, 2001)) |> List.toMap;
        Map.Map(RedBlackTree.mapWithKey((_, v) -> v + 1, t)) == expected

    @Test
    def mapWithKey08(): Bool =
        let Map.Map(t) = List.zip(List.range(0, 2000), List.range(-1000, 1000)) |> List.toMap;
        let expected = List.zip(List.range(0, 2000), List.range(-999, 1001)) |> List.toMap;
        Map.Map(RedBlackTree.mapWithKey((_, v) -> v + 1, t)) == expected


    /////////////////////////////////////////////////////////////////////////////
    // parCount                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def parCount01(): Bool =
        let Map.Map(t) = List.zip(List.range(0, 2000), List.range(0, 2000)) |> List.toMap;
        RedBlackTree.parCount(threads() - 1, (k, v) -> k == v, t) == 2000

    @Test
    def parCount02(): Bool =
        let Map.Map(t) = List.zip(List.range(0, 2000), List.range(-1000, 1000)) |> List.toMap;
        RedBlackTree.parCount(threads() - 1, (k, v) -> k - 1000 == v, t) == 2000


    /////////////////////////////////////////////////////////////////////////////
    // parMinimumBy                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def parMinimumBy01(): Bool =
        let range = List.range(0, 2000);
        let Map.Map(t) = List.zip(range, range) |> List.toMap;
        RedBlackTree.parMinimumBy(threads() - 1, (_, x, _, y) -> x <=> y, t) == Some(0, 0)

    @Test
    def parMinimumBy02(): Bool =
        let range = List.range(0, 2000);
        let Map.Map(t) = List.zip(range, List.reverse(range)) |> List.toMap;
        RedBlackTree.parMinimumBy(threads() - 1, (_, x, _, y) -> x <=> y, t) == Some(1999, 0)


    /////////////////////////////////////////////////////////////////////////////
    // parMaximumBy                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def parMaximumBy01(): Bool =
        let range = List.range(0, 2000);
        let Map.Map(t) = List.zip(range, range) |> List.toMap;
        RedBlackTree.parMaximumBy(threads() - 1, (_, x, _, y) -> x <=> y, t) == Some(1999, 1999)

    @Test
    def parMaximumBy02(): Bool =
        let range = List.range(0, 2000);
        let Map.Map(t) = List.zip(range, List.reverse(range)) |> List.toMap;
        RedBlackTree.parMaximumBy(threads() - 1, (_, x, _, y) -> x <=> y, t) == Some(0, 1999)


    /////////////////////////////////////////////////////////////////////////////
    // eq                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    def toRedBlackTree(xs: List[(k, v)]): RedBlackTree[k, v] with Order[k] =
        List.foldLeft((acc, kv) -> {let (k,v) = kv; RedBlackTree.insert(k, v, acc)}, RedBlackTree.empty(), xs)

    @Test
    def eq01(): Bool =
        RedBlackTree.empty(): RedBlackTree[Int32, Int32] == RedBlackTree.empty(): RedBlackTree[Int32, Int32]

    @Test
    def eq02(): Bool =
        toRedBlackTree(Nil): RedBlackTree[Int32, Int32] == toRedBlackTree(Nil): RedBlackTree[Int32, Int32]

    @Test
    def eq03(): Bool =
        toRedBlackTree((1, 1) :: Nil) == toRedBlackTree((1, 1) :: Nil)

    @Test
    def neq01(): Bool =
        toRedBlackTree((1, 1) :: Nil) != RedBlackTree.empty()


    /////////////////////////////////////////////////////////////////////////////
    // parExists                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def parExists01(): Bool =
        let l = List.range(1, 2001);
        let Map.Map(t) = List.zip(l, l) |> List.toMap;
        RedBlackTree.parExists(threads() - 1, (k, v) -> k == 1002 and v == 1002, t)

    @Test
    def parExists02(): Bool =
        let l1 = List.range(1, 2001);
        let l2 = List.range(-2000, 0);
        let Map.Map(t) = List.zip(l1, l2) |> List.toMap;
        RedBlackTree.parExists(threads() - 1, (k, v) -> k == 1002 and v == -999, t)

    @Test
    def parExists03(): Bool =
        let l = List.range(1, 2001);
        let Map.Map(t) = List.zip(l, l) |> List.toMap;
        not RedBlackTree.parExists(threads() - 1, (k, v) -> k < 0 or v > 3000, t)


    /////////////////////////////////////////////////////////////////////////////
    // parForAll                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def parForAll01(): Bool =
        let l = List.range(1, 2001);
        let Map.Map(t) = List.zip(l, l) |> List.toMap;
        RedBlackTree.parForAll(threads() - 1, (k, v) -> k == v, t)

    @Test
    def parForAll02(): Bool =
        let l = List.range(1, 2001);
        let Map.Map(t) = List.zip(l, l) |> List.toMap;
        not RedBlackTree.parForAll(threads() - 1, (k, _) -> k == 1, t)

    @Test
    def parForAll03(): Bool =
        let l1 = List.range(1, 2001);
        let l2 = List.range(-2000, 0);
        let Map.Map(t) = List.zip(l1, l2) |> List.toMap;
        RedBlackTree.parForAll(threads() - 1, (k, v) -> k > v, t)

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Bool = region rc {
        let x = ref 21 @ rc;
        let Map.Map(t) = Map#{};
        RedBlackTree.forEach((k, _) -> x := k, t);
        21 == deref x
    }

    @Test
    def forEach02(): Bool = region rc {
        let x = ref 21 @ rc;
        let Map.Map(t) = Map#{1 => "Hello World!"};
        RedBlackTree.forEach((k, _) -> x := k, t);
        1 == deref x
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEachWithIndex01(): Bool = region rc {
        let x = ref 21 @ rc;
        let Map.Map(t) = Map#{};
        RedBlackTree.forEachWithIndex((i, _, _) -> x := i, t);
        21 == deref x
    }

    @Test
    def forEachWithIndex02(): Bool = region rc {
        let x = ref 21 @ rc;
        let Map.Map(t) = Map#{1 => "Hello World!"};
        RedBlackTree.forEachWithIndex((i, _, _) -> x := i, t);
        0 == deref x
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldMapWithKey                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldMap01(): Bool =
        let Map.Map(t) = Map#{};
        RedBlackTree.foldMap((k, v) -> k + v, t) == 0

    @Test
    def foldMap02(): Bool =
        let Map.Map(t) = Map#{1 => 2, 3 => 5};
        RedBlackTree.foldMap((k, v) -> k + v, t) == (1 + 2 + 3 + 5)

    @Test
    def foldMap03(): Bool =
        let Map.Map(t) = Map#{1 => 2, 3 => 5};
        RedBlackTree.foldMap((k, v) -> 2 * (k + v), t) == (2 * (1 + 2) + 2 * (3 + 5))

    @Test
    def foldMap04(): Bool =
        let Map.Map(t) = Map#{1 => "a", 2 => "b"};
        RedBlackTree.foldMap((k, v) -> "${k}" + v, t) == "1a2b"


    /////////////////////////////////////////////////////////////////////////////
    // sumValues                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumValues01(): Bool =
        RedBlackTree.empty() |> RedBlackTree.sumValues == 0

    @Test
    def sumValues02(): Bool =
        let Map.Map(t) = Map#{1 => 1};
        RedBlackTree.sumValues(t) == 1

    @Test
    def sumValues03(): Bool =
        let Map.Map(t) = Map#{1 => 1, 2 => 2, 3 => 3};
        RedBlackTree.sumValues(t) == 6

    @Test
    def sumValues04(): Bool =
        let Map.Map(t) = Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3};
        RedBlackTree.sumValues(t) == 3

    @Test
    def sumValues05(): Bool =
        let Map.Map(t) = Map#{-1 => -1, -2 => -2, -3 => -3, -4 => -4};
        RedBlackTree.sumValues(t) == -10

    @Test
    def sumValues06(): Bool =
        let Map.Map(t) = Map#{10 => 10, -10 => -10};
        RedBlackTree.sumValues(t) == 0

    @Test
    def sumValues07(): Bool =
        let Map.Map(t) = List.range(1, 101) |> List.zip(List.range(1, 101)) |> List.toMap;
        RedBlackTree.sumValues(t) == 5050


    /////////////////////////////////////////////////////////////////////////////
    // sumKeys                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumKeys01(): Bool =
        RedBlackTree.empty() |> RedBlackTree.sumKeys == 0

    @Test
    def sumKeys02(): Bool =
        let Map.Map(t) = Map#{1 => 1};
        RedBlackTree.sumKeys(t) == 1

    @Test
    def sumKeys03(): Bool =
        let Map.Map(t) = Map#{1 => 1, 2 => 2, 3 => 3};
        RedBlackTree.sumKeys(t) == 6

    @Test
    def sumKeys04(): Bool =
        let Map.Map(t) = Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3};
        RedBlackTree.sumKeys(t) == 3

    @Test
    def sumKeys05(): Bool =
        let Map.Map(t) = Map#{-1 => -1, -2 => -2, -5 => -3, -4 => -4};
        RedBlackTree.sumKeys(t) == -12

    @Test
    def sumKeys06(): Bool =
        let Map.Map(t) = Map#{10 => 10, -10 => -10};
        RedBlackTree.sumKeys(t) == 0

    @Test
    def sumKeys07(): Bool =
        let Map.Map(t) = List.range(1, 101) |> List.zip(List.range(1, 101)) |> List.toMap;
        RedBlackTree.sumKeys(t) == 5050


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Bool =
        RedBlackTree.empty() |> RedBlackTree.sumWith((k, v) -> k + v) == 0

    @Test
    def sumWith02(): Bool =
        let Map.Map(t) = Map#{1 => 1};
        RedBlackTree.sumWith((k, v) -> k + v, t) == 2

    @Test
    def sumWith03(): Bool =
        let Map.Map(t) = Map#{1 => 1, 2 => 2, 3 => 3};
        RedBlackTree.sumWith((k, v) -> k + v, t) == 12

    @Test
    def sumWith04(): Bool =
        let Map.Map(t) = Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3};
        RedBlackTree.sumWith((k, v) -> k + v, t) == 6

    @Test
    def sumWith05(): Bool =
        let Map.Map(t) = Map#{-1 => -1, -2 => -2, -3 => -3, -4 => -4};
        RedBlackTree.sumWith((k, v) -> k + v, t) == -20

    @Test
    def sumWith06(): Bool =
        let Map.Map(t) = Map#{10 => 10, -10 => -10};
        RedBlackTree.sumWith((k, v) -> k + v, t) == 0


    /////////////////////////////////////////////////////////////////////////////
    // parSumWith                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def parSumWith01(): Bool =
        let Map.Map(t) = List.zip(List.range(0, 2000), List.range(0, 2000)) |> List.toMap;
        RedBlackTree.parSumWith(threads() - 1, (k, v) -> k + v, t) == 3998000

    @Test
    def parSumWith02(): Bool =
        let Map.Map(t) = List.zip(List.range(0, 2000), List.range(-1000, 1000)) |> List.toMap;
        RedBlackTree.parSumWith(threads() - 1, (k, v) -> k + v, t) == 1998000


    /////////////////////////////////////////////////////////////////////////////
    // joinKeys                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinKeys01(): Bool =
        RedBlackTree.empty(): RedBlackTree[Int32, Int32] |> RedBlackTree.joinKeys(",") == ""

    @Test
    def joinKeys02(): Bool =
        let Map.Map(t) = Map#{1 => 1};
        RedBlackTree.joinKeys(",", t) == "1"

    @Test
    def joinKeys03(): Bool =
        let Map.Map(t) = Map#{0 => 1, 1 => 2, 2 => 2};
        RedBlackTree.joinKeys(",", t) == "0,1,2"

    @Test
    def joinKeys04(): Bool =
        let Map.Map(t) = Map#{"0" => 1, "1" => 2, "2" => 2};
        RedBlackTree.joinKeys(",", t) == "0,1,2"


    /////////////////////////////////////////////////////////////////////////////
    // joinValues                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinValues01(): Bool =
        RedBlackTree.empty(): RedBlackTree[Int32, Int32] |> RedBlackTree.joinValues(",") == ""

    @Test
    def joinValues02(): Bool =
        let Map.Map(t) = Map#{1 => 1};
        RedBlackTree.joinValues(",", t) == "1"

    @Test
    def joinValues03(): Bool =
        let Map.Map(t) = Map#{0 => 1, 1 => 2, 2 => 2};
        RedBlackTree.joinValues(",", t) == "1,2,2"

    @Test
    def joinValues04(): Bool =
        let Map.Map(t) = Map#{0 => "1", 1 => "2", 2 => "2"};
        RedBlackTree.joinValues(",", t) == "1,2,2"


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinWith01(): Bool =
        RedBlackTree.empty(): RedBlackTree[Int32, Int32] |> RedBlackTree.joinWith((k, v) -> "${k} => ${v}", ",") == ""

    @Test
    def joinWith02(): Bool =
        let Map.Map(t) = Map#{1 => 1};
        RedBlackTree.joinWith((k, v) -> "${k} => ${v}", ", ", t) == "1 => 1"

    @Test
    def joinWith03(): Bool =
        let Map.Map(t) = Map#{0 => 1, 1 => 2, 2 => 2};
        RedBlackTree.joinWith((k, v) -> "${k} => ${v}", ", ", t) == "0 => 1, 1 => 2, 2 => 2"

    @Test
    def joinWith04(): Bool =
        let Map.Map(t) = Map#{0 => "1", 1 => "2", 2 => "2"};
        RedBlackTree.joinWith((k, v) -> "${k} => ${v}", ", ", t) == "0 => 1, 1 => 2, 2 => 2"

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filter01(): Bool =
        RedBlackTree.filter(x -> x > 1, RedBlackTree.empty(): RedBlackTree[Int32, Int32]) == RedBlackTree.empty()

    @Test
    def filter02(): Bool =
        RedBlackTree.filter(x -> x > 1, RedBlackTree.empty() |> RedBlackTree.insert(1, 1)) == RedBlackTree.empty()

    @Test
    def filter03(): Bool =
        RedBlackTree.filter(x -> x > 1, RedBlackTree.empty() |> RedBlackTree.insert(1, 1) |> RedBlackTree.insert(2, 2) |> RedBlackTree.insert(3, 3) |> RedBlackTree.insert(4, 4))
            |> RedBlackTree.toList |> List.sort == (2, 2) :: (3, 3) :: (4, 4) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMap01(): Bool =
        RedBlackTree.filterMap(x -> if (x > 1) Some("${x}") else None, RedBlackTree.empty(): RedBlackTree[Int32, Int32])
            == RedBlackTree.empty()

    @Test
    def filterMap02(): Bool =
        RedBlackTree.filterMap(x -> if (x > 1) Some("${x}") else None, RedBlackTree.empty() |> RedBlackTree.insert(1, 1))
            == RedBlackTree.empty()

    @Test
    def filterMap03(): Bool =
        RedBlackTree.filterMap(x -> if (x > 1) Some("${x}") else None, RedBlackTree.empty() |> RedBlackTree.insert(1, 1) |> RedBlackTree.insert(2, 2) |> RedBlackTree.insert(3, 3) |> RedBlackTree.insert(4, 4))
            |> RedBlackTree.toList |> List.sort == (2, "2") :: (3, "3") :: (4, "4") :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMutDeque01(): Bool = region rc {
        let t = RedBlackTree.empty(): RedBlackTree[Int32, Int32];
        let d1 = RedBlackTree.toMutDeque(rc, t);

        let d2 = MutDeque.new(rc);

        d1 `MutDeque.sameElements` d2
    }

    @Test
    def toMutDeque02(): Bool = region rc {
        let t = RedBlackTree.empty() |> RedBlackTree.insert(1, 1);
        let d1 = RedBlackTree.toMutDeque(rc, t);

        let d2 = MutDeque.new(rc);
        MutDeque.pushBack((1, 1), d2);

        d1 `MutDeque.sameElements` d2
    }

    @Test
    def toMutDeque03(): Bool = region rc {
        let t = RedBlackTree.empty() |> RedBlackTree.insert(1, 1) |> RedBlackTree.insert(2, 2)
                                     |> RedBlackTree.insert(3, 3) |> RedBlackTree.insert(4, 4);
        let d1 = RedBlackTree.toMutDeque(rc, t);

        let d2 = MutDeque.new(rc);
        MutDeque.pushBack((2, 2), d2);
        MutDeque.pushBack((3, 3), d2);
        MutDeque.pushBack((4, 4), d2);
        MutDeque.pushFront((1, 1), d2);

        d1 `MutDeque.sameElements` d2
    }

    @Test
    def toMutDeque04(): Bool = region rc {
        let t = RedBlackTree.empty() |> RedBlackTree.insert(1, 'a') |> RedBlackTree.insert(2, 'b')
                                     |> RedBlackTree.insert(3, 'c') |> RedBlackTree.insert(4, 'd');
        let d1 = RedBlackTree.toMutDeque(rc, t);

        let d2 = MutDeque.new(rc);
        MutDeque.pushFront((4, 'd'), d2);
        MutDeque.pushFront((3, 'c'), d2);
        MutDeque.pushFront((2, 'b'), d2);
        MutDeque.pushFront((1, 'a'), d2);

        d1 `MutDeque.sameElements` d2
    }


    /////////////////////////////////////////////////////////////////////////////
    // mapAWithKey (aka traverseWithKey)                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapAWithKey01(): Bool = region rc {
        let st = ref '#' @ rc;
        let t: RedBlackTree[Int32, Char] = RedBlackTree.empty();
        let ans = RedBlackTree.mapAWithKey((_, x) -> {st := x; Identity.Identity(x)}, t);
        ans == Identity.Identity(RedBlackTree.empty()) and deref st == '#'
    }

    @Test
    def mapAWithKey02(): Bool = region rc {
        let st = ref '#' @ rc;
        let t = RedBlackTree.empty() |> RedBlackTree.insert(1, 'a');
        let ans = RedBlackTree.mapAWithKey((_, x) -> {st := x; Identity.Identity(x)}, t);
        ans == Identity.Identity(RedBlackTree.empty() |> RedBlackTree.insert(1, 'a')) and deref st == 'a'
    }

    @Test
    def mapAWithKey03(): Bool = region rc {
        let st = ref '#' @ rc;
        let t = RedBlackTree.empty() |> RedBlackTree.insert(1, 'a') |> RedBlackTree.insert(2, 'b');
        let ans = RedBlackTree.mapAWithKey((_, x) -> {st := x; Identity.Identity(x)}, t);
        ans == Identity.Identity(RedBlackTree.empty() |> RedBlackTree.insert(1, 'a') |> RedBlackTree.insert(2, 'b')) and deref st == 'b'
    }

    @Test
    def mapAWithKey04(): Bool = region rc {
        let st = ref '#' @ rc;
        let t = RedBlackTree.empty() |> RedBlackTree.insert(1, 'a') |> RedBlackTree.insert(2, 'b') |> RedBlackTree.insert(3, 'c');
        let ans = RedBlackTree.mapAWithKey((_, x) -> {st := x; Identity.Identity(x)}, t);
        ans == Identity.Identity(RedBlackTree.empty()
                            |> RedBlackTree.insert(1, 'a')
                            |> RedBlackTree.insert(2, 'b') |> RedBlackTree.insert(3, 'c')) and deref st == 'c'
    }

    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Bool = region rc {
        RedBlackTree.empty(): RedBlackTree[Int32, Int32] |> RedBlackTree.iterator(rc) |> Iterator.toList == Nil
    }

    @Test
    def iterator02(): Bool = region rc {
        let t = RedBlackTree.empty() |> RedBlackTree.insert(1, 'a') |> RedBlackTree.insert(2, 'b')
                                     |> RedBlackTree.insert(3, 'c') |> RedBlackTree.insert(4, 'd');
        RedBlackTree.iterator(rc, t) |> Iterator.toList == (1, 'a') :: (2, 'b') :: (3, 'c') :: (4, 'd') :: Nil
    }

    @Test
    def iterator03(): Bool = region rc {
        let t = RedBlackTree.empty() |> RedBlackTree.insert(4, 'd') |> RedBlackTree.insert(3, 'c')
                                     |> RedBlackTree.insert(2, 'b') |> RedBlackTree.insert(1, 'a');
        RedBlackTree.iterator(rc, t) |> Iterator.toList == (1, 'a') :: (2, 'b') :: (3, 'c') :: (4, 'd') :: Nil
    }

    @Test
    def iterator04(): Bool = region rc {
        let l = List.range(1, 100);
        let t = List.foldLeft((ac, i) -> RedBlackTree.insert(i, i+100, ac), RedBlackTree.empty(), l);
        RedBlackTree.iterator(rc, t) |> Iterator.toList == List.map(i -> (i, i+100), List.range(1, 100))
    }

}
