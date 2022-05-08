use RedBlackTree.RedBlackTree;
use RedBlackTree.RedBlackTree.{Node};
use RedBlackTree.Color.{Red, Black};

namespace TestRedBlackTree {

    @test
    def testSize01(): Bool =
        RedBlackTree.size(RedBlackTree.empty()) == 0

    @test
    def testSize02(): Bool =
        RedBlackTree.size(RedBlackTree.insert(1, (), RedBlackTree.empty())) == 1

    @test
    def testSize03(): Bool =
        RedBlackTree.size(toTree(List.range(0, 3))) == 3

    @test
    def testSize04(): Bool =
        RedBlackTree.size(toTree(List.range(0, 7))) == 7

    @test
    def testSize05(): Bool =
        RedBlackTree.size(toTree(List.range(0, 100))) == 100

    @test
    def testRemove01(): Bool =
        not RedBlackTree.memberOf(1, RedBlackTree.remove(1, toTree(List.range(0, 100))))

    @test
    def testRemove02(): Bool =
        not RedBlackTree.memberOf(50, RedBlackTree.remove(50, toTree(List.range(0, 100))))

    @test
    def testRemove03(): Bool =
        not RedBlackTree.memberOf(99, RedBlackTree.remove(99, toTree(List.range(0, 100))))

    @test
    def testEmptyAfterRemove01(): Bool =
        Map(RedBlackTree.empty()) == Map(RedBlackTree.remove(42, RedBlackTree.insert(42, (), RedBlackTree.empty())))

    @test
    def testEmptyAfterRemove02(): Bool =
        Map(RedBlackTree.empty()) == Map(RedBlackTree.remove(2, RedBlackTree.remove(1,
            RedBlackTree.remove(0, toTree(List.range(0, 3))))))

    @test
    def testEmptyAfterRemove03(): Bool =
        let tree =
        List.foldLeft((acc, x) -> RedBlackTree.remove(x, acc), toTree(List.range(0, 20)), List.range(0, 20));
        Map(tree) == Map(RedBlackTree.empty())

    @test
    def testBinarySearchTreeInvariant01(): Bool =
        checkBinarySearchTreeInvariant(toTree(List.range(0, 10)))

    @test
    def testBinarySearchTreeInvariant02(): Bool =
        checkBinarySearchTreeInvariant(toTree(9 :: 8 :: 7 :: 6 :: 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil))

    @test
    def testBinarySearchTreeInvariant03(): Bool =
        checkBinarySearchTreeInvariant(toTree(2 :: 7 :: 1 :: 8 :: 0 :: 3 :: 5 :: 4 :: 9 :: 6 :: Nil))

    @test
    def testBinarySearchTreeInvariant04(): Bool =
        checkBinarySearchTreeInvariant(toTree(List.range(0, 100)))

    @test
    def testBinarySearchTreeInvariant05(): Bool =
        checkBinarySearchTreeInvariant(toTree(List.range(0, 1000)))

    @test
    def testBinarySearchTreeInvariant06(): Bool =
        checkBinarySearchTreeInvariant(RedBlackTree.remove(0, toTree(List.range(0, 100))))

    @test
    def testBinarySearchTreeInvariant07(): Bool =
        checkBinarySearchTreeInvariant(RedBlackTree.remove(49, toTree(List.range(0, 100))))

    @test
    def testBinarySearchTreeInvariant08(): Bool =
        checkBinarySearchTreeInvariant(RedBlackTree.remove(99, toTree(List.range(0, 100))))

    @test
    def testBinarySearchTreeInvariant09(): Bool =
        let tree =
        List.foldLeft((acc, x) -> RedBlackTree.remove(x, acc), toTree(List.range(0, 20)),
            12 :: 11 :: 80 :: 93 :: 21 :: 71 :: 39 :: 32 :: 29 :: 95 :: Nil);
        checkBinarySearchTreeInvariant(tree)

    @test
    def testBinarySearchTreeInvariant10(): Bool = checkBinarySearchTreeInvariant(random(10))

    @test
    def testBinarySearchTreeInvariant11(): Bool = checkBinarySearchTreeInvariant(random(100))

    @test
    def testBinarySearchTreeInvariant12(): Bool = checkBinarySearchTreeInvariant(random(1000))

    @test
    def testRedParentInvariant01(): Bool =
        checkRedParentInvariant(toTree(List.range(0, 10)))

    @test
    def testRedParentInvariant02(): Bool =
        checkRedParentInvariant(toTree(9 :: 8 :: 7 :: 6 :: 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil))

    @test
    def testRedParentInvariant03(): Bool =
        checkRedParentInvariant(toTree(2 :: 7 :: 1 :: 8 :: 0 :: 3 :: 5 :: 4 :: 9 :: 6 :: Nil))

    @test
    def testRedParentInvariant04(): Bool =
        checkRedParentInvariant(toTree(List.range(0, 100)))

    @test
    def testRedParentInvariant05(): Bool =
        checkRedParentInvariant(toTree(List.range(0, 1000)))

    @test
    def testRedParentInvariant06(): Bool =
        checkRedParentInvariant(RedBlackTree.remove(0, toTree(List.range(0, 100))))

    @test
    def testRedParentInvariant07(): Bool =
        checkRedParentInvariant(RedBlackTree.remove(49, toTree(List.range(0, 100))))

    @test
    def testRedParentInvariant08(): Bool =
        checkRedParentInvariant(RedBlackTree.remove(99, toTree(List.range(0, 100))))

    @test
    def testRedParentInvariant09(): Bool =
        let tree =
        List.foldLeft((acc, x) -> RedBlackTree.remove(x, acc), toTree(List.range(0, 20)),
            12 :: 11 :: 80 :: 93 :: 21 :: 71 :: 39 :: 32 :: 29 :: 95 :: Nil);
        checkRedParentInvariant(tree)

    @test
    def testRedParentInvariant10(): Bool = checkRedParentInvariant(random(10))

    @test
    def testRedParentInvariant11(): Bool = checkRedParentInvariant(random(100))

    @test
    def testRedParentInvariant12(): Bool = checkRedParentInvariant(random(1000))

    ///
    /// Returns `true` if and only if the binary search tree invariant holds for `tree`.
    ///
    def checkBinarySearchTreeInvariant(tree: RedBlackTree[k, v]): Bool with Order[k] = match tree {
        case Node(_, a, xK, _, b) =>
            if (RedBlackTree.exists((yK, _) -> (yK <=> xK) == GreaterThan, a) or RedBlackTree.exists((zK, _) -> (zK <=> xK) == LessThan, b))
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
    def random(n: Int32): RedBlackTree[Int32, Unit] = {
        region r {
            let seed = 42i64;
            let ran = Random.newWithSeed(r, seed);
            List.foldLeft((acc, x) -> RedBlackTree.insert(x, (), acc), RedBlackTree.empty(),
                List.map(_ -> Random.nextInt32(ran), List.range(0, n)))
        }
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

    @test
    def mapWithKey01(): Bool =
        let Map(t) = Map#{}: Map[Unit, Unit];
        let m = Map(RedBlackTree.mapWithKey((_, v) -> v, t));
        m == Map#{}

    @test
    def mapWithKey02(): Bool =
        let Map(t) = Map#{1 => 4};
        let m = Map(RedBlackTree.mapWithKey((k, v) -> k + v, t));
        m == Map#{1 => 5}

    @test
    def mapWithKey03(): Bool =
        let Map(t) = Map#{2 => -1, 0 => 0};
        let m = Map(RedBlackTree.mapWithKey((k, v) -> k + v, t));
        m == Map#{2 => 1, 0 => 0}

    @test
    def mapWithKey04(): Bool =
        let Map(t) = Map#{2 => -1, 5 => 15, 11 => -9};
        let m = Map(RedBlackTree.mapWithKey((k, v) -> k + v, t));
        m == Map#{2 => 1, 5 => 20, 11 => 2}

    @test
    def mapWithKey05(): Bool =
        let Map(t) = Map#{2 => -1, 5 => 15, 11 => -9, 8 => 8};
        let m = Map(RedBlackTree.mapWithKey((k, v) -> k + v, t));
        m == Map#{2 => 1, 5 => 20, 11 => 2, 8 => 16}

    @test
    def mapWithKey06(): Bool & Impure =
        let range = List.range(0, 2000);
        let m = List.zip(range, range) |> List.toMap;
        let a = new MutList(static);
        Map.mapWithKey((k, v) -> { let b = new MutList(static); MutList.push!(k, b); MutList.append!(b, a); v }, m);
        MutList.toList(a) == range

    @test
    def mapWithKey07(): Bool =
        let Map(t) = List.zip(List.range(0, 2000), List.range(0, 2000)) |> List.toMap;
        let expected = List.zip(List.range(0, 2000), List.range(1, 2001)) |> List.toMap;
        Map(RedBlackTree.mapWithKey((_, v) -> v + 1, t)) == expected

    @test
    def mapWithKey08(): Bool =
        let Map(t) = List.zip(List.range(0, 2000), List.range(-1000, 1000)) |> List.toMap;
        let expected = List.zip(List.range(0, 2000), List.range(-999, 1001)) |> List.toMap;
        Map(RedBlackTree.mapWithKey((_, v) -> v + 1, t)) == expected


    /////////////////////////////////////////////////////////////////////////////
    // parCount                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def parCount01(): Bool & Impure =
        let Map(t) = List.zip(List.range(0, 2000), List.range(0, 2000)) |> List.toMap;
        RedBlackTree.parCount(threads() - 1, (k, v) -> k == v, t) == 2000

    @test
    def parCount02(): Bool & Impure =
        let Map(t) = List.zip(List.range(0, 2000), List.range(-1000, 1000)) |> List.toMap;
        RedBlackTree.parCount(threads() - 1, (k, v) -> k - 1000 == v, t) == 2000


    /////////////////////////////////////////////////////////////////////////////
    // parMinimumBy                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def parMinimumBy01(): Bool & Impure =
        let range = List.range(0, 2000);
        let Map(t) = List.zip(range, range) |> List.toMap;
        RedBlackTree.parMinimumBy(threads() - 1, (_, x, _, y) -> x <=> y, t) == Some(0, 0)

    @test
    def parMinimumBy02(): Bool & Impure =
        let range = List.range(0, 2000);
        let Map(t) = List.zip(range, List.reverse(range)) |> List.toMap;
        RedBlackTree.parMinimumBy(threads() - 1, (_, x, _, y) -> x <=> y, t) == Some(1999, 0)

    /////////////////////////////////////////////////////////////////////////////
    // eq                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    def toRedBlackTree(xs: List[(k, v)]): RedBlackTree.RedBlackTree[k, v] with Order[k] =
        List.foldLeft((acc, kv) -> {let (k,v) = kv; RedBlackTree.insert(k, v, acc)}, RedBlackTree.empty(), xs)

    @test
    def eq01(): Bool =
        RedBlackTree.empty(): RedBlackTree[Int32, Int32] == RedBlackTree.empty(): RedBlackTree[Int32, Int32]

    @test
    def eq02(): Bool =
        toRedBlackTree(Nil): RedBlackTree[Int32, Int32] == toRedBlackTree(Nil): RedBlackTree[Int32, Int32]

    @test
    def eq03(): Bool =
        toRedBlackTree((1, 1) :: Nil) == toRedBlackTree((1, 1) :: Nil)

    @test
    def neq01(): Bool =
        toRedBlackTree((1, 1) :: Nil) != RedBlackTree.empty()


    /////////////////////////////////////////////////////////////////////////////
    // parExists                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def parExists01(): Bool & Impure =
        let l = List.range(1, 2001);
        let Map(t) = List.zip(l, l) |> List.toMap;
        RedBlackTree.parExists(threads() - 1, (k, v) -> k == 1002 and v == 1002, t)

    @test
    def parExists02(): Bool & Impure =
        let l1 = List.range(1, 2001);
        let l2 = List.range(-2000, 0);
        let Map(t) = List.zip(l1, l2) |> List.toMap;
        RedBlackTree.parExists(threads() - 1, (k, v) -> k == 1002 and v == -999, t)

    @test
    def parExists03(): Bool & Impure =
        let l = List.range(1, 2001);
        let Map(t) = List.zip(l, l) |> List.toMap;
        not RedBlackTree.parExists(threads() - 1, (k, v) -> k < 0 or v > 3000, t)


    /////////////////////////////////////////////////////////////////////////////
    // parForall                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def parForall01(): Bool & Impure =
        let l = List.range(1, 2001);
        let Map(t) = List.zip(l, l) |> List.toMap;
        RedBlackTree.parForall(threads() - 1, (k, v) -> k == v, t)

    @test
    def parForall02(): Bool & Impure =
        let l = List.range(1, 2001);
        let Map(t) = List.zip(l, l) |> List.toMap;
        not RedBlackTree.parForall(threads() - 1, (k, _) -> k == 1, t)

    @test
    def parForall03(): Bool & Impure =
        let l1 = List.range(1, 2001);
        let l2 = List.range(-2000, 0);
        let Map(t) = List.zip(l1, l2) |> List.toMap;
        RedBlackTree.parForall(threads() - 1, (k, v) -> k > v, t)


    /////////////////////////////////////////////////////////////////////////////
    // sumValues                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumValues01(): Bool =
        RedBlackTree.empty() |> RedBlackTree.sumValues == 0

    @test
    def sumValues02(): Bool =
        let Map(t) = Map#{1 => 1};
        RedBlackTree.sumValues(t) == 1

    @test
    def sumValues03(): Bool =
        let Map(t) = Map#{1 => 1, 2 => 2, 3 => 3};
        RedBlackTree.sumValues(t) == 6

    @test
    def sumValues04(): Bool =
        let Map(t) = Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3};
        RedBlackTree.sumValues(t) == 3

    @test
    def sumValues05(): Bool =
        let Map(t) = Map#{-1 => -1, -2 => -2, -3 => -3, -4 => -4};
        RedBlackTree.sumValues(t) == -10

    @test
    def sumValues06(): Bool =
        let Map(t) = Map#{10 => 10, -10 => -10};
        RedBlackTree.sumValues(t) == 0

    @test
    def sumValues07(): Bool =
        let Map(t) = List.range(1, 101) |> List.zip(List.range(1, 101)) |> List.toMap;
        RedBlackTree.sumValues(t) == 5050


    /////////////////////////////////////////////////////////////////////////////
    // sumKeys                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumKeys01(): Bool =
        RedBlackTree.empty() |> RedBlackTree.sumKeys == 0

    @test
    def sumKeys02(): Bool =
        let Map(t) = Map#{1 => 1};
        RedBlackTree.sumKeys(t) == 1

    @test
    def sumKeys03(): Bool =
        let Map(t) = Map#{1 => 1, 2 => 2, 3 => 3};
        RedBlackTree.sumKeys(t) == 6

    @test
    def sumKeys04(): Bool =
        let Map(t) = Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3};
        RedBlackTree.sumKeys(t) == 3

    @test
    def sumKeys05(): Bool =
        let Map(t) = Map#{-1 => -1, -2 => -2, -5 => -3, -4 => -4};
        RedBlackTree.sumKeys(t) == -12

    @test
    def sumKeys06(): Bool =
        let Map(t) = Map#{10 => 10, -10 => -10};
        RedBlackTree.sumKeys(t) == 0

    @test
    def sumKeys07(): Bool =
        let Map(t) = List.range(1, 101) |> List.zip(List.range(1, 101)) |> List.toMap;
        RedBlackTree.sumKeys(t) == 5050


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool =
        RedBlackTree.empty() |> RedBlackTree.sumWith((k, v) -> k + v) == 0

    @test
    def sumWith02(): Bool =
        let Map(t) = Map#{1 => 1};
        RedBlackTree.sumWith((k, v) -> k + v, t) == 2

    @test
    def sumWith03(): Bool =
        let Map(t) = Map#{1 => 1, 2 => 2, 3 => 3};
        RedBlackTree.sumWith((k, v) -> k + v, t) == 12

    @test
    def sumWith04(): Bool =
        let Map(t) = Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3};
        RedBlackTree.sumWith((k, v) -> k + v, t) == 6

    @test
    def sumWith05(): Bool =
        let Map(t) = Map#{-1 => -1, -2 => -2, -3 => -3, -4 => -4};
        RedBlackTree.sumWith((k, v) -> k + v, t) == -20

    @test
    def sumWith06(): Bool =
        let Map(t) = Map#{10 => 10, -10 => -10};
        RedBlackTree.sumWith((k, v) -> k + v, t) == 0


    /////////////////////////////////////////////////////////////////////////////
    // parSumWith                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def parSumWith01(): Bool & Impure =
        let Map(t) = List.zip(List.range(0, 2000), List.range(0, 2000)) |> List.toMap;
        RedBlackTree.parSumWith(threads() - 1, (k, v) -> k + v, t) == 3998000

    @test
    def parSumWith02(): Bool & Impure =
        let Map(t) = List.zip(List.range(0, 2000), List.range(-1000, 1000)) |> List.toMap;
        RedBlackTree.parSumWith(threads() - 1, (k, v) -> k + v, t) == 1998000


    /////////////////////////////////////////////////////////////////////////////
    // productValues                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productValues01(): Bool =
        RedBlackTree.empty() |> RedBlackTree.productValues == 0

    @test
    def productValues02(): Bool =
        let Map(t) = Map#{1 => 1};
        RedBlackTree.productValues(t) == 1

    @test
    def productValues03(): Bool =
        let Map(t) = Map#{1 => 1, 2 => 2, 3 => 3};
        RedBlackTree.productValues(t) == 6

    @test
    def productValues04(): Bool =
        let Map(t) = Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3};
        RedBlackTree.productValues(t) == -18

    @test
    def productValues05(): Bool =
        let Map(t) = Map#{-1 => -1, -2 => -2, -5 => -3, -4 => -4};
        RedBlackTree.productValues(t) == 24

    @test
    def productValues06(): Bool =
        let Map(t) = Map#{10 => 10, -10 => -10};
        RedBlackTree.productValues(t) == -100


    /////////////////////////////////////////////////////////////////////////////
    // productKeys                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productKeys01(): Bool =
        RedBlackTree.empty() |> RedBlackTree.productKeys == 0

    @test
    def productKeys02(): Bool =
        let Map(t) = Map#{1 => 1};
        RedBlackTree.productKeys(t) == 1

    @test
    def productKeys03(): Bool =
        let Map(t) = Map#{1 => 1, 2 => 2, 3 => 3};
        RedBlackTree.productKeys(t) == 6

    @test
    def productKeys04(): Bool =
        let Map(t) = Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3};
        RedBlackTree.productKeys(t) == -18

    @test
    def productKeys05(): Bool =
        let Map(t) = Map#{-1 => -1, -2 => -2, -5 => -3, -4 => -4};
        RedBlackTree.productKeys(t) == 40

    @test
    def productKeys06(): Bool =
        let Map(t) = Map#{10 => 10, -10 => -10};
        RedBlackTree.productKeys(t) == -100


    /////////////////////////////////////////////////////////////////////////////
    // productWith                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productWith01(): Bool =
        RedBlackTree.empty() |> RedBlackTree.productWith((k, v) -> k + v) == 0

    @test
    def productWith02(): Bool =
        let Map(t) = Map#{1 => 1};
        RedBlackTree.productWith((k, v) -> k + v, t) == 2

    @test
    def productWith03(): Bool =
        let Map(t) = Map#{1 => 1, 2 => 2, 3 => 3};
        RedBlackTree.productWith((k, v) -> k + v, t) == 48

    @test
    def productWith04(): Bool =
        let Map(t) = Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3};
        RedBlackTree.productWith((k, v) -> k + v, t) == -288

    @test
    def productWith05(): Bool =
        let Map(t) = Map#{-1 => -1, -2 => -2, -3 => -3, -4 => -4};
        RedBlackTree.productWith((k, v) -> k + v, t) == 384

    @test
    def productWith06(): Bool =
        let Map(t) = Map#{10 => 10, -10 => -10};
        RedBlackTree.productWith((k, v) -> k + v, t) == -400


    /////////////////////////////////////////////////////////////////////////////
    // parProductWith                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def parProductWith01(): Bool & Impure =
        let l = List.range(1, 2001);
        let Map(t) = List.zip(l, l) |> List.toMap;
        RedBlackTree.parProductWith(threads() - 1, (k, v) -> k * v, t) == List.product(l) * List.product(l)

    @test
    def parProductWith02(): Bool & Impure =
        let l1 = List.range(1, 2001);
        let l2 = List.range(-2000, 0);
        let Map(t) = List.zip(l1, l2) |> List.toMap;
        RedBlackTree.parProductWith(threads() - 1, (k, v) -> k * v, t) == List.product(l1) * List.product(l2)


    /////////////////////////////////////////////////////////////////////////////
    // joinKeys                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinKeys01(): Bool =
        RedBlackTree.empty(): RedBlackTree[Int32, Int32] |> RedBlackTree.joinKeys(",") == ""

    @test
    def joinKeys02(): Bool =
        let Map(t) = Map#{1 => 1};
        RedBlackTree.joinKeys(",", t) == "1"

    @test
    def joinKeys03(): Bool =
        let Map(t) = Map#{0 => 1, 1 => 2, 2 => 2};
        RedBlackTree.joinKeys(",", t) == "0,1,2"

    @test
    def joinKeys04(): Bool =
        let Map(t) = Map#{"0" => 1, "1" => 2, "2" => 2};
        RedBlackTree.joinKeys(",", t) == "0,1,2"


    /////////////////////////////////////////////////////////////////////////////
    // joinValues                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinValues01(): Bool =
        RedBlackTree.empty(): RedBlackTree[Int32, Int32] |> RedBlackTree.joinValues(",") == ""

    @test
    def joinValues02(): Bool =
        let Map(t) = Map#{1 => 1};
        RedBlackTree.joinValues(",", t) == "1"

    @test
    def joinValues03(): Bool =
        let Map(t) = Map#{0 => 1, 1 => 2, 2 => 2};
        RedBlackTree.joinValues(",", t) == "1,2,2"

    @test
    def joinValues04(): Bool =
        let Map(t) = Map#{0 => "1", 1 => "2", 2 => "2"};
        RedBlackTree.joinValues(",", t) == "1,2,2"


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinWith01(): Bool =
        RedBlackTree.empty(): RedBlackTree[Int32, Int32] |> RedBlackTree.joinWith((k, v) -> "${k} => ${v}", ",") == ""

    @test
    def joinWith02(): Bool =
        let Map(t) = Map#{1 => 1};
        RedBlackTree.joinWith((k, v) -> "${k} => ${v}", ", ", t) == "1 => 1"

    @test
    def joinWith03(): Bool =
        let Map(t) = Map#{0 => 1, 1 => 2, 2 => 2};
        RedBlackTree.joinWith((k, v) -> "${k} => ${v}", ", ", t) == "0 => 1, 1 => 2, 2 => 2"

    @test
    def joinWith04(): Bool =
        let Map(t) = Map#{0 => "1", 1 => "2", 2 => "2"};
        RedBlackTree.joinWith((k, v) -> "${k} => ${v}", ", ", t) == "0 => 1, 1 => 2, 2 => 2"

}
