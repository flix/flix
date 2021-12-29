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
    def testBinarySearchTreeInvariant10(): Bool & Impure = checkBinarySearchTreeInvariant(random(10))

    @test
    def testBinarySearchTreeInvariant11(): Bool & Impure = checkBinarySearchTreeInvariant(random(100))

    @test
    def testBinarySearchTreeInvariant12(): Bool & Impure = checkBinarySearchTreeInvariant(random(1000))

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
    def testRedParentInvariant10(): Bool & Impure = checkRedParentInvariant(random(10))

    @test
    def testRedParentInvariant11(): Bool & Impure = checkRedParentInvariant(random(100))

    @test
    def testRedParentInvariant12(): Bool & Impure = checkRedParentInvariant(random(1000))

    ///
    /// Returns `true` if and only if the binary search tree invariant holds for `tree`.
    ///
    def checkBinarySearchTreeInvariant(tree: RedBlackTree[k, v]): Bool with Order[k] = match tree {
        case Node(_, a, xK, _, b) =>
            if (RedBlackTree.exists((yK, _) -> yK <=> xK == GreaterThan, a) or RedBlackTree.exists((zK, _) -> zK <=> xK == LessThan, b))
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
    def random(n: Int32): RedBlackTree[Int32, Unit] & Impure = {
        let seed = 42i64;
        let r = Random.newWithSeed(seed);
        List.foldLeft((acc, x) -> RedBlackTree.insert(x, (), acc), RedBlackTree.empty(),
            List.map(_ -> Random.nextInt32(r), List.range(0, n)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // seqMapWithKey                                                           //
    /////////////////////////////////////////////////////////////////////////////

    // TODO: Add tests for parMapWithKey

    @test
    def seqMapWithKey01(): Bool =
        let Map(t) = Map#{}: Map[Int32, Int32];
        Map(RedBlackTree.seqMapWithKey((k, v) -> k + v, t)) == Map#{}

    @test
    def seqMapWithKey02(): Bool =
        let Map(t) = Map#{1 => 4};
        Map(RedBlackTree.seqMapWithKey((k, v) -> k + v, t)) == Map#{1 => 5}

    @test
    def seqMapWithKey03(): Bool =
        let Map(t) = Map#{2 => -1, 0 => 0};
        Map(RedBlackTree.seqMapWithKey((k, v) -> k + v, t)) == Map#{2 => 1, 0 => 0}

    @test
    def seqMapWithKey04(): Bool =
        let Map(t) = Map#{2 => -1, 5 => 15, 11 => -9};
        Map(RedBlackTree.seqMapWithKey((k, v) -> k + v, t)) == Map#{2 => 1, 5 => 20, 11 => 2}

    @test
    def seqMapWithKey05(): Bool =
        let Map(t) = Map#{2 => -1, 5 => 15, 11 => -9, 8 => 8};
        Map(RedBlackTree.seqMapWithKey((k, v) -> k + v, t)) == Map#{2 => 1, 5 => 20, 11 => 2, 8 => 16}

    @test
    def seqMapWithKey06(): Bool =
        let Map(t) = List.zip(List.range(0, 2000), List.range(0, 2000)) |> List.toMap;
        let expected = List.zip(List.range(0, 2000), List.range(1, 2001)) |> List.toMap;
        Map(RedBlackTree.seqMapWithKey((_, v) -> v + 1, t)) == expected

    @test
    def seqMapWithKey07(): Bool =
        let Map(t) = List.zip(List.range(0, 2000), List.range(-1000, 1000)) |> List.toMap;
        let expected = List.zip(List.range(0, 2000), List.range(-999, 1001)) |> List.toMap;
        Map(RedBlackTree.seqMapWithKey((_, v) -> v + 1, t)) == expected

    @test
    def seqMapWithKey08(): Bool & Impure =
        let range = List.range(0, 2000);
        let Map(t) = List.zip(range, range) |> List.toMap;
        let a = Array.new(-1, 2000);
        let i = ref 0;
        RedBlackTree.seqMapWithKey((_, v) -> { a[deref i] = v; i := 1 + deref i; v }, t);
        Array.sameElements(a, List.toArray(range))

    @test
    def seqMapWithKey09(): Bool & Impure =
        let range = List.range(0, 2000);
        let Map(t) = List.zip(range, range) |> List.toMap;
        let a = MutList.new();
        RedBlackTree.seqMapWithKey((_, v) -> { let b = MutList.new(); MutList.push!(v, b); MutList.append!(a, b); v }, t);
        MutList.sameElements(a, List.toMutList(range))


    /////////////////////////////////////////////////////////////////////////////
    // seqCount                                                                //
    /////////////////////////////////////////////////////////////////////////////

    // TODO: Add tests for parCount

    @test
    def seqCount01(): Bool =
        let Map(t) = Map#{}: Map[Int32, Int32];
        RedBlackTree.seqCount((k, v) -> k == v, t) == 0

    @test
    def seqCount02(): Bool =
        let Map(t) = Map#{1 => 2};
        RedBlackTree.seqCount((k, v) -> k == v, t) == 0

    @test
    def seqCount03(): Bool =
        let Map(t) = Map#{1 => 1};
        RedBlackTree.seqCount((k, v) -> k == v, t) == 1

    @test
    def seqCount04(): Bool =
        let Map(t) = Map#{1 => 2, 2 => 3};
        RedBlackTree.seqCount((k, v) -> k == v, t) == 0

    @test
    def seqCount05(): Bool =
        let Map(t) = Map#{1 => 1, 2 => 3};
        RedBlackTree.seqCount((k, v) -> k == v, t) == 1

    @test
    def seqCount06(): Bool =
        let Map(t) = List.zip(List.range(0, 2000), List.range(0, 2000)) |> List.toMap;
        RedBlackTree.seqCount((k, v) -> k == v, t) == 2000

    @test
    def seqCount07(): Bool =
        let Map(t) = List.zip(List.range(0, 2000), List.range(-1000, 1000)) |> List.toMap;
        RedBlackTree.seqCount((k, v) -> k - 1000 == v, t) == 2000

    @test
    def seqCount08(): Bool & Impure =
        let range = List.range(0, 2000);
        let Map(t) = List.zip(range, range) |> List.toMap;
        let a = MutList.new();
        RedBlackTree.seqCount((_, v) -> { let b = MutList.new(); MutList.push!(v, b); MutList.append!(a, b); true }, t);
        MutList.sameElements(a, List.toMutList(range))


    /////////////////////////////////////////////////////////////////////////////
    // seqMinimumBy                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def seqMinimumBy01(): Bool =
        let Map(t) = Map#{}: Map[Int32, Int32];
        RedBlackTree.seqMinimumBy((x, _, y, _) -> x <=> y, t) == None

    @test
    def seqMinimumBy02(): Bool =
        let Map(t) = Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5};
        RedBlackTree.seqMinimumBy((x, _, y, _) -> x <=> y, t) == Some((1, 2))

    @test
    def seqMinimumBy03(): Bool =
        let Map(t) = Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5, 0 => 2};
        RedBlackTree.seqMinimumBy((x, _, y, _) -> x <=> y, t) == Some((0, 2))

    @test
    def seqMinimumBy04(): Bool =
        let Map(t) = Map#{1 => 2, 2 => 3, -44 => 4, 5 => 5, 0 => 2};
        RedBlackTree.seqMinimumBy((x, _, y, _) -> x <=> y, t) == Some((-44, 4))

    @test
    def seqMinimumBy05(): Bool & Impure =
        // TODO: Simplify this test
        // TODO: Refactor to use parMinimumBy
        /*
         * This test checks that the tree is traversed sequentially
         * and is tightly coupled to the implementation.
         * Since the keys and values are equal, the left-most / lowest valued key
         * also holds the lowest value in the tree.
         * Thus, all comparisons should yield `LessThan`.
         * Additionally, since every node compares itself with the lowest value of
         * its left subtree and the result of that is compared with the lowest value
         * of right subtree, it yields a total of n - 1 comparisons (number of edges).
         */
        let n = 2000;
        let range = List.range(0, n);
        let Map(t) = List.zip(range, range) |> List.toMap;
        let a = MutList.new();
        RedBlackTree.seqMinimumBy((_, x, _, y) -> {
            let r = x <=> y;
            let b = MutList.new();
            MutList.push!(r, b);
            MutList.append!(a, b);
            r
        }, t);
        MutList.toList(a) == List.repeat(n - 1, LessThan)

    @test
    def seqMinimumBy06(): Bool =
        let range = List.range(0, 2000);
        let Map(t) = List.zip(range, List.reverse(range)) |> List.toMap;
        RedBlackTree.seqMinimumBy((_, x, _, y) -> x <=> y, t) == Some(1999, 0)

    // TODO: Add tests for parMinimumBy
}
