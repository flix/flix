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
        RedBlackTree.empty() == RedBlackTree.remove(42, RedBlackTree.insert(42, (), RedBlackTree.empty()))

    @test
    def testEmptyAfterRemove02(): Bool =
        RedBlackTree.empty() == RedBlackTree.remove(2, RedBlackTree.remove(1, RedBlackTree.remove(0, toTree(List.range(0, 3)))))

    @test
    def testEmptyAfterRemove03(): Bool =
        let tree =
        List.foldLeft(acc, x) -> RedBlackTree.remove(x, acc), toTree(List.range(0, 20)), List.range(0, 20));
        tree == RedBlackTree.empty()

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
        List.foldLeft(acc, x) -> RedBlackTree.remove(x, acc), toTree(List.range(0, 20)), 12 :: 11 :: 80 :: 93 :: 21 :: 71 :: 39 :: 32 :: 29 :: 95 :: Nil);
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
        List.foldLeft(acc, x) -> RedBlackTree.remove(x, acc), toTree(List.range(0, 20)), 12 :: 11 :: 80 :: 93 :: 21 :: 71 :: 39 :: 32 :: 29 :: 95 :: Nil);
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
    def checkBinarySearchTreeInvariant(tree: RedBlackTree[k, v]): Bool = match tree {
        case Node(_, a, xK, _, b) =>
        if (RedBlackTree.exists((yK, _) -> yK <=> xK > 0, a) or RedBlackTree.exists((zK, _) -> zK <=> xK < 0, b)) false
        else checkBinarySearchTreeInvariant(a) and checkBinarySearchTreeInvariant(b)
        case _ => true
    }

    ///
    /// Helper function for `testRedParentInvariant`.
    ///
    def checkRedParentInvariantHelper(tree: RedBlackTree[k, v], parentColor: RedBlackTree.Color): Bool = match tree {
        case Node(Red, a, _, _, b) =>
            if (parentColor == Red) false else checkRedParentInvariantHelper(a, Red) and     checkRedParentInvariantHelper(b, Red)
        case Node(Black, a, _, _, b) =>
            checkRedParentInvariantHelper(a, Black) and checkRedParentInvariantHelper(b, Black)
        case _ => true
    }

    ///
    /// Returns `true` if and only if no red node in `tree` has a red parent.
    ///
    def checkRedParentInvariant(tree: RedBlackTree[k, v]): Bool = checkRedParentInvariantHelper(tree, Black)

    ///
    /// Returns a tree with mappings `x -> ()` for all `x` in `xs`.
    ///
    def toTree(xs: List[Int32]): RedBlackTree[Int32, Unit]
        = List.foldLeft(acc, x) -> RedBlackTree.insert(x, (), acc), RedBlackTree.empty(), xs)

    ///
    /// Returns a tree with `n` nodes `x -> ()` where `x` is a random integer.
    ///
    def random(n: Int32): RedBlackTree[Int32, Unit] & Impure = {
        let seed = match Int64.fromString("42") {
            case Ok(s) => s
            case _ => panic "no seed given"
        };
        let r = Random.newWithSeed(seed);
        List.foldLeft(acc, x) -> RedBlackTree.insert(x, (), acc), RedBlackTree.empty(), List.map(_ -> Random.nextInt32(r), List.range(0, n)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // mapWithKey                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithKey01(): Bool =
        let Map(t) = Map#{}: Map[Unit, Int32];
        Map(RedBlackTree.mapWithKey((k, v) -> k + v, t)) == Map#{}

    @test
    def mapWithKey02(): Bool =
        let Map(t) = Map#{1 -> 4};
        Map(RedBlackTree.mapWithKey((k, v) -> k + v, t)) == Map#{1 -> 5}

    @test
    def mapWithKey03(): Bool =
        let Map(t) = Map#{2 -> -1, 0 -> 0};
        Map(RedBlackTree.mapWithKey((k, v) -> k + v, t)) == Map#{2 -> 1, 0 -> 0}

    @test
    def mapWithKey04(): Bool =
        let Map(t) = Map#{2 -> -1, 5 -> 15, 11 -> -9};
        Map(RedBlackTree.mapWithKey((k, v) -> k + v, t)) == Map#{2 -> 1, 5 -> 20, 11 -> 2}

    @test
    def mapWithKey05(): Bool =
        let Map(t) = Map#{2 -> -1, 5 -> 15, 11 -> -9, 8 -> 8};
        Map(RedBlackTree.mapWithKey((k, v) -> k + v, t)) == Map#{2 -> 1, 5 -> 20, 11 -> 2, 8 -> 16}

    @test
    def mapWithKey06(): Bool =
        let Map(t) = List.zip(List.range(0, 2000), List.range(0, 2000)) |> List.toMap;
        let expected = List.zip(List.range(0, 2000), List.range(1, 2001)) |> List.toMap;
        Map(RedBlackTree.mapWithKey((k, v) -> k + v, t)) == expected

    @test
    def mapWithKey07(): Bool =
        let Map(t) = List.zip(List.range(0, 2000), List.range(-1000, 1000)) |> List.toMap;
        let expected = List.zip(List.range(0, 2000), List.range(-999, 1001)) |> List.toMap;
        Map(RedBlackTree.mapWithKey((k, v) -> k + v, t)) == expected

    @test
    def mapWithKey08(): Bool & Impure =
        let range = List.range(0, 2000);
        let Map(t) = List.zip(range, range) |> List.toMap;
        let a = [2000; default];
        RedBlackTree.mapWithKey((k, v) -> { a[k] = v; v }, t);
        Array.sameElements(a, List.toArray(range))

    @test
    def mapWithKey09(): Bool & Impure =
        let l = ref Nil;
        let Map(t) = Map#{1 -> 1, 2 -> 2, 3 -> 3};
        t |>
        RedBlackTree.mapWithKey((_, v) -> { l := "a" :: deref l; v }) |>
        RedBlackTree.mapWithKey((_, v) -> { l := "b" :: deref l; v });
        List.reverse(deref l) == "a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil

    @test
    def mapWithKey10(): Bool & Impure =
        let i = ref 0;
        let keys = List.range(0, 2000);
        let Map(t) = List.zip(keys, List.range(-2000, 0)) |> List.toMap;
        let expected = List.zip(keys, List.range(0, 2000)) |> List.toMap;
        Map(RedBlackTree.mapWithKey((_, _) -> { i := 1 + deref i; deref i }, t)) == expected

}
