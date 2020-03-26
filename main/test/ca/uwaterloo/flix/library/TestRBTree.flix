use RBTree.RBTree;
use RBTree.RBTree.{RBNode};
use RBTree.RBColor.{Red, Black};

namespace TestRBTree {

    @test
    def testSize01(): Bool = RBTree.size(RBTree.empty()) == 0

    @test
    def testSize02(): Bool = RBTree.size(RBTree.insert(1, (), RBTree.empty())) == 1

    @test
    def testSize03(): Bool = RBTree.size(range(0, 3)) == 3

    @test
    def testSize04(): Bool = RBTree.size(range(0, 7)) == 7

    @test
    def testSize05(): Bool = RBTree.size(range(0, 100)) == 100

    @test
    def testDelete01(): Bool = RBTree.size(RBTree.delete(1, range(0, 100))) == 99

    @test
    def testDelete02(): Bool = RBTree.size(RBTree.delete(50, range(0, 100))) == 99

    @test
    def testDelete03(): Bool = RBTree.size(RBTree.delete(99, range(0, 100))) == 99

    @test
    def testBinarySearchTreeInvariant01(): Bool = checkBinarySearchTreeInvariant(range(0, 10))

    @test
    def testBinarySearchTreeInvariant02(): Bool =
        let tree = List.fold((acc, x) -> RBTree.insert(x, (), acc), RBTree.empty(), 9 :: 8 :: 7 :: 6 :: 5 :: 4  :: 3 :: 2 :: 1 :: 0 :: Nil);
        checkBinarySearchTreeInvariant(tree)

    @test
    def testBinarySearchTreeInvariant03(): Bool = checkBinarySearchTreeInvariant(range(0, 100))

    @test
    def testBinarySearchTreeInvariant04(): Bool = checkBinarySearchTreeInvariant(range(0, 1000))

    @test
    def testBinarySearchTreeInvariant05(): Bool =
        let tree = List.fold((acc, x) -> RBTree.delete(x, acc), range(0, 10), 1 :: 2 :: 3 :: Nil);
        checkBinarySearchTreeInvariant(tree)

    @test
    def testBinarySearchTreeInvariant06(): Bool =
        let tree =
        List.fold((acc, x) -> RBTree.delete(x, acc), range(0, 100), 19 :: 78 :: 58 :: 4 :: 1 :: 15 :: 94 :: Nil);
        checkBinarySearchTreeInvariant(tree)

    @test
    def testBinarySearchTreeInvariant07(): Bool & Impure = checkBinarySearchTreeInvariant(random(10))

    @test
    def testBinarySearchTreeInvariant08(): Bool & Impure = checkBinarySearchTreeInvariant(random(100))

    @test
    def testBinarySearchTreeInvariant09(): Bool & Impure = checkBinarySearchTreeInvariant(random(1000))

    @test
    def testRedParentInvariant01(): Bool = checkRedParentInvariant(range(0, 10))

    @test
    def testRedParentInvariant02(): Bool =
        let tree = List.fold((acc, x) -> RBTree.insert(x, (), acc), RBTree.empty(), 9 :: 8 :: 7 :: 6 :: 5 :: 4 :: 3 :: 2   :: 1 :: 0 :: Nil);
        checkRedParentInvariant(tree)

    @test
    def testRedParentInvariant03(): Bool = checkRedParentInvariant(range(0, 100))

    @test
    def testRedParentInvariant04(): Bool = checkRedParentInvariant(range(0, 1000))

    @test
    def testRedParentInvariant05(): Bool =
        let tree =
        List.foldLeft((acc, x) -> RBTree.delete(x, acc), range(0, 10), 1 :: 2 :: 3 :: Nil);
        checkRedParentInvariant(tree)

    @test
    def testRedParentInvariant06(): Bool =
        let tree =
        List.foldLeft((acc, x) -> RBTree.delete(x, acc), range(0, 100), 19 :: 78 :: 58 :: 4 :: 1 :: 15 :: 94    :: Nil);
        checkRedParentInvariant(tree)

    @test
    def testRedParentInvariant07(): Bool & Impure = checkRedParentInvariant(random(10))

    @test
    def testRedParentInvariant08(): Bool & Impure = checkRedParentInvariant(random(100))

    @test
    def testRedParentInvariant09(): Bool & Impure = checkRedParentInvariant(random(1000))

    ///
    /// Returns `true` if and only if the binary search tree invariant holds for `tree`.
    ///
    def checkBinarySearchTreeInvariant(tree: RBTree[k, v]): Bool = match tree {
        case RBNode(_, a, xK, _, b) =>
        if (RBTree.exists((yK, _) -> yK <=> xK > 0, a) || RBTree.exists((zK, _) -> zK <=> xK < 0, b)) false
        else checkBinarySearchTreeInvariant(a) && checkBinarySearchTreeInvariant(b)
        case _ => true
    }

    ///
    /// Helper function for `testRedParentInvariant`.
    ///
    def checkRedParentInvariantHelper(tree: RBTree[k, v], parentColor: RBTree.RBColor): Bool = match tree {
        case RBNode(Red, a, _, _, b) =>
            if (parentColor == Red) false else checkRedParentInvariantHelper(a, Red) &&     checkRedParentInvariantHelper(b, Red)
        case RBNode(Black, a, _, _, b) =>
            checkRedParentInvariantHelper(a, Black) && checkRedParentInvariantHelper(b, Black)
        case _ => true
    }

    ///
    /// Returns `true` if and only if no red node in `tree` has a red parent.
    ///
    def checkRedParentInvariant(tree: RBTree[k, v]): Bool = checkRedParentInvariantHelper(tree, Black)

    ///
    /// Helper function for `range`.
    ///
    def rangeHelper(b: Int32, n: Int32, acc: RBTree[Int32, Unit]): RBTree[Int32, Unit] =
        if (n < b) acc else rangeHelper(b, n - 1, RBTree.insert(n, (), acc))

    ///
    /// Returns a tree of all integers between `b` (inclusive) and `e` (exclusive) as keys, and () as values.
    ///
    /// Returns `empty()` if `b >= e`.
    ///
    def range(b: Int32, e: Int32): RBTree[Int32, Unit] =
        if (b >= e) RBTree.empty() else rangeHelper(b, e - 1, RBTree.empty())

    ///
    /// Returns a tree with `n` nodes `x -> ()` where `x` is a random integer.
    ///
    def random(n: Int32): RBTree[Int32, Unit] & Impure = {
        let seed = match Int64.fromString("42") {
            case Ok(s) => s
            case _ => panic "no seed given"
        };
        let r = Random.newWithSeed(seed);
        List.fold((acc, x) -> RBTree.insert(x, (), acc), RBTree.empty(), List.map(_ -> Random.nextInt32(r),     List.range(0, n)))
    }

}