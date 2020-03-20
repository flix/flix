namespace TestRBTree {

///
/// Returns `true` if and only if the binary search tree invariant holds.
///
def binarySearchTreeInvariantHolds(tree: RBTree[k, v]): Bool = match tree {
    case RBNode(_, a, xK, _, b) =>
    if (RBTree.exists((yK, _) -> yK <=> xK > 0, a) || RBTree.exists((zK, _) -> zK <=> xK < 0, b)) false
    else binarySearchTreeInvariantHolds(a) && binarySearchTreeInvariantHolds(b)
    case _ => true
}

///
/// Helper function for `redParentInvariantHolds`.
///
def redParentInvariantHoldsHelper(tree: RBTree[k, v], parentColor: RBTree.RBColor): Bool = match tree {
    case RBNode(Red, a, _, _, b) =>
        if (parentColor == Red) false else redParentInvariantHoldsHelper(a, Red) && redParentInvariantHoldsHelper(b, Red)
    case RBNode(Black, a, _, _, b) =>
        redParentInvariantHoldsHelper(a, Black) && redParentInvariantHoldsHelper(b, Black)
    case _ => true
}

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
/// Returns a tree of `n` random integers as keys, and () as values.
///
def random(n: Int32): RBTree[Int32, Unit] & Impure = {
    let seed = match Int64.fromString("42") {
        case Ok(s) => s
        case _ => panic "no seed given"
    };
    let r = Random.newWithSeed(seed);
    List.fold((acc, x) -> RBTree.insert(x, (), acc), RBTree.empty(), List.map(_ -> Random.nextInt32(r), List.range(0, n)))
}

///
/// Returns `true` if and only if no red node has a red parent.
///
def redParentInvariantHolds(tree: RBTree[k, v]): Bool = redParentInvariantHoldsHelper(tree, Black)

@test
def size01(): Bool = RBTree.size(RBTree.empty()) == 0

@test
def size02(): Bool = RBTree.size(RBTree.insert(1, (), RBTree.empty())) == 1

@test
def size03(): Bool = RBTree.size(range(0, 3)) == 3

@test
def size04(): Bool = RBTree.size(range(0, 7)) == 7

@test
def size05(): Bool = RBTree.size(range(0, 100)) == 100

@test
def delete01(): Bool = RBTree.size(RBTree.delete(1, range(0, 100))) == 99

@test
def delete02(): Bool = RBTree.size(RBTree.delete(50, range(0, 100))) == 99

@test
def delete03(): Bool = RBTree.size(RBTree.delete(99, range(0, 100))) == 99

@test
def binarySearchTreeInvariant01(): Bool = binarySearchTreeInvariantHolds(range(0, 10))

@test
def binarySearchTreeInvariant02(): Bool =
    let tree = List.fold((acc, x) -> RBTree.insert(x, (), acc), RBTree.empty(), 9 :: 8 :: 7 :: 6 :: 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil);
    binarySearchTreeInvariantHolds(tree)

@test
def binarySearchTreeInvariant03(): Bool = binarySearchTreeInvariantHolds(range(0, 100))

@test
def binarySearchTreeInvariant04(): Bool = binarySearchTreeInvariantHolds(range(0, 1000))

@test
def binarySearchTreeInvariant05(): Bool =
    let tree =
    List.fold((acc, x) -> RBTree.delete(x, acc), range(0, 10), 1 :: 2 :: 3 :: Nil);
    binarySearchTreeInvariantHolds(tree)

@test
def binarySearchTreeInvariant06(): Bool =
    let tree =
    List.fold((acc, x) -> RBTree.delete(x, acc), range(0, 100), 19 :: 78 :: 58 :: 4 :: 1 :: 15 :: 94 :: Nil);
    binarySearchTreeInvariantHolds(tree)

@test
def binarySearchTreeInvariant07(): Bool & Impure = binarySearchTreeInvariantHolds(random(10))

@test
def binarySearchTreeInvariant08(): Bool & Impure = binarySearchTreeInvariantHolds(random(100))

@test
def binarySearchTreeInvariant09(): Bool & Impure = binarySearchTreeInvariantHolds(random(1000))

@test
def redParentInvariant01(): Bool = redParentInvariantHolds(range(0, 10))

@test
def redParentInvariant02(): Bool =
    let tree =
    List.fold((acc, x) -> RBTree.insert(x, (), acc), RBTree.empty(), 9 :: 8 :: 7 :: 6 :: 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil);
    redParentInvariantHolds(tree)

@test
def redParentInvariant03(): Bool = redParentInvariantHolds(range(0, 100))

@test
def redParentInvariant04(): Bool = redParentInvariantHolds(range(0, 1000))

@test
def redParentInvariant05(): Bool =
    let tree =
    List.foldLeft((acc, x) -> RBTree.delete(x, acc), range(0, 10), 1 :: 2 :: 3 :: Nil);
    redParentInvariantHolds(tree)

@test
def redParentInvariant06(): Bool =
    let tree =
    List.foldLeft((acc, x) -> RBTree.delete(x, acc), range(0, 100), 19 :: 78 :: 58 :: 4 :: 1 :: 15 :: 94 :: Nil);
    redParentInvariantHolds(tree)

@test
def redParentInvariant07(): Bool & Impure = redParentInvariantHolds(random(10))

@test
def redParentInvariant08(): Bool & Impure = redParentInvariantHolds(random(100))

@test
def redParentInvariant09(): Bool & Impure = redParentInvariantHolds(random(1000))

}