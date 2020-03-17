namespace TestRBTree {

@test
def size01(): Bool = RBTree.size(RBTree.empty()) == 0

@test
def size02(): Bool = RBTree.size(RBTree.insert(1, RBTree.empty())) == 1

@test
def size03(): Bool = RBTree.size(RBTree.range(0, 3)) == 3

@test
def size04(): Bool = RBTree.size(RBTree.range(0, 7)) == 7

@test
def size05(): Bool = RBTree.size(RBTree.range(0, 100)) == 100

@test
def delete01(): Bool = RBTree.size(RBTree.delete(1, RBTree.range(0, 100))) == 99

@test
def delete02(): Bool = RBTree.size(RBTree.delete(50, RBTree.range(0, 100))) == 99

@test
def delete03(): Bool = RBTree.size(RBTree.delete(99, RBTree.range(0, 100))) == 99

@test
def binarySearchTreeInvariant01(): Bool = RBTree.binarySearchTreeInvariantHolds(RBTree.range(0, 10))

@test
def binarySearchTreeInvariant02(): Bool =
    let tree = List.fold((acc, x) -> RBTree.insert(x, acc), RBTree.empty(), 9 :: 8 :: 7 :: 6 :: 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil);
    RBTree.binarySearchTreeInvariantHolds(tree)

@test
def binarySearchTreeInvariant03(): Bool = RBTree.binarySearchTreeInvariantHolds(RBTree.range(0, 100))

@test
def binarySearchTreeInvariant04(): Bool = RBTree.binarySearchTreeInvariantHolds(RBTree.range(0, 1000))

@test
def binarySearchTreeInvariant05(): Bool =
    let tree = List.fold((acc, x) -> RBTree.delete(x, acc), RBTree.range(0, 10), 1 :: 2 :: 3 :: Nil);
    RBTree.binarySearchTreeInvariantHolds(tree)

@test
def binarySearchTreeInvariant06(): Bool =
    let tree = List.fold((acc, x) -> RBTree.delete(x, acc), RBTree.range(0, 100), 19 :: 78 :: 58 :: 4 :: 1 :: 15 :: 94 :: Nil);
    RBTree.binarySearchTreeInvariantHolds(tree)

@test
def binarySearchTreeInvariant07(): Bool & Impure = RBTree.binarySearchTreeInvariantHolds(RBTree.random(10))

@test
def binarySearchTreeInvariant08(): Bool & Impure = RBTree.binarySearchTreeInvariantHolds(RBTree.random(100))

@test
def binarySearchTreeInvariant09(): Bool & Impure = RBTree.binarySearchTreeInvariantHolds(RBTree.random(1000))

@test
def redParentInvariant01(): Bool = RBTree.redParentInvariantHolds(RBTree.range(0, 10))

@test
def redParentInvariant02(): Bool =
    let tree = List.fold((acc, x) -> RBTree.insert(x, acc), RBTree.empty(), 9 :: 8 :: 7 :: 6 :: 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil);
    RBTree.redParentInvariantHolds(tree)

@test
def redParentInvariant03(): Bool = RBTree.redParentInvariantHolds(RBTree.range(0, 100))

@test
def redParentInvariant04(): Bool = RBTree.redParentInvariantHolds(RBTree.range(0, 1000))

@test
def redParentInvariant05(): Bool =
    let tree = List.foldLeft((acc, x) -> RBTree.delete(x, acc), RBTree.range(0, 10), 1 :: 2 :: 3 :: Nil);
    RBTree.redParentInvariantHolds(tree)

@test
def redParentInvariant06(): Bool =
    let tree = List.foldLeft((acc, x) -> RBTree.delete(x, acc), RBTree.range(0, 100), 19 :: 78 :: 58 :: 4 :: 1 :: 15 :: 94 :: Nil);
    RBTree.redParentInvariantHolds(tree)

@test
def redParentInvariant07(): Bool & Impure = RBTree.redParentInvariantHolds(RBTree.random(10))

@test
def redParentInvariant08(): Bool & Impure = RBTree.redParentInvariantHolds(RBTree.random(100))

@test
def redParentInvariant09(): Bool & Impure = RBTree.redParentInvariantHolds(RBTree.random(1000))

}