namespace TestRedBlackTree {

/////////////////////////////////////////////////////////////////////////////
// contains                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def contains01(): Bool = RedBlackTree.contains(Node(Black, Node(Red, Empty, 1, Empty), 2, Node(Red, Empty, 3, Empty)), 3)

/////////////////////////////////////////////////////////////////////////////
// size                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def size01(): Bool = RedBlackTree.size(Empty) == 0

@test
def size02(): Bool = RedBlackTree.size(RedBlackTree.insert(Empty, 7)) == 1

@test
def size03(): Bool = RedBlackTree.size(RedBlackTree.insert(RedBlackTree.insert(Empty, 4), 11)) == 2

@test
def size04(): Bool = RedBlackTree.size(RedBlackTree.insert(RedBlackTree.insert(RedBlackTree.insert(Empty, 2), 3), 9)) == 3

@test
def size05(): Bool = RedBlackTree.size(RedBlackTree.insert(RedBlackTree.insert(RedBlackTree.insert(RedBlackTree.insert(Empty, 1), 2), 4), 7)) == 4

@test
def size06(): Bool = RedBlackTree.size(RedBlackTree.insert(RedBlackTree.insert(RedBlackTree.insert(RedBlackTree.insert(RedBlackTree.insert(Empty, 1), 2), 4), 7), 3)) == 5

@test
def size07(): Bool = RedBlackTree.size(RedBlackTree.insert(RedBlackTree.insert(RedBlackTree.insert(RedBlackTree.insert(RedBlackTree.insert(RedBlackTree.insert(Empty, 1), 2), 4), 7), 3), 0)) == 6

}