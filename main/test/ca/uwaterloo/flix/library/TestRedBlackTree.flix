namespace TestRedBlackTree {

@test
def contains01(): Bool = RedBlackTree.contains(Node(Black, Node(Red, Empty, 1, Empty), 2, Node(Red, Empty, 3, Empty)), 3)

}