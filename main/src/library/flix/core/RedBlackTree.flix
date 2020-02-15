///
/// The color of a red-black tree node.
///
enum Color {
    case Red
    case Black
}

///
/// The red-black tree type.
///
enum RedBlackTree {
    case Empty,
    case Node(Color, RedBlackTree, Int32, RedBlackTree)
}

namespace RedBlackTree {

    ///
    /// Returns true if and only if `tree` contains `x`.
    ///
    pub def contains(tree: RedBlackTree, x: Int32): Bool = match tree {
        case Empty => false
        case Node(_, a, y, _) if x < y => contains(a, x)
        case Node(_, _, y, b) if x > y => contains(b, x)
        case _ => true
    }

}