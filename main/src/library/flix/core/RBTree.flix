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
enum RBTree {
    case Empty,
    case Node(Color, RBTree, Int32, RBTree)
}

namespace RBTree {

    ///
    /// Returns a new, balanced tree identical to `tree`.
    ///
    def balance(tree: RBTree): RBTree = match tree {
        case Node(Black, Node(Red, Node(Red, a, x, b), y, c), z, d) =>
            Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
        case Node(Black, Node(Red, a, x, Node(Red, b, y, c)), z, d) =>
            Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
        case Node(Black, a, x, Node(Red, Node(Red, b, y, c), z, d)) =>
            Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
        case Node(Black, a, x, Node(Red, b, y, Node(Red, c, z, d))) =>
            Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
        case _ => tree
    }

    ///
    /// Returns a new tree identical to `tree` except the root is colored black.
    ///
    def blacken(tree: RBTree): RBTree = match tree {
        case Empty => Empty
        case Node(_, a, y, b) => Node(Black, a, y, b)
    }

    ///
    /// Helper function for `insert`.
    ///
    pub def insertHelper(tree: RBTree, x: Int32): RBTree = match tree {
        case Empty => Node(Red, Empty, x, Empty)
        case Node(color, a, y, b) if x < y => balance(Node(color, insertHelper(a, x), y, b))
        case Node(color, a, y, b) if x > y => balance(Node(color, a, y, insertHelper(b, x)))
        case Node(color, a, y, b) => Node(color, a, y, b)
    }

    ///
    /// Returns a new, balanced tree containing all elements from `tree` as well as `x`.
    ///
    pub def insert(tree: RBTree, x: Int32): RBTree =
        blacken(insertHelper(tree, x))

    ///
    /// Returns true if and only if `tree` contains `x`.
    ///
    pub def contains(tree: RBTree, x: Int32): Bool = match tree {
        case Empty => false
        case Node(_, a, y, _) if x < y => contains(a, x)
        case Node(_, _, y, b) if x > y => contains(b, x)
        case _ => true
    }

    ///
    /// Returns the number of nodes in `tree`.
    ///
    pub def size(tree: RBTree): Int32 = match tree {
        case Empty => 0
        case Node(_, a, _, b) => 1 + size(a) + size(b)
    }

    ///
    /// Helper function for `toString`.
    ///
    def intToString(i: Int32): String = {
        import java.lang.String:valueOf(Int32);
        valueOf(i)
    }

    ///
    /// Returns a rudimentary string representation of `tree`.
    ///
    pub def toString(tree: RBTree): Str = match tree {
        case Empty => "Empty"
        case Node(color, a, x, b) => match color {
            case Red => "Node(Red, ${toString(a)}, ${intToString(x)}, ${toString(b)})"
            case Black => "Node(Black, ${toString(a)}, ${intToString(x)}, ${toString(b)})"
        }
    }

}