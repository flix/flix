///
/// The color of a red-black tree node.
///
enum Color {
    case Red
    case Black
    case DoubleBlack
}

///
/// The red-black tree type.
///
enum RBTree {
    case Leaf,
    case DoubleLeaf,
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
        case Node(DoubleBlack, a, x, Node(Red, Node(Red, b, y, c), z, d)) =>
            Node(Black, Node(Black, a, x, b), y, Node(Black, c, z, d))
        case Node(DoubleBlack, Node(Red, a, x, Node(Red, b, y, c)), z, d) =>
            Node(Black, Node(Black, a, x, b), y, Node(Black, c, z, d))
        case _ => tree
    }

    ///
    /// Returns a new tree identical to `tree` except the root is colored black.
    ///
    def blacken(tree: RBTree): RBTree = match tree {
        case Node(Red, Node(Red, a, x, b), y, c) => Node(Black, Node(Red, a, x, b), y, c)
        case Node(Red, a, x, Node(Red, b, y, c)) => Node(Black, a, x, Node(Red, b, y, c))
        case _ => tree
    }

    ///
    /// Helper function for `insert`.
    ///
    def insertHelper(tree: RBTree, x: Int32): RBTree = match tree {
        case Leaf => Node(Red, Leaf, x, Leaf)
        case Node(color, a, y, b) if x < y => balance(Node(color, insertHelper(a, x), y, b))
        case Node(color, a, y, b) if x == y => Node(color, a, y, b)
        case Node(color, a, y, b) if x > y => balance(Node(color, a, y, insertHelper(b, x)))
        case _ => tree
    }

    ///
    /// Returns a new, balanced tree containing all elements from `tree` as well as `x`.
    ///
    pub def insert(tree: RBTree, x: Int32): RBTree = blacken(insertHelper(tree, x))

    ///
    /// Helper function for `delete`.
    ///
    def rotate(tree: RBTree): RBTree = match tree {
        case Node(Red, Node(DoubleBlack, a, x, b), y, Node(Black, c, z, d)) =>
            balance(Node(Black, Node(Red, Node(Black, a, x, b), y, c), z, d))
        case Node(Red, DoubleLeaf, y, Node(Black, c, z, d)) =>
            balance(Node(Black, Node(Red, Leaf, y, c), z, d))
        case Node(Red, Node(Black, a, x, b), y, Node(DoubleBlack, c, z, d)) =>
            balance(Node(Black, a, x, Node(Red, b, y, Node(Black, c, z, d))))
        case Node(Red, Node(Black, a, x, b), y, DoubleLeaf) =>
            balance(Node(Black, a, x, Node(Red, b, y, Leaf)))
        case Node(Black, Node(DoubleBlack, a, x, b), y, Node(Black, c, z, d)) =>
            balance(Node(DoubleBlack, Node(Red, Node(Black, a, x, b), y, c), z, d))
        case Node(Black, DoubleLeaf, y, Node(Black, c, z, d)) =>
            balance(Node(DoubleBlack, Node(Red, Leaf, y, c), z, d))
        case Node(Black, Node(Black, a, x, b), y, Node(DoubleBlack, c, z, d)) =>
            balance(Node(DoubleBlack, a, x, Node(Red, b, y, Node(Black, c, z, d))))
        case Node(Black, Node(Black, a, x, b), y, DoubleLeaf) =>
            balance(Node(DoubleBlack, a, x, Node(Red, b, y, Leaf)))
        case Node(Black, Node(DoubleBlack, a, w, b), x, Node(Red, Node(Black, c, y, d), z, e)) =>
            Node(Black, balance(Node(Black, Node(Red, Node(Black, a, w, b), x, c), y, d)), z, e)
        case Node(Black, DoubleLeaf, x, Node(Red, Node(Black, c, y, d), z, e)) =>
            Node(Black, balance(Node(Black, Node(Red, Leaf, x, c), y, d)), z, e)
        case Node(Black, Node(Red, a, w, Node(Black, b, x, c)), y, Node(DoubleBlack, d, z, e)) =>
            Node(Black, a, w, balance(Node(Black, b, x, Node(Red, c, y, Node(Black, d, z, e)))))
        case Node(Black, Node(Red, a, w, Node(Black, b, x, c)), y, DoubleLeaf) =>
            Node(Black, a, w, balance(Node(Black, b, x, Node(Red, c, y, Leaf))))
        case _ => tree
    }

    ///
    /// Returns a new tree identical to `tree` except the root is colored red.
    ///
    def redden(tree: RBTree): RBTree = match tree {
        case Node(Black, Node(Black, a, x, b), y, Node(Black, c, z, d)) =>
        Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
        case _ => tree
    }

    ///
    /// Helper function for `delete`.
    ///
    def minDelete(tree: RBTree): (Int32, RBTree) = match tree {
        case Node(Red, Leaf, x, Leaf) => (x, Leaf)
        case Node(Black, Leaf, x, Leaf) => (x, DoubleLeaf)
        case Node(Black, Leaf, x, Node(Red, Leaf, y, Leaf)) => (x, Node(Black, Leaf, y, Leaf))
        case Node(c, a, x, b) =>
            let (z, e) = minDelete(a);
            (z, rotate(Node(c, e, x, b)))
        case _ => (0, DoubleLeaf)
    }

    ///
    /// Helper function for `delete`.
    ///
    def deleteHelper(tree: RBTree, x: Int32): RBTree = match tree {
        case Leaf => Leaf
        case Node(Red, Leaf, y, Leaf) if x == y => Leaf
        case Node(Red, Leaf, y, Leaf) if x != y => tree
        case Node(Black, Leaf, y, Leaf) if x == y => DoubleLeaf
        case Node(Black, Leaf, y, Leaf) if x != y => tree
        case Node(Black, Node(Red, Leaf, y, Leaf), z, Leaf) if x < z =>
            Node(Black, deleteHelper(Node(Red, Leaf, y, Leaf), x), z, Leaf)
        case Node(Black, Node(Red, Leaf, y, Leaf), z, Leaf) if x == z =>
            Node(Black, Leaf, y, Leaf)
        case Node(Black, Node(Red, Leaf, y, Leaf), z, Leaf) if x > z =>
            Node(Black, Node(Red, Leaf, y, Leaf), z, Leaf)
        case Node(c, a, y, b) if x < y => rotate(Node(c, deleteHelper(a, x), y, b))
        case Node(c, a, y, b) if x == y =>
            let (z, e) = minDelete(b);
            rotate(Node(c, a, z, e))
        case Node(c, a, y, b) if x > y => rotate(Node(c, a, y, deleteHelper(b, x)))
        case _ => tree
    }

    ///
    /// Returns a new, balanced tree containg all elements of `tree` execept `x`.
    ///
    pub def delete(tree: RBTree, x: Int32): RBTree = redden(deleteHelper(tree, x))

    ///
    /// Returns true if and only if `tree` contains `x`.
    ///
    pub def contains(tree: RBTree, x: Int32): Bool = match tree {
        case Leaf => false
        case Node(_, a, y, _) if x < y => contains(a, x)
        case Node(_, _, y, b) if x > y => contains(b, x)
        case _ => true
    }

    ///
    /// Returns the height of `tree`.
    ///
    pub def height(tree: RBTree): Int32 = match tree {
        case Node(_, a, _, b) => if (height(a) > height(b)) 1 + height(a) else 1 + height(b)
        case _ => 0
    }

    ///
    /// Returns the number of nodes in `tree`.
    ///
    pub def size(tree: RBTree): Int32 = match tree {
        case Node(_, a, _, b) => 1 + size(a) + size(b)
        case _ => 0
    }

    ///
    /// Returns a rudimentary string representation of `tree`.
    ///
    pub def toString(tree: RBTree): Str = {
        import java.lang.String:valueOf(Int32);
        match tree {
            case Leaf => "Leaf"
            case DoubleLeaf => "DoubleLeaf"
            case Node(color, a, x, b) => match color {
                case Red => "Node(Red, ${toString(a)}, ${valueOf(x)}, ${toString(b)})"
                case Black => "Node(Black, ${toString(a)}, ${valueOf(x)}, ${toString(b)})"
                case DoubleBlack => "Node(DoubleBlack, ${toString(a)}, ${valueOf(x)}, ${toString(b)})"
            }
        }
    }

}