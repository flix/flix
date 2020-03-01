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
enum RedBlackTree[t] {
    case Leaf,
    case DoubleLeaf,
    case Node(Color, RedBlackTree[t], t, RedBlackTree[t])
}

namespace RedBlackTree {

    ///
    /// Returns a new, balanced tree identical to `tree`.
    ///
    def balance(tree: RedBlackTree[a]): RedBlackTree[a] = match tree {
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
    def blacken(tree: RedBlackTree[a]): RedBlackTree[a] = match tree {
        case Node(Red, Node(Red, a, x, b), y, c) => Node(Black, Node(Red, a, x, b), y, c)
        case Node(Red, a, x, Node(Red, b, y, c)) => Node(Black, a, x, Node(Red, b, y, c))
        case _ => tree
    }

    ///
    /// Helper function for `insert`.
    ///
    def insertHelper(x: a, tree: RedBlackTree[a]): RedBlackTree[a] = match tree {
        case Leaf => Node(Red, Leaf, x, Leaf)
        case Node(color, a, y, b) if x < y => balance(Node(color, insertHelper(x, a), y, b))
        case Node(color, a, y, b) if x == y => Node(color, a, y, b)
        case Node(color, a, y, b) if x > y => balance(Node(color, a, y, insertHelper(x, b)))
        case _ => tree
    }

    ///
    /// Returns a new, balanced tree containing all elements from `tree` as well as `x`.
    ///
    pub def insert(x: a, tree: RedBlackTree[a]): RedBlackTree[a] = blacken(insertHelper(x, tree))

    ///
    /// Helper function for `delete`.
    ///
    def rotate(tree: RedBlackTree[a]): RedBlackTree[a] = match tree {
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
    def redden(tree: RedBlackTree[a]): RedBlackTree[a] = match tree {
        case Node(Black, Node(Black, a, x, b), y, Node(Black, c, z, d)) =>
            Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
        case _ => tree
    }

    ///
    /// Helper function for `delete`.
    ///
    def minDelete(tree: RedBlackTree[a]): (a, RedBlackTree[a]) = match tree {
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
    def deleteHelper(x: a, tree: RedBlackTree[a]): RedBlackTree[a] = match tree {
        case Leaf => Leaf
        case Node(Red, Leaf, y, Leaf) if x == y => Leaf
        case Node(Red, Leaf, y, Leaf) if x != y => tree
        case Node(Black, Leaf, y, Leaf) if x == y => DoubleLeaf
        case Node(Black, Leaf, y, Leaf) if x != y => tree
        case Node(Black, Node(Red, Leaf, y, Leaf), z, Leaf) if x < z =>
            Node(Black, deleteHelper(x, Node(Red, Leaf, y, Leaf)), z, Leaf)
        case Node(Black, Node(Red, Leaf, y, Leaf), z, Leaf) if x == z =>
            Node(Black, Leaf, y, Leaf)
        case Node(Black, Node(Red, Leaf, y, Leaf), z, Leaf) if x > z =>
            Node(Black, Node(Red, Leaf, y, Leaf), z, Leaf)
        case Node(c, a, y, b) if x < y => rotate(Node(c, deleteHelper(x, a), y, b))
        case Node(c, a, y, b) if x == y =>
            let (z, e) = minDelete(b);
            rotate(Node(c, a, z, e))
        case Node(c, a, y, b) if x > y => rotate(Node(c, a, y, deleteHelper(x, b)))
        case _ => tree
    }

    ///
    /// Returns a new, balanced tree containg all elements of `tree` execept `x`.
    ///
    pub def delete(x: a, tree: RedBlackTree[a]): RedBlackTree[a] = redden(deleteHelper(x, tree))

    ///
    /// Returns true if and only if `tree` contains `x`.
    ///
    pub def contains(x: a, tree: RedBlackTree[a]): Bool = match tree {
        case Leaf => false
        case Node(_, a, y, _) if x < y => contains(x, a)
        case Node(_, _, y, b) if x > y => contains(x, b)
        case _ => true
    }

    ///
    /// Returns the height of `tree`.
    ///
    pub def height(tree: RedBlackTree[a]): Int32 = match tree {
        case Node(_, a, _, b) => if (height(a) > height(b)) 1 + height(a) else 1 + height(b)
        case _ => 0
    }

    ///
    /// Returns the number of nodes in `tree`.
    ///
    pub def size(tree: RedBlackTree[a]): Int32 = match tree {
        case Node(_, a, _, b) => 1 + size(a) + size(b)
        case _ => 0
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `tree` going from left to right, i.e. inorder.
    ///
    ///
    pub def foldLeft(f: (b, a) -> b, s: b, tree: RedBlackTree[a]): b = match tree {
        case Node(_, a, x, b) => foldLeft(f, f(foldLeft(f, s, a), x), b)
        case _ => s
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `tree` going from right to left, i.e. reverse inorder
    ///
    pub def foldRight(f: (b, a) -> b, s: b, tree: RedBlackTree[a]): b = match tree {
        case Node(_, a, x, b) => foldRight(f, f(foldRight(f, s, b), x), a)
        case _ => s
    }

    ///
    /// Optionally returns the left most element of `tree`.
    ///
    pub def leftMost(tree: RedBlackTree[a]): Option[a] = match tree {
        case Node(_, a, x, _) => match leftMost(a) {
            case None => Some(x)
            case _ => leftMost(a)
        }
        case _ => None
    }

    ///
    /// Optionally returns the right most element of `tree`.
    ///
    pub def rightMost(tree: RedBlackTree[a]): Option[a] = match tree {
        case Node(_, _, x, b) => match rightMost(b) {
            case None => Some(x)
            case _ => rightMost(b)
        }
        case _ => None
    }

    ///
    /// Returns a rudimentary string representation of `tree`.
    ///
    /// Note: Currently only supports RedBlackTree[Int32].
    pub def toString(tree: RedBlackTree[Int32]): Str = {
        match tree {
            case Leaf => "Leaf"
            case DoubleLeaf => "DoubleLeaf"
            case Node(color, a, x, b) => match color {
                case Red => "Node(Red, ${toString(a)}, ${Int32.toString(x)}, ${toString(b)})"
                case Black => "Node(Black, ${toString(a)}, ${Int32.toString(x)}, ${toString(b)})"
                case DoubleBlack => "Node(DoubleBlack, ${toString(a)}, ${Int32.toString(x)}, ${toString(b)})"
            }
        }
    }

}