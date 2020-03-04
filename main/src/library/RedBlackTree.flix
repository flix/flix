namespace RBTree {

    ///
    /// The red-black tree used by `Set` and `Map`.
    ///
    /// A red-black tree is a balanced binary tree.
    /// Each node is either red or black, although a
    /// transitory color double-black is allowed during deletion.
    /// The red-black tree satisfy the following invariants.
    ///
    /// 1. For all nodes with key x,
    /// the left subtree contains only nodes with keys y < x and
    /// the right subtree contains only nodes with keys z > x.
    /// 2. No red node has a red parent.
    /// 3. Every path from the root to a leaf contains the same
    /// number of black nodes.
    ///
    pub enum RBTree[t] {
        case RBLeaf,
        case RBBLeaf,
        case RBNode(RBColor, RBTree[t], t, RBTree[t])
    }

    ///
    /// The color of a red-black tree node.
    ///
    enum RBColor {
        case Red
        case Black
        case DoubleBlack
    }

    ///
    /// Returns a new, balanced tree identical to `tree`.
    ///
    def balance(tree: RBTree[a]): RBTree[a] = match tree {
        case RBNode(Black, RBNode(Red, RBNode(Red, a, x, b), y, c), z, d) =>
            RBNode(Red, RBNode(Black, a, x, b), y, RBNode(Black, c, z, d))
        case RBNode(Black, RBNode(Red, a, x, RBNode(Red, b, y, c)), z, d) =>
            RBNode(Red, RBNode(Black, a, x, b), y, RBNode(Black, c, z, d))
        case RBNode(Black, a, x, RBNode(Red, RBNode(Red, b, y, c), z, d)) =>
            RBNode(Red, RBNode(Black, a, x, b), y, RBNode(Black, c, z, d))
        case RBNode(Black, a, x, RBNode(Red, b, y, RBNode(Red, c, z, d))) =>
            RBNode(Red, RBNode(Black, a, x, b), y, RBNode(Black, c, z, d))
        case RBNode(DoubleBlack, a, x, RBNode(Red, RBNode(Red, b, y, c), z, d)) =>
            RBNode(Black, RBNode(Black, a, x, b), y, RBNode(Black, c, z, d))
        case RBNode(DoubleBlack, RBNode(Red, a, x, RBNode(Red, b, y, c)), z, d) =>
            RBNode(Black, RBNode(Black, a, x, b), y, RBNode(Black, c, z, d))
        case _ => tree
    }

    ///
    /// Returns a new tree identical to `tree` except the root is colored black.
    ///
    def blacken(tree: RBTree[a]): RBTree[a] = match tree {
        case RBNode(Red, RBNode(Red, a, x, b), y, c) => RBNode(Black, RBNode(Red, a, x, b), y, c)
        case RBNode(Red, a, x, RBNode(Red, b, y, c)) => RBNode(Black, a, x, RBNode(Red, b, y, c))
        case _ => tree
    }

    ///
    /// Helper function for `insert`.
    ///
    def insertHelper(x: a, tree: RBTree[a]): RBTree[a] = match tree {
        case RBLeaf => RBNode(Red, RBLeaf, x, RBLeaf)
        case RBNode(color, a, y, b) if x < y => balance(RBNode(color, insertHelper(x, a), y, b))
        case RBNode(color, a, y, b) if x == y => RBNode(color, a, y, b)
        case RBNode(color, a, y, b) if x > y => balance(RBNode(color, a, y, insertHelper(x, b)))
        case _ => tree
    }

    ///
    /// Returns a new, balanced tree containing all elements from `tree` as well as `x`.
    ///
    pub def insert(x: a, tree: RBTree[a]): RBTree[a] = blacken(insertHelper(x, tree))

    ///
    /// Helper function for `delete`.
    ///
    def rotate(tree: RBTree[a]): RBTree[a] = match tree {
        case RBNode(Red, RBNode(DoubleBlack, a, x, b), y, RBNode(Black, c, z, d)) =>
            balance(RBNode(Black, RBNode(Red, RBNode(Black, a, x, b), y, c), z, d))
        case RBNode(Red, RBBLeaf, y, RBNode(Black, c, z, d)) =>
            balance(RBNode(Black, RBNode(Red, RBLeaf, y, c), z, d))
        case RBNode(Red, RBNode(Black, a, x, b), y, RBNode(DoubleBlack, c, z, d)) =>
            balance(RBNode(Black, a, x, RBNode(Red, b, y, RBNode(Black, c, z, d))))
        case RBNode(Red, RBNode(Black, a, x, b), y, RBBLeaf) =>
            balance(RBNode(Black, a, x, RBNode(Red, b, y, RBLeaf)))
        case RBNode(Black, RBNode(DoubleBlack, a, x, b), y, RBNode(Black, c, z, d)) =>
            balance(RBNode(DoubleBlack, RBNode(Red, RBNode(Black, a, x, b), y, c), z, d))
        case RBNode(Black, RBBLeaf, y, RBNode(Black, c, z, d)) =>
            balance(RBNode(DoubleBlack, RBNode(Red, RBLeaf, y, c), z, d))
        case RBNode(Black, RBNode(Black, a, x, b), y, RBNode(DoubleBlack, c, z, d)) =>
            balance(RBNode(DoubleBlack, a, x, RBNode(Red, b, y, RBNode(Black, c, z, d))))
        case RBNode(Black, RBNode(Black, a, x, b), y, RBBLeaf) =>
            balance(RBNode(DoubleBlack, a, x, RBNode(Red, b, y, RBLeaf)))
        case RBNode(Black, RBNode(DoubleBlack, a, w, b), x, RBNode(Red, RBNode(Black, c, y, d), z, e)) =>
            RBNode(Black, balance(RBNode(Black, RBNode(Red, RBNode(Black, a, w, b), x, c), y, d)), z, e)
        case RBNode(Black, RBBLeaf, x, RBNode(Red, RBNode(Black, c, y, d), z, e)) =>
            RBNode(Black, balance(RBNode(Black, RBNode(Red, RBLeaf, x, c), y, d)), z, e)
        case RBNode(Black, RBNode(Red, a, w, RBNode(Black, b, x, c)), y, RBNode(DoubleBlack, d, z, e)) =>
            RBNode(Black, a, w, balance(RBNode(Black, b, x, RBNode(Red, c, y, RBNode(Black, d, z, e)))))
        case RBNode(Black, RBNode(Red, a, w, RBNode(Black, b, x, c)), y, RBBLeaf) =>
            RBNode(Black, a, w, balance(RBNode(Black, b, x, RBNode(Red, c, y, RBLeaf))))
        case _ => tree
    }

    ///
    /// Returns a new tree identical to `tree` except the root is colored red.
    ///
    def redden(tree: RBTree[a]): RBTree[a] = match tree {
        case RBNode(Black, RBNode(Black, a, x, b), y, RBNode(Black, c, z, d)) =>
            RBNode(Red, RBNode(Black, a, x, b), y, RBNode(Black, c, z, d))
        case _ => tree
    }

    ///
    /// Helper function for `delete`.
    ///
    def minDelete(tree: RBTree[a]): (a, RBTree[a]) = match tree {
        case RBNode(Red, RBLeaf, x, RBLeaf) => (x, RBLeaf)
        case RBNode(Black, RBLeaf, x, RBLeaf) => (x, RBBLeaf)
        case RBNode(Black, RBLeaf, x, RBNode(Red, RBLeaf, y, RBLeaf)) => (x, RBNode(Black, RBLeaf, y, RBLeaf))
        case RBNode(c, a, x, b) =>
            let (z, e) = minDelete(a);
            (z, rotate(RBNode(c, e, x, b)))
        case _ => (0, RBBLeaf)
    }

    ///
    /// Helper function for `delete`.
    ///
    def deleteHelper(x: a, tree: RBTree[a]): RBTree[a] = match tree {
        case RBLeaf => RBLeaf
        case RBNode(Red, RBLeaf, y, RBLeaf) if x == y => RBLeaf
        case RBNode(Red, RBLeaf, y, RBLeaf) if x != y => tree
        case RBNode(Black, RBLeaf, y, RBLeaf) if x == y => RBBLeaf
        case RBNode(Black, RBLeaf, y, RBLeaf) if x != y => tree
        case RBNode(Black, RBNode(Red, RBLeaf, y, RBLeaf), z, RBLeaf) if x < z =>
            RBNode(Black, deleteHelper(x, RBNode(Red, RBLeaf, y, RBLeaf)), z, RBLeaf)
        case RBNode(Black, RBNode(Red, RBLeaf, y, RBLeaf), z, RBLeaf) if x == z =>
            RBNode(Black, RBLeaf, y, RBLeaf)
        case RBNode(Black, RBNode(Red, RBLeaf, y, RBLeaf), z, RBLeaf) if x > z =>
            RBNode(Black, RBNode(Red, RBLeaf, y, RBLeaf), z, RBLeaf)
        case RBNode(c, a, y, b) if x < y => rotate(RBNode(c, deleteHelper(x, a), y, b))
        case RBNode(c, a, y, b) if x == y =>
            let (z, e) = minDelete(b);
            rotate(RBNode(c, a, z, e))
        case RBNode(c, a, y, b) if x > y => rotate(RBNode(c, a, y, deleteHelper(x, b)))
        case _ => tree
    }

    ///
    /// Returns a new, balanced tree containg all elements of `tree` execept `x`.
    ///
    pub def delete(x: a, tree: RBTree[a]): RBTree[a] = redden(deleteHelper(x, tree))

    ///
    /// Returns true if and only if `tree` contains `x`.
    ///
    pub def contains(x: a, tree: RBTree[a]): Bool = match tree {
        case RBLeaf => false
        case RBNode(_, a, y, _) if x < y => contains(x, a)
        case RBNode(_, _, y, b) if x > y => contains(x, b)
        case _ => true
    }

    ///
    /// Returns the height of `tree`.
    ///
    pub def height(tree: RBTree[a]): Int32 = match tree {
        case RBNode(_, a, _, b) => if (height(a) > height(b)) 1 + height(a) else 1 + height(b)
        case _ => 0
    }

    ///
    /// Returns the number of nodes in `tree`.
    ///
    pub def size(tree: RBTree[a]): Int32 = match tree {
        case RBNode(_, a, _, b) => 1 + size(a) + size(b)
        case _ => 0
    }

    pub def empty(): RBTree[a] = RBLeaf

    ///
    /// Returns true if and only if `tree` is the empty tree, i.e. `RBLeaf`.
    ///
    pub def isEmpty(tree: RBTree[a]): Bool = match tree {
        case RBNode(_, _, _, _) => false
        case _ => true
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `tree` going from left to right, i.e. inorder.
    ///
    ///
    pub def foldLeft(f: (b, a) -> b, s: b, tree: RBTree[a]): b = match tree {
        case RBNode(_, a, x, b) => foldLeft(f, f(foldLeft(f, s, a), x), b)
        case _ => s
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `tree` going from right to left, i.e. reverse inorder
    ///
    pub def foldRight(f: (b, a) -> b, s: b, tree: RBTree[a]): b = match tree {
        case RBNode(_, a, x, b) => foldRight(f, f(foldRight(f, s, b), x), a)
        case _ => s
    }

    ///
    /// Optionally returns the left most element of `tree`.
    ///
    pub def leftMost(tree: RBTree[a]): Option[a] = match tree {
        case RBNode(_, a, x, _) => if (isEmpty(a)) Some(x) else leftMost(a)
        case _ => None
    }

    ///
    /// Optionally returns the right most element of `tree`.
    ///
    pub def rightMost(tree: RBTree[a]): Option[a] = match tree {
        case RBNode(_, b, x, _) => if (isEmpty(b)) Some(x) else rightMost(b)
        case _ => None
    }

    ///
    /// Returns a rudimentary string representation of `tree`.
    ///
    /// Note: Currently only supports RBTree[Int32].
    pub def toString(tree: RBTree[Int32]): Str = {
        match tree {
            case RBLeaf => "RBLeaf"
            case RBBLeaf => "RBBLeaf"
            case RBNode(color, a, x, b) => match color {
                case Red => "RBNode(Red, ${toString(a)}, ${Int32.toString(x)}, ${toString(b)})"
                case Black => "RBNode(Black, ${toString(a)}, ${Int32.toString(x)}, ${toString(b)})"
                case DoubleBlack => "RBNode(DoubleBlack, ${toString(a)}, ${Int32.toString(x)}, ${toString(b)})"
            }
        }
    }

}