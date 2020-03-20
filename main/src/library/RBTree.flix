///
/// An immutable red-black tree implementation with keys
/// of type `k` and values of type `v`.
///
/// A red-black tree is a balanced binary search tree.
/// Each node is either red or black, although a transitory color double-black is allowed during deletion.
/// The red-black tree satisfy the following invariants.
///
/// 1. For all nodes with key `x`,
/// the left subtree contains only nodes with keys `y` < `x` and
/// the right subtree contains only nodes with keys `z` > `x`.
/// 2. No red node has a red parent.
/// 3. Every path from the root to a leaf contains the same
/// number of black nodes.
///
enum RBTree[k, v] {
    case RBLeaf,
    case RBBLeaf,
    case RBNode(RBTree.RBColor, RBTree[k, v], k, v, RBTree[k, v])
}

namespace RBTree {

    ///
    /// The color of a red-black tree node.
    ///
    pub enum RBColor {
        case Red
        case Black
        case DoubleBlack
    }

    ///
    /// Helper function for `insert` and `delete`.
    ///
    def balance(tree: RBTree[k, v]): RBTree[k, v] = match tree {
        case RBNode(Black, RBNode(Red, RBNode(Red, a, xK, xV, b), yK, yV, c), zK, zV, d) =>
            RBNode(Red, RBNode(Black, a, xK, xV, b), yK, yV, RBNode(Black, c, zK, zV, d))
        case RBNode(Black, RBNode(Red, a, xK, xV, RBNode(Red, b, yK, yV, c)), zK, zV, d) =>
            RBNode(Red, RBNode(Black, a, xK, xV, b), yK, yV, RBNode(Black, c, zK, zV, d))
        case RBNode(Black, a, xK, xV, RBNode(Red, RBNode(Red, b, yK, yV, c), zK, zV, d)) =>
            RBNode(Red, RBNode(Black, a, xK, xV, b), yK, yV, RBNode(Black, c, zK, zV, d))
        case RBNode(Black, a, xK, xV, RBNode(Red, b, yK, yV, RBNode(Red, c, zK, zV, d))) =>
            RBNode(Red, RBNode(Black, a, xK, xV, b), yK, yV, RBNode(Black, c, zK, zV, d))
        case RBNode(DoubleBlack, a, xK, xV, RBNode(Red, RBNode(Red, b, yK, yV, c), zK, zV, d)) =>
            RBNode(Black, RBNode(Black, a, xK, xV, b), yK, yV, RBNode(Black, c, zK, zV, d))
        case RBNode(DoubleBlack, RBNode(Red, a, xK, xV, RBNode(Red, b, yK, yV, c)), zK, zV, d) =>
            RBNode(Black, RBNode(Black, a, xK, xV, b), yK, yV, RBNode(Black, c, zK, zV, d))
        case _ => tree
    }

    ///
    /// Helper function for `insert`.
    ///
    def blacken(tree: RBTree[k, v]): RBTree[k, v] = match tree {
        case RBNode(Red, RBNode(Red, a, xK, xV, b), yK, yV, c) =>
            RBNode(Black, RBNode(Red, a, xK, xV, b), yK, yV, c)
        case RBNode(Red, a, xK, xV, RBNode(Red, b, yK, yV, c)) =>
            RBNode(Black, a, xK, xV, RBNode(Red, b, yK, yV, c))
        case _ => tree
    }

    ///
    /// Helper function for `insert`.
    ///
    def insertHelper(xK: k, xV: v, tree: RBTree[k, v]): RBTree[k, v] = match tree {
        case RBLeaf => RBNode(Red, RBLeaf, xK, xV, RBLeaf)
        case RBNode(color, a, yK, yV, b) =>
            let cmp = xK <=> yK;
            if (cmp < 0) balance(RBNode(color, insertHelper(xK, xV, a), yK, yV, b))
            else if (cmp == 0) RBNode(color, a, yK, yV, b)
            else  balance(RBNode(color, a, yK, yV, insertHelper(xK, xV, b)))
        case _ => tree
    }

    ///
    /// Returns a new, balanced tree containing all nodes of `tree` and a node having key `xK` and value `xV` added.
    ///
    pub def insert(xK: k, xV: v, tree: RBTree[k, v]): RBTree[k, v] = blacken(insertHelper(xK, xV, tree))

    ///
    /// Helper function for `delete`.
    ///
    def rotate(tree: RBTree[k, v]): RBTree[k, v] = match tree {
        case RBNode(Red, RBNode(DoubleBlack, a, xK, xV, b), yK, yV, RBNode(Black, c, zK, zV, d)) =>
            balance(RBNode(Black, RBNode(Red, RBNode(Black, a, xK, xV, b), yK, yV, c), zK, zV, d))
        case RBNode(Red, RBBLeaf, yK, yV, RBNode(Black, c, zK, zV, d)) =>
            balance(RBNode(Black, RBNode(Red, RBLeaf, yK, yV, c), zK, zV, d))
        case RBNode(Red, RBNode(Black, a, xK, xV, b), yK, yV, RBNode(DoubleBlack, c, zK, zV, d)) =>
            balance(RBNode(Black, a, xK, xV, RBNode(Red, b, yK, yV, RBNode(Black, c, zK, zV, d))))
        case RBNode(Red, RBNode(Black, a, xK, xV, b), yK, yV, RBBLeaf) =>
            balance(RBNode(Black, a, xK, xV, RBNode(Red, b, yK, yV, RBLeaf)))
        case RBNode(Black, RBNode(DoubleBlack, a, xK, xV, b), yK, yV, RBNode(Black, c, zK, zV, d)) =>
            balance(RBNode(DoubleBlack, RBNode(Red, RBNode(Black, a, xK, xV, b), yK, yV, c), zK, zV, d))
        case RBNode(Black, RBBLeaf, yK, yV, RBNode(Black, c, zK, zV, d)) =>
            balance(RBNode(DoubleBlack, RBNode(Red, RBLeaf, yK, yV, c), zK, zV, d))
        case RBNode(Black, RBNode(Black, a, xK, xV, b), yK, yV, RBNode(DoubleBlack, c, zK, zV, d)) =>
            balance(RBNode(DoubleBlack, a, xK, xV, RBNode(Red, b, yK, yV, RBNode(Black, c, zK, zV, d))))
        case RBNode(Black, RBNode(Black, a, xK, xV, b), yK, yV, RBBLeaf) =>
            balance(RBNode(DoubleBlack, a, xK, xV, RBNode(Red, b, yK, yV, RBLeaf)))
        case RBNode(Black, RBNode(DoubleBlack, a, wK, wV, b), xK, xV, RBNode(Red, RBNode(Black, c, yK, yV, d), zK, zV, e)) =>
            RBNode(Black, balance(RBNode(Black, RBNode(Red, RBNode(Black, a, wK, wV, b), xK, xV, c), yK, yV, d)), zK, zV, e)
        case RBNode(Black, RBBLeaf, xK, xV, RBNode(Red, RBNode(Black, c, yK, yV, d), zK, zV, e)) =>
            RBNode(Black, balance(RBNode(Black, RBNode(Red, RBLeaf, xK, xV, c), yK, yV, d)), zK, zV, e)
        case RBNode(Black, RBNode(Red, a, wK, wV, RBNode(Black, b, xK, xV, c)), yK, yV, RBNode(DoubleBlack, d, zK, zV, e)) =>
            RBNode(Black, a, wK, wV, balance(RBNode(Black, b, xK, xV, RBNode(Red, c, yK, yV, RBNode(Black, d, zK, zV, e)))))
        case RBNode(Black, RBNode(Red, a, wK, wV, RBNode(Black, b, xK, xV, c)), yK, yV, RBBLeaf) =>
            RBNode(Black, a, wK, wV, balance(RBNode(Black, b, xK, xV, RBNode(Red, c, yK, yV, RBLeaf))))
        case _ => tree
    }

    ///
    /// Helper function for `delete`.
    ///
    def redden(tree: RBTree[k, v]): RBTree[k, v] = match tree {
        case RBNode(Black, RBNode(Black, a, xK, xV, b), yK, yV, RBNode(Black, c, zK, zV, d)) =>
            RBNode(Red, RBNode(Black, a, xK, xV, b), yK, yV, RBNode(Black, c, zK, zV, d))
        case _ => tree
    }

    ///
    /// Helper function for `delete`.
    ///
    def minDelete(tree: RBTree[k, v]): (k, v, RBTree[k, v]) = match tree {
        case RBNode(Red, RBLeaf, xK, xV, RBLeaf) => (xK, xV, RBLeaf)
        case RBNode(Black, RBLeaf, xK, xV, RBLeaf) => (xK, xV, RBBLeaf)
        case RBNode(Black, RBLeaf, xK, xV, RBNode(Red, RBLeaf, yK, yV, RBLeaf)) =>
            (xK, xV, RBNode(Black, RBLeaf, yK, yV, RBLeaf))
        case RBNode(c, a, xK, xV, b) =>
            let (zK, zV, e) = minDelete(a);
            (zK, zV, rotate(RBNode(c, e, xK, xV, b)))
        case _ => panic "impossible"
    }

    ///
    /// Helper function for `delete`.
    ///
    def deleteHelper(xK: k, tree: RBTree[k, v]): RBTree[k, v] = match tree {
        case RBNode(Red, RBLeaf, yK, _, RBLeaf) => if (xK <=> yK == 0) RBLeaf else tree
        case RBNode(Black, RBLeaf, yK, _, RBLeaf) => if (xK <=> yK == 0) RBBLeaf else tree
        case RBNode(Black, RBNode(Red, RBLeaf, yK, yV, RBLeaf), zK, zV, RBLeaf) =>
            let cmp = xK <=> zK;
            if (cmp < 0) RBNode(Black, deleteHelper(xK, RBNode(Red, RBLeaf, yK, yV, RBLeaf)), zK, zV, RBLeaf)
            else if (cmp == 0) RBNode(Black, RBLeaf, yK, yV, RBLeaf)
            else RBNode(Black, RBNode(Red, RBLeaf, yK, yV, RBLeaf), zK, zV, RBLeaf)
        case RBNode(c, a, yK, yV, b) =>
            let cmp = xK <=> yK;
            if (cmp < 0) rotate(RBNode(c, deleteHelper(xK, a), yK, yV, b))
            else if (cmp == 0) let (zK, zV, e) = minDelete(b); rotate(RBNode(c, a, zK, zV, e))
            else rotate(RBNode(c, a, yK, yV, deleteHelper(xK, b)))
        case _ => tree
    }

    ///
    /// Returns a new, balanced tree containing all nodes of `tree` except the node with key `xK`.
    ///
    pub def delete(xK: k, tree: RBTree[k, v]): RBTree[k, v] = redden(deleteHelper(xK, tree))

    ///
    /// Returns true if and only if `tree` contains a node with key `xK`.
    ///
    pub def contains(xK: k, tree: RBTree[k, v]): Bool = match tree {
        case RBNode(_, a, yK, _, _) if xK <=> yK < 0 => contains(xK, a)
        case RBNode(_, _, yK, _, _) if xK <=> yK == 0 => true
        case RBNode(_, _, yK, _, b) if xK <=> yK > 0 => contains(xK, b)
        case _ => false
    }

    ///
    /// Returns the height of `tree`.
    ///
    pub def height(tree: RBTree[k, v]): Int32 = match tree {
        case RBNode(_, a, _, _, b) => if (height(a) > height(b)) 1 + height(a) else 1 + height(b)
        case _ => 0
    }

    ///
    /// Returns the number of nodes in `tree`.
    ///
    pub def size(tree: RBTree[k, v]): Int32 = match tree {
        case RBNode(_, a, _, _, b) => 1 + size(a) + size(b)
        case _ => 0
    }

    ///
    /// Returns the empty tree.
    ///
    pub def empty(): RBTree[k, v] = RBLeaf

    ///
    /// Returns true if and only if `tree` is the empty tree.
    ///
    pub def isEmpty(tree: RBTree[k, v]): Bool = match tree {
        case RBNode(_, _, _, _, _) => false
        case _ => true
    }

    ///
    /// Optionally returns the first key-value pair of `tree` that satisfies the predicate `f` when searching from left to right.
    ///
    /// The function `f` must be pure.
    ///
    pub def findLeft(f: (k, v) -> Bool, tree: RBTree[k, v]): Option[(k, v)] = match tree {
        case RBNode(_, a, xK, xV, b) => match findLeft(f, a) {
            case Some((yK, yV)) => Some((yK, yV))
            case _ => if (f(xK, xV)) Some((xK, xV)) else findLeft(f, b)
        }
        case _ => None
    }

    ///
    /// Optionally returns the first key-value pair of `tree` that satisfies the predicate `f` when searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    pub def findRight(f: (k, v) -> Bool, tree: RBTree[k, v]): Option[(k, v)] = match tree {
        case RBNode(_, a, xK, xV, b) => match findRight(f, b) {
            case Some((yK, yV)) => Some((yK, yV))
            case _ => if (f(xK, xV)) Some(xK, xV) else findRight(f, a)
        }
        case _ => None
    }

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `tree` going from left to right, i.e., inorder.
    ///
    ///
    pub def foldLeft(f: (b, k, v) -> b, s: b, tree: RBTree[k, v]): b = match tree {
        case RBNode(_, a, xK, xV, b) => foldLeft(f, f(foldLeft(f, s, a), xK, xV), b)
        case _ => s
    }

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `tree` going from right to left, i.e., reverse inorder
    ///
    pub def foldRight(f: (b, k, v) -> b, s: b, tree: RBTree[k, v]): b = match tree {
        case RBNode(_, a, xK, xV, b) => foldRight(f, f(foldRight(f, s, b), xK, xV), a)
        case _ => s
    }

    ///
    /// Helper function for `reduceLeft` and `reduceRight`.
    ///
    pub def reduceHelper(f: (k, v, k, v) -> (k, v), xK: k, xV: v, that: Option[(k, v)]): (k, v) = match that {
        case Some((yK, yV)) => f(xK, xV, yK, yV)
        case _ => (xK, xV)
    }

    ///
    /// Applies `f` to all key-value pairs in `tree` going from left to right, i.e., inorder until a single value `v` is obtained.
    ///
    pub def reduceLeft(f: (k, v, k, v) -> (k, v) & e, tree: RBTree[k, v]): Option[(k, v)] = match tree {
        case RBNode(_, a, xK, xV, b) =>
            let (yK, yV) = reduceHelper(f, xK, xV, reduceLeft(f, a));
            Some(reduceHelper(f, yK, yV, reduceLeft(f, b)))
        case _ => None
    }

    ///
    /// Applies `f` to all key-value pairs in `tree` going from right to left, i.e., reverse inorder until a single value `v` is obtained.
    ///
    pub def reduceRight(f: (k, v, k, v) -> (k, v) & e, tree: RBTree[k, v]): Option[(k, v)] = match tree {
        case RBNode(_, a, xK, xV, b) =>
            let (yK, yV) = reduceHelper(f, xK, xV, reduceRight(f, b));
            Some(reduceHelper(f, yK, yV, reduceRight(f, a)))
        case _ => None
    }

    ///
    /// Optionally returns the left most key-value pair of `tree`.
    ///
    pub def leftMost(tree: RBTree[k, v]): Option[(k, v)] = match tree {
        case RBNode(_, a, xK, xV, _) => if (isEmpty(a)) Some((xK, xV)) else leftMost(a)
        case _ => None
    }

    ///
    /// Optionally returns the right most key-value pair of `tree`.
    ///
    pub def rightMost(tree: RBTree[k, v]): Option[(k, v)] = match tree {
        case RBNode(_, b, xK, xV, _) => if (isEmpty(b)) Some((xK, xV)) else rightMost(b)
        case _ => None
    }

    ///
    /// Returns `true` if and only if at least one key-value pair in `tree` satisfies the predicate `f`.
    ///
    /// Returns `false` if `tree` is the empty tree.
    ///
    /// The function `f` must be pure.
    ///
    pub def exists(f: (k, v) -> Bool, tree: RBTree[k, v]): Bool = match tree {
        case RBNode(_, a, xK, xV, b) => f(xK, xV) || exists(f, a) || exists(f, b)
        case _ => false
    }

    ///
    /// Returns `true` if and only if all key-value pairs in `tree` satisfy the predicate `f`.
    ///
    /// Returns `true` if `tree` is the empty tree.
    ///
    /// The function `f` must be pure.
    ///
    pub def forall(f: (k, v) -> Bool, tree: RBTree[k, v]): Bool = match tree {
        case RBNode(_, a, xK, xV, b) => if (!f(xK, xV)) false else forall(f, a) && forall(f, b)
        case _ => false
    }

    ///
    /// Applies `f` to every key-value pair of `tree`.
    ///
    /// The function `f` must be impure.
    ///
    pub def foreach(f: (k, v) ~> Unit, tree: RBTree[k, v]): Unit & Impure = match tree {
        case RBNode(_, a, xK, xV, b) => foreach(f, a); f(xK, xV); foreach(f, b)
        case _ => ()
    }

}