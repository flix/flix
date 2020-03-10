namespace RBTree {

    ///
    /// The red-black tree used by `Set` and `Map`.
    ///
    /// A red-black tree is a balanced binary search tree.
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
        case RBNode(color, a, y, b) =>
            let cmp = x <=> y;
            if (cmp < 0) balance(RBNode(color, insertHelper(x, a), y, b))
            else if (cmp == 0) RBNode(color, a, y, b)
            else  balance(RBNode(color, a, y, insertHelper(x, b)))
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
        case RBNode(Red, RBLeaf, y, RBLeaf) => if (x <=> y == 0) RBLeaf else tree
        case RBNode(Black, RBLeaf, y, RBLeaf) => if (x <=> y == 0) RBBLeaf else tree
        case RBNode(Black, RBNode(Red, RBLeaf, y, RBLeaf), z, RBLeaf) =>
            let cmp = x <=> z;
            if (cmp < 0) RBNode(Black, deleteHelper(x, RBNode(Red, RBLeaf, y, RBLeaf)), z, RBLeaf)
            else if (cmp == 0) RBNode(Black, RBLeaf, y, RBLeaf)
            else RBNode(Black, RBNode(Red, RBLeaf, y, RBLeaf), z, RBLeaf)
        case RBNode(c, a, y, b) =>
            let cmp = x <=> y;
            if (cmp < 0) rotate(RBNode(c, deleteHelper(x, a), y, b))
            else if (cmp == 0) let (z, e) = minDelete(b);
                               rotate(RBNode(c, a, z, e))
            else rotate(RBNode(c, a, y, deleteHelper(x, b)))
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
        case RBNode(_, a, y, _) if x <=> y < 0 => contains(x, a)
        case RBNode(_, _, y, _) if x <=> y == 0 => true
        case RBNode(_, _, y, b) if x <=> y > 0 => contains(x, b)
        case _ => false
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

    ///
    /// Returns the empty tree.
    ///
    pub def empty(): RBTree[a] = RBLeaf

    ///
    /// Returns true if and only if `tree` is the empty tree.
    ///
    pub def isEmpty(tree: RBTree[a]): Bool = match tree {
        case RBNode(_, _, _, _) => false
        case _ => true
    }

    ///
    /// Optionally returns the first element of `tree` that satisfies the predicate `f` when searching from left to right.
    ///
    /// The function `f` must be pure.
    ///
    pub def findLeft(f: a -> Bool, tree: RBTree[a]): Option[a] = match tree {
        case RBNode(_, a, x, b) => match findLeft(f, a) {
            case Some(y) => Some(y)
            case _ => if (f(x)) Some(x) else findLeft(f, b)
        }
        case _ => None
    }

    ///
    /// Optionally returns the first element of `tree` that satisfies the predicate `f` when searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    pub def findRight(f: a -> Bool, tree: RBTree[a]): Option[a] = match tree {
        case RBNode(_, a, x, b) => match findRight(f, b) {
            case Some(y) => Some(y)
            case _ => if (f(x)) Some(x) else findRight(f, a)
        }
        case _ => None
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
    /// Helper function for `reduceLeft` and `reduceRight`.
    ///
    pub def reduceHelper(f: (a, a) -> a, this: a, that: Option[a]): a = match that {
        case Some(c) => f(c, this)
        case _ => this
    }

    ///
    /// Applies `f` to all elements in `tree` going from left to right, , i.e. inorder until a single value `v` is obtained. Returns `Some(v)`.
    /// Returns `None` if `tree` is the empty tree.
    ///
    pub def reduceLeft(f: (a, a) -> a, tree: RBTree[a]): Option[a] = match tree {
        case RBNode(_, a, x, b) => Some(reduceHelper(f, reduceHelper(f, x, reduceLeft(f, a)), reduceLeft(f, b)))
        case _ => None
    }

    ///
    /// Applies `f` to all elements in `tree` going from right to left, , i.e. reverse inorder until a single value `v` is obtained. Returns `Some(v)`.
    /// Returns `None` if `tree` is the empty tree.
    ///
    pub def reduceRight(f: (a, a) -> a, tree: RBTree[a]): Option[a] = match tree {
        case RBNode(_, a, x, b) => Some(reduceHelper(f, reduceHelper(f, x, reduceRight(f, b)), reduceRight(f, a)))
        case _ => None
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
    /// Returns `true` if and only if at least one element in `tree` satisfies the predicate `f`.
    ///
    /// Returns `false` if `xs` is the empty tree.
    ///
    /// The function `f` must be pure.
    ///
    pub def exists(f: a -> Bool, tree: RBTree[a]): Bool = match tree {
        case RBNode(_, a, x, b) => f(x) || exists(f, a) || exists(f, b)
        case _ => false
    }

    ///
    /// Returns `true` if and only if all elements in `tree` satisfy the predicate `f`.
    ///
    /// Returns `true` if `xs` is the empty tree.
    ///
    /// The function `f` must be pure.
    ///
    pub def forall(f: a -> Bool, tree: RBTree[a]): Bool = match tree {
        case RBNode(_, a, x, b) => if (!f(x)) false else forall(f, a) && forall(f, b)
        case _ => false
    }

    ///
    /// Applies `f` to every element of `tree`.
    ///
    /// The function `f` must be impure.
    ///
    pub def foreach(f: a ~> Unit, tree: RBTree[a]): Unit & Impure = match tree {
        case RBNode(_, a, x, b) => foreach(f, a); f(x); foreach(f, b)
        case _ => ()
    }

    ///
    /// Helper function for `cmp`.
    ///
    def cmpHelper(this: RBTree[a], that: RBTree[a], a: Option[a], b: Option[a]): Int32 = match a {
        case Some(x) => match b {
            case Some(y) =>
                let cmp = x <=> y;
                if (cmp < 0) -1
                else if (cmp == 0) cmpHelper(this, that, findLeft(m -> m <=> x > 0, this), findLeft(n -> n <=> y > 0, that))
                else 1
            case None => 1
        }
        case None => -1
    }

    ///
    /// Returns the result of a three-way comparison between `this` and `that`.
    ///
    /// Returns n < 0 if `this` is lexicographically smaller than `that`.
    /// Returns n = 0 if `this` is equal to `that`.
    /// Returns n > 0 if `this` is lexicographically greater than `that`.
    ///
    pub def __cmp(this: RBTree[a], that: RBTree[a]): Int32 =
        if (isEmpty(this) && isEmpty(that)) 0 else cmpHelper(this, that, leftMost(this), leftMost(that))

    ///
    /// Returns `true` if and only if `this` and `that` are equal, i.e. they have the same elements.
    ///
    pub def __eq(this: RBTree[a], that: RBTree[a]): Bool = __cmp(this, that) == 0

    ///
    /// Helper function for `range`.
    ///
    def rangeHelper(b: Int32, n: Int32, acc: RBTree[Int32]): RBTree[Int32] =
        if (n < b) acc else rangeHelper(b, n - 1, insert(n, acc))

    ///
    /// Returns a tree of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns `empty()` if `b >= e`.
    ///
    pub def range(b: Int32, e: Int32): RBTree[Int32] =
        if (b >= e) empty() else rangeHelper(b, e - 1, empty())

    ///
    /// Returns `true` if and only if the binary search tree invariant holds.
    ///
    pub def binarySearchTreeInvariantHolds(tree: RBTree[a]): Bool = match tree {
        case RBNode(_, a, x, b) =>
        if (exists(y -> y <=> x > 0, a) || exists(z -> z <=> x < 0, b)) false
        else binarySearchTreeInvariantHolds(a) && binarySearchTreeInvariantHolds(b)
        case _ => true
    }

    ///
    /// Helper function for `redParentInvariantHolds`.
    ///
    def redParentInvariantHoldsHelper(tree: RBTree[a], parentColor: RBColor): Bool = match tree {
        case RBNode(Red, a, _, b) =>
            if (parentColor == Red) false else redParentInvariantHoldsHelper(a, Red) && redParentInvariantHoldsHelper(b, Red)
        case RBNode(Black, a, _, b) =>
            redParentInvariantHoldsHelper(a, Black) && redParentInvariantHoldsHelper(b, Black)
        case _ => true
    }

    ///
    /// Returns `true` if and only if no red node has a red parent.
    ///
    pub def redParentInvariantHolds(tree: RBTree[a]): Bool = redParentInvariantHoldsHelper(tree, Black)

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