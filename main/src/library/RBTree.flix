namespace RBTree {

    ///
    /// An immutable red-black tree implementation with keys
    /// of type `k` and values of type `v`.
    ///
    /// A red-black tree is a balanced binary search tree.
    /// Each node is either red or black, although a transitory
    /// color double-black is allowed during deletion.
    /// The red-black tree satisfy the following invariants.
    ///
    /// 1. For all nodes with key `x`,
    /// the left subtree contains only nodes with keys `y` < `x` and
    /// the right subtree contains only nodes with keys `z` > `x`.
    /// 2. No red node has a red parent.
    /// 3. Every path from the root to a leaf contains the same
    /// number of black nodes.
    ///
    pub enum RBTree[k, v] {

        ///
        /// A black leaf.
        ///
        case RBLeaf

        ///
        /// A double-black leaf
        ///
        case RBBLeaf

        ///
        /// A tree node consists of a color, left subtree, key, value and right subtree.
        ///
        case RBNode(RBColor, RBTree[k, v], k, v, RBTree[k, v])

    }

    ///
    /// The color of a red-black tree node.
    ///
    pub enum RBColor {

        case Red
        case Black

        ///
        /// The special color double-black is allowed temporarily during deletion.
        ///
        case DoubleBlack

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
    /// Updates `tree` with `k -> v` if `k -> v1` is in `tree`.
    ///
    /// Otherwise, updates `tree` with `k -> v`.
    ///
    pub def insert(k: k, v: v, tree: RBTree[k, v]): RBTree[k, v] =
        insertWith((_, v1, _) -> v1, k, v, tree)

    ///
    /// Updates `tree` with `k -> f(k, v, v1)` if `k -> v1` is in `tree`.
    ///
    /// Otherwise, updates `tree` with `k -> v`.
    ///
    pub def insertWith(f: (k, v, v) -> v, k: k, v: v, tree: RBTree[k, v]): RBTree[k, v] =
        blacken(insertWithHelper(f, k, v, tree))

    ///
    /// Updates `tree` with `k -> v1` if `k -> v` is in `tree` and `f(k, v) = Some(v1)`.
    ///
    /// Otherwise, returns `tree`.
    ///
    pub def updateWith(f: (k, v) -> Option[v], k: k, tree: RBTree[k, v]): RBTree[k, v] =
        blacken(updateWithHelper(f, k, tree))

    ///
    /// Removes `k -> v` from `tree` if `tree` contains the key `k`.
    ///
    /// Otherwise, returns `tree`.
    ///
    pub def delete(k: k, tree: RBTree[k, v]): RBTree[k, v] = redden(deleteHelper(k, tree))

    ///
    /// Returns `true` if and only if `tree` is the empty tree.
    ///
    pub def isEmpty(tree: RBTree[k, v]): Bool = tree == empty()

    ///
    /// Returns `Some(v)` if `k -> v` is in `tree`.
    ///
    /// Otherwise returns `None`.
    ///
    pub def get(k: k, tree: RBTree[k, v]): Option[v] = match tree {
        case RBNode(_, a, k1, v, b) =>
            let cmp = k <=> k1;
            if (cmp < 0)
                get(k, a)
            else if (cmp == 0)
                Some(v)
            else
                get(k, b)
        case _ => None
    }

    ///
    /// Returns `true` if and only if `tree` contains the key `k`.
    ///
    pub def memberOf(k: k, tree: RBTree[k, v]): Bool = match tree {
        case RBNode(_, a, k1, _, b) =>
            let cmp = k <=> k1;
            if (cmp < 0)
                memberOf(k, a)
            else if (cmp == 0)
                true
            else
                memberOf(k, b)
        case _ => false
    }

    ///
    /// Optionally returns the first key-value pair in `tree` that satisfies the predicate `f` when searching from left to right.
    ///
    /// The function `f` must be pure.
    ///
    pub def findLeft(f: (k, v) -> Bool, tree: RBTree[k, v]): Option[(k, v)] = match tree {
        case RBNode(_, a, k, v, b) => match findLeft(f, a) {
            case None => if (f(k, v)) Some((k, v)) else findLeft(f, b)
            case Some((k1, v1)) => Some((k1, v1))
        }
        case _ => None
    }

    ///
    /// Optionally returns the first key-value pair in `tree` that satisfies the predicate `f` when searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    pub def findRight(f: (k, v) -> Bool, tree: RBTree[k, v]): Option[(k, v)] = match tree {
        case RBNode(_, a, k, v, b) => match findRight(f, b) {
            case None => if (f(k, v)) Some(k, v) else findRight(f, a)
            case Some((k1, v1)) => Some((k1, v1))
        }
        case _ => None
    }

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `tree` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(k2, f(k1, s, v1), v2)..., vn)`.
    ///
    pub def foldLeft(f: (b, k, v) -> b, s: b, tree: RBTree[k, v]): b = match tree {
        case RBNode(_, a, k, v, b) => foldLeft(f, f(foldLeft(f, s, a), k, v), b)
        case _ => s
    }

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `tree` going from right to left.
    ///
    /// That is, the result is of the form: `f(k1, v1, ...f(kn-1, vn-1, f(kn, vn, s)))`.
    ///
    pub def foldRight(f: (k, v, b) -> b, s: b, tree: RBTree[k, v]): b = match tree {
        case RBNode(_, a, k, v, b) => foldRight(f, f(k, v, foldRight(f, s, b)), a)
        case _ => s
    }

    ///
    /// Applies `f` to all key-value pairs in `tree` going from left to right until a single pair `(k, v)` is obtained.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(k1, v1, k2, v2), k3, v3)..., kn, vn))`
    ///
    /// Returns `None` if `tree` is the empty tree.
    ///
    pub def reduceLeft(f: (k, v, k, v) -> (k, v) & e, tree: RBTree[k, v]): Option[(k, v)] =
        reduceLeftHelper((x: Option[(k, v)], y: (k, v)) -> match x {
            case Some((k, v)) => Some(f(k, v, fst(y), snd(y)))
            case None => Some(y)
        }, None, tree)

    ///
    /// Applies `f` to all key-value pairs in `tree` going from right to left until a single pair `(k, v)` is obtained.
    ///
    /// That is, the result is of the form: `Some(f(k1, v1, ...f(kn-2, vn-2, f(kn-1, vn-1, kn, vn))...))`
    ///
    /// Returns `None` if `tree` is the empty tree.
    ///
    pub def reduceRight(f: (k, v, k, v) -> (k, v) & e, tree: RBTree[k, v]): Option[(k, v)] =
        reduceRightHelper((x: Option[(k, v)], y: (k, v)) -> match x {
            case Some((k, v)) => Some(f(fst(y), snd(y), k, v))
            case None => Some(y)
        }, None, tree)

    ///
    /// Returns `true` if and only if at least one key-value pair in `tree` satisfies the predicate `f`.
    ///
    /// Returns `false` if `tree` is the empty tree.
    ///
    /// The function `f` must be pure.
    ///
    pub def exists(f: (k, v) -> Bool, tree: RBTree[k, v]): Bool = match tree {
        case RBNode(_, a, k, v, b) => f(k, v) || exists(f, a) || exists(f, b)
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
        case RBNode(_, a, k, v, b) => f(k, v) && forall(f, a) && forall(f, b)
        case _ => true
    }

    ///
    /// Applies `f` to every key-value pair of `tree`.
    ///
    /// The function `f` must be impure.
    ///
    pub def foreach(f: (k, v) ~> Unit, tree: RBTree[k, v]): Unit & Impure = match tree {
        case RBNode(_, a, k, v, b) => foreach(f, a); f(k, v); foreach(f, b)
        case _ => ()
    }

    ///
    /// Helper function for `insert` and `delete`.
    ///
    def balance(tree: RBTree[k, v]): RBTree[k, v] = match tree {
        case RBNode(Black, RBNode(Red, RBNode(Red, a, k1, v1, b), k2, v2, c), k3, v3, d) =>
            RBNode(Red, RBNode(Black, a, k1, v1, b), k2, v2, RBNode(Black, c, k3, v3, d))
        case RBNode(Black, RBNode(Red, a, k1, v1, RBNode(Red, b, k2, v2, c)), k3, v3, d) =>
            RBNode(Red, RBNode(Black, a, k1, v1, b), k2, v2, RBNode(Black, c, k3, v3, d))
        case RBNode(Black, a, k1, v1, RBNode(Red, RBNode(Red, b, k2, v2, c), k3, v3, d)) =>
            RBNode(Red, RBNode(Black, a, k1, v1, b), k2, v2, RBNode(Black, c, k3, v3, d))
        case RBNode(Black, a, k1, v1, RBNode(Red, b, k2, v2, RBNode(Red, c, k3, v3, d))) =>
            RBNode(Red, RBNode(Black, a, k1, v1, b), k2, v2, RBNode(Black, c, k3, v3, d))
        case RBNode(DoubleBlack, a, k1, v1, RBNode(Red, RBNode(Red, b, k2, v2, c), k3, v3, d)) =>
            RBNode(Black, RBNode(Black, a, k1, v1, b), k2, v2, RBNode(Black, c, k3, v3, d))
        case RBNode(DoubleBlack, RBNode(Red, a, k1, v1, RBNode(Red, b, k2, v2, c)), k3, v3, d) =>
            RBNode(Black, RBNode(Black, a, k1, v1, b), k2, v2, RBNode(Black, c, k3, v3, d))
        case _ => tree
    }

    ///
    /// Helper function for `insert`.
    ///
    def blacken(tree: RBTree[k, v]): RBTree[k, v] = match tree {
        case RBNode(Red, RBNode(Red, a, k1, v1, b), k2, v2, c) =>
            RBNode(Black, RBNode(Red, a, k1, v1, b), k2, v2, c)
        case RBNode(Red, a, k1, v1, RBNode(Red, b, k2, v2, c)) =>
            RBNode(Black, a, k1, v1, RBNode(Red, b, k2, v2, c))
        case _ => tree
    }

    ///
    /// Helper function for `insertWith`.
    ///
    def insertWithHelper(f: (k, v, v) -> v, k: k, v: v, tree: RBTree[k, v]): RBTree[k, v] = match tree {
        case RBLeaf => RBNode(Red, RBLeaf, k, v, RBLeaf)
        case RBNode(color, a, k1, v1, b) =>
            let cmp = k <=> k1;
            if (cmp < 0)
                balance(RBNode(color, insertWithHelper(f, k, v, a), k1, v1, b))
            else if (cmp == 0)
                RBNode(color, a, k, f(k, v, v1), b)
            else
                balance(RBNode(color, a, k1, v1, insertWithHelper(f, k, v, b)))
        case _ => tree
    }

    ///
    /// Helper function for `updateWith`.
    ///
    def updateWithHelper(f: (k, v) -> Option[v], k: k, tree: RBTree[k, v]): RBTree[k, v] = match tree {
        case RBLeaf => tree
        case RBNode(color, a, k1, v1, b) =>
            let cmp = k <=> k1;
            if (cmp < 0)
                balance(RBNode(color, updateWithHelper(f, k, a), k1, v1, b))
            else if (cmp == 0)
                match f(k1, v1) {
                    case None => tree
                    case Some(v) => RBNode(color, a, k, v, b)
                }
            else
                balance(RBNode(color, a, k1, v1, updateWithHelper(f, k, b)))
        case _ => tree
    }

    ///
    /// Helper function for `delete`.
    ///
    def rotate(tree: RBTree[k, v]): RBTree[k, v] = match tree {
        case RBNode(Red, RBNode(DoubleBlack, a, k1, v1, b), k2, v2, RBNode(Black, c, k3, v3, d)) =>
            balance(RBNode(Black, RBNode(Red, RBNode(Black, a, k1, v1, b), k2, v2, c), k3, v3, d))
        case RBNode(Red, RBBLeaf, k2, v2, RBNode(Black, c, k3, v3, d)) =>
            balance(RBNode(Black, RBNode(Red, RBLeaf, k2, v2, c), k3, v3, d))
        case RBNode(Red, RBNode(Black, a, k1, v1, b), k2, v2, RBNode(DoubleBlack, c, k3, v3, d)) =>
            balance(RBNode(Black, a, k1, v1, RBNode(Red, b, k2, v2, RBNode(Black, c, k3, v3, d))))
        case RBNode(Red, RBNode(Black, a, k1, v1, b), k2, v2, RBBLeaf) =>
            balance(RBNode(Black, a, k1, v1, RBNode(Red, b, k2, v2, RBLeaf)))
        case RBNode(Black, RBNode(DoubleBlack, a, k1, v1, b), k2, v2, RBNode(Black, c, k3, v3, d)) =>
            balance(RBNode(DoubleBlack, RBNode(Red, RBNode(Black, a, k1, v1, b), k2, v2, c), k3, v3, d))
        case RBNode(Black, RBBLeaf, k2, v2, RBNode(Black, c, k3, v3, d)) =>
            balance(RBNode(DoubleBlack, RBNode(Red, RBLeaf, k2, v2, c), k3, v3, d))
        case RBNode(Black, RBNode(Black, a, k1, v1, b), k2, v2, RBNode(DoubleBlack, c, k3, v3, d)) =>
            balance(RBNode(DoubleBlack, a, k1, v1, RBNode(Red, b, k2, v2, RBNode(Black, c, k3, v3, d))))
        case RBNode(Black, RBNode(Black, a, k1, v1, b), k2, v2, RBBLeaf) =>
            balance(RBNode(DoubleBlack, a, k1, v1, RBNode(Red, b, k2, v2, RBLeaf)))
        case RBNode(Black, RBNode(DoubleBlack, a, k4, v4, b), k1, v1, RBNode(Red, RBNode(Black, c, k2, v2, d), k3, v3, e)) =>
            RBNode(Black, balance(RBNode(Black, RBNode(Red, RBNode(Black, a, k4, v4, b), k1, v1, c), k2, v2, d)), k3, v3, e)
        case RBNode(Black, RBBLeaf, k1, v1, RBNode(Red, RBNode(Black, c, k2, v2, d), k3, v3, e)) =>
            RBNode(Black, balance(RBNode(Black, RBNode(Red, RBLeaf, k1, v1, c), k2, v2, d)), k3, v3, e)
        case RBNode(Black, RBNode(Red, a, k4, v4, RBNode(Black, b, k1, v1, c)), k2, v2, RBNode(DoubleBlack, d, k3, v3, e)) =>
            RBNode(Black, a, k4, v4, balance(RBNode(Black, b, k1, v1, RBNode(Red, c, k2, v2, RBNode(Black, d, k3, v3, e)))))
        case RBNode(Black, RBNode(Red, a, k4, v4, RBNode(Black, b, k1, v1, c)), k2, v2, RBBLeaf) =>
            RBNode(Black, a, k4, v4, balance(RBNode(Black, b, k1, v1, RBNode(Red, c, k2, v2, RBLeaf))))
        case _ => tree
    }

    ///
    /// Helper function for `delete`.
    ///
    def redden(tree: RBTree[k, v]): RBTree[k, v] = match tree {
        case RBNode(Black, RBNode(Black, a, k1, v1, b), k2, v2, RBNode(Black, c, k3, v3, d)) =>
            RBNode(Red, RBNode(Black, a, k1, v1, b), k2, v2, RBNode(Black, c, k3, v3, d))
        case _ => tree
    }

    ///
    /// Helper function for `delete`.
    ///
    def minDelete(tree: RBTree[k, v]): (k, v, RBTree[k, v]) = match tree {
        case RBNode(Red, RBLeaf, k1, v1, RBLeaf) => (k1, v1, RBLeaf)
        case RBNode(Black, RBLeaf, k1, v1, RBLeaf) => (k1, v1, RBBLeaf)
        case RBNode(Black, RBLeaf, k1, v1, RBNode(Red, RBLeaf, k2, v2, RBLeaf)) =>
            (k1, v1, RBNode(Black, RBLeaf, k2, v2, RBLeaf))
        case RBNode(c, a, k1, v1, b) =>
            let (k3, v3, e) = minDelete(a);
            (k3, v3, rotate(RBNode(c, e, k1, v1, b)))
        case _ => panic "impossible"
    }

    ///
    /// Helper function for `delete`.
    ///
    def deleteHelper(k: k, tree: RBTree[k, v]): RBTree[k, v] = match tree {
        case RBNode(Red, RBLeaf, k1, _, RBLeaf) => if (k <=> k1 == 0) RBLeaf else tree
        case RBNode(Black, RBLeaf, k1, _, RBLeaf) => if (k <=> k1 == 0) RBBLeaf else tree
        case RBNode(Black, RBNode(Red, RBLeaf, k1, v1, RBLeaf), k2, v2, RBLeaf) =>
            let cmp = k <=> k2;
            if (cmp < 0)
                RBNode(Black, deleteHelper(k, RBNode(Red, RBLeaf, k1, v1, RBLeaf)), k2, v2, RBLeaf)
            else if
                (cmp == 0) RBNode(Black, RBLeaf, k1, v1, RBLeaf)
            else
                RBNode(Black, RBNode(Red, RBLeaf, k1, v1, RBLeaf), k2, v2, RBLeaf)
        case RBNode(c, a, k1, v1, b) =>
            let cmp = k <=> k1;
            if (cmp < 0)
                rotate(RBNode(c, deleteHelper(k, a), k1, v1, b))
            else if (cmp == 0)
                let (k2, v2, e) = minDelete(b);
                rotate(RBNode(c, a, k2, v2, e))
            else
                rotate(RBNode(c, a, k1, v1, deleteHelper(k, b)))
        case _ => tree
    }

    ///
    /// Helper function for `reduceLeft`.
    ///
    def reduceLeftHelper(f: (Option[(k, v)], (k, v)) -> Option[(k, v)], s: Option[(k, v)], tree: RBTree[k, v]): Option[(k, v)] =
        match tree {
            case RBNode(_, a, k, v, b) => reduceLeftHelper(f, f(reduceLeftHelper(f, s, a), (k, v)), b)
            case _ => s
        }

    ///
    /// Helper function for `reduceRight`.
    ///
    def reduceRightHelper(f: (Option[(k, v)], (k, v)) -> Option[(k, v)], s: Option[(k, v)], tree: RBTree[k, v]): Option[(k, v)] =
        match tree {
            case RBNode(_, a, k, v, b) => reduceRightHelper(f, f(reduceRightHelper(f, s, b), (k, v)), a)
            case _ => s
        }

}