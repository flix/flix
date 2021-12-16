/*
 * Copyright 2020 Esben Bjerre
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

namespace RedBlackTree {

    ///
    /// An immutable red-black tree implementation with keys
    /// of type `k` and values of type `v`.
    ///
    /// A red-black tree is a self-balancing binary search tree.
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
    pub enum RedBlackTree[k, v] {

        ///
        /// A black leaf.
        ///
        case Leaf

        ///
        /// A double-black leaf.
        ///
        case DoubleBlackLeaf

        ///
        /// A tree node consists of a color, left subtree, key, value and right subtree.
        ///
        case Node(Color, RedBlackTree[k, v], k, v, RedBlackTree[k, v])

    }

    instance Eq[RedBlackTree[k, v]] with Eq[k], Eq[v] {
        pub def eq(t1: RedBlackTree[k, v], t2: RedBlackTree[k, v]): Bool with Eq[v] =
            eqHelper(t1, t2, b -> b)
    }

    instance Functor[RedBlackTree[k]] {
        pub def map(f: a -> b & ef, t: RedBlackTree[k, a]): RedBlackTree[k, b] & ef = mapWithKey((_, v) -> f(v), t)
    }

    instance Traversable[RedBlackTree[k]] {
        pub def traverse(f: a -> m[b] & ef, t: RedBlackTree[k, a]): m[RedBlackTree[k, b]] & ef with Applicative[m] = traverse(f, t)
        pub override def sequence(t: RedBlackTree[k, m[a]]): m[RedBlackTree[k, a]] with Applicative[m] = sequence(t)
    }

    ///
    /// The color of a red-black tree node.
    ///
    pub enum Color with Eq, Order {

        case Red
        case Black

        ///
        /// The special color double-black is allowed temporarily during deletion.
        ///
        case DoubleBlack

    }

    ///
    /// Returns the number of threads to use for parallel evaluation.
    ///
    def threads(): Int32 =
        // Note: We use a multiple of the number of physical cores for better performance.
        let multiplier = 4;
        multiplier * Environment.getVirtualProcessors()

    ///
    /// Determines whether to use parallel evaluation.
    ///
    /// By default we only enable parallel evaluation if the tree has a certain size.
    ///
    def useParallelEvaluation(t: RedBlackTree[k, v]): Bool =
        let minSize = 2 ** RedBlackTree.blackHeight(t);
        minSize >= 1024

    ///
    /// Returns the number of nodes in `tree`.
    ///
    @Time(size(tree)) @Space(Int32.log2(size(tree)))
    pub def size(tree: RedBlackTree[k, v]): Int32 = match tree {
        case Node(_, a, _, _, b) => 1 + size(a) + size(b)
        case _ => 0
    }

    ///
    /// Returns the empty tree.
    ///
    @Time(1) @Space(1)
    pub def empty(): RedBlackTree[k, v] = Leaf

    ///
    /// Returns `true` if and only if `tree` is the empty tree.
    ///
    @Time(1) @Space(1)
    pub def isEmpty(tree: RedBlackTree[k, v]): Bool = match tree {
        case Leaf => true
        case _ => false
    }

    ///
    /// Updates `tree` with `k -> v` if `k -> v1` is in `tree`.
    ///
    /// Otherwise, updates `tree` with `k -> v`.
    ///
    @Time(Int32.log2(size(tree))) @Space(Int32.log2(size(tree)))
    pub def insert(k: k, v: v, tree: RedBlackTree[k, v]): RedBlackTree[k, v] with Order[k] =
        blacken(insertHelper(k, v, tree))

    ///
    /// Updates `tree` with `k -> f(k, v, v1)` if `k -> v1` is in `tree`.
    ///
    /// Otherwise, updates `tree` with `k -> v`.
    ///
    @Time(time(f) + Int32.log2(size(tree))) @Space(space(f) + Int32.log2(size(tree)))
    pub def insertWith(f: (k, v, v) -> v & ef, k: k, v: v, tree: RedBlackTree[k, v]): RedBlackTree[k, v] & ef with Order[k] =
        blacken(insertWithHelper(f, k, v, tree))

    ///
    /// Updates `tree` with `k -> v1` if `k -> v` is in `tree` and `f(k, v) = Some(v1)`.
    ///
    /// Otherwise, returns `tree`.
    ///
    @Time(time(f) + Int32.log2(size(tree))) @Space(space(f) + Int32.log2(size(tree)))
    pub def updateWith(f: (k, v) -> Option[v] & ef, k: k, tree: RedBlackTree[k, v]): RedBlackTree[k, v] & ef with Order[k] =
        blacken(updateWithHelper(f, k, tree))

    ///
    /// Removes `k -> v` from `tree` if `tree` contains the key `k`.
    ///
    /// Otherwise, returns `tree`.
    ///
    pub def remove(k: k, tree: RedBlackTree[k, v]): RedBlackTree[k, v] with Order[k] = redden(removeHelper(k, tree))

    ///
    /// Returns `Some(v)` if `k -> v` is in `tree`.
    ///
    /// Otherwise returns `None`.
    ///
    @Time(Int32.log2(size(tree))) @Space(1)
    pub def get(k: k, tree: RedBlackTree[k, v]): Option[v] with Order[k] = match tree {
        case Node(_, a, k1, v, b) =>
            match k <=> k1 {
                case LessThan => get(k, a)
                case EqualTo => Some(v)
                case GreaterThan => get(k, b)
            }
        case _ => None
    }

    ///
    /// Returns `true` if and only if `tree` contains the key `k`.
    ///
    @Time(Int32.log2(size(tree))) @Space(1)
    pub def memberOf(k: k, tree: RedBlackTree[k, v]): Bool with Order[k] = match tree {
        case Node(_, a, k1, _, b) =>
            match k <=> k1 {
                case LessThan => memberOf(k, a)
                case EqualTo => true
                case GreaterThan => memberOf(k, b)
            }
        case _ => false
    }

    ///
    /// Optionally returns the first key-value pair in `tree` that satisfies the predicate `f` when searching from left to right.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * size(tree)) @Space(space(f) * Int32.log2(size(tree)))
    pub def findLeft(f: (k, v) -> Bool, tree: RedBlackTree[k, v]): Option[(k, v)] = match tree {
        case Node(_, a, k, v, b) => match findLeft(f, a) {
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
    @Time(time(f) * size(tree)) @Space(space(f) * Int32.log2(size(tree)))
    pub def findRight(f: (k, v) -> Bool, tree: RedBlackTree[k, v]): Option[(k, v)] = match tree {
        case Node(_, a, k, v, b) => match findRight(f, b) {
            case None => if (f(k, v)) Some(k, v) else findRight(f, a)
            case Some((k1, v1)) => Some((k1, v1))
        }
        case _ => None
    }

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `tree` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, k1, v1), k2, v2)..., vn)`.
    ///
    pub def foldLeft(f: (b, k, v) -> b & ef, s: b, tree: RedBlackTree[k, v]): b & ef = match tree {
        case Node(_, a, k, v, b) => foldLeft(f, f(foldLeft(f, s, a), k, v), b)
        case _ => s
    }

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `tree` going from right to left.
    ///
    /// That is, the result is of the form: `f(k1, v1, ...f(kn-1, vn-1, f(kn, vn, s)))`.
    ///
    pub def foldRight(f: (k, v, b) -> b & ef, s: b, tree: RedBlackTree[k, v]): b & ef = match tree {
        case Node(_, a, k, v, b) => foldRight(f, f(k, v, foldRight(f, s, b)), a)
        case _ => s
    }

    /// Applies `f` to all key-value pairs in `tree` going from left to right until a single pair `(k, v)` is obtained.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(k1, v1, k2, v2), k3, v3)..., kn, vn))`
    ///
    /// Returns `None` if `tree` is the empty tree.
    ///
    pub def reduceLeft(f: (k, v, k, v) -> (k, v) & ef, tree: RedBlackTree[k, v]): Option[(k, v)] & ef =
        foldLeft((x, k1, v1) -> match x {
            case Some((k2, v2)) => Some(f(k2, v2, k1, v1))
            case None => Some(k1, v1)
        }, None, tree)

    ///
    /// Applies `f` to all key-value pairs in `tree` going from right to left until a single pair `(k, v)` is obtained.
    ///
    /// That is, the result is of the form: `Some(f(k1, v1, ...f(kn-2, vn-2, f(kn-1, vn-1, kn, vn))...))`.
    ///
    /// Returns `None` if `tree` is the empty tree.
    ///
    pub def reduceRight(f: (k, v, k, v) -> (k, v) & ef, tree: RedBlackTree[k, v]): Option[(k, v)] & ef =
        foldRight((k1, v1, x) -> match x {
            case Some((k2, v2)) => Some(f(k1, v1, k2, v2))
            case None => Some(k1, v1)
        }, None, tree)

    ///
    /// Returns `true` if and only if at least one key-value pair in `tree` satisfies the predicate `f`.
    ///
    /// Returns `false` if `tree` is the empty tree.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * size(tree)) @Space(space(f) * Int32.log2(size(tree)))
    pub def exists(f: (k, v) -> Bool, tree: RedBlackTree[k, v]): Bool = match tree {
        case Node(_, a, k, v, b) => f(k, v) or exists(f, a) or exists(f, b)
        case _ => false
    }

    ///
    /// Returns `true` if and only if all key-value pairs in `tree` satisfy the predicate `f`.
    ///
    /// Returns `true` if `tree` is the empty tree.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * size(tree)) @Space(space(f) * Int32.log2(size(tree)))
    pub def forall(f: (k, v) -> Bool, tree: RedBlackTree[k, v]): Bool = match tree {
        case Node(_, a, k, v, b) => f(k, v) and forall(f, a) and forall(f, b)
        case _ => true
    }

    ///
    /// Applies `f` to every key-value pair of `tree`.
    ///
    @Time(time(f) * size(tree)) @Space(space(f) * Int32.log2(size(tree)))
    pub def foreach(f: (k, v) -> Unit & ef, tree: RedBlackTree[k, v]): Unit & ef = match tree {
        case Node(_, a, k, v, b) => foreach(f, a); f(k, v); foreach(f, b)
        case _ => ()
    }

    ///
    /// Helper function for `insert` and `delete`.
    ///
    def balance(tree: RedBlackTree[k, v]): RedBlackTree[k, v] = match tree {
        case Node(Black, Node(Red, Node(Red, a, k1, v1, b), k2, v2, c), k3, v3, d) =>
            Node(Red, Node(Black, a, k1, v1, b), k2, v2, Node(Black, c, k3, v3, d))
        case Node(Black, Node(Red, a, k1, v1, Node(Red, b, k2, v2, c)), k3, v3, d) =>
            Node(Red, Node(Black, a, k1, v1, b), k2, v2, Node(Black, c, k3, v3, d))
        case Node(Black, a, k1, v1, Node(Red, Node(Red, b, k2, v2, c), k3, v3, d)) =>
            Node(Red, Node(Black, a, k1, v1, b), k2, v2, Node(Black, c, k3, v3, d))
        case Node(Black, a, k1, v1, Node(Red, b, k2, v2, Node(Red, c, k3, v3, d))) =>
            Node(Red, Node(Black, a, k1, v1, b), k2, v2, Node(Black, c, k3, v3, d))
        case Node(DoubleBlack, a, k1, v1, Node(Red, Node(Red, b, k2, v2, c), k3, v3, d)) =>
            Node(Black, Node(Black, a, k1, v1, b), k2, v2, Node(Black, c, k3, v3, d))
        case Node(DoubleBlack, Node(Red, a, k1, v1, Node(Red, b, k2, v2, c)), k3, v3, d) =>
            Node(Black, Node(Black, a, k1, v1, b), k2, v2, Node(Black, c, k3, v3, d))
        case _ => tree
    }

    ///
    /// Helper function for `insert`.
    ///
    def blacken(tree: RedBlackTree[k, v]): RedBlackTree[k, v] = match tree {
        case Node(Red, Node(Red, a, k1, v1, b), k2, v2, c) =>
            Node(Black, Node(Red, a, k1, v1, b), k2, v2, c)
        case Node(Red, a, k1, v1, Node(Red, b, k2, v2, c)) =>
            Node(Black, a, k1, v1, Node(Red, b, k2, v2, c))
        case _ => tree
    }

    ///
    /// Helper function for `insert`.
    ///
    def insertHelper(k: k, v: v, tree: RedBlackTree[k, v]): RedBlackTree[k, v] with Order[k] = match tree {
        case Leaf => Node(Red, Leaf, k, v, Leaf)
        case Node(color, a, k1, v1, b) =>
            match k <=> k1 {
                case LessThan => balance(Node(color, insertHelper(k, v, a), k1, v1, b))
                case EqualTo => Node(color, a, k, v, b)
                case GreaterThan => balance(Node(color, a, k1, v1, insertHelper(k, v, b)))
            }
        case _ => tree
    }

    ///
    /// Helper function for `insertWith`.
    ///
    def insertWithHelper(f: (k, v, v) -> v & ef, k: k, v: v, tree: RedBlackTree[k, v]): RedBlackTree[k, v] & ef with Order[k] = match tree {
        case Leaf => Node(Red, Leaf, k, v, Leaf)
        case Node(color, a, k1, v1, b) =>
            match k <=> k1 {
                case LessThan => balance(Node(color, insertWithHelper(f, k, v, a), k1, v1, b))
                case EqualTo => Node(color, a, k, f(k, v, v1), b)
                case GreaterThan => balance(Node(color, a, k1, v1, insertWithHelper(f, k, v, b)))
            }
        case _ => tree
    }

    ///
    /// Helper function for `updateWith`.
    ///
    def updateWithHelper(f: (k, v) -> Option[v] & ef, k: k, tree: RedBlackTree[k, v]): RedBlackTree[k, v] & ef with Order[k] = match tree {
        case Leaf => tree
        case Node(color, a, k1, v1, b) =>
            match k <=> k1 {
                case LessThan => balance(Node(color, updateWithHelper(f, k, a), k1, v1, b))
                case EqualTo => match f(k1, v1) {
                    case None => tree
                    case Some(v) => Node(color, a, k, v, b)
                }
                case GreaterThan => balance(Node(color, a, k1, v1, updateWithHelper(f, k, b)))
            }
        case _ => tree
    }

    ///
    /// Helper function for `delete`.
    ///
    def rotate(tree: RedBlackTree[k, v]): RedBlackTree[k, v] = match tree {
        case Node(Red, Node(DoubleBlack, a, k1, v1, b), k2, v2, Node(Black, c, k3, v3, d)) =>
            balance(Node(Black, Node(Red, Node(Black, a, k1, v1, b), k2, v2, c), k3, v3, d))
        case Node(Red, DoubleBlackLeaf, k2, v2, Node(Black, c, k3, v3, d)) =>
            balance(Node(Black, Node(Red, Leaf, k2, v2, c), k3, v3, d))
        case Node(Red, Node(Black, a, k1, v1, b), k2, v2, Node(DoubleBlack, c, k3, v3, d)) =>
            balance(Node(Black, a, k1, v1, Node(Red, b, k2, v2, Node(Black, c, k3, v3, d))))
        case Node(Red, Node(Black, a, k1, v1, b), k2, v2, DoubleBlackLeaf) =>
            balance(Node(Black, a, k1, v1, Node(Red, b, k2, v2, Leaf)))
        case Node(Black, Node(DoubleBlack, a, k1, v1, b), k2, v2, Node(Black, c, k3, v3, d)) =>
            balance(Node(DoubleBlack, Node(Red, Node(Black, a, k1, v1, b), k2, v2, c), k3, v3, d))
        case Node(Black, DoubleBlackLeaf, k2, v2, Node(Black, c, k3, v3, d)) =>
            balance(Node(DoubleBlack, Node(Red, Leaf, k2, v2, c), k3, v3, d))
        case Node(Black, Node(Black, a, k1, v1, b), k2, v2, Node(DoubleBlack, c, k3, v3, d)) =>
            balance(Node(DoubleBlack, a, k1, v1, Node(Red, b, k2, v2, Node(Black, c, k3, v3, d))))
        case Node(Black, Node(Black, a, k1, v1, b), k2, v2, DoubleBlackLeaf) =>
            balance(Node(DoubleBlack, a, k1, v1, Node(Red, b, k2, v2, Leaf)))
        case Node(Black, Node(DoubleBlack, a, k4, v4, b), k1, v1, Node(Red, Node(Black, c, k2, v2, d), k3, v3, e)) =>
            Node(Black, balance(Node(Black, Node(Red, Node(Black, a, k4, v4, b), k1, v1, c), k2, v2, d)), k3, v3, e)
        case Node(Black, DoubleBlackLeaf, k1, v1, Node(Red, Node(Black, c, k2, v2, d), k3, v3, e)) =>
            Node(Black, balance(Node(Black, Node(Red, Leaf, k1, v1, c), k2, v2, d)), k3, v3, e)
        case Node(Black, Node(Red, a, k4, v4, Node(Black, b, k1, v1, c)), k2, v2, Node(DoubleBlack, d, k3, v3, e)) =>
            Node(Black, a, k4, v4, balance(Node(Black, b, k1, v1, Node(Red, c, k2, v2, Node(Black, d, k3, v3, e)))))
        case Node(Black, Node(Red, a, k4, v4, Node(Black, b, k1, v1, c)), k2, v2, DoubleBlackLeaf) =>
            Node(Black, a, k4, v4, balance(Node(Black, b, k1, v1, Node(Red, c, k2, v2, Leaf))))
        case _ => tree
    }

    ///
    /// Helper function for `delete`.
    ///
    def redden(tree: RedBlackTree[k, v]): RedBlackTree[k, v] = match tree {
        case Node(Black, Node(Black, a, k1, v1, b), k2, v2, Node(Black, c, k3, v3, d)) =>
            Node(Red, Node(Black, a, k1, v1, b), k2, v2, Node(Black, c, k3, v3, d))
        case DoubleBlackLeaf => Leaf
        case _ => tree
    }

    ///
    /// Helper function for `delete`.
    ///
    def minDelete(tree: RedBlackTree[k, v]): (k, v, RedBlackTree[k, v]) = match tree {
        case Node(Red, Leaf, k1, v1, Leaf) => (k1, v1, Leaf)
        case Node(Black, Leaf, k1, v1, Leaf) => (k1, v1, DoubleBlackLeaf)
        case Node(Black, Leaf, k1, v1, Node(Red, Leaf, k2, v2, Leaf)) =>
            (k1, v1, Node(Black, Leaf, k2, v2, Leaf))
        case Node(c, a, k1, v1, b) =>
            let (k3, v3, e) = minDelete(a);
            (k3, v3, rotate(Node(c, e, k1, v1, b)))
        case _ => unreachable!()
    }

    ///
    /// Helper function for `delete`.
    ///
    def removeHelper(k: k, tree: RedBlackTree[k, v]): RedBlackTree[k, v] with Order[k] = match tree {
        case Node(Red, Leaf, k1, _, Leaf) => if (k <=> k1 == EqualTo) Leaf else tree
        case Node(Black, Leaf, k1, _, Leaf) => if (k <=> k1 == EqualTo) DoubleBlackLeaf else tree
        case Node(Black, Node(Red, Leaf, k1, v1, Leaf), k2, v2, Leaf) =>
            match k <=> k2 {
                case LessThan => Node(Black, removeHelper(k, Node(Red, Leaf, k1, v1, Leaf)), k2, v2, Leaf)
                case EqualTo => Node(Black, Leaf, k1, v1, Leaf)
                case GreaterThan => Node(Black, Node(Red, Leaf, k1, v1, Leaf), k2, v2, Leaf)
            }
        case Node(c, a, k1, v1, b) =>
            match k <=> k1 {
                case LessThan => rotate(Node(c, removeHelper(k, a), k1, v1, b))
                case EqualTo =>
                    let (k2, v2, e) = minDelete(b);
                    rotate(Node(c, a, k2, v2, e))
                case GreaterThan => rotate(Node(c, a, k1, v1, removeHelper(k, b)))
            }
        case _ => tree
    }

    ///
    /// Applies `f` to all key-value pairs from `tree` where `p(k)` returns `EqualTo`.
    ///
    /// The function `f` must be impure.
    ///
    pub def queryWith(p: k -> Comparison, f: (k, v) -> Unit & ef, tree: RedBlackTree[k, v]): Unit & ef = match tree {
        case Node(_, a, k, v, b) =>
            match p(k) {
                case LessThan => queryWith(p, f, b)
                case EqualTo => {
                    queryWith(p, f, a);
                    f(k, v);
                    queryWith(p, f, b)
                }
                case GreaterThan => queryWith(p, f, a)
            }
        case _ => ()
    }

    ///
    /// Extracts a range of key-value pairs from `tree`.
    ///
    /// That is, the result is a list of all pairs `f(k, v)` where `p(k)` returns `Equal`.
    ///
    pub def query(p: k -> Comparison, f: (k, v) -> a & ef, tree: RedBlackTree[k, v]): List[a] & ef = {
        let buffer = MutList.new();
        let g = k -> v -> MutList.push!(f(k, v), buffer);
        queryWith(p, g, tree);
        MutList.toList(buffer)
    } as & ef

    ///
    /// Extracts `k -> v` where `k` is the leftmost (i.e. smallest) key in the tree.
    ///
    pub def minimum(tree: RedBlackTree[k, v]): Option[(k, v)] = match tree {
        case Node(_, Leaf, k, v, _) => Some((k, v))
        case Node(_, a, _, _, _) => minimum(a)
        case _ => None
    }

    ///
    /// Extracts `k -> v` where `k` is the rightmost (i.e. largest) key in the tree.
    ///
    pub def maximum(tree: RedBlackTree[k, v]): Option[(k, v)] = match tree {
        case Node(_, _, k, v, Leaf) => Some((k, v))
        case Node(_, _, _, _, b) => maximum(b)
        case _ => None
    }

    ///
    /// Returns the black height of `tree`.
    ///
    @Time(Int32.log2(size(tree))) @Space(1)
    pub def blackHeight(tree: RedBlackTree[k, v]): Int32 = match tree {
        case Node(Black, a, _, _, _) => 1 + blackHeight(a)
        case Node(_, a, _, _, _) => blackHeight(a)
        case _ => 0
    }

    ///
    /// Returns a RedBlackTree with mappings `k -> f(k, v)` for every `k -> v` in `t`.
    ///
    /// Purity polymorphic: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def mapWithKey(f: (k, v1) -> v2 & ef, t: RedBlackTree[k, v1]): RedBlackTree[k, v2] & ef =
        let f1 = (r) -> f(r.k, r.v);
        reifyEff(f1) {
            case Pure(g) =>
                let g1 = (k, v) -> g({k = k, v = v});
                if (useParallelEvaluation(t))
                    parMapWithKey(threads() - 1, g1, t) as & Pure
                else
                    seqMapWithKey(g1, t)
            case _  => seqMapWithKey(f, t)
        }

    ///
    /// Maps `f` over the tree `t` in parallel.
    ///
    /// The implementation spawns `n` threads each applying `f` sequentially
    /// from left to right on some subtree that is disjoint from the rest of
    /// the threads.
    ///
    @Parallel
    pub def parMapWithKey(n: Int32, f: (k, v1) -> v2, t: RedBlackTree[k, v1]): RedBlackTree[k, v2] & Impure = {
        if (n <= 1)
            parMapWithKeySeq(f, t)
        else
            match t {
                case Leaf                   => Leaf
                case DoubleBlackLeaf        => DoubleBlackLeaf
                case Node(c, a, k, v, b)    =>
                    let chanL = chan RedBlackTree[k, v2] 1;
                    let chanR = chan RedBlackTree[k, v2] 1;
                    spawn (chanL <- parMapWithKey((n - 2) / 2, f, a)); // We divide the rest of the threads as follows:
                    spawn (chanR <- parMapWithKey((n - 2) / 2, f, b)); // We spawn two new threads leaving us with n - 2
                    let v1 = f(k, v);                                  // that we distribute over the two spanned threads.
                    let a1 = <- chanL;
                    let b1 = <- chanR;
                    Node(c, a1, k, v1, b1)
            }
    }

    ///
    /// Maps `f` over the tree `t` sequentially from left to right.
    ///
    /// Note that this is equivalent to `seqMapWithKey` but for performance reasons -- to avoid
    /// megamorphic calls -- we use a copy here.
    ///
    def parMapWithKeySeq(f: (k, v1) -> v2, t: RedBlackTree[k, v1]): RedBlackTree[k, v2] = match t {
        // Note that while this is identical to `seqMapWithKey` it must still be its own function
        // because the parallel path must not be reuse the seq. path.
        case Leaf                   => Leaf
        case DoubleBlackLeaf        => DoubleBlackLeaf
        case Node(c, a, k, v, b)    =>
            let a1 = parMapWithKeySeq(f, a);
            let v1 = f(k, v);
            let b1 = parMapWithKeySeq(f, b);
            Node(c, a1, k, v1, b1)
    }

    ///
    /// Sequentially maps `f` over the tree `t`.
    ///
    def seqMapWithKey(f: (k, v1) -> v2 & ef, t: RedBlackTree[k, v1]): RedBlackTree[k, v2] & ef = match t {
        case Leaf                   => Leaf
        case DoubleBlackLeaf        => DoubleBlackLeaf
        case Node(c, a, k, v, b)    =>
            let a1 = seqMapWithKey(f, a);
            let v1 = f(k, v);
            let b1 = seqMapWithKey(f, b);
            Node(c, a1, k, v1, b1)
    }

    ///
    /// Returns the number of mappings in `m` that satisfy the predicate `f`.
    ///
    /// Purity polymorphic: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def count(f: (k, v) -> Bool & ef, t: RedBlackTree[k, v]): Int32 & ef =
        let f1 = r -> f(r.k, r.v);
        reifyEff(f1) {
            case Pure(g) =>
                let g1 = (k, v) -> g({k = k, v = v});
                if (useParallelEvaluation(t))
                    parCount(threads() - 1, g1, t) as & Pure
                else
                    seqCount(g1, t)
            case _  => seqCount(f, t)
        }

    ///
    /// Applies `f` over the tree `t` in parallel and returns the number of elements
    /// that satisfy the predicate `f`.
    ///
    /// The implementation spawns `n` threads each applying `f` sequentially
    /// from left to right on some subtree that is disjoint from the rest of
    /// the threads.
    ///
    @Parallel
    pub def parCount(n: Int32, f: (k, v) -> Bool, t: RedBlackTree[k, v]): Int32 & Impure = {
        if (n <= 1)
            parCountSeq(f, t)
        else
            match t {
                case Leaf                   => 0
                case DoubleBlackLeaf        => 0
                case Node(_, a, k, v, b)    =>
                    let chanL = chan Int32 1;
                    let chanR = chan Int32 1;
                    spawn (chanL <- parCount((n - 2) / 2, f, a)); // We divide the rest of the threads as follows:
                    spawn (chanR <- parCount((n - 2) / 2, f, b)); // We spawn two new threads leaving us with n - 2
                    let v1 = if (f(k, v)) 1 else 0;               // that we distribute over the two spanned threads.
                    let a1 = <- chanL;
                    let b1 = <- chanR;
                    a1 + v1 + b1
            }
    }

    ///
    /// Applies `f` over the tree `t` sequentially from left to right and returns the number of elements
    /// that satisfy the predicate `f`.
    ///
    /// Note that this is equivalent to `seqCount` but for performance reasons -- to avoid
    /// megamorphic calls -- we use a copy here.
    ///
    def parCountSeq(f: (k, v) -> Bool, t: RedBlackTree[k, v]): Int32 = match t {
        case Leaf                   => 0
        case DoubleBlackLeaf        => 0
        case Node(_, a, k, v, b)    =>
            let a1 = seqCount(f, a);
            let v1 = if (f(k, v)) 1 else 0;
            let b1 = seqCount(f, b);
            a1 + v1 + b1
    }

    ///
    /// Applies `f` over the tree `t` sequentially from left to right and returns the number of elements
    /// that satisfy the predicate `f`.
    ///
    pub def seqCount(f: (k, v) -> Bool & ef, t: RedBlackTree[k, v]): Int32 & ef = match t {
        case Leaf                   => 0
        case DoubleBlackLeaf        => 0
        case Node(_, a, k, v, b)    =>
            let a1 = seqCount(f, a);
            let v1 = if (f(k, v)) 1 else 0;
            let b1 = seqCount(f, b);
            a1 + v1 + b1
    }


    def eqHelper(t1: RedBlackTree[k, v], t2: RedBlackTree[k, v], cont: Bool -> Bool): Bool with Eq[k], Eq[v] = match (t1, t2) {
        case (Leaf, Leaf)                                         => cont(true)
        case (DoubleBlackLeaf, DoubleBlackLeaf)                   => cont(true)
        case (Node(c1, a1, k1, v1, b1), Node(c2, a2, k2, v2, b2)) =>
            if (c1 == c2 and k1 == k2 and v1 == v2)
                eqHelper(a1, a2, ans1 -> match ans1 {
                    // Optimization: only pursue right branch if left branch equal...
                    case true => eqHelper(b1, b2, ans2 -> cont(ans1 and ans2))
                    case _    => cont(false)
                 })
            else
                cont(false)
        case _                                                    => cont(false)
    }

    def nodeA(color: Color, left: m[RedBlackTree[k, v]], k: k, value: m[v], right: m[RedBlackTree[k, v]]): m[RedBlackTree[k, v]] with Applicative[m] =
        ((((l, v, r) -> Node(color, l, k, v, r)) `Functor.map` left) `Applicative.ap` value) `Applicative.ap` right

    ///
    /// Returns a RedBlackTree with mappings `k -> f(v)` for every `k -> v` in `tree`.
    ///
    pub def mapAWithKey(f: (k, v1) -> m[v2] & ef, tree: RedBlackTree[k, v1]): m[RedBlackTree[k, v2]] & ef with Applicative[m] =
        mapAWithKeyHelper(f, tree, t -> t as & ef)

    ///
    /// Helper function for `mapAWithKey`.
    ///
    def mapAWithKeyHelper(f: (k, v1) -> m[v2] & ef,
                        tree: RedBlackTree[k, v1],
                        k: m[RedBlackTree[k, v2]] -> m[RedBlackTree[k, v2]] & ef): m[RedBlackTree[k, v2]] & ef with Applicative[m] =
        match tree {
            case Node(color, left, key, v, right) =>
                mapAWithKeyHelper(f, left, kl ->
                    mapAWithKeyHelper(f, right, kr ->
                        let mv = f(key, v);
                        k(nodeA(color, kl, key, mv, kr))))
            case _                              => k(Applicative.point(Leaf))
        }

    pub def traverse(f: v1 -> m[v2] & ef, t: RedBlackTree[k, v1]): m[RedBlackTree[k, v2]] & ef  with Applicative[m] =
        mapAWithKey((_, v) -> f(v), t)

    pub def sequence(t: RedBlackTree[k, m[v]]): m[RedBlackTree[k, v]] with Applicative[m] =
        mapAWithKey((_, v) -> v, t)

}
