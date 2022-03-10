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
            toList(t1) == toList(t2)
    }

    instance Functor[RedBlackTree[k]] {
        pub def map(f: a -> b & ef, t: RedBlackTree[k, a]): RedBlackTree[k, b] & ef = mapWithKey((_, v) -> f(v), t)
    }

    instance Foldable[RedBlackTree[k]] {
        pub def foldLeft(f: (b, v) -> b & ef, s: b, t: RedBlackTree[k, v]): b & ef = foldLeft((acc, _, v) -> f(acc, v), s, t)
        pub def foldRight(f: (v, b) -> b & ef, s: b, t: RedBlackTree[k, v]): b & ef = foldRight((_, v, acc) -> f(v, acc), s, t)
    }

    instance Traversable[RedBlackTree[k]] {
        pub def traverse(f: a -> m[b] & ef, t: RedBlackTree[k, a]): m[RedBlackTree[k, b]] & ef with Applicative[m] =
            mapAWithKey((_, v) -> f(v), t)

        pub override def sequence(t: RedBlackTree[k, m[a]]): m[RedBlackTree[k, a]] with Applicative[m] =
            mapAWithKey((_, v) -> v, t)
    }

    ///
    /// The color of a red-black tree node.
    ///
    pub enum Color {

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
    /// Updates `tree` with `k => v` if `k => v1` is in `tree`.
    ///
    /// Otherwise, updates `tree` with `k => v`.
    ///
    @Time(Int32.log2(size(tree))) @Space(Int32.log2(size(tree)))
    pub def insert(k: k, v: v, tree: RedBlackTree[k, v]): RedBlackTree[k, v] with Order[k] =
        blacken(insertHelper(k, v, tree))

    ///
    /// Updates `tree` with `k => f(k, v, v1)` if `k => v1` is in `tree`.
    ///
    /// Otherwise, updates `tree` with `k => v`.
    ///
    @Time(time(f) + Int32.log2(size(tree))) @Space(space(f) + Int32.log2(size(tree)))
    pub def insertWith(f: (k, v, v) -> v & ef, k: k, v: v, tree: RedBlackTree[k, v]): RedBlackTree[k, v] & ef with Order[k] =
        blacken(insertWithHelper(f, k, v, tree))

    ///
    /// Updates `tree` with `k => v1` if `k => v` is in `tree` and `f(k, v) = Some(v1)`.
    ///
    /// Otherwise, returns `tree`.
    ///
    @Time(time(f) + Int32.log2(size(tree))) @Space(space(f) + Int32.log2(size(tree)))
    pub def updateWith(f: (k, v) -> Option[v] & ef, k: k, tree: RedBlackTree[k, v]): RedBlackTree[k, v] & ef with Order[k] =
        blacken(updateWithHelper(f, k, tree))

    ///
    /// Removes `k => v` from `tree` if `tree` contains the key `k`.
    ///
    /// Otherwise, returns `tree`.
    ///
    pub def remove(k: k, tree: RedBlackTree[k, v]): RedBlackTree[k, v] with Order[k] = redden(removeHelper(k, tree))

    ///
    /// Returns `Some(v)` if `k => v` is in `tree`.
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


    ///
    /// Lazily applies `f` to a start value `s` and all key-value pairs in `tree` going from right to left.
    ///
    /// That is, the result is of the form: `f(k1, v1, ... lazy f(kn-1, vn-1, lazy f(kn, vn, s)))`.
    /// The foldRightLazy function exists to allow early termination of a fold.
    ///
    @Experimental
    @Lazy
    pub def foldRightLazy(f: (k, v, Lazy[b]) -> b, s: b, tree: RedBlackTree[k, v]): b =
        def loop(s2: b, tree2: RedBlackTree[k, v]) = match tree2 {
            case Node(_, a, k, v, b) => loop(f(k, v, lazy loop(s2, b)), a)
            case _ => s2
        };
        loop(s, tree)


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
        case Node(Red, Leaf, k1, _, Leaf) => if ((k <=> k1) == EqualTo) Leaf else tree
        case Node(Black, Leaf, k1, _, Leaf) => if ((k <=> k1) == EqualTo) DoubleBlackLeaf else tree
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
    /// Extracts `k => v` where `k` is the leftmost (i.e. smallest) key in the tree.
    ///
    pub def minimumKey(tree: RedBlackTree[k, v]): Option[(k, v)] = match tree {
        case Node(_, Leaf, k, v, _) => Some((k, v))
        case Node(_, a, _, _, _) => minimumKey(a)
        case _ => None
    }

    ///
    /// Extracts `k => v` where `k` is the rightmost (i.e. largest) key in the tree.
    ///
    pub def maximumKey(tree: RedBlackTree[k, v]): Option[(k, v)] = match tree {
        case Node(_, _, k, v, Leaf) => Some((k, v))
        case Node(_, _, _, _, b) => maximumKey(b)
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
    /// Returns a RedBlackTree with mappings `k => f(k, v)` for every `k => v` in `t`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def mapWithKey(f: (k, v1) -> v2 & ef, t: RedBlackTree[k, v1]): RedBlackTree[k, v2] & ef =
        let f1 = (r) -> f(r.k, r.v);
        reifyEff(f1) {
            case Pure(g) =>
                if (useParallelEvaluation(t))
                    let h = (k, v) -> g({k = k, v = v});
                    parMapWithKey(threads() - 1, h, t) as & Pure
                else
                    seqMapWithKey(f, t)
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
    def parMapWithKey(n: Int32, f: (k, v1) -> v2, t: RedBlackTree[k, v1]): RedBlackTree[k, v2] & Impure = {
        if (n <= 1)
            parSeqMapWithKey(f, t)
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
    def parSeqMapWithKey(f: (k, v1) -> v2, t: RedBlackTree[k, v1]): RedBlackTree[k, v2] = match t {
        // Note that while this is identical to `seqMapWithKey` it must still be its own function
        // because the parallel path must not be reuse the seq. path.
        case Leaf                   => Leaf
        case DoubleBlackLeaf        => DoubleBlackLeaf
        case Node(c, a, k, v, b)    =>
            let a1 = seqMapWithKey(f, a);
            let v1 = f(k, v);
            let b1 = seqMapWithKey(f, b);
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
            seqCount(f, t)
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
    def seqCount(f: (k, v) -> Bool, t: RedBlackTree[k, v]): Int32 = match t {
        case Leaf                   => 0
        case DoubleBlackLeaf        => 0
        case Node(_, a, k, v, b)    =>
            let a1 = seqCount(f, a);
            let v1 = if (f(k, v)) 1 else 0;
            let b1 = seqCount(f, b);
            a1 + v1 + b1
    }

    ///
    /// Returns the sum of all keys in the tree `t`.
    ///
    pub def sumKeys(t: RedBlackTree[Int32, v]): Int32 =
        sumWith((k, _) -> k, t)

    ///
    /// Returns the sum of all values in the tree `t`.
    ///
    pub def sumValues(t: RedBlackTree[k, Int32]): Int32 =
        sumWith((_, v) -> v, t)

    ///
    /// Returns the sum of all key-value pairs `k => v` in the tree `t`
    /// according to the function `f`.
    ///
    pub def sumWith(f: (k, v) -> Int32 & ef, t: RedBlackTree[k, v]): Int32 & ef =
        def loop(tt) = match tt {
            case Leaf                => 0
            case DoubleBlackLeaf     => 0
            case Node(_, a, k, v, b) =>
                let x = loop(a);
                let y = f(k, v);
                let z = loop(b);
                x + y + z
        };
        loop(t)

    ///
    /// Returns the sum of all key-value pairs `k => v` in the tree `t`
    /// according to the function `f`.
    ///
    /// The implementation spawns `n` threads each applying `f` sequentially
    /// from left to right on some subtree that is disjoint from the rest of
    /// the threads.
    ///
    @Parallel
    pub def parSumWith(n: Int32, f: (k, v) -> Int32, t: RedBlackTree[k, v]): Int32 & Impure = {
        if (n <= 1)
            seqSumWith(f, t)
        else
            match t {
                case Leaf                   => 0
                case DoubleBlackLeaf        => 0
                case Node(_, a, k, v, b)    =>
                    let chanL = chan Int32 1;
                    let chanR = chan Int32 1;
                    spawn (chanL <- parSumWith((n - 2) / 2, f, a)); // We divide the rest of the threads as follows:
                    spawn (chanR <- parSumWith((n - 2) / 2, f, b)); // We spawn two new threads leaving us with n - 2
                    let v1 = f(k, v);                               // that we distribute over the two spanned threads.
                    let a1 = <- chanL;
                    let b1 = <- chanR;
                    a1 + v1 + b1
            }
    }

    ///
    /// Returns the sum of all key-value pairs `k => v` in the tree `t`
    /// according to the function `f`.
    ///
    /// Note that this is equivalent to `sumWith` but for performance reasons -- to avoid
    /// megamorphic calls -- we use a copy here.
    ///
    def seqSumWith(f: (k, v) -> Int32, t: RedBlackTree[k, v]): Int32 =
        def loop(tt) = match tt {
            case Leaf                => 0
            case DoubleBlackLeaf     => 0
            case Node(_, a, k, v, b) =>
                let x = loop(a);
                let y = f(k, v);
                let z = loop(b);
                x + y + z
        };
        loop(t)

    ///
    /// Returns the product of all keys in the tree `t`.
    ///
    pub def productKeys(t: RedBlackTree[Int32, v]): Int32 =
        productWith((k, _) -> k, t)

    ///
    /// Returns the product of all values in the tree `t`.
    ///
    pub def productValues(t: RedBlackTree[k, Int32]): Int32 =
        productWith((_, v) -> v, t)

    ///
    /// Returns the product of all key-value pairs `k => v` in the tree `t`
    /// according to the function `f`.
    ///
    pub def productWith(f: (k, v) -> Int32 & ef, t: RedBlackTree[k, v]): Int32 & ef =
        def loop(tt) = match tt {
            case Leaf                => 1
            case DoubleBlackLeaf     => 1
            case Node(_, a, k, v, b) =>
                let x = loop(a);
                let y = f(k, v);
                let z = loop(b);
                x * y * z
        };
        match t {
            case Leaf            => 0
            case DoubleBlackLeaf => 0
            case _               => loop(t)
        }

    ///
    /// Returns the product of all key-value pairs `k => v` in the tree `t`
    /// according to the function `f`.
    ///
    /// The implementation spawns `n` threads each applying `f` sequentially
    /// from left to right on some subtree that is disjoint from the rest of
    /// the threads.
    ///
    @Parallel
    pub def parProductWith(n: Int32, f: (k, v) -> Int32, t: RedBlackTree[k, v]): Int32 & Impure = {
        if (n <= 1)
            seqProductWith(f, t)
        else
            match t {
                case Leaf                   => 1
                case DoubleBlackLeaf        => 1
                case Node(_, a, k, v, b)    =>
                    let chanL = chan Int32 1;
                    let chanR = chan Int32 1;
                    spawn (chanL <- parProductWith((n - 2) / 2, f, a)); // We divide the rest of the threads as follows:
                    spawn (chanR <- parProductWith((n - 2) / 2, f, b)); // We spawn two new threads leaving us with n - 2
                    let v1 = f(k, v);                                   // that we distribute over the two spanned threads.
                    let a1 = <- chanL;
                    let b1 = <- chanR;
                    a1 * v1 * b1
            }
    }

    ///
    /// Returns the product of all key-value pairs `k => v` in the tree `t`
    /// according to the function `f`.
    ///
    /// Note that this is equivalent to `productWith` but for performance reasons -- to avoid
    /// megamorphic calls -- we use a copy here.
    ///
    def seqProductWith(f: (k, v) -> Int32, t: RedBlackTree[k, v]): Int32 =
        def loop(tt) = match tt {
            case Leaf                => 1
            case DoubleBlackLeaf     => 1
            case Node(_, a, k, v, b) =>
                let x = loop(a);
                let y = f(k, v);
                let z = loop(b);
                x * y * z
        };
        match t {
            case Leaf            => 0
            case DoubleBlackLeaf => 0
            case _               => loop(t)
        }

    ///
    /// Returns the tree `t` as a list. Elements are ordered from smallest (left) to largest (right).
    ///
    pub def toList(t: RedBlackTree[k, v]): List[(k, v)] =
        RedBlackTree.foldRight((k, v, xs) -> (k, v) :: xs, Nil, t)

    ///
    /// Build a node applicatively.
    ///
    /// This is a helper function for `mapAWithKeyHelper`.
    ///
    def nodeA(color: Color, left: m[RedBlackTree[k, v]], k: k, value: m[v], right: m[RedBlackTree[k, v]]): m[RedBlackTree[k, v]] with Applicative[m] =
        ((((l, v, r) -> Node(color, l, k, v, r)) `Functor.map` left) `Applicative.ap` value) `Applicative.ap` right

    ///
    /// Returns a RedBlackTree with mappings `k => f(v)` for every `k => v` in `tree`.
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

    ///
    /// Applies `cmp` over the tree `t` in parallel and optionally returns the lowest
    /// element according to `cmp`.
    ///
    /// The implementation spawns `n` threads each applying `cmp` sequentially
    /// from left to right on some subtree that is disjoint from the rest of
    /// the threads.
    ///
    @Parallel
    pub def parMinimumBy(n: Int32, cmp: (k, v, k, v) -> Comparison, t: RedBlackTree[k, v]): Option[(k, v)] & Impure =
        parLimitBy(n, (kl, vl, kr, vr) -> if (cmp(kl, vl, kr, vr) == LessThan) (kl, vl) else (kr, vr), t)

    ///
    /// Applies `cmp` over the tree `t` in parallel and optionally returns the largest
    /// element according to `cmp`.
    ///
    /// The implementation spawns `n` threads each applying `cmp` sequentially
    /// from left to right on some subtree that is disjoint from the rest of
    /// the threads.
    ///
    @Parallel
    pub def parMaximumBy(n: Int32, cmp: (k, v, k, v) -> Comparison, t: RedBlackTree[k, v]): Option[(k, v)] & Impure =
        parLimitBy(n, (kl, vl, kr, vr) -> if (cmp(kl, vl, kr, vr) == GreaterThan) (kl, vl) else (kr, vr), t)

    ///
    /// Helper function for `minimumBy` & `maximumBy`.
    ///
    /// Applies `cmp` over the tree `t` in parallel and optionally returns the min/max (decided by `decider`)
    /// element according to `cmp`.
    ///
    /// The implementation spawns `n` threads each applying `cmp` sequentially
    /// from left to right on some subtree that is disjoint from the rest of
    /// the threads.
    ///
    @Parallel
    def parLimitBy(n: Int32, selector: (k, v, k, v) -> (k, v), t: RedBlackTree[k, v]): Option[(k, v)] & Impure =
        if (n <= 0)
            parSeqLimitBy(selector, t)
        else
            match t {
                case Leaf                => None
                case DoubleBlackLeaf     => None
                case Node(_, a, k, v, b) =>
                    let chanL = chan Option[(k, v)] 1;
                    let chanR = chan Option[(k, v)] 1;
                    spawn (chanL <- parLimitBy((n - 2) / 2, selector, a));
                    spawn (chanR <- parLimitBy((n - 2) / 2, selector, b));
                    let a1 = <- chanL;
                    let res = match a1 {
                        case None         => (k, v)
                        case Some(kl, vl) => selector(kl, vl, k, v)
                    };
                    let b1 = <- chanR;
                    match b1 {
                        case None         => Some(res)
                        case Some(kr, vr) =>
                            let (ks, vs) = res;
                            Some(selector(ks, vs, kr, vr))
                    }
            }

    ///
    /// Applies `cmp` over the tree `t` sequentially and optionally returns the lowest
    /// element according to `cmp`.
    ///
    def parSeqLimitBy(selector: (k, v, k, v) -> (k, v) & ef, t: RedBlackTree[k, v]): Option[(k, v)] & ef = match t {
        case Leaf                   => None
        case DoubleBlackLeaf        => None
        case Node(_, a, k, v, b)    =>
            let res = match parSeqLimitBy(selector, a) {
                case None         => (k, v)
                case Some(kl, vl) => selector(kl, vl, k, v)
            };
            match parSeqLimitBy(selector, b) {
                case None         => Some(res)
                case Some(kr, vr) =>
                    let (ks, vs) = res;
                    Some(selector(ks, vs, kr, vr))
            }
    }

    ///
    /// Returns the concatenation of the string representation of each key `k`
    /// in `t` with `sep` inserted between each element.
    ///
    pub def joinKeys(sep: String, t: RedBlackTree[k, v]): String with ToString[k] =
        joinWith((k, _) -> ToString.toString(k), sep, t)

    ///
    /// Returns the concatenation of the string representation of each value `v`
    /// in `t` with `sep` inserted between each element.
    ///
    pub def joinValues(sep: String, t: RedBlackTree[k, v]): String with ToString[v] =
        joinWith((_, v) -> ToString.toString(v), sep, t)

    ///
    /// Returns the concatenation of the string representation of each key-value pair
    /// `k => v` in `t` according to `f` with `sep` inserted between each element.
    ///
    pub def joinWith(f: (k, v) -> String & ef, sep: String, t: RedBlackTree[k, v]): String & ef = {
        use StringBuilder.append!;
        let lastSep = String.length(sep);
        let sb = StringBuilder.new();
        foreach((k, v) -> { append!(f(k, v), sb); append!(sep, sb) }, t);
        StringBuilder.toString(sb) |> String.dropRight(lastSep)
    } as & ef

}
