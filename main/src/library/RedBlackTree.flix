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
        pub def foldRightWithCont(f: (v, Unit -> b & ef) -> b & ef, s: b, t: RedBlackTree[k, v]): b & ef = foldRightWithCont((_, v, acc) -> f(v, acc), s, t)
    }

    instance UnorderedFoldable[RedBlackTree[k]] {
        pub def foldMap(f: v -> b & ef, t: RedBlackTree[k, v]): b & ef with CommutativeMonoid[b] = RedBlackTree.foldMap(_ -> f, t)
        override pub def isEmpty(t: RedBlackTree[k, v]): Bool = RedBlackTree.isEmpty(t)
        override pub def exists(f: v -> Bool & ef, t: RedBlackTree[k, v]): Bool & ef = RedBlackTree.exists(_ -> f, t)
        override pub def forall(f: v -> Bool & ef, t: RedBlackTree[k, v]): Bool & ef = RedBlackTree.forall(_ -> f, t)
    }

    instance Traversable[RedBlackTree[k]] {
        pub def traverse(f: a -> m[b] & ef, t: RedBlackTree[k, a]): m[RedBlackTree[k, b]] & ef with Applicative[m] =
            mapAWithKey((_, v) -> f(v), t)

        pub override def sequence(t: RedBlackTree[k, m[a]]): m[RedBlackTree[k, a]] with Applicative[m] =
            mapAWithKey((_, v) -> v, t)
    }

    instance FunctorFilter[RedBlackTree[k]] with Order[k] {
        pub def filterMap(f: a -> Option[b] & ef, t: RedBlackTree[k, a]): RedBlackTree[k, b] & ef = filterMapImplementation(f, t)
        pub override def filter(f: a -> Bool & ef, t: RedBlackTree[k, a]): RedBlackTree[k, a] & ef = filterImplementation(f, t)
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
    /// Returns the number of nodes in `t`.
    ///
    @Time(size(t)) @Space(Int32.log2(size(t)))
    pub def size(t: RedBlackTree[k, v]): Int32 =
        def loop(tt, k) = match tt {
            case Node(_, a, _, _, b) => loop(a, ks -> k(loop(b, ys -> ks + ys + 1)))
            case _                   => k(0)
        };
        loop(t, identity)

    ///
    /// Returns the empty tree.
    ///
    @Time(1) @Space(1)
    pub def empty(): RedBlackTree[k, v] = Leaf

    ///
    /// Returns `true` if and only if `t` is the empty tree.
    ///
    @Time(1) @Space(1)
    pub def isEmpty(t: RedBlackTree[k, v]): Bool = match t {
        case Leaf => true
        case _    => false
    }

    ///
    /// Updates `t` with `k => v` if `k => v1` is in `t`.
    ///
    /// Otherwise, updates `t` with `k => v`.
    ///
    @Time(Int32.log2(size(t))) @Space(Int32.log2(size(t)))
    pub def insert(k: k, v: v, t: RedBlackTree[k, v]): RedBlackTree[k, v] with Order[k] =
        def loop(tt, sk) = match tt {
            case Leaf                  => sk(Node(Red, Leaf, k, v, Leaf))
            case Node(c, a, k1, v1, b) => match k <=> k1 {
                case LessThan    => loop(a, ks -> sk(balance(Node(c, ks, k1, v1, b))))
                case EqualTo     => sk(Node(c, a, k, v, b))
                case GreaterThan => loop(b, ks -> sk(balance(Node(c, a, k1, v1, ks))))
            }
            case _ => sk(tt)
        };
        blacken(loop(t, identity))

    ///
    /// Updates `t` with `k => f(k, v, v1)` if `k => v1` is in `t`.
    ///
    /// Otherwise, updates `t` with `k => v`.
    ///
    @Time(time(f) + Int32.log2(size(t))) @Space(space(f) + Int32.log2(size(t)))
    pub def insertWith(f: (k, v, v) -> v & ef, k: k, v: v, t: RedBlackTree[k, v]): RedBlackTree[k, v] & ef with Order[k] =
        def loop(tt, sk) = match tt {
            case Leaf                  => sk(Node(Red, Leaf, k, v, Leaf))
            case Node(c, a, k1, v1, b) => match k <=> k1 {
                case LessThan    => loop(a, ks -> sk(balance(Node(c, ks, k1, v1, b))))
                case EqualTo     => sk(Node(c, a, k, f(k, v, v1), b))
                case GreaterThan => loop(b, ks -> sk(balance(Node(c, a, k1, v1, ks))))
            }
            case _ => sk(tt)
        };
        blacken(loop(t, identity))

    ///
    /// Updates `t` with `k => v1` if `k => v` is in `t` and `f(k, v) = Some(v1)`.
    ///
    /// Otherwise, returns `t`.
    ///
    @Time(time(f) + Int32.log2(size(t))) @Space(space(f) + Int32.log2(size(t)))
    pub def updateWith(f: (k, v) -> Option[v] & ef, k: k, t: RedBlackTree[k, v]): RedBlackTree[k, v] & ef with Order[k] =
        def loop(tt, sk) = match tt {
            case Leaf                  => sk(tt)
            case Node(c, a, k1, v1, b) => match k <=> k1 {
                case LessThan => loop(a, ks -> sk(balance(Node(c, ks, k1, v1, b))))
                case EqualTo  => match f(k1, v1) {
                    case None    => sk(tt)
                    case Some(v) => sk(Node(c, a, k, v, b))
                }
                case GreaterThan => loop(b, ks -> sk(balance(Node(c, a, k1, v1, ks))))
            }
            case _ => sk(tt)
        };
        blacken(loop(t, identity))

    ///
    /// Removes `k => v` from `t` if `t` contains the key `k`.
    ///
    /// Otherwise, returns `t`.
    ///
    pub def remove(k: k, t: RedBlackTree[k, v]): RedBlackTree[k, v] with Order[k] =
        def loop(tt, sk) = match tt {
        case Node(Red, Leaf, k1, _, Leaf)   => sk(if ((k <=> k1) == EqualTo) Leaf else tt)
        case Node(Black, Leaf, k1, _, Leaf) => sk(if ((k <=> k1) == EqualTo) DoubleBlackLeaf else tt)
        case Node(Black, Node(Red, Leaf, k1, v1, Leaf), k2, v2, Leaf) =>
            match k <=> k2 {
                case LessThan    => loop(Node(Red, Leaf, k1, v1, Leaf), ks -> sk(Node(Black, ks, k2, v2, Leaf)))
                case EqualTo     => sk(Node(Black, Leaf, k1, v1, Leaf))
                case GreaterThan => sk(Node(Black, Node(Red, Leaf, k1, v1, Leaf), k2, v2, Leaf))
            }
        case Node(c, a, k1, v1, b) =>
            match k <=> k1 {
                case LessThan => loop(a, ks -> sk(rotate(Node(c, ks, k1, v1, b))))
                case EqualTo  =>
                    let (k2, v2, e) = minDelete(b);
                    sk(rotate(Node(c, a, k2, v2, e)))
                case GreaterThan => loop(b, ks -> sk(rotate(Node(c, a, k1, v1, ks))))
            }
            case _ => sk(tt)
        };
        redden(loop(t, identity))

    ///
    /// Returns `Some(v)` if `k => v` is in `t`.
    ///
    /// Otherwise returns `None`.
    ///
    @Time(Int32.log2(size(t))) @Space(1)
    pub def get(k: k, t: RedBlackTree[k, v]): Option[v] with Order[k] = match t {
        case Node(_, a, k1, v, b) => match k <=> k1 {
            case LessThan    => get(k, a)
            case EqualTo     => Some(v)
            case GreaterThan => get(k, b)
        }
        case _ => None
    }

    ///
    /// Returns `true` if and only if `t` contains the key `k`.
    ///
    @Time(Int32.log2(size(t))) @Space(1)
    pub def memberOf(k: k, t: RedBlackTree[k, v]): Bool with Order[k] = match t {
        case Node(_, a, k1, _, b) => match k <=> k1 {
           case LessThan    => memberOf(k, a)
           case EqualTo     => true
           case GreaterThan => memberOf(k, b)
        }
        case _ => false
    }

    ///
    /// Optionally returns the first key-value pair in `t` that satisfies the predicate `f` when searching from left to right.
    ///
    @Time(time(f) * size(t)) @Space(space(f) * Int32.log2(size(t)))
    pub def findLeft(f: (k, v) -> Bool & ef, t: RedBlackTree[k, v]): Option[(k, v)] & ef =
        def loop(tt, sk) = match tt {
            case Node(_, a, k, v, b) => loop(a, kl -> match kl {
                case None => if (f(k, v)) Some(k, v) else loop(b, kr -> match kr {
                    case None => sk(None)
                    case _    => kr
                })
                case _ => kl
            })
            case _ => sk(None)
        };
        loop(t, k -> k as & ef)

    ///
    /// Optionally returns the first key-value pair in `t` that satisfies the predicate `f` when searching from right to left.
    ///
    @Time(time(f) * size(t)) @Space(space(f) * Int32.log2(size(t)))
    pub def findRight(f: (k, v) -> Bool & ef, t: RedBlackTree[k, v]): Option[(k, v)] & ef =
        def loop(tt, sk) = match tt {
            case Node(_, a, k, v, b) => loop(b, kr -> match kr {
                case None => if (f(k, v)) Some(k, v) else loop(a, kl -> match kl {
                    case None => sk(None)
                    case _    => kl
                })
                case _ => kr
            })
            case _ => sk(None)
        };
        loop(t, k -> k as & ef)

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `t` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, k1, v1), k2, v2)..., vn)`.
    ///
    pub def foldLeft(f: (b, k, v) -> b & ef, s: b, t: RedBlackTree[k, v]): b & ef = match t {
        case Node(_, a, k, v, b) => foldLeft(f, f(foldLeft(f, s, a), k, v), b)
        case _                   => s
    }

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `tree` going from right to left.
    ///
    /// That is, the result is of the form: `f(k1, v1, ...f(kn-1, vn-1, f(kn, vn, s)))`.
    ///
    pub def foldRight(f: (k, v, b) -> b & ef, s: b, t: RedBlackTree[k, v]): b & ef = match t {
        case Node(_, a, k, v, b) => foldRight(f, f(k, v, foldRight(f, s, b)), a)
        case _ => s
    }

    ///
    /// Returns the result of mapping each key-value pair and combining the results.
    ///
    pub def foldMap(f: (k, v) -> b & ef, t: RedBlackTree[k, v]): b & ef with Monoid[b] =
        foldLeft((acc, k, v) -> Monoid.combine(acc, f(k, v)), Monoid.empty(), t)

    ///
    /// Applies `f` to a start value `z` and all key-value pairs in `t` going from right to left.
    ///
    /// That is, the result is of the form: `f(k1, v1, ...f(kn-1, vn-1, f(kn, vn, s)))`.
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (k, v, Unit -> b & ef) -> b & ef, z: b, t: RedBlackTree[k, v]): b & ef = match t {
        case Node(_, a, k, v, b) => foldRightWithCont(f, f(k, v, _ -> foldRightWithCont(f, z, b)), a)
        case _                   => z
    }

    ///
    /// Applies `f` to all key-value pairs in `tree` going from left to right until a single pair `(k, v)` is obtained.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(k1, v1, k2, v2), k3, v3)..., kn, vn))`
    ///
    /// Returns `None` if `t` is the empty tree.
    ///
    pub def reduceLeft(f: (k, v, k, v) -> (k, v) & ef, t: RedBlackTree[k, v]): Option[(k, v)] & ef =
        foldLeft((x, k1, v1) -> match x {
            case Some((k2, v2)) => Some(f(k2, v2, k1, v1))
            case None           => Some(k1, v1)
        }, None, t)

    ///
    /// Applies `f` to all key-value pairs in `t` going from right to left until a single pair `(k, v)` is obtained.
    ///
    /// That is, the result is of the form: `Some(f(k1, v1, ...f(kn-2, vn-2, f(kn-1, vn-1, kn, vn))...))`.
    ///
    /// Returns `None` if `t` is the empty tree.
    ///
    pub def reduceRight(f: (k, v, k, v) -> (k, v) & ef, t: RedBlackTree[k, v]): Option[(k, v)] & ef =
        foldRight((k1, v1, acc) -> match acc {
            case Some((k2, v2)) => Some(f(k1, v1, k2, v2))
            case None           => Some(k1, v1)
        }, None, t)

    ///
    /// Returns `true` if and only if at least one key-value pair in `t` satisfies the predicate `f`.
    ///
    /// Returns `false` if `t` is the empty tree.
    ///
    @Time(time(f) * size(t)) @Space(space(f) * Int32.log2(size(t)))
    pub def exists(f: (k, v) -> Bool & ef, t: RedBlackTree[k, v]): Bool & ef = match t {
        case Node(_, a, k, v, b) => if (f(k, v)) true else exists(f, a) or exists(f, b)
        case _                   => false
    }

    ///
    /// Returns `true` if and only if at least one key-value pair in `t` satisfies the predicate `f`.
    ///
    /// Returns `false` if `t` is the empty tree.
    ///
    /// The function `f` must be pure.
    ///
    /// Traverses the tree `t` in parallel.
    ///
    @Parallel
    pub def parExists(n: Int32, f: (k, v) -> Bool, t: RedBlackTree[k, v]): Bool & Impure =
        if (n <= 1)
            seqExists(f, t)
        else
            match t {
                case Node(_, a, k, v, b) =>
                    let chanL = chan Bool 1;
                    let chanR = chan Bool 1;
                    if (f(k, v))
                        true
                    else {
                        spawn (chanL <- parExists((n - 2) / 2, f, a)); // We divide the rest of the threads as follows:
                        spawn (chanR <- parExists((n - 2) / 2, f, b)); // We spawn two new threads leaving us with n - 2
                        let a1 = <- chanL;                             // that we distribute over the two spanned threads.
                        let b1 = <- chanR;
                        a1 or b1
                    }
                case _ => false
            }

    ///
    /// Returns `true` if and only if at least one key-value pair in `t` satisfies the predicate `f`.
    ///
    /// Returns `false` if `t` is the empty tree.
    ///
    /// The function `f` must be pure.
    ///
    /// Note that this is equivalent to `exists` but for performance reasons -- to avoid
    /// megamorphic calls -- we use a copy here.
    ///
    def seqExists(f: (k, v) -> Bool, t: RedBlackTree[k, v]): Bool = match t {
        case Node(_, a, k, v, b) => if (f(k, v)) true else seqExists(f, a) or seqExists(f, b)
        case _                   => false
    }

    ///
    /// Returns `true` if and only if all key-value pairs in `t` satisfy the predicate `f`.
    ///
    /// Returns `true` if `t` is the empty tree.
    ///
    @Time(time(f) * size(t)) @Space(space(f) * Int32.log2(size(t)))
    pub def forall(f: (k, v) -> Bool & ef, t: RedBlackTree[k, v]): Bool & ef = match t {
        case Node(_, a, k, v, b) => if (f(k, v)) forall(f, a) and forall(f, b) else false
        case _                   => true
    }

    ///
    /// Returns `true` if and only if all key-value pairs in `t` satisfy the predicate `f`.
    ///
    /// Returns `true` if `t` is the empty tree.
    ///
    /// The function `f` must be pure.
    ///
    /// Traverses the tree `t` in parallel.
    ///
    @Parallel
    pub def parForall(n: Int32, f: (k, v) -> Bool, t: RedBlackTree[k, v]): Bool & Impure =
        if (n <= 1)
            seqForall(f, t)
        else
            match t {
                case Node(_, a, k, v, b) =>
                    let chanL = chan Bool 1;
                    let chanR = chan Bool 1;
                    if (not f(k, v))
                        false
                    else {
                        spawn (chanL <- parForall((n - 2) / 2, f, a)); // We divide the rest of the threads as follows:
                        spawn (chanR <- parForall((n - 2) / 2, f, b)); // We spawn two new threads leaving us with n - 2
                        let a1 = <- chanL;                             // that we distribute over the two spanned threads.
                        let b1 = <- chanR;
                        a1 and b1
                    }
                case _ => false
            }

    ///
    /// Returns `true` if and only if all key-value pairs in `t` satisfy the predicate `f`.
    ///
    /// Returns `true` if `t` is the empty tree.
    ///
    /// The function `f` must be pure.
    ///
    /// Note that this is equivalent to `exists` but for performance reasons -- to avoid
    /// megamorphic calls -- we use a copy here.
    ///
    def seqForall(f: (k, v) -> Bool, t: RedBlackTree[k, v]): Bool = match t {
        case Node(_, a, k, v, b) => if (f(k, v)) seqForall(f, a) and seqForall(f, b) else false
        case _                   => true
    }

    ///
    /// Applies `f` to every key-value pair of `t`.
    ///
    @Time(time(f) * size(t)) @Space(space(f) * Int32.log2(size(t)))
    pub def foreach(f: (k, v) -> Unit & ef, t: RedBlackTree[k, v]): Unit & ef = match t {
        case Node(_, a, k, v, b) => foreach(f, a); f(k, v); foreach(f, b)
        case _                   => ()
    }

    ///
    /// Helper function for `insert` and `delete`.
    ///
    def balance(t: RedBlackTree[k, v]): RedBlackTree[k, v] = match t {
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
        case _ => t
    }

    ///
    /// Helper function for `insert`.
    ///
    def blacken(t: RedBlackTree[k, v]): RedBlackTree[k, v] = match t {
        case Node(Red, Node(Red, a, k1, v1, b), k2, v2, c) =>
            Node(Black, Node(Red, a, k1, v1, b), k2, v2, c)
        case Node(Red, a, k1, v1, Node(Red, b, k2, v2, c)) =>
            Node(Black, a, k1, v1, Node(Red, b, k2, v2, c))
        case _ => t
    }

    ///
    /// Helper function for `delete`.
    ///
    def rotate(t: RedBlackTree[k, v]): RedBlackTree[k, v] = match t {
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
        case _ => t
    }

    ///
    /// Helper function for `delete`.
    ///
    def redden(t: RedBlackTree[k, v]): RedBlackTree[k, v] = match t {
        case Node(Black, Node(Black, a, k1, v1, b), k2, v2, Node(Black, c, k3, v3, d)) =>
            Node(Red, Node(Black, a, k1, v1, b), k2, v2, Node(Black, c, k3, v3, d))
        case DoubleBlackLeaf => Leaf
        case _ => t
    }

    ///
    /// Helper function for `delete`.
    ///
    def minDelete(t: RedBlackTree[k, v]): (k, v, RedBlackTree[k, v]) =
        def loop(tt, sk) = match tt {
            case Node(Red, Leaf, k1, v1, Leaf)   => sk((k1, v1, Leaf))
            case Node(Black, Leaf, k1, v1, Leaf) => sk((k1, v1, DoubleBlackLeaf))
            case Node(Black, Leaf, k1, v1, Node(Red, Leaf, k2, v2, Leaf)) =>
                sk((k1, v1, Node(Black, Leaf, k2, v2, Leaf)))
            case Node(c, a, k1, v1, b) => loop(a, match (k3, v3, e) -> sk((k3, v3, rotate(Node(c, e, k1, v1, b)))))
            case _ => unreachable!()
        };
        loop(t, identity)

    ///
    /// Applies `f` to all key-value pairs from `t` where `p(k)` returns `EqualTo`.
    ///
    /// The function `f` must be impure.
    ///
    pub def queryWith(p: k -> Comparison & ef1, f: (k, v) -> Unit & ef2, t: RedBlackTree[k, v]): Unit \ { ef1, ef2 } = match t {
        case Node(_, a, k, v, b) =>
            match p(k) {
                case LessThan    => queryWith(p, f, b)
                case EqualTo     => queryWith(p, f, a); f(k, v); queryWith(p, f, b)
                case GreaterThan => queryWith(p, f, a)
            }
        case _ => ()
    }

    ///
    /// Extracts a range of key-value pairs from `t`.
    ///
    /// That is, the result is a list of all pairs `f(k, v)` where `p(k)` returns `Equal`.
    ///
    pub def query(p: k -> Comparison & ef1, f: (k, v) -> a & ef2, t: RedBlackTree[k, v]): List[a] \ { ef1, ef2 } = region r {
        let buffer = new MutList(r);
        let g = k -> v -> MutList.push!(f(k, v), buffer);
        queryWith(p, g, t);
        MutList.toList(buffer)
    }

    ///
    /// Extracts `k => v` where `k` is the leftmost (i.e. smallest) key in the tree.
    ///
    pub def minimumKey(t: RedBlackTree[k, v]): Option[(k, v)] = match t {
        case Node(_, Leaf, k, v, _) => Some(k, v)
        case Node(_, a, _, _, _)    => minimumKey(a)
        case _                      => None
    }

    ///
    /// Extracts `k => v` where `k` is the rightmost (i.e. largest) key in the tree.
    ///
    pub def maximumKey(t: RedBlackTree[k, v]): Option[(k, v)] = match t {
        case Node(_, _, k, v, Leaf) => Some(k, v)
        case Node(_, _, _, _, b)    => maximumKey(b)
        case _                      => None
    }

    ///
    /// Returns the black height of `t`.
    ///
    @Time(Int32.log2(size(t))) @Space(1)
    pub def blackHeight(t: RedBlackTree[k, v]): Int32 =
        def loop(tt, acc) = match tt {
            case Node(Black, a, _, _, _) => loop(a, 1 + acc)
            case Node(_, a, _, _, _)     => loop(a, acc)
            case _                       => acc
        };
        loop(t, 0)

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
        case Leaf                => Leaf
        case DoubleBlackLeaf     => DoubleBlackLeaf
        case Node(c, a, k, v, b) =>
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
    pub def parCount(n: Int32, f: (k, v) -> Bool, t: RedBlackTree[k, v]): Int32 & Impure =
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

    ///
    /// Applies `f` over the tree `t` sequentially from left to right and returns the number of elements
    /// that satisfy the predicate `f`.
    ///
    def seqCount(f: (k, v) -> Bool, t: RedBlackTree[k, v]): Int32 = match t {
        case Leaf                => 0
        case DoubleBlackLeaf     => 0
        case Node(_, a, k, v, b) =>
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
    pub def sumWith(f: (k, v) -> Int32 & ef, t: RedBlackTree[k, v]): Int32 & ef = match t {
        case Leaf                => 0
        case DoubleBlackLeaf     => 0
        case Node(_, a, k, v, b) =>
            let x = sumWith(f, a);
            let y = f(k, v);
            let z = sumWith(f, b);
            x + y + z
    }

    ///
    /// Returns the sum of all key-value pairs `k => v` in the tree `t`
    /// according to the function `f`.
    ///
    /// The implementation spawns `n` threads each applying `f` sequentially
    /// from left to right on some subtree that is disjoint from the rest of
    /// the threads.
    ///
    @Parallel
    pub def parSumWith(n: Int32, f: (k, v) -> Int32, t: RedBlackTree[k, v]): Int32 & Impure =
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

    ///
    /// Returns the sum of all key-value pairs `k => v` in the tree `t`
    /// according to the function `f`.
    ///
    /// Note that this is equivalent to `sumWith` but for performance reasons -- to avoid
    /// megamorphic calls -- we use a copy here.
    ///
    def seqSumWith(f: (k, v) -> Int32, t: RedBlackTree[k, v]): Int32 = match t {
        case Leaf                => 0
        case DoubleBlackLeaf     => 0
        case Node(_, a, k, v, b) =>
            let x = seqSumWith(f, a);
            let y = f(k, v);
            let z = seqSumWith(f, b);
            x + y + z
    }

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
        RedBlackTree.foldRight((k, v, acc) -> (k, v) :: acc, Nil, t)

    ///
    /// Returns the tree `t` as a MutDeque. Elements are ordered from smallest (left) to largest (right).
    ///
    pub def toMutDeque(r: Region[r], t: RedBlackTree[k, v]): MutDeque[(k, v), r] \ Write(r) =
        let d = new MutDeque(r);
        RedBlackTree.foreach((k, v) -> let x = (k, v); MutDeque.pushBack(x, d), t);
        d

    ///
    /// Build a node applicatively.
    ///
    /// This is a helper function for `mapAWithKeyHelper`.
    ///
    def nodeA(color: Color, left: m[RedBlackTree[k, v]], k: k, value: m[v], right: m[RedBlackTree[k, v]]): m[RedBlackTree[k, v]] with Applicative[m] =
        ((((l, v, r) -> Node(color, l, k, v, r)) `Functor.map` left) `Applicative.ap` value) `Applicative.ap` right

    ///
    /// Returns a RedBlackTree with mappings `k => f(v)` for every `k => v` in `t`.
    ///
    pub def mapAWithKey(f: (k, v1) -> m[v2] & ef, t: RedBlackTree[k, v1]): m[RedBlackTree[k, v2]] & ef with Applicative[m] =
        def loop(tt, k) = match tt {
            case Node(color, left, key, v, right) => loop(left, kl -> loop(right, kr -> k(nodeA(color, kl, key, f(key, v), kr))))
            case _                                => k(Applicative.point(Leaf))
        };
        loop(t, ks -> ks as & ef)

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
    def parLimitBy(n: Int32, cmp: (k, v, k, v) -> (k, v), t: RedBlackTree[k, v]): Option[(k, v)] & Impure =
        if (n <= 0)
            seqLimitBy(cmp, t)
        else
            match t {
                case Leaf                => None
                case DoubleBlackLeaf     => None
                case Node(_, a, k, v, b) =>
                    let chanL = chan Option[(k, v)] 1;
                    let chanR = chan Option[(k, v)] 1;
                    spawn (chanL <- parLimitBy((n - 2) / 2, cmp, a));
                    spawn (chanR <- parLimitBy((n - 2) / 2, cmp, b));
                    let a1 = <- chanL;
                    let res = match a1 {
                        case None         => (k, v)
                        case Some(kl, vl) => cmp(kl, vl, k, v)
                    };
                    let b1 = <- chanR;
                    match b1 {
                        case None         => Some(res)
                        case Some(kr, vr) =>
                            let (ks, vs) = res;
                            Some(cmp(ks, vs, kr, vr))
                    }
            }

    ///
    /// Applies `cmp` over the tree `t` sequentially and optionally returns the lowest
    /// element according to `cmp`.
    ///
    def seqLimitBy(cmp: (k, v, k, v) -> (k, v) & ef, t: RedBlackTree[k, v]): Option[(k, v)] & ef = match t {
        case Node(_, a, k, v, b) =>
            let res = match seqLimitBy(cmp, a) {
                case None         => (k, v)
                case Some(kl, vl) => cmp(kl, vl, k, v)
            };
            match seqLimitBy(cmp, b) {
                case None         => Some(res)
                case Some(kr, vr) =>
                    let (ks, vs) = res;
                    Some(cmp(ks, vs, kr, vr))
            }
        case _ => None
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
    pub def joinWith(f: (k, v) -> String & ef, sep: String, t: RedBlackTree[k, v]): String & ef = region r {
        use StringBuilder.append!;
        let lastSep = String.length(sep);
        let sb = new StringBuilder(r);
        foreach((k, v) -> { append!(f(k, v), sb); append!(sep, sb) }, t);
        StringBuilder.toString(sb) |> String.dropRight(lastSep)
    }

    ///
    /// Returns a new copy of tree `t` with just the nodes that satisfy the predicate `f`.
    ///
    pub def filter(f: v -> Bool & ef, t: RedBlackTree[k, v]): RedBlackTree[k, v] & ef with Order[k] =
        filterImplementation(f, t)

    ///
    /// Temporary hack!
    /// A concrete implementation of `filter` that the `FunctorFilter` instance can use.
    /// If `FunctorFilter.filter` calls `filter` it appears to invoke a cycle and the concrete
    /// implementation is never called.
    ///
    def filterImplementation(f: v -> Bool & ef, t: RedBlackTree[k, v]): RedBlackTree[k, v] & ef with Order[k] =
        foldLeft((acc, k, v) -> if (f(v)) insert(k, v, acc) else acc, empty(), t)

    ///
    /// Collects the results of applying the partial function `f` to every element in `t`.
    /// This traverses tree `t` and produces a new tree with just nodes where applying f
    /// produces `Some(_)`.
    ///
    pub def filterMap(f: a -> Option[b] & ef, t: RedBlackTree[k, a]): RedBlackTree[k, b] & ef with Order[k] =
        filterMapImplementation(f, t)

    ///
    /// Temporary hack!
    /// A concrete implementation of `filterMap` that the `FunctorFilter` instance can use.
    ///
    def filterMapImplementation(f: a -> Option[b] & ef, t: RedBlackTree[k, a]): RedBlackTree[k, b] & ef with Order[k] =
        let step = (acc, k, a) -> match f(a) {
            case Some(b) => insert(k, b, acc)
            case None    => acc
        };
        foldLeft(step, empty(), t)
}
