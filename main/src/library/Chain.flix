/*
 * Copyright 2021 Stephen Tetley
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

///
/// The Chain type.
///
/// A chain is a list represented as an unbalanced binary tree.
/// It supports efficient append and "snoc" - appending elements at the tail
/// of the list.
///
/// Note - the constructors `Empty`, `One` and `Chain` should not be used directly.
///
pub enum Chain[t] {
    case Empty
    case One(t)
    case Chain(Chain[t], Chain[t])
}

instance Eq[Chain[a]] with Eq[a] {
    pub def eq(c1: Chain[a], c2: Chain[a]): Bool = Chain.equals(c1, c2)
}

instance SemiGroup[Chain[a]] {
    pub def combine(c1: Chain[a], c2: Chain[a]): Chain[a] = Chain.append(c1, c2)
}

instance Monoid[Chain[a]] {
    pub def empty(): Chain[a] = Chain.empty()
}

instance Functor[Chain] {
    pub def map(f: a -> b & ef, c: Chain[a]): Chain[b] & ef = Chain.map(f, c)
}

instance Applicative[Chain] {
    pub def point(x: a) : Chain[a] = Chain.singleton(x)
    pub def ap(f: Chain[a -> b & ef], x: Chain[a]) : Chain[b] & ef = Chain.ap(f, x)
}

instance Monad[Chain] {
    pub def flatMap(f: a -> Chain[b] & ef, x : Chain[a]) : Chain[b] & ef = Chain.flatMap(f, x)
}

instance Foldable[Chain] {
    pub def foldLeft(f: (b, a) -> b & ef, s: b, c: Chain[a]): b & ef = Chain.foldLeft(f, s, c)
    pub def foldRight(f: (a, b) -> b & ef, s: b, c: Chain[a]): b & ef = Chain.foldRight(f, s, c)
}

instance Traversable[Chain] {
    pub def traverse(f: a -> m[b] & ef, t: Chain[a]): m[Chain[b]] & ef with Applicative[m] = Chain.traverse(f, t)
    pub override def sequence(t: Chain[m[a]]): m[Chain[a]] with Applicative[m] = Chain.sequence(t)
}


instance ToString[Chain[a]] with ToString[a] {
    pub def toString(c: Chain[a]): String =
        let fn = () -> {
            let sb = StringBuilder.new();
            StringBuilder.appendString!(sb, "Chain#{");
            Chain.foldLeft((acc, x) -> {
                if (acc < 1)
                    StringBuilder.appendString!(sb, "${x}")
                else
                    StringBuilder.appendString!(sb, ", ${x}");
                acc + 1}, 0, c);
            StringBuilder.appendString!(sb, "}");
            StringBuilder.toString(sb)
        };
        fn() as & Pure
}


namespace Chain {

    ///
    /// A datatype for pattern matching on a chain (traversing left-to-right).
    ///
    pub enum ViewLeft[t] with Eq {
        case NoneLeft
        case SomeLeft(t, Chain[t])
    }

    ///
    /// A datatype for pattern matching on a chain (traversing right-to-left).
    ///
    pub enum ViewRight[t] with Eq {
        case NoneRight
        case SomeRight(Chain[t], t)
    }

    ///
    /// Return the empty chain.
    ///
    pub def empty(): Chain[a] = Empty

    ///
    /// Return the singleton chain with element `x`.
    ///
    pub def singleton(x: a): Chain[a] = One(x)

    ///
    /// Apply every function from `f` to every argument from `x` and return a chain with all results.
    /// For `f = f1, f2, ...` and `x = x1, x2, ...` the results appear in the order
    /// `f1(x1), f1(x2), ..., f2(x1), f2(x2), ...`.
    ///
    pub def ap(f: Chain[a -> b & ef], x: Chain[a]) : Chain[b] & ef =
        apHelper(viewLeft(f), x, empty())

    ///
    /// Helper function for `ap`.
    ///
    def apHelper(f: ViewLeft[a -> b & ef], c: Chain[a], acc: Chain[b]): Chain[b] & ef = match f {
        case NoneLeft         => acc
        case SomeLeft(f1, rs) => apHelper(viewLeft(rs), c, acc `append` map(f1, c))
    }

    ///
    /// Returns true if and only if `c` is the empty chain.
    ///
    pub def isEmpty(c: Chain[a]): Bool = match c {
        case Empty => true
        case _     => false
    }

    ///
    /// Add element `x` to the left end of Chain `c`.
    ///
    pub def cons(x: a, c: Chain[a]): Chain[a] = match c {
        case Empty => One(x)
        case _     => Chain(One(x), c)
    }

    ///
    /// Add element `x` to the right end of chain `c`.
    ///
    pub def snoc(c: Chain[a], x: a): Chain[a] = match c {
        case Empty => One(x)
        case _     => Chain(c, One(x))
    }

    ///
    /// Returns `Some(x)` if `x` is the first element of `c`.
    ///
    /// Returns `None` if `c` is empty.
    ///
    pub def head(c: Chain[a]): Option[a] = match viewLeft(c) {
        case SomeLeft(x, _) => Some(x)
        case _              => None
    }

    ///
    /// Returns `Some(x)` if `x` is the last element of `c`.
    ///
    /// Returns `None` if `c` is empty.
    ///
    pub def last(c: Chain[a]): Option[a] = match viewRight(c) {
        case SomeRight(_, x) => Some(x)
        case _               => None
    }

    ///
    /// Returns the subchain of `c` without the last element.
    /// Returns `None` if the chain `c` is empty.
    ///
    pub def init(c: Chain[a]): Option[Chain[a]] = match viewRight(c) {
        case SomeRight(rs, _) => Some(rs)
        case _                => None
    }

    ///
    /// Returns the length of `c`.
    ///
    pub def length(c: Chain[a]): Int32 = foldRight((_, acc) -> acc + 1, 0, c)

    ///
    /// Returns a new chain formed by appending the chains `c1` and `c2`.
    ///
    pub def append(c1: Chain[a], c2: Chain[a]): Chain[a] = match (c1, c2) {
        case (Empty, c) => c
        case (c, Empty) => c
        case _          => Chain(c1, c2)
    }

    ///
    /// Deconstruct a Chain from left-to-right.
    ///
    /// Returns `ViewLeft(x, rs)` if the chain is non-empty, where `x` is the leftmost
    /// element of the chain `c`, and `rs` is the rest of the chain.
    ///
    /// Returns `NoneLeft` if the chain is empty.
    ///
    pub def viewLeft(c: Chain[a]): ViewLeft[a] = viewLeftHelper(c, Empty, x -> x)

    ///
    /// Helper function for `viewLeft`.
    ///
    /// This traverses the chain from left-to-right, because the chain is represented
    /// by a binary tree, the function may have recursively descend the tree to find
    /// the leftmost node.
    ///
    /// Precondition: `c` has been properly formed with no internal `Empty` nodes.
    /// This means that `Empty` can only be encountered on the first match and it
    /// can ignore the accumulator which is also `Empty` the first time this is called.
    ///
    def viewLeftHelper(c: Chain[a], acc: Chain[a], k: ViewLeft[a] -> ViewLeft[a]): ViewLeft[a] = match c {
        case Empty       => k(NoneLeft)
        case One(x)      => k(SomeLeft(x, acc))
        case Chain(l, r) => viewLeftHelper(l, append(r, acc),  k)
    }

    ///
    /// Deconstruct a Chain from right-to-left.
    ///
    /// Returns `ViewRight(rs, x)` if the chain is non-empty, where `x` is the rightmost
    /// element of the chain `c``, and `rs` is the front of the chain.
    ///
    /// Returns `NoneRight` if the chain is empty.
    ///
    pub def viewRight(c: Chain[a]): ViewRight[a] = viewRightHelper(c, Empty, x -> x)

    ///
    /// Helper function for `viewRight`.
    ///
    /// This traverses the chain from right-to-left, because the chain is represented
    /// by a binary tree, the function may have recursively descend the tree to find
    /// the rightmost node.
    ///
    /// Precondition: `c` has been properly formed with no internal `Empty` nodes.
    /// This means that `Empty` can only be encountered on the first match and it
    /// can ignore the accumulator which is also `Empty` the first time this is called.
    ///
    def viewRightHelper(c: Chain[a], acc: Chain[a], k: ViewRight[a] -> ViewRight[a]): ViewRight[a] = match c {
        case Empty       => k(NoneRight)
        case One(x)      => k(SomeRight(acc, x))
        case Chain(l, r) => viewRightHelper(r, append(acc, l),  k)
    }

    ///
    /// Returns `true` if and only if `c` contains the element `a`.
    ///
    pub def memberOf(a: a, c: Chain[a]): Bool with Eq[a] = memberOfHelper(a, viewLeft(c))

    ///
    /// Helper function for `memberOf`.
    ///
    def memberOfHelper(a: a, v: ViewLeft[a]): Bool with Eq[a] = match v {
        case NoneLeft                 => false
        case SomeLeft(x, _) if x == a => true
        case SomeLeft(_, c)           => memberOfHelper(a, viewLeft(c))
    }

    ///
    /// Returns the position of `a` in `c`.
    ///
    /// Returns `-1` if `a` does not exist in `c`.
    ///
    pub def indexOf(a: a, c: Chain[a]): Int32 with Eq[a] = indexOfHelper(a, 0, viewLeft(c))

    ///
    /// Helper function for `indexOf`.
    ///
    def indexOfHelper(a: a, acc: Int32, v: ViewLeft[a]): Int32 with Eq[a] = match v {
        case NoneLeft       => -1
        case SomeLeft(x, c) => if (x == a) acc else indexOfHelper(a, acc + 1, viewLeft(c))
    }

    ///
    /// Alias for `findLeft`.
    ///
    /// The function `f` must be pure.
    ///
    pub def find(f: a -> Bool, c: Chain[a]): Option[a] = findLeft(f, c)

    ///
    /// Optionally returns the first element of `c` that satisfies the predicate `f` when searching from left to right.
    ///
    /// The function `f` must be pure.
    ///
    pub def findLeft(f: a -> Bool, c: Chain[a]): Option[a] = match viewLeft(c) {
        case NoneLeft        => None
        case SomeLeft(x, rs) => if (f(x)) Some(x) else findLeft(f, rs)
    }

    ///
    /// Optionally returns the first element of `c` that satisfies the predicate `f` when searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    pub def findRight(f: a -> Bool, c: Chain[a]): Option[a] = match viewRight(c) {
        case NoneRight        => None
        case SomeRight(rs, x) => if (f(x)) Some(x) else findRight(f, rs)
    }

    ///
    /// Returns a list of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns `Nil` if `b >= e`.
    ///
    pub def range(b: Int32, e: Int32): Chain[Int32] = rangeHelper(b, e, Chain.empty())

    ///
    /// Helper function for `range`.
    ///
    def rangeHelper(b: Int32, e: Int32, acc: Chain[Int32]): Chain[Int32] =
        if (b >= e) acc else rangeHelper(b + 1, e, snoc(acc, b))

    ///
    /// Returns a list with the element `a` repeated `n` times.
    ///
    /// Returns `Nil` if `n < 0`.
    ///
    pub def repeat(n: Int32, a: a): Chain[a] = repeatHelper(n, a, Empty)

    ///
    /// Helper function for `repeat`.
    ///
    def repeatHelper(n: Int32, a: a, acc: Chain[a]): Chain[a] =
        if (n <= 0)
            acc
        else {
            let ac1 = cons(a, acc);
            repeatHelper(n-1, a, ac1)
        }

    ///
    /// Alias for `scanLeft`.
    ///
    pub def scan(f: (b, a) -> b & ef, s: b, c: Chain[a]): Chain[b] & ef = scanLeft(f, s, c)

    ///
    /// Accumulates the result of applying `f` to `c` going left to right.
    ///
    /// That is, the result is of the form: `s :: f(s, x1) :: f(f(s, x1), x2)  ...`.
    ///
    pub def scanLeft(f: (b, a) -> b & ef, s: b, c: Chain[a]): Chain[b] & ef = scanLeftHelper(f, s, c, singleton(s))

    ///
    /// Helper function for `scanLeft`.
    ///
    def scanLeftHelper(f: (b, a) -> b & ef, s: b, c: Chain[a], acc: Chain[b]): Chain[b] & ef = match viewLeft(c) {
        case NoneLeft        => acc
        case SomeLeft(x, rs) => {let a = f(s, x); scanLeftHelper(f, a, rs, snoc(acc, a))}
    }

    ///
    /// Accumulates the result of applying `f` to `c` going right to left.
    ///
    /// That is, the result is of the form: `... f(xn-1, f(xn, s)) :: f(xn, s) :: s`.
    ///
    pub def scanRight(f: (a, b) -> b & ef, s: b, c: Chain[a]): Chain[b] & ef = scanRightHelper(f, s, c, singleton(s))

    ///
    /// Helper function for `scanRight`.
    ///
    def scanRightHelper(f: (a, b) -> b & ef, s: b, c: Chain[a], acc: Chain[b]): Chain[b] & ef = match viewRight(c) {
        case NoneRight        => acc
        case SomeRight(rs, x) => {let a = f(x, s); scanRightHelper(f, a, rs, cons(a, acc))}
    }

    ///
    /// Returns the result of applying `f` to every element in `c`.
    ///
    /// That is, the result is of the form: `f(x1) :: f(x2) :: ...`.
    ///
    pub def map(f: a -> b & ef, c: Chain[a]): Chain[b] & ef =
        mapHelper(f, c, Empty)

    ///
    /// Helper function for `map`.
    ///
    def mapHelper(f: a -> b & ef, c: Chain[a], acc: Chain[b]): Chain[b] & ef = match viewLeft(c) {
        case NoneLeft        => acc
        case SomeLeft(a, rs) => mapHelper(f, rs, snoc(acc, f(a)))
    }

    ///
    /// Returns the result of applying `f` to every element in `c` along with that element's index.
    ///
    /// That is, the result is of the form: `f(x1, 0) :: f(x2, 1) :: ...`.
    ///
    pub def mapWithIndex(f: (a, Int32) -> b & ef, c: Chain[a]): Chain[b] & ef =
        mapWithIndexHelper(f, c, 0, Empty)

    ///
    /// Helper function for `mapWithIndex`.
    ///
    def mapWithIndexHelper(f: (a, Int32) -> b & ef, c: Chain[a], i: Int32, acc: Chain[b]): Chain[b] & ef = match viewLeft(c) {
        case NoneLeft        => acc
        case SomeLeft(x, rs) => {let a = f(x, i); mapWithIndexHelper(f, rs, i+1, snoc(acc, a))}
    }

    ///
    /// Returns the result of applying `f` to every element in `c` and concatenating the results.
    ///
    pub def flatMap(f: a -> Chain[b] & ef, c: Chain[a]): Chain[b] & ef = flatMapHelper(f, c, Empty)

    ///
    /// Helper function for `flatMap`.
    ///
    def flatMapHelper(f: a -> Chain[b] & ef, c: Chain[a], acc: Chain[b]): Chain[b] & ef = match viewLeft(c) {
        case NoneLeft        => acc
        case SomeLeft(x, rs) => flatMapHelper(f, rs, append(acc, f(x)))
    }

    ///
    /// Returns the reverse of `c`.
    ///
    pub def reverse(c: Chain[a]): Chain[a] = reverseHelper(c, Empty)

    ///
    /// Helper function for `reverse`.
    ///
    def reverseHelper(c: Chain[a], acc: Chain[a]): Chain[a] = match viewLeft(c) {
        case NoneLeft        => acc
        case SomeLeft(x, rs) => reverseHelper(rs, cons(x, acc))
    }

    ///
    /// Returns `c` with `a` inserted between every two adjacent elements.
    ///
    pub def intersperse(a: a, c: Chain[a]): Chain[a] = match viewLeft(c) {
        case NoneLeft        => Empty
        case SomeLeft(x, rs) => intersperseHelper(a, rs, singleton(x))
    }

    ///
    /// Helper function for `reverse`.
    ///
    def intersperseHelper(sep: a, c: Chain[a], acc: Chain[a]): Chain[a] = match viewLeft(c) {
        case NoneLeft        => acc
        case SomeLeft(x, rs) => intersperseHelper(sep, rs, acc `snoc` sep `snoc` x)
    }

    ///
    /// Returns the number of elements in `c` that satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def count(f: a -> Bool, c: Chain[a]): Int32 = countHelper(f, c, 0)

    ///
    /// Helper function for `count`.
    ///
    def countHelper(f: a -> Bool, c: Chain[a], acc: Int32): Int32 = match viewLeft(c) {
        case NoneLeft        => acc
        case SomeLeft(x, rs) => if (f(x)) countHelper(f, rs, acc + 1) else countHelper(f, rs, acc)
    }

    ///
    /// Returns the concatenation of the elements in `c`.
    ///
    pub def flatten(c: Chain[Chain[a]]): Chain[a] =
        foldLeft(append, empty(), c)

    ///
    /// Returns `true` if and only if at least one element in `c` satisfies the predicate `f`.
    ///
    /// Returns `false` if `c` is empty.
    ///
    /// The function `f` must be pure.
    ///
    pub def exists(f: a -> Bool, c: Chain[a]): Bool = match viewLeft(c) {
        case NoneLeft        => false
        case SomeLeft(x, rs) => if (f(x)) true else exists(f, rs)
    }

    ///
    /// Returns `true` if and only if all elements in `c` satisfy the predicate `f`.
    ///
    /// Returns `true` if `c` is empty.
    ///
    /// The function `f` must be pure.
    ///
    pub def forall(f: a -> Bool, c: Chain[a]): Bool = match viewLeft(c) {
        case NoneLeft        => true
        case SomeLeft(x, rs) => if (not f(x)) false else forall(f, rs)
    }

    ///
    /// Returns a list of every element in `c` that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def filter(f: a -> Bool & ef, c: Chain[a]): Chain[a] & ef = filterHelper(f, c, Empty)

    ///
    /// Helper function for `filter`.
    ///
    def filterHelper(f: a -> Bool & ef, c: Chain[a], acc: Chain[a]): Chain[a] & ef = match viewLeft(c) {
        case NoneLeft        => acc
        case SomeLeft(x, rs) => if (f(x)) filterHelper(f, rs, snoc(acc, x)) else filterHelper(f, rs, acc)
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `c` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b & ef, acc: b, c: Chain[a]): b & ef = match viewLeft(c) {
        case NoneLeft        => acc
        case SomeLeft(x, rs) => {
            let b = f(acc, x);
            foldLeft(f, b, rs)
        }
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `c` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b & ef, acc: b, c: Chain[a]): b & ef = match viewRight(c) {
        case NoneRight        => acc
        case SomeRight(rs, x) => {
            let b = f(x, acc);
            foldRight(f, b, rs)
        }
    }

    ///
    /// Returns `c` without the first `n` elements.
    ///
    /// Returns `Nil` if `n > length(c)`.
    /// Returns `c` if `n < 0`.
    ///
    pub def dropLeft(n: Int32, c: Chain[a]): Chain[a] =
        if (n < 0)
            c
        else match (n, viewLeft(c)) {
            case (_, NoneLeft)        => Empty
            case (0, _)               => c
            case (i, SomeLeft(_, rs)) => dropLeft(i - 1, rs)
        }

    ///
    /// Returns `c` without the last `n` elements.
    ///
    /// Returns `Nil` if `n > length(c)`.
    /// Returns `c` if `n < 0`.
    ///
    pub def dropRight(n: Int32, c: Chain[a]): Chain[a] =
        if (n < 0)
            c
        else match (n, viewRight(c)) {
            case (_, NoneRight)        => Empty
            case (0, _)                => c
            case (i, SomeRight(rs, _)) => dropRight(i - 1, rs)
        }

    ///
    /// Returns `c` without the longest prefix that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def dropWhileLeft(f: a -> Bool, c: Chain[a]): Chain[a] = match viewLeft(c) {
        case SomeLeft(x, rs) if (f(x)) => dropWhileLeft(f, rs)
        case _                         => c
    }

    ///
    /// Returns `c` without the longest suffix that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def dropWhileRight(f: a -> Bool, c: Chain[a]): Chain[a] = match viewRight(c) {
        case SomeRight(rs, x) if (f(x)) => dropWhileRight(f, rs)
        case _                          => c
    }

    ///
    /// Returns the first `n` elements of `c`.
    ///
    /// Returns `c` if `n > length(c)`.
    /// Returns `Nil` if `n < 0`.
    ///
    pub def takeLeft(n: Int32, c: Chain[a]): Chain[a] =
        if (n < 0)
            Empty
        else
            takeLeftHelper(n, c, Empty)

    ///
    /// Helper for function for `takeLeft`.
    ///
    /// Precondition n >= 0
    ///
    def takeLeftHelper(n: Int32, c: Chain[a], acc: Chain[a]): Chain[a] = match (n, viewLeft(c)) {
        case (_, NoneLeft)        => acc
        case (i, _) if i < 1      => acc
        case (i, SomeLeft(x, rs)) => takeLeftHelper(i-1, rs, snoc(acc, x))
    }

    ///
    /// Returns the last `n` elements of `c`.
    ///
    /// Returns `c` if `n > length(c)`.
    /// Returns `Nil` if `n < 0`.
    ///
    pub def takeRight(n: Int32, c: Chain[a]): Chain[a] =
        if (n < 0)
            Empty
        else
            takeRightHelper(n, c, Empty)

    ///
    /// Helper for function for `takeRight`.
    ///
    /// Precodition: n >= 0
    ///
    def takeRightHelper(n: Int32, c: Chain[a], acc: Chain[a]): Chain[a] = match (n, viewRight(c)) {
        case (_, NoneRight)        => acc
        case (i, _)  if i < 1      => acc
        case (i, SomeRight(rs, x)) => takeRightHelper(i-1, rs, cons(x, acc))
    }

    ///
    /// Returns the longest prefix of `c` that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def takeWhileLeft(f: a -> Bool, c: Chain[a]): Chain[a] = takeWhileLeftHelper(f, c, Empty)

    ///
    /// Helper for function for `takeWhileLeft`.
    ///
    def takeWhileLeftHelper(f: a -> Bool, c: Chain[a], acc: Chain[a]): Chain[a] = match viewLeft(c) {
        case NoneLeft        => acc
        case SomeLeft(x, rs) => if (f(x)) takeWhileLeftHelper(f, rs, snoc(acc, x)) else acc
    }

    ///
    /// Returns the longest suffix of `c` that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def takeWhileRight(f: a -> Bool, c: Chain[a]): Chain[a] = takeWhileRightHelper(f, c, Empty)

    ///
    /// Helper for function for `takeWhileRight`.
    ///
    def takeWhileRightHelper(f: a -> Bool, c: Chain[a], acc: Chain[a]): Chain[a] = match viewRight(c) {
        case SomeRight(rs, x) if (f(x)) => takeWhileRightHelper(f, rs, cons(x, acc))
        case _                          => acc
    }

    ///
    /// Collects the results of applying the partial function `f` to every element in `c`.
    ///
    pub def filterMap(f: a -> Option[b] & ef, c: Chain[a]): Chain[b] & ef =
        let step = (acc, x) -> match f(x) {
            case None    => acc
            case Some(v) => snoc(acc, v)
        };
        foldLeft(step, empty(), c)

    ///
    /// Returns the first non-None result of applying the partial function `f` to each element of `c`.
    ///
    /// Returns `None` if every element of `c` is `None`.
    ///
    pub def findMap(f: a -> Option[b] & ef, c: Chain[a]): Option[b] & ef = match viewLeft(c) {
        case NoneLeft        => None
        case SomeLeft(x, rs) => match f(x) {
            case None    => findMap(f, rs)
            case Some(v) => Some(v)
        }
    }


    ///
    /// Returns a chain where the element at index `i` is `(a, b)` where
    /// `a` is the element at index `i` in `c1` and `b` is the element at index `i` in `c2`.
    ///
    /// If either `c1` or `c2` becomes depleted, then no further elements are added to the resulting chain.
    ///
    pub def zip(c1: Chain[a], c2: Chain[b]): Chain[(a,b)] = zipHelper(c1, c2, Chain.empty())

    ///
    /// Helper function for `zip`.
    ///
    def zipHelper(c1: Chain[a], c2: Chain[b], acc: Chain[(a,b)]): Chain[(a,b)] = match (viewLeft(c1), viewLeft(c2)) {
        case (SomeLeft(x, rs), SomeLeft(y, qs)) => zipHelper(rs, qs, snoc(acc, (x, y)))
        case _                                  => acc
    }

    ///
    /// Returns a chain where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `c1` and `b` is the element at index `i` in `c2`.
    ///
    /// If either `c1` or `c2` becomes depleted, then no further elements are added to the resulting chain.
    ///
    pub def zipWith(f: (a, b) -> c & ef, c1: Chain[a], c2: Chain[b]): Chain[c] & ef = zipWithHelper(f, c1, c2, Chain.empty())

    ///
    /// Helper function for `zipWith`.
    ///
    def zipWithHelper(f: (a, b) -> c & ef, c1: Chain[a], c2: Chain[b], acc: Chain[c]): Chain[c] & ef = match (viewLeft(c1), viewLeft(c2)) {
        case (SomeLeft(x, rs), SomeLeft(y, qs)) => {
            let a = f(x, y);
            zipWithHelper(f, rs, qs, snoc(acc, a))
        }
        case _                                  => acc
    }

    ///
    /// Returns a pair of chains, the first containing all first components in `c`
    /// and the second containing all second components in `c`.
    ///
    pub def unzip(c: Chain[(a, b)]): (Chain[a], Chain[b]) = unzipHelper(c, Chain.empty(), Chain.empty())

    ///
    /// Helper function for `unzip`.
    ///
    def unzipHelper(c: Chain[(a, b)], ac1: Chain[a], ac2: Chain[b]): (Chain[a], Chain[b]) = match viewLeft(c) {
        case NoneLeft               => (ac1, ac2)
        case SomeLeft((x1, x2), rs) => unzipHelper(rs, snoc(ac1, x1), snoc(ac2, x2))
    }

    ///
    /// `mapAccumLeft` is a stateful version of `map`. The accumulating paramter `s` is updated at each
    /// step in a left-to-right traversal.
    ///
    pub def mapAccumLeft(f: (s, a) -> (s, b) & ef, start: s, c: Chain[a]): (s, Chain[b]) & ef =
        mapAccumLeftHelper(f, start, c, (s1, ks) -> (s1, ks))

    ///
    /// Helper function for `mapAccumLeft`.
    ///
    def mapAccumLeftHelper(f: (s, a) -> (s, b) & ef, s1: s, c: Chain[a], k: (s, Chain[b]) -> (s, Chain[b])): (s, Chain[b]) & ef =
        match viewLeft(c) {
            case NoneLeft        => k(s1, empty())
            case SomeLeft(x, rs) => {
                let (s2, x1) = f(s1, x);
                mapAccumLeftHelper(f, s2, rs, (s3, ks) -> k(s3, cons(x1, ks)))
            }
        }

    ///
    /// `mapAccumRight` is a stateful version of `map`. The accumulating parameter `s` is updated at each
    /// step in a right-to-left traversal.
    ///
    pub def mapAccumRight(f: (s, a) -> (s, b) & ef, start: s, c: Chain[a]): (s, Chain[b]) & ef =
        mapAccumRightHelper(f, start, c, (s1, ks) -> (s1, ks))

    ///
    /// Helper function for `mapAccumRight`.
    ///
    def mapAccumRightHelper(f: (s, a) -> (s, b) & ef, s1: s, c: Chain[a], k: (s, Chain[b]) -> (s, Chain[b])): (s, Chain[b]) & ef =
        match viewRight(c) {
            case NoneRight        => k(s1, empty())
            case SomeRight(rs, x) => {
                let (s2, x1) = f(s1, x);
                mapAccumRightHelper(f, s2, rs, (s3, ks) -> k(s3, snoc(ks, x1)))
           }
        }

    ///
    /// Applies `f` to every element of `c`.
    ///
    pub def foreach(f: a -> Unit & ef, c: Chain[a]): Unit & ef = match viewLeft(c) {
        case NoneLeft        => ()
        case SomeLeft(x, rs) => f(x); foreach(f, rs)
    }

    ///
    /// Returns `c` as a list.
    ///
    pub def toList(c: Chain[a]): List[a] =
        foldRight((x, acc) -> x :: acc, Nil, c)

    ///
    /// Returns `c` as a mutable list.
    ///
    pub def toMutList(c: Chain[a]): MutList[a] & Impure =
        MutList(ref toArray(c), ref length(c))

    ///
    /// Returns the list `c` as a set.
    ///
    pub def toSet(c: Chain[a]): Set[a] with Order[a] = foldRight(Set.insert, Set.empty(), c)


    ///
    /// Returns the chain of pairs `c` that represents an association list as a map.
    ///
    /// If `c` contains multiple mappings with the same key, `toMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    pub def toMap(c: Chain[(a, b)]): Map[a, b] with Order[a] = foldRight((x, m) -> Map.insert(fst(x), snd(x), m), Map.empty(), c)


    ///
    /// Returns the chain `c` as an array.
    ///
    pub def toArray(c: Chain[a]) : Array[a] & Impure = match head(c) {
        case None => []
        case Some(x) =>
            let arr = Array.new(x, length(c));
            let f = (i, b) -> { arr[i] = b; i + 1 };
            foldLeft(f, 0, c);
            arr
        }

    ///
    /// Returns `true` if and only if `c1` and `c2` and equal.
    ///
    pub def equals(c1: Chain[a], c2: Chain[a]): Bool with Eq[a] = equalsHelper(viewLeft(c1), viewLeft(c2))

    ///
    /// Helper function for `equals`.
    ///
    /// Note: Chains are considered equal if their (ordered) list of elements are equal.
    ///
    /// Because they may have different shapes due to construction we use a view to
    /// decide equality which imposes an order on the Chain.
    ///
    def equalsHelper(v1: ViewLeft[a], v2: ViewLeft[a]): Bool with Eq[a] = match (v1, v2) {
        case (NoneLeft, NoneLeft)                           => true
        case (SomeLeft(x, cx), SomeLeft(y, cy)) if (x == y) => equalsHelper(viewLeft(cx), viewLeft(cy))
        case _                                              => false
    }

    ///
    /// Sort chain `c` so that elements are ordered from low to high according to their `Order` instance.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `c`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sort(c: Chain[a]): Chain[a] with Order[a] =
        (c |> toArray |> Array.sort |> Array.toChain) as & Pure

    /// Sort chain `c` so that elements are ordered from low to high according to the `Order` instance
    /// for the values obtained by applying `f` to each element.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `c`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortBy(f: a -> b, c: Chain[a]): Chain[a] with Order[b] =
        (c |> toArray |> Array.sortBy(f) |> Array.toChain) as & Pure

    ///
    /// Sort chain `c` so that elements are ordered from low to high according to the comparison function `cmp`.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `c`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortWith(cmp: (a,a) -> Comparison, c: Chain[a]): Chain[a] =
        (c |> toArray |> Array.sortWith(cmp) |> Array.toChain) as & Pure

    ///
    /// Helper function for `traverse` and `sequence`.
    ///
    /// Builds an "applicative chain" from an applicative chain of the front of the chain
    /// "snocing" a last element of one applictive action.
    ///
    def snocA(mxs: f[Chain[a]], mx: f[a]): f[Chain[a]] with Applicative[f] =
        (((xs, x) -> snoc(xs, x)) `Functor.map` mxs) `Applicative.ap` mx

    ///
    /// Returns the result of running all the actions in the chain `c`.
    ///
    pub def sequence(c: Chain[m[a]]): m[Chain[a]] with Applicative[m] =
        def loop(v, acc) = match v {
            case NoneLeft        => acc
            case SomeLeft(x, rs) => loop(viewLeft(rs), snocA(acc, x))
        };
        loop(viewLeft(c), Applicative.point(empty()))

    ///
    /// Returns the result of applying the applicative mapping function `f` to all the elements of the
    /// chain `c`.
    ///
    pub def traverse(f: a -> m[b] & ef, c: Chain[a]): m[Chain[b]] & ef with Applicative[m] =
        def loop(v, acc) = match v {
            case NoneLeft        => acc as & ef
            case SomeLeft(x, rs) => loop(viewLeft(rs), snocA(acc, f(x)))
        };
        loop(viewLeft(c), Applicative.point(empty()))

}
