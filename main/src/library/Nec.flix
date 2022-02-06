/*
 * Copyright 2022 Stephen Tetley
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
/// The NonEmpty Chain type.
///
/// A chain is a list represented as an unbalanced binary tree.
/// It supports efficient append and "snoc" - appending elements at the tail
/// of the list.
///
/// Note - the constructors `NecOne` and `Nec` should not be used directly.
///
pub enum Nec[t] {
    case NecOne(t)
    case Nec(Nec[t], Nec[t])
}

instance Eq[Nec[a]] with Eq[a] {
    pub def eq(c1: Nec[a], c2: Nec[a]): Bool = Nec.equals(c1, c2)
}

instance Hash[Nec[a]] with Hash[a] {
    pub def hash(c: Nec[a]): Int32 = 39119 + Hash.hash(Nec.toList(c))
}

instance SemiGroup[Nec[a]] {
    pub def combine(c1: Nec[a], c2: Nec[a]): Nec[a] = Nec.append(c1, c2)
}

instance Functor[Nec] {
    pub def map(f: a -> b & ef, c: Nec[a]): Nec[b] & ef = Nec.map(f, c)
}

instance Applicative[Nec] {
    pub def point(x: a) : Nec[a] = Nec.singleton(x)
    pub def ap(f: Nec[a -> b & ef], x: Nec[a]) : Nec[b] & ef = Nec.ap(f, x)
}

instance Monad[Nec] {
    pub def flatMap(f: a -> Nec[b] & ef, x : Nec[a]) : Nec[b] & ef = Nec.flatMap(f, x)
}

instance Foldable[Nec] {
    pub def foldLeft(f: (b, a) -> b & ef, s: b, c: Nec[a]): b & ef = Nec.foldLeft(f, s, c)
    pub def foldRight(f: (a, b) -> b & ef, s: b, c: Nec[a]): b & ef = Nec.foldRight(f, s, c)
}

instance Traversable[Nec] {
    pub def traverse(f: a -> m[b] & ef, t: Nec[a]): m[Nec[b]] & ef with Applicative[m] = Nec.traverse(f, t)
    pub override def sequence(t: Nec[m[a]]): m[Nec[a]] with Applicative[m] = Nec.sequence(t)
}

instance Reducible[Nec] {
    pub def reduceLeftTo(f: (b, a) -> b & ef1, g: a -> b & ef2, l: Nec[a]): b & (ef1 and ef2) = Nec.reduceLeftTo(f, g, l)
    pub def reduceRightTo(f: (a, b) -> b & ef1, g: a -> b & ef2, l: Nec[a]): b & (ef1 and ef2) = Nec.reduceRightTo(f, g, l)
}

instance Iterable[Nec] {
    pub def iterator(t: Nec[a]): Iterator[a] & Impure =
        Nec.toIterator(t)
}

instance ToString[Nec[a]] with ToString[a] {
    pub def toString(c: Nec[a]): String =
        let fn = () -> {
            let sb = StringBuilder.new();
            StringBuilder.appendString!(sb, "Nec#{");
            Nec.foldLeft((acc, x) -> {
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


namespace Nec {

    use Functor.{<$>};
    use Applicative.{<*>};

    ///
    /// A datatype for pattern matching on a Nec (traversing left-to-right).
    ///
    pub enum ViewLeft[a] with Eq {
        case OneLeft(a)
        case SomeLeft(a, Nec[a])
    }

    ///
    /// A datatype for pattern matching on a Nec (traversing right-to-left).
    ///
    pub enum ViewRight[a] with Eq {
        case OneRight(a)
        case SomeRight(Nec[a], a)
    }

    ///
    /// Returns `true` if and only if `c1` and `c2` and equal.
    ///
    pub def equals(c1: Nec[a], c2: Nec[a]): Bool with Eq[a] = equalsHelper(c1, c2)

    ///
    /// Helper function for `equals`.
    ///
    /// Note: Necs are considered equal if their (ordered) list of elements are equal.
    ///
    /// Because they may have different shapes due to construction we use a view to
    /// decide equality which imposes an order on the Nec.
    ///
    def equalsHelper(c1: Nec[a], c2: Nec[a]): Bool with Eq[a] =
        match (viewLeft(c1), viewLeft(c2)) {
            case (OneLeft(x), OneLeft(y))                       => x == y
            case (SomeLeft(x, xs), SomeLeft(y, ys)) if (x == y) => equalsHelper(xs, ys)
            case _                                              => false
        }

    ///
    /// Return the singleton Nec with element `x`.
    ///
    pub def singleton(x: a): Nec[a] = NecOne(x)

    ///
    /// Returns true if and only if `c` is a single element Nec.
    ///
    pub def isSingleton(c: Nec[a]): Bool = match c {
        case NecOne(_) => true
        case _         => false
    }

    ///
    /// Add element `x` to the left end of Nec `c`.
    ///
    pub def cons(x: a, c: Nec[a]): Nec[a] = Nec(NecOne(x), c)

    ///
    /// Add element `x` to the right end of Nec `c`.
    ///
    pub def snoc(c: Nec[a], x: a): Nec[a] = Nec(c, NecOne(x))

    ///
    /// Returns the first element of `c`.
    ///
    pub def head(c: Nec[a]): a = match viewLeft(c) {
        case OneLeft(x)     => x
        case SomeLeft(x, _) => x
    }

    ///
    /// Returns the last element of `c`.
    ///
    pub def last(c: Nec[a]): a = match viewRight(c) {
        case OneRight(x)     => x
        case SomeRight(_, x) => x
    }

    ///
    /// Returns the list of elements in `c` without the last element.
    ///
    pub def init(c: Nec[a]): List[a] = match viewRight(c) {
        case OneRight(_)      => Nil
        case SomeRight(rs, _) => toList(rs)
    }

    ///
    /// Returns all elements in `c` without the first element.
    ///
    pub def tail(c: Nec[a]): List[a] = match viewLeft(c) {
        case OneLeft(_)      => Nil
        case SomeLeft(_, rs) => toList(rs)
    }

    ///
    /// Returns the length of `c`.
    ///
    pub def length(c: Nec[a]): Int32 = foldRight((_, acc) -> acc + 1, 0, c)

    ///
    /// Returns a new Nec formed by appending the Necs `c1` and `c2`.
    ///
    pub def append(c1: Nec[a], c2: Nec[a]): Nec[a] = Nec(c1, c2)

    ///
    /// Deconstruct a Nec from left-to-right.
    ///
    /// Returns `ViewLeft(x, rs)` if the Nec is non-empty, where `x` is the leftmost
    /// element of the Nec `c`, and `rs` is the rest of the Nec.
    ///
    /// Returns `NoneLeft` if the Nec is empty.
    ///
    pub def viewLeft(c: Nec[a]): ViewLeft[a] = viewLeftHelper(c, None, x -> x)

    ///
    /// Helper function for `viewLeft`.
    ///
    /// This traverses the Nec from left-to-right, because the Nec is represented
    /// by a binary tree, the function may have to recursively descend the tree to
    /// find the leftmost node.
    ///
    def viewLeftHelper(c: Nec[a], acc: Option[Nec[a]], k: ViewLeft[a] -> ViewLeft[a]): ViewLeft[a] = match (c, acc) {
        case (NecOne(x), None)       => k(OneLeft(x))
        case (NecOne(x), Some(acc1)) => k(SomeLeft(x, acc1))
        case (Nec(l, r), None)       => viewLeftHelper(l, Some(r),  k)
        case (Nec(l, r), Some(acc1)) => viewLeftHelper(l, Some(append(r, acc1)),  k)
    }

    ///
    /// Deconstruct a Nec from right-to-left.
    ///
    /// Returns `ViewRight(rs, x)` if the Nec is non-empty, where `x` is the rightmost
    /// element of the Nec `c``, and `rs` is the front of the Nec.
    ///
    /// Returns `NoneRight` if the Nec is empty.
    ///
    pub def viewRight(c: Nec[a]): ViewRight[a] = viewRightHelper(c, None, x -> x)

    ///
    /// Helper function for `viewRight`.
    ///
    /// This traverses the Nec from right-to-left, because the Nec is represented
    /// by a binary tree, the function may have to recursively descend the tree to
    /// find the rightmost node.
    ///
    def viewRightHelper(c: Nec[a], acc: Option[Nec[a]], k: ViewRight[a] -> ViewRight[a]): ViewRight[a] = match (c, acc) {
        case (NecOne(x), None)       => k(OneRight(x))
        case (NecOne(x), Some(acc1)) => k(SomeRight(acc1, x))
        case (Nec(l, r), None)       => viewRightHelper(r, Some(l),  k)
        case (Nec(l, r), Some(acc1)) => viewRightHelper(r, Some(append(acc1, l)),  k)
    }

    ///
    /// Returns `true` if and only if `c` contains the element `a`.
    ///
    pub def memberOf(a: a, c: Nec[a]): Bool with Eq[a] = memberOfHelper(a, c)

    ///
    /// Helper function for `memberOf`.
    ///
    def memberOfHelper(a: a, c: Nec[a]): Bool with Eq[a] = match viewLeft(c) {
        case OneLeft(x)               => x == a
        case SomeLeft(x, _) if x == a => true
        case SomeLeft(_, c1)          => memberOfHelper(a, c1)
    }


    ///
    /// Finds the smallest element of `l` according to the `Order` on `a`.
    ///
    pub def minimum(l: Nec[a]): a with Order[a] =
        reduceLeft(Order.min, l)

    ///
    /// Finds the smallest element of `l` according to the given comparator `cmp`.
    ///
    pub def minimumBy(cmp: (a, a) -> Comparison, l: Nec[a]): a =
        reduceLeft(Order.minBy(cmp), l)

    ///
    /// Finds the largest element of `l` according to the `Order` on `a`.
    ///
    pub def maximum(l: Nec[a]): a with Order[a] =
        reduceLeft(Order.max, l)

    ///
    /// Finds the largest element of `l` according to the given comparator `cmp`.
    ///
    pub def maximumBy(cmp: (a, a) -> Comparison, l: Nec[a]): a =
        reduceLeft(Order.maxBy(cmp), l)

    ///
    /// Returns the position of `a` in `c`.
    ///
    /// Returns `-1` if `a` does not exist in `c`.
    ///
    pub def indexOf(a: a, c: Nec[a]): Int32 with Eq[a] = indexOfHelper(a, 0, c)

    ///
    /// Helper function for `indexOf`.
    ///
    def indexOfHelper(a: a, acc: Int32, c: Nec[a]): Int32 with Eq[a] = match viewLeft(c) {
        case OneLeft(x)      => if (x == a) acc else -1
        case SomeLeft(x, c1) => if (x == a) acc else indexOfHelper(a, acc + 1, c1)
    }

    ///
    /// Alias for `findLeft`.
    ///
    /// The function `f` must be pure.
    ///
    pub def find(f: a -> Bool, c: Nec[a]): Option[a] = findLeft(f, c)

    ///
    /// Optionally returns the first element of `c` that satisfies the predicate `f` when searching from left to right.
    ///
    /// The function `f` must be pure.
    ///
    pub def findLeft(f: a -> Bool, c: Nec[a]): Option[a] = match viewLeft(c) {
        case OneLeft(x)      => if (f(x)) Some(x) else None
        case SomeLeft(x, rs) => if (f(x)) Some(x) else findLeft(f, rs)
    }

    ///
    /// Optionally returns the first element of `c` that satisfies the predicate `f` when searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    pub def findRight(f: a -> Bool, c: Nec[a]): Option[a] = match viewRight(c) {
        case OneRight(x)      => if (f(x)) Some(x) else None
        case SomeRight(rs, x) => if (f(x)) Some(x) else findRight(f, rs)
    }

    ///
    /// Returns a list of all integers between `b` (inclusive) and `e` (exclusive) wrapped in `Some`.
    ///
    /// Returns `None` if `b >= e`.
    ///
    pub def range(b: Int32, e: Int32): Option[Nec[Int32]] =
        if (b < e)
            rangeHelper(b, e, ks -> ks) |> Some
        else
            None

    ///
    /// Helper function for `range`.
    ///
    def rangeHelper(b: Int32, e: Int32, k: Nec[Int32] -> Nec[Int32]): Nec[Int32] = match (e-1) {
        case e1 if b == e1 => k(singleton(b))
        case e1 if b < e1  => rangeHelper(b+1, e, ks -> k(cons(b, ks)))
        case e1 if b > e1  => unreachable!()
    }


    ///
    /// Returns the result of applying `f` to every element in `c`.
    ///
    /// That is, the result is of the form: `f(x1) :: f(x2) :: ...`.
    ///
    pub def map(f: a -> b & ef, c: Nec[a]): Nec[b] & ef =
        mapHelper(f, c, k -> k as & ef)

    ///
    /// Helper function for `map`.
    ///
    def mapHelper(f: a -> b & ef, c: Nec[a], k: Nec[b] -> Nec[b] & ef): Nec[b] & ef = match viewLeft(c) {
        case OneLeft(x)      => {let a = f(x); k(NecOne(a))}
        case SomeLeft(x, rs) => mapHelper(f, rs, ks -> {let a = f(x); k(cons(a, ks))})
    }

    ///
    /// Returns the result of applying `f` to every element in `c` along with that element's index.
    ///
    /// That is, the result is of the form: `f(x1, 0) :: f(x2, 1) :: ...`.
    ///
    pub def mapWithIndex(f: (a, Int32) -> b & ef, c: Nec[a]): Nec[b] & ef =
        mapWithIndexHelper(f, c, 0, k -> k as & ef)

    ///
    /// Helper function for `mapWithIndex`.
    ///
    def mapWithIndexHelper(f: (a, Int32) -> b & ef, c: Nec[a], i: Int32, k: Nec[b] -> Nec[b] & ef): Nec[b] & ef = match viewLeft(c) {
        case OneLeft(x)      => {let a = f(x, i); k(NecOne(a))}
        case SomeLeft(x, rs) => mapWithIndexHelper(f, rs, i+1, ks -> {let a = f(x, i); k(cons(a, ks))})
    }
    ///
    /// Apply every function from `f` to every argument from `x` and return a Nec with all results.
    /// For `f = f1, f2, ...` and `x = x1, x2, ...` the results appear in the order
    /// `f1(x1), f1(x2), ..., f2(x1), f2(x2), ...`.
    ///
    pub def ap(f: Nec[a -> b & ef], c: Nec[a]) : Nec[b] & ef =
        apHelper(f, c, k -> k as & ef)

    ///
    /// Helper function for `ap`.
    ///
    def apHelper(f: Nec[a -> b & ef], c: Nec[a], k: Nec[b] -> Nec[b] & ef): Nec[b] & ef = match viewLeft(f) {
        case OneLeft(f1)      => k(map(f1, c))
        case SomeLeft(f1, rs) => apHelper(rs, c, ks -> k(map(f1, c) `append` ks))
    }

    ///
    /// Returns the result of applying `f` to every element in `c` and concatenating the results.
    ///
    pub def flatMap(f: a -> Nec[b] & ef, c: Nec[a]): Nec[b] & ef = flatMapHelper(f, c, k -> k as & ef)

    ///
    /// Helper function for `flatMap`.
    ///
    def flatMapHelper(f: a -> Nec[b] & ef, c: Nec[a], k: Nec[b] -> Nec[b] & ef): Nec[b] & ef = match viewLeft(c) {
        case OneLeft(x)      => {let a = f(x); k(a)}
        case SomeLeft(x, rs) => flatMapHelper(f, rs, ks -> {let a = f(x); k(append(a, ks))})
    }

    ///
    /// Returns the reverse of `c`.
    ///
    pub def reverse(c: Nec[a]): Nec[a] = match viewLeft(c) {
        case OneLeft(x)      => singleton(x)
        case SomeLeft(x, rs) => reverseHelper(rs, singleton(x))
    }

    ///
    /// Helper function for `reverse`.
    ///
    /// Use an accumulator rather than CPS, as it will be built "naturally" in reverse order.
    ///
    def reverseHelper(c: Nec[a], acc: Nec[a]): Nec[a] = match viewLeft(c) {
        case OneLeft(x)      => cons(x, acc)
        case SomeLeft(x, rs) => reverseHelper(rs, cons(x, acc))
    }

    ///
    /// Returns `l` with every occurrence of `from` replaced by `to`.
    ///
    @Time(length(l)) @Space(length(l))
    pub def replace(from: {from :: a}, to: {to :: a}, l: Nec[a]): Nec[a] with Eq[a] =
        map(e -> if (e == from.from) to.to else e, l)

    ///
    /// Returns all permutations of `c` in lexicographical order by element indices in `c`.
    ///
    /// That is, `c` is the first permutation and `reverse(c)` is the last permutation.
    ///
    pub def permutations(c: Nec[a]): Nec[List[a]] = match viewLeft(c) {
        case OneLeft(x)      => singleton(x :: Nil)
        case SomeLeft(x, xs) => match fromList(List.permutations(x :: toList(xs))) {
            case Some(ans) => ans
            case None      => unreachable!()
        }
    }
    ///
    /// Returns all subsequences of `l` in lexicographical order by element indices in `l`.
    ///
    /// That is, `l` is the first subsequence and `Nil` is the last subsequence.
    ///
    pub def subsequences(c: Nec[a]): Nec[List[a]] = match viewLeft(c) {
        case OneLeft(x)      => cons(x :: Nil, singleton(Nil))
        case SomeLeft(x, xs) => match fromList(List.subsequences(x :: toList(xs))) {
            case Some(ans) => ans
            case None      => unreachable!()
        }
    }

    ///
    /// Helper for the `permutations` and `subsequences` functions.
    ///
    /// Uses a worker-wrapper idiom for the loop (passing the head and
    /// the rest of the list) so loop never produces an empty list.
    ///
    def fromList(l: List[a]): Option[Nec[a]] =
        def loop(x, xs, k) = match xs {
            case Nil       => k(singleton(x))
            case x1 :: rs  => loop(x1, rs, ks -> k(cons(x, ks)))
        };
        match l {
            case Nil => None
            case x :: xs => loop(x, xs, ks -> ks) |> Some
        }

    ///
    /// Returns `c` with `a` inserted between every two adjacent elements.
    ///
    pub def intersperse(sep: a, c: Nec[a]): Nec[a] = match viewLeft(c) {
        case OneLeft(x)      => singleton(x)
        case SomeLeft(x, rs) => intersperseHelper(sep, rs, ks -> cons(x, ks))
    }

    ///
    /// Helper function for `intersperse`.
    ///
    def intersperseHelper(sep: a, c: Nec[a], k: Nec[a] -> Nec[a]): Nec[a] = match viewLeft(c) {
        case OneLeft(x)      => k(cons(sep, singleton(x)))
        case SomeLeft(x, rs) => intersperseHelper(sep, rs, ks -> k(cons(sep, cons(x, ks))))
    }

    ///
    /// Returns the number of elements in `c` that satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def count(f: a -> Bool, c: Nec[a]): Int32 = countHelper(f, c, 0)

    ///
    /// Helper function for `count`.
    ///
    def countHelper(f: a -> Bool, c: Nec[a], acc: Int32): Int32 = match viewLeft(c) {
        case OneLeft(x)      => if (f(x)) acc + 1 else acc
        case SomeLeft(x, rs) => if (f(x)) countHelper(f, rs, acc + 1) else countHelper(f, rs, acc)
    }

    ///
    /// Returns the sum of all elements in the Nec `c`.
    ///
    pub def sum(c: Nec[Int32]): Int32 =
        Foldable.sum(c)

    ///
    /// Returns the sum of all elements in the Nec `c` according to the function `f`.
    ///
    pub def sumWith(f: a -> Int32 & ef, c: Nec[a]): Int32 & ef =
        Foldable.sumWith(f, c)

    ///
    /// Returns the product of all elements in the list `l`.
    ///
    pub def product(c: Nec[Int32]): Int32 =
        Foldable.product(c)

    ///
    /// Returns the product of all elements in the list `l` according to the function `f`.
    ///
    pub def productWith(f: a -> Int32 & ef, c: Nec[a]): Int32 & ef =
        Foldable.productWith(f, c)

    ///
    /// Returns the concatenation of the elements in `c`.
    ///
    pub def flatten(c: Nec[Nec[a]]): Nec[a] = match viewLeft(c) {
        case OneLeft(xs)       => xs
        case SomeLeft(xs, xss) => foldLeft(append, xs, xss)
    }

    ///
    /// Returns `true` if and only if at least one element in `c` satisfies the predicate `f`.
    ///
    /// Returns `false` if `c` is empty.
    ///
    /// The function `f` must be pure.
    ///
    pub def exists(f: a -> Bool, c: Nec[a]): Bool = match viewLeft(c) {
        case OneLeft(x)      => f(x)
        case SomeLeft(x, rs) => if (f(x)) true else exists(f, rs)
    }

    ///
    /// Returns `true` if and only if all elements in `c` satisfy the predicate `f`.
    ///
    /// Returns `true` if `c` is empty.
    ///
    /// The function `f` must be pure.
    ///
    pub def forall(f: a -> Bool, c: Nec[a]): Bool = match viewLeft(c) {
        case OneLeft(x)      => f(x)
        case SomeLeft(x, rs) => if (not f(x)) false else forall(f, rs)
    }

    ///
    /// Returns a list of every element in `c` that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def filter(f: a -> Bool & ef, c: Nec[a]): List[a] & ef = filterHelper(f, c, ks -> ks as & ef)

    ///
    /// Helper function for `filter`.
    ///
    def filterHelper(f: a -> Bool & ef, c: Nec[a], k: List[a] -> List[a] & ef): List[a] & ef = match viewLeft(c) {
        case OneLeft(x)      => if (f(x)) k(x :: Nil) else k(Nil)
        case SomeLeft(x, rs) => if (f(x)) filterHelper(f, rs, ks -> k(x :: ks)) else filterHelper(f, rs, k)
    }

    ///
    /// Returns the result of applying `combine` to all the elements in `l`, using `empty` as the initial value.
    ///
    pub def fold(l: Nec[a]): a with Monoid[a] = Foldable.fold(l)

    ///
    /// Applies `f` to a start value `s` and all elements in `c` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b & ef, acc: b, c: Nec[a]): b & ef = match viewLeft(c) {
        case OneLeft(x)      => f(acc, x)
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
    pub def foldRight(f: (a, b) -> b & ef, acc: b, c: Nec[a]): b & ef = match viewRight(c) {
        case OneRight(x)      => f(x, acc)
        case SomeRight(rs, x) => {
            let b = f(x, acc);
            foldRight(f, b, rs)
        }
    }

    ///
    /// Lazily applies `f` to a start value `s` and all elements in `c` going from right to left.
    /// The function f must be pure.
    ///
    /// That is, the result is of the form: `f(x1, ...lazy f(xn-1, lazy f(xn, s))...)`.
    /// The foldRightLazy function exists to allow early termination of a fold.
    ///
    pub def foldRightLazy(f: (a, Lazy[b]) -> b, s: b, c: Nec[a]): b =
        def loop(l2) = match viewLeft(l2) {
            case OneLeft(x)      => f(x, lazy s)
            case SomeLeft(y, ys) => f(y, lazy loop(ys))
        };
        loop(c)

    ///
    /// Collects the results of applying the partial function `f` to every element in `c`.
    ///
    pub def filterMap(f: a -> Option[b] & ef, c: Nec[a]): List[b] & ef =
        def loop(l, k) = match viewLeft(l) {
            case OneLeft(x)      => match (f(x)) {
                case Some(a) => k(a :: Nil)
                case None    => k(Nil)
            }
            case SomeLeft(x, rs) => match f(x) {
                case Some(a) => loop(rs, ks -> k(a :: ks))
                case None    => loop(rs, k)
            }
        };
        loop(c, ks -> ks as & ef)

    ///
    /// Returns the first non-None result of applying the partial function `f` to each element of `c`.
    ///
    /// Returns `None` if every element of `c` is `None`.
    ///
    pub def findMap(f: a -> Option[b] & ef, c: Nec[a]): Option[b] & ef = match viewLeft(c) {
        case OneLeft(x)      => f(x)
        case SomeLeft(x, rs) => match f(x) {
            case Some(v) => Some(v)
            case None    => findMap(f, rs)
        }
    }

    ///
    /// Returns a Nec where the element at index `i` is `(a, b)` where
    /// `a` is the element at index `i` in `c1` and `b` is the element at index `i` in `c2`.
    ///
    /// If either `c1` or `c2` becomes depleted, then no further elements are added to the resulting Nec.
    ///
    pub def zip(c1: Nec[a], c2: Nec[b]): Nec[(a,b)] = zipHelper(c1, c2, k -> k)

    ///
    /// Helper function for `zip`.
    ///
    def zipHelper(c1: Nec[a], c2: Nec[b], k: Nec[(a,b)] -> Nec[(a,b)]): Nec[(a,b)] =
        match (viewLeft(c1), viewLeft(c2)) {
            case (SomeLeft(x, xs), SomeLeft(y, ys)) => zipHelper(xs, ys, ks -> k(cons((x, y), ks)))
            case (OneLeft(x), OneLeft(y))           => k(NecOne((x, y)))
            case (SomeLeft(x, _), OneLeft(y))       => k(NecOne((x, y)))
            case (OneLeft(x), SomeLeft(y, _))       => k(NecOne((x, y)))
        }

    ///
    /// Returns a Nec where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `c1` and `b` is the element at index `i` in `c2`.
    ///
    /// If either `c1` or `c2` becomes depleted, then no further elements are added to the resulting Nec.
    ///
    pub def zipWith(f: (a, b) -> c & ef, c1: Nec[a], c2: Nec[b]): Nec[c] & ef = zipWithHelper(f, c1, c2, ks -> ks as & ef)

    ///
    /// Helper function for `zipWith`.
    ///
    def zipWithHelper(f: (a, b) -> c & ef, c1: Nec[a], c2: Nec[b], k: Nec[c] -> Nec[c] & ef): Nec[c] & ef = match (viewLeft(c1), viewLeft(c2)) {
        case (OneLeft(x), OneLeft(y))           => {let a = f(x, y); k(singleton(a))}
        case (OneLeft(x), SomeLeft(y, _))       => {let a = f(x, y); k(singleton(a))}
        case (SomeLeft(x, _), OneLeft(y))       => {let a = f(x, y); k(singleton(a))}
        case (SomeLeft(x, rs), SomeLeft(y, qs)) => {
            let a = f(x, y);
            zipWithHelper(f, rs, qs, ks -> k(cons(a, ks)))
        }

    }

    ///
    /// Returns a pair of Necs, the first containing all first components in `c`
    /// and the second containing all second components in `c`.
    ///
    pub def unzip(c: Nec[(a, b)]): (Nec[a], Nec[b]) = unzipHelper(c, (ks, ls) -> (ks, ls))

    ///
    /// Helper function for `unzip`.
    ///
    def unzipHelper(c: Nec[(a, b)], k: (Nec[a], Nec[b]) -> (Nec[a], Nec[b])): (Nec[a], Nec[b]) = match viewLeft(c) {
        case OneLeft((a, b))      => k(singleton(a), singleton(b))
        case SomeLeft((a, b), rs) => unzipHelper(rs, (ks, ls) -> k(cons(a, ks), cons(b, ls)))
    }


    ///
    /// `mapAccumLeft` is a stateful version of `map`. The accumulating paramter `s` is updated at each
    /// step in a left-to-right traversal.
    ///
    pub def mapAccumLeft(f: (s, a) -> (s, b) & ef, start: s, c: Nec[a]): (s, Nec[b]) & ef =
        mapAccumLeftHelper(f, start, c, (s1, ks) -> (s1, ks) as & ef)

    ///
    /// Helper function for `mapAccumLeft`.
    ///
    def mapAccumLeftHelper(f: (s, a) -> (s, b) & ef, s1: s, c: Nec[a], k: (s, Nec[b]) -> (s, Nec[b]) & ef): (s, Nec[b]) & ef =
        match viewLeft(c) {
            case OneLeft(x)      => {
                let (s2, x1) = f(s1, x);
                k(s2, NecOne(x1))
            }
            case SomeLeft(x, rs) => {
                let (s2, x1) = f(s1, x);
                mapAccumLeftHelper(f, s2, rs, (s3, ks) -> k(s3, cons(x1, ks)))
            }
        }

    ///
    /// `mapAccumRight` is a stateful version of `map`. The accumulating parameter `s` is updated at each
    /// step in a right-to-left traversal.
    ///
    pub def mapAccumRight(f: (s, a) -> (s, b) & ef, start: s, c: Nec[a]): (s, Nec[b]) & ef =
        mapAccumRightHelper(f, start, c, (s1, ks) -> (s1, ks) as & ef)

    ///
    /// Helper function for `mapAccumRight`.
    ///
    def mapAccumRightHelper(f: (s, a) -> (s, b) & ef, s1: s, c: Nec[a], k: (s, Nec[b]) -> (s, Nec[b]) & ef): (s, Nec[b]) & ef =
        match viewRight(c) {
            case OneRight(x)      => {
                let (s2, x1) = f(s1, x);
                k(s2, NecOne(x1))
            }
            case SomeRight(rs, x) => {
                let (s2, x1) = f(s1, x);
                mapAccumRightHelper(f, s2, rs, (s3, ks) -> k(s3, snoc(ks, x1)))
           }
        }

    ///
    /// Applies `f` to every element of `c`.
    ///
    pub def foreach(f: a -> Unit & ef, c: Nec[a]): Unit & ef = match viewLeft(c) {
        case OneLeft(x)      => f(x)
        case SomeLeft(x, rs) => f(x); foreach(f, rs)
    }

    ///
    /// Returns `c` as a list.
    ///
    pub def toList(c: Nec[a]): List[a] =
        foldRight((x, acc) -> x :: acc, Nil, c)

    ///
    /// Returns `c` as a mutable list.
    ///
    pub def toMutList(c: Nec[a]): MutList[a] & Impure =
        MutList(ref toArray(c), ref length(c))

    ///
    /// Returns the list `c` as a set.
    ///
    pub def toSet(c: Nec[a]): Set[a] with Order[a] = foldRight(Set.insert, Set.empty(), c)


    ///
    /// Returns the Nec of pairs `c` that represents an association list as a map.
    ///
    /// If `c` contains multiple mappings with the same key, `toMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    pub def toMap(c: Nec[(a, b)]): Map[a, b] with Order[a] = foldRight((x, m) -> Map.insert(fst(x), snd(x), m), Map.empty(), c)

    ///
    /// Returns a map with elements of `l` as keys and `f` applied as values.
    ///
    /// If `l` contains multiple mappings with the same key, `toMapWith` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    pub def toMapWith(f: a -> b, l: Nec[a]): Map[a, b] with Order[a] =
        foldRight((x, m) -> Map.insert(x, f(x), m), Map.empty(), l)

    ///
    /// Returns the Nec `c` as an array.
    ///
    pub def toArray(c: Nec[a]) : Array[a] & Impure =
        let x = head(c);
        let arr = Array.new(x, length(c));
        let f = (i, b) -> { arr[i] = b; i + 1 };
        foldLeft(f, 0, c);
        arr

    ///
    /// Returns an iterator over `c`.
    ///
    pub def toIterator(c: Nec[a]): Iterator[a] & Impure = toIteratorHelper(Some(viewLeft(c)))

    ///
    /// Returns an iterator over `l`.
    ///
    def toIteratorHelper(vl: Option[ViewLeft[a]]): Iterator[a] & Impure =
        let cursor = ref vl;
        let done = () -> match (deref cursor) {
            case None    => true
            case Some(_) => false
        };
        let next = () -> match (deref cursor) {
            case None                 => bug!("Empty Iterator!")
            case Some(OneLeft(x))     => {
                cursor := None;
                x
            }
            case Some(SomeLeft(x, xs)) => {
                cursor := Some(viewLeft(xs));
                x
            }
        };
        Iterator(done, next)

    ///
    /// Helper for the `sort` functions.
    ///
    def fromArray(arr: Array[a]): Option[Nec[a]] & Impure =
        def loop(ix, end, acc) = if (ix >= end) acc else {let x = arr[ix]; loop(ix+1, end, snoc(acc, x))};
        let len = Array.length(arr);
        if (len < 1)
            None
        else {
            let acc = singleton(arr[0]);
            loop(1, len, acc) |> Some
        }


    ///
    /// Sort Nec `c` so that elements are ordered from low to high according to their `Order` instance.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `c`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sort(l: Nec[a]): Nec[a] with Order[a] =
        let ans = (l |> toArray |> Array.sort |> fromArray) as & Pure;
        match ans {
            case Some(n1) => n1
            case None     => unreachable!()
        }


    /// Sort Nec `c` so that elements are ordered from low to high according to the `Order` instance
    /// for the values obtained by applying `f` to each element.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `c`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortBy(f: a -> b, l: Nec[a]): Nec[a] with Order[b] =
        let ans = (l |> toArray |> Array.sortBy(f) |> fromArray) as & Pure;
        match ans {
            case Some(n1) => n1
            case None     => unreachable!()
        }

    ///
    /// Sort Nec `c` so that elements are ordered from low to high according to the comparison function `cmp`.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `c`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortWith(cmp: (a, a) -> Comparison, l: Nec[a]): Nec[a] =
        let ans = (l |> toArray |> Array.sortWith(cmp) |> fromArray) as & Pure;
        match ans {
            case Some(n1) => n1
            case None     => unreachable!()
        }

    ///
    /// Helper function for `traverse` and `sequence`.
    ///
    /// Builds an "applicative Nec".
    ///
    def consA(mx: f[a], mxs: f[Nec[a]]): f[Nec[a]] with Applicative[f] =
        (((x, xs) -> cons(x, xs)) <$> mx) <*> mxs

    ///
    /// Returns the result of running all the actions in the Nec `c`.
    ///
    pub def sequence(c: Nec[m[a]]): m[Nec[a]] with Applicative[m] =
        def loop(l2, k) = match viewLeft(l2) {
            case OneLeft(x)      => k(NecOne <$> x)
            case SomeLeft(x, rs) => loop(rs, ks -> k(consA(x, ks)))
        };
        loop(c, ks -> ks)

    ///
    /// Returns the result of applying the applicative mapping function `f` to all the elements of the
    /// Nec `c`.
    ///
    pub def traverse(f: a -> m[b] & ef, c: Nec[a]): m[Nec[b]] & ef with Applicative[m] =
        def loop(l2, k) = match viewLeft(l2) {
            case OneLeft(x)      => k(NecOne <$> f(x))
            case SomeLeft(x, rs) => loop(rs, ks -> k(consA(f(x), ks)))
        };
        loop(c, ks -> ks as & ef)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `c` with `sep` inserted between each element.
    ///
    pub def join(sep: String, c: Nec[a]): String with ToString[a] =
        Foldable.join(sep, c)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `c` according to `f` with `sep` inserted between each element.
    ///
    pub def joinWith(f: a -> String & ef, sep: String, c: Nec[a]): String =
        Foldable.joinWith(f, sep, c)


    ///
    /// Applies `combine` to all elements in `c` until a single value is obtained.
    ///
    pub def reduce(c: Nec[a]): a with SemiGroup[a] = match viewLeft(c) {
        case OneLeft(x)      => x
        case SomeLeft(x, xs) => foldLeft((acc, y) -> SemiGroup.combine(acc, y), x, xs)
    }

    ///
    /// Applies `f` to all elements in `c` going from left to right until a single value `v` is obtained.
    ///
    /// That is, the result is of the form: `f(...f(f(x1, x2), x3)..., xn)`
    ///
    pub def reduceLeft(f: (a, a) -> a & ef, c: Nec[a]): a & ef = match viewLeft(c) {
        case OneLeft(x)      => x
        case SomeLeft(x, xs) => foldLeft(f, x, xs)
    }

    ///
    /// Applies `f` to all elements in `c` going from right to left until a single value `v` is obtained.
    ///
    /// That is, the result is of the form: `Some(f(x1, ...f(xn-2, f(xn-1, xn))...))`
    ///
    pub def reduceRight(f: (a, a) -> a & ef, c: Nec[a]): a & ef = match viewRight(c) {
        case OneRight(x)      => x
        case SomeRight(xs, x) => foldRight(f, x, xs)
    }

    ///
    /// Left-associative reduction of a structure.
    /// Applies `g` to the initial element of `l` and combines it
    /// with the remainder of `l` using `f` going from left to right.
    ///
    pub def reduceLeftTo(f: (b, a) -> b & ef1, g: a -> b & ef2, c: Nec[a]): b & (ef1 and ef2) = match viewLeft(c) {
        case OneLeft(x)      => g(x)
        case SomeLeft(x, xs) => foldLeft(f, g(x), xs)
    }

    ///
    /// Right-associative reduction of a structure.
    /// Applies `g` to the initial element of `l` and combines it
    /// with the remainder of `l` using `f` going from right to left.
    ///
    pub def reduceRightTo(f: (a, b) -> b & ef1, g: a -> b & ef2, c: Nec[a]): b & (ef1 and ef2) = match viewRight(c) {
        case OneRight(x)      => g(x)
        case SomeRight(xs, x) => foldRight(f, g(x), xs)
    }

}
