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

import java.util.{List => JList}

///
/// The NonEmpty Chain type.
///
/// A chain is a list represented as an unbalanced binary tree.
/// It supports efficient append and "snoc" - appending elements at the tail
/// of the list.
///
/// Note - the constructors `NecOne` and `Nec` should not be used directly.
///
pub enum Nec[t] with Sendable {
    case NecOne(t)
    case Nec(Nec[t], Nec[t])
}

instance Eq[Nec[a]] with Eq[a] {
    pub def eq(c1: Nec[a], c2: Nec[a]): Bool = Nec.equals(c1, c2)
}

instance Order[Nec[a]] with Order[a] {
    ///
    /// Compares `c1` and `c2` lexicographically.
    ///
    pub def compare(c1: Nec[a], c2: Nec[a]): Comparison =
        use Nec.ViewLeft;
        match (Nec.viewLeft(c1), Nec.viewLeft(c2)) {
            case (ViewLeft.OneLeft(x), ViewLeft.OneLeft(y))           => x <=> y
            case (ViewLeft.OneLeft(x), ViewLeft.SomeLeft(y, _))       => match (x <=> y) {
                case Comparison.EqualTo => Comparison.LessThan
                case cmp     => cmp
            }
            case (ViewLeft.SomeLeft(x, _), ViewLeft.OneLeft(y))       => match (x <=> y) {
                case Comparison.EqualTo => Comparison.GreaterThan
                case cmp     => cmp
            }
            case (ViewLeft.SomeLeft(x, xs), ViewLeft.SomeLeft(y, ys)) =>
                let cmp = x <=> y;
                if (cmp == Comparison.EqualTo) xs <=> ys else cmp
        }
}

instance Hash[Nec[a]] with Hash[a] {
    pub def hash(c: Nec[a]): Int32 = 39119 + Hash.hash(Nec.toList(c))
}

instance SemiGroup[Nec[a]] {
    pub def combine(c1: Nec[a], c2: Nec[a]): Nec[a] = Nec.append(c1, c2)
}

instance Functor[Nec] {
    pub def map(f: a -> b \ ef, c: Nec[a]): Nec[b] \ ef = Nec.map(f, c)
}

instance Applicative[Nec] {
    pub def point(x: a) : Nec[a] = Nec.singleton(x)
    pub def ap(f: Nec[a -> b \ ef], x: Nec[a]) : Nec[b] \ ef = Nec.ap(f, x)
}

instance Monad[Nec] {
    pub def flatMap(f: a -> Nec[b] \ ef, x : Nec[a]) : Nec[b] \ ef = Nec.flatMap(f, x)
}

instance MonadZip[Nec] {
    pub def zipWith(f: (a, b) -> c \ ef, xs: Nec[a], ys: Nec[b]): Nec[c] \ ef = Nec.zipWith(f, xs, ys)
    pub def zipWithA(f: (a, b) -> f[c] \ ef, xs: Nec[a], ys: Nec[b]): f[Nec[c]] \ ef with Applicative[f] = Nec.zipWithA(f, xs, ys)
    redef zip(xs: Nec[a], ys: Nec[b]): Nec[(a, b)] = Nec.zip(xs, ys)
    redef unzip(xs: Nec[(a, b)]): (Nec[a], Nec[b]) = Nec.unzip(xs)
}

instance Foldable[Nec] {
    pub def foldLeft(f: (b, a) -> b \ ef, s: b, c: Nec[a]): b \ ef = Nec.foldLeft(f, s, c)
    pub def foldRight(f: (a, b) -> b \ ef, s: b, c: Nec[a]): b \ ef = Nec.foldRight(f, s, c)
    pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, s: b, c: Nec[a]): b \ ef = Nec.foldRightWithCont(f, s, c)
}

instance UnorderedFoldable[Nec] {
    pub def foldMap(f: a -> b \ ef, c: Nec[a]): b \ ef with CommutativeMonoid[b] = Nec.foldMap(f, c)
    redef isEmpty(_: Nec[a]): Bool = false
    redef exists(f: a -> Bool \ ef, c: Nec[a]): Bool \ ef = Nec.exists(f, c)
    redef forAll(f: a -> Bool \ ef, c: Nec[a]): Bool \ ef = Nec.forAll(f, c)
    redef memberOf(x: a, c: Nec[a]): Bool with Eq[a] = Nec.memberOf(x, c)
}

instance Traversable[Nec] {
    pub def traverse(f: a -> m[b] \ ef, t: Nec[a]): m[Nec[b]] \ ef with Applicative[m] = Nec.traverse(f, t)
    redef sequence(t: Nec[m[a]]): m[Nec[a]] with Applicative[m] = Nec.sequence(t)
}

instance Reducible[Nec] {
    pub def reduceLeftTo(f: (b, a) -> b \ ef1, g: a -> b \ ef2, l: Nec[a]): b \ {ef1, ef2} = Nec.reduceLeftTo(f, g, l)
    pub def reduceRightTo(f: (a, b) -> b \ ef1, g: a -> b \ ef2, l: Nec[a]): b \ {ef1, ef2} = Nec.reduceRightTo(f, g, l)
    redef head(l: Nec[a]): a = Nec.head(l)
    redef last(l: Nec[a]): a = Nec.last(l)
    redef init(l: Nec[a]): List[a] = Nec.init(l)
    redef tail(l: Nec[a]): List[a] = Nec.tail(l)
    redef exists(f: a -> Bool \ ef, l: Nec[a]): Bool \ ef = Nec.exists(f, l)
    redef forAll(f: a -> Bool \ ef, l: Nec[a]): Bool \ ef = Nec.forAll(f, l)
    redef find(f: a -> Bool \ ef, l: Nec[a]): Option[a] \ ef = Nec.find(f, l)
    redef findLeft(f: a -> Bool \ ef, l: Nec[a]): Option[a] \ ef = Nec.findLeft(f, l)
    redef findRight(f: a -> Bool \ ef, l: Nec[a]): Option[a] \ ef = Nec.findRight(f, l)
    redef memberOf(a: a, l: Nec[a]): Bool with Eq[a] = Nec.memberOf(a, l)
    redef dropWhile(f: a -> Bool \ ef, l: Nec[a]): List[a] \ ef = Nec.dropWhileLeft(f, l)
    redef takeWhile(f: a -> Bool \ ef, l: Nec[a]): List[a] \ ef = Nec.takeWhileLeft(f, l)
    redef toArray(rc: Region[r], l: Nec[a]): Array[a, r] \ r = Nec.toArray(rc, l)
    redef toVector(l: Nec[a]): Vector[a] = Nec.toVector(l)
    redef toList(l: Nec[a]): List[a] = Nec.toList(l)
}

instance ToString[Nec[a]] with ToString[a] {
    pub def toString(c: Nec[a]): String = region rc {
        "Nec#{" + (Nec.iterator(rc, c) |> Iterator.join(", ")) + "}"
    }
}

instance Iterable[Nec[a]] {
    type Elm = a
    pub def iterator(rc: Region[r], l: Nec[a]): Iterator[a, r, r] \ r = Nec.iterator(rc, l)
}

instance ToJava[Nec[a]] {
    type Out = JList
    type Aef = IO
    pub def toJava(l: Nec[a]): JList \ IO = Adaptor.toList(l)
}

mod Nec {
    use ViewLeft.{OneLeft, SomeLeft}
    use ViewRight.{OneRight, SomeRight}

    use Functor.{<$>}
    use Applicative.{<*>}

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
    pub def equals(c1: Nec[a], c2: Nec[a]): Bool with Eq[a] = viewLeft(c1) == viewLeft(c2)

    //
    // Implementation Note: Necs are considered equal if their (ordered) list of elements are equal.
    //
    // Because they may have different shapes due to construction we use a view to
    // decide equality which imposes an order on the Nec.
    //

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
        case ViewLeft.OneLeft(x)     => x
        case ViewLeft.SomeLeft(x, _) => x
    }

    ///
    /// Returns the last element of `c`.
    ///
    pub def last(c: Nec[a]): a = match viewRight(c) {
        case ViewRight.OneRight(x)     => x
        case ViewRight.SomeRight(_, x) => x
    }

    ///
    /// Returns the list of elements in `c` without the last element.
    ///
    pub def init(c: Nec[a]): List[a] = match viewRight(c) {
        case ViewRight.OneRight(_)      => Nil
        case ViewRight.SomeRight(rs, _) => toList(rs)
    }

    ///
    /// Returns all elements in `c` without the first element.
    ///
    pub def tail(c: Nec[a]): List[a] = match viewLeft(c) {
        case ViewLeft.OneLeft(_)      => Nil
        case ViewLeft.SomeLeft(_, rs) => toList(rs)
    }

    ///
    /// Returns the number of elements in `c`.
    ///
    pub def length(c: Nec[a]): Int32 = foldRight((_, acc) -> acc + 1, 0, c)

    ///
    /// Returns the number of elements in `c`.
    ///
    pub def size(c: Nec[a]): Int32 = length(c)

    ///
    /// Returns a new Nec formed by appending the Necs `c1` and `c2`.
    ///
    pub def append(c1: Nec[a], c2: Nec[a]): Nec[a] = Nec(c1, c2)

    ///
    /// Deconstruct a Nec from left-to-right.
    ///
    /// Returns `ViewLeft.SomeLeft(x, rs)` if the Nec has at least two elements, where `x` is the leftmost
    /// element of the Nec `c`, and `rs` is the rest of the Nec.
    ///
    /// Returns `ViewLeft.OneLeft` if the Nec has a single element.
    ///
    pub def viewLeft(c: Nec[a]): ViewLeft[a] =
        def loop(c1: Nec[a], rs: Option[Nec[a]], k: ViewLeft[a] -> ViewLeft[a]) = match (c1, rs) {
            case (NecOne(x), None)      => k(ViewLeft.OneLeft(x))
            case (NecOne(x), Some(rs1)) => k(ViewLeft.SomeLeft(x, rs1))
            case (Nec(l, r), None)      => loop(l, Some(r),  k)
            case (Nec(l, r), Some(rs1)) => loop(l, Some(append(r, rs1)),  k)
        };
        loop(c, None, x -> x)


    ///
    /// Deconstruct a Nec from right-to-left.
    ///
    /// Returns `ViewRight.SomeRight(rs, x)` if the Nec has at least two elements, where `x` is the rightmost
    /// element of the Nec `c`, and `rs` is the front of the Nec.
    ///
    /// Returns `ViewRight.OneRight` if the Nec has a single element.
    ///
    pub def viewRight(c: Nec[a]): ViewRight[a] =
        def loop(c1: Nec[a], rs: Option[Nec[a]], k: ViewRight[a] -> ViewRight[a]) = match (c1, rs) {
            case (NecOne(x), None)      => k(ViewRight.OneRight(x))
            case (NecOne(x), Some(rs1)) => k(ViewRight.SomeRight(rs1, x))
            case (Nec(l, r), None)      => loop(r, Some(l),  k)
            case (Nec(l, r), Some(rs1)) => loop(r, Some(append(rs1, l)),  k)
        };
        loop(c, None, x -> x)

    ///
    /// Returns `true` if and only if `c` contains the element `a`.
    ///
    pub def memberOf(a: a, c: Nec[a]): Bool with Eq[a] =
        def loop(c1) = match viewLeft(c1) {
            case ViewLeft.OneLeft(x)               => x == a
            case ViewLeft.SomeLeft(x, _) if x == a => true
            case ViewLeft.SomeLeft(_, c2)          => loop(c2)
        };
        loop(c)

    ///
    /// Finds the smallest element of `c` according to the `Order` on `a`.
    ///
    pub def minimum(c: Nec[a]): a with Order[a] =
        reduceLeft(Order.min, c)

    ///
    /// Finds the smallest element of `c` according to the given comparator `cmp`.
    ///
    pub def minimumBy(cmp: (a, a) -> Comparison, c: Nec[a]): a =
        reduceLeft(Order.minBy(cmp), c)

    ///
    /// Finds the largest element of `c` according to the `Order` on `a`.
    ///
    pub def maximum(c: Nec[a]): a with Order[a] =
        reduceLeft(Order.max, c)

    ///
    /// Finds the largest element of `c` according to the given comparator `cmp`.
    ///
    pub def maximumBy(cmp: (a, a) -> Comparison, c: Nec[a]): a =
        reduceLeft(Order.maxBy(cmp), c)

    ///
    /// Optionally returns the position of `a` in `c`.
    ///
    pub def indexOf(a: a, c: Nec[a]): Option[Int32] with Eq[a] =
        def loop(acc: Int32, c1: Nec[a]) = match viewLeft(c1) {
            case ViewLeft.OneLeft(x)      => if (x == a) Some(acc) else None
            case ViewLeft.SomeLeft(x, c2) => if (x == a) Some(acc) else loop(acc + 1, c2)
        };
        loop(0, c)

    ///
    /// Alias for `findLeft`.
    ///
    pub def find(f: a -> Bool \ ef, c: Nec[a]): Option[a] \ ef = findLeft(f, c)

    ///
    /// Optionally returns the first element of `c` that satisfies the predicate `f` when searching from left to right.
    ///
    pub def findLeft(f: a -> Bool \ ef, c: Nec[a]): Option[a] \ ef = match viewLeft(c) {
        case ViewLeft.OneLeft(x)      => if (f(x)) Some(x) else None
        case ViewLeft.SomeLeft(x, rs) => if (f(x)) Some(x) else findLeft(f, rs)
    }

    ///
    /// Optionally returns the first element of `c` that satisfies the predicate `f` when searching from right to left.
    ///
    pub def findRight(f: a -> Bool \ ef, c: Nec[a]): Option[a] \ ef = match viewRight(c) {
        case ViewRight.OneRight(x)      => if (f(x)) Some(x) else None
        case ViewRight.SomeRight(rs, x) => if (f(x)) Some(x) else findRight(f, rs)
    }

    ///
    /// Returns a list of all integers between `b` (inclusive) and `e` (exclusive) wrapped in `Some`.
    ///
    /// Returns `None` if `b >= e`.
    ///
    pub def range(b: Int32, e: Int32): Option[Nec[Int32]] =
        def loop(ix: Int32, k: Nec[Int32] -> Nec[Int32]) = match (e-1) {
            case e1 if ix == e1 => k(singleton(ix))
            case e1 if ix < e1  => loop(ix+1, ks -> k(cons(ix, ks)))
            case _              => unreachable!()
        };
        if (b < e)
            loop(b, ks -> ks) |> Some
        else
            None

    ///
    /// Returns the result of applying `f` to every element in `c`.
    ///
    /// That is, the result is of the form: `f(x1) :: f(x2) :: ...`.
    ///
    pub def map(f: a -> b \ ef, c: Nec[a]): Nec[b] \ ef =
        def loop(c1: Nec[a], k: Nec[b] -> Nec[b] \ ef) = match viewLeft(c1) {
            case ViewLeft.OneLeft(x)      => {let a = f(x); k(NecOne(a))}
            case ViewLeft.SomeLeft(x, rs) => loop(rs, ks -> {let a = f(x); k(cons(a, ks))})
        };
        loop(c, x -> checked_ecast(x))

    ///
    /// Returns the result of applying `f` to every element in `c` along with that element's index.
    ///
    /// That is, the result is of the form: `f(x1, 0) :: f(x2, 1) :: ...`.
    ///
    pub def mapWithIndex(f: (Int32, a) -> b \ ef, c: Nec[a]): Nec[b] \ ef =
        def loop(c1: Nec[a], i: Int32, k: Nec[b] -> Nec[b] \ ef) = match viewLeft(c1) {
            case ViewLeft.OneLeft(x)      => {let a = f(i, x); k(NecOne(a))}
            case ViewLeft.SomeLeft(x, rs) => loop(rs, i+1, ks -> {let a = f(i, x); k(cons(a, ks))})
        };
        loop(c, 0, x -> checked_ecast(x))

    ///
    /// Apply every function from `f` to every argument from `x` and return a Nec with all results.
    /// For `f = f1, f2, ...` and `x = x1, x2, ...` the results appear in the order
    /// `f1(x1), f1(x2), ..., f2(x1), f2(x2), ...`.
    ///
    pub def ap(f: Nec[a -> b \ ef], c: Nec[a]) : Nec[b] \ ef =
        def loop(f1: Nec[a -> b \ ef], k: Nec[b] -> Nec[b] \ ef) = match viewLeft(f1) {
            case ViewLeft.OneLeft(f2)      => k(map(f2, c))
            case ViewLeft.SomeLeft(f2, rs) => loop(rs, ks -> k(map(f2, c) `append` ks))
        };
        loop(f, x -> checked_ecast(x))

    ///
    /// Returns the result of applying `f` to every element in `c` and concatenating the results.
    ///
    pub def flatMap(f: a -> Nec[b] \ ef, c: Nec[a]): Nec[b] \ ef =
        def loop(c1: Nec[a], k: Nec[b] -> Nec[b] \ ef) = match viewLeft(c1) {
            case ViewLeft.OneLeft(x)      => {let a = f(x); k(a)}
            case ViewLeft.SomeLeft(x, rs) => loop(rs, ks -> {let a = f(x); k(append(a, ks))})
        };
        loop(c, x -> checked_ecast(x))

    ///
    /// Returns the reverse of `c`.
    ///
    pub def reverse(c: Nec[a]): Nec[a] =
        // Use an accumulator rather than CPS, as it will be built "naturally" in reverse order.
        def loop(c1: Nec[a], acc: Nec[a]) = match viewLeft(c1) {
            case ViewLeft.OneLeft(x)      => cons(x, acc)
            case ViewLeft.SomeLeft(x, rs) => loop(rs, cons(x, acc))
        };
        // Do one step before the loop so we have some content.
        match viewLeft(c) {
            case ViewLeft.OneLeft(x)      => singleton(x)
            case ViewLeft.SomeLeft(x, rs) => loop(rs, singleton(x))
        }

    ///
    /// Returns `l` with every occurrence of `src` replaced by `dst`.
    ///
    pub def replace(src: {src = a}, dst: {dst = a}, l: Nec[a]): Nec[a] with Eq[a] =
        map(e -> if (e == src#src) dst#dst else e, l)

    ///
    /// Returns all permutations of `c` in lexicographical order by element indices in `c`.
    ///
    /// That is, `c` is the first permutation and `reverse(c)` is the last permutation.
    ///
    pub def permutations(c: Nec[a]): Nec[List[a]] = match viewLeft(c) {
        case ViewLeft.OneLeft(x)      => singleton(x :: Nil)
        case ViewLeft.SomeLeft(x, xs) => match fromList(List.permutations(x :: toList(xs))) {
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
        case ViewLeft.OneLeft(x)      => cons(x :: Nil, singleton(Nil))
        case ViewLeft.SomeLeft(x, xs) => match fromList(List.subsequences(x :: toList(xs))) {
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
    pub def intersperse(sep: a, c: Nec[a]): Nec[a] =
        def loop(c1: Nec[a], k: Nec[a] -> Nec[a]) = match viewLeft(c1) {
            case ViewLeft.OneLeft(x)      => k(cons(sep, singleton(x)))
            case ViewLeft.SomeLeft(x, rs) => loop(rs, ks -> k(cons(sep, cons(x, ks))))
        };
        // Do one step before the loop so we have some content.
        match viewLeft(c) {
            case ViewLeft.OneLeft(x)      => singleton(x)
            case ViewLeft.SomeLeft(x, rs) => loop(rs, ks -> cons(x, ks))
        }

    ///
    /// Returns the number of elements in `c` that satisfy the predicate `f`.
    ///
    pub def count(f: a -> Bool \ ef, c: Nec[a]): Int32 \ ef =
        def loop(c1: Nec[a], acc: Int32) = match viewLeft(c1) {
            case ViewLeft.OneLeft(x)      => if (f(x)) acc + 1 else acc
            case ViewLeft.SomeLeft(x, rs) => loop(rs, if (f(x)) acc + 1 else acc)
        };
        loop(c, 0)

    ///
    /// Returns the sum of all elements in the Nec `c`.
    ///
    pub def sum(c: Nec[Int32]): Int32 =
        Foldable.sum(c)

    ///
    /// Returns the sum of all elements in the Nec `c` according to the function `f`.
    ///
    pub def sumWith(f: a -> Int32 \ ef, c: Nec[a]): Int32 \ ef =
        Foldable.sumWith(f, c)

    ///
    /// Returns the concatenation of the elements in `c`.
    ///
    pub def flatten(c: Nec[Nec[a]]): Nec[a] = match viewLeft(c) {
        case ViewLeft.OneLeft(xs)       => xs
        case ViewLeft.SomeLeft(xs, xss) => foldLeft(append, xs, xss)
    }

    ///
    /// Returns `true` if and only if at least one element in `c` satisfies the predicate `f`.
    ///
    /// Returns `false` if `c` is empty.
    ///
    pub def exists(f: a -> Bool \ ef, c: Nec[a]): Bool \ ef = match viewLeft(c) {
        case ViewLeft.OneLeft(x)      => f(x)
        case ViewLeft.SomeLeft(x, rs) => if (f(x)) true else exists(f, rs)
    }

    ///
    /// Returns `true` if and only if all elements in `c` satisfy the predicate `f`.
    ///
    /// Returns `true` if `c` is empty.
    ///
    pub def forAll(f: a -> Bool \ ef, c: Nec[a]): Bool \ ef = match viewLeft(c) {
        case ViewLeft.OneLeft(x)      => f(x)
        case ViewLeft.SomeLeft(x, rs) => if (not f(x)) false else forAll(f, rs)
    }

    ///
    /// Returns a list of every element in `c` that satisfies the predicate `f`.
    ///
    pub def filter(f: a -> Bool \ ef, c: Nec[a]): List[a] \ ef =
        def loop(c1: Nec[a], k: List[a] -> List[a] \ ef) = match viewLeft(c1) {
            case ViewLeft.OneLeft(x)      => if (f(x)) k(x :: Nil) else k(Nil)
            case ViewLeft.SomeLeft(x, rs) => if (f(x)) loop(rs, ks -> k(x :: ks)) else loop(rs, k)
        };
        loop(c, x -> checked_ecast(x))



    ///
    /// Returns the result of applying `combine` to all the elements in `l`, using `empty` as the initial value.
    ///
    pub def fold(l: Nec[a]): a with Monoid[a] = Foldable.fold(l)

    ///
    /// Applies `f` to a start value `s` and all elements in `c` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b \ ef, acc: b, c: Nec[a]): b \ ef = match viewLeft(c) {
        case ViewLeft.OneLeft(x)      => f(acc, x)
        case ViewLeft.SomeLeft(x, rs) => {
            let b = f(acc, x);
            foldLeft(f, b, rs)
        }
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `c` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b \ ef, s: b, c: Nec[a]): b \ ef = match viewRight(c) {
        case ViewRight.OneRight(x)      => f(x, s)
        case ViewRight.SomeRight(rs, x) => {
            let b = f(x, s);
            foldRight(f, b, rs)
        }
    }

    ///
    /// Applies `f` to a start value `z` and all elements in `c` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, z))...)`.
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, z: b, c: Nec[a]): b \ ef = match viewLeft(c) {
        case ViewLeft.OneLeft(x)      => f(x, _ -> checked_ecast(z))
        case ViewLeft.SomeLeft(x, rs) => f(x, _ -> foldRightWithCont(f, z, rs))
    }

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap(f: a -> b \ ef, c: Nec[a]): b \ ef with Monoid[b] =
        foldLeft((acc, x) -> Monoid.combine(acc, f(x)), Monoid.empty(), c)

    ///
    /// Collects the results of applying the partial function `f` to every element in `c`.
    ///
    pub def filterMap(f: a -> Option[b] \ ef, c: Nec[a]): List[b] \ ef =
        def loop(l, k) = match viewLeft(l) {
            case ViewLeft.OneLeft(x)      => match (f(x)) {
                case Some(a) => k(a :: Nil)
                case None    => k(Nil)
            }
            case ViewLeft.SomeLeft(x, rs) => match f(x) {
                case Some(a) => loop(rs, ks -> k(a :: ks))
                case None    => loop(rs, k)
            }
        };
        loop(c, identity)

    ///
    /// Returns the first non-None result of applying the partial function `f` to each element of `c`.
    ///
    /// Returns `None` if f(c) for every element of c is `None`.
    ///
    pub def findMap(f: a -> Option[b] \ ef, c: Nec[a]): Option[b] \ ef = match viewLeft(c) {
        case ViewLeft.OneLeft(x)      => f(x)
        case ViewLeft.SomeLeft(x, rs) => match f(x) {
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
    pub def zip(c1: Nec[a], c2: Nec[b]): Nec[(a,b)] =
        def loop(nec1: Nec[a], nec2: Nec[b], k: Nec[(a,b)] -> Nec[(a,b)]) = match (viewLeft(nec1), viewLeft(nec2)) {
            case (ViewLeft.SomeLeft(x, xs), ViewLeft.SomeLeft(y, ys)) => loop(xs, ys, ks -> k(cons((x, y), ks)))
            case (ViewLeft.OneLeft(x), ViewLeft.OneLeft(y))           => k(NecOne((x, y)))
            case (ViewLeft.SomeLeft(x, _), ViewLeft.OneLeft(y))       => k(NecOne((x, y)))
            case (ViewLeft.OneLeft(x), ViewLeft.SomeLeft(y, _))       => k(NecOne((x, y)))
        };
        loop(c1, c2, k -> k)

    ///
    /// Returns a Nec where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `c1` and `b` is the element at index `i` in `c2`.
    ///
    /// If either `c1` or `c2` becomes depleted, then no further elements are added to the resulting Nec.
    ///
    pub def zipWith(f: (a, b) -> c \ ef, c1: Nec[a], c2: Nec[b]): Nec[c] \ ef =
        def loop(nec1: Nec[a], nec2: Nec[b], k: Nec[c] -> Nec[c] \ ef) = match (viewLeft(nec1), viewLeft(nec2)) {
            case (ViewLeft.OneLeft(x), ViewLeft.OneLeft(y))           => {let a = f(x, y); k(singleton(a))}
            case (ViewLeft.OneLeft(x), ViewLeft.SomeLeft(y, _))       => {let a = f(x, y); k(singleton(a))}
            case (ViewLeft.SomeLeft(x, _), ViewLeft.OneLeft(y))       => {let a = f(x, y); k(singleton(a))}
            case (ViewLeft.SomeLeft(x, rs), ViewLeft.SomeLeft(y, qs)) => {
                let a = f(x, y);
                loop(rs, qs, ks -> k(cons(a, ks)))
            }
        };
        loop(c1, c2, x -> checked_ecast(x))

    ///
    /// Returns a pair of Necs, the first containing all first components in `c`
    /// and the second containing all second components in `c`.
    ///
    pub def unzip(c: Nec[(a, b)]): (Nec[a], Nec[b]) =
        def loop(c1: Nec[(a, b)], k: (Nec[a], Nec[b]) -> (Nec[a], Nec[b])) = match viewLeft(c1) {
            case ViewLeft.OneLeft((a, b))      => k(singleton(a), singleton(b))
            case ViewLeft.SomeLeft((a, b), rs) => loop(rs, (ks, ls) -> k(cons(a, ks), cons(b, ls)))
        };
        loop(c, (ks, ls) -> (ks, ls))

    ///
    /// Returns a Nec where each element `e` is mapped to `(i, e)` where `i`
    /// is the index of `e`.
    ///
    pub def zipWithIndex(c: Nec[a]): Nec[(Int32, a)] =
        def loop(c1, k, i) = match viewLeft(c1) {
            case ViewLeft.OneLeft(x)      => k(NecOne(i, x))
            case ViewLeft.SomeLeft(x, rs) => loop(rs, ks -> k(cons((i, x), ks)), i + 1)
        };
        loop(c, k -> k, 0)

    ///
    /// Generalize `zipWith` to an applicative functor `f`.
    ///
    pub def zipWithA(f: (a, b) -> f[c] \ ef, xs: Nec[a], ys: Nec[b]): f[Nec[c]] \ ef with Applicative[f] =
        def loop(v1, v2, k) = match (v1, v2) {
            case (ViewLeft.SomeLeft(x, c1), ViewLeft.SomeLeft(y, c2)) => loop(viewLeft(c1), viewLeft(c2), ks -> k(cons <$> f(x,y) <*> ks))
            case (ViewLeft.SomeLeft(x, _), ViewLeft.OneLeft(y))       => k(singleton <$> f(x,y))
            case (ViewLeft.OneLeft(x), ViewLeft.SomeLeft(y, _))       => k(singleton <$> f(x,y))
            case (ViewLeft.OneLeft(x), ViewLeft.OneLeft(y))           => k(singleton <$> f(x,y))
        };
        loop(viewLeft(xs), viewLeft(ys), x -> checked_ecast(x))

    ///
    /// `mapAccumLeft` is a stateful version of `map`. The accumulating parameter `s` is updated at each
    /// step in a left-to-right traversal.
    ///
    pub def mapAccumLeft(f: (s, a) -> (s, b) \ ef, start: s, c: Nec[a]): (s, Nec[b]) \ ef =
        def loop(s1, c1, k) = match viewLeft(c1) {
            case ViewLeft.OneLeft(x)      => {
                let (s2, x1) = f(s1, x);
                k((s2, NecOne(x1)))
            }
            case ViewLeft.SomeLeft(x, rs) => {
                let (s2, x1) = f(s1, x);
                loop(s2, rs, match (s3, ks) -> k((s3, cons(x1, ks))))
            }
        };
        loop(start, c, identity)

    ///
    /// `mapAccumRight` is a stateful version of `map`. The accumulating parameter `s` is updated at each
    /// step in a right-to-left traversal.
    ///
    pub def mapAccumRight(f: (s, a) -> (s, b) \ ef, start: s, c: Nec[a]): (s, Nec[b]) \ ef =
        def loop(s1, c1, k) = match viewRight(c1) {
            case ViewRight.OneRight(x)      => {
                let (s2, x1) = f(s1, x);
                k((s2, NecOne(x1)))
            }
            case ViewRight.SomeRight(rs, x) => {
                let (s2, x1) = f(s1, x);
                loop(s2, rs, match (s3, ks) -> k((s3, snoc(ks, x1))))
           }
        };
        loop(start, c, identity)

    ///
    /// Applies `f` to every element of `c`.
    ///
    pub def forEach(f: a -> Unit \ ef, c: Nec[a]): Unit \ ef = match viewLeft(c) {
        case ViewLeft.OneLeft(x)      => f(x)
        case ViewLeft.SomeLeft(x, rs) => f(x); forEach(f, rs)
    }

    ///
    /// Applies `f` to every element of `c` along with that element's index.
    ///
    pub def forEachWithIndex(f: (Int32, a) -> Unit \ ef, c: Nec[a]): Unit \ ef =
        def loop(v, i) = match v {
            case ViewLeft.OneLeft(x)      => f(i, x)
            case ViewLeft.SomeLeft(x, rs) => f(i, x); loop(viewLeft(rs), i+1)
        };
        loop(viewLeft(c), 0)

    ///
    /// Returns `c` as a list.
    ///
    pub def toList(c: Nec[a]): List[a] =
        foldRight((x, acc) -> x :: acc, Nil, c)

    ///
    /// Returns `c` as a MutDeque.
    ///
    pub def toMutDeque(rc: Region[r], c: Nec[a]): MutDeque[a, r] \ r  =
        let d = MutDeque.empty(rc);
        forEach(x -> MutDeque.pushBack(x, d), c);
        d

    ///
    /// Returns `c` as a mutable list.
    ///
    pub def toMutList(rc1: Region[r], c: Nec[a]): MutList[a, r] \ r = region rc2 {
        Array.toMutList(rc1, toArray(rc2, c)) // `Array.toMutList` respects the invariant of `MutList`
    }

    ///
    /// Returns the list `c` as a set.
    ///
    pub def toSet(c: Nec[a]): Set[a] with Order[a] = foldRight((x, acc) -> Set.insert(x, acc), Set.empty(), c)

    ///
    /// Returns the Nec of pairs `c` that represents an association list as a map.
    ///
    /// If `c` contains multiple mappings with the same key, `toMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    pub def toMap(c: Nec[(a, b)]): Map[a, b] with Order[a] = foldRight((x, acc) -> Map.insert(fst(x), snd(x), acc), Map.empty(), c)

    ///
    /// Returns a map with elements of `l` as keys and `f` applied as values.
    ///
    /// If `l` contains multiple mappings with the same key, `toMapWith` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    pub def toMapWith(f: a -> b, l: Nec[a]): Map[a, b] with Order[a] =
        foldRight((x, acc) -> Map.insert(x, f(x), acc), Map.empty(), l)

    ///
    /// Returns the Nec `c` as an array.
    ///
    pub def toArray(rc: Region[r], c: Nec[a]): Array[a, r] \ r =
        let x = head(c);
        let arr = Array.repeat(rc, length(c), x);
        forEach(match (i, b) -> Array.put(b, i, arr), zipWithIndex(c));
        arr

    ///
    /// Returns the Nec `c` as a vector.
    ///
    pub def toVector(c: Nec[a]): Vector[a] = region rc {
        let arr = Array.empty(rc, length(c));
        forEachWithIndex((i, x) -> Array.put(x, i, arr), c);
        Array.toVector(arr)
    }

    ///
    /// Returns an iterator over `c`.
    ///
    pub def iterator(rc: Region[r], c: Nec[a]): Iterator[a, r, r] \ r =
        iteratorHelper(rc, Some(viewLeft(c)))

    ///
    /// Returns an iterator over `l`.
    ///
    def iteratorHelper(rc: Region[r], vl: Option[ViewLeft[a]]): Iterator[a, r, r] \ r =
        let cursor = Ref.fresh(rc, vl);
        let next = () -> match (Ref.get(cursor)) {
            case None             => None
            case Some(OneLeft(x)) =>
                Ref.put(None, cursor);
                Some(x)
            case Some(SomeLeft(x, xs)) =>
                Ref.put(Some(viewLeft(xs)), cursor);
                Some(x)
        };
        Iterator.iterate(rc, next)

    ///
    /// Returns an iterator over `c` zipped with the indices of the elements.
    ///
    pub def enumerator(rc: Region[r], c: Nec[a]): Iterator[(Int32, a), r, r] \ r =
        iterator(rc, c) |> Iterator.zipWithIndex

    ///
    /// Helper for the `sort` functions.
    ///
    def fromArray(arr: Array[a, r]): Option[Nec[a]] \ r =
        def loop(ix, end, acc) = if (ix >= end) acc else {let x = Array.get(ix, arr); loop(ix+1, end, snoc(acc, x))};
        let len = Array.length(arr);
        if (len < 1)
            None
        else {
            let acc = singleton(Array.get(0, arr));
            loop(1, len, acc) |> Some
        }


    ///
    /// Sort Nec `c` so that elements are ordered from low to high according to their `Order` instance.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `c`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sort(c: Nec[a]): Nec[a] with Order[a] = region rc {
        let ans = toArray(rc, c) !> Array.sort! |> fromArray;
        match ans {
            case Some(n1) => n1
            case None     => unreachable!()
        }
    }


    /// Sort Nec `c` so that elements are ordered from low to high according to the `Order` instance
    /// for the values obtained by applying `f` to each element.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `c`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortBy(f: a -> b, c: Nec[a]): Nec[a] with Order[b] = region rc {
        let ans = toArray(rc, c) !> Array.sortBy!(f) |> fromArray;
        match ans {
            case Some(n1) => n1
            case None     => unreachable!()
        }
    }

    ///
    /// Sort Nec `c` so that elements are ordered from low to high according to the comparison function `cmp`.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `c`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortWith(cmp: (a, a) -> Comparison, c: Nec[a]): Nec[a] = region rc {
        let ans = toArray(rc, c) !> Array.sortWith!(cmp) |> fromArray;
        match ans {
            case Some(n1) => n1
            case None     => unreachable!()
        }
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
            case ViewLeft.OneLeft(x)      => k(NecOne <$> x)
            case ViewLeft.SomeLeft(x, rs) => loop(rs, ks -> k(consA(x, ks)))
        };
        loop(c, ks -> ks)

    ///
    /// Returns the result of applying the applicative mapping function `f` to all the elements of the
    /// Nec `c`.
    ///
    pub def traverse(f: a -> m[b] \ ef, c: Nec[a]): m[Nec[b]] \ ef with Applicative[m] =
        def loop(l2, k) = match viewLeft(l2) {
            case ViewLeft.OneLeft(x)      => k(NecOne <$> f(x))
            case ViewLeft.SomeLeft(x, rs) => {let ans = f(x); loop(rs, ks -> k(consA(ans, ks)))}
        };
        loop(c, identity)

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
    pub def joinWith(f: a -> String \ ef, sep: String, c: Nec[a]): String \ ef =
        Foldable.joinWith(f, sep, c)


    ///
    /// Applies `combine` to all elements in `c` until a single value is obtained.
    ///
    pub def reduce(c: Nec[a]): a with SemiGroup[a] = match viewLeft(c) {
        case ViewLeft.OneLeft(x)      => x
        case ViewLeft.SomeLeft(x, xs) => foldLeft((acc, y) -> SemiGroup.combine(acc, y), x, xs)
    }

    ///
    /// Applies `f` to all elements in `c` going from left to right until a single value `v` is obtained.
    ///
    /// That is, the result is of the form: `f(...f(f(x1, x2), x3)..., xn)`
    ///
    pub def reduceLeft(f: (a, a) -> a \ ef, c: Nec[a]): a \ ef = match viewLeft(c) {
        case ViewLeft.OneLeft(x)      => x
        case ViewLeft.SomeLeft(x, xs) => foldLeft(f, x, xs)
    }

    ///
    /// Applies `f` to all elements in `c` going from right to left until a single value `v` is obtained.
    ///
    /// That is, the result is of the form: `Some(f(x1, ...f(xn-2, f(xn-1, xn))...))`
    ///
    pub def reduceRight(f: (a, a) -> a \ ef, c: Nec[a]): a \ ef = match viewRight(c) {
        case ViewRight.OneRight(x)      => x
        case ViewRight.SomeRight(xs, x) => foldRight((a, acc) -> f(a, acc), x, xs)
    }

    ///
    /// Left-associative reduction of a structure.
    /// Applies `g` to the initial element of `c` and combines it
    /// with the remainder of `c` using `f` going from left to right.
    ///
    pub def reduceLeftTo(f: (b, a) -> b \ ef1, g: a -> b \ ef2, c: Nec[a]): b \ {ef1, ef2} = match viewLeft(c) {
        case ViewLeft.OneLeft(x)      => g(x)
        case ViewLeft.SomeLeft(x, xs) => foldLeft(f, g(x), xs)
    }

    ///
    /// Right-associative reduction of a structure.
    /// Applies `g` to the initial element of `c` and combines it
    /// with the remainder of `c` using `f` going from right to left.
    ///
    pub def reduceRightTo(f: (a, b) -> b \ ef1, g: a -> b \ ef2, c: Nec[a]): b \ {ef1, ef2} = match viewRight(c) {
        case ViewRight.OneRight(x)      => g(x)
        case ViewRight.SomeRight(xs, x) => foldRight((a, acc) -> f(a, acc), g(x), xs)
    }

    ///
    /// Returns `c` without the longest prefix that satisfies the predicate `f`.
    ///
    pub def dropWhileLeft(f: a -> Bool \ ef, c: Nec[a]): List[a] \ ef =
        def loop(c1) = match viewLeft(c1) {
            case ViewLeft.OneLeft(x)      => if (f(x)) Nil else (x :: Nil)
            case ViewLeft.SomeLeft(x, rs) => if (f(x)) loop(rs) else x :: toList(rs)
        };
        loop(c)

    ///
    /// Returns `c` without the longest sufffix that satisfies the predicate `f`.
    ///
    pub def dropWhileRight(f: a -> Bool \ ef, c: Nec[a]): List[a] \ ef =
        def loop(c1) = match viewRight(c1) {
            case ViewRight.OneRight(x)      => if (f(x)) Nil else (x :: Nil)
            case ViewRight.SomeRight(rs, x) => if (f(x)) loop(rs) else toList(rs `snoc` x)
        };
        loop(c)

    ///
    /// Returns the longest prefix of `c` that satisfies the predicate `f`.
    ///
    pub def takeWhileLeft(f: a -> Bool \ ef, c: Nec[a]): List[a] \ ef =
        def loop(c1, k) = match viewLeft(c1) {
            case ViewLeft.OneLeft(x)      => if (f(x)) k(x :: Nil) else k(Nil)
            case ViewLeft.SomeLeft(x, rs) => if (f(x)) loop(rs, ks -> k(x :: ks)) else k(Nil)
        };
        loop(c, identity)


    ///
    /// Returns the longest prefix of `c` that satisfies the predicate `f`.
    ///
    pub def takeWhileRight(f: a -> Bool \ ef, c: Nec[a]): List[a] \ ef =
        def loop(c1, ac) = match viewRight(c1) {
            case ViewRight.OneRight(x)      => if (f(x)) (x :: ac) else ac
            case ViewRight.SomeRight(rs, x) => if (f(x)) loop(rs, x :: ac) else ac
        };
        loop(c, Nil)

    ///
    /// Optionally returns the Nec `c` shuffled using the Fisher–Yates shuffle.
    ///
    pub def shuffle(c: Nec[a]): Option[Nec[a]] \ NonDet = region rc {
        toArray(rc, c) !> Array.shuffle |> Array.toNec
    }

}
