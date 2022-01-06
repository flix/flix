/*
 * Copyright 2019 Liam Palmer, Magnus Madsen
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
/// The List type.
///
/// A list is either the empty list represented by `Nil`, or
/// an element `v` followed by a list `vs` represented by `v :: vs`.
///
pub enum List[t] {
    case Nil,
    case Cons(t, List[t])
}

instance Boxable[List[t]]

instance ToString[List[a]] with ToString[a] {
    pub def toString(l: List[a]): String = List.toString(l)
}

instance Hash[List[a]] with Hash[a] {
    pub def hash(l: List[a]): Int32 =
        List.foldLeft((acc, x) -> acc * 31 + Hash.hash(x), 37199, l)
}

instance Eq[List[a]] with Eq[a] {
    pub def eq(l1: List[a], l2: List[a]): Bool = match (l1, l2) {
        case (Nil, Nil) => true
        case (x :: rs, y :: qs) => if (x != y) false else rs == qs
        case _ => false
    }
}

instance Order[List[a]] with Order[a] {

    ///
    /// Compares `l1` and `l2` lexicographically.
    ///
    pub def compare(l1: List[a], l2: List[a]): Comparison = match (l1, l2) {
        case (_ :: _, Nil) => GreaterThan
        case (Nil, Nil) => EqualTo
        case (Nil, _ :: _) => LessThan
        case (z :: zs, w :: ws) =>
            let cmp = z <=> w;
            if (cmp == EqualTo) zs <=> ws else cmp
    }

}

instance Functor[List] {
    pub def map(f: a -> b & ef, l: List[a]): List[b] & ef = List.map(f, l)
}

instance Applicative[List] {
    pub def point(a: a) : List[a] = List.point(a)
    pub def ap(f: List[a -> b & e], x: List[a]) : List[b] & e = List.ap(f, x)
}

instance Monad[List] {
    pub def flatMap(f: a -> List[b] & ef, x : List[a]) : List[b] & ef = List.flatMap(f, x)
}

instance Foldable[List] {
    pub def foldLeft(f: (b, a) -> b & ef, s: b, l: List[a]): b & ef = List.foldLeft(f, s, l)
    pub def foldRight(f: (a, b) -> b & ef, s: b, l: List[a]): b & ef = List.foldRight(f, s, l)
}

instance Traversable[List] {
    pub def traverse(f: a -> m[b] & ef, t: List[a]): m[List[b]] & ef with Applicative[m] = List.traverse(f, t)
    pub override def sequence(t: List[m[a]]): m[List[a]] with Applicative[m] = List.sequence(t)
}

namespace List {

    ///
    /// Renders the list `l` to a String.
    ///
    pub def toString(l: List[a]): String with ToString[a] = toStringHelper(l) as & Pure

    ///
    /// Helper function for `toString`.
    ///
    def toStringHelper(l: List[a]): String & Impure with ToString[a] =
        let sb = StringBuilder.new();
        List.foreach(x -> StringBuilder.appendString!(sb, "${x} :: "), l);
        StringBuilder.appendString!(sb, "Nil");
        StringBuilder.toString(sb)

    ///
    /// Returns true if and only if `l` is the empty list, i.e. `Nil`.
    ///
    @Time(1) @Space(1)
    pub def isEmpty(l: List[a]): Bool = match l {
        case Nil => true
        case _ => false
    }

    ///
    /// Returns `Some(x)` if `x` is the first element of `l`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    @Time(1) @Space(1)
    pub def head(l: List[a]): Option[a] = match l {
        case Nil => None
        case x :: _ => Some(x)
    }

    ///
    /// Returns `Some(x)` if `x` is the last element of `l`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    @Time(length(l)) @Space(1)
    pub def last(l: List[a]): Option[a] = match l {
        case Nil => None
        case x :: Nil => Some(x)
        case _ :: rs => last(rs)
    }

    ///
    /// Returns the length of `l`.
    ///
    @Time(length(l)) @Space(1)
    pub def length(l: List[a]): Int32 =
        def loop(l2, acc) = match l2 {
            case Nil     => acc
            case _ :: rs => loop(rs, acc + 1)
        };
        loop(l, 0)

    ///
    /// Returns `l2` appended to `l1`.
    ///
    /// The infix operator `:::` is an alias for `append` (`l1 ::: l2 = append(l1, l2)`).
    ///
    @Time(length(l1)) @Space(length(l1))
    pub def append(l1: List[a], l2: List[a]): List[a] = {
        foldRight((x, acc) -> x :: acc, l2, l1)
    }

    ///
    /// Returns `true` if and only if `l` contains the element `x`.
    ///
    @Time(length(l)) @Space(1)
    pub def memberOf(a: a, l: List[a]): Bool with Eq[a] =
        def loop(l2) = match l2 {
            case Nil     => false
            case x :: xs => if (a == x) true else loop(xs)
        };
        loop(l)

    ///
    /// Optionally finds the smallest element of `l` according to the `Order` on `a`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def minimum(l: List[a]): Option[a] with Order[a] =
        reduceLeft(Order.min, l)

    ///
    /// Optionally finds the smallest element of `l` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def minimumBy(cmp: (a, a) -> Comparison, l: List[a]): Option[a] =
        reduceLeft(Order.minBy(cmp), l)

    ///
    /// Optionally finds the largest element of `l` according to the `Order` on `a`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def maximum(l: List[a]): Option[a] with Order[a] =
        reduceLeft(Order.max, l)

    ///
    /// Optionally finds the largest element of `l` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def maximumBy(cmp: (a, a) -> Comparison, l: List[a]): Option[a] =
        reduceLeft(Order.maxBy(cmp), l)

    ///
    /// Returns the position of `x` in `l`.
    ///
    /// Returns `-1` if `a` does not exist in `l`.
    ///
    pub def indexOf(a: a, l: List[a]): Int32 with Eq[a] =
        def loop(l2, acc) = match l2 {
            case Nil     => -1
            case x :: xs => if (a == x) acc else loop(xs, acc + 1)
        };
        loop(l, 0)

    ///
    /// Alias for `findLeft`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(l)) @Space(space(f))
    pub def find(f: a -> Bool, l: List[a]): Option[a] = findLeft(f, l)

    ///
    /// Optionally returns the first element of `l` that satisfies the predicate `f` when searching from left to right.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(l)) @Space(space(f))
    pub def findLeft(f: a -> Bool, l: List[a]): Option[a] = match l {
        case Nil => None
        case x :: xs => if (f(x)) Some(x) else findLeft(f, xs)
    }

    ///
    /// Optionally returns the first element of `l` that satisfies the predicate `f` when searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(l)) @Space(space(f))
    pub def findRight(f: a -> Bool, l: List[a]): Option[a] = match l {
        case Nil     => None
        case x :: xs => Option.withDefault(default = if (f(x)) Some(x) else None, findRight(f, xs))
    }

    ///
    /// Returns a list of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns `Nil` if `b >= e`.
    ///
    @Time(e - b) @Space(e - b)
    pub def range(b: Int32, e: Int32): List[Int32] =
        def loop(n, k) =
            if (n >= e)
                k(Nil)
            else
                loop(n + 1, ks -> k(n :: ks));
        loop(b, ks -> ks)

    ///
    /// Returns a list with the element `x` repeated `n` times.
    ///
    /// Returns `Nil` if `n < 0`.
    ///
    @Time(n) @Space(n)
    pub def repeat(n: Int32, a: a): List[a] = repeatHelper(a, n, ks -> ks)

    ///
    /// Helper function for `repeat`.
    ///
    def repeatHelper(a: a, n: Int32, k: List[a] -> List[a]): List[a] =
        if (n <= 0) k(Nil) else repeatHelper(a, n - 1, ks -> k(a :: ks))


    ///
    /// Alias for `scanLeft`.
    ///
    pub def scan(f: (b, a) -> b & ef, s: b, l: List[a]): List[b] & ef = scanLeft(f, s, l)

    ///
    /// Accumulates the result of applying `f` to `l` going left to right.
    ///
    /// That is, the result is of the form: `s :: f(s, x1) :: f(f(s, x1), x2)  ...`.
    ///
    pub def scanLeft(f: (b, a) -> b & ef, s: b, l: List[a]): List[b] & ef = s :: scanLeftHelper(f, s, l)

    ///
    /// Helper function for `scanLeft`.
    ///
    def scanLeftHelper(f: (b, a) -> b & ef, s: b, l: List[a]): List[b] & ef = match l {
        case Nil     => Nil
        case x :: xs => f(s, x) :: scanLeftHelper(f, f(s, x), xs)
    }

    ///
    /// Accumulates the result of applying `f` to `l` going right to left.
    ///
    /// That is, the result is of the form: `... f(xn-1, f(xn, s)) :: f(xn, s) :: s`.
    ///
    pub def scanRight(f: (a, b) -> b & ef, s: b, l: List[a]): List[b] & ef = match l {
        case Nil     => s :: Nil
        case x :: xs => match scanRight(f, s, xs) {
            case y :: ys => f(x, y) :: y :: ys
            case _       => unreachable!()
        }
    }

    ///
    /// Returns the result of applying `f` to every element in `l`.
    ///
    /// That is, the result is of the form: `f(x1) :: f(x2) :: ...`.
    ///
    pub def map(f: a -> b & ef, l: List[a]): List[b] & ef =
        def loop(l2, k) = match l2 {
            case Nil     => k(Nil)
            case x :: xs => {
                let y = f(x);
                loop(xs, ys -> k(y :: ys))
            }
        };
        loop(l, identity)

    ///
    /// Return the singleton list with element `x`.
    ///
    pub def point(a: a) : List[a] = a :: Nil

    ///
    /// Apply every function from `f` to every argument from `x` and return a list with all results.
    /// For `f = f1, f2, ...` and `x = x1, x2, ...` the results appear in the order
    /// `f1(x1), f1(x2), ..., f2(x1), f2(x2), ...`.
    ///
    pub def ap(f: List[a -> b & e], x: List[a]) : List[b] & e =
        map(g -> map(g, x), f) |> flatten

    // TODO: Efficient implementations of lift2,...,lift5, redirect liftA2,...,liftA5 to those.
    ///
    /// Lift a binary function to work on lists of its original arguments, returning a list
    /// of applying all combinations of arguments.
    /// For argument lists `l1 = x1, x2, ...` and `l2 = y1, y2, ...` the results appear in the order
    /// `f(x1,y1), f(x1,y2), ..., f(x2,y1), f(x2,y2), ...`.
    ///
    pub def lift2(f: t1 -> t2 -> r & e, l1: List[t1], l2: List[t2]): List[r] & e = Applicative.liftA2(f, l1, l2)

    ///
    /// Lift a ternary function to work on lists of its original arguments, returning a list
    /// of applying all combinations of arguments.
    /// For argument lists `l1 = x1, x2, ...`, `l2 = y1, y2, ...` and `l3 = z1, z2, ...` the results appear
    /// in the following order:
    ///
    /// ```
    /// f(x1,y1,z1), f(x1,y1,z2), ..., f(x1,y2,z1), f(x1,y2,z2), ...,
    /// f(x2,y1,z1), f(x2,y1,z2), ..., f(x2,y2,z1), f(x2,y2,z2), ...`
    /// ...
    /// ```
    ///
    pub def lift3(f: t1 -> t2 -> t3 -> r & e, l1: List[t1], l2: List[t2], l3: List[t3]): List[r] & e = Applicative.liftA3(f, l1, l2, l3)

    /// Lift a 4-ary function to work on lists of its original arguments, returning a list
    /// of applying all combinations of arguments. The results appear in the order extending the pattern from `lift3`.
    pub def lift4(f: t1 -> t2 -> t3 -> t4 -> r & e, l1: List[t1], l2: List[t2], l3: List[t3], l4: List[t4]): List[r] & e = Applicative.liftA4(f, l1, l2, l3, l4)

    /// Lift a 5-ary function to work on lists of its original arguments, returning a list
    /// of applying all combinations of arguments. The results appear in the order extending the pattern from `lift3`.
    pub def lift5(f: t1 -> t2 -> t3 -> t4 -> t5 -> r & e, l1: List[t1], l2: List[t2], l3: List[t3], l4: List[t4], l5: List[t5]): List[r] & e = Applicative.liftA5(f, l1, l2, l3, l4, l5)

    ///
    /// Returns the result of applying `f` to every element in `l` along with that element's index.
    ///
    /// That is, the result is of the form: `f(x1, 0) :: f(x2, 1) :: ...`.
    ///
    @Time(time(f) * length(l)) @Space(space(f) * length(l))
    pub def mapWithIndex(f: (a, Int32) -> b & ef, l: List[a]): List[b] & ef = mapWithIndexHelper(f, l, 0, ks -> ks)

    ///
    /// Helper function for `mapWithIndex`.
    ///
    def mapWithIndexHelper(f: (a, Int32) -> b & ef, l: List[a], i: Int32, k: List[b] -> List[b]): List[b] & ef = match l {
        case Nil     => k(Nil)
        case x :: xs => {
            let y = f(x, i);
            mapWithIndexHelper(f, xs, i+1, ys -> k(y :: ys))
        }
    }

    ///
    /// Returns the result of applying `f` to every element in `l` and concatenating the results.
    ///
    @Time(time(f) * length(l)) @Space(time(f) * length(l))
    pub def flatMap(f: a -> List[b] & ef, l: List[a]): List[b] & ef = flatMapHelper(f, l, ks -> ks)

    ///
    /// Helper function for `flatMap`.
    ///
    def flatMapHelper(f: a -> List[b] & ef, l: List[a], k: List[b] -> List[b]): List[b] & ef = match l {
        case Nil     => k(Nil)
        case x :: xs => {
            let y = f(x);
            flatMapHelper(f, xs, ys -> k(y ::: ys))
        }
    }

    ///
    /// Returns the reverse of `l`.
    ///
    @Time(length(l)) @Space(length(l))
    pub def reverse(l: List[a]): List[a] =
        def loop(l2, acc) = match l2 {
            case Nil     => acc
            case x :: xs => loop(xs, x :: acc)
        };
        loop(l, Nil)

    ///
    /// Returns `l` with its elements rotated `n` positions to the left.
    ///
    /// That is, returns a new list where the first `n mod length(l)` elements in `l`
    /// are the last `n mod length(l)` elements of the new list.
    ///
    @Time(length(l)) @Space(length(l))
    pub def rotateLeft(n: Int32, l: List[a]): List[a] =
        let len = length(l);
        if (len == 0)
            l
        else {
            let rem = n rem len;
            let rotate = if (rem < 0) rem + len else rem;
            drop(rotate, l) ::: take(rotate, l)
        }

    ///
    /// Returns `l` with its elements rotated `n` positions to the right.
    ///
    /// That is, returns a new list where the last `n mod length(l)` elements in `l`
    /// are the first `n mod length(l)` elements of the new list.
    ///
    @Time(length(l)) @Space(length(l))
    pub def rotateRight(n: Int32, l: List[a]): List[a] = rotateLeft(-n, l)

    ///
    /// Returns `l` with the element at index `i` replaced by `x`.
    ///
    /// Returns `l` if `i < 0` or `i > length(l)-1`.
    ///
    @Time(i) @Space(i)
    pub def update(i: Int32, a: a, l: List[a]): List[a] = updateHelper(i, a, l, ks -> ks)

    ///
    /// Helper function for `update`.
    ///
    def updateHelper(i: Int32, a: a, l: List[a], k: List[a] -> List[a]): List[a] = match (i, l) {
        case (_, Nil)     => k(l)
        case (0, _ :: xs) => k(a :: xs)
        case (p, x :: xs) => updateHelper(p - 1, a, xs, ks -> k(x :: ks))
    }

    ///
    /// Returns `l` with every occurrence of `a` replaced by `b`.
    ///
    @Time(length(l)) @Space(length(l))
    pub def replace(a: a, b: a, l: List[a]): List[a] with Eq[a] = map(e -> if (e == a) b else e, l)

    ///
    /// Returns `l2` with the `n` elements starting at index `i` replaced with the elements of `l1`.
    ///
    /// If any of the indices `i, i+1, i+2, ... , i+n-1` are out of range in `l2` then no patching is done at these indices.
    /// If `l1` becomes depleted then no further patching is done.
    /// If patching occurs at index `i+j` in `l2`, then the element at index `j` in `l1` is used.
    ///
    @Time(n) @Space(length(l2))
    pub def patch(i: Int32, n: Int32, l1: List[a], l2: List[a]): List[a] = patchHelper(i, n, drop(-i, l1), l2, 0, ks -> ks)

    ///
    /// Helper function for `patch`.
    ///
    def patchHelper(i: Int32, n: Int32, l1: List[a], l2: List[a], c: Int32, k: List[a] -> List[a]): List[a] = match (l1, l2) {
        case (x :: xs, y :: ys) => {
            if (c >= i and c < i + n)
                patchHelper(i, n, xs, ys, c + 1, ks -> k(x :: ks))
            else
                patchHelper(i, n, l1, ys, c + 1, ks -> k(y :: ks))
        }
        case _ => k(l2)
    }

    ///
    /// Returns all permutations of `l` in lexicographical order by element indices in `l`.
    ///
    /// That is, `l` is the first permutation and `reverse(l)` is the last permutation.
    ///
    @Time(Int32.factorial(length(l))) @Space(Int32.factorial(length(l)))
    pub def permutations(l: List[a]): List[List[a]] = match l {
        case Nil => Nil :: Nil
        case _   => permutationHelper(0, l)
    }

    ///
    /// Helper function for `permutations`.
    /// Returns all permutations of `l` starting with an element at or after index `i`.
    ///
    def permutationHelper(i: Int32, l: List[a]): List[List[a]] =
        if (i == length(l))
            Nil
        else
            applyHelper(at(i, l), permutations(removeIndex(i, l))) ::: permutationHelper(i + 1, l)

    ///
    /// Helper function for `permutations`.
    ///
    def at(i: Int32, l: List[a]): a = match (i, l) {
        case (0, x :: _)  => x
        case (p, _ :: xs) => at(p - 1, xs)
        case _ => unreachable!()
    }

    ///
    /// Helper function for `permutations`.
    ///
    def removeIndex(i: Int32, l: List[a]): List[a] = match (i, l) {
        case (_, Nil) => l
        case (0, _ :: xs) => xs
        case (p, x :: xs) => x :: removeIndex(p - 1, xs)
    }

    ///
    /// Returns all subsequences of `l` in lexicographical order by element indices in `l`.
    ///
    /// That is, `l` is the first subsequence and `Nil` is the last subsequence.
    ///
    @Time(length(l) * length(l)) @Space(length(l) * length(l))
    pub def subsequences(l: List[a]): List[List[a]] = match l {
        case Nil     => Nil :: Nil
        case x :: xs =>
            let r = subsequences(xs);
            applyHelper(x, r) ::: r
    }

    ///
    /// Helper function for `permutations` and `subsequences`.
    /// Returns `l` with `x` added to the beginning of each element in `l`.
    ///
    def applyHelper(a: a, l: List[List[a]]): List[List[a]] = match l {
        case Nil     => Nil
        case x :: xs => (a :: x) :: applyHelper(a, xs)
    }

    ///
    /// Returns `l` with `x` inserted between every two adjacent elements.
    ///
    @Time(length(l)) @Space(length(l))
    pub def intersperse(a: a, l: List[a]): List[a] = intersperseHelper(a, l, ks -> ks)

    ///
    /// Helper function for `intersperse`.
    ///
    def intersperseHelper(a: a, l: List[a], k: List[a] -> List[a]): List[a] = match l {
        case x1 :: x2 :: xs => intersperseHelper(a, x2 :: xs, ks -> k(x1 :: a :: ks))
        case _              => k(l)
    }

    ///
    /// Returns the concatenation of the elements in `l2` with the elements of `l1` inserted between every two adjacent elements.
    ///
    /// That is, returns `y1 :: x1 ... xn :: y2 :: ... yn-1 :: x1 :: ... :: xn :: yn :: Nil`.
    ///
    @Time(length(l2) * length(l1)) @Space(length(l2) * length(l1))
    pub def intercalate(l1: List[a], l2: List[List[a]]): List[a] = intercalateHelper(l1, l2, ks -> ks)

    ///
    /// Helper function for `intercalate`.
    ///
    def intercalateHelper(l1: List[a], l2: List[List[a]], k: List[a] -> List[a]): List[a] = match l2 {
        case Nil            => k(Nil)
        case y :: Nil       => k(y)
        case y1 :: y2 :: ys => intercalateHelper(l1, y2 :: ys, ks -> k(y1 ::: l1 ::: ks))
    }

    ///
    /// Returns the transpose of `l`.
    ///
    /// Returns `l` if the dimensions of the elements of `l` are mismatched.
    ///
    @Time(length(l) * length(l)) @Space(length(l) * length(l))
    pub def transpose(l: List[List[a]]): List[List[a]] = match l {
        case Nil    => Nil
        case x :: _ =>
            let len = length(x);
            if (not uniformHelper(l, len) or len == 0) l else transposeHelper(l, len)
    }

    ///
    /// Helper function for `transpose`.
    ///
    def uniformHelper(l: List[List[a]], len: Int32): Bool = match l {
        case Nil     => true
        case x :: xs => if (length(x) == len) uniformHelper(xs, len) else false
    }

    ///
    /// Helper function for `transpose`.
    ///
    def transposeHelper(l: List[List[a]], len: Int32): List[List[a]] = match l {
        case Nil     => repeat(len, Nil)
        case x :: xs => applyListHelper(x, transposeHelper(xs, len))
    }

    ///
    /// Helper function for `transpose`.
    ///
    def applyListHelper(l1: List[a], l2: List[List[a]]): List[List[a]] = match (l1, l2) {
        case (Nil, Nil)         => Nil
        case (x :: xs, y :: ys) => (x :: y) :: applyListHelper(xs, ys)
        case _                  => unreachable!()
    }

    ///
    /// Returns `true` if and only if `l1` is a prefix of `l2`.
    ///
    @Time(length(l1)) @Space(1)
    pub def isPrefixOf(l1: List[a], l2: List[a]): Bool with Eq[a] = match (l1, l2) {
        case (Nil, _)           => true
        case (_, Nil)           => false
        case (x :: xs, y :: ys) => if (x == y) isPrefixOf(xs, ys) else false
    }

    ///
    /// Returns `true` if and only if `l1` is an infix of `l2`.
    ///
    @Time(length(l1)) @Space(1)
    pub def isInfixOf(l1: List[a], l2: List[a]): Bool with Eq[a] = match (l1, l2) {
        case (Nil, _)     => true
        case (_, Nil)     => false
        case (_, _ :: ys) => if (isPrefixOf(l1, l2)) true else isInfixOf(l1, ys)
    }

    ///
    /// Returns `true` if and only if `l1` is a suffix of `l2`.
    ///
    @Time(length(l1)) @Space(Int32.max(length(l1), length(l2)))
    pub def isSuffixOf(l1: List[a], l2: List[a]): Bool with Eq[a] = isPrefixOf(reverse(l1), reverse(l2))

    ///
    /// Returns the result of applying `combine` to all the elements in `l`, using `empty` as the initial value.
    ///
    pub def fold(l: List[a]): a with Monoid[a] = Foldable.fold(l)

    ///
    /// Applies `f` to a start value `s` and all elements in `l` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b & ef, s: b, l: List[a]): b & ef =
         def loop(t, l2) = match l2 {
            case Nil     => t
            case x :: xs => loop(f(t, x), xs)
         };
         loop(s, l)

    ///
    /// Applies `f` to a start value `s` and all elements in `l` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b & ef, s: b, l: List[a]): b & ef =
        def loop(l2, k) = match l2 {
            case Nil     => k(s)
            case x :: xs => loop(xs, ks -> k(f(x, ks)))
        };
        loop(l, ks -> ks as & ef) // NB: Cast required because the identity continuation is pure,
                                  // but the intermediate continuations are effect polymorphic

    ///
    /// Lazily applies `f` to a start value `s` and all elements in `l` going from right to left.
    /// The function f must be pure.
    ///
    /// That is, the result is of the form: `f(x1, ...lazy f(xn-1, lazy f(xn, s))...)`.
    /// The foldRightL function exists to allow early termination of a fold.
    ///
    pub def foldRightL(f: (a, Lazy[b]) -> b, s: b, l: List[a]): b =
        def loop(l2) = match l2 {
            case Nil     => s
            case y :: ys => f(y, lazy loop(ys))
        };
        loop(l)

    ///
    /// Applies `f` to all elements in `l` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(x1, x2), x3)..., xn))`
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def reduceLeft(f: (a, a) -> a & ef, l: List[a]): Option[a] & ef = match l {
        case Nil     => None
        case x :: xs => Some(foldLeft(f, x, xs))
    }

    ///
    /// Applies `f` to all elements in `l` going from right to left until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(x1, ...f(xn-2, f(xn-1, xn))...))`
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def reduceRight(f: (a, a) -> a & ef, l: List[a]): Option[a] & ef = match l {
        case Nil     => None
        case x :: xs => match reduceRight(f, xs) {
            case None    => Some(x)
            case Some(v) => Some(f(x, v))
        }
    }

    ///
    /// Returns the number of elements in `l` that satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(l)) @Space(space(f) * length(l))
    pub def count(f: a -> Bool, l: List[a]): Int32 =
        foldLeft((acc, x) -> if (f(x)) acc + 1 else acc, 0, l)

    ///
    /// Returns the concatenation of the elements in `l`.
    ///
    @Time(length(l)) @Space(length(l))
    pub def flatten(l: List[List[a]]): List[a] = flattenHelper(l, ks -> ks)

    ///
    /// Helper function for `flatten`.
    ///
    def flattenHelper(l: List[List[a]], k: List[a] -> List[a]): List[a] = match l {
        case Nil     => k(Nil)
        case x :: xs => flattenHelper(xs, ks -> k(x ::: ks))
    }

    ///
    /// Returns `true` if and only if at least one element in `l` satisfies the predicate `f`.
    ///
    /// Returns `false` if `l` is empty.
    ///
    @Time(time(f) * length(l)) @Space(space(f))
    pub def exists(f: a -> Bool & ef, l: List[a]): Bool & ef =
        def loop(l2) = match l2 {
            case Nil     => false
            case x :: xs => if (f(x)) true else loop(xs)
        };
        loop(l)

    ///
    /// Returns `true` if and only if all elements in `l` satisfy the predicate `f`.
    ///
    /// Returns `true` if `l` is empty.
    ///
    @Time(time(f) * length(l)) @Space(space(f))
    pub def forall(f: a -> Bool & ef, l: List[a]): Bool & ef =
        def loop(l2) = match l2 {
            case Nil     => true
            case x :: xs => if (not f(x)) false else loop(xs)
        };
        loop(l)

    ///
    /// Returns a list of every element in `l` that satisfies the predicate `f`.
    ///
    @Time(time(f) * length(l)) @Space(space(f) * length(l))
    pub def filter(f: a -> Bool & ef, l: List[a]): List[a] & ef =
        def loop(l2, k) = match l2 {
            case Nil     => k(Nil)
            case x :: xs => if (f(x)) loop(xs, ks -> k(x :: ks)) else loop(xs, k)
        };
        loop(l, ks -> ks as & ef) // NB: Cast required because the identity continuation is pure,
                                  // but the intermediate continuations are effect polymorphic

    ///
    /// Returns the sublist of `l` without the last element.
    /// Returns `None` if the list `l` is `Nil`.
    ///
    @Time(length(l)) @Space(length(l))
    pub def init(l: List[a]): Option[List[a]] = initHelper(l, Nil)

    ///
    /// Helper function for `init`.
    ///
    def initHelper(l: List[a], acc: List[a]): Option[List[a]] = match l {
        case Nil      => None
        case _ :: Nil => Some(reverse(acc))
        case x :: xs  => initHelper(xs, x :: acc)
    }

    ///
    /// Returns the sublist of `l` from index `b` (inclusive) to index `e` (exclusive).
    ///
    /// That is, an element at index `i` in `l` is part of the returned sublist if and only if `i >= b` and `i < e`.
    /// Note: Indices that are out of bounds in `l` are not considered (i.e. slice(b, e, l) = slice(max(0,b), min(length(l),e), l)).
    ///
    @Time(e - b) @Space(e - b)
    pub def slice(b: Int32, e: Int32, l: List[a]): List[a] =
        if (b < e) sliceHelper(b, e, l, 0, ks -> ks) else Nil

    ///
    /// Helper function for `slice`.
    ///
    /// Precondition:  b < e
    ///
    def sliceHelper(b: Int32, e: Int32, l: List[a], i: Int32, k: List[a] -> List[a]): List[a] = match l {
        case Nil     => k(Nil)
        case x :: xs => match i {
            case pos if (pos < b)  => sliceHelper(b, e, xs, i + 1, k)
            case pos if (pos >= e) => k(Nil)
            case _                 => sliceHelper(b, e, xs, i + 1, ks -> k(x :: ks))
        }
    }

    ///
    /// Returns a pair of lists `(l1, l2)`.
    ///
    /// `l1` contains all elements of `l` that satisfy the predicate `f`.
    /// `l2` contains all elements of `l` that do not satisfy the predicate `f`.
    ///
    @Time(time(f) * length(l)) @Space(space(f) * length(l))
    pub def partition(f: a -> Bool & ef, l: List[a]): (List[a], List[a]) & ef = partitionHelper(f, l, (ks, ls) -> (ks, ls))

    ///
    /// Helper function for `partition`.
    ///
    def partitionHelper(f: a -> Bool & ef, l: List[a], k: (List[a], List[a]) -> (List[a], List[a])): (List[a], List[a]) & ef = match l {
        case Nil     => k(Nil, Nil)
        case x :: xs =>
            if (f(x))
                partitionHelper(f, xs, (ks, ls) -> k(x :: ks, ls))
            else
                partitionHelper(f, xs, (ks, ls) -> k(ks, x :: ls))
    }

    ///
    /// Returns a pair of lists `(l1, l2)`.
    ///
    /// `l1` is the longest prefix of `l` that satisfies the predicate `f`.
    /// `l2` is the remainder of `l`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(l)) @Space(space(f) * length(l))
    pub def span(f: a -> Bool, l: List[a]): (List[a], List[a]) = spanHelper(f, l, (ks, ls) -> (ks, ls))

    ///
    /// Helper function for `span`.
    ///
    def spanHelper(f: a -> Bool, l: List[a], k: (List[a], List[a]) -> (List[a], List[a])): (List[a], List[a]) = match l {
        case Nil     => k(Nil, Nil)
        case x :: xs =>
            if (f(x))
                spanHelper(f, xs, (ks, ls) -> k(x :: ks, ls))
            else
                k(Nil, l)
    }

    ///
    /// Returns `l` without the first `n` elements.
    ///
    /// Returns `Nil` if `n > length(l)`.
    /// Returns `l` if `n < 0`.
    ///
    @Time(n) @Space(length(l) - n)
    pub def drop(n: Int32, l: List[a]): List[a] =
        def loop(m, l2) =
            if (m <= 0)
                l2
            else
                match (m, l2) {
                    case (_, Nil)     => Nil
                    case (i, _ :: xs) => loop(i - 1, xs)
                };
        loop(n, l)

    ///
    /// Returns `l` without the longest prefix that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(l)) @Space(space(f))
    pub def dropWhile(f: a -> Bool, l: List[a]): List[a] = match l {
        case Nil     => Nil
        case x :: xs => if (f(x)) dropWhile(f, xs) else l
    }

    ///
    /// Returns the first `n` elements of `l`.
    ///
    /// Returns `l` if `n > length(l)`.
    /// Returns `Nil` if `n < 0`.
    ///
    @Time(n) @Space(n)
    pub def take(n: Int32, l: List[a]): List[a] =
        def loop(m, l2, k) =
            if (m < 0)
                k(Nil)
            else
                match (m, l2) {
                    case (_, Nil)     => k(Nil)
                    case (0, _)       => k(Nil)
                    case (i, x :: xs) => loop(i - 1, xs, ks -> k(x :: ks))
                };
        loop(n, l, ks -> ks)

    ///
    /// Returns the longest prefix of `l` that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(l)) @Space(space(f) * length(l))
    pub def takeWhile(f: a -> Bool, l: List[a]): List[a] =
        takeWhileHelper(f, l, ks -> ks)

    ///
    /// Helper function for `takeWhile`.
    ///
    def takeWhileHelper(f: a -> Bool, l: List[a], k: List[a] -> List[a]): List[a] = match l {
        case Nil     => k(Nil)
        case x :: xs => if (f(x)) takeWhileHelper(f, xs, ks -> k(x :: ks)) else k(Nil)
    }

    ///
    /// Partitions `l` into sublists such that for any two elements `x` and `y` in a sublist, `f(x, y)` is true.
    ///
    /// A sublist is created by iterating through the remaining elements of `l` from left to right and adding an
    /// element to the sublist if and only if doing so creates no conflicts with the elements already in the sublist.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(l)) @Space(space(f) * length(l))
    pub def groupBy(f: (a, a) -> Bool, l: List[a]): List[List[a]] = match l {
        case Nil => Nil
        case x :: xs =>
            let (r1, r2) = extractHelper(f, xs, x :: Nil, Nil);
            r1 :: groupBy(f, r2)
    }

    ///
    /// Helper function for `groupBy`.
    ///
    def extractHelper(f: (a, a) -> Bool, l1: List[a], l2: List[a], l3: List[a]): (List[a], List[a]) = match l1 {
        case Nil     => (reverse(l2), reverse(l3))
        case x :: xs => if (agreeHelper(f, x, l2)) extractHelper(f, xs, x :: l2, l3) else extractHelper(f, xs, l2, x :: l3)
    }

    ///
    /// Helper function for `groupBy`.
    ///
    def agreeHelper(f: (a, a) -> Bool, a: a, l: List[a]): Bool = match l {
        case Nil     => true
        case x :: xs => if (f(x, a) and f(a, x)) agreeHelper(f, a, xs) else false
    }

    ///
    /// Returns a list where the element at index `i` is `(a, b)` where
    /// `a` is the element at index `i` in `l1` and `b` is the element at index `i` in `l2`.
    ///
    /// If either `l1` or `l2` becomes depleted, then no further elements are added to the resulting list.
    ///
    @Time(Int32.min(length(l1), length(l2))) @Space(Int32.min(length(l1), length(l2)))
    pub def zip(l1: List[a], l2: List[b]): List[(a, b)] = zipHelper(l1, l2, ks -> ks)

    ///
    /// Helper function for `zip`.
    ///
    def zipHelper(l1: List[a], l2: List[b], k: List[(a,b)] -> List[(a, b)]): List[(a, b)] = match (l1, l2) {
            case (x :: xs, y :: ys) => zipHelper(xs, ys, ks -> k((x, y) :: ks))
            case _                  => k(Nil)
        }

    ///
    /// Returns a list where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `l1` and `b` is the element at index `i` in `l2`.
    ///
    /// If either `l1` or `l2` becomes depleted, then no further elements are added to the resulting list.
    ///
    @Time(time(f) * Int32.min(length(l1), length(l2))) @Space(space(f) * Int32.min(length(l1), length(l2)))
    pub def zipWith(f: (a, b) -> c & ef, l1: List[a], l2: List[b]): List[c] & ef = zipWithHelper(f, l1, l2, ks -> ks)

    ///
    /// Helper function for `zipWith`.
    ///
    def zipWithHelper(f: (a, b) -> c & ef, l1: List[a], l2: List[b], k: List[c] -> List[c]): List[c] & ef = match (l1, l2) {
        case (x :: xs, y :: ys) => {
            let z = f(x, y);
            zipWithHelper(f, xs, ys, ks -> k(z :: ks))
        }
        case _ => k(Nil)
    }

    ///
    /// Returns a pair of lists, the first containing all first components in `l`
    /// and the second containing all second components in `l`.
    ///
    @Time(length(l)) @Space(length(l))
    pub def unzip(l: List[(a, b)]): (List[a], List[b]) =
        def loop(l2, k) = match l2 {
            case Nil            => k(Nil, Nil)
            case (x1, x2) :: xs => loop(xs, (ks1, ks2) -> k(x1 :: ks1, x2 :: ks2))
        };
        loop(l, (ks1, ks2) -> (ks1, ks2))

    ///
    /// Returns a list where the element at index `i` is `(a, b, c)` where
    /// `a` is the element at index `i` in `l1`, `b` is the element at index `i` in `l2`
    /// and `c` is the element at index `i` in `l3`.
    ///
    /// If any one of `l1`, `l2` or `l3` become depleted, then no further elements are added to the resulting list.
    ///
    pub def zip3(l1: List[a], l2: List[b], l3: List[c]): List[(a, b, c)] =
        zipWith3((x, y, z) -> (x, y, z), l1, l2, l3)

    ///
    /// Returns a list where the element at index `i` is `f(a, b, c)` where
    /// `a` is the element at index `i` in `l1`, `b` is the element at index `i` in `l2`
    /// and `c` is the element at index `i` in `l3`.
    ///
    /// If any one of `l1`, `l2` or `l3` become depleted, then no further elements are added to the resulting list.
    ///
    pub def zipWith3(f: (a, b, c) -> d & ef, l1: List[a], l2: List[b], l3: List[c]): List[d] & ef =
        zipWith3Helper(f, l1, l2, l3, ks -> ks)

    ///
    /// Helper function for `zipWith3`.
    ///
    def zipWith3Helper(f: (a, b, c) -> d & ef, l1: List[a], l2: List[b], l3: List[c], k: List[d] -> List[d]): List[d] & ef =
        match (l1, l2, l3) {
            case (x :: xs, y :: ys, z :: zs) => {
                let r = f(x, y, z);
                zipWith3Helper(f, xs, ys, zs, ks -> k(r :: ks))
            }
            case _ => k(Nil)
        }

    ///
    /// Returns a triple of lists, the first containing all first components in `l`
    /// the second containing all second components in `l` and the third containing all
    /// third components in `l`.
    ///
    pub def unzip3(l: List[(a, b, c)]): (List[a], List[b], List[c]) = unzip3Helper(l, (ks1, ks2, ks3) -> (ks1, ks2, ks3))

    ///
    /// Helper function for `unzip3`.
    ///
    def unzip3Helper(l: List[(a, b, c)], k: (List[a], List[b], List[c]) -> (List[a], List[b], List[c])): (List[a], List[b], List[c]) = match l {
        case Nil                => k(Nil, Nil, Nil)
        case (x1, x2, x3) :: xs => unzip3Helper(xs, (ks1, ks2, ks3) -> k(x1 :: ks1, x2 :: ks2, x3 :: ks3))
    }

    ///
    /// Helper function for `flatMap2`.
    ///
    def flatMap2Helper(f: (a, b) -> List[c] & ef, l1: List[a], l2: List[b], k: List[c] -> List[c]): List[c] & ef = match (l1, l2) {
        case (x :: xs, y :: ys) => {
            let z = f(x, y);
            flatMap2Helper(f, xs, ys, ks -> k(z ::: ks))
        }
        case _ => k(Nil)
    }

    ///
    /// Alias for `foldLeft2`.
    ///
    @Time(time(f) * Int32.min(length(l1), length(l2))) @Space(space(f) * Int32.min(length(l1), length(l2)))
    pub def fold2(f: (c, a, b) -> c & ef, c: c, l1: List[a], l2: List[b]): c & ef = foldLeft2(f, c, l1, l2)

    ///
    /// Accumulates the result of applying `f` pairwise to the elements of `l1` and `l2`
    /// starting with the initial value `c` and going from left to right.
    ///
    @Time(time(f) * Int32.min(length(l1), length(l2))) @Space(space(f) * Int32.min(length(l1), length(l2)))
    pub def foldLeft2(f: (c, a, b) -> c & ef, c: c, l1: List[a], l2: List[b]): c & ef = match (l1, l2) {
        case (x :: xs, y :: ys) => foldLeft2(f, f(c, x, y), xs, ys)
        case _ => c
    }

    ///
    /// Accumulates the result of applying `f` pairwise to the elements of `l1` and `l2`
    /// starting with the initial value `c` and going from right to left.
    ///
    @Time(time(f) * Int32.min(length(l1), length(l2))) @Space(space(f) * Int32.min(length(l1), length(l2)))
    pub def foldRight2(f: (a, b, c) -> c & ef, c: c, l1: List[a], l2: List[b]): c & ef =
        let len1 = length(l1);
        let len2 = length(l2);
            foldRight2Helper(f, c, drop(len1 - len2, l1), drop(len2 - len1, l2))

    ///
    /// Helper function for `foldRight2`.
    ///
    def foldRight2Helper(f: (a, b, c) -> c & ef, c: c, l1: List[a], l2: List[b]): c & ef = match (l1, l2) {
        case (x :: xs, y :: ys) => f(x, y, foldRight2(f, c, xs, ys))
        case _ => c
    }

    ///
    /// Collects the results of applying the partial function `f` to every element in `l`.
    ///
    @Time(time(f) * length(l)) @Space(space(f) * length(l))
    pub def filterMap(f: a -> Option[b] & ef, l: List[a]): List[b] & ef =
        filterMapHelper(f, l, ks -> ks)

    ///
    /// Helper function for `filterMap`.
    ///
    def filterMapHelper(f: a -> Option[b] & ef, l: List[a], k: List[b] -> List[b]): List[b] & ef = match l {
        case Nil     => k(Nil)
        case x :: xs => match f(x) {
            case None    => filterMapHelper(f, xs, k)
            case Some(v) => filterMapHelper(f, xs, vs -> k(v :: vs))
        }
    }
    ///
    /// Returns the first non-None result of applying the partial function `f` to each element of `l`.
    ///
    /// Returns `None` if every element of `l` is `None`.
    ///
    @Time(time(f) * length(l)) @Space(space(f))
    pub def findMap(f: a -> Option[b] & ef, l: List[a]): Option[b] & ef = match l {
        case Nil     => None
        case x :: xs => match f(x) {
            case None    => findMap(f, xs)
            case Some(v) => Some(v)
        }
    }

    ///
    /// Render the list `l` as a String. Elements are rendered with the
    /// function `f` and separated with the string `sep`.
    ///
    @Time(time(f) * length(l)) @Space(space(f) * length(l))
    pub def formatWith(f: a -> String, sep: String, l: List[a]): String =
        formatWithHelper(f, sep, l) as & Pure

    ///
    /// Helper function for `formatWith`.
    ///
    def formatWithHelper(f: a -> String, sep: String, l: List[a]): String & Impure =
        let sb = StringBuilder.new();
        match l {
            case Nil     => ()
            case x :: xs => {
                StringBuilder.appendString!(sb, f(x));
                foreach(x1 -> {StringBuilder.appendString!(sb, sep); StringBuilder.appendString!(sb, f(x1))}, xs)
            }
        };
        StringBuilder.toString(sb)

    ///
    /// Returns the elements of `l` as a lazy list.
    ///
    pub def toLazyList(l: List[a]): LazyList[a] = match l {
        case Nil     => ENil
        case x :: xs => LCons(x, lazy toLazyList(xs))
    }

    ///
    /// Returns the list `l` as a chain.
    ///
    pub def toChain(l: List[a]): Chain[a] =
        List.foldLeft(Chain.snoc, Chain.empty(), l)

    ///
    /// Returns `l` as a mutable list.
    ///
    pub def toMutList(l: List[a]): MutList[a] & Impure = {
        let v = MutList.new();
        foreach(x -> MutList.push!(x, v), l);
        v
    }

    ///
    /// Returns the list `l` as a set.
    ///
    @Time(length(l)) @Space(length(l))
    pub def toSet(l: List[a]): Set[a] with Order[a] = foldRight(Set.insert, Set.empty(), l)

    ///
    /// Returns the association list `l` as a map.
    ///
    /// If `l` contains multiple mappings with the same key, `toMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    @Time(length(l)) @Space(length(l))
    pub def toMap(l: List[(a, b)]): Map[a, b] with Order[a] = foldRight((x, m) -> Map.insert(fst(x), snd(x), m), Map.empty(), l)

    ///
    /// Returns a map with elements of `s` as keys and `f` applied as values.
    ///
    /// If `s` contains multiple mappings with the same key, `toMapWith` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    pub def toMapWith(f: a -> b, l: List[a]): Map[a, b] with Order[a] =
            List.foldRight((x, m) -> Map.insert(x, f(x), m), Map.empty(), l)

    ///
    /// Applies `f` to every element of `l`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def foreach(f: a -> Unit & ef, l: List[a]): Unit & ef = match l {
        case Nil     => ()
        case x :: xs => f(x); foreach(f, xs)
    }

    ///
    /// Returns the list `l` as an array.
    ///
    @Time(length(l)) @Space(length(l))
    pub def toArray(l: List[a]) : Array[a] & Impure = match head(l) {
        case None    => []
        case Some(x) =>
            let a = Array.new(x, length(l));
            let f = (i, b) -> { a[i] = b; i + 1 };
            foldLeft(f, 0, l);
            a
        }

    ///
    /// Sort list `l` so that elements are ordered from low to high according to their `Order` instance.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `l`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sort(l: List[a]): List[a] with Order[a] =
        (l |> toArray |> Array.sort |> Array.toList) as & Pure

    /// Sort list `l` so that elements are ordered from low to high according to the `Order` instance
    /// for the values obtained by applying `f` to each element.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `l`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortBy(f: a -> b, l: List[a]): List[a] with Order[b] =
        (l |> toArray |> Array.sortBy(f) |> Array.toList) as & Pure

    ///
    /// Sort list `l` so that elements are ordered from low to high according to the comparison function `cmp`.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `l`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortWith(cmp: (a,a) -> Comparison, l: List[a]): List[a] =
        (l |> toArray |> Array.sortWith(cmp) |> Array.toList) as & Pure

    ///
    /// Build a list by applying `f` to the seed value `st`.
    ///
    /// `f` should return `Some(a,st1)` to signal a new list element `a` and a new seed value `st1`.
    ///
    /// `f` should return `None` to signal the end of building the list.
    ///
    pub def unfold(f: s -> Option[(a, s)] & ef, st: s): List[a] & ef = unfoldHelper(f, st, ks -> ks)

    ///
    /// Helper function for `unfold`.
    ///
    def unfoldHelper(f: s -> Option[(a, s)] & ef, st: s, k: List[a] -> List[a]): List[a] & ef = match f(st) {
        case None         => k(Nil)
        case Some(a, st1) => unfoldHelper(f, st1, ks -> k(a :: ks))
    }

    ///
    /// Build a list by applying the function `next` to `()`. `next` is expected to encapsulate
    /// a stateful resource such as a file handle that can be iterated.
    ///
    /// `next` should return `Some(a)` to signal a new list element `a`.
    ///
    /// `next` should return `None` to signal the end of building the list.
    ///
    pub def unfoldWithIter(next: Unit -> Option[a] & ef): List[a] & ef = unfoldWithIterHelper(next, ks -> ks)

    ///
    /// Helper function for `unfoldWithIter`.
    ///
    def unfoldWithIterHelper(next: Unit -> Option[a] & ef, k: List[a] -> List[a]): List[a] & ef = match next() {
        case None    => k(Nil)
        case Some(a) => unfoldWithIterHelper(next, ks -> k(a :: ks))
    }


    ///
    /// Build a list by applying the function `next` to `()`. `next` is expected to encapsulate
    /// a stateful resource such as a file handle that can be iterated.
    ///
    /// `next` should return `Ok(Some(a)` to signal a new list element `Ok(a)`.
    ///
    /// `next` should return `Ok(None)` to signal the end of building the list.
    ///
    /// `next` should return `Err(e)` to signal that an error occurred. The function returns `Err(e)`.
    ///
    pub def unfoldWithOkIter(next: Unit -> Result[Option[a], e] & ef): Result[List[a], e] & ef = unfoldWithOkIterHelper(next, ks -> ks)

    ///
    /// Helper function for `unfoldWithOkIter`.
    ///
    def unfoldWithOkIterHelper(next: Unit -> Result[Option[a], e] & ef, k: Result[List[a], e] -> Result[List[a], e]): Result[List[a], e] & ef = match next() {
        case Ok(None)    => k(Ok(Nil))
        case Err(e)      => k(Err(e))
        case Ok(Some(a)) => unfoldWithOkIterHelper(next, Result.flatMap(ks -> k(Ok(a :: ks))))
    }

    ///
    /// Returns the list `l` with duplicates removed. The first occurence of
    /// an element is kept and except for the removal of subsequent duplicates
    /// the order of `l` is preserved.
    ///
    /// `distinct` uses the Flix's builtin equality test. Use `distinctWith` if you
    /// need a custom equality test.
    ///
    pub def distinct(l: List[a]): List[a] with Eq[a] =
        distinctHelper(l, Nil, ks -> ks)

    ///
    /// Helper function for `distinct`.
    ///
    def distinctHelper(l1: List[a], l2: List[a], k: List[a] -> List[a]): List[a] with Eq[a] = match l1 {
        case Nil     => k(Nil)
        case x :: xs =>
            if (memberOf(x, l2))
                distinctHelper(xs, l2, k)
            else
                distinctHelper(xs, (x :: l2), ks -> k(x :: ks))
    }

    ///
    /// Returns the list `l` with duplicates removed using the supplied function
    /// `f` for comparison. The first occurrence of an element is kept and except
    /// for the removal of subsequent duplicates the order of `l` is preserved.
    ///
    pub def distinctWith(f: (a, a) -> Bool, l: List[a]): List[a] =
        distinctWithHelper(f, l, Nil, ks -> ks)

    ///
    /// Helper function for `distinctWith`.
    ///
    def distinctWithHelper(f: (a, a) -> Bool, l1: List[a], l2: List[a], k: List[a] -> List[a]): List[a] = match l1 {
        case Nil     => k(Nil)
        case x :: xs =>
            if (memberOfWith(f, x, l2))
                distinctWithHelper(f, xs, l2, k)
            else
                distinctWithHelper(f, xs, (x :: l2), ks -> k(x :: ks))
    }

    ///
    /// Helper function for `distinctWith`.
    ///
    /// This is a generalization of `memberOf` that uses the function `f` for comparison.
    ///
    def memberOfWith(f: (a, a) -> Bool, a: a, l: List[a]): Bool = match l {
        case Nil     => false
        case x :: xs =>
            if (f(a, x))
                true
            else
                memberOfWith(f, a, xs)
    }

    ///
    /// Returns the sum of all elements in the list `l`.
    ///
    pub def sum(l: List[Int32]): Int32 =
        def loop(l2, acc) = match l2 {
            case Nil     => acc
            case x :: xs => loop(xs, x + acc)
        };
        loop(l, 0)

    ///
    /// Returns the elements of `l` as a `DemandList`.
    ///
    pub def toDemandList(l: List[a]): DemandList.DemandList[a, d] =
        use DemandList.{DemandList};
        match l {
            case Nil     => DemandList.ENil
            case x :: xs => DemandList.LList(lazy DemandList.LCons(x, lazy toDemandList(xs)))
        }

    ///
    /// Returns an iterator over `l`.
    ///
    pub def toIterator(l: List[a]): Iterator[a] & Impure =
        let cursor = ref l;
        let done = () -> match (deref cursor) {
            case Nil => true
            case _   => false
        };
        let next = () -> match (deref cursor) {
            case Nil     => bug!("Empty Iterator!")
            case x :: xs => {
                cursor := xs;
                x
            }
        };
        Iterator(done, next)

    ///
    /// Returns the association list `l` as a `DelayMap`.
    ///
    /// If `l` contains multiple mappings with the same key, `toDelayMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    pub def toDelayMap(l: List[(a, b)]): DelayMap[a, b] with Order[a] =
        List.foldRight((x, m) -> DelayMap.insert(fst(x), snd(x), m), DelayMap.empty(), l)

    ///
    /// Helper function for `traverse` and `sequence`.
    ///
    /// Builds an "applicative list" from a head of one applicative action and an
    /// applicative list of the tail.
    ///
    def consA(mx: f[a], ml: f[List[a]]): f[List[a]] with Applicative[f] =
        (((x, xs) -> x :: xs) `Functor.map` mx) `Applicative.ap` ml

    ///
    /// Returns the result of running all the actions in the list `l`.
    ///
    pub def sequence(l: List[m[a]]): m[List[a]] with Applicative[m] =
        def loop(l2, k) = match l2 {
            case Nil      => k(Applicative.point(Nil))
            case mx :: rs => loop(rs, ks -> k(consA(mx, ks)))
        };
        loop(l, ks -> ks)

    ///
    /// Returns the result of applying the applicative mapping function `f` to all the elements of the
    /// list `l`.
    ///
    pub def traverse(f: a -> m[b] & ef, l: List[a]): m[List[b]] & ef with Applicative[m] =
        def loop(l2, k) = match l2 {
            case Nil     => k(Applicative.point(Nil))
            case x :: xs => loop(xs, ks -> k(consA(f(x), ks)))
        };
        loop(l, ks -> ks as & ef)
}
