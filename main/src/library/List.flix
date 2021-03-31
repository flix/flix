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
enum List[t] {
    case Nil,
    case Cons(t, List[t])
}

instance ToString[List[a]] with ToString[a] {
    pub def toString(xs: List[a]): String =
        List.foldRight((x, acc) -> "${x} :: ${acc}", "Nil", xs)
}

instance Hash[List[a]] with Hash[a] {
    pub def hash(xs: List[a]): Int =
        List.foldLeft((acc, x) -> acc * 31 + Hash.hash(x), 37199, xs)
}

instance Eq[List[a]] with Eq[a] {
    pub def eq(xs: List[a], ys: List[a]): Bool = match (xs, ys) {
        case (Nil, Nil) => true
        case (x :: rs, y :: qs) => if (x != y) false else rs == qs
        case _ => false
    }
}

instance Order[List[a]] with Order[a] {

    ///
    /// Compares `xs` and `ys` lexicographically.
    ///
    pub def compare(xs: List[a], ys: List[a]): Comparison = match (xs, ys) {
        case (_ :: _, Nil) => GreaterThan
        case (Nil, Nil) => EqualTo
        case (Nil, _ :: _) => LessThan
        case (z :: zs, w :: ws) =>
            let cmp = z <=> w;
            if (cmp == EqualTo) zs <=> ws else cmp
    }

}

instance Functor[List] {
    pub def map(f: a -> b & e, xs: List[a]): List[b] & e = List.map(f, xs)
}

instance Foldable[List] {
    pub def foldLeft(f: (b, a) -> b & e, s: b, xs: List[a]): b & e = List.foldLeft(f, s, xs)
    pub def foldRight(f: (a, b) -> b & e, s: b, xs: List[a]): b & e = List.foldRight(f, s, xs)
}

namespace List {

    ///
    /// Returns true if and only if `xs` is the empty list, i.e. `Nil`.
    ///
    @Time(1) @Space(1)
    pub def isEmpty(xs: List[a]): Bool = match xs {
        case Nil => true
        case _ => false
    }

    ///
    /// Returns `Some(x)` if `x` is the first element of `xs`.
    ///
    /// Returns `None` if `xs` is empty.
    ///
    @Time(1) @Space(1)
    pub def head(xs: List[a]): Option[a] = match xs {
        case Nil => None
        case x :: _ => Some(x)
    }

    ///
    /// Returns `Some(x)` if `x` is the last element of `xs`.
    ///
    /// Returns `None` if `xs` is empty.
    ///
    @Time(length(xs)) @Space(1)
    pub def last(xs: List[a]): Option[a] = match xs {
        case Nil => None
        case x :: Nil => Some(x)
        case _ :: rs => last(rs)
    }

    ///
    /// Returns the length of `xs`.
    ///
    @Time(length(xs)) @Space(1)
    pub def length(xs: List[a]): Int32 = lengthHelper(xs, 0)

    ///
    /// Helper function for `length`.
    ///
    def lengthHelper(xs: List[a], ac: Int32): Int32 = match xs {
        case _ :: rs => lengthHelper(rs, ac + 1)
        case Nil => ac
    }

    ///
    /// Returns `ys` appended to `xs`.
    ///
    /// The infix operator `:::` is an alias for `append` (`xs ::: ys = append(xs, ys)`).
    ///
    @Time(length(xs)) @Space(length(xs))
    pub def append(xs: List[a], ys: List[a]): List[a] = {
        foldRight((x, acc) -> x :: acc, ys, xs)
    }

    ///
    /// Returns `true` if and only if `xs` contains the element `a`.
    ///
    @Time(length(xs)) @Space(1)
    pub def memberOf(a: a, xs: List[a]): Bool with Eq[a] = match xs {
        case Nil => false
        case x :: rs => if (x == a) true else memberOf(a, rs)
    }

    ///
    /// Optionally finds the smallest element of `xs` according to the `Order` on `a`.
    ///
    /// Returns `None` if `xs` is empty.
    ///
    pub def minimum(xs: List[a]): Option[a] with Order[a] =
        reduceLeft(Order.min, xs)

    ///
    /// Optionally finds the smallest element of `xs` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `xs` is empty.
    ///
    pub def minimumBy(cmp: (a, a) -> Comparison, xs: List[a]): Option[a] =
        reduceLeft(Order.minBy(cmp), xs)

    ///
    /// Optionally finds the largest element of `xs` according to the `Order` on `a`.
    ///
    /// Returns `None` if `xs` is empty.
    ///
    pub def maximum(xs: List[a]): Option[a] with Order[a] =
        reduceLeft(Order.max, xs)

    ///
    /// Optionally finds the largest element of `xs` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `xs` is empty.
    ///
    pub def maximumBy(cmp: (a, a) -> Comparison, xs: List[a]): Option[a] =
        reduceLeft(Order.maxBy(cmp), xs)

    ///
    /// Returns the position of `a` in `xs`.
    ///
    /// Returns `-1` if `a` does not exist in `xs`.
    ///
    @Time(length(xs)) @Space(1)
    pub def indexOf(a: a, xs: List[a]): Int32 with Eq[a] = indexOfHelper(a, xs, 0)

    ///
    /// Helper function for `indexOf`.
    ///
    def indexOfHelper(a: a, xs: List[a], acc: Int32): Int32 with Eq[a] = match xs {
        case Nil => -1
        case x :: rs => if (x == a) acc else indexOfHelper(a, rs, acc+1)
    }

    ///
    /// Alias for `findLeft`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(xs)) @Space(space(f))
    pub def find(f: a -> Bool, xs: List[a]): Option[a] = findLeft(f, xs)

    ///
    /// Optionally returns the first element of `xs` that satisfies the predicate `f` when searching from left to right.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(xs)) @Space(space(f))
    pub def findLeft(f: a -> Bool, xs: List[a]): Option[a] = match xs {
        case Nil => None
        case x :: rs => if (f(x)) Some(x) else findLeft(f, rs)
    }

    ///
    /// Optionally returns the first element of `xs` that satisfies the predicate `f` when searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(xs)) @Space(space(f))
    pub def findRight(f: a -> Bool, xs: List[a]): Option[a] = match xs {
        case Nil => None
        case x :: rs => Option.withDefault(findRight(f, rs), if (f(x)) Some(x) else None)
    }






    ///
    /// Returns a list of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns `Nil` if `b >= e`.
    ///
    @Time(e - b) @Space(e - b)
    pub def range(b: Int32, e: Int32): List[Int32] = rangeHelper(b, e, xs -> xs)

    ///
    /// Helper function for `range`.
    ///
    def rangeHelper(b: Int32, e: Int32, k: List[Int32] -> List[Int32]): List[Int32] =
        if (b >= e) k(Nil) else rangeHelper(b + 1, e, xs -> k(b :: xs))

    ///
    /// Returns a list with the element `a` repeated `n` times.
    ///
    /// Returns `Nil` if `n < 0`.
    ///
    @Time(n) @Space(n)
    pub def repeat(a: a, n: Int32): List[a] = repeatHelper(a, n, xs -> xs)

    ///
    /// Helper function for `repeat`.
    ///
    def repeatHelper(a: a, n: Int32, k: List[a] -> List[a]): List[a] =
        if (n <= 0) k(Nil) else repeatHelper(a, n - 1, xs -> k(a :: xs))


    ///
    /// Alias for `scanLeft`.
    ///
    pub def scan(f: (b, a) -> b & e, s: b, xs: List[a]): List[b] & e = scanLeft(f, s, xs)

    ///
    /// Accumulates the result of applying `f` to `xs` going left to right.
    ///
    /// That is, the result is of the form: `s :: f(s, x1) :: f(f(s, x1), x2)  ...`.
    ///
    pub def scanLeft(f: (b, a) -> b & e, s: b, xs: List[a]): List[b] & e = s :: scanLeftHelper(f, s, xs)

    ///
    /// Helper function for `scanLeft`.
    ///
    def scanLeftHelper(f: (b, a) -> b & e, s: b, xs: List[a]): List[b] & e = match xs {
        case Nil => Nil
        case x :: rs => f(s, x) :: scanLeftHelper(f, f(s, x), rs)
    }

    ///
    /// Accumulates the result of applying `f` to `xs` going right to left.
    ///
    /// That is, the result is of the form: `... f(xn-1, f(xn, s)) :: f(xn, s) :: s`.
    ///
    pub def scanRight(f: (a, b) -> b & e, s: b, xs: List[a]): List[b] & e = match xs {
        case Nil => s :: Nil
        case x :: rs => match scanRight(f, s, rs) {
            case y :: ys => f(x, y) :: y :: ys
            case _ => unreachable!()
        }
    }






    ///
    /// Returns the result of applying `f` to every element in `xs`.
    ///
    /// That is, the result is of the form: `f(x1) :: f(x2) :: ...`.
    ///
    @Time(time(f) * length(xs)) @Space(space(f) * length(xs))
    pub def map(f: a -> b & e, xs: List[a]): List[b] & e =
        mapHelper(f, xs, ys -> ys)

    ///
    /// Helper function for `map`.
    ///
    def mapHelper(f: a -> b & e, xs: List[a], k: List[b] -> List[b]): List[b] & e = match xs {
        case Nil => k(Nil)
        case x :: rs => {
            let a = f(x);
            mapHelper(f, rs, ys -> k(a :: ys))
        }
    }


    ///
    /// Returns the result of applying `f` to every element in `xs` along with that element's index.
    ///
    /// That is, the result is of the form: `f(x1, 0) :: f(x2, 1) :: ...`.
    ///
    @Time(time(f) * length(xs)) @Space(space(f) * length(xs))
    pub def mapWithIndex(f: (a, Int32) -> b & e, xs: List[a]): List[b] & e = mapWithIndexHelper(f, xs, 0)

    ///
    /// Helper function for `mapWithIndex`.
    ///
    def mapWithIndexHelper(f: (a, Int32) -> b & e, xs: List[a], i: Int32): List[b] & e = match xs {
        case Nil => Nil
        case x :: rs => f(x, i) :: mapWithIndexHelper(f, rs, i+1)
    }

    ///
    /// Returns the result of applying `f` to every element in `xs` and concatenating the results.
    ///
    @Time(time(f) * length(xs)) @Space(time(f) * length(xs))
    pub def flatMap(f: a -> List[b] & e, xs: List[a]): List[b] & e = match xs {
        case Nil => Nil
        case x :: rs => f(x) ::: flatMap(f, rs)
    }

    ///
    /// Returns the reverse of `xs`.
    ///
    @Time(length(xs)) @Space(length(xs))
    pub def reverse(xs: List[a]): List[a] = reverseHelper(xs, Nil)

    ///
    /// Helper function for `reverse`.
    ///
    def reverseHelper(xs: List[a], acc: List[a]): List[a] = match xs {
        case Nil => acc
        case x :: rs => reverseHelper(rs, x :: acc)
    }

    ///
    /// Returns `xs` with its elements rotated `n` positions to the left.
    ///
    /// That is, returns a new list where the first `n mod length(xs)` elements in `xs`
    /// are the last `n mod length(xs)` elements of the new list.
    ///
    @Time(length(xs)) @Space(length(xs))
    pub def rotateLeft(n: Int32, xs: List[a]): List[a] =
        let len = length(xs);
            if (len == 0) xs else
                let rem = n % len;
                let rotate = if (rem < 0) rem + len else rem;
                    drop(rotate, xs) ::: take(rotate, xs)

    ///
    /// Returns `xs` with its elements rotated `n` positions to the right.
    ///
    /// That is, returns a new list where the last `n mod length(xs)` elements in `xs`
    /// are the first `n mod length(xs)` elements of the new list.
    ///
    @Time(length(xs)) @Space(length(xs))
    pub def rotateRight(n: Int32, xs: List[a]): List[a] = rotateLeft(-n, xs)

    ///
    /// Returns `xs` with the element at index `i` replaced by `a`.
    ///
    /// Returns `xs` if `i < 0` or `i > length(xs)-1`.
    ///
    @Time(i) @Space(i)
    pub def update(i: Int32, a: a, xs: List[a]): List[a] = match (i, xs) {
        case (_, Nil) => xs
        case (0, _ :: rs) => a :: rs
        case (p, x :: rs) => x :: update(p-1, a, rs)
    }

    ///
    /// Returns `xs` with every occurrence of `x` replaced by `y`.
    ///
    @Time(length(xs)) @Space(length(xs))
    pub def replace(x: a, y: a, xs: List[a]): List[a] with Eq[a] = map(e -> if (e == x) y else e, xs)

    ///
    /// Returns `ys` with the `n` elements starting at index `i` replaced with the elements of `xs`.
    ///
    /// If any of the indices `i, i+1, i+2, ... , i+n-1` are out of range in `ys` then no patching is done at these indices.
    /// If `xs` becomes depleted then no further patching is done.
    /// If patching occurs at index `i+j` in `ys`, then the element at index `j` in `xs` is used.
    ///
    @Time(n) @Space(length(ys))
    pub def patch(i: Int32, n: Int32, xs: List[a], ys: List[a]): List[a] = patchHelper(i, n, drop(-i, xs), ys, 0)

    ///
    /// Helper function for `patch`.
    ///
    def patchHelper(i: Int32, n: Int32, xs: List[a], ys: List[a], c: Int32): List[a] = match (xs, ys) {
        case (x :: qs, y :: rs) => if (c >= i and c < i+n) x :: patchHelper(i, n, qs, rs, c+1) else y :: patchHelper(i, n, xs, rs, c+1)
        case _ => ys
    }

    ///
    /// Returns all permutations of `xs` in lexicographical order by element indices in `xs`.
    ///
    /// That is, `xs` is the first permutation and `reverse(xs)` is the last permutation.
    ///
    @Time(Int32.factorial(length(xs))) @Space(Int32.factorial(length(xs)))
    pub def permutations(xs: List[a]): List[List[a]] = match xs {
        case Nil => Nil :: Nil
        case _ => permutationHelper(0, xs)
    }

    ///
    /// Helper function for `permutations`.
    /// Returns all permutations of `xs` starting with an element at or after index `i`.
    ///
    def permutationHelper(i: Int32, xs: List[a]): List[List[a]] =
        if (i == length(xs)) Nil
        else applyHelper(at(i, xs), permutations(removeIndex(i, xs))) ::: permutationHelper(i+1, xs)

    ///
    /// Helper function for `permutations`.
    ///
    def at(i: Int32, xs: List[a]): a = match (i, xs) {
        case (0, x :: _) => x
        case (p, _ :: rs) => at(p - 1, rs)
        case _ => unreachable!()
    }

    ///
    /// Helper function for `permutations`.
    ///
    def removeIndex(i: Int32, xs: List[a]): List[a] = match (i, xs) {
        case (_, Nil) => xs
        case (0, _ :: rs) => rs
        case (p, x :: rs) => x :: removeIndex(p - 1, rs)
    }

    ///
    /// Returns all subsequences of `xs` in lexicographical order by element indices in `xs`.
    ///
    /// That is, `xs` is the first subsequence and `Nil` is the last subsequence.
    ///
    @Time(length(xs) * length(xs)) @Space(length(xs) * length(xs))
    pub def subsequences(xs: List[a]): List[List[a]] = match xs {
        case Nil => Nil :: Nil
        case x :: rs =>
            let r = subsequences(rs);
                applyHelper(x, r) ::: r
    }

    ///
    /// Helper function for `permutations` and `subsequences`.
    /// Returns `xs` with `a` added to the beginning of each element in `xs`.
    ///
    def applyHelper(a: a, xs: List[List[a]]): List[List[a]] = match xs {
        case Nil => Nil
        case x :: rs => (a :: x) :: applyHelper(a, rs)
    }

    ///
    /// Returns `xs` with `a` inserted between every two adjacent elements.
    ///
    @Time(length(xs)) @Space(length(xs))
    pub def intersperse(a: a, xs: List[a]): List[a] = match xs {
        case x1 :: x2 :: rs => x1 :: a :: intersperse(a, x2 :: rs)
        case _ => xs
    }

    ///
    /// Returns the concatenation of the elements in `ys` with the elements of `xs` inserted between every two adjacent elements.
    ///
    /// That is, returns `y1 :: x1 ... xn :: y2 :: ... yn-1 :: x1 :: ... :: xn :: yn :: Nil`.
    ///
    @Time(length(ys) * length(xs)) @Space(length(ys) * length(xs))
    pub def intercalate(xs: List[a], ys: List[List[a]]): List[a] = match ys {
        case Nil => Nil
        case y :: Nil => y
        case y1 :: y2 :: rs => y1 ::: xs ::: intercalate(xs, y2 :: rs)
    }

    ///
    /// Returns the transpose of `xs`.
    ///
    /// Returns `xs` if the dimensions of the elements of `xs` are mismatched.
    ///
    @Time(length(xs) * length(xs)) @Space(length(xs) * length(xs))
    pub def transpose(xs: List[List[a]]): List[List[a]] = match xs {
        case Nil => Nil
        case x :: _ =>
            let len = length(x);
                if (not uniformHelper(xs, len) or len == 0) xs else transposeHelper(xs, len)
    }

    ///
    /// Helper function for `transpose`.
    ///
    def uniformHelper(xs: List[List[a]], l: Int32): Bool = match xs {
        case Nil => true
        case x :: rs => if (length(x) == l) uniformHelper(rs, l) else false
    }

    ///
    /// Helper function for `transpose`.
    ///
    def transposeHelper(xs: List[List[a]], l: Int32): List[List[a]] = match xs {
        case Nil => repeat(Nil, l)
        case x :: rs => applyListHelper(x, transposeHelper(rs, l))
    }

    ///
    /// Helper function for `transpose`.
    ///
    def applyListHelper(xs: List[a], ys: List[List[a]]): List[List[a]] = match (xs, ys) {
        case (Nil, Nil) => Nil
        case (x :: rs, y :: qs) => (x :: y) :: applyListHelper(rs, qs)
        case _ => unreachable!()
    }






    ///
    /// Returns `true` if and only if `xs` is a prefix of `ys`.
    ///
    @Time(length(xs)) @Space(1)
    pub def isPrefixOf(xs: List[a], ys: List[a]): Bool with Eq[a] = match (xs, ys) {
        case (Nil, _) => true
        case (_, Nil) => false
        case (x :: qs, y :: rs) => if (x == y) isPrefixOf(qs, rs) else false
    }

    ///
    /// Returns `true` if and only if `xs` is an infix of `ys`.
    ///
    @Time(length(xs)) @Space(1)
    pub def isInfixOf(xs: List[a], ys: List[a]): Bool with Eq[a] = match (xs, ys) {
        case (Nil, _) => true
        case (_, Nil) => false
        case (_, _ :: rs) => if (isPrefixOf(xs, ys)) true else isInfixOf(xs, rs)
    }

    ///
    /// Returns `true` if and only if `xs` is a suffix of `ys`.
    ///
    @Time(length(xs)) @Space(Int32.max(length(xs), length(ys)))
    pub def isSuffixOf(xs: List[a], ys: List[a]): Bool with Eq[a] = isPrefixOf(reverse(xs), reverse(ys))





    ///
    /// Returns the result of applying `combine` to all the elements in `xs`, using `empty` as the initial value.
    ///
    pub def fold(xs: List[a]): a with Monoid[a] = Foldable.fold(xs)

    ///
    /// Applies `f` to a start value `s` and all elements in `xs` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b & e, s: b, xs: List[a]): b & e = match xs {
        case Nil => s
        case x :: rs => foldLeft(f, f(s, x), rs)
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `xs` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b & e, s: b, xs: List[a]): b & e = foldRightHelper(f, s, xs, s1 -> s1 as & e)

    ///
    /// Helper function for `foldRight`.
    ///
    def foldRightHelper(f: (a, b) -> b & e, s: b, xs: List[a], k: b -> b & e): b & e = match xs {
        case Nil => k(s)
        case x :: rs => foldRightHelper(f, s, rs, s1 -> k(f(x, s1)))
    }

    ///
    /// Applies `f` to all elements in `xs` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(x1, x2), x3)..., xn))`
    ///
    /// Returns `None` if `xs` is empty.
    ///
    pub def reduceLeft(f: (a, a) -> a & e, xs: List[a]): Option[a] & e = match xs {
        case Nil => None
        case x :: rs => Some(foldLeft(f, x, rs))
    }

    ///
    /// Applies `f` to all elements in `xs` going from right to left until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(x1, ...f(xn-2, f(xn-1, xn))...))`
    ///
    /// Returns `None` if `xs` is empty.
    ///
    pub def reduceRight(f: (a, a) -> a & e, xs: List[a]): Option[a] & e = match xs {
        case Nil => None
        case x :: rs => match reduceRight(f, rs) {
            case None => Some(x)
            case Some(v) => Some(f(x, v))
        }
    }






    ///
    /// Returns the number of elements in `xs` that satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(xs)) @Space(space(f) * length(xs))
    pub def count(f: a -> Bool, xs: List[a]): Int32 = match xs {
        case Nil => 0
        case x :: rs =>
            let r = count(f, rs);
                if (f(x)) r+1 else r
    }

    ///
    /// Returns the concatenation of the elements in `xs`.
    ///
    @Time(length(xs)) @Space(length(xs))
    pub def flatten(xs: List[List[a]]): List[a] = match xs {
        case Nil => Nil
        case x :: rs => x ::: flatten(rs)
    }

    ///
    /// Returns `true` if and only if at least one element in `xs` satisfies the predicate `f`.
    ///
    /// Returns `false` if `xs` is empty.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(xs)) @Space(space(f))
    pub def exists(f: a -> Bool, xs: List[a]): Bool = match xs {
        case Nil => false
        case x :: rs => if (f(x)) true else exists(f, rs)
    }

    ///
    /// Returns `true` if and only if all elements in `xs` satisfy the predicate `f`.
    ///
    /// Returns `true` if `xs` is empty.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(xs)) @Space(space(f))
    pub def forall(f: a -> Bool, xs: List[a]): Bool = match xs {
        case Nil => true
        case x :: rs => if (not f(x)) false else forall(f, rs)
    }






    ///
    /// Returns a list of every element in `xs` that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(xs)) @Space(space(f) * length(xs))
    pub def filter(f: a -> Bool, xs: List[a]): List[a] = match xs {
        case Nil => Nil
        case x :: rs =>
            let r = filter(f, rs);
                if (f(x)) x :: r else r
    }

    ///
    /// Returns the sublist of `xs` without the last element.
    /// Returns `None` if the list `xs` is `Nil`.
    ///
    @Time(length(xs)) @Space(length(xs))
    pub def init(xs: List[a]): Option[List[a]] = initHelper(xs, Nil)

    ///
    /// Helper function for `init`.
    ///
    def initHelper(xs: List[a], acc: List[a]): Option[List[a]] = match xs {
        case Nil => None
        case _ :: Nil => Some(reverse(acc))
        case x :: rs => initHelper(rs, x :: acc)
    }

    ///
    /// Returns the sublist of `xs` from index `b` (inclusive) to index `e` (exclusive).
    ///
    /// That is, an element at index `i` in `xs` is part of the returned sublist if and only if `i >= b` and `i < e`.
    /// Note: Indices that are out of bounds in `xs` are not considered (i.e. slice(b, e, xs) = slice(max(0,b), min(length(xs),e), xs)).
    ///
    @Time(e - b) @Space(e - b)
    pub def slice(b: Int32, e: Int32, xs: List[a]): List[a] = sliceHelper(b, e, xs, 0)

    ///
    /// Helper function for `slice`.
    ///
    def sliceHelper(b: Int32, e: Int32, xs: List[a], i: Int32): List[a] = match xs {
        case Nil => Nil
        case x :: rs =>
            let r = sliceHelper(b, e, rs, i+1);
                if (i >= b and i < e) x :: r else r
    }

    ///
    /// Returns a pair of lists `(ys, zs)`.
    ///
    /// `ys` contains all elements of `xs` that satisfy the predicate `f`.
    /// `zs` contains all elements of `xs` that do not satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(xs)) @Space(space(f) * length(xs))
    pub def partition(f: a -> Bool, xs: List[a]): (List[a], List[a]) = match xs {
        case Nil => (Nil, Nil)
        case x :: rs =>
            let (r1, r2) = partition(f, rs);
                if (f(x)) (x :: r1, r2) else (r1, x :: r2)
    }

    ///
    /// Returns a pair of lists `(ys, zs)`.
    ///
    /// `ys` is the longest prefix of `xs` that satisfies the predicate `f`.
    /// `zs` is the remainder of `xs`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(xs)) @Space(space(f) * length(xs))
    pub def span(f: a -> Bool, xs: List[a]): (List[a], List[a]) = match xs {
        case Nil => (Nil, Nil)
        case x :: rs =>
            if (f(x))
                let (r1, r2) = span(f, rs);
                (x :: r1, r2)
            else
                (Nil, xs)
    }

    ///
    /// Returns `xs` without the first `n` elements.
    ///
    /// Returns `Nil` if `n > length(xs)`.
    /// Returns `xs` if `n < 0`.
    ///
    @Time(n) @Space(length(xs) - n)
    pub def drop(n: Int32, xs: List[a]): List[a] = if (n < 0) xs else match (n, xs) {
        case (_, Nil) => Nil
        case (0, _) => xs
        case (i, _ :: rs) => drop(i - 1, rs)
    }

    ///
    /// Returns `xs` without the longest prefix that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(xs)) @Space(space(f))
    pub def dropWhile(f: a -> Bool, xs: List[a]): List[a] = match xs {
        case Nil => Nil
        case x :: rs => if (f(x)) dropWhile(f, rs) else xs
    }

    ///
    /// Returns the first `n` elements of `xs`.
    ///
    /// Returns `xs` if `n > length(xs)`.
    /// Returns `Nil` if `n < 0`.
    ///
    @Time(n) @Space(n)
    pub def take(n: Int32, xs: List[a]): List[a] = if (n < 0) Nil else match (n, xs) {
        case (_, Nil) => Nil
        case (0, _) => Nil
        case (i, x :: rs) => x :: take(i-1, rs)
    }

    ///
    /// Returns the longest prefix of `xs` that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(xs)) @Space(space(f) * length(xs))
    pub def takeWhile(f: a -> Bool, xs: List[a]): List[a] = match xs {
        case Nil => Nil
        case x :: rs => if (f(x)) x :: takeWhile(f, rs) else Nil
    }






    ///
    /// Partitions `xs` into sublists such that for any two elements `x` and `y` in a sublist, `f(x, y)` is true.
    ///
    /// A sublist is created by iterating through the remaining elements of `xs` from left to right and adding an
    /// element to the sublist if and only if doing so creates no conflicts with the elements already in the sublist.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(xs)) @Space(space(f) * length(xs))
    pub def groupBy(f: (a, a) -> Bool, xs: List[a]): List[List[a]] = match xs {
        case Nil => Nil
        case x :: rs =>
            let (r1, r2) = extractHelper(f, rs, x :: Nil, Nil);
                r1 :: groupBy(f, r2)
    }

    ///
    /// Helper function for `groupBy`.
    ///
    def extractHelper(f: (a, a) -> Bool, xs: List[a], ps: List[a], ns: List[a]): (List[a], List[a]) = match xs {
        case Nil => (reverse(ps), reverse(ns))
        case x :: rs => if (agreeHelper(f, x, ps)) extractHelper(f, rs, x :: ps, ns) else extractHelper(f, rs, ps, x :: ns)
    }

    ///
    /// Helper function for `groupBy`.
    ///
    def agreeHelper(f: (a, a) -> Bool, a: a, xs: List[a]): Bool = match xs {
        case Nil => true
        case x :: rs => if (f(x, a) and f(a, x)) agreeHelper(f, a, rs) else false
    }






    ///
    /// Returns a list where the element at index `i` is `(a, b)` where
    /// `a` is the element at index `i` in `xs` and `b` is the element at index `i` in `ys`.
    ///
    /// If either `xs` or `ys` becomes depleted, then no further elements are added to the resulting list.
    ///
    @Time(Int32.min(length(xs), length(ys))) @Space(Int32.min(length(xs), length(ys)))
    pub def zip(xs: List[a], ys: List[b]): List[(a,b)] = zipHelper(xs, ys, ks -> ks)

    ///
    /// Helper function for `zip`.
    ///
    def zipHelper(xs: List[a], ys: List[b], k: List[(a,b)] -> List[(a,b)]): List[(a,b)] = match (xs, ys) {
            case (x :: rs, y :: qs) => zipHelper(rs, qs, ks -> k((x, y) :: ks))
            case _ => k(Nil)
        }

    ///
    /// Returns a list where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `xs` and `b` is the element at index `i` in `ys`.
    ///
    /// If either `xs` or `ys` becomes depleted, then no further elements are added to the resulting list.
    ///
    @Time(time(f) * Int32.min(length(xs), length(ys))) @Space(space(f) * Int32.min(length(xs), length(ys)))
    pub def zipWith(f: (a, b) -> c & e, xs: List[a], ys: List[b]): List[c] & e = zipWithHelper(f, xs, ys, ks -> ks)

    ///
    /// Helper function for `zipWith`.
    ///
    def zipWithHelper(f: (a, b) -> c & e, xs: List[a], ys: List[b], k: List[c] -> List[c]): List[c] & e = match (xs, ys) {
        case (x :: rs, y :: qs) => {
            let a = f(x, y);
            zipWithHelper(f, rs, qs, ks -> k(a :: ks))
        }
        case _ => k(Nil)
    }

    ///
    /// Returns a pair of lists, the first containing all first components in `xs`
    /// and the second containing all second components in `xs`.
    ///
    @Time(length(xs)) @Space(length(xs))
    pub def unzip(xs: List[(a, b)]): (List[a], List[b]) = unzipHelper(xs, (ks1, ks2) -> (ks1, ks2))

    ///
    /// Helper function for `zip`.
    ///
    def unzipHelper(xs: List[(a, b)], k: (List[a], List[b]) -> (List[a], List[b])): (List[a], List[b]) = match xs {
        case Nil => k(Nil, Nil)
        case (x1, x2) :: rs => unzipHelper(rs, (ks1, ks2) -> k(x1 :: ks1, x2 :: ks2))
    }

    ///
    /// Returns a list where the element at index `i` is `(a, b, c)` where
    /// `a` is the element at index `i` in `xs`, `b` is the element at index `i` in `ys`
    /// and `c` is the element at index `i` in `zs`.
    ///
    /// If any one of `xs`, `ys` or `zs` become depleted, then no further elements are added to the resulting list.
    ///
    pub def zip3(xs: List[a], ys: List[b], zs: List[c]): List[(a, b, c)] =
        zipWith3((x, y, z) -> (x, y, z), xs, ys, zs)

    ///
    /// Returns a list where the element at index `i` is `f(a, b, c)` where
    /// `a` is the element at index `i` in `xs`, `b` is the element at index `i` in `ys`
    /// and `c` is the element at index `i` in `zs`.
    ///
    /// If any one of `xs`, `ys` or `zs` become depleted, then no further elements are added to the resulting list.
    ///
    pub def zipWith3(f: (a, b, c) -> d & ef, xs: List[a], ys: List[b], zs: List[c]): List[d] & ef =
        zipWith3Helper(f, xs, ys, zs, ks -> ks)

    ///
    /// Helper function for `zipWith3`.
    ///
    def zipWith3Helper(f: (a, b, c) -> d & ef, xs: List[a], ys: List[b], zs: List[c], k: List[d] -> List[d]): List[d] & ef =
        match (xs, ys, zs) {
            case (x :: rs, y :: qs, z :: ss) => {
                let a = f(x, y, z);
                zipWith3Helper(f, rs, qs, ss, ks -> k(a :: ks))
            }
            case _ => k(Nil)
        }

    ///
    /// Returns a triple of lists, the first containing all first components in `xs`
    /// the second containing all second components in `xs` and the third containing all
    /// third components in `xs`.
    ///
    pub def unzip3(xs: List[(a, b, c)]): (List[a], List[b], List[c]) = unzip3Helper(xs, (ks1, ks2, ks3) -> (ks1, ks2, ks3))

    ///
    /// Helper function for `unzip3`.
    ///
    def unzip3Helper(xs: List[(a, b, c)], k: (List[a], List[b], List[c]) -> (List[a], List[b], List[c])): (List[a], List[b], List[c]) = match xs {
        case Nil => k(Nil, Nil, Nil)
        case (x1, x2, x3) :: rs => unzip3Helper(rs, (ks1, ks2, ks3) -> k(x1 :: ks1, x2 :: ks2, x3 :: ks3))
    }

    ///
    /// Returns a list where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `xs` and `b` is the element at index `i` in `ys`.
    ///
    /// If either `xs` or `ys` becomes depleted, then no further elements are added to the resulting list.
    /// Functionally equivalent to zipWith.
    ///
    @Time(time(f) * Int32.min(length(xs), length(ys))) @Space(space(f) * Int32.min(length(xs), length(ys)))
    pub def map2(f: (a, b) -> c & e, xs: List[a], ys: List[b]): List[c] & e = zipWith(f, xs, ys)

    ///
    /// Concatenates the results of applying `f` pairwise to the elements of `xs` and `ys`.
    ///
    @Time(time(f) * Int32.min(length(xs), length(ys))) @Space(space(f) * Int32.min(length(xs), length(ys)))
    pub def flatMap2(f: (a, b) -> List[c] & e, xs: List[a], ys: List[b]): List[c] & e = match (xs, ys) {
        case (x :: rs, y :: qs) => f(x, y) ::: flatMap2(f, rs, qs)
        case _ => Nil
    }

    ///
    /// Alias for `foldLeft2`.
    ///
    @Time(time(f) * Int32.min(length(xs), length(ys))) @Space(space(f) * Int32.min(length(xs), length(ys)))
    pub def fold2(f: (c, a, b) -> c & e, c: c, xs: List[a], ys: List[b]): c & e = foldLeft2(f, c, xs, ys)

    ///
    /// Accumulates the result of applying `f` pairwise to the elements of `xs` and `ys`
    /// starting with the initial value `c` and going from left to right.
    ///
    @Time(time(f) * Int32.min(length(xs), length(ys))) @Space(space(f) * Int32.min(length(xs), length(ys)))
    pub def foldLeft2(f: (c, a, b) -> c & e, c: c, xs: List[a], ys: List[b]): c & e = match (xs, ys) {
        case (x :: rs, y :: qs) => foldLeft2(f, f(c, x, y), rs, qs)
        case _ => c
    }

    ///
    /// Accumulates the result of applying `f` pairwise to the elements of `xs` and `ys`
    /// starting with the initial value `c` and going from right to left.
    ///
    @Time(time(f) * Int32.min(length(xs), length(ys))) @Space(space(f) * Int32.min(length(xs), length(ys)))
    pub def foldRight2(f: (a, b, c) -> c & e, c: c, xs: List[a], ys: List[b]): c & e =
        let len1 = length(xs);
        let len2 = length(ys);
            foldRight2Helper(f, c, drop(len1-len2, xs), drop(len2-len1, ys))

    ///
    /// Helper function for `foldRight2`.
    ///
    def foldRight2Helper(f: (a, b, c) -> c & e, c: c, xs: List[a], ys: List[b]): c & e = match (xs, ys) {
        case (x :: rs, y :: qs) => f(x, y, foldRight2(f, c, rs, qs))
        case _ => c
    }






    ///
    /// Collects the results of applying the partial function `f` to every element in `xs`.
    ///
    @Time(time(f) * length(xs)) @Space(space(f) * length(xs))
    pub def filterMap(f: a -> Option[b] & e, xs: List[a]): List[b] & e = match xs {
        case Nil => Nil
        case x :: rs => match f(x) {
                            case None => filterMap(f, rs)
                            case Some(v) => v :: filterMap(f, rs)
        }
    }

    ///
    /// Returns the first non-None result of applying the partial function `f` to each element of `xs`.
    ///
    /// Returns `None` if every element of `xs` is `None`.
    ///
    @Time(time(f) * length(xs)) @Space(space(f))
    pub def findMap(f: a -> Option[b] & e, xs: List[a]): Option[b] & e = match xs {
        case Nil => None
        case x :: rs => match f(x) {
                            case None => findMap(f, rs)
                            case Some(v) => Some(v)
        }
    }

    ///
    /// Render the list `xs` as a String. Elements are rendered with the
    /// function `f` and separated with the string `sep`.
    ///
    @Time(time(f) * length(xs)) @Space(space(f) * length(xs))
    pub def formatWith(f: a -> String, sep: String, xs: List[a]): String =
        let s = foldRight((x, acc) -> "${f(x)}${sep}${acc}", "", xs) as & Pure;
        String.dropRight(String.length(sep), s)

    ///
    /// Returns `xs` as a mutable list.
    ///
    pub def toMut(xs: List[a]): MutList[a] & Impure =
        MutList(ref toArray(xs), ref length(xs))

    ///
    /// Returns the list `xs` as a set.
    ///
    @Time(length(xs)) @Space(length(xs))
    pub def toSet(xs: List[a]): Set[a] with Order[a] = foldRight(Set.insert, Set.empty(), xs)

    ///
    /// Returns the association list `xs` as a map.
    ///
    /// If `xs` contains multiple mappings with the same key, `toMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    @Time(length(xs)) @Space(length(xs))
    pub def toMap(xs: List[(a, b)]): Map[a, b] with Order[a] = foldRight((x, m) -> Map.insert(fst(x), snd(x), m), Map.empty(), xs)

    ///
    /// Applies `f` to every element of `xs`.
    ///
    /// The function `f` must be impure.
    ///
    @Time(time(f)) @Space(space(f))
    pub def foreach(f: a ~> Unit, xs: List[a]): Unit & Impure = match xs {
        case Nil => ()
        case x :: rs => f(x); foreach(f, rs)
    }

    ///
    /// Returns the list `xs` as an array.
    ///
    @Time(length(xs)) @Space(length(xs))
    pub def toArray(xs: List[a]) : Array[a] & Impure = match head(xs) {
        case None => []
        case Some(x) =>
            let a = Array.new(x, length(xs));
            let f = (i,b) -> { a[i] = b; i + 1 };
            foldLeft(f, 0, xs);
            a
        }

    ///
    /// Sort list `xs` with the comparing function `cmp`.
    ///
    /// The sort implementation is a mergesort.
    /// Acknowledgement: derived from Thomas Nordin's `sortBy` in the Haskell base libraries.
    ///
    @Time(length(xs) * Int32.log2(length(xs))) @Space(length(xs))
    pub def sortWith(cmp: (a,a) -> Int32, xs: List[a]): List[a] =
        sortGenerateSequences(cmp, xs, zss -> zss) |> sortMergeAll(cmp)

    ///
    /// Helper function for `sortWith`.
    ///
    def sortGenerateSequences(cmp: (a,a) -> Int32, xs: List[a], k: List[List[a]] -> List[List[a]]): List[List[a]] = match xs {
        case a :: b :: rs =>
            if (cmp(a, b) > 0)
                sortDescendingStep(cmp, b, a :: Nil, rs, k)
            else
                sortAscendingStep(cmp, b, vs -> a :: vs, rs, k)
        case rs => k(rs :: Nil)
    }

    ///
    /// Helper function for `sortWith`.
    ///
    def sortDescendingStep(cmp: (a,a) -> Int32, x: a, xs: List[a], ys: List[a], k: List[List[a]] -> List[List[a]]): List[List[a]] = match ys {
        case r1 :: rs if (cmp(x, r1) > 0) =>
            sortDescendingStep(cmp, r1, x :: xs, rs, k)
        case rs =>
            sortGenerateSequences(cmp, rs, zss -> k((x :: xs) :: zss))
    }

    ///
    /// Helper function for `sortWith`.
    ///
    def sortAscendingStep(cmp: (a,a) -> Int32, x: a, xsf: List[a] -> List[a], ys: List[a], k: List[List[a]] -> List[List[a]]): List[List[a]] = match ys {
        case r1 :: rs if (cmp(x, r1) <= 0)  =>
            sortAscendingStep(cmp, r1, vs -> xsf(x :: vs), rs, k)
        case rs =>
            let x1 = xsf(x :: Nil);
            sortGenerateSequences(cmp, rs, zss -> k(x1 :: zss))
        }

    ///
    /// Helper function for `sortWith`.
    ///
    def sortMergeAll(cmp: (a,a) -> Int32, xss: List[List[a]]) : List[a] = match xss {
        case x :: Nil => x
        case rs => {
            let xss = sortMergePairs(cmp, rs, zs -> zs);
            sortMergeAll(cmp, xss)
        }
    }

    ///
    /// Helper function for `sortWith`.
    ///
    def sortMergePairs(cmp: (a,a) -> Int32, xss: List[List[a]], k: List[List[a]] -> List[List[a]]) : List[List[a]] = match xss {
        case a :: b :: rs => {
            let x = sortMerge(cmp, a, b, zs -> zs);
            sortMergePairs(cmp, rs, zss -> k(x :: zss))
        }
        case rs => k(rs)
    }

    ///
    /// Helper function for `sortWith`.
    ///
    def sortMerge(cmp: (a,a) -> Int32, xs: List[a], ys: List[a], k: List[a] -> List[a]) : List[a] = match (xs, ys) {
        case (r :: rs, s :: ss) =>
            if (cmp(r,s) > 0)
                sortMerge(cmp, xs, ss, zs -> k(s :: zs))
            else
                sortMerge(cmp, rs, ys, zs -> k(r :: zs))
        case (Nil, ss) => k(ss)
        case (rs, Nil) => k(rs)
    }

    ///
    /// Build a list by applying `f` to the seed value `st`.
    ///
    /// `f` should return `Some(a,st1)` to signal a new list element `a` and a new seed value `st1`.
    ///
    /// `f` should return `None` to signal the end of building the list.
    ///
    pub def unfold(f: s -> Option[(a, s)] & e, st: s): List[a] & e = unfoldHelper(f, st, xs -> xs)

    ///
    /// Helper function for `unfold`.
    ///
    def unfoldHelper(f: s -> Option[(a, s)] & e, st: s, k: List[a] -> List[a]): List[a] & e = match f(st) {
        case None => k(Nil)
        case Some(a, st1) => unfoldHelper(f, st1, xs -> k(a :: xs))
    }

    ///
    /// Build a list by applying the function `next` to `()`. `next` is expected to encapsulate
    /// a stateful resource such as a file handle that can be iterated.
    ///
    /// `next` should return `Some(a)` to signal a new list element `a`.
    ///
    /// `next` should return `None` to signal the end of building the list.
    ///
    pub def unfoldWithIter(next: () ~> Option[a]): List[a] & Impure = unfoldWithIterHelper(next, xs -> xs)

    ///
    /// Helper function for `unfoldWithIter`.
    ///
    def unfoldWithIterHelper(next: () ~> Option[a], k: List[a] -> List[a]): List[a] & Impure = match next() {
        case None => k(Nil)
        case Some(a) => unfoldWithIterHelper(next, xs -> k(a :: xs))
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
    pub def unfoldWithOkIter(next: Unit ~> Result[Option[a], e]): Result[List[a], e] & Impure = unfoldWithOkIterHelper(next, xs -> xs)

    ///
    /// Helper function for `unfoldWithOkIter`.
    ///
    def unfoldWithOkIterHelper(next: Unit ~> Result[Option[a], e], k: Result[List[a], e] -> Result[List[a], e]): Result[List[a], e] & Impure = match next() {
        case Ok(None) => k(Ok(Nil))
        case Err(e) => k(Err(e))
        case Ok(Some(a)) => unfoldWithOkIterHelper(next, Result.flatMap(xs -> k(Ok(a :: xs))))
    }

    ///
    /// Returns the list `xs` with duplicates removed. The first occurence of
    /// an element is kept and except for the removal of subsequent duplicates
    /// the order of `xs` is preserved.
    ///
    /// `distinct` uses the Flix's builtin equality test. Use `distinctWith` if you
    /// need a custom equality test.
    ///
    pub def distinct(xs: List[a]): List[a] with Eq[a] =
        distinctHelper(xs, Nil, ks -> ks)

    ///
    /// Helper function for `distinct`.
    ///
    def distinctHelper(xs: List[a], ys: List[a], k: List[a] -> List[a]): List[a] with Eq[a] = match xs {
        case Nil => k(Nil)
        case x :: rs =>
            if (memberOf(x, ys))
                distinctHelper(rs, ys, k)
            else
                distinctHelper(rs, (x :: ys), ks -> k(x :: ks))
    }

    ///
    /// Returns the list `xs` with duplicates removed using the supplied function
    /// `f` for comparison. The first occurence of an element is kept and except
    /// for the removal of subsequent duplicates the order of `xs` is preserved.
    ///
    pub def distinctWith(f: (a, a) -> Bool, xs: List[a]): List[a] =
        distinctWithHelper(f, xs, Nil, ks -> ks)

    ///
    /// Helper function for `distinctWith`.
    ///
    def distinctWithHelper(f: (a, a) -> Bool, xs: List[a], ys: List[a], k: List[a] -> List[a]): List[a] = match xs {
        case Nil => k(Nil)
        case x :: rs =>
            if (memberOfWith(f, x, ys))
                distinctWithHelper(f, rs, ys, k)
            else
                distinctWithHelper(f, rs, (x :: ys), ks -> k(x :: ks))
    }

    ///
    /// Helper function for `distinctWith`.
    ///
    /// This is a generalization of `memberOf` that uses the function `f` for comparison.
    ///
    def memberOfWith(f: (a, a) -> Bool, x: a, xs: List[a]): Bool = match xs {
        case Nil => false
        case y :: rs =>
            if (f(x, y))
                true
            else
                memberOfWith(f, x, rs)
    }

    /////////////////////////////////////////////////////////////////////////////
    // Lints: Simplifications                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @lint
    def filterFilter(): Bool =
        forall [a] (f: a -> Bool, g: a -> Bool, xs: List[a]).
            filter(g, filter(f, xs)) === filter(x -> f(x) and g(x), xs)

    @lint
    def findFind(): Bool =
        forall [a] (f: a -> Bool, g: a -> Bool, xs: List[a]).
            Option.find(g, List.find(f, xs)) === find(x -> f(x) and g(x), xs)

    @lint
    def mapIdentity(): Bool =
        forall [a] (xs: List[a]).
            map(x -> x, xs) === xs

    @lint
    def mapMap1(): Bool =
        forall [a, b, c] (f: a -> b, g: b -> c, xs: List[a]).
            map(g, map(f, xs)) === map(f >> g, xs)

    @lint
    def mapMap2(): Bool =
        forall [a, b, c] (f: a ~> b, g: b -> c, xs: List[a]).
            map(g, map(f, xs)) === map(f >> g, xs)

    @lint
    def mapMap3(): Bool =
        forall [a, b, c] (f: a -> b, g: b ~> c, xs: List[a]).
            map(g, map(f, xs)) === map(f >> g, xs)

    @lint
    def mapZip(): Bool =
      forall [a, b, c] (f: (a, b) -> c, xs: List[a], ys: List[b]).
        map(p -> f(fst(p), snd(p)), List.zip(xs, ys)) === map2(f, xs, ys)

    /////////////////////////////////////////////////////////////////////////////
    // Lints: Use Cases                                                        //
    /////////////////////////////////////////////////////////////////////////////


}
