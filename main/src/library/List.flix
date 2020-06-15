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
    pub def length(xs: List[a]): Int32 = match xs {
        case Nil => 0
        case _ :: rs => 1 + length(rs)
    }

    ///
    /// Returns `ys` appended to `xs`.
    ///
    /// The infix operator `:::` is an alias for `append` (`xs ::: ys = append(xs, ys)`).
    ///
    pub def append(xs: List[a], ys: List[a]): List[a] = match xs {
        case Nil => ys
        case x :: rs => x :: append(rs, ys)
    }

    ///
    /// Returns `true` if and only if `xs` contains the element `a`.
    ///
    pub def memberOf(a: a, xs: List[a]): Bool = match xs {
        case Nil => false
        case x :: rs => if (x == a) true else memberOf(a, rs)
    }

    ///
    /// Returns the position of `a` in `xs`.
    ///
    /// Returns `-1` if `a` does not exist in `xs`.
    ///
    pub def indexOf(a: a, xs: List[a]): Int32 = indexOfHelper(a, xs, 0)

    ///
    /// Helper function for `indexOf`.
    ///
    def indexOfHelper(a: a, xs: List[a], acc: Int32): Int32 = match xs {
        case Nil => -1
        case x :: rs => if (x == a) acc else indexOfHelper(a, rs, acc+1)
    }

    ///
    /// Alias for `findLeft`.
    ///
    /// The function `f` must be pure.
    ///
    pub def find(f: a -> Bool, xs: List[a]): Option[a] = findLeft(f, xs)

    ///
    /// Optionally returns the first element of `xs` that satisfies the predicate `f` when searching from left to right.
    ///
    /// The function `f` must be pure.
    ///
    pub def findLeft(f: a -> Bool, xs: List[a]): Option[a] = match xs {
        case Nil => None
        case x :: rs => if (f(x)) Some(x) else findLeft(f, rs)
    }

    ///
    /// Optionally returns the first element of `xs` that satisfies the predicate `f` when searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    pub def findRight(f: a -> Bool, xs: List[a]): Option[a] = match xs {
        case Nil => None
        case x :: rs => Option.withDefault(findRight(f, rs), if (f(x)) Some(x) else None)
    }






    ///
    /// Returns a list of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns `Nil` if `b >= e`.
    ///
    pub def range(b: Int32, e: Int32): List[Int32] = if (b >= e) Nil else b :: range(b + 1, e)

    ///
    /// Returns a list with the element `a` repeated `n` times.
    ///
    /// Returns `Nil` if `n < 0`.
    ///
    pub def repeat(a: a, n: Int32): List[a] = if (n <= 0) Nil else a :: repeat(a, n - 1)

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
            case _ => panic("unreachable")
        }
    }






    ///
    /// Returns the result of applying `f` to every element in `xs`.
    ///
    /// That is, the result is of the form: `f(x1) :: f(x2) :: ...`.
    ///
    @Time(time(f) * length(xs)) @Space(space(f) * length(xs))
    pub def map(f: a -> b & e, xs: List[a]): List[b] & e = match xs {
        case Nil => Nil
        case x :: rs => f(x) :: map(f, rs)
    }

    ///
    /// Returns the result of applying `f` to every element in `xs` along with that element's index.
    ///
    /// That is, the result is of the form: `f(x1, 0) :: f(x2, 1) :: ...`.
    ///
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
    pub def flatMap(f: a -> List[b] & e, xs: List[a]): List[b] & e = match xs {
        case Nil => Nil
        case x :: rs => f(x) ::: flatMap(f, rs)
    }

    ///
    /// Returns the reverse of `xs`.
    ///
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
    pub def rotateRight(n: Int32, xs: List[a]): List[a] = rotateLeft(-n, xs)

    ///
    /// Returns `xs` with the element at index `i` replaced by `a`.
    ///
    /// Returns `xs` if `i < 0` or `i > length(xs)-1`.
    ///
    pub def update(i: Int32, a: a, xs: List[a]): List[a] = match (i, xs) {
        case (_, Nil) => xs
        case (0, _ :: rs) => a :: rs
        case (p, x :: rs) => x :: update(p-1, a, rs)
    }

    ///
    /// Returns `xs` with every occurrence of `x` replaced by `y`.
    ///
    pub def replace(x: a, y: a, xs: List[a]): List[a] = map(e -> if (e == x) y else e, xs)

    ///
    /// Returns `ys` with the `n` elements starting at index `i` replaced with the elements of `xs`.
    ///
    /// If any of the indices `i, i+1, i+2, ... , i+n-1` are out of range in `ys` then no patching is done at these indices.
    /// If `xs` becomes depleted then no further patching is done.
    /// If patching occurs at index `i+j` in `ys`, then the element at index `j` in `xs` is used.
    ///
    pub def patch(i: Int32, n: Int32, xs: List[a], ys: List[a]): List[a] = patchHelper(i, n, drop(-i, xs), ys, 0)

    ///
    /// Helper function for `patch`.
    ///
    def patchHelper(i: Int32, n: Int32, xs: List[a], ys: List[a], c: Int32): List[a] = match (xs, ys) {
        case (x :: qs, y :: rs) => if (c >= i && c < i+n) x :: patchHelper(i, n, qs, rs, c+1) else y :: patchHelper(i, n, xs, rs, c+1)
        case _ => ys
    }

    ///
    /// Returns all permutations of `xs` in lexicographical order by element indices in `xs`.
    ///
    /// That is, `xs` is the first permutation and `reverse(xs)` is the last permutation.
    ///
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
        case _ => panic("unreachable")
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
    pub def intersperse(a: a, xs: List[a]): List[a] = match xs {
        case x1 :: x2 :: rs => x1 :: a :: intersperse(a, x2 :: rs)
        case _ => xs
    }

    ///
    /// Returns the concatenation of the elements in `ys` with the elements of `xs` inserted between every two adjacent elements.
    ///
    /// That is, returns `y1 :: x1 ... xn :: y2 :: ... yn-1 :: x1 :: ... :: xn :: yn :: Nil`.
    ///
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
    pub def transpose(xs: List[List[a]]): List[List[a]] = match xs {
        case Nil => Nil
        case x :: _ =>
            let len = length(x);
                if (!uniformHelper(xs, len) || len == 0) xs else transposeHelper(xs, len)
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
        case _ => panic("unreachable")
    }






    ///
    /// Returns `true` if and only if `xs` is a prefix of `ys`.
    ///
    pub def isPrefixOf(xs: List[a], ys: List[a]): Bool = match (xs, ys) {
        case (Nil, _) => true
        case (_, Nil) => false
        case (x :: qs, y :: rs) => if (x == y) isPrefixOf(qs, rs) else false
    }

    ///
    /// Returns `true` if and only if `xs` is an infix of `ys`.
    ///
    pub def isInfixOf(xs: List[a], ys: List[a]): Bool = match (xs, ys) {
        case (Nil, _) => true
        case (_, Nil) => false
        case (_, _ :: rs) => if (isPrefixOf(xs, ys)) true else isInfixOf(xs, rs)
    }

    ///
    /// Returns `true` if and only if `xs` is a suffix of `ys`.
    ///
    pub def isSuffixOf(xs: List[a], ys: List[a]): Bool = isPrefixOf(reverse(xs), reverse(ys))





    ///
    /// Alias for `foldLeft`.
    ///
    pub def fold(f: (b, a) -> b & e, s: b, xs: List[a]): b & e = foldLeft(f, s, xs)

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
    pub def foldRight(f: (a, b) -> b & e, s: b, xs: List[a]): b & e = match xs {
        case Nil => s
        case x :: rs => f(x, foldRight(f, s, rs))
    }

    ///
    /// Alias for `reduceLeft`.
    ///
    pub def reduce(f: (a, a) -> a & e, xs: List[a]): Option[a] & e = reduceLeft(f, xs)

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
    pub def count(f: a -> Bool, xs: List[a]): Int32 = match xs {
        case Nil => 0
        case x :: rs =>
            let r = count(f, rs);
                if (f(x)) r+1 else r
    }

    ///
    /// Returns the concatenation of the elements in `xs`.
    ///
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
    pub def forall(f: a -> Bool, xs: List[a]): Bool = match xs {
        case Nil => true
        case x :: rs => if (!f(x)) false else forall(f, rs)
    }






    ///
    /// Returns a list of every element in `xs` that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
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
    pub def slice(b: Int32, e: Int32, xs: List[a]): List[a] = sliceHelper(b, e, xs, 0)

    ///
    /// Helper function for `slice`.
    ///
    def sliceHelper(b: Int32, e: Int32, xs: List[a], i: Int32): List[a] = match xs {
        case Nil => Nil
        case x :: rs =>
            let r = sliceHelper(b, e, rs, i+1);
                if (i >= b && i < e) x :: r else r
    }

    ///
    /// Returns a pair of lists `(ys, zs)`.
    ///
    /// `ys` contains all elements of `xs` that satisfy the predicate `f`.
    /// `zs` contains all elements of `xs` that do not satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
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
        case x :: rs => if (f(x, a) && f(a, x)) agreeHelper(f, a, rs) else false
    }






    ///
    /// Returns a list where the element at index `i` is `(a, b)` where
    /// `a` is the element at index `i` in `xs` and `b` is the element at index `i` in `ys`.
    ///
    /// If either `xs` or `ys` becomes depleted, then no further elements are added to the resulting list.
    ///
    @Time(Int32.min(length(xs), length(ys))) @Space(Int32.min(length(xs), length(ys)))
    pub def zip(xs: List[a], ys: List[b]): List[(a,b)] = match (xs, ys) {
        case (x :: rs, y :: qs) => (x, y) :: zip(rs, qs)
        case _ => Nil
    }

    ///
    /// Returns a list where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `xs` and `b` is the element at index `i` in `ys`.
    ///
    /// If either `xs` or `ys` becomes depleted, then no further elements are added to the resulting list.
    ///
    pub def zipWith(f: (a, b) -> c & e, xs: List[a], ys: List[b]): List[c] & e = match (xs, ys) {
        case (x :: rs, y :: qs) => f(x, y) :: zipWith(f, rs, qs)
        case _ => Nil
    }

    ///
    /// Returns a pair of lists, the first containing all first components in `xs`
    /// and the second containing all second components in `xs`.
    ///
    pub def unzip(xs: List[(a, b)]): (List[a], List[b]) = match xs {
        case Nil => (Nil, Nil)
        case (x1, x2) :: rs =>
            let (r1, r2) = unzip(rs);
                (x1 :: r1, x2 :: r2)
    }






    ///
    /// Returns a list where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `xs` and `b` is the element at index `i` in `ys`.
    ///
    /// If either `xs` or `ys` becomes depleted, then no further elements are added to the resulting list.
    /// Functionally equivalent to zipWith.
    ///
    pub def map2(f: (a, b) -> c & e, xs: List[a], ys: List[b]): List[c] & e = zipWith(f, xs, ys)

    ///
    /// Concatenates the results of applying `f` pairwise to the elements of `xs` and `ys`.
    ///
    pub def flatMap2(f: (a, b) -> List[c] & e, xs: List[a], ys: List[b]): List[c] & e = match (xs, ys) {
        case (x :: rs, y :: qs) => f(x, y) ::: flatMap2(f, rs, qs)
        case _ => Nil
    }

    ///
    /// Alias for `foldLeft2`.
    ///
    pub def fold2(f: (c, a, b) -> c & e, c: c, xs: List[a], ys: List[b]): c & e = foldLeft2(f, c, xs, ys)

    ///
    /// Accumulates the result of applying `f` pairwise to the elements of `xs` and `ys`
    /// starting with the initial value `c` and going from left to right.
    ///
    pub def foldLeft2(f: (c, a, b) -> c & e, c: c, xs: List[a], ys: List[b]): c & e = match (xs, ys) {
        case (x :: rs, y :: qs) => foldLeft2(f, f(c, x, y), rs, qs)
        case _ => c
    }

    ///
    /// Accumulates the result of applying `f` pairwise to the elements of `xs` and `ys`
    /// starting with the initial value `c` and going from right to left.
    ///
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
    pub def findMap(f: a -> Option[b] & e, xs: List[a]): Option[b] & e = match xs {
        case Nil => None
        case x :: rs => match f(x) {
                            case None => findMap(f, rs)
                            case Some(v) => Some(v)
        }
    }



    ///
    /// Returns the list of characters `xs` as a string.
    ///
    pub def toString(xs: List[Char]): Str =
        List.foldLeft((acc, c) -> String.concat(acc, Char.toString(c)), "", xs)


    ///
    /// Returns the list `xs` as a set.
    ///
    pub def toSet(xs: List[a]): Set[a] = foldRight(Set.insert, Set.empty(), xs)

    ///
    /// Returns the association list `xs` as a map.
    ///
    /// If `xs` contains multiple mappings with the same key, `toMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    pub def toMap(xs: List[(a, b)]): Map[a, b] = foldRight((x, m) -> Map.insert(fst(x), snd(x), m), Map.empty(), xs)

    ///
    /// Applies `f` to every element of `xs`.
    ///
    /// The function `f` must be impure.
    ///
    pub def foreach(f: a ~> Unit, xs: List[a]): Unit & Impure = match xs {
        case Nil => ()
        case x :: rs => f(x); foreach(f, rs)
    }

    ///
    /// Returns the list `xs` as an array.
    ///
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
    /// Helper function for `unfoldWithOkIter`
    ///
    def unfoldWithOkIterHelper(next: Unit ~> Result[Option[a], e], k: Result[List[a], e] -> Result[List[a], e]): Result[List[a], e] & Impure = match next() {
        case Ok(None) => k(Ok(Nil))
        case Err(e) => k(Err(e))
        case Ok(Some(a)) => unfoldWithOkIterHelper(next, Result.flatMap(xs -> k(Ok(a :: xs))))
    }

    ///
    /// Compares `xs` and `ys` lexicographically.
    ///
    def __cmp(xs: List[a], ys: List[a]): Int32 = match (xs, ys) {
        case (_ :: _, Nil) => 1
        case (Nil, Nil) => 0
        case (Nil, _ :: _) => -1
        case (z :: zs, w :: ws) =>
            let cmp = z <=> w;
            if (cmp == 0) __cmp(zs, ws) else cmp
    }

    ///
    /// Returns `true` if and only if `xs` and `ys` and equal.
    ///
    pub def __eq(xs: List[a], ys: List[a]): Bool = match (xs, ys) {
        case (Nil, Nil) => true
        case (x :: rs, y :: qs) => x == y && __eq(rs, qs)
        case _ => false
    }

    /////////////////////////////////////////////////////////////////////////////
    // Lints: Simplifications                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @lint
    def filterFilter(): Bool =
        forall [a] (f: a -> Bool, g: a -> Bool, xs: List[a]).
            filter(g, filter(f, xs)) === filter(x -> f(x) && g(x), xs)

    @lint
    def findFind(): Bool =
        forall [a] (f: a -> Bool, g: a -> Bool, xs: List[a]).
            Option.find(g, List.find(f, xs)) === find(x -> f(x) && g(x), xs)

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
        List.map(p -> f(fst(p), snd(p)), List.zip(xs, ys)) === List.map2(f, xs, ys)

    /////////////////////////////////////////////////////////////////////////////
    // Lints: Use Cases                                                        //
    /////////////////////////////////////////////////////////////////////////////


}
