/*
 * Copyright 2016 Liam Palmer
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

namespace List {

    ///
    /// Returns true if and only if `xs` is the empty list, i.e. `Nil`.
    ///
    def isEmpty[a](xs: List[a]): Bool = match xs with {
        case Nil => true
        case _ => false
    }

    ///
    /// Returns `Some(x)` if `x` is the first element of `xs`.
    /// Returns `None` if `xs` is empty.
    ///
    def headOpt[a](xs: List[a]): Option[a] = match xs with {
        case Nil => None
        case x :: _ => Some(x)
    }

    ///
    /// Returns `Some(x)` if `x` is the last element of `xs`.
    /// Returns `None` if `xs` is empty.
    ///
    def lastOpt[a](xs: List[a]): Option[a] = match xs with {
        case Nil => None
        case x :: Nil => Some(x)
        case x :: rs => lastOpt(rs)
    }

    ///
    /// Returns the length of `xs`.
    ///
    def length[a](xs: List[a]): Int32 = match xs with {
        case Nil => 0
        case x :: rs => 1 + length(rs)
    }

    ///
    /// Returns `ys` appended to `xs`.
    ///
    def append[a](xs: List[a], ys: List[a]): List[a] = match xs with {
        case Nil => ys
        case x :: rs => x :: append(rs, ys)
    }

    ///
    /// Returns the element at index `i` in `xs`.
    /// If `i < 0` or `i > length(xs)-1` the semantics are undefined.
    ///
    def at[a](i: Int32, xs: List[a]): a = match (i, xs) with {
        case (0, x :: _) => x
        case (p, x :: rs) => at(p - 1, rs)
    }

    ///
    /// Returns `xs` with the first occurrence of `a` removed.
    /// Returns `xs` if `a` does not exist in `xs`.
    ///
    def remove[a](a: a, xs: List[a]): List[a] = match xs with {
        case Nil => Nil
        case x :: rs => if (x == a) rs else x :: remove(a, rs)
    }

    ///
    /// Returns `xs` with the element at index `i` removed.
    /// Returns `xs` if `i < 0` or `i > length(xs)-1`.
    ///
    def removeIndex[a](i: Int32, xs: List[a]): List[a] = match (i, xs) with {
        case (_, Nil) => xs
        case (0, x :: rs) => rs
        case (p, x :: rs) => x :: removeIndex(p - 1, rs)
    }

    ///
    /// Returns true if and only if `xs` contains the element `a`.
    ///
    def memberOf[a](a: a, xs: List[a]): Bool = match xs with {
        case Nil => false
        case x :: rs => if (x == a) true else memberOf(a, rs)
    }

    ///
    /// Returns the position of `a` in `xs`.
    /// Returns -1 if `a` does not exist in `xs`.
    ///
    def indexOf[a](a: a, xs: List[a]): Int32 = indexOfHelper(a, xs, 0)

    ///
    /// Helper function for `indexOf`.
    ///
    @internal
    def indexOfHelper[a](a: a, xs: List[a], acc: Int32): Int32 = match xs with {
        case Nil => -1
        case x :: rs => if (x == a) acc else indexOfHelper(a, rs, acc+1)
    }

    ///
    /// Alias for `findLeft`.
    ///
    def find[a](f: a -> Bool, xs: List[a]): Option[a] = findLeft(f, xs)

    ///
    /// Optionally returns the first element of `xs` that satisfies the predicate `f` when searching from left to right.
    ///
    def findLeft[a](f: a -> Bool, xs: List[a]): Option[a] = match xs with {
        case Nil => None
        case x :: rs => if (f(x)) Some(x) else findLeft(f, rs)
    }

    ///
    /// Optionally returns the first element of `xs` that satisfies the predicate `f` when searching from right to left.
    ///
    def findRight[a](f: a -> Bool, xs: List[a]): Option[a] = match xs with {
        case Nil => None
        case x :: rs => Option.withDefault(findRight(f, rs), if (f(x)) Some(x) else None)
    }

    //
    // ## List Building
    //

    ///
    /// Returns a list of all integers between `b` (inclusive) and `e` (exclusive).
    /// Returns `Nil` if `b >= e`.
    ///
    def range(b: Int32, e: Int32): List[Int32] = if (b >= e) Nil else b :: range(b + 1, e)

    ///
    /// Returns a list with the element `a` repeated `n` times.
    /// Returns `Nil` if `n < 0`.
    ///
    def repeat[a](a: a, n: Int32): List[a] = if (n <= 0) Nil else a :: repeat(a, n - 1)

    ///
    /// Alias for `scanLeft`.
    ///
    def scan[a,b](f: (b, a) -> b, s: b, xs: List[a]): List[b] = scanLeft(f, s, xs)

    ///
    /// Accumulates the result of applying `f` to `xs` going left to right.
    /// That is, the result is of the form: `s :: f(s, x1) :: f(f(s, x1), x2)  ...`.
    ///
    def scanLeft[a,b](f: (b, a) -> b, s: b, xs: List[a]): List[b] = s :: scanLeftHelper(f, s, xs)

    ///
    /// Helper function for `scanLeft`.
    ///
    @internal
    def scanLeftHelper[a,b](f: (b, a) -> b, s: b, xs: List[a]): List[b] = match xs with {
        case Nil => Nil
        case x :: rs => f(s, x) :: scanLeftHelper(f, f(s, x), rs)
    }

    ///
    /// Accumulates the result of applying `f` to `xs` going right to left.
    /// That is, the result is of the form: `... f(xn-1, f(xn, s)) :: f(xn, s) :: s`.
    ///
    def scanRight[a,b](f: (a, b) -> b, s: b, xs: List[a]): List[b] = match xs with {
        case Nil => s :: Nil
        case x :: rs => match scanRight(f, s, rs) with {
            case y :: ys => f(x, y) :: y :: ys
        }
    }

    //
    // ## List Transformation
    //

    ///
    /// Returns the result of applying `f` to every element in `xs`.
    /// That is, the result is of the form: `f(x1) :: f(x2) :: ...`.
    ///
    def map[a,b](f: a -> b, xs: List[a]): List[b] = match xs with {
        case Nil => Nil
        case x :: rs => f(x) :: map(f, rs)
    }

    ///
    /// Returns the result of applying `f` to every element in `xs` along with that element's index.
    /// That is, the result is of the form: `f(x1, 0) :: f(x2, 1) :: ...`.
    ///
    def mapWithIndex[a,b](f: (a, Int32) -> b, xs: List[a]): List[b] = mapWithIndexHelper(f, xs, 0)

    ///
    /// Helper function for `mapWithIndex`.
    ///
    @internal
    def mapWithIndexHelper[a,b](f: (a, Int32) -> b, xs: List[a], i: Int32): List[b] = match xs with {
        case Nil => Nil
        case x :: rs => f(x, i) :: mapWithIndexHelper(f, rs, i+1)
    }

    ///
    /// Returns the result of applying `f` to every element in `xs` and concatenating the results.
    ///
    def flatMap[a,b](f: a -> List[b], xs: List[a]): List[b] = match xs with {
        case Nil => Nil
        case x :: rs => f(x) ::: flatMap(f, rs)
    }

    ///
    /// Returns the reverse of `xs`.
    ///
    def reverse[a](xs: List[a]): List[a] = reverseHelper(xs, Nil)

    ///
    /// Helper function for `reverse`.
    ///
    @internal
    def reverseHelper[a](xs: List[a], acc: List[a]): List[a] = match xs with {
        case Nil => acc
        case x :: rs => reverseHelper(rs, x :: acc)
    }

    ///
    /// Returns `xs` with its elements rotated `n` positions to the left.
    /// That is, returns a new list where the first `n mod length(xs)` elements in `xs`
    /// are the last `n mod length(xs)` elements of the new list.
    ///
    def rotateLeft[a](n: Int32, xs: List[a]): List[a] =
        let len = length(xs);
            if (len == 0) xs else
                let rem = n % len;
                let rotate = if (rem < 0) rem + len else rem;
                    drop(rotate, xs) ::: take(rotate, xs)

    ///
    /// Returns `xs` with its elements rotated `n` positions to the right.
    /// That is, returns a new list where the last `n mod length(xs)` elements in `xs`
    /// are the first `n mod length(xs)` elements of the new list.
    ///
    def rotateRight[a](n: Int32, xs: List[a]): List[a] = rotateLeft(-n, xs)

    ///
    /// Returns `xs` with the element at index `i` replaced by `a`.
    /// Returns `xs` if `i < 0` or `i > length(xs)-1`.
    ///
    def replace[a](i: Int32, a: a, xs: List[a]): List[a] = match (i, xs) with {
        case (_, Nil) => xs
        case (0, x :: rs) => a :: rs
        case (p, x :: rs) => x :: replace(p-1, a, rs)
    }

    ///
    /// Returns `ys` with the `n` elements starting at index `i` replaced with the elements of `xs`.
    /// If any of the indices `i, i+1, i+2, ... , i+n-1` are out of range in `ys` then no patching is done at these indices.
    /// If `xs` becomes depleted then no further patching is done.
    /// If patching occurs at index `i+j` in `ys`, then the element at index `j` in `xs` is used.
    ///
    def patch[a](i: Int32, n: Int32, xs: List[a], ys: List[a]): List[a] = patchHelper(i, n, drop(-i, xs), ys, 0)

    ///
    /// Helper function for `patch`.
    ///
    @internal
    def patchHelper[a](i: Int32, n: Int32, xs: List[a], ys: List[a], c: Int32): List[a] = match (xs, ys) with {
        case (x :: qs, y :: rs) => if (c >= i && c < i+n) x :: patchHelper(i, n, qs, rs, c+1) else y :: patchHelper(i, n, xs, rs, c+1)
        case _ => ys
    }

    ///
    /// Returns all permutations of `xs` in lexicographical order by element indices in `xs`.
    /// That is, `xs` is the first permutation and `reverse(xs)` is the last permutation.
    ///
    def permutations[a](xs: List[a]): List[List[a]] = match xs with {
        case Nil => Nil :: Nil
        case _ => permutationHelper(0, xs)
    }

    ///
    /// Helper function for `permutations`.
    /// Returns all permutations of `xs` starting with an element at or after index `i`.
    ///
    @internal
    def permutationHelper[a](i: Int32, xs: List[a]): List[List[a]] =
        if (i == length(xs)) Nil
        else applyHelper(at(i, xs), permutations(removeIndex(i, xs))) ::: permutationHelper(i+1, xs)

    ///
    /// Returns all subsequences of `xs` in lexicographical order by element indices in `xs`.
    /// That is, `xs` is the first subsequence and `Nil` is the last subsequence.
    ///
    def subsequences[a](xs: List[a]): List[List[a]] = match xs with {
        case Nil => Nil :: Nil
        case x :: rs =>
            let r = subsequences(rs);
                applyHelper(x, r) ::: r
    }

    ///
    /// Helper function for `permutations` and `subsequences`.
    /// Returns `xs` with `a` added to the beginning of each element in `xs`.
    ///
    @internal
    def applyHelper[a](a: a, xs: List[List[a]]): List[List[a]] = match xs with {
        case Nil => Nil
        case x :: rs => (a :: x) :: applyHelper(a, rs)
    }

    ///
    /// Returns `xs` with `a` inserted between every two adjacent elements.
    ///
    def intersperse[a](a: a, xs: List[a]): List[a] = match xs with {
        case x1 :: x2 :: rs => x1 :: a :: intersperse(a, x2 :: rs)
        case _ => xs
    }

    ///
    /// Returns the concatenation of the elements in `ys` with the elements of `xs` inserted between every two adjacent elements.
    /// That is, returns y1 + x + y2 + x + ... + x + yn.
    ///
    def intercalate[a](xs: List[a], ys: List[List[a]]): List[a] = match ys with {
        case Nil => Nil
        case y :: Nil => y
        case y1 :: y2 :: rs => y1 ::: xs ::: intercalate(xs, y2 :: rs)
    }

    ///
    /// Returns the transpose of `xs`.
    /// Returns `xs` if the dimensions of the elements of `xs` are mismatched.
    ///
    def transpose[a](xs: List[List[a]]): List[List[a]] = match xs with {
        case Nil => Nil
        case x :: rs =>
            let len = length(x);
                if (!uniformHelper(xs, len) || len == 0) xs else transposeHelper(xs, len)
    }

    ///
    /// Helper function for `transpose`.
    ///
    @internal
    def uniformHelper[a](xs: List[List[a]], l: Int32): Bool = match xs with {
        case Nil => true
        case x :: rs => if (length(x) == l) uniformHelper(rs, l) else false
    }

    ///
    /// Helper function for `transpose`.
    ///
    @internal
    def transposeHelper[a](xs: List[List[a]], l: Int32): List[List[a]] = match xs with {
        case Nil => repeat(Nil, l)
        case x :: rs => applyListHelper(x, transposeHelper(rs, l))
    }

    ///
    /// Helper function for `transpose`.
    ///
    @internal
    def applyListHelper[a](xs: List[a], ys: List[List[a]]): List[List[a]] = match (xs, ys) with {
        case (Nil, Nil) => Nil
        case (x :: rs, y :: qs) => (x :: y) :: applyListHelper(rs, qs)
    }

    //
    // ## List Predicates
    //

    ///
    /// Returns `true` if and only if `xs` is a prefix of `ys`.
    ///
    def isPrefixOf[a](xs: List[a], ys: List[a]): Bool = match (xs, ys) with {
        case (Nil, _) => true
        case (_, Nil) => false
        case (x :: qs, y :: rs) => if (x == y) isPrefixOf(qs, rs) else false
    }

    ///
    /// Returns `true` if and only if `xs` is an infix of `ys`.
    ///
    def isInfixOf[a](xs: List[a], ys: List[a]): Bool = match (xs, ys) with {
        case (Nil, _) => true
        case (_, Nil) => false
        case (_, y :: rs) => if (isPrefixOf(xs, ys)) true else isInfixOf(xs, rs)
    }

    ///
    /// Returns `true` if and only if `xs` is a suffix of `ys`.
    ///
    def isSuffixOf[a](xs: List[a], ys: List[a]): Bool = isPrefixOf(reverse(xs), reverse(ys))

    //
    // ## Fold And Reduce
    //

    ///
    /// Alias for foldLeft.
    ///
    def fold[a,b](f: (b, a) -> b, s: b, xs: List[a]): b = foldLeft(f, s, xs)

    ///
    /// Applies `f` to a start value `s` and all elements in `xs` going from left to right.
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    def foldLeft[a,b](f: (b, a) -> b, s: b, xs: List[a]): b = match xs with {
        case Nil => s
        case x :: rs => foldLeft(f, f(s, x), rs)
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `xs` going from right to left.
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    def foldRight[a,b](f: (a, b) -> b, s: b, xs: List[a]): b = match xs with {
        case Nil => s
        case x :: rs => f(x, foldRight(f, s, rs))
    }

    ///
    /// Alias for reduceLeftOpt.
    ///
    def reduceOpt[a](f: (a, a) -> a, xs: List[a]): Option[a] = reduceLeftOpt(f, xs)

    ///
    /// Applies `f` to all elements in `xs` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    /// That is, the result is of the form: `Some(f(...f(f(x1, x2), x3)..., xn))`
    /// Returns `None` is `xs` is empty.
    ///
    def reduceLeftOpt[a](f: (a, a) -> a, xs: List[a]): Option[a] = match xs with {
        case Nil => None
        case x :: rs => Some(foldLeft(f, x, rs))
    }

    ///
    /// Applies `f` to all elements in `xs` going from right to left until a single value `v` is obtained.  Returns `Some(v)`.
    /// That is, the result is of the form: `Some(f(x1, ...f(xn-2, f(xn-1, xn))...))`
    /// Returns `None` is `xs` is empty.
    ///
    def reduceRightOpt[a](f: (a, a) -> a, xs: List[a]): Option[a] = match xs with {
        case Nil => None
        case x :: rs => match reduceRightOpt(f, rs) with {
            case None => Some(x)
            case Some(v) => Some(f(x, v))
        }
    }

    //
    // ## Special Folds
    //

    ///
    /// Returns the number of elements in `xs` that satisfy the predicate `f`.
    ///
    def count[a](f: a -> Bool, xs: List[a]): Int32 = match xs with {
        case Nil => 0
        case x :: rs =>
            let r = count(f, rs);
                if (f(x)) r+1 else r
    }

    ///
    /// Returns the concatenation of the elements in `xs`.
    ///
    def concat[a](xs: List[List[a]]): List[a] = match xs with {
        case Nil => Nil
        case x :: rs => x ::: concat(rs)
    }

    ///
    /// Returns `true` if and only if at least one element in `xs` satisfies the predicate `f`.
    ///
    def exists[a](f: a -> Bool, xs: List[a]): Bool = match xs with {
        case Nil => false
        case x :: rs => if (f(x)) true else exists(f, rs)
    }

    ///
    /// Returns `true` if and only if all elements in `xs` satisfy the predicate `f`.
    ///
    def forall[a](f: a -> Bool, xs: List[a]): Bool = match xs with {
        case Nil => true
        case x :: rs => if (!f(x)) false else forall(f, rs)
    }

    ///
    /// Returns `true` if and only if all elements in `xs` are `true`.
    ///
    def and[a](xs: List[Bool]): Bool =
        let f = x -> x;
            forall(f, xs)

    ///
    /// Returns `true` if and only if at least one element in `xs` is `true`.
    ///
    def or[a](xs: List[Bool]): Bool =
        let f = x -> x;
            exists(f, xs)

    //
    // ## Sub Lists
    //

    ///
    /// Returns a list of every element in `xs` that satisfies the predicate `f`.
    ///
    def filter[a](f: a -> Bool, xs: List[a]): List[a] = match xs with {
        case Nil => Nil
        case x :: rs =>
            let r = filter(f, rs);
                if (f(x)) x :: r else r
    }

    ///
    /// Returns the sublist of `xs` from index `b` (inclusive) to index `e` (exclusive).
    ///
    def slice[a](b: Int32, e: Int32, xs: List[a]): List[a] = sliceHelper(b, e, xs, 0)

    ///
    /// Helper function for `slice`.
    ///
    @internal
    def sliceHelper[a](b: Int32, e: Int32, xs: List[a], i: Int32): List[a] = match xs with {
        case Nil => Nil
        case x :: rs =>
            let r = sliceHelper(b, e, rs, i+1);
                if (i >= b && i < e) x :: r else r
    }

    ///
    /// Returns a pair of lists `(ys, zs)`.
    /// `ys` contains all elements of `xs` that satisfy the predicate `f`.
    /// `zs` contains all elements of `xs` that do not satisfy the predicate `f`.
    ///
    def partition[a](f: a -> Bool, xs: List[a]): (List[a], List[a]) = match xs with {
        case Nil => (Nil, Nil)
        case x :: rs =>
            let (r1, r2) = partition(f, rs);
                if (f(x)) (x :: r1, r2) else (r1, x :: r2)
    }

    ///
    /// Returns a pair of lists `(ys, zs)`.
    /// `ys` is the longest prefix of `xs` that satisfies the predicate `f`.
    /// `zs` is the remainder of `xs`.
    ///
    def span[a](f: a -> Bool, xs: List[a]): (List[a], List[a]) = match xs with {
        case Nil => (Nil, Nil)
        case x :: rs => if (f(x))
                            let (r1, r2) = span(f, rs);
                                (x :: r1, r2)
                        else
                            (Nil, xs)
    }

    ///
    /// Returns `xs` with the first `n` elements removed.
    /// Returns `Nil` if `n > length(xs)`.
    /// Returns `xs` if `n < 0`.
    ///
    def drop[a](n: Int32, xs: List[a]): List[a] = if (n < 0) xs else match (n, xs) with {
        case (_, Nil) => Nil
        case (0, _) => xs
        case (i, x :: rs) => drop(i-1, rs)
    }

    ///
    /// Returns `xs` with the longest prefix that satisfies the predicate `f` removed.
    //
    def dropWhile[a](f: a -> Bool, xs: List[a]): List[a] = match xs with {
        case Nil => Nil
        case x :: rs => if (f(x)) dropWhile(f, rs) else xs
    }

    ///
    /// Returns the first `n` elements of `xs`.
    /// Returns `xs` if `n > length(xs)`.
    /// Returns `Nil` if `n < 0`.
    ///
    def take[a](n: Int32, xs: List[a]): List[a] = if (n < 0) Nil else match (n, xs) with {
        case (_, Nil) => Nil
        case (0, _) => Nil
        case (i, x :: rs) => x :: take(i-1, rs)
    }

    ///
    /// Returns the longest prefix of `xs` that satisfies the predicate `f`.
    //
    def takeWhile[a](f: a -> Bool, xs: List[a]): List[a] = match xs with {
        case Nil => Nil
        case x :: rs => if (f(x)) x :: takeWhile(f, rs) else Nil
    }

    //
    // ## Aggregation and Sorting
    //

    ///
    /// Partitions `xs` into sublists such that for any two elements `x` and `y` in a sublist, `f(x, y)` is true.
    /// A sublist is created by iterating through the remaining elements of `xs` from left to right and adding an
    /// element to the sublist if and only if doing so creates no conflicts with the elements already in the sublist.
    ///
    def groupBy[a](f: (a, a) -> Bool, xs: List[a]): List[List[a]] = match xs with {
        case Nil => Nil
        case x :: rs =>
            let (r1, r2) = extractHelper(f, rs, x :: Nil, Nil);
                r1 :: groupBy(f, r2)
    }

    ///
    /// Helper function for `groupBy`.
    ///
    @internal
    def extractHelper[a](f: (a, a) -> Bool, xs: List[a], ps: List[a], ns: List[a]): (List[a], List[a]) = match xs with {
        case Nil => (reverse(ps), reverse(ns))
        case x :: rs => if (agreeHelper(f, x, ps)) extractHelper(f, rs, x :: ps, ns) else extractHelper(f, rs, ps, x :: ns)
    }

    ///
    /// Helper function for `groupBy`.
    ///
    @internal
    def agreeHelper[a](f: (a, a) -> Bool, a: a, xs: List[a]): Bool = match xs with {
        case Nil => true
        case x :: rs => if (f(x, a) && f(a, x)) agreeHelper(f, a, rs) else false
    }

    //
    // ## Zipping and Unzipping
    //

    ///
    /// Returns a list where the element at index `i` is `(a, b)` where
    /// `a` is the element at index `i` in `xs` and `b` is the element at index `i` in `ys`.
    /// If either `xs` or `ys` becomes depleted, then no further elements are added to the resulting list.
    ///
    def zip[a,b](xs: List[a], ys: List[b]): List[(a,b)] = match (xs, ys) with {
        case (x :: rs, y :: qs) => (x, y) :: zip(rs, qs)
        case _ => Nil
    }

    ///
    /// Returns a list where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `xs` and `b` is the element at index `i` in `ys`.
    /// If either `xs` or `ys` becomes depleted, then no further elements are added to the resulting list.
    ///
    def zipWith[a,b,c](f: (a, b) -> c, xs: List[a], ys: List[b]): List[c] = match (xs, ys) with {
        case (x :: rs, y :: qs) => f(x, y) :: zipWith(f, rs, qs)
        case _ => Nil
    }

    ///
    /// Returns a pair of lists, the first containing all first components in `xs`
    /// and the second containing all second components in `xs`.
    ///
    def unzip[a,b](xs: List[(a,b)]): (List[a], List[b]) = match xs with {
        case Nil => (Nil, Nil)
        case (x1, x2) :: rs =>
            let (r1, r2) = unzip(rs);
                (x1 :: r1, x2 :: r2)
    }

    //
    // ## Two List Operations
    //

    ///
    /// Returns a list where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `xs` and `b` is the element at index `i` in `ys`.
    /// If either `xs` or `ys` becomes depleted, then no further elements are added to the resulting list.
    /// Functionally equivalent to zipWith.
    ///
    def map2[a,b,c](f: (a, b) -> c, xs: List[a], ys: List[b]): List[c] = zipWith(f, xs, ys)

    ///
    /// Concatenates the results of applying `f` pairwise to the elements of `xs` and `ys`.
    ///
    def flatMap2[a,b,c](f: (a, b) -> List[c], xs: List[a], ys: List[b]): List[c] = match (xs, ys) with {
        case (x :: rs, y :: qs) => f(x, y) ::: flatMap2(f, rs, qs)
        case _ => Nil
    }

    ///
    /// Accumulates the result of applying `f` pairwise to the elements of `xs` and `ys`
    /// starting with the initial value `c` and going from left to right.
    /// Alias for foldLeft2.
    ///
    def fold2[a,b,c](f: (c, a, b) -> c, c: c, xs: List[a], ys: List[b]): c = foldLeft2(f, c, xs, ys)

    ///
    /// Accumulates the result of applying `f` pairwise to the elements of `xs` and `ys`
    /// starting with the initial value `c` and going from left to right.
    ///
    def foldLeft2[a,b,c](f: (c, a, b) -> c, c: c, xs: List[a], ys: List[b]): c = match (xs, ys) with {
        case (x :: rs, y :: qs) => foldLeft2(f, f(c, x, y), rs, qs)
        case _ => c
    }

    ///
    /// Accumulates the result of applying `f` pairwise to the elements of `xs` and `ys`
    /// starting with the initial value `c` and going from right to left.
    ///
    def foldRight2[a,b,c](f: (a, b, c) -> c, c: c, xs: List[a], ys: List[b]): c =
        let len1 = length(xs);
        let len2 = length(ys);
            foldRight2Helper(f, c, drop(len1-len2, xs), drop(len2-len1, ys))

    ///
    /// Helper function for `foldRight2`.
    ///
    def foldRight2Helper[a,b,c](f: (a, b, c) -> c, c: c, xs: List[a], ys: List[b]): c = match (xs, ys) with {
        case (x :: rs, y :: qs) => f(x, y, foldRight2(f, c, rs, qs))
        case _ => c
    }

    //
    // ## Combined Operations
    //

    ///
    /// Returns the result of applying `f` to every element in `xs` and concatenating the results.
    /// Alias for flatMap.
    ///
    def concatMap[a,b](f: a -> List[b], xs: List[a]): List[b] = flatMap(f, xs)

    ///
    /// Collects the results of applying the partial function `f` to every element in `xs`.
    ///
    def filterMap[a,b](f: a -> Option[b], xs: List[a]): List[b] = match xs with {
        case Nil => Nil
        case x :: rs => match f(x) with {
                            case None => filterMap(f, rs)
                            case Some(v) => v :: filterMap(f, rs)
        }
    }

    ///
    /// Returns the first non-None result of applying the partial function `f` to each element of `xs`.
    /// Returns `None` if every element of `xs` is `None`.
    ///
    def findMap[a,b](f: a -> Option[b], xs: List[a]): Option[b] = match xs with {
        case Nil => None
        case x :: rs => match f(x) with {
                            case None => findMap(f, rs)
                            case Some(v) => Some(v)
        }
    }

    //
    // ## List Conversions
    //
    def toSet[a](xs: List[a]): Set[a] = foldRight(Set/insert, Set/empty(), xs)

    // ## TODO
    ////- **toMap(xs: List[(A, B)]): Map[A, B]**
    ////    - Returns the association list as a map. If the list contains multiple values for the same key, the value appearing latest in the list is used.
}