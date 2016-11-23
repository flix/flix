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
    /// Returns true iff `xs` is the empty list, i.e. `Nil`.
    ///
    def null[a](xs: List[a]): Bool = match xs with {
        case Nil => true
        case _ => false
    }

    ///
    /// Returns the first element of `xs`.
    /// If `xs` is empty the semantics are undefined.
    ///
    def head[a](xs: List[a]): a = match xs with {
        case x :: _ => x
    }

    ///
    /// Returns the last element of `xs`.
    /// If `xs` is empty the semantics are undefined.
    ///
    def last[a](xs: List[a]): a = match xs with {
        case x :: Nil => x
        case x :: rs => last(rs)
    }

    ///
    /// Returns the list consisting of every element of `xs` except the last element.
    /// If `xs` is empty the semantics are undefined.
    ///
    def init[a](xs: List[a]): List[a] = match xs with {
        case x :: Nil => Nil
        case x :: rs => x :: init(rs)
    }

    ///
    /// Returns the list consisting of every element of `xs` except the first element.
    /// If `xs` is empty the semantics are undefined.
    ///
    def tail[a](xs: List[a]): List[a] = match xs with {
        case _ :: rs => rs
    }

    ///
    /// Returns the first `n` elements of `xs` in a list.
    /// If `n < 0` or `n > length(xs)` the semantics are undefined.
    ///
    def prefix[a](n: Int32, xs: List[a]): List[a] = match (n, xs) with {
        case (0, _) => Nil
        case (i, x :: rs) => x :: prefix(i-1, rs)
    }

    ///
    /// Returns the last `n` elements of `xs` in a list.
    /// If `n < 0` or `n > length(xs)` the semantics are undefined.
    ///
    def suffix[a](n: Int32, xs: List[a]): List[a] = match (length(xs) - n, xs) with {
        case (0, _) => xs
        case (_, x :: rs) => suffix(n, rs)
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
    /// Returns the element at position `p` in `xs`.
    /// If `p` < 0 or `p` > length(xs)-1 the semantics are undefined.
    ///
    def at[a](p: Int32, xs: List[a]): a = match (p, xs) with {
        case (0, x :: _) => x
        case (i, x :: rs) => at(i - 1, rs)
    }

    ///
    /// Returns `xs` with the element at position `p` removed.
    /// If `p` < 0 or `p` > length(xs)-1 the semantics are undefined.
    ///
    def remove[a](p: Int32, xs: List[a]): List[a] = match (p, xs) with {
        case (0, x :: rs) => rs
        case (i, x :: rs) => x :: remove(i - 1, rs)
    }

    ///
    /// Returns true iff `xs` contains the element `x`.
    ///
    def memberOf[a](x: a, xs: List[a]): Bool = match xs with {
        case Nil => false
        case y :: rs => if (x == y) true else memberOf(x, rs)
    }

    ///
    /// Returns the position of `x` in `xs`.
    /// If `x` does not exist in `xs` the semantics are undefined.
    ///
    def indexOf[a](x: a, xs: List[a]): Int32 = match xs with {
        case y :: rs => if (x == y) 0 else 1 + indexOf(x, rs)
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
        case x :: rs => match findRight(f, rs) with {
            case None => if (f(x)) Some(x) else None
            case Some(v) => Some(v)
        }
    }

    //
    // ## List Building
    //

    ///
    /// Returns a list of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    def range(b: Int32, e: Int32): List[Int32] =
        if (b >= e)
            Nil
        else
            b :: range(b + 1, e)

    ///
    /// Returns a list with the element `x` repeated `n` times.
    ///
    def repeat[a](x: a, n: Int32): List[a] =
        if (n == 0)
            Nil
        else
            x :: repeat(x, n - 1)

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
    /// Accumulates the result of applying `f` to `xs` going left to right without including `s` at the beginning.
    /// That is, scanLeft(f, s, xs) = s :: scanLeftHelper(f, s, xs).
    ///
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
    /// Returns the result of applying `f` to every element in `xs` along with that element's index + `i`.
    /// That is, the result is of the form: `f(x1, i) :: f(x2, i+1) :: ...`.
    /// mapWithIndex(f, xs) = mapWithIndexHelper(f, xs, 0).
    ///
    def mapWithIndexHelper[a,b](f: (a, Int32) -> b, xs: List[a], i: Int32): List[b] = match xs with {
        case Nil => Nil
        case x :: rs => f(x, i) :: mapWithIndexHelper(f, rs, i+1)
    }

    ///
    /// Returns the result of applying `f` to every element in `xs` and concatenating the results.
    /// Alias for `flatMap`.
    ///
    def concatMap[a,b](f: a -> List[b], xs: List[a]): List[b] = flatMap(f, xs)

    ///
    /// Returns the result of applying `f` to every element in `xs` and concatenating the results.
    /// Aliased as concatMap.
    ///
    def flatMap[a,b](f: a -> List[b], xs: List[a]): List[b] = match xs with {
        case Nil => Nil
        case x :: rs => append(f(x), flatMap(f, rs))
    }

    ///
    /// Returns the reverse of `xs`.
    ///
    def reverse[a](xs: List[a]): List[a] = reverseHelper(xs, Nil)

    ///
    /// Helper function for `reverse`.
    /// Returns `acc` appended to the reverse of `xs`.
    /// reverse(xs) = reverseHelper(xs, Nil).
    ///
    def reverseHelper[a](xs: List[a], acc: List[a]): List[a] = match xs with {
        case Nil => acc
        case x :: rs => reverseHelper(rs, x :: acc)
    }

    ///
    /// Returns `xs` with its elements rotated `n` positions to the left.
    /// That is, returns a new list where the first `n` elements in `xs` are the last `n` elements of the new list.
    /// If `n < 0` or `n > length(xs)` the semantics are undefined.
    ///
    def rotateLeft[a](n: Int32, xs: List[a]): List[a] = append(suffix(length(xs)-n, xs), prefix(n, xs))

    ///
    /// Returns `xs` with its elements rotated `n` positions to the right.
    /// That is, returns a new list where the last `n` elements in `xs` are the first `n` elements of the new list.
    /// If `n < 0` or `n > length(xs)` the semantics are undefined.
    ///
    def rotateRight[a](n: Int32, xs: List[a]): List[a] = append(suffix(n, xs), prefix(length(xs)-n, xs))

    ///
    /// Returns `xs` with the element at position `p` replaced by `x`.
    /// If `p < 0` or `p > length(xs)-1` the semantics are undefined.
    ///
    def replace[a](p: Int32, x: a, xs: List[a]): List[a] = match (p, xs) with {
        case (0, r :: rs) => x :: rs
        case (i, r :: rs) => r :: replace(i-1, x, rs)
    }

    ///
    /// Returns `ys` with the `n` elements starting at position `p` replaced with elements from `xs`.
    /// If `n = 0` then `ys` is returned.
    /// Otherwise, if `n < 0` or `p < 0` or `p + n > length(xs)` the semantics are undefined.
    ///
    def patch[a](p: Int32, n: Int32, xs: List[a], ys: List[a]): List[a] = match (p, n, xs, ys) with {
        case (_, 0, _, _) => ys
        case (0, _, x :: qs, y :: rs) => x :: patch(0, n-1, qs, rs)
        case (l, _, _, y :: rs) => y :: patch(l-1, n, xs, rs)
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
    /// Returns all permutations of `xs` starting with an element at or after position `p`.
    ///
    def permutationHelper[a](p: Int32, xs: List[a]): List[List[a]] =
        if (p == length(xs)) Nil
        else append(apply(at(p, xs), permutations(remove(p, xs))), permutationHelper(p+1, xs))

    ///
    /// Returns all subsequences of `xs` in lexicographical order by element indices in `xs`.
    /// That is, `xs` is the first subsequence and `Nil` is the last subsequence.
    ///
    def subsequences[a](xs: List[a]): List[List[a]] = match xs with {
        case Nil => Nil :: Nil
        case x :: rs => append(apply(x, subsequences(rs)), subsequences(rs))
    }

    ///
    /// Helper function for `permutations` and `subsequences`.
    /// Returns `xs` with `x` added to the beginning of each element of `xs`.
    ///
    def apply[a](x: a, ps: List[List[a]]): List[List[a]] = match ps with {
        case Nil => Nil
        case p :: rs => (x :: p) :: apply(x, rs)
    }

    ///
    /// Returns `xs` with `x` inserted between every two adjacent elements.
    ///
    def intersperse[a](x: a, xs: List[a]): List[a] = match xs with {
        case x1 :: x2 :: rs => x1 :: x :: intersperse(x, x2 :: rs)
        case _ => xs
    }

////- **intersperse(x: A, xs: List[A]): List[A]**
////    - Returns the list with the element x inserted between every element.
////- **intercalate(xs: List[A], ys: List[List[A]]): List[A]**
////    - Returns the concatenation of the list ys with the list xs inserted between every element.
////- **transpose(xs: List[List[A]]): List[List[A]]**
////    - Returns the transpose of the list.
////    - If the dimensions of the lists are mismatched the semantics are undefined.
////
//
//    //
//    // ## List Predicates
//    //
//
//    /**
//     *  Returns `true` if the list `xs` is a prefix of the list `ys`.
//     */
//    def isPrefixOf(xs: List[A], ys: List[B]): Bool = match (xs, ys) with {
//        case (Nil, _) => true
//        case (_, Nil) => false
//        case (x :: xs1, y :: ys1) => if (x == y) isPrefixOf(xs1, ys1) else false
//    }
//
//    /**
//     *  Returns `true` if the list `xs` is an infix of the list `ys`.
//     */
//    def isInfixOf(xs: List[A], ys: List[B]): Bool = ??? : Bool // TODO
//
//    /**
//     *  Returns `true` if the list `xs` is a suffix of the list `ys`.
//     */
//    def isSuffixOf(xs: List[A], ys: List[B]): Bool = isPrefixOf(reverse(xs), reverse(ys))
//
//    //
//    // ## Fold And Reduce
//    //
//
//    /**
//     *  Alias of foldLeft.
//     */
//    def fold(f: (B, A) -> B, b: B, xs: List[A]): B = foldLeft(f, b, xs)
//
//    /**
//     *  Left-associative fold of the list xs.
//     */
//    def foldLeft(f: (B, A) -> B, b: B, xs: List[A]): B = ??? : B
//
//    //- **foldRight(f: (A, B) => B, b: B, xs: List[A]): B**
//    //    - Right-associative fold of the list xs.
//    //- **reduce(f: (A, A) => A, xs: List[A]): A**
//    //    - Alias for reduceLeft.
//    //- **reduceLeft(f: (A, A) => A, xs: List[A]): A**
//    //    - Left-associative reduce of the list xs.
//    //    - If the list is empty the semantics are undefined.
//    //- **reduceOpt(f: (A, A) => A, xs: List[A]): Opt[A]**
//    //    - Alias for reduceLeftOpt.
//    //- **reduceLeftOpt(f: (A, A) => A, xs: List[A]): Opt[A]**
//    //    - Safe variant of reduceLeft that returns None if the list is empty.
//    //- **reduceRight(f: (A, A) => A, xs: List[A]): A**
//    //    - Right-associative reduce of the list xs.
//    //    - If the list is empty the semantics are undefined.
//    //- **reduceRightOpt(f: (A, A) => A, xs: List[A]): Opt[A]**
//    //    - Safe variant of reduceRight that returns None if the list is empty.
////
////## Special Folds
////- **count(f: A => Bool, xs: List[A]): Int**
////    - Returns the number of elements in the list that satisfy the predicate f.
////- **concat(xs: List[List[A]]): List[A]**
////    - Returns the concatenation of the lists in the list xs.
////- **exists(f: A => Bool, xs: List[A]): Bool**
////    - Returns true iff at least one element in the list satisfies the predicate f.
////    - Returns false if the list is empty.
////- **forall(f: A => Bool, xs: List[A]): Bool**
////    - Returns true iff every element in the list satisfies the predicate f.
////    - Returns true if the list is empty.
////- **and(xs: List[Bool]): Bool**
////    - Returns true iff every element in the list is true. Returns true if the list is empty.
////- **or(xs: List[Bool]): Bool**
////    - Returns true iff at least one element in the list is true. Returns false if the list is empty.
////
////## Sub Lists
////- **filter(f: A => Bool, xs: List[A]): List[A]**
////    - Returns a list of every element in the list xs that satisfy the predicate f.
////- **slice(b: Int, e: Int, xs: List[A]): List[A]**
////    - Returns the sublist from position b (inclusive) until position e (exclusive).
////- **partition(f: A => Bool, xs: List[A]): (List[A], List[A])**
////    - Returns a pair of lists (ys, zs) where are the elements in xs are those that satisfy the predicate f and the elements in zs are those that do not.
////- **span(f: A => Bool, xs: List[A]): (List[A], List[A])**
////    - Returns a pair of lists (ys, zs) where ys is the longest prefix of xs that satisfies the predicate f and zs is the remainder of xs. Functionally equivalent to (takeWhile(f, xs), dropWhile(f, xs)).
////- **drop(n: Int, xs: List[A]): List[A]**
////    - Returns the list except for the first n elements.
////    - Returns the empty list if n is larger than the length of the list.
////- **dropWhile(f: A => Bool, xs: List[A]): List[A]**
////    - Returns the list except for the longest prefix that satisfy the predicate f.
////- **take(n: Int, xs: List[A]): List[A]**
////    - Returns the first n elements of xs.
////    - Returns the entire list if n is larger than the length of the list.
////- **takeWhile(f: A => Bool, xs: List[A]): List[A]**
////    - Returns the longest prefix of xs that satisfy the predicate f.
////
////## Aggregation and Sorting
////- **sum(xs: List[Int]): Int**
////    - Returns the sum of the elements in the list.
////    - Returns zero if the list is empty.
////- **product(xs: List[Int]): Int**
////    - Returns the product of the elements in the list.
////    - Returns one if the list is empty.
////- **min(xs: List[Int]): Int**
////    - Returns the minimum element of a non-empty list.
////    - If the list is empty the semantics are undefined.
////- **max(xs: List[Int]): Int**
////    - Returns the maximum element of a non-empty list.
////    - If the list is empty the semantics are undefined.
////- **minBy(f: (A, A) => Bool, xs: List[Int]): Int**
////    - Returns the minimum element of a non-empty list according the total order defined by the predicate f. That is, if f(x, y) == true then x is smaller or equal to y. Two elements are equal if f(x, y) = f(y, x).
////    - If the list is empty the semantics are undefined.
////- **maxBy(f: (A, A) => Bool, xs: List[Int]): Int**
////    - Similar to minBy but returns the largest element.
////- **sort(xs: List[Int]): Int**
////    - Returns the elements of the list sorted according to the natural order.
////- **sortBy(f: (A, A) => Bool, xs: List[Int]): Int**
////    - Returns the elements of the list sorted according to the total order defined by the predicate f. That is, if f(x, y) == true then x is smaller or equal to y. Two elements are equal if f(x, y) = f(y, x).
////- **groupBy(f: (A, A) => Bool, xs: List[A]): List[List[A]]**
////    - Partitions the list xs into sublists such that for any two elements x and y in a sub list f(x, y) is true.
////
////## Zipping and Unzipping
////- **zip(xs: List[A], ys: List[B]): List[(A, B)]**
////    - Returns a list where each element at index i is the pair (a, b) where a is the element at position i in xs and b is the element at position i in ys.
////- **zipWith(f: (A, B) => C, xs: List[A], ys: List[B]): List[C]**
////    - Like zip but uses the function f to compute each element in the result.
////    - Functionally equivalent to: map(f, zip(xs, ys)).
////- **unzip(xs: List[(A, B)]): (List[A], List[B])**
////    - Returns a pair where the first component is a list of all first components in xs and the second component is a list of all second components in xs.
////
////## Two List Operations
////- **map2(f: (A, B) => C, xs: List[A], ys: List[B]): List[C]**
////    - Pairwise applies the function f to the elements of the lists xs and ys. Functionally equivalent to zipWith.
////- **flatMap2(f: (A, B) => List[C], xs: List[A], ys: List[B]): List[C]**
////    - Pairwise applies the function f to the elements of the lists xs and ys and concatenates the result in one list. Functionally equivalent to flatten(map2(f, xs, ys)).
////- **fold2(f: (C, A, B) => C, c: C, xs: List[A], ys: List[B]): C**
////    - Alias for foldLeft2.
////- **foldLeft2(f: (C, A, B) => C, c: C, xs: List[A], ys: List[B]): C**
////    - Accumulates the result of applying the function f pairwise to the elements of xs and ys starting with the initial value c going from left to right. Functionally similar to foldLeft(f, c, zip(xs, ys)).
////- **foldRight2(f: (A, B, C) => C, c: C, xs: List[A], ys: List[B]): C**
////    - Similar to foldLeft2 but goes from the right to the left.
////
////## Combined Operations
////- **concatMap(f: A => List[B], xs: List[A]): List[B]**
////    - Alias for flatMap.
////- **filterMap(f: A => Opt[B], xs: List[A]): List[B]**
////    - Collects the result of applying the partial function f to each element of the list.
////- **findMap(f: A => Opt[B], xs: List[A]): Opt[B]**
////    - Collects the first result of applying the partial function f to each element of the list.
////
////## List Conversions
////- **toMap(xs: List[(A, B)]): Map[A, B]**
////    - Returns the association list as a map. If the list contains multiple values for the same key, the value appearing latest in the list is used.
////- **toSet(xs: List[A]): Set[A]**
////    - Returns the list as a set.
////
////## Order and Lattice Operations
////- **leq(xs: List[A], ys: List[A]): Bool**
////    - Returns true if every element in xs is pairwise smaller or equal to its corresponding element in ys. That is, for any position i in xs, it must be the case that at(i, xs) is smaller or equal to at(i, ys) (if it exists) according to the partial order on A. Functionally equivalent to to zipWith(leq, xs, ys).and where leq is the partial order on A.
////- **isAscChain(xs: List[A]): Bool**
////    - Returns true iff the elements in xs form an ascending chain. That is, if  according to the partial order on A.
////    - NB: In general, isAscChain is not the negation of isDescChain since a list may fail to be a chain due to two elements being incomparable.
////- **isDescChain(xs: List[A]): Bool**
////    - Returns true iff the elements in xs form a descending chain. That is, if  according to the partial order on A.
////    - NB: In general, isDescChain is not the negation of isAscChain since a list may fail to be a chain due to two elements being incomparable.
////- **join(xs: List[A]): A**
////    - Returns the least upper bound of all element in xs. If the list is empty, returns the bottom element of A. Functionally equivalent to foldLeft(lub, bot, xs) where lub and bot are the least upper bound and bottom element of A, respectively.
////- **meet(xs: List[A]): A**
////    - Similar to lub, but with the greatest lower bound and top element.
////- **widen(xs: List[A]): A**
////    - Similar to lub, but with the widening operator.
////- **narrow(xs: List[A]): A**
////    - Similar to lub, but with the narrowing operator.
////- **zipWithJoin(xs: List[A], ys: List[A]): List[A]**
////    - Returns the pairwise least upper bound of the two lists xs and ys. Functionally equivalent to zipWith(lub, xs, ys) where lub is the least upper bound of A.
////- **zipWithMeet(xs: List[A], ys: List[A]): List[A]**
////    - Similar to zipWithJoin, but with the greatest lower bound.
////
//
//
}