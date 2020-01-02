/*
 * Copyright 2019 Magnus Madsen
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

namespace Array {

    ///
    /// Returns a fresh array of length `l` filled with the element `x`.
    ///
    /// Equivalent to the expression `[x; l]`.
    ///
    pub def new(x: a, l: Int): Array[a] = [x; l]

    ///
    /// Returns the element at position `i` in the array `a`.
    ///
    /// Equivalent to the expression `a[i]`.
    ///
    pub def get(a: Array[a], i: Int): a = a[i]

    ///
    /// Stores the value `x` at position `i` in the array `a`.
    ///
    /// Equivalent to the expression `a[i] = x`.
    ///
    pub def put(a: Array[a], i: Int, x: a): Array[a] = a[i] = x; a

    ///
    /// Returns the length of the array `a`.
    ///
    pub def length(a: Array[a]): Int = length[a]

    ///
    /// Returns a fresh array with the elements from the array `a` from index `b` (inclusive) until index `e` (exclusive).
    ///
    /// Equivalent to the expression `a[b..e]`.
    ///
    pub def slice(a: Array[a], b: Int, e: Int): Array[a] = a[b..e]

    ///
    /// Returns the array `a` as a list.
    ///
    pub def toList(a: Array[a]): List[a] = toListHelper(a, 0)

    ///
    /// Helper for toList.
    ///
    def toListHelper(a: Array[a], i: Int): List[a] =
        if (i == length(a))
            Nil
        else
            a[i] :: toListHelper(a, i + 1)

    ///
    /// Build an array of length `len` by applying `f` to the successive indices.
    ///
    pub def init(f: Int32 -> a, len: Int32): Array[a] =
        if (len <= 0)
            []
        else {
            let x = f(0);
            let a = new(x, len);
            initHelper(a, f, len, 1);
            a
        }

    ///
    /// Helper function for `init`.
    ///
    def initHelper(a: Array[a], f: i -> a, len: Int32, i: Int32): Unit =
        if (i >= len)
            ()
        else {
            a[i] = f(i);
            initHelper(a, f, len, i+1)
        }

    ///
    /// Apply the effectful function `f` to all the elements in the array `a`.
    ///
    pub def iter(f: a -> Unit, a: Array[a]): Unit =
        let len = length(a);
        if (len < 1) () else iterHelper(f, a, len, 0)

    ///
    /// Helper for `iter`.
    ///
    def iterHelper(f: a -> Unit, a: Array[a], len: Int32, i: Int32): Unit =
        if (i >= len)
            ()
        else {
            f(a[i]);
            iterHelper(f, a, len, i + 1)
        }


    ///
    /// Returns an array of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns `Nil` if `b >= e`.
    ///
    pub def range(b: Int32, e: Int32): Array[Int32] =
        if (b >= e)
            []
        else {
            let f = x -> x + b;
            init(f, e-b)
        }

    ///
    /// Return a new array, appending the elements `b` to elements of `a`.
    ///
    pub def append(a: Array[a], b: Array[b]): Array[a] =
        let len1 = length(a);
        let len2 = length(b);
        if (len2 == 0)
            slice(a, 0, len1)
        else
            if (len1 == 0)
                slice(b, 0, len2)
            else {
                let c = new(b[0], len1 + len2);
                let j = appendHelper(a, c, 0, 0);
                let _ = appendHelper(b, c, 0, j);
                c
            }
    ///
    /// Helper function for `append`.
    ///
    def appendHelper(a: Array[a], c: Array[a], i: Int32, j: Int32): Int32 =
        if (i >= length(a))
            j
        else {
            let _ = c[j] = a[i];
            appendHelper(a, c, i+1, j+1)
        }
    ///
    /// Returns the concatenation of the elements in `a`.
    ///
    pub def flatten(a: Array[Array[a]]) : Array[a] =
        foldLeft((acc,b) -> append(acc,b), [], a)
    ///
    /// Returns the array of characters `a` as a string.
    ///
    pub def toString(a: Array[Char]) : Str =
        let sb = StringBuilder.new();
        iter(c -> StringBuilder.appendChar(sb,c), a);
        StringBuilder.toString(sb)

    ///
    /// Returns the result of applying `f` to every element in `a`.
    ///
    /// The result is a new array.
    ///
    pub def map(f: a -> b, a: Array[a]): Array[b] =
        let len = length(a);
        init(i -> f(a[i]), len)


    ///
    /// Returns an array where the element at index `i` is `f(a[i], b[i])`.
    ///
    /// If either `a` or `b` becomes depleted, then no further elements are added to the resulting array.
    ///
    pub def map2(f: (a, b) -> c, a: Array[a], b: Array[b]): Array[c] =
        let len = Int32.min(length(a), length(b));
        init(i -> f(a[i], b[i]), len)



    ///
    /// Returns the result of applying `f` to every element in `a` along with that element's index.
    ///
    /// That is, the result is of the form: `[ f(a[0], 0), f(a[1], 1), ... ]`.
    ///
    pub def mapWithIndex(f: (a, Int32) -> b, a: Array[a]): Array[b] =
        let len = length(a);
        init(i -> f(a[i], i), len)

    ///
    /// Returns the result of applying `f` to every element in `a` and concatenating the results.
    ///
    pub def flatMap(f: a -> Array[b], a: Array[a]): Array[b] =
        let len = length(a);
        init(i -> f(a[i]), len) |> flatten

    ///
    /// Concatenates the results of applying `f` pairwise to the elements of `a` and `b`.
    ///
    pub def flatMap2(f: (a, b) -> Array[c], a: Array[a], b: Array[b]): Array[c] =
        let len = Int32.min(length(a), length(b));
        init(i -> f(a[i], b[i]), len) |> flatten


    ///
    /// Returns the number of elements in `a` that satisfy the predicate `f`.
    ///
    pub def count(f: a -> Bool, a: Array[a]): Int32 =
        foldLeft((b,x) -> if (f(x)) b + 1 else b, 0, a)

    ///
    /// Returns an array with the element `x` repeated `n` times.
    ///
    /// Returns `[]` if `n <= 0`.
    ///
    pub def repeat(x: a, n: Int32): Array[a] =
        if (n > 0) new(x, n) else []

    ///
    /// Returns the reverse of `a`.
    ///
    pub def reverse(a: Array[a]): Array[a] =
        let len = length(a);
        let end = len - 1;
        init(i -> a[end - i], len)

    ///
    /// Alias for `scanLeft`.
    ///
    pub def scan(f: (b, a) -> b, s: b, xs: Array[a]): Array[b] = scanLeft(f, s, xs)

    ///
    /// Accumulates the result of applying `f` to `a` going left to right.
    ///
    /// That is, the result is of the form: `[s , f(s, x1), f(f(s, x1), x2),  ...]`.
    ///
    pub def scanLeft(f: (b, a) -> b, s: b, a: Array[a]): Array[b] =
        let len = length(a) + 1;
        let b = new(s, len);
        scanLeftHelper(f, s, a, b, len, 1);
        b

    ///
    /// Helper for `scanLeft`.
    ///
    def scanLeftHelper(f: (b, a) -> b, s: b, a: Array[a], b: array[b], len: Int32, i: Int32) : Unit =
        if (i >= len)
            ()
        else {
            let s1 = f(s, a[i-1]);
            b[i] = s1;
            scanLeftHelper(f, s1, a, b, len, i+1)
        }

    ///
    /// Accumulates the result of applying `f` to `xs` going right to left.
    ///
    /// That is, the result is of the form: `[..., f(xn-1, f(xn, s)), f(xn, s), s]`.
    ///
    pub def scanRight(f: (a, b) -> b, s: b, a: Array[a]): Array[b] =
        let len = length(a);
        let b = new(s, len+1);
        scanRightHelper(f, s, a, b, len-1);
        b

    ///
    /// Helper for `scanRight`.
    ///
    def scanRightHelper(f: (a, b) -> b, s: b, a: Array[a], b: array[b], i: Int32) : Unit =
        if (i < 0)
            ()
        else {
            let s1 = f(a[i], s);
            b[i] = s1;
            scanRightHelper(f, s1, a, b, i-1)
        }

    ///
    /// Alias for `foldLeft`.
    ///
    pub def fold(f: (b, a) -> b, s: b, a: Array[a]): b = foldLeft(f, s, a)


    ///
    /// Applies `f` to a start value `s` and all elements in `a` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, a[0]), a[1])..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b, s: b, a: Array[a]): b =
        foldLeftHelper(f, s, a, 0)

    ///
    /// Helper function for `foldLeft`.
    ///
    def foldLeftHelper(f: (b, a) -> b, s: b, a: Array[a], i: Int): b =
        if (i >= length(a))
            s
        else
            let s1 = f(s, a[i]);
            foldLeftHelper(f, s1, a, i + 1)


    ///
    /// Applies `f` to a start value `s` and all elements in `a` going from right to left.
    ///
    /// That is, the result is of the form: `f(a[0], ...f(a[n-1], f(a[n], s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b, s: b, a: Array[a]): b =
        foldRightHelper(f, s, a, length(a) - 1)

    ///
    /// Helper function for `foldRight`.
    ///
    def foldRightHelper(f: (a, b) -> b, s: b, a: Array[a], i: Int): b =
        if (i < 0)
            s
        else
            let s1 = f(a[i], s);
            foldRightHelper(f, s1, a, i - 1)

    ///
    /// Alias for `foldLeft2`.
    ///
    pub def fold2(f: (c, a, b) -> c, c: c, a: Array[a], b: Array[b]): c =
        foldLeft2(f, c, a, b)

    ///
    /// Accumulates the result of applying `f` pairwise to the elements of `a` and `b`
    /// starting with the initial value `c` and going from left to right.
    ///
    pub def foldLeft2(f: (c, a, b) -> c, c: c, a: Array[a], b: Array[b]): c =
        foldLeft2Helper(f, c, a, b, 0)

    ///
    /// Helper function for `foldLeft2`.
    ///
    def foldLeft2Helper(f: (c, a, b) -> c , s: c, a: Array[a], b: Array[b], i: Int): c =
        if (i >= length(a) || i >= length(b))
            s
        else
            let s1 = f(s, a[i], b[i]);
            foldLeft2Helper(f, s1, a, b, i + 1)

    ///
    /// Accumulates the result of applying `f` pairwise to the elements of `a` and `b`
    /// starting with the initial value `c` and going from right to left.
    ///
    pub def foldRight2(f: (a, b, c) -> c, c: c, a: Array[a], b: Array[b]): c =
        let len1 = length(a);
        let len2 = length(b);
        let i = Int32.min(len1, len2) - 1;
        foldRight2Helper(f, c, drop(len1-len2, a), drop(len2-len1, b), i)

    ///
    /// Helper function for `foldRight2`.
    ///
    def foldRight2Helper(f: (a, b, c) -> c , s: c, a: Array[a], b: Array[b], i: Int): c =
        if (i < 0)
            s
        else
            let s1 = f(a[i], b[i], s);
            foldRight2Helper(f, s1, a, b, i - 1)

    ///
    /// Alias for `reduceLeft`.
    ///
    pub def reduce(f: (a, a) -> a, a: Array[a]): Option[a] = reduceLeft(f, a)

    ///
    /// Applies `f` to all elements in `a` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// Returns `None` if `a` is empty.
    ///
    pub def reduceLeft(f: (a, a) -> a, a: Array[a]): Option[a] =
        if (length(a) > 0)
            Some(reduceLeftHelper(f, a, a[0], 1))
        else
            None

    ///
    /// Helper function for `reduceLeft`.
    ///
    def reduceLeftHelper(f: (a, a) -> a, a: Array[a], x: a, i:Int32): a =
        if (i >= length(a))
            x
        else
            reduceLeftHelper(f, a, f(x, a[i]), i+1)

    ///
    /// Applies `f` to all elements in `a` going from right to left until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// Returns `None` if `a` is empty.
    ///
    pub def reduceRight(f: (a, a) -> a, a: Array[a]): Option[a] =
        if (length(a) > 0)
            let end = length(a) - 1;
            let ac = a[end];
            Some(reduceRightHelper(f, a, ac, end-1))
        else
            None

    ///
    /// Helper function for `reduceRight`.
    ///
    def reduceRightHelper(f: (a, a) -> a, a: Array[a], x: a, i:Int32): a =
        if (i < 0)
            x
        else
            reduceRightHelper(f, a, f(a[i], x), i-1)

    ///
    /// Returns `a` without the first `n` elements.
    ///
    /// Returns `[]]` if `n > length(xs)`.
    /// Returns `a` if `n < 0`.
    ///
    pub def drop(n: Int, a: Array[a]) : Array[a] =
        if (n > length(a))
            []
        else {
            if (n < 1)
                slice(a, 0, length(a))
            else {
                let last = length(a);
                slice(a, n, last)
            }
        }

    ///
    /// Returns `a` without the longest prefix that satisfies the predicate `f`.
    ///
    pub def dropWhile(f: a -> Bool, a: Array[a]): Array[a] =
        match findIndexOf(x -> !(f(x)), a) {
            case None => []
            case Some(i) => drop(i, a)
        }


    ///
    /// Returns the first `n` elements of `a`.
    ///
    /// Returns `a` if `n > length(xs)`.
    /// Returns `[]]` if `n < 0`.
    ///
    pub def take(n: Int, a: Array[a]) : Array[a] =
        if (n > length(a))
            a
        else {
            if (n < 1)
                []
            else
                slice(a, 0, n)
        }

    ///
    /// Returns the longest prefix of `a` that satisfies the predicate `f`.
    ///
    pub def takeWhile(f: a -> Bool, a: Array[a]): Array[a] =
        match findIndexOf(x -> !(f(x)), a) {
            case None => a
            case Some(i) => take(i, a)
        }

    ///
    /// Returns `true` if and only if at least one element in `a` satisfies the predicate `f`.
    ///
    /// Returns `false` if `a` is empty.
    ///
    pub def exists(f: a -> Bool, a: Array[a]): Bool =
        existsHelper(f, a, 0)

    ///
    /// Helper function for `exists`.
    ///
    def existsHelper(f: a -> Bool, a: Array[a], i: Int): Bool =
        if (i >= length(a))
            false
        else
            if (f(a[i]) == true)
                true
            else
                existsHelper(f, a, i+1)

    ///
    /// Returns `true` if and only if all elements in `a` satisfy the predicate `f`.
    ///
    /// Returns `true` if `a` is empty.
    ///
    pub def forall(f: a -> Bool, a: Array[a]): Bool =
        forallHelper(f, a, 0)

    ///
    /// Helper function for `forall`.
    ///
    def forallHelper(f: a -> Bool, a: Array[a], i: Int): Bool =
        if (i >= length(a))
            true
        else
            if (f(a[i]) == false)
                false
            else
                forallHelper(f, a, i+1)


    ///
    /// Returns `true` if and only if `a` contains the element `x`.
    ///
    pub def memberOf(x: a, a: Array[a]): Bool =
        exists(y -> y == x, a)

    /// Alias for `IndexOfLeft`
    ///
    pub def indexOf(x: a, a: Array[a]): Option[Int32] = indexOfLeft(x,a)

    ///
    /// Optionally returns the position of the first occurence of `x` in `a`
    /// searching from left to right.
    ///
    pub def indexOfLeft(x: a, a: Array[a]): Option[Int32] =
        let i = indexOfLeftHelper(x, a, 0);
        if (i < 0) None else Some(i)

    ///
    /// Helper function for `indexOfLeft`.
    ///
    def indexOfLeftHelper(x: a, a: Array[a], i: Int32): Int32 =
        if (i >= length(a))
            -1
        else
            if (a[i] == x) i else indexOfLeftHelper(x, a, i+1)

    ///
    /// Optionally returns the position of the first occurence of `x` in `a`
    /// searching from right to left.
    ///
    pub def indexOfRight(x: a, a: Array[a]): Option[Int32] =
        let start = length(a) - 1;
        let i = indexOfRightHelper(x, a, start);
        if (i < 0) None else Some(i)

    ///
    /// Helper function for `indexOfLeft`.
    ///
    def indexOfRightHelper(x: a, a: Array[a], i: Int32): Int32 =
        if (i < 0)
            -1
        else
            if (a[i] == x) i else indexOfRightHelper(x, a, i-1)

    ///
    /// Alias for `findLeft`.
    ///
    pub def find(f: a -> Bool, xs: Array[a]): Option[a] = findLeft(f, xs)

    ///
    /// Optionally returns the first element of `a` that satisfies the predicate `f` when searching from left to right.
    ///
    pub def findLeft(f: a -> Bool, a: Array[a]): Option[a] =
        match findIndexOfLeft(x -> f(x), a) {
            case None => None
            case Some(i) => Some(a[i])
        }

    ///
    /// Optionally returns the first element of `xs` that satisfies the predicate `f` when searching from right to left.
    ///
    pub def findRight(f: a -> Bool, a: Array[a]): Option[a] =
        match findIndexOfRight(x -> f(x), a) {
            case None => None
            case Some(i) => Some(a[i])
        }

    ///
    /// Alias for `findIndexOfLeft`.
    ///
    pub def findIndexOf(f: a -> Bool, a: Array[a]): Option[Int] = findIndexOfLeft(f,a)

    ///
    /// Optionally returns the position of the first element in `x` satisfying `f`.
    ///
    pub def findIndexOfLeft(f: a -> Bool, a: Array[a]): Option[Int] =
        if (length(a) < 1)
            None
        else {
            let i = findIndexOfLeftHelper(f, a, 0);
            if (i < 0) None else Some(i)
        }

    ///
    /// Helper function for `findIndexOfLeft`.
    ///
    def findIndexOfLeftHelper(f: a -> Bool, a: Array[a], i: Int32): Int32 =
        if (i >= length(a))
            -1
        else
            if (f(a[i])) i else findIndexOfLeftHelper(f, a, i + 1)

    ///
    /// Optionally returns the position of the first element in `x` satisfying `f`
    /// searching from right to left.
    ///
    pub def findIndexOfRight(f: a -> Bool, a: Array[a]): Option[Int] =
        let len = length(a);
        if (len < 1)
            None
        else {
            let i = findIndexOfRightHelper(f, a, len-1);
            if (i < 0) None else Some(i)
        }

    ///
    /// Helper function for `findIndexOfRight`.
    ///
    def findIndexOfRightHelper(f: a -> Bool, a: Array[a], i: Int32): Int32 =
        if (i < 0)
            -1
        else
            if (f(a[i])) i else findIndexOfRightHelper(f, a, i - 1)

    ///
    /// Returns an array of every element in `a` that satisfies the predicate `f`.
    ///
    pub def filter(f: a -> Bool, a: Array[a]): Array[a] =
        foldRight((x,xs) -> if (f(x)) x :: xs else xs, Nil, a) |> List.toArray

    ///
    /// Collects the results of applying the partial function `f` to every element in `a`.
    ///
    pub def filterMap(f: a -> Option[b], a: Array[a]): Array[b] =
        foldRight((x,xs) -> match f(x) {
            case None => xs
            case Some(b) => b :: xs }, Nil, a) |> List.toArray

    ///
    /// Returns the first non-None result of applying the partial function `f` to each element of `xs`.
    ///
    /// Returns `None` if every element of `xs` is `None`.
    ///
    pub def findMap(f: a -> Option[b], a: Array[a]): Option[b] =
        findMapHelper(f, a, 0)

    ///
    /// Helper function for `findMap`.
    ///
    def findMapHelper(f: a -> Option[b], a: Array[a], i: Int32): Option[b] =
        if (i >= length(a))
            None
        else
            match (f(a[i])) {
                case Some(v) => Some(v)
                case None => findMapHelper(f, a, i+1)
            }

    ///
    /// Rotate the contents of array `a` by `n` steps to the left.
    ///
    pub def rotateLeft(n: Int32, a: Array[a]): Array[a] =
        if (length(a) < 1)
            []
        else {
            if (n < 0)
                rotateRightHelper(Int32.abs(n), a)
            else
                rotateLeftHelper(n, a)
        }

    ///
    /// Helper function for `rotateLeft`, `n` must be positive.
    ///
    def rotateLeftHelper(n: Int32, a: Array[a]): Array[a] =
        let len = length(a);
        let f = i -> { let i1 = n + i; a[i1 % len] };
        init(f, len)

    ///
    /// Rotate the contents of array `a` by `n` steps to the right.
    ///
    pub def rotateRight(n: Int32, a: Array[a]): Array[a] =
        if (length(a) < 1)
            []
        else {
            if (n < 0)
                rotateLeftHelper(Int32.abs(n), a)
            else
                rotateRightHelper(n, a)
        }

    ///
    /// Helper function for `rotateRight`, `n` must be positive.
    ///
    pub def rotateRightHelper(n: Int32, a: Array[a]): Array[a] =
        let len = length(a);
        let n1 = n % len;
        let start = len - n1;
        let f = i -> { let i1 = start + i; a[i1 % len] };
        init(f, len)

    ///
    /// Returns `a` with every occurrence of `x` replaced by `y`.
    ///
    pub def replace(x: a, y: a, a: Array[a]): Array[a] =
        map(e -> if (e == x) y else e, a)

    ///
    /// Returns an array where the element at index `i` is `(x, y)` where
    /// `x` is the element at index `i` in `a` and `y` is the element at index `i` in `b`.
    ///
    /// If either `a` or `b` becomes depleted, then no further elements are added to the resulting array.
    ///
    pub def zip(a: Array[a], b: Array[b]): Array[(a,b)] =
        let len = Int32.min(length(a), length(b));
        init(i -> (a[i], b[i]), len)

    ///
    /// Returns an array where the element at index `i` is `f(x, y)` where
    /// `x` is the element at index `i` in `a` and `y` is the element at index `i` in `b`.
    ///
    /// If either `a` or `b` becomes depleted, then no further elements are added to the resulting array.
    ///
    pub def zipWith(f: (a, b) -> c, a: Array[a], b: Array[b]): Array[c] =
        let len = Int32.min(length(a), length(b));
        init(i -> f(a[i], b[i]), len)

    ///
    /// Returns the transpose of `a`.
    ///
    /// Returns `a` if the dimensions of the elements of `a` are mismatched.
    ///
    pub def transpose(a: Array[Array[a]]): Array[Array[a]] =
        let ilen = length(a);
        if (ilen == 0)
            []
        else {
            let jlen = length(a[0]);
            if (jlen == 0 || uniformHelper(a, jlen))
                a
            else {
                init(i -> init(j -> a[j][i], ilen), jlen)
            }
        }

    ///
    /// Helper function for `transpose`.
    ///
    def uniformHelper(a: Array[Array[a]], l: Int32): Bool =
        exists(x -> length(x) != l, a)
}
