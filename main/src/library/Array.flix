/*
 * Copyright 2019 Magnus Madsen, Stephen Tetley
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


// TODO: Unsafe instance. Will be removed. (Blocked by Datalog engine usage.)
instance Foldable[Array] {
    pub def foldLeft(f: (b, a) -> b & ef, s: b, a: Array[a]): b & ef = Array.foldLeft(f, s, a) as & ef
    pub def foldRight(f: (a, b) -> b & ef, s: b, a: Array[a]): b & ef = Array.foldRight(f, s, a) as & ef
}

namespace Array {

    ///
    /// Returns `true` if the elements of `a` are equal to the elements of `b`.
    ///
    pub def eq(a: Array[v], b: Array[v]): Bool & Impure with Eq[v] =
        (Array.length(a) == Array.length(b)) and
        (Array.foldLeft((acc, p) -> let (x,y) = p; (acc and (x == y)), true, Array.zip(a, b)))

    ///
    /// Compares `a` and `b` lexicographically.
    ///
    pub def compare(a: Array[v], b: Array[v]): Comparison & Impure with Order[v] =
        compareHelper(a, b, Int32.min(Array.length(a), Array.length(b)), 0)

    def compareHelper(a: Array[v], b: Array[v], len: Int32, i: Int32): Comparison & Impure with Order[v] =
        if (i < len) {
            let cmp = a[i] <=> b[i];
            if (cmp == EqualTo)
                compareHelper(a, b, len, i + 1)
            else
                cmp
        } else if (i < Array.length(a)) {
            GreaterThan
        } else if (i < Array.length(b)) {
            LessThan
        } else {
            EqualTo
        }

    ///
    /// Returns a fresh array of length `l` filled with the element `x`.
    ///
    /// Equivalent to the expression `[x; l]`.
    ///
    @Time(l) @Space(l)
    pub def new(x: a, l: Int32): Array[a] & Impure = [x; l]

    ///
    /// Returns the element at position `i` in the array `a`.
    ///
    /// Equivalent to the expression `a[i]`.
    ///
    @Time(1) @Space(1)
    pub def get(i: Int32, a: Array[a]): a & Impure = a[i]

    ///
    /// Stores the value `x` at position `i` in the array `a`.
    ///
    /// Equivalent to the expression `a[i] = x`.
    ///
    @Time(1) @Space(1)
    pub def put(x: a, i: Int32, a: Array[a]): Array[a] & Impure = a[i] = x; a

    ///
    /// Returns `true` if the given array `a` is empty.
    ///
    @Time(1) @Space(1)
    pub def isEmpty(a: Array[a]): Bool = a.length == 0

    ///
    /// Returns the length of the array `a`.
    ///
    @Time(1) @Space(1)
    pub def length(a: Array[a]): Int32 = a.length

    ///
    /// Returns a fresh array with the elements from the array `a` from index `b` (inclusive) until index `e` (exclusive).
    ///
    /// Equivalent to the expression `a[b..e]`.
    ///
    @Time(e - b) @Space(e - b)
    pub def slice(b: Int32, e: Int32, a: Array[a]): Array[a] & Impure = a[b..e]

    ///
    /// Returns the array `a` as a list.
    ///
    @Time(length(a)) @Space(length(a))
    pub def toList(a: Array[a]): List[a] & Impure = toListHelper(a, a.length, Nil)

    ///
    /// Helper function for `toList`.
    ///
    def toListHelper(a: Array[a], i: Int32, acc: List[a]): List[a] & Impure =
        if (i == 0) {
            acc
        } else {
            let x = a[i - 1];
            toListHelper(a, i - 1, x :: acc)
        }

    ///
    /// Returns the array `a` as a lazy list.
    ///
    pub def toLazyList(a: Array[a]): LazyList[a] & Impure =
        LList(lazy toLazyListHelper(a, 0)) as & Impure

    ///
    /// Helper function for `toList`.
    ///
    def toLazyListHelper(a: Array[a], i: Int32): LazyList[a] =
        if (i == a.length) {
            ENil
        } else {
            let x = a[i] as & Pure; // TODO: Unsafe cast.
            LCons(x, lazy toLazyListHelper(a, i + 1))
        }

    ///
    /// Returns the array `arr` as a chain.
    ///
    pub def toChain(arr: Array[a]): Chain[a] & Impure =
        toChainHelper(arr, arr.length, Chain.empty())

    ///
    /// Helper function for `toChain`.
    ///
    def toChainHelper(a: Array[a], i: Int32, ac: Chain[a]): Chain[a] & Impure =
        if (i == 0) {
            ac
        } else {
            let x = a[i - 1];
            toChainHelper(a, i - 1, Chain.cons(x, ac))
        }

    ///
    /// Returns `Some(x)` if `x` is the first element of `a`.
    ///
    /// Returns `None` if `a` is empty.
    ///
    @Time(1) @Space(1)
    pub def head(a: Array[a]): Option[a] & Impure =
        if (length(a) > 0) Some(a[0]) else None

    ///
    /// Returns `Some(x)` if `x` is the last element of `a`.
    ///
    /// Returns `None` if `a` is empty.
    ///
    @Time(1) @Space(1)
    pub def last(a: Array[a]): Option[a] & Impure =
        let len = length(a);
        if (len > 0) Some(a[len-1]) else None

    ///
    /// Return a new array, appending the elements `b` to elements of `a`.
    ///
    @Time(length(a) + length(b)) @Space(length(a) + length(b))
    pub def append(a: Array[a], b: Array[a]): Array[a] & Impure =
        appendHelper(a, b)

    ///
    /// Helper function for `append`.
    ///
    def appendHelper(a: Array[a], b: Array[a]): Array[a] & Impure =
        let len1 = length(a);
        let len2 = length(b);
        if (len1 == 0)
            slice(0, len2, b)
        else {
            let out = new(a[0], len1 + len2);
            updateSequence!(0, a, out);
            updateSequence!(len1, b, out);
            out
        }

    ///
    /// Returns `true` if and only if `a` contains the element `x`.
    ///
    @Time(length(a)) @Space(1)
    pub def memberOf(x: a, a: Array[a]): Bool & Impure with Eq[a] =
        exists(y -> y == x, a)

    ///
    /// Optionally finds the smallest element of `a` according to the `Order` on `a`.
    ///
    /// Returns `None` if `a` is empty.
    ///
    pub def minimum(a: Array[a]): Option[a] & Impure with Order[a] =
        reduceLeft(Order.min, a)

    ///
    /// Optionally finds the smallest element of `a` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `a` is empty.
    ///
    pub def minimumBy(cmp: (a, a) -> Comparison, a: Array[a]): Option[a] & Impure =
        reduceLeft(Order.minBy(cmp), a)

    ///
    /// Optionally finds the largest element of `a` according to the `Order` on `a`.
    ///
    /// Returns `None` if `a` is empty.
    ///
    pub def maximum(a: Array[a]): Option[a] & Impure with Order[a] =
        reduceLeft(Order.max, a)

    ///
    /// Optionally finds the largest element of `a` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `a` is empty.
    ///
    pub def maximumBy(cmp: (a, a) -> Comparison, a: Array[a]): Option[a] & Impure =
        reduceLeft(Order.maxBy(cmp), a)

    ///
    /// Alias for `IndexOfLeft`
    ///
    @Time(length(a)) @Space(1)
    pub def indexOf(x: a, a: Array[a]): Option[Int32] & Impure with Eq[a] = indexOfLeft(x,a)

    ///
    /// Optionally returns the position of the first occurence of `x` in `a`
    /// searching from left to right.
    ///
    @Time(length(a)) @Space(1)
    pub def indexOfLeft(x: a, a: Array[a]): Option[Int32] & Impure with Eq[a] =
        let i = indexOfLeftHelper(x, a, length(a), 0);
        if (i < 0) None else Some(i)

    ///
    /// Helper function for `indexOfLeft`.
    ///
    def indexOfLeftHelper(x: a, a: Array[a], len:Int32, i: Int32): Int32 & Impure with Eq[a] =
        if (i >= len)
            -1
        else
            if (a[i] == x)
                i
            else
                indexOfLeftHelper(x, a, len, i+1)

    ///
    /// Optionally returns the position of the first occurence of `x` in `a`
    /// searching from right to left.
    ///
    @Time(length(a)) @Space(1)
    pub def indexOfRight(x: a, a: Array[a]): Option[Int32] & Impure with Eq[a] =
        let start = length(a) - 1;
        let i = indexOfRightHelper(x, a, start);
        if (i < 0) None else Some(i)

    ///
    /// Helper function for `indexOfRight`.
    ///
    def indexOfRightHelper(x: a, a: Array[a], i: Int32): Int32 & Impure with Eq[a] =
        if (i < 0)
            -1
        else
            if (a[i] == x)
                i
            else
                indexOfRightHelper(x, a, i-1)

    ///
    /// Return the positions of the all the occurences of `x` in `a`.
    ///
    @Time(length(a)) @Space(length(a))
    pub def indices(x: a, a: Array[a]): Array[Int32] & Impure with Eq[a] =
        findIndices(y -> x == y, a)

    ///
    /// Alias for `findLeft`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(xs)) @Space(space(f))
    pub def find(f: a -> Bool, xs: Array[a]): Option[a] & Impure = findLeft(f, xs)

    ///
    /// Optionally returns the first element of `a` that satisfies the predicate `f` when searching from left to right.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(a)) @Space(space(f))
    pub def findLeft(f: a -> Bool, a: Array[a]): Option[a] & Impure =
        match findIndexOfLeft(x -> f(x), a) {
            case None => None
            case Some(i) => Some(a[i])
        }

    ///
    /// Optionally returns the first element of `xs` that satisfies the predicate `f` when searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(a)) @Space(space(f))
    pub def findRight(f: a -> Bool, a: Array[a]): Option[a] & Impure =
        match findIndexOfRight(x -> f(x), a) {
            case None => None
            case Some(i) => Some(a[i])
        }

    ///
    /// Returns an array of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns `[]` if `b >= e`.
    ///
    @Time(e - b) @Space(e - b)
    pub def range(b: Int32, e: Int32): Array[Int32] & Impure =
        if (b >= e)
            []
        else {
            let f = x -> x + b;
            init(f, e-b)
        }

    ///
    /// Returns an array with the element `x` repeated `n` times.
    ///
    /// Returns `[]` if `n <= 0`.
    ///
    @Time(n) @Space(n)
    pub def repeat(n: Int32, x: a): Array[a] & Impure =
        if (n <= 0)
            []
        else
            new(x, n)

    ///
    /// Alias for `scanLeft`.
    ///
    pub def scan(f: (b, a) -> b & ef, s: b, xs: Array[a]): Array[b] & Impure = scanLeft(f, s, xs)

    ///
    /// Accumulates the result of applying `f` to `a` going left to right.
    ///
    /// That is, the result is of the form: `[s , f(s, x1), f(f(s, x1), x2),  ...]`.
    ///
    pub def scanLeft(f: (b, a) -> b & ef, s: b, a: Array[a]): Array[b] & Impure =
        let len = length(a) + 1;
        let b = new(s, len);
        scanLeftHelper(f, s, a, b, len, 1);
        b

    ///
    /// Helper function for `scanLeft`.
    ///
    def scanLeftHelper(f: (b, a) -> b & ef, s: b, a: Array[a], b: Array[b], len: Int32, i: Int32) : Unit & Impure =
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
    pub def scanRight(f: (a, b) -> b & ef, s: b, a: Array[a]): Array[b] & Impure =
        let len = length(a);
        let b = new(s, len+1);
        scanRightHelper(f, s, a, b, len-1);
        b

    ///
    /// Helper function for `scanRight`.
    ///
    def scanRightHelper(f: (a, b) -> b & ef, s: b, a: Array[a], b: Array[b], i: Int32) : Unit & Impure =
        if (i < 0)
            ()
        else {
            let s1 = f(a[i], s);
            b[i] = s1;
            scanRightHelper(f, s1, a, b, i-1)
        }

    ///
    /// Returns the result of applying `f` to every element in `a`.
    ///
    /// The result is a new array.
    ///
    @Time(time(f) * length(a)) @Space(space(f) * length(a))
    pub def map(f: a -> b & ef, a: Array[a]): Array[b] & Impure =
        let len = length(a);
        init(i -> f(a[i]), len)

    ///
    /// Apply `f` to every element in array `a`. Array `a` is mutated.
    ///
    @Time(time(f) * length(a)) @Space(space(f) * length(a))
    pub def transform!(f: a -> a, a: mut Array[a]): Unit & Impure =
        transformHelper!(f, length(a), a, 0)

    ///
    /// Helper function for `transform!`
    ///
    def transformHelper!(f: a -> a, len: Int32, a: mut Array[a], i: Int32): Unit & Impure =
        if (i >= len)
            ()
        else {
            a[i] = f(a[i]);
            transformHelper!(f, len, a, i+1)
        }

    ///
    /// Returns the result of applying `f` to every element in `a` along with that element's index.
    ///
    /// That is, the result is of the form: `[ f(a[0], 0), f(a[1], 1), ... ]`.
    ///
    @Time(time(f) * length(a)) @Space(space(f) * length(a))
    pub def mapWithIndex(f: (a, Int32) -> b & ef, a: Array[a]): Array[b] & Impure =
        let len = length(a);
        init(i -> f(a[i], i), len)

    ///
    /// Apply `f` to every element in array `a` along with that element's index. Array `a` is mutated.
    ///
    @Time(time(f) * length(a)) @Space(space(f))
    pub def transformWithIndex!(f: (a, Int32) -> a, a: mut Array[a]): Unit & Impure =
        transformWithIndexHelper!(f, length(a), a, 0)

    ///
    /// Helper function for `transformWithIndex!`
    ///
    def transformWithIndexHelper!(f: (a, Int32) -> a, len: Int32, a: mut Array[a], i: Int32): Unit & Impure =
        if (i >= len)
            ()
        else {
            a[i] = f(a[i], i);
            transformWithIndexHelper!(f, len, a, i+1)
        }

    ///
    /// Returns the result of applying `f` to every element in `a` and concatenating the results.
    ///
    @Time(time(f) * length(a)) @Space(space(f) * length(a))
    pub def flatMap(f: a -> Array[b] & ef, a: Array[a]): Array[b] & Impure =
        let len = length(a);
        init(i -> f(a[i]), len) |> flatten

    ///
    /// Returns the reverse of `a`.
    ///
    @Time(length(a)) @Space(length(a))
    pub def reverse(a: Array[a]): Array[a] & Impure =
        let len = length(a);
        let end = len - 1;
        init(i -> a[end - i], len)

    ///
    /// Reverse the array `a`, mutating it in place.
    ///
    @Time(length(a)) @Space(1)
    pub def reverse!(a: mut Array[a]): Unit & Impure =
        let len = length(a);
        reverseHelper!(a, len / 2, 0, len - 1)

    ///
    /// Helper function for `reverse!`.
    ///
    def reverseHelper!(a: mut Array[a], halflen: Int32, i: Int32, j: Int32): Unit & Impure =
        if (i >= halflen)
            ()
        else {
            let x = a[i];
            let y = a[j];
            put(y, i, a);
            put(x, j, a);
            reverseHelper!(a, halflen, i+1, j-1)
        }

    ///
    /// Rotate the contents of array `a` by `n` steps to the left.
    ///
    @Time(n) @Space(length(a))
    pub def rotateLeft(n: Int32, a: Array[a]): Array[a] & Impure =
        if (length(a) < 1)
            []
        else
            if (n < 0)
                rotateRightHelper(Int32.abs(n), a)
            else
                rotateLeftHelper(n, a)

    ///
    /// Helper function for `rotateLeft`.
    ///
    /// Precondition: `n` must be positive.
    ///
    def rotateLeftHelper(n: Int32, a: Array[a]): Array[a] & Impure =
        let len = length(a);
        let f = i -> { let i1 = n + i; a[i1 rem len] };
        init(f, len)

    ///
    /// Rotate the contents of array `a` by `n` steps to the right.
    ///
    @Time(n) @Space(length(a))
    pub def rotateRight(n: Int32, a: Array[a]): Array[a] & Impure =
        if (length(a) < 1)
            []
        else
            if (n < 0)
                rotateLeftHelper(Int32.abs(n), a)
            else
                rotateRightHelper(n, a)

    ///
    /// Helper function for `rotateRight`.
    ///
    /// Precondition: `n` must be positive.
    ///
    def rotateRightHelper(n: Int32, a: Array[a]): Array[a] & Impure =
        let len = length(a);
        let n1 = n rem len;
        let start = len - n1;
        let f = i -> { let i1 = start + i; a[i1 rem len]};
        init(f, len)

    ///
    /// Returns a copy of `a` with the element at index `i` replaced by `x`.
    ///
    /// Returns a shallow copy of `a` if `i < 0` or `i > length(xs)-1`.
    ///
    @Time(length(a)) @Space(length(a))
    pub def update(i: Int32, x: a, a: Array[a]): Array[a] & Impure =
        let f = ix -> if (ix == i) x else a[ix];
        init(f, length(a))

    ///
    /// Returns a copy of `a` with every occurrence of `x` replaced by `y`.
    ///
    @Time(length(a)) @Space(length(a))
    pub def replace(x: a, y: a, a: Array[a]): Array[a] & Impure with Eq[a] =
        map(e -> if (e == x) y else e, a)

    ///
    /// Replace every occurrence of `x` by `y` in the array `a`, mutating it in place.
    ///
    @Time(length(a)) @Space(1)
    pub def replace!(x: a, y: a, a: mut Array[a]): Unit & Impure with Eq[a] =
        transform!(e -> if (e == x) y else e, a)

    ///
    /// Returns `b` with the `n` elements starting at index `i` replaced with the elements of `a`.
    ///
    /// If any of the indices `i, i+1, i+2, ... , i+n-1` are out of range in `b` then no patching is done at these indices.
    /// If `a` becomes depleted then no further patching is done.
    /// If patching occurs at index `i+j` in `b`, then the element at index `j` in `a` is used.
    ///
    @Time(n) @Space(length(b))
    pub def patch(i: Int32, n: Int32, a: Array[a], b: Array[a]): Array[a] & Impure =
        let len1 = length(a);
        let size = if (n > len1) len1 else n;
        let sub = slice(0, size, a);
        updateSequence(i, sub, b)

    ///
    /// Update the mutable array `b` replacing `n` elements starting at index `i` with the corresponding elements of array `a`.
    ///
    /// If any of the indices `i, i+1, i+2, ... , i+n-1` are out of range in `b` then no patching is done at these indices.
    /// If `a` becomes depleted then no further patching is done.
    /// If patching occurs at index `i+j` in `b`, then the element at index `j` in `a` is used.
    ///
    @Time(n) @Space(1)
    pub def patch!(i: Int32, n: Int32, a: Array[a], b: mut Array[a]): Unit & Impure =
        let len1 = length(a);
        let size = if (n > len1) len1 else n;
        let sub = slice(0, size, a);
        updateSequence!(i, sub, b)

    ///
    /// Returns `a` with `x` inserted between every two adjacent elements.
    ///
    @Time(length(a)) @Space(length(a))
    pub def intersperse(x: a, a: Array[a]): Array[a] & Impure =
        let len1 = length(a);
        let len2 = len1 + len1 - 1;
        if (len2 <= 0)
            []
        else {
            let b = new(x, len2);
            let f = { (v,i) -> let j = i + i; b[j] = v };
            foreachWithIndex(f, a);
            b
        }

    ///
    /// Returns the concatenation of the elements in `arrs` with the elements of `sep` inserted between every two adjacent elements.
    ///
    @Time(length(arrs)) @Space(length(arrs))
    pub def intercalate(sep: Array[a], arrs: Array[Array[a]]): Array[a] & Impure =
        let count = length(arrs);
        let sepLength = length(sep);
        let sepCount = if (count < 2) 0 else count - 1;
        let len = sumLengths(arrs) + (sepCount * sepLength);
        match headArrays(arrs){
            case None => []
            case Some(x) =>
                let out = new(x, len);
                let overwrite = { (a,st) ->
                    let (pos,i) = st;
                    if (i == 0) {
                        let pos1 = arrayWrites(0, a, out);
                        (pos1, 1)
                    } else {
                        let pos1 = arrayWrites(pos, sep, out);
                        let pos2 = arrayWrites(pos1, a, out);
                        (pos2, i+1)
                    }};
                foreachAccum(overwrite, (0,0), arrs);
                out
        }

    ///
    /// Imperatively write `sub` at position `pos` in array `out`
    ///
    /// Return the new write position which is `pos + length(sub)`.
    ///
    /// Helper function for `intercalate` which needs to do sucessive writing.
    ///
    def arrayWrites(pos: Int32, sub: Array[a], out: Array[a]): Int32 & Impure =
        updateSequence!(pos, sub, out);
        pos + length(sub)

    ///
    /// Sum the lengths of an array of arrays.
    ///
    /// Helper function for `intercalate` and `flatten`.
    ///
    def sumLengths(arrs: Array[Array[a]]): Int32 & Impure =
        foldLeft((ac,a) -> ac + length(a), 0, arrs)

    ///
    /// Find the first non-empty array of an array of arrays (left-to-right).
    ///
    /// Helper function for `intercalate` and `flatten`.
    ///
    def headArrays(arrs: Array[Array[a]]): Option[a] & Impure =
        headArraysHelper(arrs, length(arrs), 0)

    ///
    /// Helper function for `headArrays`.
    ///
    def headArraysHelper(arrs: Array[Array[a]], len: Int32, i: Int32): Option[a] & Impure =
        if (i >= len)
            None
        else
            match head(arrs[i]) {
                case Some(x) => Some(x)
                case None => headArraysHelper(arrs, len, i+1)
            }

    ///
    /// Returns the transpose of `a`.
    ///
    /// Returns `a` if the dimensions of the elements of `a` are mismatched.
    ///
    @Time(length(a) * length(a)) @Space(length(a) * length(a))
    pub def transpose(a: Array[Array[a]]): Array[Array[a]] & Impure =
        let ilen = length(a);
        if (ilen == 0)
            []
        else {
            let jlen = length(a[0]);
            if (jlen == 0 or uniformHelper(a, jlen))
                a
            else
                init(i -> init(j -> a[j][i], ilen), jlen)
        }

    ///
    /// Helper function for `transpose`.
    ///
    def uniformHelper(a: Array[Array[a]], l: Int32): Bool & Impure =
        exists(x -> length(x) != l, a)

    ///
    /// Returns `true` if and only if `a1` is a prefix of `a2`.
    ///
    @Time(length(a1)) @Space(1)
    pub def isPrefixOf(a1: Array[a], a2: Array[a]): Bool & Impure with Eq[a] =
        let len1 = length(a1);
        if (len1 > length(a2))
            false
        else
            isPrefixOfHelper(a1, a2, len1, 0)

    ///
    /// Helper function for `isPrefixOf`.
    ///
    def isPrefixOfHelper(a1: Array[a], a2: Array[a], len: Int32, i: Int32): Bool & Impure with Eq[a] =
        if (i >= len)
            true
        else
            if (a1[i] != a2[i])
                false
            else
                isPrefixOfHelper(a1, a2, len, i+1)

    ///
    /// Returns `true` if and only if `a1` is a infix of `a2`.
    ///
    @Time(length(a1)) @Space(1)
    pub def isInfixOf(a1: Array[a], a2: Array[a]): Bool & Impure with Eq[a] =
        let len1 = length(a1);
        let len2 = length(a2);
        if (len1 > len2)
            false
        else
            if (len1 == 0)
                true
            else
                isInfixOfSearch(a1, a2, len1, len2, 0)

    ///
    /// Helper function for `isInfixOf` - scan a2 to find a match with first element of a1.
    ///
    /// Precondition: len1 (length of a1) > 0
    ///
    def isInfixOfSearch(a1: Array[a], a2: Array[a], len1: Int32, len2: Int32, j: Int32): Bool & Impure with Eq[a] =
        if (j >= len2)
            false
        else
            if (a1[0] == a2[j])
                isInfixOfCheck(a1, a2, len1, len2, 1, j+1)
            else
                isInfixOfSearch(a1, a2, len1, len2, j+1)

    ///
    /// Helper function for `isInfixOf` - a1 has started matching, scan to see if it all matches.
    ///
    def isInfixOfCheck(a1: Array[a], a2: Array[a], len1: Int32, len2: Int32, i: Int32, j: Int32): Bool & Impure with Eq[a] =
        if (i >= len1)
            // a1 exhausted, so success
            true
        else
            if (j >= len2)
                // a2 exhausted, a1 still trying to match, so failure
                false
            else
                if(a1[i] == a2[j])
                    isInfixOfCheck(a1, a2, len1, len2, i+1, j+1)
                else
                    isInfixOfSearch(a1, a2, len1, len2, j+1)

    ///
    /// Returns `true` if and only if `a1` is a suffix of `a2`.
    ///
    @Time(length(a1)) @Space(1)
    pub def isSuffixOf(a1: Array[a], a2: Array[a]): Bool & Impure with Eq[a] =
        let len1 = length(a1);
        let len2 = length(a2);
        if (len1 > len2)
            false
        else
            isSuffixOfHelper(a1, a2, len1-1, len2-1)

    ///
    /// Helper function for `isSuffixOf`.
    ///
    /// Precondition: i <= j
    ///
    def isSuffixOfHelper(a1: Array[a], a2: Array[a], i: Int32, j: Int32): Bool & Impure with Eq[a] =
        if (i < 0)
            true
        else
            if (a1[i] != a2[j])
                false
            else
                isSuffixOfHelper(a1, a2, i-1, j-1)

    ///
    /// Returns the result of applying `combine` to all the elements in `a`, using `empty` as the initial value.
    ///
    pub def fold(a: Array[a]): a & Impure with Monoid[a] = foldLeft(Monoid.combine, Monoid.empty(), a)

    ///
    /// Applies `f` to a start value `s` and all elements in `a` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, a[0]), a[1])..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b & ef, s: b, a: Array[a]): b & Impure =
        foldLeftHelper(f, s, a, length(a), 0)

    ///
    /// Helper function for `foldLeft`.
    ///
    def foldLeftHelper(f: (b, a) -> b & ef, s: b, a: Array[a], len: Int32, i: Int32): b & Impure =
        if (i >= len)
            s
        else {
            let s1 = f(s, a[i]) as & Impure;
            foldLeftHelper(f, s1, a, len, i + 1)
        }

    ///
    /// Applies `f` to a start value `s` and all elements in `a` going from right to left.
    ///
    /// That is, the result is of the form: `f(a[0], ...f(a[n-1], f(a[n], s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b & ef, s: b, a: Array[a]): b & Impure =
        foldRightHelper(f, s, a, length(a) - 1)

    ///
    /// Helper function for `foldRight`.
    ///
    def foldRightHelper(f: (a, b) -> b & ef, s: b, a: Array[a], i: Int32): b & Impure =
        if (i < 0)
            s
        else {
            let s1 = f(a[i], s) as & Impure;
            foldRightHelper(f, s1, a, i - 1)
        }

    ///
    /// Applies `f` to all elements in `a` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// Returns `None` if `a` is empty.
    ///
    pub def reduceLeft(f: (a, a) ->  a & ef, a: Array[a]): Option[a] & Impure =
        let len = length(a);
        if (len == 0)
            None
        else
            Some(reduceLeftHelper(f, a, a[0], len, 1))

    ///
    /// Helper function for `reduceLeft`.
    ///
    def reduceLeftHelper(f: (a, a) ->  a & ef, a: Array[a], x: a, len: Int32, i:Int32): a & Impure =
        if (i >= len)
            x
        else {
            let s1 = f(x, a[i]) as & Impure;
            reduceLeftHelper(f, a, s1, len, i+1)
        }

    ///
    /// Applies `f` to all elements in `a` going from right to left until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// Returns `None` if `a` is empty.
    ///
    pub def reduceRight(f: (a, a) -> a & ef, a: Array[a]): Option[a] & Impure =
        let len = length(a);
        if (len == 0)
            None
        else {
            let end = len - 1;
            let ac = a[end];
            Some(reduceRightHelper(f, a, ac, end-1))
        }

    ///
    /// Helper function for `reduceRight`.
    ///
    def reduceRightHelper(f: (a, a) -> a & ef, a: Array[a], x: a, i:Int32): a & Impure =
        if (i < 0)
            x
        else {
            let s1 = f(a[i], x) as & Impure;
            reduceRightHelper(f, a, s1, i-1)
        }

    ///
    /// Returns the number of elements in `a` that satisfy the predicate `f`.
    ///
    @Time(time(f) * length(a)) @Space(space(f))
    pub def count(f: a -> Bool, a: Array[a]): Int32 & Impure =
        foldLeft((b,x) -> if (f(x)) b + 1 else b, 0, a) as & Impure

    ///
    /// Returns the concatenation of the arrays of in the array `arrs`.
    ///
    @Time(length(arrs)) @Space(length(arrs))
    pub def flatten(arrs: Array[Array[a]]) : Array[a] & Impure =
        let len = sumLengths(arrs);
        match headArrays(arrs) {
            case None => []
            case Some(x) => {
                let out = new(x, len);
                foreachAccum((a, pos) -> arrayWrites(pos, a, out), 0, arrs);
                out
            }
        }

    ///
    /// Returns `true` if and only if at least one element in `a` satisfies the predicate `f`.
    ///
    /// Returns `false` if `a` is empty.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(a)) @Space(space(f))
    pub def exists(f: a -> Bool, a: Array[a]): Bool & Impure =
        existsHelper(f, a, length(a), 0)

    ///
    /// Helper function for `exists`.
    ///
    def existsHelper(f: a -> Bool, a: Array[a], len: Int32, i: Int32): Bool & Impure =
        if (i >= len)
            false
        else
            if (f(a[i]) == true)
                true
            else
                existsHelper(f, a, len, i+1)

    ///
    /// Returns `true` if and only if all elements in `a` satisfy the predicate `f`.
    ///
    /// Returns `true` if `a` is empty.
    ///
    @Time(time(f) * length(a)) @Space(space(f))
    pub def forall(f: a -> Bool, a: Array[a]): Bool & Impure =
        forallHelper(f, a, length(a), 0)

    ///
    /// Helper function for `forall`.
    ///
    def forallHelper(f: a -> Bool, a: Array[a], len: Int32, i: Int32): Bool & Impure =
        if (i >= len)
            true
        else
            if (f(a[i]) == false)
                false
            else
                forallHelper(f, a, len, i+1)

    ///
    /// Returns an array of every element in `a` that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(a)) @Space(space(f) * length(a))
    pub def filter(f: a -> Bool, a: Array[a]): Array[a] & Impure =
        let len = length(a);
        if (len < 1) {
            []
        } else {
            let out = new(a[0], len);
            let endPos = filterHelper(f, 0, a, 0, out);
            slice(0, endPos, out)
        }

    ///
    /// Helper function for `filter`.
    /// The array `out` is mutated if successes are found as the initial array `a` is iterated.
    /// The returned Int32 is the current "write position" and it tells the calling function where to slice `out`.
    ///
    def filterHelper(f: a -> Bool, aix: Int32, a: Array[a], oix: Int32, out: Array[a]): Int32 & Impure =
        if (aix >= length(a))
            oix
        else {
            let x = a[aix];
            if (f(x)) {
                out[oix] = x;
                filterHelper(f, aix + 1, a, oix + 1, out)
            } else {
                filterHelper(f, aix + 1, a, oix, out)
            }
        }

    ///
    /// Returns a pair of lists `(a1, a2)`.
    ///
    /// `a1` contains all elements of `a` that satisfy the predicate `f`.
    /// `a2` contains all elements of `a` that do not satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(a)) @Space(space(f) * length(a))
    pub def partition(f: a -> Bool, a: Array[a]): (Array[a], Array[a]) & Impure =
        let step = { (x, ac) ->
            let (a1,a2) = ac;
            if (f(x)) (x :: a1, a2) else (a1, x :: a2)
        };
        let (xs,ys) = foldRight(step, (Nil,Nil), a);
        (List.toArray(xs), List.toArray(ys))

    ///
    /// Returns a pair of arrays `(a1, a2)`.
    ///
    /// `a1` is the longest prefix of `a` that satisfies the predicate `f`.
    /// `a2` is the remainder of `a`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(a)) @Space()
    pub def span(f: a -> Bool, a: Array[a]): (Array[a], Array[a]) & Impure =
        match findIndexOfLeft(x -> not (f(x)), a) {
            case None => (takeLeft(length(a), a), [])
            case Some(i) => (takeLeft(i, a), dropLeft(i, a))
        }

    ///
    /// Alias for `dropLeft`.
    ///
    @Time(length(a) - n) @Space(length(a) - n)
    pub def drop(n: Int32, a: Array[a]) : Array[a] & Impure = dropLeft(n, a)

    ///
    /// Returns a copy of array `a`, dropping the first `n` elements.
    ///
    /// Returns `[]` if `n > length(a)`.
    ///
    @Time(length(a) - n) @Space(length(a) - n)
    pub def dropLeft(n: Int32, a: Array[a]) : Array[a] & Impure =
        let len = length(a);
        if (n > len)
            []
        else {
            let start = if (n < 0) 0 else n;
            slice(start, len, a)
        }

    ///
    /// Returns a copy of array `a`, dropping the last `n` elements.
    ///
    /// Returns `[]` if `n > length(a)`.
    ///
    @Time(length(a) - n) @Space(length(a) - n)
    pub def dropRight(n: Int32, a: Array[a]): Array[a] & Impure =
        let len = length(a);
        if (n >= len)
            []
        else {
            let end = if (n < 0) len else len - n;
            slice(0, end, a)
        }

    ///
    /// Alias for `dropWhileLeft`.
    ///
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(a)) @Space(space(f) * length(a))
    pub def dropWhile(f: a -> Bool, a: Array[a]): Array[a] & Impure = dropWhileLeft(f, a)

    ///
    /// Returns copy of array `a` without the longest prefix that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(a)) @Space(space(f) * length(a))
    pub def dropWhileLeft(f: a -> Bool, a: Array[a]): Array[a]  & Impure =
        match findIndexOfLeft(x -> not (f(x)), a) {
            case None => []
            case Some(i) => dropLeft(i, a)
        }

    ///
    /// Returns copy of array `a` without the longest suffix that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(a)) @Space(space(f) * length(a))
    pub def dropWhileRight(f: a -> Bool, a: Array[a]): Array[a] & Impure =
        match findIndexOfRight(x -> not (f(x)), a) {
            case None => []
            case Some(i) => slice(0, i + 1, a)
        }

    ///
    /// Alias for `takeLeft`.
    ///
    @Time(n) @Space(n)
    pub def take(n: Int32, a: Array[a]) : Array[a] & Impure = takeLeft(n, a)

    ///
    /// Returns a fresh array taking first `n` elements of `a`.
    ///
    /// Returns `a` if `n > length(xs)`.
    ///
    @Time(n) @Space(n)
    pub def takeLeft(n: Int32, a: Array[a]) : Array[a] & Impure =
        if (n <= 0)
            []
        else {
            let len = length(a);
            let end = if (n > len) len else n;
            slice(0, end, a)
        }

    ///
    /// Returns a fresh array taking last `n` elements of `a`.
    ///
    /// Returns `a` if `n > length(xs)`.
    ///
    @Time(n) @Space(n)
    pub def takeRight(n: Int32, a: Array[a]) : Array[a] & Impure =
        if (n <= 0)
            []
        else {
            let len = length(a);
            let start = if (n > len) 0 else len - n;
            slice(start, len, a)
        }

    ///
    /// Alias for `takeWhileLeft`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(a)) @Space(space(f) * length(a))
    pub def takeWhile(f: a -> Bool, a: Array[a]): Array[a] & Impure = takeWhileLeft(f, a)

    ///
    /// Returns the longest prefix of `a` that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(a)) @Space(space(f) * length(a))
    pub def takeWhileLeft(f: a -> Bool, a: Array[a]): Array[a] & Impure =
        match findIndexOfLeft(x -> not (f(x)), a) {
            case None => a
            case Some(i) => takeLeft(i, a)
        }

    ///
    /// Returns the longest suffix of `a` that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(a)) @Space(space(f) * length(a))
    pub def takeWhileRight(f: a -> Bool, a: Array[a]): Array[a] & Impure =
        match findIndexOfRight(x -> not (f(x)), a) {
            case None => slice(0, length(a), a)
            case Some(i) => slice(i + 1, length(a), a)
        }

    ///
    /// Partitions `a` into subarrays such that for any two elements `x` and `y` in a subarray, `f(x, y)` is true.
    ///
    /// A subarray is created by iterating through the remaining elements of `a` from left to right and adding an
    /// element to the subarray if and only if doing so creates no conflicts with the elements already in the subarray.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(a) * length(a)) @Space(space(f) * length(a))
    pub def groupBy(f: (a, a) -> Bool, a: Array[a]): Array[Array[a]] & Impure =
        let xs = toList(a);
        groupByHelper(f, xs, Nil) |> List.toArray

    ///
    /// Helper function for `groupBy`.
    ///
    def groupByHelper(f: (a, a) -> Bool, xs: List[a], ac: List[Array[a]]): List[Array[a]] & Impure = match xs {
        case Nil => List.reverse(ac)
        case x :: rs =>
            let (r1, r2) = extractHelper(f, rs, x :: Nil, Nil);
            groupByHelper(f, r2, r1 :: ac)
    }

    ///
    /// Helper function for `groupBy`.
    ///
    def extractHelper(f: (a, a) -> Bool, xs: List[a], ps: List[a], ns: List[a]): (Array[a], List[a]) & Impure = match xs {
        case Nil => {
            let a = List.toArray(ps);
            reverse!(a);
            (a, List.reverse(ns))
        }
        case x :: rs =>
            if (agreeHelper(f, x, ps))
                extractHelper(f, rs, x :: ps, ns)
            else
                extractHelper(f, rs, ps, x :: ns)
    }

    ///
    /// Helper function for `groupBy`.
    ///
    def agreeHelper(f: (a, a) -> Bool, a: a, xs: List[a]): Bool = match xs {
        case Nil => true
        case x :: rs =>
            if ((f(x, a) and f(a, x)) == false)
                false
            else
                agreeHelper(f, a, rs)
    }

    ///
    /// Returns an array where the element at index `i` is `(x, y)` where
    /// `x` is the element at index `i` in `a` and `y` is the element at index `i` in `b`.
    ///
    /// If either `a` or `b` becomes depleted, then no further elements are added to the resulting array.
    ///
    @Time(Int32.min(length(a), length(b))) @Space(Int32.min(length(a), length(b)))
    pub def zip(a: Array[a], b: Array[b]): Array[(a,b)] & Impure =
        let len = Int32.min(length(a), length(b));
        init(i -> (a[i], b[i]), len)

    ///
    /// Returns an array where the element at index `i` is `f(x, y)` where
    /// `x` is the element at index `i` in `a` and `y` is the element at index `i` in `b`.
    ///
    /// If either `a` or `b` becomes depleted, then no further elements are added to the resulting array.
    ///
    @Time(time(f) * Int32.min(length(a), length(b))) @Space(space(f) * Int32.min(length(a), length(b)))
    pub def zipWith(f: (a, b) -> c & ef, a: Array[a], b: Array[b]): Array[c] & Impure =
        let len = Int32.min(length(a), length(b));
        init(i -> f(a[i], b[i]), len)

    ///
    /// Returns a pair of arrays, the first containing all first components in `a`
    /// and the second containing all second components in `a`.
    ///
    @Time(length(a)) @Space(length(a))
    pub def unzip(a: Array[(a,b)]): (Array[a], Array[b]) & Impure =
        init2(i -> a[i], length(a))

    ///
    /// Returns an array where the element at index `i` is `f(a[i], b[i])`.
    ///
    /// If either `a` or `b` becomes depleted, then no further elements are added to the resulting array.
    ///
    @Time(time(f) * Int32.min(length(a), length(b))) @Space(space(f) * Int32.min(length(a), length(b)))
    pub def map2(f: (a, b) -> c & ef, a: Array[a], b: Array[b]): Array[c] & Impure =
        let len = Int32.min(length(a), length(b));
        init(i -> f(a[i], b[i]), len)

    ///
    /// Concatenates the results of applying `f` pairwise to the elements of `a` and `b`,
    /// ignoring additional elements of the longer array.
    ///
    @Time(time(f) * Int32.min(length(a), length(b))) @Space(space(f) * Int32.min(length(a), length(b)))
    pub def flatMap2(f: (a, b) -> Array[c] & ef, a: Array[a], b: Array[b]): Array[c] & Impure =
        let len = Int32.min(length(a), length(b));
        init(i -> f(a[i], b[i]), len) |> flatten

    ///
    /// Alias for `foldLeft2`.
    ///
    @Time(time(f) * Int32.min(length(a), length(b))) @Space(space(f))
    pub def fold2(f: (c, a, b) -> c & ef, c: c, a: Array[a], b: Array[b]): c & Impure =
        foldLeft2(f, c, a, b)

    ///
    /// Accumulates the result of applying `f` pairwise to the elements of `a` and `b`
    /// starting with the initial value `c` and going from left to right.
    ///
    @Time(time(f) * Int32.min(length(a), length(b))) @Space(space(f))
    pub def foldLeft2(f: (c, a, b) ->  c & ef, c: c, a: Array[a], b: Array[b]): c & Impure =
        let lena = length(a);
        let lenb = length(b);
        foldLeft2Helper(f, c, a, b, lena, lenb, 0)

    ///
    /// Helper function for `foldLeft2`.
    ///
    def foldLeft2Helper(f: (c, a, b) ->  c & ef, s: c, a: Array[a], b: Array[b], lena: Int32, lenb: Int32, i: Int32): c & Impure =
        if (i >= lena or i >= lenb)
            s
        else {
            let x = a[i];
            let y = b[i];
            let s1 = f(s, x, y) as & Impure;
            foldLeft2Helper(f, s1, a, b, lena, lenb, i+1)
        }

    ///
    /// Accumulates the result of applying `f` pairwise to the elements of `a` and `b`
    /// starting with the initial value `c` and going from right to left.
    ///
    @Time(time(f) * Int32.min(length(a), length(b))) @Space(space(f))
    pub def foldRight2(f: (a, b, c) -> c & ef, c: c, a: Array[a], b: Array[b]): c & Impure =
        let starta = length(a) - 1;
        let startb = length(b) - 1;
        foldRight2Helper(f, c, a, b, starta, startb)

    ///
    /// Helper function for `foldRight2`.
    ///
    def foldRight2Helper(f: (a, b, c) -> c & ef, s: c, a: Array[a], b: Array[b], ia: Int32, ib: Int32): c & Impure =
        if (ia < 0 or ib < 0)
            s
        else {
            let x = a[ia];
            let y = b[ib];
            let s1 = f(x, y, s) as & Impure;
            foldRight2Helper(f, s1, a, b, ia - 1, ib - 1)
        }

    ///
    /// Collects the results of applying the partial function `f` to every element in `a`.
    ///
    @Time(time(f) * length(a)) @Space(space(f) * length(a))
    pub def filterMap(f: a -> Option[b] & ef, a: Array[a]): Array[b] & Impure =
        foldRight((x,xs) -> match f(x) {
            case None => xs
            case Some(b) => b :: xs }, Nil, a) |> List.toArray

    ///
    /// Returns the first non-None result of applying the partial function `f` to each element of `xs`.
    ///
    /// Returns `None` if every element of `xs` is `None`.
    ///
    @Time(time(f) * length(a)) @Space(space(f))
    pub def findMap(f: a -> Option[b] & ef, a: Array[a]): Option[b] & Impure =
        findMapHelper(f, a, length(a), 0)

    ///
    /// Helper function for `findMap`.
    ///
    def findMapHelper(f: a -> Option[b] & ef, a: Array[a], len: Int32, i: Int32): Option[b] & Impure =
        if (i >= len)
            None
        else {
            let x = f(a[i]);
            match x {
                case Some(v) => Some(v)
                case None => findMapHelper(f, a, len, i+1)
            }
        }

    ///
    /// Returns the array `a` as a set.
    ///
    @Time(length(a)) @Space(length(a))
    pub def toSet(a: Array[a]): Set[a] & Impure with Order[a] = foldRight(Set.insert, Set.empty(), a)

    ///
    /// Returns the association list `xs` as a map.
    ///
    /// If `xs` contains multiple mappings with the same key, `toMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    @Time(length(a)) @Space(length(a))
    pub def toMap(a: Array[(a, b)]): Map[a, b] & Impure with Order[a] =
        foldRight((x, m) -> Map.insert(fst(x), snd(x), m), Map.empty(), a)

    ///
    /// Returns the array `a` as a MutList.
    ///
    pub def toMutList(a: Array[a]): MutList[a] & Impure =
        MutList(ref a, ref length(a))

    ///
    /// Alias for `findIndexOfLeft`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(a)) @Space(space(f))
    pub def findIndexOf(f: a -> Bool, a: Array[a]): Option[Int32] & Impure = findIndexOfLeft(f,a)

    ///
    /// Optionally returns the position of the first element in `x` satisfying `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(a)) @Space(space(f))
    pub def findIndexOfLeft(f: a -> Bool, a: Array[a]): Option[Int32] & Impure =
        if (length(a) < 1)
            None
        else {
            let i = findIndexOfLeftHelper(f, a, 0);
            if (i < 0) None else Some(i)
        }

    ///
    /// Helper function for `findIndexOfLeft`.
    ///
    def findIndexOfLeftHelper(f: a -> Bool, a: Array[a], i: Int32): Int32 & Impure =
        if (i >= length(a))
            -1
        else
            if (f(a[i]))
                i
            else
                findIndexOfLeftHelper(f, a, i + 1)

    ///
    /// Optionally returns the position of the first element in `a` satisfying `f`
    /// searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(a)) @Space(space(f))
    pub def findIndexOfRight(f: a -> Bool, a: Array[a]): Option[Int32] & Impure =
        let len = length(a);
        let i = findIndexOfRightHelper(f, a, len-1);
        if (i < 0) None else Some(i)

    ///
    /// Helper function for `findIndexOfRight`.
    ///
    def findIndexOfRightHelper(f: a -> Bool, a: Array[a], i: Int32): Int32 & Impure =
        if (i < 0)
            -1
        else
            if (f(a[i]))
                i
            else
                findIndexOfRightHelper(f, a, i - 1)

    ///
    /// Returns the positions of the all the elements in `a` satisfying `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(a)) @Space(space(f) * length(a))
    pub def findIndices(f: a -> Bool, a: Array[a]): Array[Int32] & Impure =
        findIndicesHelper(f, a, length(a), 0) |> List.toArray

    ///
    /// Helper function for `findIndices`.
    ///
    def findIndicesHelper(f: a -> Bool, a: Array[a], len: Int32, i: Int32): List[Int32] & Impure =
        if (i >= len)
            Nil
        else {
            let r = findIndicesHelper(f, a, len, i+1);
            if (f(a[i])) i :: r else r
        }

    ///
    /// Build an array of length `len` by applying `f` to the successive indices.
    ///
    @Time(time(f) * len) @Space(space(f) * len)
    pub def init(f: Int32 -> a & ef, len: Int32): Array[a] & Impure =
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
    def initHelper(a: Array[a], f: Int32 -> a & ef, len: Int32, i: Int32): Unit & Impure =
        if (i >= len)
            ()
        else {
            a[i] = f(i);
            initHelper(a, f, len, i+1)
        }

    ///
    /// Build a pair of arrays of length `len` by applying `f` to the successive indices.
    ///
    @Time(time(f) * len) @Space(space(f) * len)
    pub def init2(f: Int32 -> (a,b) & ef, len: Int32): (Array[a], Array[b]) & Impure =
        if (len <= 0)
            ([], [])
        else {
            let (x,y) = f(0);
            let a = new(x, len);
            let b = new(y, len);
            init2Helper(a, b, f, len, 1);
            (a, b)
        }

    ///
    /// Helper function for `init2`.
    ///
    def init2Helper(a: Array[a], b: Array[b], f: Int32 -> (a,b) & ef, len: Int32, i: Int32): Unit & Impure =
        if (i >= len)
            ()
        else {
            let (x,y) = f(i);
            a[i] = x as a & ef;
            b[i] = y as b & ef;
            init2Helper(a, b, f, len, i+1)
        }

    ///
    /// Returns `true` if arrays `a` and `b` have the same elements, i.e. are structurally equal.
    ///
    @Time(length(a)) @Space(1)
    pub def sameElements(a: Array[a], b: Array[a]): Bool & Impure with Eq[a] =
        let alen = length(a);
        let blen = length(b);
        if (alen == blen)
            sameElementsHelper(a, b, alen, 0)
        else
            false

    ///
    /// Helper function for `sameElements`.
    ///
    def sameElementsHelper(a: Array[a], b: Array[a], len: Int32, i: Int32): Bool & Impure with Eq[a] =
        if (i >= len)
            true
        else
            if (a[i] != b[i])
                false
            else
                sameElementsHelper(a, b, len, i+1)

    ///
    /// Apply the effectful function `f` to all the elements in the array `a`.
    ///
    @Time(time(f) * length(a)) @Space(space(f))
    pub def foreach(f: a -> Unit & ef, a: Array[a]): Unit & Impure =
        let len = length(a);
        if (len < 1)
            ()
        else
            foreachHelper(f, a, len, 0)

    ///
    /// Helper function for `foreach`.
    ///
    def foreachHelper(f: a -> Unit & ef, a: Array[a], len: Int32, i: Int32): Unit & Impure =
        if (i >= len)
            ()
        else {
            f(a[i]);
            foreachHelper(f, a, len, i + 1)
        }

    ///
    /// Apply the effectful function `f` to all the elements in the array `a`.
    ///
    @Time(time(f) * length(a)) @Space(space(f))
    pub def foreachWithIndex(f: (a, Int32) -> Unit & ef, a: Array[a]): Unit & Impure =
        let len = length(a);
        if (len < 1)
            ()
        else
            foreachWithIndexHelper(f, a, len, 0)

    ///
    /// Helper function for `foreachWithIndex`.
    ///
    def foreachWithIndexHelper(f: (a, Int32) -> Unit & ef, a: Array[a], len: Int32, i: Int32): Unit & Impure =
        if (i >= len)
            ()
        else {
            f(a[i], i);
            foreachWithIndexHelper(f, a, len, i + 1)
        }

    ///
    /// Apply the effectful function `f` to all the elements in the array `a` and the accumulator `s`.
    /// The function returns the final value of the accumulator `s`.
    ///
    @Time(time(f) * length(a)) @Space(space(f))
    pub def foreachAccum(f: (a, st) -> st & ef, s: st, a: Array[a]): st & Impure =
        foreachAccumHelper(f, a, length(a), s, 0)

    ///
    /// Helper function for `foreachAccum`.
    ///
    def foreachAccumHelper(f: (a, st) -> st & ef, a: Array[a], len: Int32, s: st, i: Int32): st & Impure =
        if (i >= len)
            s
        else {
            let s1 = f(a[i], s);
            foreachAccumHelper(f, a, len, s1, i + 1)
        }

    ///
    /// Returns a copy of `a` with the elements starting at index `i` replaced by `sub`.
    ///
    @Time(length(a)) @Space(length(a))
    pub def updateSequence(i: Int32, sub: Array[a], a: Array[a]): Array[a] & Impure =
        let end = i + length(sub);
        let f = ix -> if (ix >= i and ix < end) sub[ix - i] else a[ix];
        init(f, length(a))

    ///
    /// Update the mutable array `a` with the elements starting at index `i` replaced by `sub`.
    ///
    @Time(length(a)) @Space(1)
    pub def updateSequence!(i: Int32, sub: Array[a], a: mut Array[a]): Unit & Impure =
        let end = i + length(sub);
        let f = { (_,ix) ->
            if (ix >= i and ix < end)
                a[ix] = sub[ix-i]
            else
                ()
        };
        foreachWithIndex(f, a)

    ///
    /// Render the array `a` as a String. Elements are rendered with the
    /// function `f` and seperated with the string `sep`.
    ///
    @Time(time(f) * length(a)) @Space(space(f) * length(a))
    pub def formatWith(f: a -> String, sep: String, a: Array[a]): String & Impure =
        let sb = StringBuilder.new();
        let step = (x,i) ->
            if (i == 0)
                StringBuilder.appendString!(sb, f(x))
            else {
                StringBuilder.appendString!(sb, sep);
                StringBuilder.appendString!(sb, f(x))
            };
        foreachWithIndex(step, a);
        StringBuilder.toString(sb)

    ///
    /// Returns a sorted copy of array `a`, where the elements are ordered from low to high according to
    /// their `Order` instance.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `a`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sort(a: Array[a]): Array[a] & Impure with Order[a] = sortWith(Order.compare, a)

    ///
    /// Returns a sorted copy of array `a`, where the elements are ordered from low to high according to
    /// the `Order` instance for the values obtained by applying `f` to each element.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `a`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortBy(f: a -> b, a: Array[a]): Array[a] & Impure with Order[b] = sortWith(Order.compare `on` f, a)

    ///
    /// Returns a sorted copy of array `a`, where the elements are ordered from low to high according to
    /// the comparison function `cmp`.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `a`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortWith(cmp: (a,a) -> Comparison, a: Array[a]): Array[a] & Impure =
        let len = length(a);
        let b = slice(0, len, a);
        sortWith!(cmp, b);
        b

    ///
    /// Sort array `a` so that elements are ordered from low to high according to their `Order` instance.
    /// The array is mutated in-place.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `a`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sort!(a: mut Array[a]): Unit & Impure with Order[a] = sortWith!(Order.compare, a)

    ///
    /// Sort array `a` so that elements are ordered from low to high according to the `Order` instance for
    /// the values obtained by applying `f` to each element. The array is mutated in-place.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `a`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortBy!(f: a -> b, a: mut Array[a]): Unit & Impure with Order[b] = sortWith!(Order.compare `on` f, a)

    ///
    /// Sort array `a` so that elements are ordered from low to high according to the comparison function `cmp`.
    /// The array is mutated in-place.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `a`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortWith!(cmp: (a,a) -> Comparison, a: mut Array[a]): Unit & Impure = sortWithin!(cmp, 0, length(a) - 1, a)

    ///
    /// Sort array `a` between the indices `lo` and `hi` (both inclusive) so that elements in that range are ordered
    /// from low to high according to the comparison function `cmp`. The array is mutated in-place where elements
    /// outside the specified range are not changed. If `lo >= hi`, this does nothing.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `a`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortWithin!(cmp: (a,a) -> Comparison, lo: Int32, hi: Int32, a: mut Array[a]): Unit & Impure =
        if (lo >= hi)
            ()
        else {
            let p = quicksortPartition(cmp, a, lo, hi);
            sortWithin!(cmp, lo, p - 1, a);
            sortWithin!(cmp, p + 1, hi, a)
        }

    ///
    /// Swap the elements at `i` and `j` (helper for sorting).
    ///
    /// Precondition: `i` and `j` are within bounds.
    ///
    def swap!(a: mut Array[a], i: Int32, j: Int32): Unit & Impure =
        let x = a[i];
        let y = a[j];
        a[i] = y;
        a[j] = x;
        ()

    ///
    /// Partition step of the Quicksort algorithm.
    ///
    def quicksortPartition(cmp: (a,a) -> Comparison, a: Array[a], lo: Int32, hi: Int32): Int32 & Impure =
        let pivot = a[hi];
        let i = forWithAccum(lo, hi, lo, (j,ix) ->
            if (cmp(a[j], pivot) == LessThan) {
                swap!(a, ix, j);
                ix+1
            } else
                ix
            );
        swap!(a, i, hi);
        i

    ///
    /// A for loop with an accumulator (helper for sorting).
    /// This function is not array specific so is not necessarily Impure.
    ///
    /// precondition: lo <= hi
    ///
    def forWithAccum(lo: Int32, hi: Int32, ac: a, f: (Int32, a) -> a & ef): a & ef =
        if (lo == hi)
            f(lo, ac)
        else {
            let ac1 = f(lo, ac);
            forWithAccum(lo+1, hi, ac1, f)
        }

    ///
    /// Returns the sum of all elements in the list `a`.
    ///
    pub def sum(a: Array[Int32]): Int32 & Impure =
        sumAcc(a, 0, 0)

    ///
    /// Helper function for `sum`.
    ///
    def sumAcc(a: Array[Int32], i: Int32, acc: Int32): Int32 & Impure =
        if (i < length(a))
            sumAcc(a, i + 1, a[i] + acc)
        else
            acc

    ///
    /// Copies the range between `b` (inclusive) and `e` (exclusive) of a into a new array.
    ///
    /// A valid range has the following properties:
    ///   - 0 <= `b` <= `length(a)`.
    ///   - `e` >= `b`.
    /// If the range is valid and greater than the length of `a`, the resulting array with have the length of the range
    /// and include the specified range of `a`.
    ///
    pub def copyOfRange(b: Int32, e: Int32, a: Array[a]): Array[a] & Impure =
        match (reifyType a) {
            case ErasedType =>
                let arr = a as Array[##java.lang.Object];
                import static java.util.Arrays.copyOfRange(Array[##java.lang.Object], Int32, Int32) as copy;
                copy(arr, b, e) as Array[a]
            case ReifiedBool =>
                let arr = a as Array[Bool];
                import static java.util.Arrays.copyOfRange(Array[Bool], Int32, Int32) as copy;
                copy(arr, b, e) as Array[a]
            case ReifiedChar =>
                let arr = a as Array[Char];
                import static java.util.Arrays.copyOfRange(Array[Char], Int32, Int32) as copy;
                copy(arr, b, e) as Array[a]
            case ReifiedFloat32 =>
                let arr = a as Array[Float32];
                import static java.util.Arrays.copyOfRange(Array[Float32], Int32, Int32) as copy;
                copy(arr, b, e) as Array[a]
            case ReifiedFloat64 =>
                let arr = a as Array[Float64];
                import static java.util.Arrays.copyOfRange(Array[Float64], Int32, Int32) as copy;
                copy(arr, b, e) as Array[a]
            case ReifiedInt8 =>
                let arr = a as Array[Int8];
                import static java.util.Arrays.copyOfRange(Array[Int8], Int32, Int32) as copy;
                copy(arr, b, e) as Array[a]
            case ReifiedInt16 =>
                let arr = a as Array[Int16];
                import static java.util.Arrays.copyOfRange(Array[Int16], Int32, Int32) as copy;
                copy(arr, b, e) as Array[a]
            case ReifiedInt32 =>
                let arr = a as Array[Int32];
                import static java.util.Arrays.copyOfRange(Array[Int32], Int32, Int32) as copy;
                copy(arr, b, e) as Array[a]
            case ReifiedInt64 =>
                let arr = a as Array[Int64];
                import static java.util.Arrays.copyOfRange(Array[Int64], Int32, Int32) as copy;
                copy(arr, b, e) as Array[a]
            case ReifiedArray(_) =>
                let arr = a as Array[##java.lang.Object];
                import static java.util.Arrays.copyOfRange(Array[##java.lang.Object], Int32, Int32) as copy;
                copy(arr, b, e) as Array[a]
        }

   ///
   /// Returns an iterator over `a`.
   ///
   pub def toIterator(a: Array[a]): Iterator[a] & Impure = {
       let len = Array.length(a);
       let copy = a[0 .. len];
       let i = ref 0;
       let done = () -> (deref i) >= len;
       let next = () -> {
           let j = deref i;
           let x = copy[j];
           i := j + 1;
           x
       };
       Iterator(done, next)
   }
}
