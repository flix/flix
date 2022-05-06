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

instance Scoped[ScopedArray[a]] {
    pub def regionOf(_: ScopedArray[a, r]): Region[r] = () as Region[r]
}

namespace Array {

    ///
    /// Compares `a` and `b` lexicographically.
    ///
    pub def compare(a: Array[v], b: Array[v]): Comparison & Impure with Order[v] =
        let len = Int32.min(Array.length(a), Array.length(b));
        def loop(i) = {
            if (i < len) {
                let cmp = a[i] <=> b[i];
                if (cmp == EqualTo)
                    loop(i + 1)
                else
                    cmp
            } else if (i < Array.length(a)) {
                GreaterThan
            } else if (i < Array.length(b)) {
                LessThan
            } else {
                EqualTo
            }
        };
        loop(0)

    ///
    /// Returns a string representation of the given array `a`.
    ///
    pub def toString(a: Array[a]): String with ToString[a] =
        def loop(i, acc) =
            if (i < a.length) {
                if (i == 0) loop(i + 1, "${a[i]}") as & Pure
                else loop(i + 1, "${acc}, ${a[i]}") as & Pure
            } else {
                "[${acc}]"
            };
        loop(0, "")

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
    pub def get(i: Int32, a: ScopedArray[a, r]): a \ Read(r) = a[i]

    ///
    /// Stores the value `x` at position `i` in the array `a`.
    ///
    /// Equivalent to the expression `a[i] = x`.
    ///
    @Time(1) @Space(1)
    pub def put(x: a, i: Int32, a: ScopedArray[a, r]): ScopedArray[a, r] \ Write(r) =
        a[i] = x; a

    ///
    /// Returns `true` if the given array `a` is empty.
    ///
    @Time(1) @Space(1)
    pub def isEmpty(a: Array[a]): Bool = a.length == 0

    ///
    /// Returns the length of the array `a`.
    ///
    @Time(1) @Space(1)
    pub def length(a: ScopedArray[a, r]): Int32 = a.length

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
    pub def toList(a: ScopedArray[a, r]): List[a] \ Read(r) =
        def loop(i, acc) = {
            if (i <= 0) {
                acc
            } else {
                let x = a[i - 1];
                loop(i - 1, x :: acc)
            }
        };
        loop(a.length, Nil)

    ///
    /// Returns the array `a` as a `DelayList`.
    ///
    pub def toDelayList(a: ScopedArray[a, r1]): DelayList[a] \ Read(r1) = region r2 {
        let cp = Array.copyOfRange(0, a.length, a, r2);
        def loop(i, k) = {
            if (i >= cp.length) {
                k(ENil)
            } else {
                let x = cp[i];
                loop(i + 1, ks -> ECons(x, ks))
            }
        };
        loop(0, identity)
    }

    ///
    /// Returns the array `arr` as a chain.
    ///
    pub def toChain(arr: ScopedArray[a, r]): Chain[a] \ Read(r) =
        def loop(i, acc) = {
            if (i == 0) {
                acc
            } else {
                let x = arr[i - 1];
                loop(i - 1, Chain.cons(x, acc))
            }
        };
        loop(arr.length, Chain.empty())

    ///
    /// Returns `Some(x)` if `x` is the first element of `a`.
    ///
    /// Returns `None` if `a` is empty.
    ///
    @Time(1) @Space(1)
    pub def head(a: ScopedArray[a, r]): Option[a] \ Read(r) =
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
    pub def memberOf(x: a, a: ScopedArray[a, r]): Bool \ Read(r) with Eq[a] =
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
    /// Optionally returns the position of the first occurrence of `a` in `arr`
    /// searching from left to right.
    ///
    @Time(length(arr)) @Space(1)
    pub def indexOfLeft(a: a, arr: Array[a]): Option[Int32] & Impure with Eq[a] =
        def loop(i) = {
            if (i >= arr.length) {
                -1
            } else {
                if (arr[i] == a)
                    i
                else
                    loop(i + 1)
            }
        };
        let i = loop(0);
        if (i < 0) None else Some(i)

    ///
    /// Optionally returns the position of the first occurrence of `a` in `arr`
    /// searching from right to left.
    ///
    @Time(length(arr)) @Space(1)
    pub def indexOfRight(a: a, arr: Array[a]): Option[Int32] & Impure with Eq[a] =
        def loop(i) = {
            if (i < 0)
                -1
            else
                if (arr[i] == a)
                    i
                else
                    loop(i - 1)
        };
        let i = loop(length(arr) - 1);
        if (i < 0) None else Some(i)

    ///
    /// Return the positions of the all the occurrences of `a` in `arr`.
    ///
    @Time(length(arr)) @Space(length(arr))
    pub def indices(a: a, arr: Array[a]): Array[Int32] & Impure with Eq[a] =
        findIndices(b -> a == b, arr)

    ///
    /// Alias for `findLeft`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(arr)) @Space(space(f))
    pub def find(f: a -> Bool, arr: Array[a]): Option[a] & Impure = findLeft(f, arr)

    ///
    /// Optionally returns the first element of `a` that satisfies the predicate `f` when searching from left to right.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(arr)) @Space(space(f))
    pub def findLeft(f: a -> Bool, arr: Array[a]): Option[a] & Impure =
        match findIndexOfLeft(x -> f(x), arr) {
            case None => None
            case Some(i) => Some(arr[i])
        }

    ///
    /// Optionally returns the first element of `xs` that satisfies the predicate `f` when searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(arr)) @Space(space(f))
    pub def findRight(f: a -> Bool, arr: Array[a]): Option[a] & Impure =
        match findIndexOfRight(x -> f(x), arr) {
            case None => None
            case Some(i) => Some(arr[i])
        }

    ///
    /// Returns an array of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns `[]` if `b >= e`.
    ///
    @Time(e - b) @Space(e - b)
    pub def range(b: Int32, e: Int32, r: Region[r]): ScopedArray[Int32, r] \ Write(r) =
        if (b >= e)
            [] @ r
        else {
            let f = x -> x + b;
            init(f, e - b, r)
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
    pub def scan(f: (b, a) -> b & ef, s: b, arr: Array[a]): Array[b] & Impure = scanLeft(f, s, arr)

    ///
    /// Accumulates the result of applying `f` to `arr` going left to right.
    ///
    /// That is, the result is of the form: `[s , f(s, x1), f(f(s, x1), x2),  ...]`.
    ///
    pub def scanLeft(f: (b, a) -> b & ef, s: b, arr: Array[a]): Array[b] & Impure =
        let len = length(arr) + 1;
        let b = new(s, len);
        def loop(i, acc) = {
            if (i >= len)
                ()
            else {
                let s1 = f(acc, arr[i - 1]);
                b[i] = s1;
                loop(i + 1, s1)
            }
        };
        loop(1, s);
        b

    ///
    /// Accumulates the result of applying `f` to `xs` going right to left.
    ///
    /// That is, the result is of the form: `[..., f(xn-1, f(xn, s)), f(xn, s), s]`.
    ///
    pub def scanRight(f: (a, b) -> b & ef, s: b, a: Array[a]): Array[b] & Impure =
        let len = length(a);
        let b = new(s, len + 1);
        def loop(i, acc) = {
            if (i < 0)
                ()
            else {
                let s1 = f(a[i], acc);
                b[i] = s1;
                loop(i - 1, s1)
            }
        };
        loop(len - 1, s);
        b

    ///
    /// Returns the result of applying `f` to every element in `a`.
    ///
    /// The result is a new array.
    ///
    @Time(time(f) * length(a)) @Space(space(f) * length(a))
    pub def map(f: a -> b & ef, a: Array[a]): Array[b] & Impure =
        let len = length(a);
        init(i -> f(a[i]), len, static)

    ///
    /// Apply `f` to every element in array `arr`. Array `arr` is mutated.
    ///
    @Time(time(f) * length(arr)) @Space(space(f) * length(arr))
    pub def transform!(f: a -> a, arr: Array[a]): Unit & Impure =
        let len = length(arr);
        def loop(i) = {
            if (i >= len)
                ()
            else {
                arr[i] = f(arr[i]);
                loop(i + 1)
            }
        };
        loop(0)

    ///
    /// Returns the result of applying `f` to every element in `a` along with that element's index.
    ///
    /// That is, the result is of the form: `[ f(a[0], 0), f(a[1], 1), ... ]`.
    ///
    @Time(time(f) * length(a)) @Space(space(f) * length(a))
    pub def mapWithIndex(f: (a, Int32) -> b & ef, a: Array[a]): Array[b] & Impure =
        let len = length(a);
        init(i -> f(a[i], i), len, static)

    ///
    /// Apply `f` to every element in array `a` along with that element's index. Array `a` is mutated.
    ///
    @Time(time(f) * length(arr)) @Space(space(f))
    pub def transformWithIndex!(f: (a, Int32) -> a, arr: Array[a]): Unit & Impure =
        let len = length(arr);
        def loop(i) = {
            if (i >= len)
                ()
            else {
                arr[i] = f(arr[i], i);
                loop(i + 1)
            }
        };
        loop(0)

    ///
    /// Returns the result of applying `f` to every element in `a` and concatenating the results.
    ///
    @Time(time(f) * length(a)) @Space(space(f) * length(a))
    pub def flatMap(f: a -> Array[b] & ef, a: Array[a]): Array[b] & Impure =
        let len = length(a);
        init(i -> f(a[i]), len, static) |> flatten

    ///
    /// Returns the reverse of `a`.
    ///
    @Time(length(a)) @Space(length(a))
    pub def reverse(a: Array[a]): Array[a] & Impure =
        let len = length(a);
        let end = len - 1;
        init(i -> a[end - i], len, static)

    ///
    /// Reverse the array `arr`, mutating it in place.
    ///
    @Time(length(arr)) @Space(1)
    pub def reverse!(arr: ScopedArray[a, r]): Unit \ { Read(r), Write(r) } =
        let len = length(arr);
        let halflen = len / 2;
        def loop(i, j) = {
            if (i >= halflen)
                ()
            else {
                let x = arr[i];
                let y = arr[j];
                put(y, i, arr);
                put(x, j, arr);
                loop(i + 1, j - 1)
            }
        };
        loop(0, len - 1)

    ///
    /// Rotate the contents of array `arr` by `n` steps to the left.
    ///
    @Time(n) @Space(length(arr))
    pub def rotateLeft(n: Int32, arr: Array[a]): Array[a] & Impure =
        let len = length(arr);
        if (len < 1)
            []
        else
            if (n < 0)
                rotateRightHelper(Int32.abs(n), arr)
            else
                rotateLeftHelper(n, arr)

    ///
    /// Helper function for `rotateLeft` and `rotateRight`.
    ///
    /// Precondition: `n` must be positive.
    ///
    /// This is an explicit helper to avoid code duplication.
    ///
    def rotateLeftHelper(n: Int32, arr: Array[a]): Array[a] & Impure =
        let len = length(arr);
        let f = i -> { let i1 = n + i; arr[i1 rem len] };
        init(f, len, static)

    ///
    /// Rotate the contents of array `arr` by `n` steps to the right.
    ///
    @Time(n) @Space(length(arr))
    pub def rotateRight(n: Int32, arr: Array[a]): Array[a] & Impure =
        if (length(arr) < 1)
            []
        else
            if (n < 0)
                rotateLeftHelper(Int32.abs(n), arr)
            else
                rotateRightHelper(n, arr)

    ///
    /// Helper function for `rotateRight` and `rotateLeft`.
    ///
    /// Precondition: `n` must be positive.
    ///
    /// This is an explicit helper to avoid code duplication.
    ///
    def rotateRightHelper(n: Int32, a: Array[a]): Array[a] & Impure =
        let len = length(a);
        let n1 = n rem len;
        let start = len - n1;
        let f = i -> { let i1 = start + i; a[i1 rem len]};
        init(f, len, static)

    ///
    /// Returns a copy of `a` with the element at index `i` replaced by `x`.
    ///
    /// Returns a shallow copy of `a` if `i < 0` or `i > length(xs)-1`.
    ///
    @Time(length(a)) @Space(length(a))
    pub def update(i: Int32, x: a, a: Array[a]): Array[a] & Impure =
        let f = ix -> if (ix == i) x else a[ix];
        init(f, length(a), static)

    ///
    /// Returns a copy of `a` with every occurrence of `from` replaced by `to`.
    ///
    @Time(length(a)) @Space(length(a))
    pub def replace(from: {from :: a}, to: {to :: a}, a: Array[a]): Array[a] & Impure with Eq[a] =
        map(e -> if (e == from.from) to.to else e, a)

    ///
    /// Replace every occurrence of `from` by `to` in the array `a`, mutating it in place.
    ///
    @Time(length(a)) @Space(1)
    pub def replace!(from: {from :: a}, to: {to :: a}, a: Array[a]): Unit & Impure with Eq[a] =
        transform!(e -> if (e == from.from) to.to else e, a)

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
    pub def patch!(i: Int32, n: Int32, a: Array[a], b: Array[a]): Unit & Impure =
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
    def arrayWrites(pos: Int32, sub: ScopedArray[a, r1], out: ScopedArray[a, r2]): Int32 \ { Read(r1), Write(r2) } =
        updateSequence!(pos, sub, out);
        pos + length(sub)

    ///
    /// Sum the lengths of an array of arrays.
    ///
    /// Helper function for `intercalate` and `flatten`.
    ///
    def sumLengths(arrs: ScopedArray[ScopedArray[a, r1], r2]): Int32 \ Read(r1, r2) =
        foldLeft((acc, a) -> acc + length(a), 0, arrs)

    ///
    /// Find the first non-empty array of an array of arrays (left-to-right).
    ///
    /// Helper function for `intercalate` and `flatten`.
    ///
    def headArrays(arrs: ScopedArray[ScopedArray[a, r1], r2]): Option[a] \ Read(r1, r2) =
        let len = length(arrs);
        def loop(i) = {
            if (i >= len)
                None
            else
                match head(arrs[i]) {
                    case Some(x) => Some(x)
                    case None    => loop(i + 1)
                }
        };
        loop(0)

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
                init(i -> init(j -> a[j][i], ilen, static), jlen, static)
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
            def loop(i) = {
                if (i >= len1)
                    true
                else
                    if (a1[i] != a2[i])
                        false
                    else
                        loop(i + 1)
            };
            loop(0)

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
            def loop(i, j) = {
                if (i < 0)
                    true
                else
                    if (a1[i] != a2[j])
                        false
                    else
                        loop(i - 1, j - 1)
            };
            loop(len1 - 1, len2 - 1)

    ///
    /// Returns the result of applying `combine` to all the elements in `a`, using `empty` as the initial value.
    ///
    pub def fold(arr: Array[a]): a & Impure with Monoid[a] = foldLeft(Monoid.combine, Monoid.empty(), arr)

    ///
    /// Applies `f` to a start value `s` and all elements in `a` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, a[0]), a[1])..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b & ef, s: b, arr: Array[a]): b & Impure =
        let len = length(arr);
        def loop(i, acc) = {
            if (i >= len)
                acc
            else
                loop(i + 1, f(acc, arr[i]))
        };
        loop(0, s)

    ///
    /// Applies `f` to a start value `s` and all elements in `a` going from right to left.
    ///
    /// That is, the result is of the form: `f(a[0], ...f(a[n-1], f(a[n], s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b & ef, s: b, arr: ScopedArray[a, r]): b \ { ef, Read(r) } =
        def loop(i, acc) = {
            if (i < 0)
                acc
            else
                loop(i - 1, f(arr[i], acc))
        };
        loop(length(arr) - 1, s)

    ///
    /// Applies `f` to a start value `z` and all elements in `a` going from right to left.
    ///
    /// That is, the result is of the form: `f(a[0], ...f(a[n-1], f(a[n], z))...)`.
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (a, Unit -> b & Impure) -> b & ef, z: b, arr: Array[a]): b & Impure =
        def loop(i) = {
            if (i == length(arr))
                z
            else
                f(arr[i], _ -> loop(i + 1))
        };
        loop(0)

    ///
    /// Applies `f` to all elements in `a` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// Returns `None` if `a` is empty.
    ///
    pub def reduceLeft(f: (a, a) ->  a & ef, a: Array[a]): Option[a] & Impure =
        let len = length(a);
        def loop(i, acc) = {
            if (i >= len)
                acc
            else
                loop(i + 1, f(acc, a[i]))
        };
        if (len == 0)
            None
        else
            Some(loop(1, a[0]))

    ///
    /// Applies `f` to all elements in `arr` going from right to left until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// Returns `None` if `arr` is empty.
    ///
    pub def reduceRight(f: (a, a) -> a & ef, arr: Array[a]): Option[a] & Impure =
        let len = length(arr);
        def loop(i, acc) = {
            if (i < 0)
                acc
            else
                loop(i - 1, f(arr[i], acc))
        };
        if (len == 0)
            None
        else
            Some(loop(len - 2, arr[len - 1]))

    ///
    /// Returns the number of elements in `a` that satisfy the predicate `f`.
    ///
    @Time(time(f) * length(a)) @Space(space(f))
    pub def count(f: a -> Bool, a: Array[a]): Int32 & Impure =
        foldLeft((b,x) -> if (f(x)) b + 1 else b, 0, a) as & Impure

    ///
    /// Returns the sum of all elements in the array `a`.
    ///
    pub def sum(a: Array[Int32]): Int32 & Impure =
        foldLeft((acc, x) -> acc + x, 0, a)

    ///
    /// Returns the sum of all elements in the array `a` according to the function `f`.
    ///
    pub def sumWith(f: a -> Int32 & ef, a: Array[a]): Int32 & Impure =
        foldLeft((acc, x) -> acc + f(x), 0, a)

    ///
    /// Returns the product of all elements in the array `a`.
    ///
    pub def product(a: Array[Int32]): Int32 & Impure =
        if (isEmpty(a))
            0
        else
            foldLeft((acc, x) -> acc * x, 1, a)

    ///
    /// Returns the product of all elements in the array `a` according to the function `f`.
    ///
    pub def productWith(f: a -> Int32 & ef, a: Array[a]): Int32 & Impure =
        if (isEmpty(a))
            0
        else
            foldLeft((acc, x) -> acc * f(x), 1, a)

    ///
    /// Returns the concatenation of the arrays of in the array `arrs`.
    ///
    @Time(length(arrs)) @Space(length(arrs))
    pub def flatten(arrs: ScopedArray[ScopedArray[a, r1], r2]): ScopedArray[a, r2] \ { Read(r1), Write(r2) } =
        let len = sumLengths(arrs);
        match headArrays(arrs) {
            case None    => [] @ Scoped.regionOf(arrs)
            case Some(x) => {
                let out = [x; len] @ Scoped.regionOf(arrs);
                foreachAccum((a, pos) -> arrayWrites(pos, a, out), 0, arrs);
                out
            }
        }

    ///
    /// Returns `true` if and only if at least one element in `arr` satisfies the predicate `f`.
    ///
    /// Returns `false` if `arr` is empty.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(arr)) @Space(space(f))
    pub def exists(f: a -> Bool, arr: ScopedArray[a, r]): Bool \ Read(r) =
        let len = length(arr);
        def loop(i) = {
            if (i >= len)
                false
            else if (f(arr[i]))
                true
            else
                loop(i + 1)
        };
        loop(0)

    ///
    /// Returns `true` if and only if all elements in `arr` satisfy the predicate `f`.
    ///
    /// Returns `true` if `arr` is empty.
    ///
    @Time(time(f) * length(arr)) @Space(space(f))
    pub def forall(f: a -> Bool, arr: Array[a]): Bool & Impure =
        let len = length(arr);
        def loop(i) = {
            if (i >= len)
                true
            else
                if (f(arr[i]) == false)
                    false
                else
                    loop(i + 1)
        };
        loop(0)

    ///
    /// Returns an array of every element in `arr` that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(arr)) @Space(space(f) * length(arr))
    pub def filter(f: a -> Bool, arr: Array[a]): Array[a] & Impure =
        let len = length(arr);
        if (len < 1) {
            []
        } else {
            let out = new(arr[0], len);
            def loop(i, j) = {
                if (i >= len)
                    j
                else {
                    let x = arr[i];
                    if (f(x)) {
                        out[j] = x;
                        loop(i + 1, j + 1)
                    } else {
                        loop(i + 1, j)
                    }
                }
            };
            let endPos = loop(0, 0);
            slice(0, endPos, out)
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
    pub def partition(f: a -> Bool, a: ScopedArray[a, r]): (ScopedArray[a, r], ScopedArray[a, r]) \ { Read(r), Write(r) } =
        let step = { (x, acc) ->
            let (a1, a2) = acc;
            if (f(x)) (x :: a1, a2) else (a1, x :: a2)
        };
        let (xs, ys) = foldRight(step, (Nil, Nil), a);
        let r = Scoped.regionOf(a);
        (List.toArray(xs, r), List.toArray(ys, r))

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
    pub def groupBy(f: (a, a) -> Bool, a: ScopedArray[a, r]): ScopedArray[ScopedArray[a, r], r] \ Write(r) =
        let xs = toList(a);
        let r = Scoped.regionOf(a);
        List.toArray(groupByHelper(f, xs, Nil, r), r)

    ///
    /// Helper function for `groupBy`.
    ///
    def groupByHelper(f: (a, a) -> Bool, xs: List[a], ac: List[ScopedArray[a, r]], r: Region[r]): List[ScopedArray[a, r]] \ Write(r) = match xs {
        case Nil => List.reverse(ac)
        case x :: rs =>
            let (r1, r2) = extractHelper(f, rs, x :: Nil, Nil, r);
            groupByHelper(f, r2, r1 :: ac, r)
    }

    ///
    /// Helper function for `groupBy`.
    ///
    def extractHelper(f: (a, a) -> Bool, xs: List[a], ps: List[a], ns: List[a], r: Region[r]): (ScopedArray[a, r], List[a]) \ Write(r) = match xs {
        case Nil => {
            let a = List.toArray(ps, r);
            reverse!(a);
            (a, List.reverse(ns))
        }
        case x :: rs =>
            if (agreeHelper(f, x, ps))
                extractHelper(f, rs, x :: ps, ns, r)
            else
                extractHelper(f, rs, ps, x :: ns, r)
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
        init(i -> (a[i], b[i]), len, static)

    ///
    /// Returns an array where the element at index `i` is `f(x, y)` where
    /// `x` is the element at index `i` in `a` and `y` is the element at index `i` in `b`.
    ///
    /// If either `a` or `b` becomes depleted, then no further elements are added to the resulting array.
    ///
    @Time(time(f) * Int32.min(length(a), length(b))) @Space(space(f) * Int32.min(length(a), length(b)))
    pub def zipWith(f: (a, b) -> c & ef, a: Array[a], b: Array[b]): Array[c] & Impure =
        let len = Int32.min(length(a), length(b));
        init(i -> f(a[i], b[i]), len, static)

    ///
    /// Returns a pair of arrays, the first containing all first components in `a`
    /// and the second containing all second components in `a`.
    ///
    @Time(length(a)) @Space(length(a))
    pub def unzip(a: Array[(a,b)]): (Array[a], Array[b]) & Impure =
        init2(i -> a[i], length(a))

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
        def loop(i, acc) = {
            if (i >= lena or i >= lenb)
                acc
            else
                loop(i + 1, f(acc, a[i], b[i]))
        };
        loop(0, c)

    ///
    /// Accumulates the result of applying `f` pairwise to the elements of `a` and `b`
    /// starting with the initial value `c` and going from right to left.
    ///
    @Time(time(f) * Int32.min(length(a), length(b))) @Space(space(f))
    pub def foldRight2(f: (a, b, c) -> c & ef, c: c, a: Array[a], b: Array[b]): c & Impure =
        def loop(i, j, acc) = {
            if (i < 0 or j < 0)
                acc
            else
                loop(i - 1, j - 1, f(a[i], b[j], acc))
        };
        let starta = length(a) - 1;
        let startb = length(b) - 1;
        loop(starta, startb, c)

    ///
    /// Collects the results of applying the partial function `f` to every element in `a`.
    ///
    @Time(time(f) * length(a)) @Space(space(f) * length(a))
    pub def filterMap(f: a -> Option[b] & ef, a: ScopedArray[a, r]): ScopedArray[b, r] \ { ef, Read(r), Write(r) } =
        let g = (x, xs) -> match f(x) {
            case None => xs
            case Some(b) => b :: xs
        };
        let l = foldRight(g, Nil, a);
        List.toArray(l, Scoped.regionOf(a))

    ///
    /// Returns the first non-None result of applying the partial function `f` to each element of `xs`.
    ///
    /// Returns `None` if every element of `xs` is `None`.
    ///
    @Time(time(f) * length(a)) @Space(space(f))
    pub def findMap(f: a -> Option[b] & ef, a: Array[a]): Option[b] & Impure =
        let len = length(a);
        def loop(i) = {
            if (i >= len)
                None
            else
                let x = f(a[i]);
                match x {
                    case Some(v) => Some(v)
                    case None    => loop(i + 1)
                }
        };
        loop(0)

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
    pub def toMutList(a: ScopedArray[a, r]): MutList[a, r] \ { Read(r), Write(r) } =
        let minCap = MutList.minCapacity();
        let len = length(a);
        let c = Order.max(len, minCap);
        let r = Scoped.regionOf(a);
        MutList(ref copyOfRange(0, c, a, r) @ r, ref len @ r)

    ///
    /// Alias for `findIndexOfLeft`.
    ///
    @Time(time(f) * length(a)) @Space(space(f))
    pub def findIndexOf(f: a -> Bool & ef, a: Array[a]): Option[Int32] & Impure = findIndexOfLeft(f,a)

    ///
    /// Optionally returns the position of the first element in `x` satisfying `f`.
    ///
    @Time(time(f) * length(a)) @Space(space(f))
    pub def findIndexOfLeft(f: a -> Bool & ef, a: Array[a]): Option[Int32] & Impure =
        let len = length(a);
        if (len < 1)
            None
        else {
            def loop(i) = {
                if (i >= len)
                    -1
                else
                    if (f(a[i]))
                        i
                    else
                        loop(i + 1)
            };
            let i = loop(0);
            if (i < 0) None else Some(i)
        }

    ///
    /// Optionally returns the position of the first element in `a` satisfying `f`
    /// searching from right to left.
    ///
    @Time(time(f) * length(a)) @Space(space(f))
    pub def findIndexOfRight(f: a -> Bool & ef, a: Array[a]): Option[Int32] & Impure =
        let len = length(a);
        def loop(i) = {
            if (i < 0)
                -1
            else
                if (f(a[i]))
                    i
                else
                    loop(i - 1)
        };
        let i = loop(len - 1);
        if (i < 0) None else Some(i)

    ///
    /// Returns the positions of the all the elements in `a` satisfying `f`.
    ///
    @Time(time(f) * length(a)) @Space(space(f) * length(a))
    pub def findIndices(f: a -> Bool & ef, a: Array[a]): Array[Int32] & Impure =
        let len = length(a);
        let l = new MutList(static);
        def loop(i) = {
            if (i >= len)
                Nil
            else {
                if (f(a[i])) MutList.push!(i, l) else ();
                loop(i + 1)
            }
        };
        loop(0);
        MutList.toArray(l)

    ///
    /// Build an array of length `len` by applying `f` to the successive indices.
    ///
    @Time(time(f) * len) @Space(space(f) * len)
    pub def init(f: Int32 -> a & ef, len: Int32, r: Region[r]): ScopedArray[a, r] \ { ef, Write(r) } =
        if (len <= 0)
            [] @ r
        else {
            let x = f(0);
            let a = [x; len] @ r;
            def loop(i) = {
                if (i < len) {
                    a[i] = f(i);
                    loop(i + 1)
                } else
                    ()
            };
            loop(1);
            a
        }

    ///
    /// Build a pair of arrays of length `len` by applying `f` to the successive indices.
    ///
    @Time(time(f) * len) @Space(space(f) * len)
    pub def init2(f: Int32 -> (a, b) & ef, len: Int32): (Array[a], Array[b]) & Impure =
        if (len <= 0)
            ([], [])
        else {
            let (x, y) = f(0);
            let arr = new(x, len);
            let brr = new(y, len);
            def loop(i) = {
                if (i < len) {
                    let (a, b) = f(i);
                    arr[i] = a;
                    brr[i] = b;
                    loop(i + 1)
                } else
                    ()
            };
            loop(1);
            (arr, brr)
        }

    ///
    /// Returns `true` if arrays `a` and `b` have the same elements, i.e. are structurally equal.
    ///
    @Time(length(a)) @Space(1)
    pub def sameElements(a: ScopedArray[a, r1], b: ScopedArray[a, r2]): Bool \ Read(r1, r2) with Eq[a] =
        let alen = length(a);
        let blen = length(b);
        def loop(i) = {
            if (i >= alen)
                true
            else
                if (a[i] != b[i])
                    false
                else
                    loop(i + 1)
        };
        if (alen == blen)
            loop(0)
        else
            false

    ///
    /// Apply the effectful function `f` to all the elements in the array `a`.
    ///
    @Time(time(f) * length(a)) @Space(space(f))
    pub def foreach(f: a -> Unit & ef, a: Array[a]): Unit & Impure =
        let len = length(a);
        def loop(i) = {
            if (i >= len)
                ()
            else {
                f(a[i]);
                loop(i + 1)
            }
        };
        if (len < 1)
            ()
        else
            loop(0)

    ///
    /// Apply the effectful function `f` to all the elements in the array `a`.
    ///
    @Time(time(f) * length(a)) @Space(space(f))
    pub def foreachWithIndex(f: (a, Int32) -> Unit & ef, a: ScopedArray[a, r]): Unit \ { ef, Read(r), Write(r) } =
        let len = length(a);
        def loop(i) = {
            if (i >= len)
                ()
            else {
                f(a[i], i);
                loop(i + 1)
            }
        };
        if (len < 1)
            ()
        else
            loop(0)

    ///
    /// Apply the effectful function `f` to all the elements in the array `a` and the accumulator `s`.
    /// The function returns the final value of the accumulator `s`.
    ///
    @Time(time(f) * length(a)) @Space(space(f))
    def foreachAccum(f: (a, b) -> b & ef, s: b, a: ScopedArray[a, r]): b \ { ef, Read(r) } =
        let len = length(a);
        def loop(i, acc) = {
            if (i >= len)
                acc
            else
                loop(i + 1, f(a[i], acc))
        };
        loop(0, s)

    ///
    /// Returns a copy of `a` with the elements starting at index `i` replaced by `sub`.
    ///
    @Time(length(a)) @Space(length(a))
    pub def updateSequence(i: Int32, sub: ScopedArray[a, r1], a: ScopedArray[a, r2]): ScopedArray[a, r2] \ { Read(r1), Write(r2) } =
        let end = i + length(sub);
        let f = ix -> if (ix >= i and ix < end) sub[ix - i] else a[ix];
        init(f, length(a), Scoped.regionOf(a))

    ///
    /// Update the mutable array `a` with the elements starting at index `i` replaced by `sub`.
    ///
    @Time(length(a)) @Space(1)
    pub def updateSequence!(i: Int32, sub: ScopedArray[a, r1], a: ScopedArray[a, r2]): Unit \ { Read(r1), Write(r2) } =
        let end = i + length(sub);
        let f = { (_,ix) ->
            if (ix >= i and ix < end)
                a[ix] = sub[ix-i]
            else
                ()
        };
        foreachWithIndex(f, a)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `a` with `sep` inserted between each element.
    ///
    pub def join(sep: String, a: Array[a]): String & Impure with ToString[a] =
        joinWith(x -> ToString.toString(x), sep, a)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `a` according to `f` with `sep` inserted between each element.
    ///
    pub def joinWith(f: a -> String & ef, sep: String, a: Array[a]): String & Impure =
        use StringBuilder.append!;
        let lastSep = String.length(sep);
        let sb = StringBuilder.new();
        foreach(x -> { append!(f(x), sb); append!(sep, sb) }, a);
        StringBuilder.toString(sb) |> String.dropRight(lastSep)

    ///
    /// Returns a sorted copy of array `a`, where the elements are ordered from low to high according to
    /// their `Order` instance.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `a`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sort(a: ScopedArray[a, r]): ScopedArray[a, r] \ { Read(r), Write(r) } with Order[a] =
        sortWith(Order.compare, a)

    ///
    /// Returns a sorted copy of array `a`, where the elements are ordered from low to high according to
    /// the `Order` instance for the values obtained by applying `f` to each element.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `a`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortBy(f: a -> b, a: ScopedArray[a, r]): ScopedArray[a, r] \ { Read(r), Write(r) } with Order[b] =
        sortWith(Order.compare `on` f, a)

    ///
    /// Returns a sorted copy of array `a`, where the elements are ordered from low to high according to
    /// the comparison function `cmp`.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `a`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortWith(cmp: (a,a) -> Comparison, a: ScopedArray[a, r]): ScopedArray[a, r] \ { Read(r), Write(r) } =
        let len = length(a);
        let b = copyOfRange(0, len, a, Scoped.regionOf(a));
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
    pub def sort!(a: ScopedArray[a, r]): Unit \ { Read(r), Write(r) } with Order[a] =
        sortWith!(Order.compare, a)

    ///
    /// Sort array `a` so that elements are ordered from low to high according to the `Order` instance for
    /// the values obtained by applying `f` to each element. The array is mutated in-place.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `a`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortBy!(f: a -> b, a: ScopedArray[a, r]): Unit \ { Read(r), Write(r) } with Order[b] =
        sortWith!(Order.compare `on` f, a)

    ///
    /// Sort array `a` so that elements are ordered from low to high according to the comparison function `cmp`.
    /// The array is mutated in-place.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `a`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortWith!(cmp: (a,a) -> Comparison, a: ScopedArray[a, r]): Unit \ { Read(r), Write(r) } =
        sortWithin!(cmp, 0, length(a) - 1, a)

    ///
    /// Sort array `a` between the indices `lo` and `hi` (both inclusive) so that elements in that range are ordered
    /// from low to high according to the comparison function `cmp`. The array is mutated in-place where elements
    /// outside the specified range are not changed. If `lo >= hi`, this does nothing.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `a`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortWithin!(cmp: (a,a) -> Comparison, lo: Int32, hi: Int32, a: ScopedArray[a, r]): Unit \ { Read(r), Write(r) }=
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
    def swap!(a: ScopedArray[a, r], i: Int32, j: Int32): Unit \ { Read(r), Write(r) } =
        let x = a[i];
        let y = a[j];
        a[i] = y;
        a[j] = x;
        ()

    ///
    /// Partition step of the Quicksort algorithm.
    ///
    def quicksortPartition(cmp: (a,a) -> Comparison, a: ScopedArray[a, r], lo: Int32, hi: Int32): Int32 \ { Read(r), Write(r) } =
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
    /// Copies the range between `b` (inclusive) and `e` (exclusive) of a into a new array.
    ///
    /// A valid range has the following properties:
    ///   - 0 <= `b` <= `length(a)`.
    ///   - `e` >= `b`.
    /// If the range is valid and greater than the length of `a`, the resulting array with have the length of the range
    /// and include the specified range of `a`.
    ///
    pub def copyOfRange(b: Int32, e: Int32, a: ScopedArray[a, r1], _: Region[r2]): ScopedArray[a, r2] \ { Read(r1), Write(r2) } =
        match (reifyType a) {
            case ErasedType =>
                let arr = a as ScopedArray[##java.lang.Object, r1];
                import static java.util.Arrays.copyOfRange(ScopedArray[##java.lang.Object, r1], Int32, Int32): ScopedArray[##java.lang.Object, r2] & r1 and r2 as copy;
                copy(arr, b, e) as ScopedArray[a, r2]
            case ReifiedUnit =>
                let arr = a as ScopedArray[Unit, r1];
                import static java.util.Arrays.copyOfRange(ScopedArray[Unit, r1], Int32, Int32): ScopedArray[Unit, r2] & r1 and r2 as copy;
                copy(arr, b, e) as ScopedArray[a, r2]
            case ReifiedBool =>
                let arr = a as ScopedArray[Bool, r1];
                import static java.util.Arrays.copyOfRange(ScopedArray[Bool, r1], Int32, Int32): ScopedArray[Bool, r2] & r1 and r2 as copy;
                copy(arr, b, e) as ScopedArray[a, r2]
            case ReifiedChar =>
                let arr = a as ScopedArray[Char, r1];
                import static java.util.Arrays.copyOfRange(ScopedArray[Char, r1], Int32, Int32): ScopedArray[Char, r2] & r1 and r2 as copy;
                copy(arr, b, e) as ScopedArray[a, r2]
            case ReifiedFloat32 =>
                let arr = a as ScopedArray[Float32, r1];
                import static java.util.Arrays.copyOfRange(ScopedArray[Float32, r1], Int32, Int32): ScopedArray[Float32, r2] & r1 and r2 as copy;
                copy(arr, b, e) as ScopedArray[a, r2]
            case ReifiedFloat64 =>
                let arr = a as ScopedArray[Float64, r1];
                import static java.util.Arrays.copyOfRange(ScopedArray[Float64, r1], Int32, Int32): ScopedArray[Float64, r2] & r1 and r2 as copy;
                copy(arr, b, e) as ScopedArray[a, r2]
            case ReifiedInt8 =>
                let arr = a as ScopedArray[Int8, r1];
                import static java.util.Arrays.copyOfRange(ScopedArray[Int8, r1], Int32, Int32): ScopedArray[Int8, r2] & r1 and r2 as copy;
                copy(arr, b, e) as ScopedArray[a, r2]
            case ReifiedInt16 =>
                let arr = a as ScopedArray[Int16, r1];
                import static java.util.Arrays.copyOfRange(ScopedArray[Int16, r1], Int32, Int32): ScopedArray[Int16, r2] & r1 and r2 as copy;
                copy(arr, b, e) as ScopedArray[a, r2]
            case ReifiedInt32 =>
                let arr = a as ScopedArray[Int32, r1];
                import static java.util.Arrays.copyOfRange(ScopedArray[Int32, r1], Int32, Int32): ScopedArray[Int32, r2] & r1 and r2 as copy;
                copy(arr, b, e) as ScopedArray[a, r2]
            case ReifiedInt64 =>
                let arr = a as ScopedArray[Int64, r1];
                import static java.util.Arrays.copyOfRange(ScopedArray[Int64, r1], Int32, Int32): ScopedArray[Int64, r2] & r1 and r2 as copy;
                copy(arr, b, e) as ScopedArray[a, r2]
            case ReifiedArray(_) =>
                let arr = a as ScopedArray[##java.lang.Object, r1];
                import static java.util.Arrays.copyOfRange(ScopedArray[##java.lang.Object, r1], Int32, Int32): ScopedArray[##java.lang.Object, r2] & r1 and r2 as copy;
                copy(arr, b, e) as ScopedArray[a, r2]
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
