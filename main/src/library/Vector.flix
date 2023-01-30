/*
 *  Copyright 2023 Magnus Madsen, Stephen Tetley
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

///
/// A Vector data type which is fundamentally like Array but is immutable.
///

type alias Vector[a] = Array[a, true]

namespace Vector {

    ///
    /// Compares `a` and `b` lexicographically.
    ///
    pub def compare(a: Vector[a], b: Vector[a]): Comparison with Order[a] =
        let len = Int32.min(length(a), length(b));
        def loop(i) = {
            if (i < len) {
                let cmp = get(i, a) <=> get(i, b);
                if (cmp == Comparison.EqualTo)
                    loop(i + 1)
                else
                    cmp
            } else if (i < Array.length(a)) {
                Comparison.GreaterThan
            } else if (i < Array.length(b)) {
                Comparison.LessThan
            } else {
                Comparison.EqualTo
            }
        };
        loop(0)

    ///
    /// Returns a string representation of the given vector `v`.
    ///
    pub def toString(v: Vector[a]): String with ToString[a] = region rc {
        let sb = new StringBuilder(rc);
        let first = ref true @ rc;

        StringBuilder.appendString!("Vector#{", sb);

        let f = (x) -> {
            if (not deref first) StringBuilder.appendString!(", ", sb) else first := false;
            StringBuilder.appendString!("${x}", sb)
        };
        forEach(f, v);

        StringBuilder.appendString!("}", sb);
        StringBuilder.toString(sb)
    }

    ///
    /// Returns an empty (length zero) vector.
    ///
    pub def empty(): Vector[a] = region rc {
        buildVector(rc, 0, _ -> ())
    }

    ///
    /// Retrieves the value at position `i` in the vector `v`.
    ///
    pub def get(i: Int32, v: Vector[a]): a =
        v[i]

    ///
    /// Optionally returns the element at position `i` in the vector `v`.
    ///
    pub def nth(i: Int32, v: Vector[a]): Option[a] =
        if (0 <= i and i < length(v))
            Some(get(i, v))
        else
            None

    ///
    /// Returns `true` if the given vector `v` is empty.
    ///
    pub def isEmpty(v: Vector[a]): Bool = length(v) == 0

    ///
    /// Returns the length of the vector `v`.
    ///
    pub def length(v: Vector[a]): Int32 = Array.length(v)

    ///
    /// Returns a fresh vector ...
    ///
    pub def buildVector(rc: Region[r], len: Int32, f: Array[a, r] -> Unit \ ef): Vector[a] \ ef =
        unsafe_cast (
            let arr = Array.new(rc, len);
            let _ = f(arr);
            arr
            ) as Array[a, true] \ ef

    pub def liftArrayProducer(rc: Region[r], f: Region[r] -> Array[a, r] \ ef): Vector[a] \ ef =
        unsafe_cast f(rc) as Vector[a]


    ///
    /// Returns a fresh array with the elements from the vector `v` from index `b` (inclusive) until index `e` (exclusive).
    ///
    pub def slice(start: {start = Int32}, end: {end = Int32}, v: Vector[a]): Vector[a] = region rc {
        liftArrayProducer(rc, r1 -> Array.slice(r1, start.start, end.end, v))
    }

    ///
    /// Returns the vector `v` as an array.
    ///
    pub def toArray(rc: Region[r], v: Vector[a]): Array[a, r] \ Write(r) =
        let arr = Array.new(rc, length(v));
        forEachWithIndex((i, x) -> Array.put(x, i, arr), v);
        arr


    ///
    /// Returns the vector `v` as a list.
    ///
    pub def toList(v: Vector[a]): List[a] =
        def loop(i, acc) = {
            if (i == 0) {
                acc
            } else {
                let x = get(i - 1, v);
                loop(i - 1, x :: acc)
            }
        };
        loop(length(v), Nil)

    ///
    /// Optionally returns the vector `v` as a non-empty list.
    ///
    /// If `v` is empty return `None`, otherwise return the Nel wrapped in `Some`.
    ///
    pub def toNel(v: Vector[a]): Option[Nel[a]] =
        def loop(i, acc) = {
            if (i == 0) {
                acc
            } else {
                let x = get(i - 1, v);
                loop(i - 1, Nel.cons(x, acc))
            }
        };
        if (Array.isEmpty(v)) {
            None
        } else {
            let i = length(v) - 1;
            Some(loop(i, Nel.singleton(get(i, v))))
        }

    ///
    /// Returns the vector `v` as a `DelayList`.
    ///
    pub def toDelayList(v: Vector[a]): DelayList[a] =
        def loop(i, acc) = {
            if (i < 0)
                acc
            else
                loop(i - 1, DelayList.ECons(get(i, v), acc))
        };
        loop(length(v) - 1, DelayList.ENil)

    ///
    /// Returns the vector `v` as a chain.
    ///
    pub def toChain(v: Vector[a]): Chain[a] =
        def loop(i, acc) = {
            if (i == 0) {
                acc
            } else {
                let x = get(i - 1, v);
                loop(i - 1, Chain.cons(x, acc))
            }
        };
        loop(length(v), Chain.empty())

    ///
    /// Optionally returns the vector `v` as a non-empty chain.
    ///
    /// If `v` is empty return `None`, otherwise return the Nec wrapped in `Some`.
    ///
    pub def toNec(v: Vector[a]): Option[Nec[a]] =
        def loop(i, acc) = {
            if (i == 0) {
                acc
            } else {
                let x = get(i - 1, v);
                loop(i - 1, Nec.cons(x, acc))
            }
        };
        if (isEmpty(v)) {
            None
        } else {
            let i = length(v) - 1;
            Some(loop(i, Nec.singleton(get(i, v))))
        }

    ///
    /// Returns `v` as a MutDeque.
    ///
    pub def toMutDeque(rc: Region[r], v: Vector[a]): MutDeque[a, r] \ Write(r)   =
        let d = new MutDeque(rc);
        forEach(x -> MutDeque.pushBack(x, d), v);
        d

    ///
    /// Returns `Some(x)` if `x` is the first element of `v`.
    ///
    /// Returns `None` if `v` is empty.
    ///
    pub def head(v: Vector[a]): Option[a] =
        nth(0, v)

    ///
    /// Returns `Some(x)` if `x` is the last element of `v`.
    ///
    /// Returns `None` if `v` is empty.
    ///
    pub def last(v: Vector[a]): Option[a] =
        nth(length(v) - 1, v)

    ///
    /// Return a new vector, appending the elements `b` to elements of `a`.
    ///
    pub def append(a: Vector[a], b: Vector[a]): Vector[a] = region rc {
        buildVector(rc, length(a) + length(b), arr -> {
            Array.updateSequence!(0, a, arr);
            Array.updateSequence!(length(a), b, arr)
        })
    }


    ///
    /// Returns `true` if and only if `v` contains the element `x`.
    ///
    pub def memberOf(x: a, v: Vector[a]): Bool with Eq[a] =
        exists(y -> y == x, v)

    ///
    /// Optionally finds the smallest element of `v` according to the `Order` on `v`.
    ///
    /// Returns `None` if `v` is empty.
    ///
    pub def minimum(v: Vector[a]): Option[a] with Order[a] =
        reduceLeft(Order.min, v)

    ///
    /// Optionally finds the smallest element of `v` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `v` is empty.
    ///
    pub def minimumBy(cmp: (a, a) -> Comparison, v: Vector[a]): Option[a] =
        reduceLeft(Order.minBy(cmp), v)

    ///
    /// Optionally finds the largest element of `v` according to the `Order` on `v`.
    ///
    /// Returns `None` if `v` is empty.
    ///
    pub def maximum(v: Vector[a]): Option[a] with Order[a] =
        reduceLeft(Order.max, v)

    ///
    /// Optionally finds the largest element of `v` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `v` is empty.
    ///
    pub def maximumBy(cmp: (a, a) -> Comparison, v: Vector[a]): Option[a] =
        reduceLeft(Order.maxBy(cmp), v)

    ///
    /// Alias for `IndexOfLeft`
    ///
    pub def indexOf(x: a, v: Vector[a]): Option[Int32] with Eq[a] =
        indexOfLeft(x, v)

    ///
    /// Optionally returns the position of the first occurrence of `a` in `v`
    /// searching from left to right.
    ///
    pub def indexOfLeft(a: a, v: Vector[a]): Option[Int32] with Eq[a] =
        def loop(i) = {
            if (i >= length(v))
                -1
            else if (get(i, v) == a)
                i
            else
                loop(i + 1)
        };
        let i = loop(0);
        if (i < 0) None else Some(i)

    ///
    /// Optionally returns the position of the first occurrence of `a` in `v`
    /// searching from right to left.
    ///
    pub def indexOfRight(a: a, v: Vector[a]): Option[Int32] with Eq[a] =
        def loop(i) = {
            if (i < 0)
                -1
            else if (get(i, v) == a)
                i
            else
                loop(i - 1)
        };
        let i = loop(length(v) - 1);
        if (i < 0) None else Some(i)

    ///
    /// Return the positions of the all the occurrences of `a` in `v`.
    ///
    pub def indices(a: a, v: Vector[a]): Vector[Int32] with Eq[a] =
        findIndices(b -> a == b, v)

    ///
    /// Alias for `findLeft`.
    ///
    pub def find(f: a -> Bool \ ef, v: Vector[a]): Option[a] \ ef =
        findLeft(f, v)

    ///
    /// Optionally returns the first element of `v` that satisfies the predicate `f` when searching from left to right.
    ///
    pub def findLeft(f: a -> Bool \ ef, v: Vector[a]): Option[a] \ ef =
        match findIndexOfLeft(f, v) {
            case None    => None
            case Some(i) => Some(get(i, v))
        }

    ///
    /// Optionally returns the first element of `v` that satisfies the predicate `f` when searching from right to left.
    ///
    pub def findRight(f: a -> Bool \ ef, v: Vector[a]): Option[a] \ ef =
        match findIndexOfRight(x -> f(x), v) {
            case None    => None
            case Some(i) => Some(get(i, v))
        }

    ///
    /// Returns a vector of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns an empty vector if `b >= e`.
    ///
    pub def range(b: Int32, e: Int32): Vector[Int32] = region rc {
        let len = e - b;
        let s = ref b @ rc;
        buildVector(rc, if (len < 0) 0 else len, Array.transform!(_ -> {let n = deref s; s := n+1; n}))
    }

    ///
    /// Returns a vector with the element `x` repeated `n` times.
    ///
    /// Returns an empty vector if `n <= 0`.
    ///
    pub def repeat(n: Int32, x: a): Vector[a] = region rc {
        buildVector(rc, n, arr -> Array.transform!(_ -> x, arr))
    }

    ///
    /// Alias for `scanLeft`.
    ///
    pub def scan(f: (b, a) -> b \ ef, s: b, v: Vector[a]): Vector[b] \ ef =
        scanLeft(f, s, v)

    ///
    /// Accumulates the result of applying `f` to `v` going left to right.
    ///
    /// That is, the result is of the form: `[s , f(s, x1), f(f(s, x1), x2),  ...]`.
    ///
    pub def scanLeft(f: (b, a) -> b \ ef, s: b, v: Vector[a]): Vector[b] \ ef = region rc {
        let acc = ref s @ rc;
        buildVector(rc, length(v) + 1, arr -> {
            Array.put(s, 0, arr);
            let step = (i, x) -> {
                let s1 = f(deref acc, x);
                acc := s1;
                Array.put(s1, i+1, arr)
            };
            forEachWithIndex(step, v)
        })
    }

    ///
    /// Accumulates the result of applying `f` to `xs` going right to left.
    ///
    /// That is, the result is of the form: `[..., f(xn-1, f(xn, s)), f(xn, s), s]`.
    ///
    pub def scanRight(f: (a, b) -> b \ ef, s: b, v: Vector[a]): Vector[b] \ ef = region rc {
        let len = length(v);
        let acc = ref s @ rc;
        buildVector(rc, len + 1, arr -> {
            Array.put(s, len, arr);
            def loop(i) = {
                if (i < 0)
                    ()
                else {
                    let s1 = f(get(i, v), deref acc);
                    acc := s1;
                    Array.put(s1, i, arr);
                    loop(i - 1)
                }
            };
            loop(len - 1)
        })
    }

    ///
    /// Returns the result of applying `f` to every element in `v`.
    ///
    /// The result is a new vector.
    ///
    pub def map(f: a -> b \ ef, v: Vector[a]): Vector[b] \ ef = region rc {
        let len = length(v);
        buildVector(rc, len, arr -> forEachWithIndex((i, x) -> Array.put(f(x), i, arr), v))
    }

    ///
    /// Returns the result of applying `f` to every element in `v` along with that element's index.
    ///
    /// That is, the result is of the form: `[ f(a[0], 0), f(a[1], 1), ... ]`.
    ///
    pub def mapWithIndex(f: (Int32, a) -> b \ ef, v: Vector[a]): Vector[b] \ ef = region rc {
        let len = length(v);
        buildVector(rc, len, arr -> forEachWithIndex((i, x) -> Array.put(f(i, x), i, arr), v))
    }

    ///
    /// Returns the result of applying `f` to every element in `v` and concatenating the results.
    ///
    pub def flatMap(f: a -> Vector[b] \ ef, v: Vector[a]): Vector[b] \ ef =
        let len = length(v);
        init(i -> f(get(i, v)), len) |> flatten

    ///
    /// Returns the reverse of `v`.
    ///
    pub def reverse(v: Vector[a]): Vector[a] = region rc {
        let len = length(v);
        buildVector(rc, len, arr -> forEachWithIndex((i, x) -> Array.put(x, len - (i + 1), arr), v))
    }

    ///
    /// Rotate the contents of vector `v` by `n` steps to the left.
    ///
    pub def rotateLeft(n: Int32, v: Vector[a]): Vector[a] =
        let len = length(v);
        if (len < 1)
            empty()
        else
            if (n < 0)
                rotateRightHelper(Int32.abs(n), v)
            else
                rotateLeftHelper(n, v)

    ///
    /// Helper function for `rotateLeft` and `rotateRight`.
    ///
    /// Precondition: `n` must be positive.
    ///
    /// This is an explicit helper to avoid code duplication.
    ///
    def rotateLeftHelper(n: Int32, v: Vector[a]): Vector[a] = region rc {
        let len = length(v);
        buildVector(rc, len, arr ->
            forEachWithIndex((i, x) -> {let ix = Int32.mod(i - n, len); Array.put(x, ix, arr)}, v)
        )
    }

    ///
    /// Rotate the contents of vector `v` by `n` steps to the right.
    ///
    pub def rotateRight(n: Int32, v: Vector[a]): Vector[a] =
        if (length(v) < 1)
            empty()
        else
            if (n < 0)
                rotateLeftHelper(Int32.abs(n), v)
            else
                rotateRightHelper(n, v)

    ///
    /// Helper function for `rotateRight` and `rotateLeft`.
    ///
    /// Precondition: `n` must be positive.
    ///
    /// This is an explicit helper to avoid code duplication.
    ///
    def rotateRightHelper(n: Int32, v: Vector[a]): Vector[a] = region rc {
        let len = length(v);
        buildVector(rc, len, arr ->
            forEachWithIndex((i, x) -> {let ix = Int32.mod(i + n, len); Array.put(x, ix, arr)}, v)
        )
     }

    ///
    /// Returns a copy of `v` with the element at index `i` replaced by `x`.
    ///
    /// Returns a copy of `v` if `i < 0` or `i > length(xs)-1`.
    ///
    pub def update(i: Int32, x: a, v: Vector[a]): Vector[a] = region rc {
        let len = length(v);
        buildVector(rc, len, arr ->
            forEachWithIndex((ix, x1) -> Array.put(if (ix == i) x else x1, ix, arr), v)
        )
    }

    ///
    /// Returns `b` with the `n` elements starting at index `i` replaced with the elements of `a`.
    ///
    /// If any of the indices `i, i+1, i+2, ... , i+n-1` are out of range in `b` then no patching is done at these indices.
    /// If `a` becomes depleted then no further patching is done.
    /// If patching occurs at index `i+j` in `b`, then the element at index `j` in `a` is used.
    ///
    pub def patch(i: Int32, n: Int32, a: Vector[a], b: Vector[a]): Vector[a] =
        let len1 = length(a);
        let size = if (n > len1) len1 else n;
        let sub = slice(start = 0, end = size, a);
        updateSequence(i, sub, b)

    ///
    /// Returns a copy of `v` with every occurrence of `from` replaced by `to`.
    ///
    pub def replace(from: {from = a}, to: {to = a}, v: Vector[a]): Vector[a] with Eq[a] =
        map(e -> if (e == from.from) to.to else e, v)

    ///
    /// Sum the lengths of a vector of vectors.
    ///
    /// Helper function for `intercalate` and `flatten`.
    ///
    def sumLengths(vs: Vector[Vector[a]]): Int32 =
        foldLeft((acc, a) -> acc + length(a), 0, vs)

    ///
    /// Returns `true` if and only if `a` is a prefix of `b`.
    ///
    pub def isPrefixOf(a: Vector[a], b: Vector[a]): Bool with Eq[a] =
        let len1 = length(a);
        if (len1 > length(b))
            false
        else
            def loop(i) = {
                if (i >= len1)
                    true
                else if (get(i, a) != get(i, b))
                    false
                else
                    loop(i + 1)
            };
            loop(0)

    ///
    /// Returns `true` if and only if `a` is a infix of `b`.
    ///
    pub def isInfixOf(a: Vector[a], b: Vector[a]): Bool with Eq[a] =
        let len1 = length(a);
        let len2 = length(b);
        if (len1 > len2)
            false
        else if (len1 == 0)
            true
        else
            isInfixOfSearch(a, b, len1, len2, 0)

    ///
    /// Helper function for `isInfixOf` - scan `b` to find a match with first element of `b`.
    ///
    /// Precondition: len1 (length of `a`) > 0
    ///
    def isInfixOfSearch(a: Vector[a], b: Vector[a], len1: Int32, len2: Int32, j: Int32): Bool with Eq[a] =
        if (j >= len2)
            false
        else if (get(0, a) == get(j, b))
            isInfixOfCheck(a, b, len1, len2, 1, j+1)
        else
            isInfixOfSearch(a, b, len1, len2, j+1)

    ///
    /// Helper function for `isInfixOf` - `a` has started matching, scan to see if it all matches.
    ///
    def isInfixOfCheck(a: Vector[a], b: Vector[a], len1: Int32, len2: Int32, i: Int32, j: Int32): Bool with Eq[a] =
        if (i >= len1)
            // `a` exhausted, so success
            true
        else if (j >= len2)
            // `b` exhausted, `a` still trying to match, so failure
            false
        else if(get(i, a) == get(j, b))
            isInfixOfCheck(a, b, len1, len2, i+1, j+1)
        else
            isInfixOfSearch(a, b, len1, len2, j+1)

    ///
    /// Returns `true` if and only if `a` is a suffix of `b`.
    ///
    pub def isSuffixOf(a: Vector[a], b: Vector[a]): Bool with Eq[a] =
        let len1 = length(a);
        let len2 = length(b);
        if (len1 > len2)
            false
        else
            def loop(i, j) = {
                if (i < 0)
                    true
                else if (get(i, a) != get(j, b))
                    false
                else
                    loop(i - 1, j - 1)
            };
            loop(len1 - 1, len2 - 1)

    ///
    /// Returns the result of applying `combine` to all the elements in `v`, using `empty` as the initial value.
    ///
    pub def fold(v: Vector[a]): a with Monoid[a] = foldLeft(Monoid.combine, Monoid.empty(), v)

    ///
    /// Applies `f` to a start value `s` and all elements in `v` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, a[0]), a[1])..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b \ ef, s: b, v: Vector[a]): b \ ef =
        let len = length(v);
        def loop(i, acc) = {
            if (i >= len)
                acc
            else
                loop(i + 1, f(acc, get(i, v)))
        };
        loop(0, s)

    ///
    /// Applies `f` to a start value `s` and all elements in `v` going from right to left.
    ///
    /// That is, the result is of the form: `f(a[0], ...f(a[n-1], f(a[n], s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b \ ef, s: b, v: Vector[a]): b \ ef =
        def loop(i, acc) = {
            if (i < 0)
                acc
            else
                loop(i - 1, f(get(i, v), acc))
        };
        loop(length(v) - 1, s)

    ///
    /// Applies `f` to a start value `z` and all elements in `v` going from right to left.
    ///
    /// That is, the result is of the form: `f(a[0], ...f(a[n-1], f(a[n], z))...)`.
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, z: b, v: Vector[a]): b \ ef =
        def loop(i) = {
            if (i == length(v))
                z
            else
                f(get(i, v), _ -> loop(i + 1))
        };
        loop(0)

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap(f: a -> b \ ef, v: Vector[a]): b \ ef with Monoid[b] =
        foldLeft((acc, x) -> Monoid.combine(acc, f(x)), Monoid.empty(), v)

    ///
    /// Applies `f` to all elements in `v` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// Returns `None` if `v` is empty.
    ///
    pub def reduceLeft(f: (a, a) ->  a \ ef, v: Vector[a]): Option[a] \ ef =
        let len = length(v);
        def loop(i, acc) = {
            if (i >= len)
                acc
            else
                loop(i + 1, f(acc, get(i, v)))
        };
        if (len == 0)
            None
        else
            Some(loop(1, get(0, v)))

    ///
    /// Applies `f` to all elements in `v` going from right to left until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// Returns `None` if `v` is empty.
    ///
    pub def reduceRight(f: (a, a) -> a \ ef, v: Vector[a]): Option[a] \ ef =
        let len = length(v);
        def loop(i, acc) = {
            if (i < 0)
                acc
            else
                loop(i - 1, f(get(i, v), acc))
        };
        if (len == 0)
            None
        else
            Some(loop(len - 2, get(len - 1, v)))

    ///
    /// Returns the number of elements in `v` that satisfy the predicate `f`.
    ///
    pub def count(f: a -> Bool \ ef, v: Vector[a]): Int32 \ ef =
        foldLeft((b, x) -> if (f(x)) b + 1 else b, 0, v)

    ///
    /// Returns the sum of all elements in the vector `v`.
    ///
    pub def sum(v: Vector[Int32]): Int32 =
        foldLeft((acc, x) -> acc + x, 0, v)

    ///
    /// Returns the sum of all elements in the vector `v` according to the function `f`.
    ///
    pub def sumWith(f: a -> Int32 \ ef, v: Vector[a]): Int32 \ ef =
        foldLeft((acc, x) -> acc + f(x), 0, v)

    ///
    /// Returns the product of all elements in the vector `v`.
    ///
    pub def product(v: Vector[Int32]): Int32 =
        if (isEmpty(v))
            1
        else
            foldLeft((acc, x) -> acc * x, 1, v)

    ///
    /// Returns the product of all elements in the vector `v` according to the function `f`.
    ///
    pub def productWith(f: a -> Int32 \ ef, v: Vector[a]): Int32 \ ef =
        if (isEmpty(v))
            1
        else
            foldLeft((acc, x) -> acc * f(x), 1, v)

    ///
    /// Returns the concatenation of the vectorss of in the vector `vs`.
    ///
    pub def flatten(vs: Vector[Vector[a]]): Vector[a] = region rc {
        let len = sumLengths(vs);
        let pos = ref 0 @ rc;
        buildVector(rc, len, arr ->
            forEach(v -> {
                let i = deref pos;
                pos := i + length(v);
                Array.updateSequence!(i, v, arr)
                }, vs)
        )
    }

    ///
    /// Returns `true` if and only if at least one element in `v` satisfies the predicate `f`.
    ///
    /// Returns `false` if `v` is empty.
    ///
    pub def exists(f: a -> Bool \ ef, v: Vector[a]): Bool \ ef =
        let len = length(v);
        def loop(i) = {
            if (i >= len)
                false
            else if (f(get(i, v)))
                true
            else
                loop(i + 1)
        };
        loop(0)

    ///
    /// Returns `true` if and only if all elements in `v` satisfy the predicate `f`.
    ///
    /// Returns `true` if `v` is empty.
    ///
    pub def forAll(f: a -> Bool \ ef, v: Vector[a]): Bool \ ef =
        let len = length(v);
        def loop(i) = {
            if (i >= len)
                true
            else if (f(get(i, v)))
                loop(i + 1)
            else
                false
        };
        loop(0)

    ///
    /// Returns an array of every element in `arr` that satisfies the predicate `f`.
    ///
    pub def filter(f: a -> Bool \ ef, v: Vector[a]): Vector[a] \ ef = region rc {
        let len = length(v);
        let ix = ref 0 @ rc;
        if (len < 1)
            empty()
        else
            buildVector(rc, len, arr ->
                forEach(x ->
                    if (f(x)) {
                        let i = deref ix;
                        ix := i + 1;
                        Array.put(x, i, arr)
                    } else (), v)
                ) |> slice(start = 0, end = deref ix)
    }

    ///
    /// Returns a pair of vectors `(v1, v2)`.
    ///
    /// `v1` contains all elements of `v` that satisfy the predicate `f`.
    /// `v2` contains all elements of `v` that do not satisfy the predicate `f`.
    ///
    pub def partition(f: a -> Bool \ ef, v: Vector[a]): (Vector[a], Vector[a]) \ ef =
        let step = { (x, acc) ->
            let (a1, a2) = acc;
            if (f(x)) (x :: a1, a2) else (a1, x :: a2)
        };
        let (xs, ys) = foldRight(step, (Nil, Nil), v);
        (List.toVector(xs), List.toVector(ys))

    ///
    /// Returns a pair of vectors `(v1, v2)`.
    ///
    /// `v1` is the longest prefix of `v` that satisfies the predicate `f`.
    /// `v2` is the remainder of `v`.
    ///
    pub def span(f: a -> Bool \ ef, v: Vector[a]): (Vector[a], Vector[a]) \ ef =
        match findIndexOfLeft(x -> not (f(x)), v) {
            case None    => (takeLeft(length(v), v), empty())
            case Some(i) => (takeLeft(i, v), dropLeft(i, v))
        }

    ///
    /// Alias for `dropLeft`.
    ///
    pub def drop(n: Int32, v: Vector[a]): Vector[a] =
        dropLeft(n, v)

    ///
    /// Returns a copy of vector `v`, dropping the first `n` elements.
    ///
    /// Returns an empty vector if `n > length(v)`.
    ///
    pub def dropLeft( n: Int32, v: Vector[a]): Vector[a] =
        let len = length(v);
        if (n > len)
            empty()
        else {
            let start = if (n < 0) 0 else n;
            slice(start = start, end = len, v)
        }

    ///
    /// Returns a copy of vector `v`, dropping the last `n` elements.
    ///
    /// Returns an empty vector if `n > length(v)`.
    ///
    pub def dropRight(n: Int32, v: Vector[a]): Vector[a] =
        let len = length(v);
        if (n >= len)
            empty()
        else {
            let end = if (n < 0) len else len - n;
            slice(start = 0, end = end, v)
        }

    ///
    /// Alias for `dropWhileLeft`.
    ///
    pub def dropWhile(f: a -> Bool \ ef, v: Vector[a]): Vector[a] \ ef =
        dropWhileLeft(f, v)

    ///
    /// Returns copy of vector `v` without the longest prefix that satisfies the predicate `f`.
    ///
    pub def dropWhileLeft(f: a -> Bool \ ef, v: Vector[a]): Vector[a] \ ef =
        match findIndexOfLeft(x -> not (f(x)), v) {
            case None    => empty()
            case Some(i) => dropLeft(i, v)
        }

    ///
    /// Returns copy of vector `v` without the longest suffix that satisfies the predicate `f`.
    ///
    pub def dropWhileRight(f: a -> Bool \ ef, v: Vector[a]): Vector[a] \ ef =
        match findIndexOfRight(x -> not (f(x)), v) {
            case None    => empty()
            case Some(i) => slice(start = 0, end = i + 1, v)
        }

    ///
    /// Alias for `takeLeft`.
    ///
    pub def take(n: Int32, v: Vector[a]): Vector[a] =
        takeLeft(n, v)

    ///
    /// Returns a fresh vector taking first `n` elements of `v`.
    ///
    /// Returns a copy of `v` if `n > length(v)`.
    ///
    pub def takeLeft(n: Int32, v: Vector[a]): Vector[a] =
        if (n <= 0)
            empty()
        else {
            let len = length(v);
            let end = if (n > len) len else n;
            slice(start = 0, end = end, v)
        }

    ///
    /// Returns a fresh vector taking last `n` elements of `v`.
    ///
    /// Returns a copy `v` if `n > length(v)`.
    ///
    pub def takeRight(n: Int32, v: Vector[a]): Vector[a] =
        if (n <= 0)
            empty()
        else {
            let len = length(v);
            let start = if (n > len) 0 else len - n;
            slice( start = start, end = len, v)
        }

    ///
    /// Alias for `takeWhileLeft`.
    ///
    pub def takeWhile(f: a -> Bool \ ef, a: Vector[a]): Vector[a] \ ef =
        takeWhileLeft(f, a)

    ///
    /// Returns the longest prefix of `v` that satisfies the predicate `f`.
    ///
    pub def takeWhileLeft(f: a -> Bool \ ef, v: Vector[a]): Vector[a] \ ef =
        match findIndexOfLeft(x -> not (f(x)), v) {
            case None    => slice(start = 0, end = length(v), v)
            case Some(i) => takeLeft(i, v)
        }

    ///
    /// Returns the longest suffix of `v` that satisfies the predicate `f`.
    ///
    pub def takeWhileRight(f: a -> Bool \ ef, v: Vector[a]): Vector[a] \ ef =
        match findIndexOfRight(x -> not (f(x)), v) {
            case None    => slice(start = 0, end = length(v), v)
            case Some(i) => slice(start = i + 1, end = length(v), v)
        }

    ///
    /// Returns the vector `v` as a set.
    ///
    pub def toSet(v: Vector[a]): Set[a] with Order[a] =
        foldRight(Set.insert, Set.empty(), v)

    ///
    /// Returns the association vector `v` as a map.
    ///
    /// If `v` contains multiple mappings with the same key, `toMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    pub def toMap(v: Vector[(a, b)]): Map[a, b] with Order[a] =
        foldRight((x, m) -> Map.insert(fst(x), snd(x), m), Map.empty(), v)

    ///
    /// Returns the array `a` as a MutList.
    ///
    pub def toMutList(rc: Region[r], v: Vector[a]): MutList[a, r] \ Write(r) =
        toArray(rc, v) |> Array.toMutList(rc)

    ///
    /// Alias for `findIndexOfLeft`.
    ///
    pub def findIndexOf(f: a -> Bool \ ef, v: Vector[a]): Option[Int32] \ ef =
        findIndexOfLeft(f, v)

    ///
    /// Optionally returns the position of the first element in `v` satisfying `f`.
    ///
    pub def findIndexOfLeft(f: a -> Bool \ ef, v: Vector[a]): Option[Int32] \ ef =
        let len = length(v);
        if (len < 1)
            None
        else {
            def loop(i) = {
                if (i >= len)
                    -1
                else if (f(get(i, v)))
                    i
                else
                    loop(i + 1)
            };
            let i = loop(0);
            if (i < 0) None else Some(i)
        }

    ///
    /// Optionally returns the position of the first element in `v` satisfying `f`
    /// searching from right to left.
    ///
    pub def findIndexOfRight(f: a -> Bool \ ef, v: Vector[a]): Option[Int32] \ ef =
        let len = length(v);
        def loop(i) = {
            if (i < 0)
                -1
            else if (f(get(i, v)))
                i
            else
                loop(i - 1)
        };
        let i = loop(len - 1);
        if (i < 0) None else Some(i)

    ///
    /// Returns the positions of the all the elements in `v` satisfying `f`.
    ///
    pub def findIndices(f: a -> Bool \ ef, v: Vector[a]): Vector[Int32] \ ef = region rc {
        let len = length(v);
        let l = unsafe_cast MutList.new(rc) as MutList[Int32, true] \ ef;
        def loop(i) = {
            if (i >= len)
                ()
            else {
                if (f(get(i, v))) {
                    unsafe_cast MutList.push!(i, l) as Unit \ ef
                } else ();
                loop(i + 1)
            }
        };
        loop(0);
        unsafe_cast MutList.toArray(rc, l) as Array[Int32, true] \ ef
    }

    ///
    /// Build an array of length `len` by applying `f` to the successive indices.
    ///
    pub def init(f: Int32 -> a \ ef, len: Int32): Vector[a] \ ef = region rc {
        buildVector(rc, len, Array.transformWithIndex!((i, _) -> f(i)))
    }

    ///
    /// Returns `true` if arrays `a` and `b` have the same elements in the same order, i.e. are structurally equal.
    ///
    pub def sameElements(a: Vector[a], b: Vector[a]): Bool with Eq[a] =
        let alen = length(a);
        let blen = length(b);
        def loop(i) = {
            if (i >= alen)
                true
            else if (get(i, a) != get(i, b))
                false
            else
                loop(i + 1)
        };
        if (alen == blen)
            loop(0)
        else
            false

    ///
    /// Returns an iterator over `v`
    ///
    pub def iterator(rc: Region[r], v: Vector[a]): Iterator[a, r, r] \ { Write(r) } =
        Iterator.range(rc, 0, length(v)) |> Iterator.map(i -> get(i, v))

    ///
    /// Returns an iterator over `v` zipped with the indices of the elements.
    ///
    /// Modifying `a` while using an iterator has undefined behavior and is dangerous.
    ///
    pub def enumerator(rc: Region[r], v: Vector[a]): Iterator[(Int32, a), r, r] \ { Write(r) } =
        iterator(rc, v) |> Iterator.zipWithIndex


    ///
    /// Apply the effectful function `f` to all the elements in the vector `v`.
    ///
    pub def forEach(f: a -> Unit \ ef, v: Vector[a]): Unit \ ef =
        let len = length(v);
        def loop(i) = {
            if (i >= len)
                ()
            else {
                f(get(i, v));
                loop(i + 1)
            }
        };
        loop(0)

    ///
    /// Apply the effectful function `f` to all the elements in the vector `v`.
    ///
    pub def forEachWithIndex(f: (Int32, a) -> Unit \ ef, v: Vector[a]): Unit \ ef =
        let len = length(v);
        def loop(i) = {
            if (i >= len)
                ()
            else {
                f(i, get(i, v));
                loop(i + 1)
            }
        };
        loop(0)

    ///
    /// Returns a copy of `v` with the elements starting at index `i` replaced by `sub`.
    ///
    pub def updateSequence(i: Int32, sub: Vector[a], v: Vector[a]): Vector[a] =
        let end = i + length(sub);
        let len = length(v);
        let f = ix -> if (ix >= i and ix < end) get(ix - i, sub) else get(ix, v);
        init(f, len)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `v` with `sep` inserted between each element.
    ///
    pub def join(sep: String, v: Vector[a]): String with ToString[a] =
        joinWith(ToString.toString, sep, v)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `v` according to `f` with `sep` inserted between each element.
    ///
    pub def joinWith(f: a -> String \ ef, sep: String, v: Vector[a]): String \ ef = region r1 {
        use StringBuilder.append!;
        let lastSep = String.length(sep);
        let sb = new StringBuilder(r1);
        forEach(x -> { append!(f(x), sb); append!(sep, sb) }, v);
        StringBuilder.toString(sb) |> String.dropRight(lastSep)
    }

}
