/*
 * Copyright 2019 Magnus Madsen, Esben Bjerre
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
/// Represents a mutable list.
///
/// Invariant
///   - The length is always higher than the total capacity of the array.
///   - The capacity of the array is always 8 or more.
///
pub enum MutList[a: Type, r: Region] {
    case MutList(Region[r], Ref[Array[a, r], r], Ref[Int32, r])
}

mod MutList {

    ///
    /// Constant which stores the minimum capacity of a MutList.
    ///
    @Internal
    pub def minCapacity(): Int32 = 8

    ///
    /// Returns a string representation of the given MutList `l`.
    ///
    pub def toString(l: MutList[a, r]): String \ Read(r) with ToString[a] = region rc2 {
        let sb = StringBuilder.new(rc2);
        StringBuilder.appendString!("MutList#{", sb);
        forEachWithIndex((i, x) -> {
            if (i < 1)
                StringBuilder.appendString!("${x}", sb)
            else
                StringBuilder.appendString!(", ${x}", sb)
            }, l);
        StringBuilder.appendString!("}", sb);
        StringBuilder.toString(sb)
    }

    ///
    /// Returns a new empty mutable list with a default capacity.
    ///
    pub def new(r: Region[r]): MutList[a, r] \ Write(r) =
        MutList(r, ref Array.new(r, minCapacity()) @ r, ref 0 @ r)

    ///
    /// Returns a mutable list of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns an empty mutable list if `b >= e`.
    ///
    pub def range(r: Region[r], b: Int32, e: Int32): MutList[Int32, r] \ Write(r) =
        let minCap = minCapacity();
        let d = e - b;
        let c = Order.max(d, minCap);
        let f = i -> { let x = b + i; if (x < e) x else Reflect.default() };
        MutList(r, ref Array.init(r, f, c) @ r, ref d @ r)

    ///
    /// Optionally returns the element at position `i` in the mutable list `v`.
    ///
    pub def nth(i: Int32, v: MutList[a, r]): Option[a] \ Read(r) =
        let MutList(_, a, l) = v;
        if (0 <= i and i < deref l)
            Array.nth(i, deref a)
        else
            None

    ///
    /// Returns the number of elements in the given mutable list `v`.
    ///
    pub def length(v: MutList[a, r]): Int32 \ Read(r) =
        let MutList(_, _, l) = v;
        deref l

    ///
    /// Returns `true` if the given mutable list `v` is empty.
    ///
    pub def isEmpty(v: MutList[a, r]): Bool \ Read(r) =
        length(v) == 0

    ///
    /// Returns `true` if the given element `x` is a member of the given mutable list `v`.
    ///
    pub def memberOf(x: a, v: MutList[a, r]): Bool \ Read(r) with Eq[a] =
        exists(y -> y == x, v)

    ///
    /// Optionally finds the smallest element of `v` according to the `Order` on `a`.
    ///
    /// Returns `None` if `v` is empty.
    ///
    pub def minimum(v: MutList[a, r]): Option[a] \ Read(r) with Order[a] =
        reduceLeft(Order.min, v)

    ///
    /// Optionally finds the smallest element of `v` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `v` is empty.
    ///
    pub def minimumBy(cmp: (a, a) -> Comparison, v: MutList[a, r]): Option[a] \ Read(r) =
        reduceLeft(Order.minBy(cmp), v)

    ///
    /// Optionally finds the largest element of `v` according to the `Order` on `a`.
    ///
    /// Returns `None` if `v` is empty.
    ///
    pub def maximum(v: MutList[a, r]): Option[a] \ Read(r) with Order[a] =
        reduceLeft(Order.max, v)

    ///
    /// Optionally finds the largest element of `v` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `v` is empty.
    ///
    pub def maximumBy(cmp: (a, a) -> Comparison, v: MutList[a, r]): Option[a] \ Read(r) =
        reduceLeft(Order.maxBy(cmp), v)

    ///
    /// Returns the number of elements in the given mutable list `v` that satisfies the given predicate `f`.
    ///
    /// Returns `0` if the given mutable list `v` is empty.
    ///
    pub def count(f: a -> Bool \ ef, v: MutList[a, r]): Int32 \ { ef, Read(r) } =
        foldLeft((acc, x) -> if (f(x)) acc + 1 else acc, 0, v)

    ///
    /// Returns the sum of all elements in the MutList `v`.
    ///
    pub def sum(v: MutList[Int32, r]): Int32 \ Read(r) =
        foldLeft((acc, x) -> acc + x, 0, v)

    ///
    /// Returns the sum of all elements in the MutList `v` according to the function `f`.
    ///
    pub def sumWith(f: a -> Int32 \ ef, v: MutList[a, r]): Int32 \ { ef, Read(r) } =
        foldLeft((acc, x) -> acc + f(x), 0, v)

    ///
    /// Returns `true` if the given predicate `f` holds for at least one element of the given mutable list `v`.
    ///
    /// Returns `false` if the given mutable list `v` is empty.
    ///
    pub def exists(f: a -> Bool \ ef, v: MutList[a, r]): Bool \ { ef, Read(r) } =
        let MutList(_, ra, rl) = v;
        let a = deref ra;
        let l = deref rl;
        def loop(i) = {
            if (i >= l)
                false
            else
                if (f(Array.get(i, a))) true else loop(i + 1)
        };
        loop(0)

    ///
    /// Returns `true` if the given predicate `f` holds for all elements of the given mutable list `v`.
    ///
    /// Returns `true` if the given mutable list `v` is empty.
    ///
    pub def forAll(f: a -> Bool \ ef, v: MutList[a, r]): Bool \ { ef, Read(r) } =
        let MutList(_, ra, rl) = v;
        let a = deref ra;
        let l = deref rl;
        def loop(i) = {
            if (i >= l)
                true
            else
                if (f(Array.get(i, a))) loop(i + 1) else false
        };
        loop(0)

    ///
    /// Optionally returns the first element of the given mutable list `v`.
    ///
    /// Returns `None` if the given mutable list `v` is empty.
    ///
    pub def head(v: MutList[a, r]): Option[a] \ Read(r) =
        let MutList(_, a, _) = v;
        if (isEmpty(v))
            None
        else
            Array.head(deref a)

    ///
    /// Optionally returns the last element of the given mutable list `v`.
    ///
    /// Returns `None` if the given mutable list `v` is empty.
    ///
    pub def last(v: MutList[a, r]): Option[a] \ Read(r) =
        let MutList(_, a, l) = v;
        let len = deref l;
        if (len > 0) Some(Array.get(len - 1, deref a)) else None

    ///
    /// Alias for `IndexOfLeft`
    ///
    pub def indexOf(x: a, v: MutList[a, r]): Option[Int32] \ Read(r) with Eq[a] =
        indexOfLeft(x, v)

    ///
    /// Optionally returns the position of the first occurrence of `x` in `v`
    /// searching from left to right.
    ///
    pub def indexOfLeft(x: a, v: MutList[a, r]): Option[Int32] \ Read(r) with Eq[a] =
        let MutList(_, ra, rl) = v;
        let a = deref ra;
        let l = deref rl;
        def loop(i) = {
            if (i >= l)
                None
            else
                if (x == Array.get(i, a)) Some(i) else loop(i + 1)
        };
        loop(0)

    ///
    /// Optionally returns the position of the first occurrence of `x` in `v`
    /// searching from right to left.
    ///
    pub def indexOfRight(x: a, v: MutList[a, r]): Option[Int32] \ Read(r) with Eq[a] =
        let MutList(_, ra, rl) = v;
        let a = deref ra;
        let l = deref rl;
        def loop(i) = {
            if (i < 0)
                None
            else
                if (x == Array.get(i, a)) Some(i) else loop(i - 1)
        };
        loop(l - 1)

    ///
    /// Alias for `findLeft`.
    ///
    pub def find(f: a -> Bool, v: MutList[a, r]): Option[a] \ Read(r) =
        findLeft(f, v)

    ///
    /// Optionally returns the left-most element in the given mutable list `v` that satisfies the given predicate `f`.
    ///
    /// Returns `None` if no element satisfies the given predicate `f`.
    /// Returns `None` if the given mutable list `v` is empty.
    ///
    pub def findLeft(f: a -> Bool, v: MutList[a, r]): Option[a] \ Read(r) =
        let MutList(_, ra, rl) = v;
        let a = deref ra;
        let l = deref rl;
        def loop(i) = {
            if (i >= l)
                None
            else
                if (f(Array.get(i, a))) Some(Array.get(i, a)) else loop(i + 1)

        };
        loop(0)

    ///
    /// Optionally returns the right-most element in the given mutable list `v` that satisfies the given predicate `f`.
    ///
    /// Returns `None` if no element satisfies the given predicate `f`.
    /// Returns `None` if the given mutable list `v` is empty.
    ///
    pub def findRight(f: a -> Bool, v: MutList[a, r]): Option[a] \ Read(r) =
        let MutList(_, ra, rl) = v;
        let a = deref ra;
        let l = deref rl;
        def loop(i) = {
            if (i < 0)
                None
            else
                if (f(Array.get(i, a))) Some(Array.get(i, a)) else loop(i - 1)
        };
        loop(l - 1)

    ///
    /// Alias for `scanLeft`.
    ///
    pub def scan(r1: Region[r1], f: (b, a) -> b \ ef, s: b, v: MutList[a, r2]): MutList[b, r1] \ { ef, Read(r2), Write(r1) } =
        scanLeft(r1, f, s, v)

    ///
    /// Accumulates the result of applying `f` to `v` going left to right.
    ///
    pub def scanLeft(r1: Region[r1], f: (b, a) -> b \ ef, s: b, v: MutList[a, r2]): MutList[b, r1] \ { ef, Read(r2), Write(r1) } =
        let MutList(_, ra, rl) = v;
        let a = deref ra;
        let l = deref rl;
        let n = l + 1;
        let b = Array.repeat(r1, n, s);
        def loop(i, acc) = {
            if (i >= n)
                ()
            else {
                let s1 = f(acc, Array.get(i - 1, a));
                Array.put(s1, i, b);
                loop(i + 1, s1)
            }
        };
        loop(1, s);
        MutList(r1, ref b @ r1, ref n @ r1)

    ///
    /// Accumulates the result of applying `f` to `v` going right to left.
    ///
    pub def scanRight(r1: Region[r1], f: (a, b) -> b \ ef, s: b, v: MutList[a, r2]): MutList[b, r1] \ { ef, Read(r2), Write(r1) } =
        let MutList(_, ra, rl) = v;
        let a = deref ra;
        let l = deref rl;
        let n = l + 1;
        let b = Array.repeat(r1, n, s);
        def loop(i, acc) = {
            if (i < 0)
                ()
            else {
                let s1 = f(Array.get(i, a), acc);
                Array.put(s1, i, b);
                loop(i - 1, s1)
            }
        };
        loop(l - 1, s);
        MutList(r1, ref b @ r1, ref n @ r1)

    ///
    /// Apply `f` to every element in `v`.
    ///
    /// The result is a new mutable list.
    ///
    pub def map(r1: Region[r1], f: a -> b \ ef, v: MutList[a, r]): MutList[b, r1] \ { ef, Read(r), Write(r1) } =
        if (isEmpty(v))
            MutList.new(r1)
        else {
            let MutList(_, ra, rl) = v;
            let a = deref ra;
            let l = deref rl;
            let x = f(Array.get(0, a));
            let b = Array.repeat(r1, Array.length(a), x);
            def loop(i) = {
                if (i >= l)
                    ()
                else {
                    Array.put(f(Array.get(i, a)), i, b);
                    loop(i + 1)
                }
            };
            loop(1);
            MutList(r1, ref b @ r1, ref l @ r1)
        }

    ///
    /// Returns the result of applying `f` to every element in `v` along with that element's index.
    ///
    pub def mapWithIndex(r1: Region[r1], f: (Int32, a) -> b \ ef, v: MutList[a, r]): MutList[b, r1] \ { ef, Read(r), Write(r1) } =
        if (isEmpty(v))
            MutList.new(r1)
        else {
            let MutList(_, ra, rl) = v;
            let a = deref ra;
            let l = deref rl;
            let x = f(0, Array.get(0, a));
            let b = Array.repeat(r1, Array.length(a), x);
            def loop(i) = {
                if (i >= l)
                    ()
                else {
                    Array.put(f(i, Array.get(i, a)), i, b);
                    loop(i + 1)
                }
            };
            loop(1);
            MutList(r1, ref b @ r1, ref l @ r1)
        }

    ///
    /// Apply `f` to every element in `v`.
    ///
    pub def transform!(f: a -> a, v: MutList[a, r]): Unit \ { Read(r), Write(r) } =
        let MutList(_, ra, rl) = v;
        let a = deref ra;
        let l = deref rl;
        def loop(i) = {
            if (i >= l)
                ()
            else {
                Array.put(f(Array.get(i, a)), i, a);
                loop(i + 1)
            }
        };
        loop(0)

    ///
    /// Apply `f` to every element in `v` along with that element's index.
    ///
    pub def transformWithIndex!(f: (Int32, a) -> a, v: MutList[a, r]): Unit \ Read(r) =
        let MutList(_, ra, rl) = v;
        let a = deref ra;
        let l = deref rl;
        def loop(i) = {
            if (i >= l)
                ()
            else {
                Array.put(f(i, Array.get(i, a)), i, a);
                loop(i + 1)
            }
        };
        loop(0)

    ///
    /// Applies `f` to a start value `s` and all elements in `a` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, a[0]), a[1])..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b \ ef, s: b, v: MutList[a, r]): b \ { ef, Read(r) } =
        let MutList(_, ra, rl) = v;
        let a = deref ra;
        let l = deref rl;
        def loop(i, acc) = {
            if (i >= l)
                acc
            else {
                let s1 = f(acc, Array.get(i, a));
                loop(i + 1, s1)
            }
        };
        loop(0, s)

    ///
    /// Applies `f` to a start value `s` and all elements in `a` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, a[0]), a[1])..., xn)`.
    ///
    /// The implementation is tail recursive.
    ///
    pub def foldRight(f: (a, b) -> b \ ef, s: b, v: MutList[a, r]): b \ { ef, Read(r) } =
        let MutList(_, ra, rl) = v;
        let a = deref ra;
        let l = deref rl;
        def loop(i, acc) = {
            if (i < 0)
                acc
            else {
                let s1 = f(Array.get(i, a), acc);
                loop(i - 1, s1)
            }
        };
        loop(l - 1, s)

    ///
    /// Applies `f` to a start value `z` and all elements in `a` going from left to right.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, z))...)`.
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (a, Unit -> b \ {ef, r}) -> b \ {ef, r}, z: b, v: MutList[a, r]): b \ { ef, Read(r) } =
        let MutList(_, ra, rl) = v;
        let a = deref ra;
        let l = deref rl;
        def loop(i) = {
            if (i >= l)
                z
            else {
                f(Array.get(i, a), _ -> loop(i + 1))
            }
        };
        loop(0)

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap(f: a -> b \ ef, v: MutList[a, r]): b \ {ef, Read(r)} with Monoid[b] =
        foldLeft((acc, x) -> Monoid.combine(acc, f(x)), Monoid.empty(), v)

    ///
    /// Applies `f` to all elements in `v` going from left to right until a single value `v` is obtained. Returns `Some(v)`.
    ///
    /// Returns `None` if `v` is empty.
    ///
    pub def reduceLeft(f: (a, a) -> a \ ef, v: MutList[a, r]): Option[a] \ { ef, Read(r) } =
        foldLeft((acc, x) -> match acc {
            case Some(y) => Some(f(y, x))
            case None => Some(x)
        }, None, v)

    ///
    /// Applies `f` to all elements in `v` going from right to left until a single value `v` is obtained. Returns `Some(v)`.
    ///
    /// Returns `None` if `v` is empty.
    ///
    pub def reduceRight(f: (a, a) -> a \ ef, v: MutList[a, r]): Option[a] \ { ef, Read(r) } =
        foldRight((x, acc) -> match acc {
            case Some(y) => Some(f(x, y))
            case None => Some(x)
        }, None, v)

    ///
    /// Removes all elements from the given mutable list `v`.
    ///
    pub def clear!(v: MutList[a, r]): Unit \ { Read(r), Write(r) } =
        let MutList(r, a, l) = v;
        a := Array.new(r, Array.length(deref a));
        l := 0

    ///
    /// Returns a shallow copy of the given mutable list `v`.
    /// The capacity of the copy is equal to the length of the list.
    ///
    pub def copy(r1: Region[r1], v: MutList[a, r]): MutList[a, r1] \ { Read(r), Write(r1) } =
        let MutList(_, a, l) = v;
        let len = deref l;
        if (len > minCapacity())
            MutList(r1, ref Array.copyOfRange(r1, 0, len, deref a) @ r1, ref len @ r1)
        else
            MutList(r1, ref Array.copyOfRange(r1, 0, capacity(v), deref a) @ r1, ref len @ r1)

    ///
    /// Optionally removes and returns the last element in the given mutable list `v`.
    ///
    pub def pop!(v: MutList[a, r]): Option[a] \ { Read(r), Write(r) } =
        let MutList(_, a, l) = v;
        let len = deref l;
        if (len > 0)
            let last = Array.get(len - 1, deref a);
            l := len - 1;
            Array.put(Reflect.default(), len - 1, deref a);
            compress!(v);
            Some(last)
        else
            None

    ///
    /// Inserts the given element `x` at the end of the given mutable list `v`.
    ///
    pub def push!(x: a, v: MutList[a, r]): Unit \ { Read(r), Write(r) } =
        let MutList(_, a, l) = v;
        let len = deref l;
        if (capacity(v) - len == 0)
            reserve!(len, v)
        else ();
        Array.put(x, len, deref a);
        l := len + 1

    ///
    /// Inserts the given element `x` at the given position `i` in the given mutable list `v`.
    ///
    /// Shifts elements as necessary. Possibly expensive operation.
    ///
    /// If the given index `i` exceeds the length of the mutable list, the element is inserted at the last position.
    ///
    pub def insert!(x: a, i: Int32, v: MutList[a, r]): Unit \ { Read(r), Write(r) } =
        let MutList(r, a, l) = v;
        let len = deref l;
        if (capacity(v) - len == 0)
            reserve!(len, v)
        else ();
        let sub = Array.copyOfRange(r, i, len, deref a);
        Array.updateSequence!(i + 1, sub, deref a);
        Array.put(x, i, deref a);
        l := len + 1

    ///
    /// Removes the element at the given position `i` in the given mutable list `v`.
    ///
    /// Shifts elements as necessary. Possibly expensive operation.
    ///
    /// If the given index `i` exceeds the length of the mutable list, no element is removed.
    ///
    pub def remove!(i: Int32, v: MutList[a, r]): Unit \ { Read(r), Write(r) } =
        let MutList(_, ra, rl) = v;
        let a = deref ra;
        let l = deref rl;
        let n = l - 1;
        def loop(i1) = {
            if (i1 < n) {
                Array.put(Array.get(i1 + 1, a), i1, a);
                loop(i1 + 1)
            }
            else if (i1 == n) {
                Array.put(Reflect.default(), i1, a)
            }
            else
                ()
        };
        if (i < l) {
            loop(i);
            rl := n;
            compress!(v)
        }
        else
            ()

    ///
    /// Appends `m` to `v` i.e. inserts all elements from `m` into the end of `v`.
    ///
    pub def pushAll!(m: m[a], v: MutList[a, r]): Unit \ { Read(r), Write(r) } with Foldable[m] =
        Foldable.forEach(x -> MutList.push!(x, v), m)

    ///
    /// Appends `m` to `v` i.e. inserts all elements from `m` into the end of `v`.
    ///
    pub def append!(m: m[a], v: MutList[a, r]): Unit \ { Read(r), Write(r) } with Foldable[m] =
        pushAll!(m, v)

    ///
    /// Removes all elements from the given mutable list `v` that do not satisfy the given predicate `f`.
    ///
    pub def retain!(f: a -> Bool, v: MutList[a, r]): Unit \ { Read(r), Write(r) } =
        let MutList(r, a1, l1) = v;
        let l = MutList.new(r);
        forEach(e -> if (f(e)) push!(e, l) else (), v);
        let MutList(_, a2, l2) = l;
        a1 := deref a2;
        l1 := deref l2

    ///
    /// Replaces all occurrences of the `from` with `to` in the given mutable list `v`.
    ///
    pub def replace!(from: {from = a}, to: {to = a}, v: MutList[a, r]): Unit \ { Read(r), Write(r) } with Eq[a] =
        transform!(e -> if (e == from.from) to.to else e, v)

    ///
    /// Reverses the order of the elements in the given mutable list `v`.
    ///
    pub def reverse!(v: MutList[a, r]): Unit \ { Read(r), Write(r) } =
        let MutList(_, ra, rl) = v;
        let a = deref ra;
        let l = deref rl;
        let halflen = l / 2;
        def loop(i, j) = {
            if (i >= halflen)
                ()
            else {
                let x = Array.get(i, a);
                let y = Array.get(j, a);
                Array.put(y, i, a);
                Array.put(x, j, a);
                loop(i + 1, j - 1)
            }
        };
        loop(0, l - 1)

    ///
    /// Shrinks the given mutable list `v` down to a capacity of `n` elements but no less than 8.
    ///
    /// Truncates the mutable list as needed.
    ///
    def shrinkTo!(n: Int32, v: MutList[a, r]): Unit \ { Read(r), Write(r) } =
        let minCap = minCapacity();
        let capv = capacity(v);
        let MutList(r, a, l) = v;
        if (n < capv and capv != minCap) {
            let len = deref l;
            let newCap = Order.max(n, minCap);
            a := Array.copyOfRange(r, 0, newCap, deref a);
            l := Order.min(len, newCap)
        }
        else
            ()

    ///
    /// Shrinks the given mutable list `v` to its actual size.
    ///
    pub def shrink!(v: MutList[a, r]): Unit \ { Read(r), Write(r) } =
        shrinkTo!(length(v), v)

    ///
    /// Truncates the given mutable list `v` to the given length `l`.
    ///
    /// That is, after the operation, the mutable list has length at most `l`.
    ///
    /// If the given length `l` is negative, all elements are removed.
    ///
    pub def truncate!(l: Int32, v: MutList[a, r]): Unit \ { Read(r), Write(r) } =
        if (l < 0)
            clear!(v)
        else if (l < length(v)) {
            let MutList(r, a, l0) = v;
            let minCap = minCapacity();
            let c = Order.max(l, minCap);
            l0 := l;
            Array.updateSequence!(0, deref a, Array.new(r, c))
        }
        else
            ()

    ///
    /// Increases the capacity of the given mutable list `v` by at least `n`.
    ///
    /// That is, after the call, the mutable list is guaranteed to have space for at least `n` additional elements.
    ///
    /// The content of the mutable list is unchanged.
    ///
    pub def reserve!(n: Int32, v: MutList[a, r]): Unit \ { Read(r), Write(r) } =
        let MutList(r, a, l) = v;
        a := Array.copyOfRange(r, 0, (deref l) + n, deref a)

    ///
    /// Returns `v` as an immutable list.
    ///
    pub def toList(v: MutList[a, r]): List[a] \ Read(r) =
        foldRight((x, acc) -> x :: acc, Nil, v)

    ///
    /// Returns `v` as an array.
    ///
    pub def toArray(r1: Region[r1], v: MutList[a, r2]): Array[a, r1] \ { Read(r2), Write(r1) } =
        let MutList(_, a, l) = v;
        Array.copyOfRange(r1, 0, deref l, deref a)

    ///
    /// Returns `xs` as a vector.
    ///
    pub def toVector(xs: MutList[a, r]): Vector[a] \ Read(r) = region rc {
        let arr = Array.new(rc, length(xs));
        forEachWithIndex((i, x) -> Array.put(x, i, arr), xs);
        Array.toVector(arr)
    }

    ///
    /// Returns the mutable list `xs` as a chain.
    ///
    pub def toChain(xs: MutList[a, r]): Chain[a] \ Read(r) =
        foldLeft((ac, x) -> Chain.snoc(ac, x), Chain.empty(), xs)

    ///
    /// Returns `v` as a MutDeque.
    ///
    pub def toMutDeque(r1: Region[r1], v: MutList[a, r2]): MutDeque[a, r1] \ { Read(r2), Write(r1) }  =
        let d = MutDeque.new(r1);
        forEach(x -> MutDeque.pushBack(x, d), v);
        d

    ///
    /// Returns `true` if the mutable lists `v1` and `v2` have the same elements in the same order, i.e. are structurally equal.
    ///
    pub def sameElements(v1: MutList[a, r1], v2: MutList[a, r2]): Bool \ Read(r1, r2) with Eq[a] =
        let MutList(_, ra1, rl1) = v1;
        let MutList(_, ra2, rl2) = v2;
        let a1 = deref ra1;
        let a2 = deref ra2;
        let l1 = deref rl1;
        let l2 = deref rl2;
        def loop(i) = {
            if (i >= l1)
                true
            else if (Array.get(i, a1) == Array.get(i, a2))
                loop(i + 1)
            else
                false
        };
        if (l1 == l2) loop(0) else false

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `v` with `sep` inserted between each element.
    ///
    pub def join(sep: String, v: MutList[a, r]): String \ Read(r) with ToString[a] =
        joinWith(ToString.toString, sep, v)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `v` according to `f` with `sep` inserted between each element.
    ///
    pub def joinWith(f: a -> String \ ef, sep: String, v: MutList[a, r]): String \ { ef, Read(r) } = region rc1 {
        let sb = StringBuilder.new(rc1);
        let step = (i, x) ->
            if (i == 0)
                StringBuilder.appendString!(f(x), sb)
            else {
                StringBuilder.appendString!(sep, sb);
                StringBuilder.appendString!(f(x), sb)
            };
        forEachWithIndex(step, v);
        StringBuilder.toString(sb)
    }

    ///
    /// Returns an iterator over `l`.
    ///
    /// Modifying `l` while using an iterator has undefined behavior and is dangerous.
    ///
    pub def iterator(rc: Region[r1], l: MutList[a, r2]): Iterator[a, r1 and r2, r1] \ { Write(r1), Read(r2) } =
        let MutList(_, a, len) = l;
        let len1 = deref len;
        let ix = ref 0 @ rc;
        let next = () -> {
            let i = deref ix;
            if (i < len1) {
                let x = Array.get(i, deref a);
                ix := i + 1;
                Some(x)
            } else {
                None
            }
        };
        Iterator.iterate(rc, next)

    ///
    /// Applies `f` to all the elements in `v`.
    ///
    pub def forEach(f: a -> Unit \ ef, v: MutList[a, r]): Unit \ { ef, Read(r) } =
        let MutList(_, ra, rl) = v;
        let a = deref ra;
        let l = deref rl;
        def loop(i) = {
            if (i >= l)
                ()
            else {
                f(Array.get(i, a));
                loop(i + 1)
            }
        };
        loop(0)

    ///
    /// Applies `f` to all the elements in `v` along with that element's index.
    ///
    pub def forEachWithIndex(f: (Int32, a) -> Unit \ ef, v: MutList[a, r]): Unit \ { ef, Read(r) } =
        let MutList(_, ra, rl) = v;
        let a = deref ra;
        let l = deref rl;
        def loop(i) = {
            if (i >= l)
                ()
            else {
                f(i, Array.get(i, a));
                loop(i + 1)
            }
        };
        loop(0)

    ///
    /// Compresses the given mutable list `v` if needed.
    ///
    /// The mutable list will be shrunk to 1/2 of its size if the load factor is less than 1/4.
    ///
    pub def compress!(v: MutList[a, r]): Unit \ { Read(r), Write(r) } =
        let c = capacity(v);
        let len = length(v);
        let loadFactor = Int32.toFloat32(len) / Int32.toFloat32(c);
        if (loadFactor < 1.0f32/4.0f32 and len > 0) {
            if (len == 1)
                shrinkTo!(1, v)
            else
                shrinkTo!(c / 2, v)
        }
        else
            ()

    ///
    /// Returns the capacity of `v`.
    ///
    def capacity(v: MutList[a, r]): Int32 \ Read(r) =
        let MutList(_, a, _) = v;
        Array.length(deref a)

    ///
    /// Returns a sorted copy of MutList `v`, where the elements are ordered from low to high according to
    /// their `Order` instance.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `v`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sort(r1: Region[r1], v: MutList[a, r]): MutList[a, r1] \ { Read(r), Write(r1) } with Order[a] =
        sortWith(r1, Order.compare, v)

    ///
    /// Returns a sorted copy of MutList `v`, where the elements are ordered from low to high according to
    /// the `Order` instance for the values obtained by applying `f` to each element.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `v`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortBy(r1: Region[r1], f: a -> b, v: MutList[a, r]): MutList[a, r1] \ { Read(r), Write(r1) } with Order[b] =
        sortWith(r1, Order.compare `on` f, v)

    ///
    /// Returns a sorted copy of MutList `v`, where the elements are ordered from low to high according to
    /// the comparison function `cmp`.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `v`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortWith(r1: Region[r1], cmp: (a,a) -> Comparison, v: MutList[a, r]): MutList[a, r1] \ { Read(r), Write(r1) } =
        let vCopy = copy(r1, v);
        sortWith!(cmp, vCopy);
        vCopy

    ///
    /// Sort MutList `v` so that elements are ordered from low to high according to their `Order` instance.
    /// The MutList is mutated in-place.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `v`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sort!(v: MutList[a, r]): Unit \ { Read(r), Write(r) } with Order[a] =
        sortWith!(Order.compare, v)

    ///
    /// Sort MutList `v` so that elements are ordered from low to high according to the `Order` instance for
    /// the values obtained by applying `f` to each element. The MutList is mutated in-place.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `v`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortBy!(f: a -> b, v: MutList[a, r]): Unit \ { Read(r), Write(r) } with Order[b] =
        sortWith!(Order.compare `on` f, v)

    ///
    /// Sort MutList `v` so that elements are ordered from low to high according to the comparison function `cmp`.
    /// The MutList is mutated in-place.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `v`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortWith!(cmp: (a,a) -> Comparison, v: MutList[a, r]): Unit \ { Read(r), Write(r) } =
        let MutList(_, a, l) = v;
        Array.sortWithin!(cmp, 0, (deref l) - 1, deref a)

    ///
    /// Shuffles `v` using the Fisher–Yates shuffle.
    ///
    pub def shuffle(r1: Region[r1], rnd: Random, v: MutList[a, r1]): MutList[a, r1] \ { Read(r1), Write(r1), NonDet } = region rc2 {
        toArray(rc2, v) !> Array.shuffle(rnd) |> Array.toMutList(r1)
    }

}
