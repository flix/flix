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
pub enum MutList[a] {
    case MutList(Ref[Array[a]], Ref[Int32])
}

instance ToString[MutList[a]] with ToString[a] {
    pub def toString(v: MutList[a]): String = {
        let s = MutList.formatWith(x -> "${x}", ", ", v) as & Pure;
        "MutList#{${s}}"
    }
}

instance Eq[MutList[a]] with Eq[a] {
    pub def eq(xs: MutList[a], ys: MutList[a]): Bool =
        (let MutList(a, _) = xs;
        let MutList(b, _) = ys;
        deref a == deref b) as & Pure
}

instance Order[MutList[v]] with Order[v] {
    ///
    /// Compares `u` and `v` lexicographically.
    ///
    pub def compare(u: MutList[v], v: MutList[v]): Comparison =
        (let MutList(a, _) = u;
        let MutList(b, _) = v;
        deref a <=> deref b) as & Pure
}

instance Functor[MutList] {
    pub def map(f: a -> b & e, v: MutList[a]): MutList[b] & e = MutList.map(f, v) as & e
}

instance Foldable[MutList] {
    pub def foldLeft(f: (b, a) -> b & e, s: b, v: MutList[a]): b & e = MutList.foldLeft(f, s, v) as & e
    pub def foldRight(f: (a, b) -> b & e, s: b, v: MutList[a]): b & e = MutList.foldRight(f, s, v) as & e
}

namespace MutList {

    def minCapacity(): Int32 = 8

    ///
    /// Returns a new empty mutable list with a default capacity.
    ///
    pub def new(): MutList[a] & Impure = MutList(ref [default; minCapacity()], ref 0)

    ///
    /// Returns a mutable list of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns an empty mutable list if `b >= e`.
    ///
    pub def range(b: Int32, e: Int32): MutList[Int32] & Impure =
        let minCap = minCapacity();
        if (e - b < minCap)
            let f = i -> if (b + i < e) b + i else default;
            MutList(ref Array.init(f, minCap), ref (e - b))
        else
            MutList(ref Array.range(b, e), ref (e - b))

    ///
    /// Returns the element at position `i` in the mutable list `v`.
    ///
    pub def get(i: Int32, v: MutList[a]): a & Impure =
        let MutList(a, _) = v;
        Array.get(deref a, i)

    ///
    /// Returns the number of elements in the given mutable list `v`.
    ///
    pub def length(v: MutList[a]): Int32 & Impure =
        let MutList(_, l) = v;
        deref l

    ///
    /// Returns `true` if the given mutable list `v` is empty.
    ///
    pub def isEmpty(v: MutList[a]): Bool & Impure = length(v) == 0

    ///
    /// Returns `true` if the given element `x` is a member of the given mutable list `v`.
    ///
    pub def memberOf(x: a, v: MutList[a]): Bool & Impure with Eq[a] = exists(y -> y == x, v)

    ///
    /// Optionally finds the smallest element of `v` according to the `Order` on `a`.
    ///
    /// Returns `None` if `v` is empty.
    ///
    pub def minimum(v: MutList[a]): Option[a] & Impure with Order[a] =
        reduceLeft(Order.min, v)

    ///
    /// Optionally finds the smallest element of `v` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `v` is empty.
    ///
    pub def minimumBy(cmp: (a, a) -> Comparison, v: MutList[a]): Option[a] & Impure =
        reduceLeft(Order.minBy(cmp), v)

    ///
    /// Optionally finds the largest element of `v` according to the `Order` on `a`.
    ///
    /// Returns `None` if `v` is empty.
    ///
    pub def maximum(v: MutList[a]): Option[a] & Impure with Order[a] =
        reduceLeft(Order.max, v)

    ///
    /// Optionally finds the largest element of `v` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `v` is empty.
    ///
    pub def maximumBy(cmp: (a, a) -> Comparison, v: MutList[a]): Option[a] & Impure =
        reduceLeft(Order.maxBy(cmp), v)

    ///
    /// Returns the number of elements in the given mutable list `v` that satisfies the given predicate `f`.
    ///
    /// Returns `0` if the given mutable list `v` is empty.
    ///
    pub def count(f: a -> Bool, v: MutList[a]): Int32 & Impure =
        foldLeft((acc, x) -> if (f(x)) acc + 1 else acc, 0, v)

    ///
    /// Returns `true` if the given predicate `f` holds for at least one element of the given mutable list `v`.
    ///
    /// Returns `false` if the given mutable list `v` is empty.
    ///
    pub def exists(f: a -> Bool, v: MutList[a]): Bool & Impure =
        let MutList(a, l) = v;
        existsHelper(f, deref a, deref l, 0)

    ///
    /// Helper function for `exists`.
    ///
    def existsHelper(f: a -> Bool, a: Array[a], len: Int32, i: Int32): Bool & Impure =
        if (i >= len)
            false
        else
            f(a[i]) or existsHelper(f, a, len, i + 1)

    ///
    /// Returns `true` if the given predicate `f` holds for all elements of the given mutable list `v`.
    ///
    /// Returns `true` if the given mutable list `v` is empty.
    ///
    pub def forall(f: a -> Bool, v: MutList[a]): Bool & Impure =
        let MutList(a, l) = v;
        forallHelper(f, deref a, deref l, 0)

    ///
    /// Helper function for `forall`.
    ///
    def forallHelper(f: a -> Bool, a: Array[a], len: Int32, i: Int32): Bool & Impure =
        if (i >= len)
            true
        else
            f(a[i]) and existsHelper(f, a, len, i + 1)

    ///
    /// Optionally returns the first element of the given mutable list `v`.
    ///
    /// Returns `None` if the given mutable list `v` is empty.
    ///
    pub def head(v: MutList[a]): Option[a] & Impure =
        let MutList(a, _) = v;
        if (isEmpty(v))
            None
        else
            Array.head(deref a)

    ///
    /// Optionally returns the last element of the given mutable list `v`.
    ///
    /// Returns `None` if the given mutable list `v` is empty.
    ///
    pub def last(v: MutList[a]): Option[a] & Impure =
        let MutList(a, l) = v;
        let len = deref l;
        if (len > 0) Some((deref a)[len - 1]) else None

    ///
    /// Alias for `IndexOfLeft`
    ///
    pub def indexOf(x: a, v: MutList[a]): Option[Int32] & Impure with Eq[a] = indexOfLeft(x, v)

    ///
    /// Optionally returns the position of the first occurrence of `x` in `v`
    /// searching from left to right.
    ///
    pub def indexOfLeft(x: a, v: MutList[a]): Option[Int32] & Impure with Eq[a] =
        let MutList(a, l) = v;
        indexOfLeftHelper(x, deref a, deref l, 0)

    ///
    /// Helper function for `indexOfLeft`.
    ///
    def indexOfLeftHelper(x: a, a: Array[a], len: Int32, i: Int32): Option[Int32] & Impure with Eq[a] =
        if (i >= len)
            None
        else
            if (x == a[i]) Some(i) else indexOfLeftHelper(x, a, len, i + 1)

    ///
    /// Optionally returns the position of the first occurrence of `x` in `v`
    /// searching from right to left.
    ///
    pub def indexOfRight(x: a, v: MutList[a]): Option[Int32] & Impure with Eq[a] =
        let MutList(a, l) = v;
        indexOfRightHelper(x, deref a, (deref l) - 1)

    ///
    /// Helper function for `indexOfRight`.
    ///
    def indexOfRightHelper(x: a, a: Array[a], i: Int32): Option[Int32] & Impure with Eq[a] =
        if (i < 0)
            None
        else
            if (x == a[i]) Some(i) else indexOfRightHelper(x, a, i - 1)

    ///
    /// Alias for `findLeft`.
    ///
    pub def find(f: a -> Bool, v: MutList[a]): Option[a] & Impure = findLeft(f, v)

    ///
    /// Optionally returns the left-most element in the given mutable list `v` that satisfies the given predicate `f`.
    ///
    /// Returns `None` if no element satisfies the given predicate `f`.
    /// Returns `None` if the given mutable list `v` is empty.
    ///
    pub def findLeft(f: a -> Bool, v: MutList[a]): Option[a] & Impure =
        let MutList(a, l) = v;
        findLeftHelper(f, deref a, deref l, 0)

    ///
    /// Helper function for `findLeft`.
    ///
    def findLeftHelper(f: a -> Bool, a: Array[a], len: Int32, i: Int32): Option[a] & Impure =
        if (i >= len)
            None
        else
            if (f(a[i])) Some(a[i]) else findLeftHelper(f, a, len, i + 1)

    ///
    /// Optionally returns the right-most element in the given mutable list `v` that satisfies the given predicate `f`.
    ///
    /// Returns `None` if no element satisfies the given predicate `f`.
    /// Returns `None` if the given mutable list `v` is empty.
    ///
    pub def findRight(f: a -> Bool, v: MutList[a]): Option[a] & Impure =
        let MutList(a, l) = v;
        findRightHelper(f, deref a, (deref l) - 1)

    ///
    /// Helper function for `findRight`.
    ///
    def findRightHelper(f: a -> Bool, a: Array[a], i: Int32): Option[a] & Impure =
        if (i < 0)
            None
        else
            if (f(a[i])) Some(a[i]) else findRightHelper(f, a, i - 1)

    ///
    /// Alias for `scanLeft`.
    ///
    pub def scan(f: (b, a) ->b & e, s: b, v: MutList[a]): MutList[b] & Impure = scanLeft(f, s, v)

    ///
    /// Accumulates the result of applying `f` to `v` going left to right.
    ///
    pub def scanLeft(f: (b, a) -> b & e, s: b, v: MutList[a]): MutList[b] & Impure =
        let MutList(a, l) = v;
        let len = deref l;
        let b = [s; len + 1];
        scanLeftHelper(f, s, deref a, b, 1, len + 1);
        MutList(ref b, ref (len + 1))

    ///
    /// Helper function for `scanLeft`.
    ///
    def scanLeftHelper(f: (b, a) -> b & e, s: b, a: Array[a], b: Array[b], i: Int32, n: Int32): Unit & Impure =
        if (i >= n)
            ()
        else {
            let s1 = f(s, a[i - 1]);
            b[i] = s1;
            scanLeftHelper(f, s1, a, b, i + 1, n)
        }

    ///
    /// Accumulates the result of applying `f` to `v` going right to left.
    ///
    pub def scanRight(f: (a, b) -> b & e, s: b, v: MutList[a]): MutList[b] & Impure =
        let MutList(a, l) = v;
        let len = deref l;
        let b = [s; len + 1];
        scanRightHelper(f, s, deref a, b, len - 1);
        MutList(ref b, ref (len + 1))

    ///
    /// Helper function for `scanRight`.
    ///
    def scanRightHelper(f: (a, b) -> b & e, s: b, a: Array[a], b: Array[b], i: Int32): Unit & Impure =
        if (i < 0)
            ()
        else {
            let s1 = f(a[i], s);
            b[i] = s1;
            scanRightHelper(f, s1, a, b, i - 1)
        }

    ///
    /// Apply `f` to every element in `v`.
    ///
    /// The result is a new mutable list.
    ///
    pub def map(f: a -> b & e, v: MutList[a]): MutList[b] & Impure =
        let MutList(a, l) = v;
        let len = deref l;
        MutList(ref Array.init(i -> f((deref a)[i]), len), ref len)

    ///
    /// Apply `f` to every element in `v`.
    ///
    pub def transform!(f: a -> a, v: mut MutList[a]): Unit & Impure =
        let MutList(a, l) = v;
        transformHelper!(f, deref a, 0, deref l)

    ///
    /// Helper function for `transform!`
    ///
    def transformHelper!(f: a -> a, a: Array[a], i: Int32, n: Int32): Unit & Impure =
        if (i >= n)
            ()
        else {
            a[i] = f(a[i]);
            transformHelper!(f, a, i + 1, n)
        }

    ///
    /// Returns the result of applying `f` to every element in `v` along with that element's index.
    ///
    pub def mapWithIndex(f: (a, Int32) -> b & e, v: MutList[a]): MutList[b] & Impure =
        let MutList(a, l) = v;
        let len = deref l;
        MutList(ref Array.init(i -> f((deref a)[i], i), len), ref len)

    ///
    /// Apply `f` to every element in `v` along with that element's index.
    ///
    pub def transformWithIndex!(f: (a, Int32) -> a, v: mut MutList[a]): Unit & Impure =
        let MutList(a, l) = v;
        transformWithIndexHelper!(f, deref a, 0, deref l)

    ///
    /// Helper function for `transformWithIndex!`
    ///
    def transformWithIndexHelper!(f: (a, Int32) -> a, a: Array[a], i: Int32, n: Int32): Unit & Impure =
        if (i >= n)
            ()
        else {
            a[i] = f(a[i], i);
            transformWithIndexHelper!(f, a, i + 1, n)
        }

    ///
    /// Returns the result of applying `combine` to all the elements in `v`, using `empty` as the initial value.
    ///
    pub def fold(v: MutList[a]): a with Monoid[a] = Foldable.fold(v)

    ///
    /// Applies `f` to a start value `s` and all elements in `a` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, a[0]), a[1])..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b & e, s: b, v: MutList[a]): b & Impure =
        let len = length(v);
        foldLeftHelper(f, s, v, len, 0)

    ///
    /// Helper function for `foldLeft`.
    ///
    def foldLeftHelper(f: (b, a) -> b & e, s: b, v: MutList[a], len: Int32, i: Int32): b & Impure =
        if (i < len)
            let s1 = f(s, get(i, v)) as & Impure;
            foldLeftHelper(f, s1, v, len, i + 1)
        else
            s

    ///
    /// Applies `f` to a start value `s` and all elements in `a` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, a[0]), a[1])..., xn)`.
    ///
    pub def foldRight(f: (a, b) -> b & e, s: b, v: MutList[a]): b & Impure =
        let len = length(v);
        foldRightHelper(f, s, v, len - 1)

    ///
    /// Helper function for `foldRight`.
    ///
    def foldRightHelper(f: (a, b) -> b & e, s: b, v: MutList[a], i: Int32): b & Impure =
        if (i < 0)
            s
        else
            let s1 = f(get(i, v), s) as & Impure;
            foldRightHelper(f, s1, v, i - 1)

    ///
    /// Applies `f` to all elements in `v` going from left to right until a single value `v` is obtained. Returns `Some(v)`.
    ///
    /// Returns `None` if `v` is empty.
    ///
    pub def reduceLeft(f: (a, a) -> a & e, v: MutList[a]): Option[a] & Impure = foldLeft((acc, x) -> match acc {
        case Some(y) => Some(f(y, x))
        case None => Some(x)
    }, None, v)

    ///
    /// Applies `f` to all elements in `v` going from right to left until a single value `v` is obtained. Returns `Some(v)`.
    ///
    /// Returns `None` if `v` is empty.
    ///
    pub def reduceRight(f: (a, a) -> a & e, v: MutList[a]): Option[a] & Impure = foldRight((x, acc) -> match acc {
        case Some(y) => Some(f(x, y))
        case None => Some(x)
    }, None, v)

    ///
    /// Removes all elements from the given mutable list `v`.
    ///
    pub def clear!(v: mut MutList[a]): Unit & Impure =
        let MutList(a, l) = v;
        a := Array.new(default, Array.length(deref a));
        l := 0

    ///
    /// Returns a shallow copy of the given mutable list `v`.
    /// The capacity of the copy is equal to the length of the list.
    ///
    // TODO: Check for minCapacity
    pub def copy(v: MutList[a]): MutList[a] & Impure =
        let MutList(a, l) = v;
        let len = deref l;
        MutList(ref Array.slice(deref a, 0, len), ref len)

    ///
    /// Optionally removes and returns the last element in the given mutable list `v`.
    ///
    // TODO: Change assumption that length can be 0
    pub def pop!(v: mut MutList[a]): Option[a] & Impure =
        let MutList(a, l) = v;
        let len = deref l;
        if (len > 0)
            let last = (deref a)[len - 1];
            l := len - 1;
            Array.put(deref a, len - 1, default);
            compress!(v);
            Some(last)
        else
            None

    ///
    /// Inserts the given element `x` at the end of the given mutable list `v`.
    ///
    pub def push!(x: a, v: mut MutList[a]): Unit & Impure =
        let MutList(a, l) = v;
        let len = deref l;
        if (capacity(v) - len == 0)
            reserve!(len, v)
        else ();
        (deref a)[len] = x;
        l := len + 1

    ///
    /// Inserts the given element `x` at the given position `i` in the given mutable list `v`.
    ///
    /// Shifts elements as necessary. Possibly expensive operation.
    ///
    /// If the given index `i` exceeds the length of the mutable list, the element is inserted at the last position.
    ///
    pub def insert!(x: a, i: Int32, v: mut MutList[a]): Unit & Impure =
        let MutList(a, l) = v;
        let len = deref l;
        if (capacity(v) - len == 0)
            reserve!(len, v)
        else ();
        let sub = Array.slice(deref a, i, len);
        Array.updateSequence!(i + 1, sub, deref a);
        (deref a)[i] = x;
        l := len + 1

    ///
    /// Removes the element at the given position `i` in the given mutable list `v`.
    ///
    /// Shifts elements as necessary. Possibly expensive operation.
    ///
    /// If the given index `i` exceeds the length of the mutable list, no element is removed.
    ///
    pub def remove!(i: Int32, v: mut MutList[a]): Unit & Impure =
        let MutList(a, l) = v;
        let len = deref l;
        if (i < len) {
            removeHelper!(i, len, deref a);
            l := len - 1;
            compress!(v)
        }
        else
            ()

    ///
    /// Helper function for `remove!`.
    ///
    def removeHelper!(i: Int32, n: Int32, a: Array[a]): Unit & Impure =
        if (i < n - 1) {
            a[i] = a[i + 1];
            removeHelper!(i + 1, n, a)
        }
        else if (i == n - 1)
            a[i] = default
        else
            ()

    ///
    /// Inserts all the elements in the given mutable list `v2` into the mutable list `v1`.
    ///
    pub def append!(v1: mut MutList[a], v2: MutList[a]): Unit & Impure =
        let MutList(a1, l1) = v1;
        let MutList(a2, l2) = v2;
        let len1 = deref l1;
        let len2 = deref l2;
        if (len2 > Array.length(deref a1) - len1)
            reserve!(len2, v1)
        else
            ();
        Array.updateSequence!(len1, deref a2, deref a1);
        l1 := len1 + len2

    ///
    /// Removes all elements from the given mutable list `v` that do not satisfy the given predicate `f`.
    ///
    pub def retain!(f: a -> Bool, v: mut MutList[a]): Unit & Impure =
        let MutList(a1, l1) = v;
        let l = new();
        foreach(e -> if (f(e)) push!(e, l) else (), v);
        let MutList(a2, l2) = l;
        a1 := deref a2;
        l1 := deref l2

    ///
    /// Replaces all occurrences of the `x` with `y` in the given mutable list `v`.
    ///
    pub def replace!(x: a, y: a, v: mut MutList[a]): Unit & Impure with Eq[a] =
        transform!(e -> if (e == x) y else e, v)

    ///
    /// Reverses the order of the elements in the given mutable list `v`.
    ///
    pub def reverse!(v: mut MutList[a]): Unit & Impure =
        let MutList(a, l) = v;
        let len = deref l;
        reverseHelper!(deref a, len / 2, 0, len - 1)

    ///
    /// Helper function for `reverse!`.
    ///
    def reverseHelper!(a: mut Array[a], halflen: Int32, i: Int32, j: Int32): Unit & Impure =
        if (i >= halflen)
            ()
        else {
            let x = a[i];
            let y = a[j];
            Array.put(a, i, y);
            Array.put(a, j, x);
            reverseHelper!(a, halflen, i + 1, j - 1)
        }

    ///
    /// Shrinks the given mutable list `v` down to a capacity of `n` elements.
    ///
    /// Truncates the mutable list as needed.
    ///
    pub def shrinkTo!(n: Int32, v: mut MutList[a]): Unit & Impure =
        let minCap = minCapacity();
        let MutList(a, l) = v;
        let len = deref l;
        if (n < minCap) {
            a := [default; minCap];
            l := if (len < minCap) len else minCap
        }
        else if (n < capacity(v)) {
            let f = i -> if (i < n) (deref a)[i] else default;
            a := Array.init(f, n);
            l := if (len < n) len else n
        }
        else
            ()

    ///
    /// Shrinks the given mutable list `v` to its actual size.
    ///
    pub def shrink!(v: mut MutList[a]): Unit & Impure = shrinkTo!(length(v), v)

    ///
    /// Truncates the given mutable list `v` to the given length `l`.
    ///
    /// That is, after the operation, the mutable list has length at most `l`.
    ///
    /// If the given length `l` is negative, all elements are removed.
    ///
    pub def truncate!(l: Int32, v: mut MutList[a]): Unit & Impure =
        if (l < 0)
            clear!(v)
        else if (l < length(v)) {
            let MutList(a, l0) = v;
            let minCap = minCapacity();
            let c = if (l < minCap) minCap else l;
            l0 := l;
            Array.updateSequence!(0, deref a, Array.new(default, c))
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
    pub def reserve!(n: Int32, v: mut MutList[a]): Unit & Impure =
        let MutList(a, l) = v;
        let len = deref l;
        let cap = capacity(v);
        if (n > cap - len)
            let f = i ->
                if (i < len)
                    (deref a)[i]
                else
                    default;
            a := Array.init(f, cap + n)
        else ()

    ///
    /// Returns `v` as an immutable list.
    ///
    pub def toImm(v: MutList[a]): List[a] & Impure =
        toImmHelper(v, Nil)

    ///
    /// Helper function for `toImm`
    ///
    def toImmHelper(v: MutList[a], acc: List[a]): List[a] & Impure =
        if (isEmpty(v))
            acc
        else
            let x = match pop!(v) {
                case Some(a) => a
                case _       => unreachable!()
            };
            toImmHelper(v, x :: acc)

    ///
    /// Returns `v` as an array.
    ///
    pub def toArray(v: MutList[a]): Array[a] & Impure =
        let MutList(a, l) = v;
        Array.slice(deref a, 0, deref l)

    ///
    /// Returns `true` if the mutable lists `v1` and `v2` have the same elements, i.e. are structurally equal.
    ///
    pub def sameElements(v1: MutList[a], v2: MutList[a]): Bool & Impure with Eq[a] =
        let MutList(a1, l1) = v1;
        let MutList(a2, l2) = v2;
        let len1 = deref l1;
        let len2 = deref l2;
        len1 == len2 and sameElementsHelper(deref a1, deref a2, 0, len1)

    ///
    /// Helper function for `sameElements`.
    ///
    def sameElementsHelper(a1: Array[a], a2: Array[a], i: Int32, n: Int32): Bool & Impure with Eq[a] =
        if (i >= n)
            true
        else
            a1[i] == a2[i] and sameElementsHelper(a1, a2, i + 1, n)

    ///
    /// Render `v` as a String. Elements are rendered with the
    /// function `f` and separated with the string `sep`.
    ///
    pub def formatWith(f: a -> String, sep: String, v: MutList[a]): String & Impure =
        let sb = StringBuilder.new();
        let step = (x,i) ->
            if (i == 0)
                StringBuilder.appendString!(sb, f(x))
            else {
                StringBuilder.appendString!(sb, sep);
                StringBuilder.appendString!(sb, f(x))
            };
        foreachWithIndex(step, v);
        StringBuilder.toString(sb)

    ///
    /// Apply the effectful function `f` to all the elements in `v`.
    ///
    pub def foreach(f: a ~> Unit, v: MutList[a]): Unit & Impure =
        let MutList(a, l) = v;
        let len = deref l;
        if (len < 1)
            ()
        else
            foreachHelper(f, deref a, 0, len)

    ///
    /// Helper function for `foreach`.
    ///
    def foreachHelper(f: a ~> Unit, a: Array[a], i: Int32, n: Int32): Unit & Impure =
        if (i >= n)
            ()
        else {
            f(a[i]);
            foreachHelper(f, a, i + 1, n)
        }

    ///
    /// Apply the effectful function `f` to all the elements in `v`.
    ///
    pub def foreachWithIndex(f: (a, Int32) ~> Unit, v: MutList[a]): Unit & Impure =
        let MutList(a, l) = v;
        let len = deref l;
        if (len < 1)
            ()
        else
            foreachWithIndexHelper(f, deref a, 0, len)

    ///
    /// Helper function for `foreachWithIndex`.
    ///
    def foreachWithIndexHelper(f: (a, Int32) ~> Unit, a: Array[a], i: Int32, n: Int32): Unit & Impure =
        if (i >= n)
            ()
        else {
            f(a[i], i);
            foreachWithIndexHelper(f, a, i + 1, n)
        }

    ///
    /// Compresses the given mutable list `v` if needed.
    ///
    /// The mutable list will be shrunk to 1/2 of its size if the load factor is less than 1/4.
    ///
    pub def compress!(v: mut MutList[a]): Unit & Impure =
        let c = capacity(v);
        let len = length(v);
        let loadFactor = Int32.toFloat32(len) / Int32.toFloat32(c);
        if (loadFactor < 1.0f32/4.0f32 and len > 0) {
            if (len == 1)
                shrinkTo!(1, v)
            else
                shrinkTo!(c / 2, v)
        }
        else ()

    ///
    /// Returns the capacity of `v`.
    ///
    pub def capacity(v: MutList[a]): Int32 & Impure =
        let MutList(a, _) = v;
        Array.length(deref a)

    ///
    /// Returns a sorted copy of MutList `v`, where the elements are ordered from low to high according to
    /// their `Order` instance.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `v`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sort(v: MutList[a]): MutList[a] & Impure with Order[a] = sortWith(Order.compare, v)

    ///
    /// Returns a sorted copy of MutList `v`, where the elements are ordered from low to high according to
    /// the `Order` instance for the values obtained by applying `f` to each element.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `v`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortBy(f: a -> b, v: MutList[a]): MutList[a] & Impure with Order[b] = sortWith(Order.compare `on` f, v)

    ///
    /// Returns a sorted copy of MutList `v`, where the elements are ordered from low to high according to
    /// the comparison function `cmp`.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `v`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortWith(cmp: (a,a) -> Comparison, v: MutList[a]): MutList[a] & Impure =
        let vCopy = copy(v);
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
    pub def sort!(v: mut MutList[a]): Unit & Impure with Order[a] = sortWith!(Order.compare, v)

    ///
    /// Sort MutList `v` so that elements are ordered from low to high according to the `Order` instance for
    /// the values obtained by applying `f` to each element. The MutList is mutated in-place.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `v`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortBy!(f: a -> b, v: mut MutList[a]): Unit & Impure with Order[b] = sortWith!(Order.compare `on` f, v)

    ///
    /// Sort MutList `v` so that elements are ordered from low to high according to the comparison function `cmp`.
    /// The MutList is mutated in-place.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `v`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortWith!(cmp: (a,a) -> Comparison, v: mut MutList[a]): Unit & Impure =
        let MutList(a, l) = v;
        Array.sortWithin!(cmp, deref a, 0, (deref l) - 1)

}
