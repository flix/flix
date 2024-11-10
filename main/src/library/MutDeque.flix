/*
 * Copyright 2021 Jakob Schneider Villumsen
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
/// Represents a mutable deque.
///
/// Explanation of component types (from left to right):
/// The 1st component is a reference the backing array.
/// The 2nd component is a reference to the front index.
/// The 3rd component is a reference to the back index.
///
/// If front == back then the deque is empty.
/// Otherwise, the front index always points to an element (going counter-clockwise)
/// and the back index always points to the first empty index (going clockwise).
///
pub struct MutDeque[a: Type, r: Region] {
    r: Region[r],
    mut values: Array[a, r],
    mut front: Int32,
    mut back: Int32
}

instance Iterable[MutDeque[a, r]] {
    type Elm = a
    type Aef = r
    pub def iterator(rc: Region[r1], md: MutDeque[a, r]): Iterator[a, r + r1, r1] \ (r + r1) = MutDeque.iterator(rc, md)
}

mod MutDeque {

    ///
    /// Constant denoting the minimum allowed capacity of the backing array.
    ///
    def minCapacity(): Int32 = 8

    ///
    /// Constant denoting the smallest valid load factor.
    ///
    /// The load factor is the ratio between number of elements in the array and its total capacity.
    /// I.e. `(number of elements) / capacity`.
    ///
    /// If the load factor falls below or is equal to `minLoadFactor` the array should be compressed.
    ///
    def minLoadFactor(): Float32 = 1.0f32 / 4.0f32

    ///
    /// Constant denoting the largest valid load factor.
    ///
    /// The load factor is the ratio between number of elements in the array and its total capacity.
    /// I.e. `(number of elements) / capacity`.
    ///
    /// If the load factor exceeds or is equal to `maxLoadFactor` the array should be expanded.
    ///
    def maxLoadFactor(): Float32 = 3.0f32 / 4.0f32

    ///
    /// Returns a string representation of the given MutDeque `d`.
    ///
    pub def toString(d: MutDeque[a, r]): String \ r with ToString[a] = region rc2 {
        let sb = StringBuilder.empty(rc2);
        StringBuilder.appendString("MutDeque#{", sb);
        forEachWithIndex((i, x) -> {
            if (i < 1)
                StringBuilder.appendString("${x}", sb)
            else
                StringBuilder.appendString(", ${x}", sb)
            }, d);
        StringBuilder.appendString("}", sb);
        StringBuilder.toString(sb)
    }

    ///
    /// Returns an empty MutDeque.
    ///
    pub def empty(rc: Region[r]): MutDeque[a, r] \ r =
        let arr = Array.empty(rc, minCapacity());
        new MutDeque @ rc {r = rc, values = arr, front = 0, back = 0}

    ///
    /// Returns the number of elements in `d`.
    ///
    pub def size(d: MutDeque[a, r]): Int32 \ r =
        computeSize(capacity(d), d->front, d->back)

    ///
    /// Returns the size of a MutDeque, where `l` = array length, `f` = front index, `b` = back index.
    ///
    def computeSize(c: Int32, f: Int32, b: Int32): Int32 =
        if (f <= b) // The elements laid out without "wrapping around" the array.
            b - f
        else
            c - (f - b) // Subtract the complement of number of elements from the capacity

    ///
    /// Returns `true` if `d` is empty.
    ///
    pub def isEmpty(d: MutDeque[a, r]): Bool \ r =
        d->front == d->back

    ///
    /// Returns `true` if `d` is non-empty.
    ///
    pub def nonEmpty(d: MutDeque[a, r]): Bool \ r = not isEmpty(d)

    ///
    /// Returns the sum of all elements in the deque `d`.
    ///
    pub def sum(d: MutDeque[Int32, r]): Int32 \ r =
        sumWith(identity, d)

    ///
    /// Returns the sum of all elements in the deque `d` according to the function `f`.
    ///
    pub def sumWith(f: a -> Int32 \ ef, d: MutDeque[a, r]): Int32 \ { ef, r } =
        foldLeft((acc, x) -> f(x) + acc, 0, d)

    ///
    /// Applies `f` to a start value `s` and all elements in `d` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b \ ef, s: b, d: MutDeque[a, r]): b \ { ef, r } =
        let c = capacity(d) - 1;
        def loop(i, e, acc) =
            if (i == e)
                acc
            else
                loop(Int32.bitwiseAnd(i + 1, c), e, f(acc, Array.get(i, d->values)));
        loop(d->front, d->back, s)

    ///
    /// Applies `f` to a start value `s` and all elements in `d` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b \ ef, s: b, d: MutDeque[a, r]): b \ { ef, r } =
        let c = capacity(d) - 1;
        def loop(i, e, acc) =
            if (i == e)
                acc
            else {
                let j = Int32.bitwiseAnd(i - 1, c);
                loop(j, e, f(Array.get(j, d->values), acc))
            };
        loop(d->back, d->front, s)

    ///
    /// Applies `f` to a start value `z` and all elements in `d` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, z))...)`.
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (a, Unit -> b \ {ef, r}) -> b \ {ef, r}, s: b, d: MutDeque[a, r]): b \ { ef, r } =
        let c = capacity(d) - 1;
        def loop(i, e) =
            if (i == e)
                s
            else {
                f(Array.get(i, d->values), _ -> loop(Int32.bitwiseAnd(i + 1, c), e))
            };
        loop(d->front, d->back)

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap(f: a -> b \ ef, d: MutDeque[a, r]): b \ { ef, r } with Monoid[b] =
        foldLeft((acc, x) -> Monoid.combine(acc, f(x)), Monoid.empty(), d)

    ///
    /// Returns `Some(x)` where `x` is the element at the front. Returns `None` if `d` is empty.
    ///
    pub def popFront(d: MutDeque[a, r]): Option[a] \ r =
        if (isEmpty(d)) {
            None
        }
        else { // Get the element `x` at the front, update index, optionally compress array, return `Some(x)`.
            let x = Array.get(d->front, d->values);
            d->front = (d->front + 1) `Int32.modulo` capacity(d);
            compress!(d);
            Some(x)
        }

    ///
    /// Returns `Some(x)` where `x` is the element at the back. Returns `None` if `d` is empty.
    ///
    pub def popBack(d: MutDeque[a, r]): Option[a] \ r =
        if (isEmpty(d)) {
            None
        }
        else { // Update index such that back points to a valid element `x`, get element, optionally compress array, return `Some(x)`.
            let b1 = (d->back - 1) `Int32.modulo` capacity(d);
            let x = Array.get(b1, d->values);
            d->back = b1;
            compress!(d);
            Some(x)
        }

    ///
    /// Pushes `x` to the front of `d`.
    ///
    pub def pushFront(x: a, d: MutDeque[a, r]): Unit \ r =
        let f1 = (d->front - 1) `Int32.modulo` capacity(d);     // Update index such that it points to an empty index. This will never overlap with the back index.
        Array.put(x, f1, d->values);                            // Store `x` in the array.
        d->front = f1;                                          // Update the front index reference.
        expand!(d)                                              // Optionally expand `d`.

    ///
    /// Pushes `x` to the back of `d`.
    ///
    pub def pushBack(x: a, d: MutDeque[a, r]): Unit \ r =
        Array.put(x, d->back, d->values);                           // Store `x` in the array.
        d->back = (d->back + 1) `Int32.modulo` capacity(d);         // Update back index reference to point to next empty index.
        expand!(d)                                                  // Optionally expand `d`.

    ///
    /// Optionally returns the front element. Does *not* remove it.
    ///
    pub def peekFront(d: MutDeque[a, r]): Option[a] \ r =
        let f1 = d->front;
        let b1 = d->back;
        if (f1 == b1)
            None
        else
            Some(Array.get(f1, d->values))

    ///
    /// Optionally returns the back element. Does *not* remove it.
    ///
    pub def peekBack(d: MutDeque[a, r]): Option[a] \ r =
        let f1 = d->front;
        let b1 = d->back;
        if (f1 == b1)
            None
        else
            let c = capacity(d) - 1;
            let i = Int32.bitwiseAnd(b1 - 1, c);
            Some(Array.get(i, d->values))

    ///
    /// Doubles the capacity of `d` if the load factor is greater than or equal to `maxLoadFactor`.
    ///
    def expand!(d: MutDeque[a, r]): Unit \ r =
        if (shouldExpand(d)) grow!(d) else ()

    ///
    /// Returns `true` if the load factor is greater than or equal to `maxLoadFactor`.
    ///
    def shouldExpand(d: MutDeque[a, r]): Bool \ r =
        loadFactorOf(size(d), capacity(d)) >= maxLoadFactor()

    ///
    /// Doubles the capacity of `d`.
    ///
    def grow!(d: MutDeque[a, r]): Unit \ r =
        let c = capacity(d);
        let arr = Array.empty(d->r, Int32.leftShift(c, 1));     // Allocate empty array `arr` with double the capacity of `a`.
        copyElements!(d->r, d->front, d->back, d->values, arr); // Copy elements from old array `a` to empty array `arr`.
        d->values = arr;                                        // Update references.
        d->back = computeSize(c, d->front, d->back);
        d->front = 0

    ///
    /// Compresses MutDeque `d` if the load factor is less than or equal to `minLoadFactor`.
    ///
    def compress!(d: MutDeque[a, r]): Unit \ r =
        if (shouldCompress(d)) shrink!(d) else ()

    ///
    /// Returns `true` if the load factor is less than or equal to 1 / 4.
    ///
    def shouldCompress(d: MutDeque[a, r]): Bool \ r =
        loadFactorOf(size(d), capacity(d)) <= minLoadFactor()

    ///
    /// Shrinks MutDeque `d` to half its size but never below `minCapacity`.
    ///
    def shrink!(d: MutDeque[a, r]): Unit \ r =
        let mc = minCapacity();
        let c = capacity(d);
        if (c > mc) {                                               // Prevent the backing array from shrinking below `minCapacity`.
            let arr = Array.empty(d->r, Int32.rightShift(c, 1));    // Allocate empty array `arr` with half the capacity of `a`.
            copyElements!(d->r, d->front, d->back, d->values, arr); // Copy elements from old array `a` to empty array `arr`.
            d->values = arr;                                        // Update references.
            d->back = computeSize(c, d->front, d->back);
            d->front = 0
        } else {
            ()
        }


    ///
    /// Copies the elements from `a` to `a1`. Mutates the array `a1`.
    ///
    def copyElements!(rc2: Region[r2], f: Int32, b: Int32, a: Array[a, r1], a1: Array[a, r2]): Unit \ { r1, r2 } =
        let c = Array.length(a);
        if (f < b) { // If this predicate is true the elements do not "wrap around" in the array, i.e. the elements are laid out sequentially from [0 .. b].
            Array.updateSequence(0,     Array.slice(rc2, start = f, end = b, a), a1)
        } else {
            Array.updateSequence(0,     Array.slice(rc2, start = f, end = c, a), a1); // Copy the front elements of `a` to a1[0 .. (c - f)].
            Array.updateSequence(c - f, Array.slice(rc2, start = 0, end = b, a), a1)  // Copy the back  elements of `a` to a1[(c - f) .. b].
        }

    ///
    /// Returns the load factor, given size `s` and capacity `c`.
    ///
    def loadFactorOf(s: Int32, c: Int32): Float32 =
        Int32.toFloat32(s) / Int32.toFloat32(c)

    ///
    /// Returns the capacity of `d`.
    ///
    def capacity(d: MutDeque[a, r]): Int32 \ r =
        Array.length(d->values)

    ///
    /// Returns `true` if `MutDeque`s `a` and `b` have the same elements in the same order, i.e. are structurally equal.
    ///
    pub def sameElements(d1: MutDeque[t, r1], d2: MutDeque[t, r2]): Bool \ { r1, r2 } with Eq[t] = region rc3 {
        let aSize = size(d1);
        let bSize = size(d2);
        if (aSize == bSize) {
            let a1 = Array.empty(rc3, aSize);
            let b1 = Array.empty(rc3, bSize);
            copyElements!(rc3, d1->front, d1->back, d1->values, a1);
            copyElements!(rc3, d2->front, d2->back, d2->values, b1);
            Array.sameElements(a1, b1)
        } else
            false
    }

    ///
    /// Returns `d` as a `List`.
    ///
    pub def toList(d: MutDeque[a, r]): List[a] \ r =
        foldRight((x, acc) -> x :: acc, Nil, d)

    ///
    /// Returns `d` as an array.
    ///
    pub def toArray(rc1: Region[r1], d: MutDeque[a, r2]): Array[a, r1] \ { r2, r1 } =
        let len = MutDeque.capacity(d);
        let i = d->front;
        let j = d->back;
        if (i == j)
            Array#{} @ rc1
        else if (i  < j)
            Array.copyOfRange(rc1, i, j, d->values)
        else
            Array.append(rc1, Array.copyOfRange(rc1, i, len, d->values), Array.copyOfRange(rc1, 0, j, d->values))

    ///
    /// Returns `d` as a vector.
    ///
    pub def toVector(d: MutDeque[a, r]): Vector[a] \ r = region rc {
        let arr = Array.empty(rc, size(d));
        forEachWithIndex((i, x) -> Array.put(x, i, arr), d);
        Array.toVector(arr)
    }

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `d` with `sep` inserted between each element.
    ///
    pub def join(sep: String, d: MutDeque[a, r]): String \ r with ToString[a] =
        joinWith(ToString.toString, sep, d)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `d` according to `f` with `sep` inserted between each element.
    ///
    pub def joinWith(f: a -> String \ ef, sep: String, d: MutDeque[a, r]): String \ { ef, r } = region rc1 {
        use StringBuilder.append;
        let lastSep = String.length(sep);
        let sb = StringBuilder.empty(rc1);
        forEach(x -> { append(f(x), sb); append(sep, sb) }, d);
        StringBuilder.toString(sb) |> String.dropRight(lastSep)
    }

    ///
    /// Returns an iterator over `d`.
    ///
    /// Modifying `d` while using an iterator has undefined behavior and is dangerous.
    ///
    pub def iterator(rc: Region[r1], d: MutDeque[a, r2]): Iterator[a, r1 + r2, r1] \ { r1, r2 } =
        let i = Ref.fresh(rc, d->front);
        let next = () -> {
            if (Ref.get(i) < d->back) {
                let x = Array.get(Ref.get(i), d->values);
                Ref.put(Ref.get(i) + Int32.bitwiseAnd(1, capacity(d) - 1), i);
                Some(x)
            } else {
                None
            }
        };
        Iterator.iterate(rc, next)

    ///
    /// Apply the effectful function `f` to all the elements in the MutDeque `d`.
    ///
    pub def forEach(f: a -> Unit \ ef, d: MutDeque[a, r]): Unit \ { ef, r } =
        let c = capacity(d) - 1;

        def loop(i) = {
            if (i == d->back) {
                ()
            } else {
                match Array.nth(i, d->values) {
                    case Some(x) => f(x)
                    case None    => bug!("An error occurred in MutDeque.forEach!")
                };
                loop(Int32.bitwiseAnd(i + 1, c))
            }
        };
        loop(d->front)

    ///
    /// Apply the effectful function `f` to all the elements in the MutDeque `d`
    /// along with that element's index.
    ///
    pub def forEachWithIndex(f: (Int32, a) -> Unit \ ef, d: MutDeque[a, r]): Unit \ { ef, r } = region rc {
        let ix = Ref.fresh(rc, 0);
        forEach(x -> { let i = Ref.get(ix); f(i, x); Ref.put(i + 1, ix) }, d)
    }

    ///
    /// Shuffles a copy of `d` using the Fisher–Yates shuffle.
    ///
    pub def shuffle(rc1: Region[r1], d: MutDeque[a, r2]): MutDeque[a, r1] \ { r2, r1, NonDet } = region rc3 {
        toArray(rc3, d) !> Array.shuffle |> Array.toMutDeque(rc1)
    }

}
