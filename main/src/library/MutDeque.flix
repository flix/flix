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

use Array.{length, get, put};

/// Represents a mutable deque.
pub enum MutDeque[a] {
    case MutDeque(Ref[Array[a]], Ref[Int32], Ref[Int32])
}


namespace MutDeque {

    ///
    /// Creates a new MutDeque with default values.
    //
    pub def new(): MutDeque[a] & Impure = MutDeque(ref [], ref 0, ref 0)


    ///
    /// Removes and returns the front element as an optional of d in O(1) time. Returns None if d is empty.
    ///
    pub def popFront(d: MutDeque[a]): Option[a] & Impure = match d {
        case _ if isEmpty(d) => None

        case MutDeque(arr, f, _) if isWithinBounds(deref f, deref arr) =>
            let front = deref f;
            f := increment(front)(withRespectTo(deref arr));
            Some(get(deref arr, front))

        case MutDeque(arr, _, _) => Some((deref arr)[-1]) // Internal Error - should be unreachable
    }


    ///
    /// Removes and returns the back element as an optional of d in O(1) time. Returns None if d is empty.
    ///
    pub def popBack(d: MutDeque[a]): Option[a] & Impure = match d {
        case _ if isEmpty(d) => None

        case MutDeque(arr, _, b) if isWithinBounds(deref b, deref arr) =>
            b := decrement(deref b)(withRespectTo(deref arr));
            Some(get(deref arr, deref b))

        case MutDeque(arr, _, _) => Some((deref arr)[-1]) // Internal Error - should be unreachable
    }


    ///
    /// Pushes x to the front of d in amortized O(1) time.
    ///
    pub def pushFront(x: a, d: MutDeque[a]): Unit & Impure = match d {
        case MutDeque(arr, front, back) if requiresExpansion(d) =>
            let oldIndex = computeEndIndexOf(deref arr);
            arr := expandWithDefault(d, x);
            front := length(deref arr) - 1;
            back := oldIndex - 1

        case MutDeque(arr, front, _) =>
            front := decrement(deref front)(withRespectTo(deref arr));
            arr := put(deref arr, deref front, x)
    }


    ///
    /// Pushes x to the back of d in amortized O(1) time.
    ///
    pub def pushBack(x: a, d: MutDeque[a]): Unit & Impure = match d {
        case MutDeque(arr, front, back) if requiresExpansion(d) =>
            let oldEndIndex = computeEndIndexOf(deref arr);
            arr := expandWithDefault(d, x);
            front := 0;
            back := oldEndIndex

        case MutDeque(arr, _, back) =>
            arr := put(deref arr, deref back, x);
            back := increment(deref back)(withRespectTo(deref arr))
    }


    ///
    /// Returns the number of elements in d that satisfy the predicate f in O(n) time.
    ///
    pub def count(f: a -> Bool, d: MutDeque[a]): Int32 & Impure = {
        foldLeft((acc, x) -> if (f(x)) acc + 1 else acc, 0, d)
    }


    ///
    /// Applies f to a start value s and all elements in d, going from left to right.
    ///
    pub def foldLeft(f: (b, a) -> b & e, s: b, d: MutDeque[a]): b & Impure = match d {
        case _ if isEmpty(d)        => s
        case MutDeque(arr, _, back) =>
            let (currentVal, nextEl) = getFoldElementsFrom(d);
            foldLeft(f, f(s, currentVal), MutDeque(arr, nextEl, back))
    }


    ///
    /// Applies f to a start value s and all elements in d, going from right to left.
    ///
    pub def foldRight(f: (a, b) -> b & e, s: b, d: MutDeque[a]): b & Impure = match d {
        case _ if isEmpty(d)        => s
        case MutDeque(arr, _, back) =>
            let (currentVal, nextEl) = getFoldElementsFrom(d);
            f(currentVal, foldRight(f, s, MutDeque(arr, nextEl, back)))
    }


    ///
    /// Returns true if d is empty, else false.
    ///
    pub def isEmpty(d: MutDeque[a]): Bool & Impure = {
        let MutDeque(_, front, back) = d;
        deref front == deref back
    }


    ///
    /// Helper function for foldLeft and foldRight.
    ///
    def getFoldElementsFrom(d: MutDeque[a]): (a, Ref[Int32]) & Impure = {
        let MutDeque(arr, front, _) = d;
        let currentVal = get(deref arr, deref front);
        let nextEl = ref increment(deref front)(withRespectTo(deref arr));
        (currentVal, nextEl)
    }


    ///
    /// Helper function for push operations.
    ///
    def requiresExpansion(d: MutDeque[a]): Bool & Impure =
        let MutDeque(arr, front, back) = d;
        length(deref arr) == 0 || (deref back + 1) % length(deref arr) == deref front


    ///
    /// Helper function to increment indices.
    ///
    def increment(i: Int32, limit: Int32): Int32 = (i + 1) % limit


    ///
    /// Helper function to decrement indices.
    ///
    def decrement(i: Int32, limit: Int32): Int32 = if (i - 1 < 0) limit - 1 else i - 1


    ///
    /// Helper function used in increment calls to improve readability.
    ///
    def withRespectTo(arr: Array[a]): Int32 = length(arr)


    ///
    /// Helper function for push operations which copies the elements of d to an array of double length and writes x to remaining "empty" entries.
    ///
    def expandWithDefault(d: MutDeque[a], x: a): Array[a] & Impure = {
        let MutDeque(arr, _, _) = d;
        if (length(deref arr) == 0) Array.init(_ -> x, 2)
        else {
            let doubleLength = 2 * length(deref arr);
            Array.init(_ -> {
                match popFront(d) {
                    case None    => x
                    case Some(v) => v
                }
            }, doubleLength)
        }
    }


    ///
    /// Helper function for pop operations to determine if index i is within bounds of arr.
    ///
    def isWithinBounds(i: Int32, arr: Array[a]): Bool = {
        i >= 0 && i < length(arr)
    }


    ///
    /// Helper function for push operations that determines wrap-around index
    ///
    def computeEndIndexOf(arr: Array[a]): Int32 =
        let len = length(arr);
        if (len == 0) 1 else len
}
