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
pub enum MutDeque[a] {
    case MutDeque(Ref[Array[a]], Ref[Int32], Ref[Int32])
}

namespace MutDeque {

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
    /// Returns an empty MutDeque.
    ///
    pub def new(): MutDeque[a] & Impure =
        MutDeque(ref [$DEFAULT$; minCapacity()], ref 0, ref 0)

    ///
    /// Returns the number of elements in `d`.
    ///
    pub def size(d: MutDeque[a]): Int32 & Impure =
        let MutDeque(_, f, b) = d;
        computeSize(capacity(d), deref f, deref b)

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
    pub def isEmpty(d: MutDeque[a]): Bool & Impure =
        let MutDeque(_, f, b) = d;
        deref f == deref b

    ///
    /// Returns `Some(x)` where `x` is the element at the front. Returns `None` if `d` is empty.
    ///
    pub def popFront(d: MutDeque[a]): Option[a] & Impure =
        if (isEmpty(d)) {
            None
        }
        else { // Get the element `x` at the front, update index, optionally compress array, return `Some(x)`.
            let MutDeque(a, f, _) = d;
            let a1 = deref a;
            let f1 = deref f;
            let x = a1[f1];
            f := (f1 + 1) mod capacity(d);
            compress!(d);
            Some(x)
        }

    ///
    /// Returns `Some(x)` where `x` is the element at the back. Returns `None` if `d` is empty.
    ///
    pub def popBack(d: MutDeque[a]): Option[a] & Impure =
        if (isEmpty(d)) {
            None
        }
        else { // Update index such that back points to a valid element `x`, get element, optionally compress array, return `Some(x)`.
            let MutDeque(a, _, b) = d;
            let a1 = deref a;
            let b1 = (deref b - 1) mod capacity(d);
            let x = a1[b1];
            b := b1;
            compress!(d);
            Some(x)
        }

    ///
    /// Pushes `x` to the front of `d`.
    ///
    pub def pushFront(x: a, d: MutDeque[a]): Unit & Impure =
        let MutDeque(a, f, _) = d;
        let a1 = deref a;
        let f1 = (deref f - 1) mod capacity(d); // Update index such that it points to an empty index. This will never overlap with the back index.
        a1[f1] = x;                               // Store `x` in the array.
        f := f1;                                  // Update the front index reference.
        expand!(d)                                // Optionally expand `d`.

    ///
    /// Pushes `x` to the back of `d`.
    ///
    pub def pushBack(x: a, d: MutDeque[a]): Unit & Impure =
        let MutDeque(a, _, b) = d;
        let a1 = deref a;
        let b1 = deref b;
        a1[b1] = x;                      // Store `x` in the array.
        b := (b1 + 1) mod capacity(d); // Update back index reference to point to next empty index.
        expand!(d)                       // Optionally expand `d`.

    ///
    /// Doubles the capacity of `d` if the load factor is greater than or equal to `maxLoadFactor`.
    ///
    def expand!(d: MutDeque[a]): Unit & Impure =
        if (shouldExpand(d)) grow!(d) else ()

    ///
    /// Returns `true` if the load factor is greater than or equal to `maxLoadFactor`.
    ///
    def shouldExpand(d: MutDeque[a]): Bool & Impure =
        loadFactorOf(size(d), capacity(d)) >= maxLoadFactor()

    ///
    /// Doubles the capacity of `d`.
    ///
    def grow!(d: MutDeque[a]): Unit & Impure = {
        let MutDeque(a, f, b) = d;
        let c = capacity(d);
        let arr = [$DEFAULT$; 2 * c];                  // Allocate new array `arr` with double the capacity of `a`.
        copyElements!(deref f, deref b, deref a, arr); // Copy elements from old array `a` to new array `arr`.
        a := arr;                                      // Update references.
        b := computeSize(c, deref f, deref b);
        f := 0
    }

    ///
    /// Compresses MutDeque `d` if the load factor is less than or equal to `minLoadFactor`.
    ///
    def compress!(d: MutDeque[a]): Unit & Impure =
        if (shouldCompress(d)) shrink!(d) else ()

    ///
    /// Returns `true` if the load factor is less than or equal to 1 / 4.
    ///
    def shouldCompress(d: MutDeque[a]): Bool & Impure =
        loadFactorOf(size(d), capacity(d)) <= minLoadFactor()

    ///
    /// Shrinks MutDeque `d` to half its size but never below `minCapacity`.
    ///
    def shrink!(d: MutDeque[a]): Unit & Impure =
        let MutDeque(a, f, b) = d;
        let mc = minCapacity();
        let c = capacity(d);
        if (c > mc) {                                      // Prevent the backing array from shrinking below `minCapacity`.
            let arr = [$DEFAULT$; c / 2];                  // Allocate new array `arr` with half the capacity of `a`.
            copyElements!(deref f, deref b, deref a, arr); // Copy elements from old array `a` to new array `arr`.
            a := arr;                                      // Update references.
            b := computeSize(c, deref f, deref b);
            f := 0
        } else {
            ()
        }

    ///
    /// Copies the elements from `a` to `a1`. Mutates the array `a1`.
    ///
    def copyElements!(f: Int32, b: Int32, a: Array[a], a1: Array[a]): Unit & Impure =
        let c = Array.length(a);
        if (f < b) { // If this predicate is true the elements do not "wrap around" in the array, i.e. the elements are laid out sequentially from [0 .. b].
            Array.updateSequence!(0,     a[f .. b], a1)
        } else {
            Array.updateSequence!(0,     a[f .. c], a1); // Copy the front elements of `a` to a1[0 .. (c - f)].
            Array.updateSequence!(c - f, a[0 .. b], a1)  // Copy the back  elements of `a` to a1[(c - f) .. b].
        }

    ///
    /// Returns the load factor, given size `s` and capacity `c`.
    ///
    def loadFactorOf(s: Int32, c: Int32): Float32 =
        Int32.toFloat32(s) / Int32.toFloat32(c)

    ///
    /// Returns the capacity of `d`.
    ///
    def capacity(d: MutDeque[a]): Int32 & Impure =
        let MutDeque(a, _, _) = d;
        Array.length(deref a)

}
