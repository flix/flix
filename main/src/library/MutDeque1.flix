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


/// Represents a mutable deque.
pub enum MutDeque[a] {
    case MutDeque(Ref[Array[a]], Ref[Int32], Ref[Int32])
}

namespace MutDeque {

    ///
    /// Constant denoting the minimum allowed length of the backing array.
    ///
    def minCapacity(): Int32 = 8

    ///
    /// Returns an empty MutDeque.
    ///
    pub def new(): MutDeque[a] & Impure = MutDeque(ref [default; minCapacity()], ref 0, ref 0)

    ///
    /// Returns the number of elements in the MutDeque `d`.
    ///
    pub def size(d: MutDeque[a]): Int32 & Impure =
        let MutDeque(_, f, b) = d;
        computeSize(capacity(d), deref f, deref b)

    ///
    /// Returns the size of a MutDeque, where `l` = array length, `f` = front index, `b` = back index.
    ///
    def computeSize(l: Int32, f: Int32, b: Int32): Int32 =
        if (f <= b)
            b - f
        else
            l - (f - b)

    ///
    /// Returns `true` if the MutDeque `d` is empty.
    ///
    pub def isEmpty(d: MutDeque[a]): Bool & Impure =
        let MutDeque(_, f, b) = d;
        let f1 = deref f;
        let b1 = deref b;
        f1 == b1

    ///
    /// Returns `Some(x)` where `x` is the element at the front. Returns `None` if `d` is empty.
    ///
    pub def popFront(d: MutDeque[a]): Option[a] & Impure =
        let MutDeque(a, f, _) = d;
        let a1 = deref a;
        let f1 = deref f;
        if (isEmpty(d))
            None
        else {
            let x = a1[f1];
            let c = capacity(d);
            f := (f1 + 1) `mod` c;
            compress!(d);
            Some(x)
        }

    ///
    /// Returns `Some(x)` where `x` is the element at the back. Returns `None` if `d` is empty.
    ///
    pub def popBack(d: MutDeque[a]): Option[a] & Impure =
        let MutDeque(a, _, b) = d;
        let a1 = deref a;
        let c = capacity(d);
        let b1 = (deref b - 1) `mod` c;
        if (isEmpty(d))
            None
        else {
            let x = a1[b1];
            b := b1;
            compress!(d);
            Some(x)
        }

    ///
    /// Pushes `x` to the front of `d`.
    ///
    pub def pushFront(x: a, d: MutDeque[a]): Unit & Impure =
        let MutDeque(a, f, b) = d;
        let a1 = deref a;
        let c = capacity(d);
        let f1 = (deref f - 1) `mod` c;
        a1[f1] = x;
        let b1 = deref b;
        if (computeSize(c, f1, b1) >= c / 2) {
            expand!(a1, f1, b1, d)
        }
        else {
            a := a1;
            f := f1
        }

    ///
    /// Pushes `x` to the back of `d`.
    ///
    pub def pushBack(x: a, d: MutDeque[a]): Unit & Impure =
        let MutDeque(a, f, b) = d;
        let a1 = deref a;
        let c = capacity(d);
        let b1 = deref b;
        a1[b1] = x;
        let b2 = (b1 + 1) `mod` c;
        let f1 = deref f;
        if (computeSize(c, f1, b2) >= c / 2) { // TODO: refactor this into helper function as in `pop` operations.
            expand!(a1, f1, b2, d)
        }
        else {
            a := a1;
            b := b2
        }

    ///
    /// Expands `d` to double its length.
    ///
    def expand!(arr: Array[a], f: Int32, b: Int32, d: MutDeque[a]): Unit & Impure =
        let MutDeque(a1, f1, b1) = d;
        let c = capacity(d);
        let arr1 = [default; 2 * c];
        if (f <= b)
            Array.patch!(0, b - f, arr[f .. b], arr1)
        else {
            Array.patch!(0, b,     arr[0 .. b], arr1);
            Array.patch!(b, c - f, arr[f .. c], arr1)
        };
        f1 := 0;
        b1 := computeSize(c, f, b);
        a1 := arr1

    ///
    /// Compresses MutDeque `d` if the load factor is below 1 / 2.
    ///
    def compress!(d: MutDeque[a]): Unit & Impure =
        if (shouldBeCompressed(d)) {
            shrink!(d)
        } else
            ()

    ///
    /// Shrinks MutDeque `d` to half its size unless it's than the minimum capacity.
    ///
    def shrink!(d: MutDeque[a]): Unit & Impure =
        let MutDeque(a, f, b) = d;
        let mc = minCapacity();
        let c = capacity(d);
        if (c > mc) {
            let a1 = deref a;
            let f1 = deref f;
            let b1 = deref b;
            let s = size(d);
            let arr = [default; c / 2];
            if (f1 < b1) {
                Array.patch!(0, b1 - f1, a1[f1 .. b1], arr)
            } else {
                Array.patch!(0,  b1,     a1[0 .. b1], arr);
                Array.patch!(b1, s - f1, a1[f1 .. s], arr)
            };
            a := arr;
            f := 0;
            b := s
        } else
            ()

    ///
    /// Returns `true` if the load factor is less than 1 / 2.
    ///
    def shouldBeCompressed(d: MutDeque[a]): Bool & Impure =
        let s = size(d);
        let c = capacity(d);
        let loadFactor = loadFactorOf(s, c);
        loadFactor < 1.0f32 / 4.0f32 and s > 0 // and c > minCapacity()

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

    ///
    /// Returns `a` mod `b`.
    ///
    def mod(a: Int32, b: Int32): Int32 =
        ((a % b) + b) % b

}