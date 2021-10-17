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
    pub def new(): MutDeque[a] & Impure =
        MutDeque(ref [default; minCapacity()], ref 0, ref 0)

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
        deref f == deref b

    ///
    /// Returns `Some(x)` where `x` is the element at the front. Returns `None` if `d` is empty.
    ///
    pub def popFront(d: MutDeque[a]): Option[a] & Impure =
        let MutDeque(a, f, _) = d;
        let a1 = deref a;
        let f1 = deref f;
        if (isEmpty(d)) {
            None
        }
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
        if (isEmpty(d)) {
            None
        }
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
        let MutDeque(a, f, _) = d;
        let a1 = deref a;
        let c = capacity(d);
        let f1 = (deref f - 1) `mod` c;
        a1[f1] = x;
        a := a1;
        f := f1;
        expand!(d)

    ///
    /// Pushes `x` to the back of `d`.
    ///
    pub def pushBack(x: a, d: MutDeque[a]): Unit & Impure =
        let MutDeque(a, _, b) = d;
        let a1 = deref a;
        let c = capacity(d);
        let b1 = deref b;
        a1[b1] = x;
        a := a1;
        b := (b1 + 1) `mod` c;
        expand!(d)

    ///
    /// Expands `d` to double its length.
    ///
    def expand!(d: MutDeque[a]): Unit & Impure =
        if (shouldExpand(d)) grow!(d) else ()

    ///
    /// Returns `true` if the load factor is greater or equal to 1 / 2.
    ///
    def shouldExpand(d: MutDeque[a]): Bool & Impure =
        loadFactorOf(size(d), capacity(d)) >= 3.0f32 / 4.0f32

    ///
    /// Expands the MutDeque `d` to double its size.
    ///
    def grow!(d: MutDeque[a]): Unit & Impure =
        let MutDeque(a0, f0, b0) = d;
        let a = deref a0;
        let f = deref f0;
        let b = deref b0;
        let c = capacity(d);
        let arr = [default; 2 * c];
        if (f < b) {
            Array.patch!(0, b - f, a[f .. b], arr)
        }
        else {
            Array.patch!(0, b,     a[0 .. b], arr);
            Array.patch!(b, c - f, a[f .. c], arr)
        };
        a0 := arr;
        f0 := 0;
        b0 := computeSize(c, f, b)

    ///
    /// Compresses MutDeque `d` if the load factor is below 1 / 2.
    ///
    def compress!(d: MutDeque[a]): Unit & Impure =
        if (shouldCompress(d)) shrink!(d) else ()

    ///
    /// Shrinks MutDeque `d` to half its size unless it's than the minimum capacity.
    ///
    def shrink!(d: MutDeque[a]): Unit & Impure =
        let MutDeque(a0, f0, b0) = d;
        let mc = minCapacity();
        let c = capacity(d);
        if (c > mc) {
            let a = deref a0;
            let f = deref f0;
            let b = deref b0;
            let s = size(d);
            let arr = [default; c / 2];
            if (f < b) {
                Array.patch!(0, b - f, a[f .. b], arr)
            } else {
                Array.patch!(0, b,     a[0 .. b], arr);
                Array.patch!(b, s - f, a[f .. s], arr)
            };
            a0 := arr;
            f0 := 0;
            b0 := s
        } else {
            ()
        }

    ///
    /// Returns `true` if the load factor is less than 1 / 2.
    ///
    def shouldCompress(d: MutDeque[a]): Bool & Impure =
        loadFactorOf(size(d), capacity(d)) <= 1.0f32 / 4.0f32

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
        (a + b) % b

}