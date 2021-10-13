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
// TODO: Add Doc
// TODO: Add tests

    def minCapacity(): Int32 = 8

    pub def new(): MutDeque[a] & Impure = MutDeque(ref [default; minCapacity()], ref 0, ref 0)

    pub def size(d: MutDeque[a]): Int32 & Impure =
        let MutDeque(a, f, b) = d;
        computeSize(Array.length(deref a), deref f, deref b)

    ///
    /// Returns the size of a MutDeque, where `l` = array length, `f` = front index, `b` = back index.
    ///
    def computeSize(l: Int32, f: Int32, b: Int32): Int32 =
        if (f <= b)
            b - f
        else
            l - (f - b)

    pub def isEmpty(d: MutDeque[a]): Bool & Impure =
        let MutDeque(_, f, b) = d;
        let f1 = deref f;
        let b1 = deref b;
        f1 == b1

    // TODO: Collapse array if load factor < threshold
    pub def popFront(d: MutDeque[a]): a & Impure =
        let MutDeque(a, f, _) = d;
        let a1 = deref a;
        let f1 = deref f;
        let x = a1[f1];
        let c = Array.length(a1);
        f := (f1 + 1) `mod` c;
        x

    // TODO: Collapse array if load factor < threshold
    pub def popBack(d: MutDeque[a]): a & Impure =
        let MutDeque(a, _, b) = d;
        let a1 = deref a;
        let c = Array.length(a1);
        let b1 = (deref b - 1) `mod` c;
        let x = a1[b1];
        b := b1;
        x

    // TODO: Expand array if elements > capacity / 2
    pub def pushFront(x: a, d: MutDeque[a]): Unit & Impure =
        let MutDeque(a, f, b) = d;
        let a1 = deref a;
        let l = Array.length(a1);
        let f1 = (deref f - 1) `mod` l;
        a1[f1] = x;
        let b1 = deref b;
        println("front: ${f1}"); // DEBUG
        if (computeSize(l, f1, b1) >= l / 2)
            expand(a1, f1, b1, d)
        else
            a := a1;
            f := f1

    pub def pushBack(x: a, d: MutDeque[a]): Unit & Impure =
        let MutDeque(a, f, b) = d;
        let a1 = deref a;
        let l = Array.length(a1);
        let b1 = deref b;
        a1[b1] = x;
        let b2 = (b1 + 1) `mod` l;
        let f1 = deref f;
        if (computeSize(l, f1, b2) >= l / 2)
            expand(a1, f1, b2, d)
        else
            a := a1;
            b := b2

    ///
    /// Expands `d` to double its length.
    ///
    def expand(arr: Array[a], f: Int32, b: Int32, d: MutDeque[a]): Unit & Impure =
        println("Expanding:${f}, ${b}"); // DEBUG
        let MutDeque(a1, f1, b1) = d;
        let l = Array.length(arr);
        let arr1 = [default; 2 * l];
        if (f <= b)
            Array.patch!(0, b - f, arr[f .. b], arr1)
        else {
            Array.patch!(0, b,     arr[0 .. b], arr1);
            Array.patch!(b, l - f, arr[f .. l], arr1);
            ()
        };
        f1 := 0;
        b1 := computeSize(l, f, b);
        a1 := arr1

    ///
    /// Returns `a` mod `b`.
    ///
    def mod(a: Int32, b: Int32): Int32 =
        let r = a % b;
        if (r < 0)
            let r1 = abs(b) - abs(r);
            if (a < 0) r1 else -r1
        else
            r

    ///
    /// Returns the absolute value of `x`.
    ///
    def abs(x: Int32): Int32 =
        if (x < 0) -x else x

}