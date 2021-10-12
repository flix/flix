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

    pub def new(): MutDeque[a] & Impure = MutDeque(ref Array.new(default, minCapacity()), ref 0, ref 0)

    pub def size(d: MutDeque[a]): Int32 & Impure =
        let MutDeque(a, f, b) = d;
        let f1 = deref f;
        let b1 = deref b;
        if (f1 <= b1) b1 - f1 else Array.length(deref a) - (f1 - b1)

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
        f := mod(f1 + 1, c);
        x

    // TODO: Collapse array if load factor < threshold
    pub def popBack(d: MutDeque[a]): a & Impure =
        let MutDeque(a, _, b) = d;
        let a1 = deref a;
        let c = Array.length(a1);
        let b1 = mod(deref b - 1, c);
        let x = a1[b1];
        b := b1;
        x

    // TODO: Expand array if elements > capacity / 2
    pub def pushFront(x: a, d: MutDeque[a]): Unit & Impure =
        let MutDeque(a, f, _) = d;
        let a1 = deref a;
        let c = Array.length(a1);
        let f1 = mod(deref f - 1, c);
        a1[f1] = x;
        a := a1;
        f := f1

    // TODO: Expand array if elements > capacity / 2
    pub def pushBack(x: a, d: MutDeque[a]): Unit & Impure =
        let MutDeque(a, _, b) = d;
        let a1 = deref a;
        let c = Array.length(a1);
        let b1 = deref b;
        a1[b1] = x;
        a := a1;
        b := mod(b1 + 1, c)

    ///
    /// Returns `a` mod `b`.
    ///
    def mod(a: Int32, _b: Int32): Int32 = a

}