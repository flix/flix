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

    def minSize(): Int32 = 8

    pub def new(): MutDeque[a] & Impure = MutDeque(ref Array.new(default, minSize()), ref 0, ref 0)

    pub def size(d: MutDeque[a]): Int32 & Impure =
        let MutDeque(_, f, b) = d;
        let f1 = deref f1;
        let b1 = deref b;
        if (f1 < b1) b1 - f1 else f1 - b1

    pub def isEmpty(d: MutDeque[a]): Bool & Impure =
        let MutDeque(_, f, b) = d;
        let f1 = deref f1;
        let b1 = deref b;
        f1 == b1

    pub def popFront(d: MutDeque[a]): Unit & Impure = ???

    pub def popBack(d: MutDeque[a]): Unit & Impure = ???

    pub def pushFront(d: MutDeque[a]): Unit & Impure = ???

    pub def pushBack(d: MutDeque[a]): Unit & Impure = ???

}