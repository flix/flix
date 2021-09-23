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

pub enum Step[a, s] {
    case Done,
    case Skip(s),
    case Yield(a, s)
}

pub enum Stream[a, s] {
    case Stream(s -> Step[a, s], s)
}

namespace Stream {

    ///
    /// Returns an empty stream.
    ///
    pub def empty(): Stream[a, s] =
        Stream(_ -> Done, default)

    ///
    /// Returns true if and only if the step function of `s` evaluates to `Done`.
    ///
    pub def isEmpty(s: Stream[a, s]): Bool =
        let Stream(next0, s0) = s;
        match next0(s0) {
            case Done => true
            case _    => false
        }

    ///
    /// Returns `s` as a `List`.
    ///
    pub def toList(s: Stream[a, s]): List[a] =
        let Stream(next0, s0) = s;
        toListAcc(next0, s0, ks -> ks)

    ///
    /// Helper function for `toList`.
    ///
    def toListAcc(next: s -> Step[a, s], s0: s, k: List[a] -> List[a]): List[a] = match next(s0) {
        case Done         => k(Nil)
        case Skip(s1)     => toListAcc(next, s1, k)
        case Yield(x, s1) => toListAcc(next, s1, ks -> k(x :: ks))
    }

}
