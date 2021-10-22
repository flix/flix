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
/// The type of the done function.
///
type alias Done = Unit ~> Bool

///
/// The type of the next function.
///
type alias Next[a] = Unit ~> a

///
/// An iterator is a stream of elements that are produced on demand.
///
pub enum Iterator[a] {
    case Iterator(Done, Next[a])
}

namespace Iterator {

    ///
    /// Returns an iterator of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns an empty iterator if `b >= e`.
    ///
    pub def range(b: Int32, e: Int32): Iterator[Int32] & Impure =
        let i = ref b;
        let done = () -> deref i >= e;
        let next = () -> {
                let res = deref i;
                if (res < e) {
                    i := res + 1;
                    res
                }
                else {
                    bug!("Empty Iterator!")
                }
            };
        Iterator(done, next)

    ///
    /// Returns an iterator over an iterable with the element `x` repeated `n` times.
    ///
    /// Returns an empty iterator if `n < 0`.
    ///
    pub def repeat(x: a, n: Int32): Iterator[a] & Impure =
        let i = ref 0;
        let done = () -> deref i >= n;
        let next = () -> {
              let j = deref i;
              if (j < n) {
                i := j + 1;
                x
              }
              else {
                bug!("Empty Iterator!")
              }
            };
        Iterator(done, next)

    ///
    /// Returns an array with the elements of `iter`.
    ///
    pub def toArray(iter: Iterator[a]): Array[a] & Impure =
        let Iterator(done, next) = iter;
        toArrayAcc(done, next, MutList.new())

    ///
    /// Helper function for `toArray`.
    ///
    def toArrayAcc(done: Done, next: Next[a], v: MutList[a]): Array[a] & Impure =
        if (done())
            MutList.toArray(v)
        else
            toArrayAcc(done, next, MutList.push!(next(), v))

    ///
    /// Returns the contents of `iter` as a list. Consumes the entire iterator.
    ///
    pub def toList(iter: Iterator[a]): List[a] & ef =
        let Iterator(done, next) = iter;
        (toListAcc(done, next, Nil) |> List.reverse) as & ef

    ///
    /// Helper function for `toList`.
    ///
    def toListAcc(done: Done, next: Next[a], acc: List[a]): List[a] & Impure =
        if (done()) {
            acc
        }
        else {
            toListAcc(done, next, next() :: acc)
        }

    ///
    /// Returns the contents of `iter` as a map. Consumes the entire iterator.
    ///
    pub def toMap(iter: Iterator[(a, b)]): Map[a, b] & Impure =
        let Iterator(done, next) = iter;
        toMapAcc(done, next, Map.empty())

    ///
    /// Helper function for `toMap`.
    ///
    def toMapAcc(done: Done, next: Next[(a, b)], acc: Map[a, b]): Map[a, b] & Impure =
        if (done())
            acc
        else
            toMapAcc(done, next, Map.insert(next(), acc))

}