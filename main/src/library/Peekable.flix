/*
 *  Copyright 2025 Ry Wiese
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

///
/// Extends `Readable` with the ability to `peek` at values that have not yet been `read`,
/// `skip` values that don't need to be read, and perform other kinds of conditional reads.
///
trait Peekable[t] with Readable[t] {

    ///
    /// Returns `Ok(Some(x))` where x is the next value to be read, or `Ok(None)` if the `reader` is empty.
    /// Does not read `x` from the `reader`.
    ///
    /// Returns `Err(err)` if there is an IO error.
    ///
    pub def peek(reader: t): Result[IoError, Option[Readable.Elm[t]]] \ Readable.Aef[t]

    ///
    /// Advances the reader by `n`, skipping over the elements without reading them.
    ///
    /// Returns `Ok(k)` where `k` is the number of items skipped.
    ///
    /// Guarantees that `0 <= k <= max(n, 0)`. If `k < n`, then EOF has been reached.
    ///
    /// Returns `Err(err)` if there is an IO error.
    ///
    pub def skip(n: Int32, reader: t): Result[IoError, Int32] \ Readable.Aef[t]

    ///
    /// Reads from `reader` until an element is reached that does not satisfy predicate `p`.
    ///
    /// Returns `Ok(vec)` where `vec` is a vector of the first `k` elements where `k` is the index of the first element that does not satisfy `p`.
    /// This element is not included in the vector, and will be read on the next read.
    ///
    /// All elements of `vec` are guaranteed to satisfy `p`.
    ///
    /// Returns `Err(err)` if there is an IO error.
    ///
    pub def readWhile(p: Readable.Elm[t] -> Bool, reader: t): Result[IoError, Vector[Readable.Elm[t]]] \ Readable.Aef[t]

    ///
    /// Reads from `reader` until an element is reached that satisfies predicate `p`.
    ///
    /// Returns `Ok(vec)` where `vec` is a vector of the first `k` elements where `k` is the index of the first element that satisfies `p`.
    /// This element is not included in the vector, and will be read on the next read.
    ///
    /// All elements of `vec` are guaranteed to not satisfy `p`.
    ///
    /// Returns `Err(err)` if there is an IO error.
    ///
    pub def readUntil(p: Readable.Elm[t] -> Bool, reader: t): Result[IoError, Vector[Readable.Elm[t]]] \ Readable.Aef[t] = Peekable.readWhile(x -> not p(x), reader)

}
