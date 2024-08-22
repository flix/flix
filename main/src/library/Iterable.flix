/*
 *  Copyright 2022 Nina Andrup Pedersen
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
/// A trait for immutable data structures that can be iterated.
///
pub trait Iterable[t] {
    ///
    /// The element type of the Iterable.
    ///
    type Elm[t]: Type

    ///
    /// The associated effect of creating the Iterable and accessing the elements
    /// in the underlying data structure or generator.
    ///
    /// The effect is embedded in the type of the Iterator.
    ///
    type Aef[t]: Eff = {}

    ///
    /// Returns an iterator over `t`.
    ///
    pub def iterator(rc: Region[r], t: t): Iterator[Iterable.Elm[t], r + aef, r] \ (r + aef) where Iterable.Aef[t] ~ aef

    ///
    /// Returns an iterator over `t` zipped with the indices of the elements.
    ///
    pub def enumerator(rc: Region[r], t: t): Iterator[(Int32, Iterable.Elm[t]), r + aef, r] \ (r + aef) where Iterable.Aef[t] ~ aef =
        Iterable.iterator(rc, t) |> Iterator.zipWithIndex

    ///
    /// Returns the size of `t`.
    ///
    pub def size(t: t): Int32 \ Iterable.Aef[t] = region rc {
        t |> Iterable.iterator(rc) |> Iterator.foldLeft(acc -> _ -> acc + 1, 0)
    }

}
