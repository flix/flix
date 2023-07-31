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
/// A type class for data structures that can be iterated.
///
pub class Iterable[t: Type] {

    type Effect: Eff
    type Elem: Type

    ///
    /// Returns an iterator over `t`.
    ///
    pub def iterator(rc: Region[r], t: t): Iterator[Iterable.Elem[t], Iterable.Effect[t] + r, r] \ r

    ///
    /// Returns an iterator over `t` zipped with the indices of the elements.
    ///
    pub def enumerator(rc: Region[r], t: t): Iterator[(Int32, Iterable.Elem[t]), Iterable.Effect[t] + r, r] \ r =
        Iterable.iterator(rc, t) |> Iterator.zipWithIndex

}
