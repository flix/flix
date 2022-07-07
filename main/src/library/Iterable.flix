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
pub class Iterable[t: Type -> Type] {

    ///
    /// Returns an iterator of `t`.
    ///
    pub def iterator(r: Region[r], t: t[a]): Iterator[a, r] \ Write(r)

    ///
    /// Returns an iterator over `t` zipped with the indices of the elements.
    ///
    pub def enumerator(r: Region[r], t: t[a]): Iterator[(a, Int32), r] \ Write(r) =
        Iterable.iterator(r, t) |> Iterator.zipWithIndex

}
