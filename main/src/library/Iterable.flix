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
/// A type class for mutable data structures that can be iterated.
///
pub class Iterable[t: Type -> Region -> Type] {

    ///
    /// Returns an iterator over `t` in the same region as `t`.
    ///
    pub def iterator(t: t[a, r]): Iterator[a, r] \ Write(r)

    ///
    /// Returns an iterator over `t` in the same region as `t` zipped with the indices of the elements.
    ///
    pub def enumerator(t: t[a, r]): Iterator[(a, Int32), r] \ Write(r) =
        Iterable.iterator(t) |> Iterator.zipWithIndex

}
