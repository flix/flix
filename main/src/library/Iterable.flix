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
/// The type of the done function.
///
type alias StaticDone = Unit -> Bool & Impure

///
/// The type of the next function.
///
type alias StaticNext[a] = Unit -> a & Impure

///
/// An iterator is a stream of elements that are produced on demand.
///
pub enum StaticIterator[a] {
    case StaticIterator(StaticDone, StaticNext[a])
}

///
/// A type class for data structures that can be iterated.
///
pub class Iterable[t: Type -> Type] {

    ///
    /// Returns an iterator of `t`.
    ///
    pub def iterator(t: t[a]): StaticIterator[a] & Impure

}
