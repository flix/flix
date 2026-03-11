/*
 * Copyright 2024 Matthew Lutze
 * Copyright 2024 Jonathan Lindegaard Starup
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
/// An `Indexable` instance on a type means that it has a (partial or total) mapping from indices
/// (`Idx`) to elements (`Elm`). The mapping can be accessed by `get`.
///
/// If all indices might not be defined (e.g. only a range of integers is valid) then then `Aef`
/// should include an error like `OutOfBounds` or `KeyNotFound`.
///
/// Note that the set of defined indices can be infinite which does not naturally allow iteration.
/// An integer map with a default element would always contain all integers indices. This implies
/// that iteration would go through all integers.
///
pub trait Indexable[t] {

    /// The argument type of `get`, used to access the elements of the data structure.
    type Idx: Type

    /// The return type of `get` and the type of elements in the data structure.
    type Elm: Type

    /// The effect of `get`.
    type Aef: Eff

    /// Retrieve the element at index `i` in `t`.
    pub def get(t: t, i: Indexable.Idx[t]): Indexable.Elm[t] \ Indexable.Aef[t]

}
