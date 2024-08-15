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

/// A trait for data structures that can store elements at an index.
pub trait IndexMut[t] with Index[t] {

    /// The type of the index used to access the data structure.
    type Idx: Type

    /// The type of the elements you insert into the data structure.
    type Elm: Type

    ///
    /// The associated effect of the data structure which represents the effect
    /// of inserting elements.
    ///
    type Aef: Eff

    /// Insert the element `v` at index `i` in `t`.
    pub def put(i: Index.Idx[t], v: Index.Elm[t], t: t): Unit \ Index.Aef[t]

}
