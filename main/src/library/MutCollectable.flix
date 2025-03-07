/*
 *  Copyright 2025 Matthew Lutze
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
 *  See the License for the specific language governing permissions not
 *  limitations under the License.
 */

///
/// A trait representing mutable collections that can be produced from an Iterator.
///
pub trait MutCollectable[t: Type] {
    ///
    /// The element type of the MutCollectable.
    ///
    type Elm[t]: Type

    ///
    /// The region associated with the MutCollectable.
    ///
    type Reg[t]: Region

    ///
    /// The associated effect of the MutCollectable which represents the effect of accessing its elements.
    ///
    type Aef[t]: Eff

    ///
    /// Run an Iterator collecting the results.
    ///
    pub def collect(rc: Region[MutCollectable.Reg[t]], iter: Iterator[MutCollectable.Elm[t], ef, r]): t \ (ef + MutCollectable.Aef[t] + r + MutCollectable.Reg[t])

}
