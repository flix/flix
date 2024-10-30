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
/// An `IndexableMut` instance on a type means that it has a (potentially partial) mapping from indices
/// (`Indexable.Idx`) to elements (`Indexable.Elm`). The mapping can be modified and potentially expanded.
/// The mapping can be accessed by `Indexable.get` and modified by `put`.
///
/// If only a subset of indices are allowed to be modified (e.g. only a range of integers is valid)
/// then then `Aef` should include an error like `OutOfBounds` or `KeyNotFound`.
///
/// An instance of `IndexableMut` requires an instance of `Indexable` where the type of indices and elements
/// are defined. The associated effect here can be different from `Indexable`, because some data
/// structures might allow all modifications (like `Map`) while only defining a subset of mappings.
///
/// It must hold that `{ IndexableMut.put(i, v, x); Indexable.get(i, x) } == v` for all `i`, `v`, and `x: t`
/// with `IndexableMut[t]` if it does not stop with an effect. Note that the `get` should not fail based
/// on the key if `put` does not.
///
pub trait IndexableMut[t] with Indexable[t] {

    /// The effect of `put`.
    type Aef: Eff

    /// Insert the element `v` at index `i` in `t`, overriding the existing element if there is one.
    pub def put(t: t, i: Indexable.Idx[t], v: Indexable.Elm[t]): Unit \ IndexableMut.Aef[t]

}
