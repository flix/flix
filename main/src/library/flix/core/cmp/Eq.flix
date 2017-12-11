/*
 * Copyright 2017 Magnus Madsen
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

/**
 * Total Equality type class.
 */
pub class Eq[a] <= PartialEq[a] {

    ///
    /// Reflexive Law: An element `x` must be equal to itself.
    ///
    law reflexive(): Bool = ∀(x: a). eq(x, x)

}

// TODO: Add remaining primitive types.
// TODO: Add remaining compound types.

impl Eq[Bool]

impl Eq[Int32]

impl Eq[(a, b)] <= Eq[a], Eq[b]

impl Eq[Option[a]] <= Eq[a]

impl Eq[List[a]] <= Eq[a]

impl Eq[Result[a, b]] <= Eq[a], Eq[b]

disallow Eq[Float32]

disallow Eq[Float64]

disallow Eq[a -> b]

