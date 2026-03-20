/*
 * Copyright 2025 Magnus Madsen
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

pub mod Math {

    ///
    /// An effect used to generate random numbers.
    ///
    pub eff Random {

        ///
        /// Returns a pseudorandom 64-bit floating-point number in the range [0.0, 1.0].
        ///
        def randomFloat64(): Float64

        ///
        /// Returns a pseudorandom 64-bit integer.
        ///
        def randomInt64(): Int64

    }

    ///
    /// An effect used to shuffle collections.
    ///
    pub eff Shuffle {

        ///
        /// Returns a permutation of integers from 0 to `len - 1`.
        ///
        /// The permutation is represented as a vector where each position
        /// contains a unique integer in the range [0, len).
        ///
        def permutation(len: Int32): Vector[Int32]

    }

}
