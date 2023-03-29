/*
 * Copyright 2023 Magnus Madsen
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
/// A module for the generation of random values.
///
/// Inspired by the Chance.js library: https://chancejs.com/
///
mod Control {

    ///
    /// An algebraic effect for the generation of a 32-bit random integer.
    ///
    pub eff Random {

        ///
        /// Returns a pseudo-random Int32 value.
        ///
        pub def random(): Unit /* Int32 */

    }

    mod Random {

        ///
        /// Randomly returns `true` or `false`.
        ///
        pub def bool(): Bool \ Random = checked_ecast(???)

        ///
        /// Randomly returns `true` or `false`.
        ///
        /// Returns `true` with the given `likelyhood` which is a number between `0.0` and `1.0`.
        ///
        pub def boolWithChance(likehood: Float64): Bool \ Random = checked_ecast(???)


    }

    ///
    /// A type class for types that can be randomly generated.
    ///
    pub class Arbitrary[t] {
        ///
        /// Returns a random t.
        ///
        pub def random(): t \ Random
    }

    instance Arbitrary[Bool] {
        pub def random(): Bool \ Random = checked_ecast(???)
    }

    // TODO: Add instances for Char
    // TODO: Add instances for Float32
    // TODO: Add instances for Float64
    // TODO: Add instances for BigDecimal
    // TODO: Add instances for Int8
    // TODO: Add instances for Int16
    // TODO: Add instances for Int32
    // TODO: Add instances for Int64
    // TODO: Add instances for BigInt
    // TODO: Add instances for String

    instance Arbitrary[(a, b)] with Arbitrary[a], Arbitrary[b] {
        pub def random(): (a, b) \ Random = (Arbitrary.random(), Arbitrary.random())
    }

    // TODO: Add instances for pairs N=1...10

    // TODO: Add instances for Option?
    // TODO: Add instances for Result?

}
