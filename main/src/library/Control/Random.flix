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

        ///
        /// Randomly returns a character from the pool 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()'.
        ///
        pub def char(): Char \ Random = checked_ecast(???)

        ///
        /// Randomly returns a character drawn from the characthers in `pool`.
        ///
        pub def charWithPool(pool: String): Char \ Random = checked_ecast(???)

        // TODO: charAlpha, charNumeric, charLower, charUpper (???)

        // TODO: Floats

        // TODO: integers

        // TODO: letter

        // TODO: natural
        pub def natural(min: Int32, max: Int32): Int32 \ Random = checked_ecast(???)

        // TODO: prime

        // TODO: String

        // TODO: sets

        ///
        /// Randomly returns an element from `l` with equal chance.
        ///
        pub def pick(l: Nel[a]): a \ Random = checked_ecast(???)

        // TODO: Shufflers.




        // TODO: Domains: geohash, date, hour, year

        //
        // Geography
        //

        ///
        /// Randomly returns city (one of the top 25 cities by population).
        ///
        pub def city(): String \ Random = checked_ecast(???)

        ///
        /// Randomly returns a country, e.g. "Denmark".
        ///
        pub def country(): String \ Random = checked_ecast(???)

        ///
        /// Randomly returns a two-letter country code following ISO 3166-1, e.g. "DK".
        ///
        pub def countryCode(): String \ Random = checked_ecast(???)

        ///
        /// Randomly returns a pair of a valid latitude and longitude.
        ///
        pub def coordinates(): String \ Random = checked_ecast(???)

        ///
        /// Randomly returns a valid latitude.
        ///
        pub def latitude(): String \ Random = checked_ecast(???)

        ///
        /// Randomly returns a valid longitude.
        ///
        pub def longitude(): String \ Random = checked_ecast(???)

        ///
        /// Randomly returns a valid geohash (see https://en.wikipedia.org/wiki/Geohash).
        ///
        pub def geohash(): String \ Random = checked_ecast(???)

        //
        // Time
        //

        ///
        /// Randomly returns an hour: an integer between [1, 12].
        ///
        pub def hour12(): Int32 \ Random = natural(1, 12 + 1)

        ///
        /// Randomly returns an hour: an integer between [1, 24].
        ///
        pub def hour24(): Int32 \ Random = natural(1, 24 + 1)

        ///
        /// Randomly returns a week day: one of ["Monday", "Tuesday", ...].
        ///
        pub def weekday(): String \ Random = pick(Nel.Nel("Monday", "Tuesday" :: Nil)) // TODO

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
