 /*
 * Copyright 2023 Magnus Madsen, Xavier deSouza
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
        /// Returns `true` with the given `likelihood` which is a number between `0.0` and `1.0`.
        ///
        pub def boolWithChance(likelihood: Float64): Bool \ Random = checked_ecast(???)


        ///
        /// Randomly returns `head` or `tail`
        ///
        pub def flipCoin(): String \ Random = checked_ecast(???)

        ///
        /// Randomly returns "head" or "tail", based on the `likelihood` of "head" from `0.0` to `1.0`
        ///
        pub def flipWeightedCoin(likelihood: Float64): String \ Random = checked_ecast(???)

        ///
        /// Randomly returns a natural number from 1-6
        ///
        pub def rollDie(): Int32 \ Random = checked_ecast(???)

        ///
        /// Randomly returns a natural number from 1-n
        ///
        pub def rollNSidedDie(n: Int32): Int32 \ Random = checked_ecast(???)

        ///
        /// Randomly returns the sum of a value `v` and a natural number from 1-n
        ///
        pub def rollNSidedDiePlusValue(n: Int32, v:Int32): String \ Random = checked_ecast(???)

        ///
        /// Randomly returns a character from the pool 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()'.
        ///
        pub def char(): Char \ Random = checked_ecast(???)

        ///
        /// Randomly returns a character drawn from the characthers in `pool`.
        ///
        pub def charWithPool(pool: String): Char \ Random = checked_ecast(???)

        ///
        /// Randomly returns a character between the range of `lo` and `hi`, inclusive
        ///
        pub def charWithRange(low: Int32, hi: Int32): Char \ Random = checked_ecast(???)

        ///
        /// Randomly returns any char value (0-255)
        /// NOTE: Return type may have to be `int`
        pub def charAny(): Char \ Random = checked_ecast(???)

        ///
        /// Randomly returns a numeric (0-9) character
        ///
        pub def charNumeric(): Char \ Random = checked_ecast(???)

        ///
        /// Randomly returns a lowercase alphabetic charater
        ///
        pub def charLower(): Char \ Random = checked_ecast(???)

        ///
        /// Randomly returns an uppercase alphabetic character
        ///
        pub def charUpper(): Char \ Random = checked_ecast(???)

        ///
        /// Randomly returns a semipronouncable syllable
        ///NOTE: will possibly be dank to implement. at least 1 vowel for every 2 consonants?
        pub def syllable(): String \ Random = checked_ecast(???)

        ///
        /// Randomly returns a string of specified length comprised of alphanumeric chars
        ///
        pub def string(length: Int32): String \ Random = checked_ecast(???)

        ///
        /// Randomly returns a string of specified length comprised of any ASCII values
        ///
        pub def asciiString(length: Int32): String \ Random = checked_ecast(???)

        ///
        /// Randomly returns one of the top (TODO) most popular first names
        ///
        pub def firstName(): String \ Random = checked_ecast(???)

        ///
        /// Randomly returns one of the top (TODO) most popular last names
        ///
        pub def lastName(): String \ Random = checked_ecast(???)

        ///
        /// Randomly returns a first followed by a last name (separated by ' ')
        ///
        pub def fullName(): String \ Random = checked_ecast(???)

        ///
        /// Randomly returns a first, middle and last name (each separated by ' ')
        ///
        pub def fullNameWithMiddle(): String \ Random = checked_ecast(???)

        ///
        /// Randomly returns a plant name.
        ///
        pub def plant(): String \ Random = checked_ecast(???)

        ///
        /// Randomly returns an animal name.
        ///
        pub def animal(): String \ Random = checked_ecast(???)

        ///
        /// Randomly returns an 8-bit integer
        ///
        pub def int8(): Int8 \ Random = checked_ecast(???)

        ///
        /// Randomly returns a 32-bit integer
        ///
        pub def int32(): Int32 \ Random = checked_ecast(???)

        ///
        /// Randomly returns a 64-bit integer
        ///
        pub def int64(): Int32 \ Random = checked_ecast(???)

        ///
        /// Randomly returns a natural number (which can be zero)
        ///
        pub def natural(min: Int32, max: Int32): Int32 \ Random = checked_ecast(???)

        ///
        /// Randomly returns a natural number in the range [min, max]
        ///
        pub def naturalWithRange(min: Int32, max: Int32): Int32 \ Random = checked_ecast(???)

        ///
        /// Returns a random prime number.
        ///NOTE: Runtime?
        pub def prime(): Int32 \ Random = checked_ecast(???)

        ///
        /// Optionally returns a random prime number which is equal-to or larger-than `min`.
        ///NOTE: Runtime?
        pub def primeWithMin(min: Int32): Option[Int32] \ Random = checked_ecast(???)

        ///
        /// Optionally returns a random prime number which is smaller-than or equal-to `max`.
        ///NOTE: Runtime?
        pub def primeWithMax(max: Int32): Option[Int32] \ Random = checked_ecast(???)

        ///
        /// Optionally returns a random prime number which is between `min` and `max` (both inclusive).
        ///NOTE: Runtime?
        pub def primeWithMinMax(min: Int32, max: Int32): Option[Int32] \ Random = checked_ecast(???)


        ///
        /// Returns a random 32-bit floating point number
        ///
        pub def float32(): Float32 \ Random = checked_ecast(???)

        ///
        /// Returns a random 64-bit floating point number
        pub def float64(): Float64 \ Random = checked_ecast(???)

        ///
        /// Returns a random amount of currency (_.xx)
        ///NOTE: consider formatting: Int32 or tuples?
        pub def currency(): Float32 \ Random = checked_ecast(???)

        ///
        /// Returns a random L + ratio
        ///Note: Consider (Int32, Int32) format instead of floating point
        pub def ratio(): Float32 \ Random = checked_ecast(???)

        // TODO: sets

        ///
        /// Randomly returns an element from `l` with equal chance.
        ///
        pub def pick(l: Nel[a]): a \ Random = checked_ecast(???)

        // TODO: Shufflers.

        //
        // Geography
        // Note: should some of these be floats (i.e. latitude, longitude)?

        ///
        /// Randomly returns city (one of the top 25 cities by population).
        ///
        pub def city(): String \ Random = checked_ecast(???)

        ///
        /// Randomly returns a country, e.g. "Denmark".
        ///
        pub def country(): String \ Random = checked_ecast(???)
        //hardcode list of like 120 countries?

        ///
        /// Randomly returns a two-letter country code following ISO 3166-1, e.g. "DK".
        ///
        pub def countryCode(): String \ Random = checked_ecast(???)

        ///
        /// Randomly returns a pair of a valid latitude and longitude.
        ///
        pub def coordinates(): String \ Random = checked_ecast(???)
        //merely valid LL ranges, or ones that are relevant(i.e., in a country, on land)?

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
        /// Randomly returns a number of minutes
        ///
        pub def minutes(): Int32 \ Random = checked_ecast(???)

        ///
        /// Randomly returns a number of seconds
        ///TODO: combine with minutes? code will be the same
        pub def seconds(): Int32 \ Random = checked_ecast(???)

        ///
        /// Randomly returns a time consisting of hours and minutes
        ///
        pub def timeHM(): (Int32, Int32) \ Random = checked_ecast(???)

        ///
        /// Randomly returns a time consisting of hours, minutes, and seconds
        ///
        pub def timeHMS(): (Int32, Int32, Int32) \ Random = checked_ecast(???)

        ///
        /// Randomly returns a week day: one of ["Monday", "Tuesday", ...].
        ///
        pub def weekday(): String \ Random = pick(Nel.Nel("Monday", "Tuesday" :: Nil)) // TODO

        ///
        /// Randomly returns a month
        ///
        pub def month(): String \ Random = checked_ecast(???)

        ///
        /// Randomly returns a day of a given month
        ///NOTE: watch for month day limits (e.g. February has 28 days)
        pub def dayMonth(): (Int32, String) \ Random = checked_ecast(???)

        ///
        /// Randomly returns a year after 0 C.E.
        ///
        pub def year(): Int32 \ Random = checked_ecast(???)

        ///
        /// Randomly returns a date comprised of a day, month, and a year
        ///NOTE: account for edge case possibilities (e.g. leap years)
        pub def dayMonthYear(): (Int32, String, Int32) \ Random = checked_ecast(???)
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

    instance Arbitrary[Char] {
        pub def random(): Char \ Random = checked_ecast(???)
    }

    instance Arbitrary[Float32] {
        pub def random(): Float32 \ Random = checked_ecast(???)
    }

    instance Arbitrary[Float64] {
        pub def random(): Float64 \ Random = checked_ecast(???)
    }

    instance Arbitrary[BigDecimal] {
        pub def random(): BigDecimal \ Random = checked_ecast(???)
    }

    instance Arbitrary[Int8] {
        pub def random(): Int8 \ Random = checked_ecast(???)
    }

    instance Arbitrary[Int32] {
        pub def random(): Int32 \ Random = checked_ecast(???)
    }

    instance Arbitrary[Int64] {
        pub def random(): Int64 \ Random = checked_ecast(???)
    }

    instance Arbitrary[BigInt] {
        pub def random(): BigInt \ Random = checked_ecast(???)
    }

    instance Arbitrary[String] { //specify size?
        pub def random(): String \ Random = checked_ecast(???)
    }

    instance Arbitrary[(a, b)] with Arbitrary[a], Arbitrary[b] {
        pub def random(): (a, b) \ Random = (Arbitrary.random(), Arbitrary.random())
    }

    instance Arbitrary[(a, b, c)] with Arbitrary[a], Arbitrary[b], Arbitrary[c] {
        pub def random(): (a, b, c) \ Random = (Arbitrary.random(), Arbitrary.random(), Arbitrary.random())
    }

    instance Arbitrary[(a, b, c, d)] with Arbitrary[a], Arbitrary[b], Arbitrary[c], Arbitrary[d] {
        pub def random(): (a, b, c, d) \ Random = (Arbitrary.random(), Arbitrary.random(), Arbitrary.random(), Arbitrary.random())
    }

    instance Arbitrary[(a, b, c, d, e)] with Arbitrary[a], Arbitrary[b], Arbitrary[c], Arbitrary[d], Arbitrary[e] {
        pub def random(): (a, b, c, d, e) \ Random =
            (Arbitrary.random(), Arbitrary.random(), Arbitrary.random(), Arbitrary.random(), Arbitrary.random())
    }

    instance Arbitrary[(a, b, c, d, e, f)] with Arbitrary[a], Arbitrary[b], Arbitrary[c], Arbitrary[d], Arbitrary[e], Arbitrary[f] {
        pub def random(): (a, b, c, d, e, f) \ Random =
            (Arbitrary.random(), Arbitrary.random(), Arbitrary.random(), Arbitrary.random(), Arbitrary.random(), Arbitrary.random())
    }

    instance Arbitrary[(a, b, c, d, e, f, g)] with Arbitrary[a], Arbitrary[b], Arbitrary[c], Arbitrary[d], Arbitrary[e], Arbitrary[f], Arbitrary[g] {
        pub def random(): (a, b, c, d, e, f, g) \ Random =
            (Arbitrary.random(), Arbitrary.random(), Arbitrary.random(), Arbitrary.random(), Arbitrary.random(), Arbitrary.random(), Arbitrary.random())
    }

    instance Arbitrary[(a, b, c, d, e, f, g, h)] with
        Arbitrary[a], Arbitrary[b], Arbitrary[c], Arbitrary[d], Arbitrary[e], Arbitrary[f], Arbitrary[g], Arbitrary[h] {

        pub def random(): (a, b, c, d, e, f, g, h) \ Random =
            (Arbitrary.random(), Arbitrary.random(), Arbitrary.random(), Arbitrary.random(),
             Arbitrary.random(), Arbitrary.random(), Arbitrary.random(), Arbitrary.random())
    }

    instance Arbitrary[(a, b, c, d, e, f, g, h, i)] with
        Arbitrary[a], Arbitrary[b], Arbitrary[c], Arbitrary[d], Arbitrary[e], Arbitrary[f], Arbitrary[g], Arbitrary[h], Arbitrary[i] {

        pub def random(): (a, b, c, d, e, f, g, h, i) \ Random =
            (Arbitrary.random(), Arbitrary.random(), Arbitrary.random(), Arbitrary.random(), Arbitrary.random(),
             Arbitrary.random(), Arbitrary.random(), Arbitrary.random(), Arbitrary.random())
    }

    instance Arbitrary[(a, b, c, d, e, f, g, h, i, j)] with
        Arbitrary[a], Arbitrary[b], Arbitrary[c], Arbitrary[d], Arbitrary[e], Arbitrary[f], Arbitrary[g], Arbitrary[h], Arbitrary[i], Arbitrary[j] {

        pub def random(): (a, b, c, d, e, f, g, h, i, j) \ Random =
            (Arbitrary.random(), Arbitrary.random(), Arbitrary.random(), Arbitrary.random(), Arbitrary.random(),
             Arbitrary.random(), Arbitrary.random(), Arbitrary.random(), Arbitrary.random(), Arbitrary.random())
    }

    instance Arbitrary[Option[t]] with Arbitrary[t] {
        pub def random(): Option[t] \ Random = if (Control.Random.bool()) None else Some(Arbitrary.random())
    }

    instance Arbitrary[Result[e, t]] with Arbitrary[t] {
        pub def random(): Result[e, t] \ Random =
            if (Control.Random.bool()) Ok(Arbitrary.random()) else Err(???) //TODO:
    }

    //Return a List of 0-5 random elements of type `t`
    instance Arbitrary[List[t]] with Arbitrary[t] {
        pub def random(): List[t] \ Random =
            let consXTimes = (x, l) -> if (x == 0) l else List.Cons(Arbitrary.random(), l);
            consXTimes(Control.Random.rollDie() - 1, Nil)
    }

    //Return a Set of 0-5 random elements of type `t`
    instance Arbitrary[Set[t]] with Arbitrary[t], Order[t] {
        pub def random(): Set[t] \ Random =
            let insertXTimes = (x, l) -> if (x == 0) l else Set.insert(Arbitrary.random(), l);
            insertXTimes(Control.Random.rollDie() - 1, Set.empty())
    }

    //Return a Map of 0-5 random key value pairs
    instance Arbitrary[Map[k, v]] with Arbitrary[k], Arbitrary[v], Order[k] {
        pub def random(): Map[k, v] \ Random =
            let insertXTimes = (x, m) -> if (x == 0) m else Map.insert(Arbitrary.random(), Arbitrary.random(), m);
            insertXTimes(Control.Random.rollDie() - 1, Map.empty())
    }
}
