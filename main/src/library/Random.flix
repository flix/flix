/*
 *  Copyright 2024 Magnus Madsen
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
/// An effect used to generate random numbers.
///
pub eff Random {

    ///
    /// Returns a pseudorandom boolean.
    ///
    def randomBool(): Bool

    ///
    /// Returns a pseudorandom 32-bit floating-point number.
    ///
    def randomFloat32(): Float32

    ///
    /// Returns a pseudorandom 64-bit floating-point number.
    ///
    def randomFloat64(): Float64

    ///
    /// Returns a pseudorandom 32-bit integer.
    ///
    def randomInt32(): Int32

    ///
    /// Returns a pseudorandom 64-bit integer.
    ///
    def randomInt64(): Int64

    ///
    /// Returns a Gaussian distributed 64-bit floating point number.
    ///
    def randomGaussian(): Float64

}

mod Random {

    import java.util.{Random => JRandom}

    ///
    /// Handles the `Random` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Random` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): (a -> b \ (ef - Random) + IO) \ IO =
        let rnd = new JRandom(); // Reuse Random instance.
        x -> try {
            f(x)
        } with Random {
            def randomBool(k)       = k(rnd.nextBoolean())
            def randomFloat32(k)    = k(rnd.nextFloat())
            def randomFloat64(k)    = k(rnd.nextDouble())
            def randomInt32(k)      = k(rnd.nextInt())
            def randomInt64(k)      = k(rnd.nextLong())
            def randomGaussian(k)   = k(rnd.nextGaussian())
        }

    ///
    /// Runs the `Random` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Random` effect using the `IO` effect.
    ///
    pub def run(f: Unit -> a \ ef): a \ (ef - Random) + IO = handle(f)()

    ///
    /// Runs the `Random` effect of the given function `f` from an initial seed.
    ///
    /// In other words, re-interprets the `Random` effect using seeded deterministic randomness.
    ///
    pub def handleWithSeed(seed: Int64, f: a -> b \ ef): (a -> b \ (ef - Random) + IO) \ IO =
        let rnd = new JRandom(seed); // Reuse Random instance.
        x -> try {
            f(x)
        } with Random {
            def randomBool(k)       = k(rnd.nextBoolean())
            def randomFloat32(k)    = k(rnd.nextFloat())
            def randomFloat64(k)    = k(rnd.nextDouble())
            def randomInt32(k)      = k(rnd.nextInt())
            def randomInt64(k)      = k(rnd.nextLong())
            def randomGaussian(k)   = k(rnd.nextGaussian())
        }

    ///
    /// Runs the `Random` effect of the given function `f` from an initial seed.
    ///
    /// In other words, re-interprets the `Random` effect using seeded deterministic randomness.
    ///
    pub def runWithSeed(seed: Int64, f: Unit -> a \ ef): a \ (ef - Random) + IO = handleWithSeed(seed, f)()

}
