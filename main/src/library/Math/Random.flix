/*
 * Copyright 2024 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

pub mod Math.Random {

    use Math.Random

    import java.util.{Random => JRandom}

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
    /// Handles the `Random` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Random` effect using the `IO` effects.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - Random) + IO =
        x -> {
            let rnd = new JRandom(); // Reuse Random instance.
            run {
                f(x)
            } with handler Random {
                def randomFloat64(k)    = k(rnd.nextDouble())
                def randomInt64(k)      = k(rnd.nextLong())
            }
        }

    ///
    /// Runs the `Random` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Random` effect using the `IO` effects.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Random) + IO = handle(f)()

    ///
    /// Runs the `Random` effect of the given function `f` from an initial seed.
    ///
    /// In other words, re-interprets the `Random` effect using seeded deterministic randomness.
    ///
    pub def handleWithSeed(seed: Int64, f: a -> b \ ef): a -> b \ ef - Random =
        // Note: Given a *specific* seed, JRandom is fully deterministic (i.e. pure).
        x -> {
            let rnd = unsafe IO { new JRandom(seed) };
            run {
                f(x)
            } with handler Random {
                def randomFloat64(k)  = k(unsafe IO { rnd.nextDouble() })
                def randomInt64(k)    = k(unsafe IO { rnd.nextLong() })
            }
        }

    ///
    /// Runs the `Random` effect of the given function `f` from an initial seed.
    ///
    /// In other words, re-interprets the `Random` effect using seeded deterministic randomness.
    ///
    pub def runWithSeed(seed: Int64, f: Unit -> a \ ef): a \ ef - Random = handleWithSeed(seed, f)()

}
