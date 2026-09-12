/*
 * Copyright 2024 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
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
                def randomFloat64(k) = k(rnd.nextDouble())
                def randomInt64(k)   = k(rnd.nextLong())
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
                def randomFloat64(k) = k(unsafe IO { rnd.nextDouble() })
                def randomInt64(k)   = k(unsafe IO { rnd.nextLong() })
            }
        }

    ///
    /// Runs the `Random` effect of the given function `f` from an initial seed.
    ///
    /// In other words, re-interprets the `Random` effect using seeded deterministic randomness.
    ///
    pub def runWithSeed(seed: Int64, f: Unit -> a \ ef): a \ ef - Random = handleWithSeed(seed, f)()

}
