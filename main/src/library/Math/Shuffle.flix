/*
 * Copyright 2025 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Math.Shuffle {

    use Math.Shuffle

    import java.util.{Random => JRandom}

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

    ///
    /// Handles the `Shuffle` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Shuffle` effect using the `NonDet` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - Shuffle) + NonDet = x ->
        run {
            f(x)
        } with handler Shuffle {
            def permutation(len, k) = {
                region rc {
                    let arr = Array.range(rc, 0, len);
                    fisherYatesShuffle(arr);
                    k(Array.toVector(arr))
                }
            }
        }

    ///
    /// Runs the `Shuffle` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Shuffle` effect using the `NonDet` effect.
    ///
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Shuffle) + NonDet = handle(f)()

    ///
    /// Fisher-Yates shuffle algorithm for arrays.
    ///
    def fisherYatesShuffle(arr: Array[Int32, r]): Unit \ { r, NonDet } = unsafe IO as r {
        let rnd = new JRandom();
        let len = Array.length(arr);
        def loop(i) = {
            if (i >= len - 1)
                ()
            else {
                let j = i + rnd.nextInt(len - i);
                let temp = Array.get(i, arr);
                Array.put(Array.get(j, arr), i, arr);
                Array.put(temp, j, arr);
                loop(i + 1)
            }
        };
        loop(0)
    }

}
