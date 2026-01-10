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

///
/// An effect used to shuffle collections.
///
eff Shuffle {

    ///
    /// Returns a permutation of integers from 0 to `len - 1`.
    ///
    /// The permutation is represented as a vector where each position
    /// contains a unique integer in the range [0, len).
    ///
    def permutation(len: Int32): Vector[Int32]

}

pub mod Shuffle {

    import java.util.{Random => JRandom}

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
    def fisherYatesShuffle(arr: Array[Int32, r]): Unit \ {r, NonDet} = unsafe IO as r {
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
