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
mod Eff {

    ///
    /// An effect used to throw a biased coin.
    ///
    pub eff BiasedCoin {

        ///
        /// Returns `true` with probability `p`.
        ///
        def flip(p: Float64): Bool

    }

    mod BiasedCoin {

        import java.util.{Random => JRandom}

        ///
        /// Handles the `BiasedCoin` effect of the given function `f`.
        ///
        /// In other words, re-interprets the `BiasedCoin` effect using the `IO` effect.
        ///
        pub def handle(f: a -> b \ ef): (a -> b \ (ef - BiasedCoin) + IO) \ IO =
            let rnd = new JRandom();
            x -> try {
                f(x)
            } with BiasedCoin {
                def flip(p, k) = k(rnd.nextDouble() <= p)
            }

        ///
        /// Runs the `BiasedCoin` effect of the given function `f`.
        ///
        /// In other words, re-interprets the `BiasedCoin` effect using the `IO` effect.
        ///
        pub def runWithHandler(f: Unit -> a \ ef): a \ (ef - BiasedCoin) + IO = handle(f)()

    }

}
