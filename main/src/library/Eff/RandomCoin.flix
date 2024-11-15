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
    /// An effect used to throw a random coin.
    ///
    pub eff RandomCoin {

        ///
        /// Returns `true` or `false` with equal chance.
        ///
        def flip(): Bool

    }

    mod RandomCoin {

        import java.util.{Random => JRandom}

        ///
        /// Handles the `RandomCoin` effect of the given function `f`.
        ///
        /// In other words, re-interprets the `RandomCoin` effect using the `IO` effect.
        ///
        pub def handle(f: a -> b \ ef): (a -> b \ (ef - RandomCoin) + IO) \ IO =
            let rnd = new JRandom();
            x -> try {
                f(x)
            } with RandomCoin {
                def flip(k) = k(rnd.nextBoolean())
            }

        ///
        /// Runs the `RandomCoin` effect of the given function `f`.
        ///
        /// In other words, re-interprets the `RandomCoin` effect using the `IO` effect.
        ///
        pub def runWithIO(f: Unit -> a \ ef): a \ (ef - RandomCoin) + IO = handle(f)()

    }

}
