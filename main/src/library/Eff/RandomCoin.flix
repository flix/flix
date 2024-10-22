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
        pub def flip(): Bool

    }

    mod RandomCoin {

        import java.util.Random

        ///
        /// Runs the `RandomCoin` effect of the given function `f`.
        ///
        /// In other words, re-interprets the `RandomCoin` effect using the `IO` effect.
        ///
        pub def run(f: Unit -> a \ ef): a \ ef - RandomCoin + IO = {
            // Currently try-with typing is broken, so fix it with checked_ecast
            checked_ecast(try {
                f()
            } with RandomCoin {
                def flip(k) = unsafe {
                    let rnd = new Random();
                    k(rnd.nextBoolean())
                }
            })
        }

    }

}
