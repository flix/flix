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
mod Eff.Random {

    pub eff RandomCoin {
        ///
        /// Returns `true` or `false` with equal chance.
        ///
        pub def flip(): Bool
    }

    mod RandomCoin {
        ///
        /// An IO handler for `RandomCoin`.
        ///
        pub def ioHandler(f: Unit -> a \ ef + RandomCoin): a \ ef + IO =
            let r = Random.fresh();
            try f() with RandomCoin {
                def flip(k) = k(Random.nextBool(r))
            }
    }

    pub eff BiasedCoin {
        ///
        /// Returns `true` with probability `p`.
        ///
        pub def flip(p: Float64): Bool
    }

    mod BiasedCoin {
        ///
        /// An IO handler for `RandomCoin`.
        ///
        pub def ioHandler(f: Unit -> a \ ef + BiasedCoin): a \ ef + IO =
            let r = Random.fresh();
            try f() with BiasedCoin {
                def flip(p, k) = if (Random.nextFloat64(r) < p) k(true) else k(false)
            }
    }

}
