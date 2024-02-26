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
mod TestEffRandom {

    use Eff.Random.RandomCoin
    use Eff.Random.BiasedCoin

    @Test
    pub def testRandomCoin01(): Unit \ IO =
        RandomCoin.ioHandler(() -> do RandomCoin.flip());
        ()

    pub def testBiasedCoin(): Unit \ IO =
        BiasedCoin.ioHandler(() -> do BiasedCoin.flip(0.8));
        ()

}
