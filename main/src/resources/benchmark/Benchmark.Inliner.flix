/*
 * Copyright 2024 Jakob Schneider Villumsen
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

mod Benchmark.Inliner {

    pub def map(f: a -> b, l: List[a]) : List[b] = {
        def mp(xs, acc) = match xs {
            case Nil     => rev(acc)
            case z :: zs => mp(zs, f(z) :: acc)
        };
        mp(l, Nil)
    }

    pub def rev(l: List[a]): List[a] = {
        def rv(xs, acc) = match xs {
            case Nil     => acc
            case z :: zs => rv(zs, z :: acc)
        };
        rv(l, Nil)
    }

    pub def range(bot: Int32, top: Int32): List[Int32] = {
        def rng(i, acc) = if (i < bot) acc else rng(i - 1, i :: acc);
        rng(top - 1, Nil)
    }

    pub def length(l: List[a]): Int32 = {
        def len(xs, acc) = match xs {
            case Nil     => acc
            case _ :: zs => len(zs, acc + 1)
        };
        len(l, 0)
    }

}
