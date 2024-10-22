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

mod Benchmark.Inliner.Optimized {

    pub def mp(xs: List[Int32], acc: List[Int32]): List[Int32] = match xs {
        case Nil     => acc
        case z :: zs => mp(zs, z + 1 :: acc)
    }

    pub def fmp(l: List[Int32], acc: List[Int32]): List[Int32] = match l {
        case Nil     => acc
        case x :: xs =>
            if (Int32.remainder(x, 2) == 0)
                fmp(xs, acc)
            else
                fmp(xs, x :: acc)
    }

    pub def rv(xs: List[a], acc: List[a]): List[a] = match xs {
        case Nil     => acc
        case z :: zs => rv(zs, z :: acc)
    }

    pub def ln(xs: List[a], acc: Int32): Int32 = match xs {
        case Nil     => acc
        case _ :: zs => ln(zs, acc + 1)
    }

    pub def rng(i: Int32, acc: List[Int32]): List[Int32] = if (i < 0) acc else rng(i - 1, i :: acc)

}
