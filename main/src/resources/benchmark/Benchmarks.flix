/*
 * Copyright 2024 Magnus Madsen, Jakob Schneider Villumsen
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
/// Returns the list of benchmarks.
///
/// A benchmark has a name and a function to evaluate.
///
def benchmarks(): Map[String, Unit -> Unit \ IO] = Map#{
    "List.filter"    => () -> List.range(0, 10_000) |> List.filter(x -> Int32.modulo(x, 2) == 0) |> blackhole,
    "List.foldLeft"  => () -> List.range(0, 10_000) |> List.foldLeft(Add.add, 0) |> blackhole,
    "List.foldRight" => () -> List.range(0, 10_000) |> List.foldRight(Add.add, 0) |> blackhole,
    "List.map"       => () -> List.range(0, 10_000) |> List.map(x -> x + 1) |> blackhole,
    "List.length"    => () -> List.range(0, 10_000) |> List.length |> blackhole,
    "List.reverse"   => () -> List.range(0, 10_000) |> List.reverse |> blackhole,
    "InlinerMap100k" => () -> {
        let l = InlinerBenchmark.range(0, 100_000);
        let l2 = InlinerBenchmark.map(x -> x + 1, l);
        println(InlinerBenchmark.length(l2))
    },
    "InlinerMap100kOptimized" => () -> {
        let top = 99999; // 100_000 - 1
        let l = InlinerBenchmarkOptimized.rng(top, Nil);
        let l2 = InlinerBenchmarkOptimized.mp(l, Nil);
        let l3 = InlinerBenchmarkOptimized.rv(l2, Nil);
        let l4 = InlinerBenchmarkOptimized.ln(l3, 0);
        println(l4)
    }
}

mod InlinerBenchmarkOptimized {

    pub def mp(xs: List[Int32], acc: List[Int32]): List[Int32] = match xs {
        case Nil     => acc
        case z :: zs => mp(zs, z + 1 :: acc)
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
