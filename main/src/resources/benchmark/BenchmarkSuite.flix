/*
 * Copyright 2024 Magnus Madsen
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
import java.lang.System

///
/// Main entry point.
///
def main(): Unit \ IO =
    let results = runAllBenchmarks(benchmarks(), 100, 1000);
    println(toJSON(results))

///
/// Computes the median running time of every benchmark with `k` samples and `n` runs in each sample.
///
def runAllBenchmarks(m: Map[String, Unit -> Unit \ IO], k: Int32, n: Int32): Map[String, Int64] \ IO =
    Map.map(f -> runBenchmarkAndComputeMedian(f, k, n), m)

///
/// Returns the *median* running time of `f` over `k` samples with `n` runs in each sample.
///
def runBenchmarkAndComputeMedian(f: Unit -> Unit \ IO, k: Int32, n: Int32): Int64 \ IO =
    List.range(0, k) |> List.map(_ -> runSingleBenchmark(f, n)) |> median

///
/// Returns the time, in nanoseconds, it takes to run `f` exactly `n` times.
///
def runSingleBenchmark(f: Unit -> Unit \ IO, n: Int32): Int64 \ IO =
    let t = System.nanoTime();
    iterateN(f, n);
    System.nanoTime() - t

///
/// Runs the function `f` exactly `n` times.
///
def iterateN(f: Unit -> Unit \ IO, n: Int32): Unit \ IO = match n {
        case 0 => ()
        case _ => {f(); iterateN(f, n - 1)}
    }

///
/// Consumes `t` in a way that the optimizer cannot discard.
///
def blackhole(t: a): Unit \ IO =
    Ref.fresh(Static, t); ()

///
/// Returns the median of the given list `l`.
///
def median(l: List[Int64]): Int64 =
    List.sort(l) |> List.drop(List.length(l) / 2) |> List.head |> Option.getWithDefault(0i64)

///
/// Returns the given map `m` as a JSON string.
///
def toJSON(m: Map[String, Int64]): String =
    let kv = Map.joinWith((k, v) -> "  { \"name\": \"${k}\", \"time\": ${v} }", ",\n", m);
    "[\n${kv}\n]"

mod InlinerBenchmarkNormal {

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

mod InlinerBenchmarkInlined {

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
