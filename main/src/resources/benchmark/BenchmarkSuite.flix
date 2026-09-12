/*
 * Copyright 2024 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

import java.lang.System

///
/// Main entry point.
///
def main(): Unit \ IO =
    let results = runAllBenchmarks(benchmarks(), 100, 10_000);
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
    discard Ref.fresh(Static, t)

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
