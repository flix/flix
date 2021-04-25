/*
 * Copyright 2021 Magnus Madsen
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
namespace Benchmark {

    ///
    /// A benchmark has a name and a function to execute.
    ///
    pub enum Benchmark {
        case Benchmark({name: String, f: Unit -> Unit})
    }

    ///
    /// Returns the minimum number of iterations a benchmark can be executed.
    ///
    def minIterations(): Int32 = 1

    ///
    /// Returns the maximum number of iterations a benchmark can be executed.
    ///
    def maxIterations(): Int32 = 1_000_000

    ///
    /// The estimated ratio between a cold and warm run. Determined empirically.
    ///
    /// This number does not affect the results. Only how accurate the tool is
    /// at spending the same amount of time.
    ///
    def scaleFactor(): Int64 = 2i64

    ///
    /// A smart constructor for a benchmark.
    ///
    pub def defBenchmark(name: String, f: Unit -> a): Benchmark =
        let fn = () -> {
            let _ = f();
            ()
        };
        Benchmark({name = name, f = fn})

    pub enum BenchmarkResult {
        case Success({name: String, rounds: Int32, total: Int64})
    }

    ///
    /// Runs all benchmarks in `bs`. Attempts to spend no more than `budget` time.
    ///
    pub def runWithBudget(bs: Array[Benchmark], budget: Int64): Int32 & Impure =
        if (bs.length == 0) {
            println("No benchmarks");
            1
        } else {
            import java.lang.System:nanoTime();
            let t = nanoTime();
            printHeader();
            println("");
            let benchmarksWithEstimates = warmupAndEstimate(bs);
            println("");
            println("");
            printCols();
            runAll(benchmarksWithEstimates, budget);
            println("");
            let e = nanoTime() - t;
            println("Total time elapsed: ${toSeconds(e)} seconds. Time budget was: ${toSeconds(budget)} seconds.");
            0
        }

    ///
    /// Runs every benchmark in `bs` once and estimates its duration.
    ///
    def warmupAndEstimate(bs: Array[Benchmark]): Array[(Benchmark, Int64)] & Impure = {
        print("Warmup: ");
        bs |> Array.map(b -> {
            let time = runBenchmarkOnce(b);
            print(".");
            (b, time)
        })
    }

    ///
    /// Runs all the given benchmarks with estimates `bs` and the given time `budget`.
    ///
    def runAll(bs: Array[(Benchmark, Int64)], budget: Int64): Unit & Impure = {
        let budgetPerBenchmark = budget / (Int32.toInt64(bs.length));
        bs |> Array.foreach(match (b, cost) ->
            let iters = iterations(cost, budgetPerBenchmark);
            runBenchmark(b, iters) |> printRow
        )
    }

    ///
    /// Runs the given benchmark `b` exactly `n` times.
    ///
    def runBenchmark(b: Benchmark, n: Int32): BenchmarkResult & Impure =
        import java.lang.System:nanoTime();
        let Benchmark(r) = b;
        let t = nanoTime();
        let _ = loop(r.f, n);
        let e = nanoTime() - t;
        Success({name = r.name, rounds = n, total = e})

    ///
    /// Returns the number of nanoseconds it is estimated that a warm JVM
    /// will take to execute the given function `f`.
    ///
    /// Runs `f` once and divides the time elapsed by the `scaleFactor`.
    ///
    def runBenchmarkOnce(b: Benchmark): Int64 & Impure =
        let Benchmark(r) = b;
        import java.lang.System:nanoTime();
        let t = nanoTime();
        let _ = (r.f)();
        (nanoTime() - t) / scaleFactor()

    ///
    /// Executes the given function `f` exactly `n` times.
    ///
    def loop(f: Unit -> a, n: Int32): Unit & Impure =
        if (n == 0)
            ()
        else {
            f() as & Impure;
            loop(f, n - 1)
        }

    ///
    /// Returns the number of iterations that a benchmark should execute
    /// given an estimated `cost` and an estimated `budget`.
    ///
    def iterations(cost: Int64, budget: Int64): Int32 =
        if (cost == 0i64)
            maxIterations()
        else
            Int64.clampToInt32(budget / cost, minIterations(), maxIterations())

    ///
    /// Prints the benchmark header.
    ///
    def printHeader(): Unit & Impure = {
        println("------------------------------ Running Benchmarks ------------------------------")
    }

    ///
    /// Prints the column names.
    ///
    def printCols(): Unit & Impure = {
        let namePart = String.padRight(40, ' ', "Benchmark");
        let iterPart = String.padLeft(20, ' ', "Iterations");
        let timePart = String.padLeft(20, ' ', "Time (avg)");
        println("${namePart}${iterPart}${timePart}")
    }

    ///
    /// Prints information about the given benchmark result `s`.
    ///
    def printRow(s: BenchmarkResult): Unit & Impure = {
        let Success(r) = s;
        let avgNs = r.total / Int32.toInt64(r.rounds);
        let avgMs = toMicroSeconds(avgNs);
        let namePart = String.padRight(40, ' ', r.name);
        let iterPart = String.padLeft(20, ' ', "${r.rounds}");
        let avgTimePart = String.padLeft(20, ' ', "${avgMs} us/op");
        "${namePart}${iterPart}${avgTimePart}" |> println
    }

    ///
    /// Returns the given nano seconds `d` in micro seconds.
    ///
    def toMicroSeconds(d: Int64): Int64 = d / 1_000i64

    ///
    /// Returns the given nano seconds `d` in mili seconds.
    ///
    def toMiliSeconds(d: Int64): Int64 = toMicroSeconds(d) / 1_000i64

    ///
    /// Returns the given nano seconds `d` in seconds.
    ///
    def toSeconds(d: Int64): Int64 = toMiliSeconds(d) / 1_000i64
}
