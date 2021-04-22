namespace Bench/Exp/FixpointProjectIn {

    use Benchmark.Benchmark;
    use Benchmark.defBenchmark;

    pub def benchmarks(): Array[Benchmark] & Impure = [
        benchmarkProjectInto()
    ] |> Array.flatten

    pub def benchmarkProjectInto(): Array[Benchmark] & Impure =
        let l100     = List.range(1,       100) |> List.map(x -> (x, x, x));
        let l1000    = List.range(1,     1_000) |> List.map(x -> (x, x, x));
        let l10000   = List.range(1,    10_000) |> List.map(x -> (x, x, x));
        let l100000  = List.range(1,   100_000) |> List.map(x -> (x, x, x));
        let l1000000 = List.range(1, 1_000_000) |> List.map(x -> (x, x, x));
        [
            defBenchmark("Project 100 (Int, Int, Int) into A", () -> { project l100 into A }),
            defBenchmark("Project 1000 (Int, Int, Int) into A", () -> { project l1000 into A }),
            defBenchmark("Project 10000 (Int, Int, Int) into A", () -> { project l10000 into A }),
            defBenchmark("Project 100000 (Int, Int, Int) into A", () -> { project l100000 into A }),
            defBenchmark("Project 1000000 (Int, Int, Int) into A", () -> { project l1000000 into A })
        ]

}
