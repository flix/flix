namespace Bench/Exp/FixpointProjectIn {

    use Benchmark.Benchmark;
    use Benchmark.defBenchmark;

    pub def benchmarks(): Array[Benchmark] & Impure = [
        benchmarkProjectInto(),
        benchmarkQuery()
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

    pub def benchmarkQuery(): Array[Benchmark] & Impure =
        let l100     = project (List.range(1,       100) |> List.map(x -> (x, x, x))) into A;
        let l1000    = project (List.range(1,     1_000) |> List.map(x -> (x, x, x))) into A;
        let l10000   = project (List.range(1,    10_000) |> List.map(x -> (x, x, x))) into A;
        //let l100000  = project (List.range(1,   100_000) |> List.map(x -> (x, x, x))) into A;
        //let l1000000 = project (List.range(1, 1_000_000) |> List.map(x -> (x, x, x))) into A;
        [
            defBenchmark("Select 100 (Int, Int, Int)", () -> { query l100 select (x, y, z) from A(x, y, z) }),
            defBenchmark("Select 1000 (Int, Int, Int)", () -> { query l1000 select (x, y, z) from A(x, y, z) }),
            defBenchmark("Select 10000 (Int, Int, Int)", () -> { query l10000 select (x, y, z) from A(x, y, z) })
            //defBenchmark("Select 100000 (Int, Int, Int)", () -> { query l100000 select (x, y, z) from A(x, y, z) })
            //defBenchmark("Select 1000000 (Int, Int, Int)", () -> { query l1000000 select (x, y, z) from A(x, y, z) })
        ]

}
