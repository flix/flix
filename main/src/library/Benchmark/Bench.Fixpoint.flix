namespace Bench/Exp/FixpointProjectIn {

    use Benchmark.Benchmark;
    use Benchmark.defBenchmark;

    pub def benchmarks(): Array[Benchmark] & Impure = [
        benchmarkProjectInto(),
        benchmarkQuery(),
        benchmarkSolve()
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

    pub def benchmarkSolve(): Array[Benchmark] & Impure =
        let p = #{
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        let e8   = project (List.range(1,   8) |> List.map(x -> (x, x + 1))) into Edge;
        let e16  = project (List.range(1,  16) |> List.map(x -> (x, x + 1))) into Edge;
        let e32  = project (List.range(1,  32) |> List.map(x -> (x, x + 1))) into Edge;
        let e64  = project (List.range(1,  64) |> List.map(x -> (x, x + 1))) into Edge;
        let e128 = project (List.range(1, 128) |> List.map(x -> (x, x + 1))) into Edge;
        [
            defBenchmark("Closure 8", () -> { solve p, e8 }),
            defBenchmark("Closure 16", () -> { solve p, e16 }),
            defBenchmark("Closure 32", () -> { solve p, e32 }),
            defBenchmark("Closure 64", () -> { solve p, e64 }),
            defBenchmark("Closure 128", () -> { solve p, e128 })
        ]

}
