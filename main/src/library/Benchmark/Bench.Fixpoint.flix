namespace Bench/Exp/FixpointProjectIn {

    use Benchmark.Benchmark;
    use Benchmark.defBenchmark;

    pub def benchmarks(): Array[Benchmark] & Impure = [
        benchmarkProjectInto(),
        benchmarkSelect(),
        benchmarkSolve()
    ] |> Array.flatten

    pub def benchmarkProjectInto(): Array[Benchmark] & Impure =
        let mkFacts = n -> List.range(1, n) |> List.map(x -> (x, x));
        let facts04 = mkFacts(2 ** 4);
        let facts08 = mkFacts(2 ** 8);
        let facts12 = mkFacts(2 ** 12);
        let facts16 = mkFacts(2 ** 16);
        let facts20 = mkFacts(2 ** 20);
        [
            defBenchmark("Project (Int, Int) into A (n = ${2 ** 04})", () -> { project facts04 into A }),
            defBenchmark("Project (Int, Int) into A (n = ${2 ** 08})", () -> { project facts08 into A }),
            defBenchmark("Project (Int, Int) into A (n = ${2 ** 12})", () -> { project facts12 into A }),
            defBenchmark("Project (Int, Int) into A (n = ${2 ** 16})", () -> { project facts16 into A }),
            defBenchmark("Project (Int, Int) into A (n = ${2 ** 20})", () -> { project facts20 into A })
        ]

    pub def benchmarkSelect(): Array[Benchmark] & Impure =
        let mkFacts = n -> (project (List.range(1, n) |> List.map(x -> (x, x))) into A);
        let facts04 = mkFacts(2 ** 4);
        let facts08 = mkFacts(2 ** 8);
        let facts12 = mkFacts(2 ** 12);
        [
            defBenchmark("Select (Int, Int) from A (n = ${2 ** 04})", () -> { query facts04 select (x, y) from A(x, y) }),
            defBenchmark("Select (Int, Int) from A (n = ${2 ** 08})", () -> { query facts08 select (x, y) from A(x, y) }),
            defBenchmark("Select (Int, Int) from A (n = ${2 ** 12})", () -> { query facts12 select (x, y) from A(x, y) })
        ]

    pub def benchmarkSolve(): Array[Benchmark] & Impure =
        let mkEdges = n -> project (List.range(1, n) |> List.map(x -> (x, x + 1))) into Edge;
        let p = #{
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        let edges8   = mkEdges(2 ** 3);
        let edges16  = mkEdges(2 ** 4);
        let edges32  = mkEdges(2 ** 5);
        let edges64  = mkEdges(2 ** 6);
        let edges128 = mkEdges(2 ** 7);
        [
            defBenchmark("Solve (Closure) (n = ${  8 ** 2})", () -> { solve p, edges8 }),
            defBenchmark("Solve (Closure) (n = ${ 16 ** 2})", () -> { solve p, edges16 }),
            defBenchmark("Solve (Closure) (n = ${ 32 ** 2})", () -> { solve p, edges32 }),
            defBenchmark("Solve (Closure) (n = ${ 64 ** 2})", () -> { solve p, edges64 }),
            defBenchmark("Solve (Closure) (n = ${128 ** 2})", () -> { solve p, edges128 })
        ]

}
