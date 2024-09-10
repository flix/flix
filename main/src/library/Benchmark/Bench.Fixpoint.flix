mod Bench.Fixpoint {

    use Benchmark.Benchmark
    use Benchmark.defBenchmark

    pub def runBenchmarks() : Unit \ IO = region rc {
        let s1 = Bench.Fixpoint.benchmarks();
        discard Benchmark.runWithBudget(s1 |> List.toArray(rc), 250_000_000_000i64);
        ()
    }

    pub def benchmarks(): List[Benchmark] =
        (
            benchmarkProjectInto() ::
            benchmarkSelect() ::
            benchmarkSolve() ::
            benchmarkCrossProduct2() ::
            benchmarkCrossProduct3() ::
            benchmarkSolveIntersect() ::
            benchmarkCompile() ::
            Nil
        ) |> List.flatten

    pub def benchmarkProjectInto(): List[Benchmark] =
        let mkFacts = n -> List.range(1, n) |> List.map(x -> (x, x));
        let facts04 = mkFacts(2 ** 4);
        let facts08 = mkFacts(2 ** 8);
        let facts12 = mkFacts(2 ** 12);
        let facts16 = mkFacts(2 ** 16);
        let facts20 = mkFacts(2 ** 20);
        defBenchmark("Project (Int, Int) into A (n = ${2 ** 04})", -> { project facts04 into A }) ::
        defBenchmark("Project (Int, Int) into A (n = ${2 ** 08})", -> { project facts08 into A }) ::
        defBenchmark("Project (Int, Int) into A (n = ${2 ** 12})", -> { project facts12 into A }) ::
        defBenchmark("Project (Int, Int) into A (n = ${2 ** 16})", -> { project facts16 into A }) ::
        defBenchmark("Project (Int, Int) into A (n = ${2 ** 20})", -> { project facts20 into A }) ::
        Nil

    pub def benchmarkSelect(): List[Benchmark] =
        let mkFacts = n -> (project (List.range(1, n) |> List.map(x -> (x, x))) into A);
        let facts04 = mkFacts(2 ** 4);
        let facts08 = mkFacts(2 ** 8);
        let facts12 = mkFacts(2 ** 12);
        let facts16 = mkFacts(2 ** 16);
        let facts20 = mkFacts(2 ** 20);
        defBenchmark("Select (Int, Int) from A (n = ${2 ** 04})", -> { query facts04 select (x, y) from A(x, y) }) ::
        defBenchmark("Select (Int, Int) from A (n = ${2 ** 08})", -> { query facts08 select (x, y) from A(x, y) }) ::
        defBenchmark("Select (Int, Int) from A (n = ${2 ** 12})", -> { query facts12 select (x, y) from A(x, y) }) ::
        defBenchmark("Select (Int, Int) from A (n = ${2 ** 16})", -> { query facts16 select (x, y) from A(x, y) }) ::
        defBenchmark("Select (Int, Int) from A (n = ${2 ** 20})", -> { query facts20 select (x, y) from A(x, y) }) ::
        Nil

    pub def benchmarkSolve(): List[Benchmark] =
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
        let edges256 = mkEdges(2 ** 8);
        let edges512 = mkEdges(2 ** 9);
        //let edges1024 = mkEdges(2 ** 10);
        //let edges2048 = mkEdges(2 ** 11);
        defBenchmark("Solve (Closure) (n = ${  8 ** 2})", -> { solve p, edges8 }) ::
        defBenchmark("Solve (Closure) (n = ${ 16 ** 2})", -> { solve p, edges16 }) ::
        defBenchmark("Solve (Closure) (n = ${ 32 ** 2})", -> { solve p, edges32 }) ::
        defBenchmark("Solve (Closure) (n = ${ 64 ** 2})", -> { solve p, edges64 }) ::
        defBenchmark("Solve (Closure) (n = ${128 ** 2})", -> { solve p, edges128 }) ::
        defBenchmark("Solve (Closure) (n = ${256 ** 2})", -> { solve p, edges256 }) ::
        defBenchmark("Solve (Closure) (n = ${512 ** 2})", -> { solve p, edges512 }) ::
        //defBenchmark("Solve (Closure) (n = ${1024 ** 2})", -> { solve p, edges1024 }) ::
        //defBenchmark("Solve (Closure) (n = ${2048 ** 2})", -> { solve p, edges2048 }) ::
        Nil

    pub def benchmarkCrossProduct2(): List[Benchmark] =
        let mkA = n -> project (List.range(1, n) |> List.map(x -> (0, x))) into A;
        let mkB = n -> project (List.range(1, n) |> List.map(x -> (0, x))) into B;
        let p = #{
            Cross(x, y) :- A(0, x), B(0, y).
        };
        let facts16   = mkA(16)   <+> mkB(16);
        let facts32   = mkA(32)   <+> mkB(32);
        let facts64   = mkA(64)   <+> mkB(64);
        let facts128  = mkA(128)  <+> mkB(128);
        let facts256  = mkA(256)  <+> mkB(256);
        let facts512  = mkA(512)  <+> mkB(512);
        let facts1024 = mkA(1024) <+> mkB(1024);
        //let facts2048 = mkA(2048) <+> mkB(2048);
        defBenchmark("Solve (Cross Product 2) (n = ${  16 ** 2})", -> { solve p,   facts16 }) ::
        defBenchmark("Solve (Cross Product 2) (n = ${  32 ** 2})", -> { solve p,   facts32 }) ::
        defBenchmark("Solve (Cross Product 2) (n = ${  64 ** 2})", -> { solve p,   facts64 }) ::
        defBenchmark("Solve (Cross Product 2) (n = ${ 128 ** 2})", -> { solve p,  facts128 }) ::
        defBenchmark("Solve (Cross Product 2) (n = ${ 256 ** 2})", -> { solve p,  facts256 }) ::
        defBenchmark("Solve (Cross Product 2) (n = ${ 512 ** 2})", -> { solve p,  facts512 }) ::
        defBenchmark("Solve (Cross Product 2) (n = ${1024 ** 2})", -> { solve p, facts1024 }) ::
        //defBenchmark("Solve (Cross Product 2) (n = ${2048 ** 2})", -> { solve p, facts2048 }) ::
        Nil

    pub def benchmarkCrossProduct3(): List[Benchmark] =
        let mkA = n -> project (List.range(1, n) |> List.map(x -> (0, x))) into A;
        let mkB = n -> project (List.range(1, n) |> List.map(x -> (0, x))) into B;
        let mkC = n -> project (List.range(1, n) |> List.map(x -> (0, x))) into C;
        let p = #{
            Cross(x, y, z) :- A(0, x), B(0, y), C(0, z).
        };
        let facts16   = mkA(16)  <+> mkB(16)  <+> mkC(16);
        let facts32   = mkA(32)  <+> mkB(32)  <+> mkC(32);
        let facts64   = mkA(64)  <+> mkB(64)  <+> mkC(64);
        //let facts128  = mkA(128) <+> mkB(128) <+> mkC(128);
        defBenchmark("Solve (Cross Product 3) (n = ${  16 ** 3})", -> { solve p,  facts16 }) ::
        defBenchmark("Solve (Cross Product 3) (n = ${  32 ** 3})", -> { solve p,  facts32 }) ::
        defBenchmark("Solve (Cross Product 3) (n = ${  64 ** 3})", -> { solve p,  facts64 }) ::
        // defBenchmark("Solve (Cross Product 3) (n = ${  128 ** 3})", -> { solve p,  facts128 }) ::
        Nil

    pub def benchmarkSolveIntersect(): List[Benchmark] =
        let mkA = n -> project (List.range(1, n) |> List.map(x -> (0, x))) into A;
        let mkB = n -> project (List.range(n / 2, n + n / 2) |> List.map(x -> (0, x))) into B;
        let p = #{
            C(0, x) :- A(0, x), B(0, x).
        };
        let facts04 = mkA(2 ** 4)  <+> mkB(2 ** 4);
        let facts08 = mkA(2 ** 8)  <+> mkB(2 ** 8);
        let facts12 = mkA(2 ** 12) <+> mkB(2 ** 12);
        let facts16 = mkA(2 ** 16) <+> mkB(2 ** 16);
        let facts20 = mkA(2 ** 20) <+> mkB(2 ** 20);
        //let facts21 = mkA(2 ** 21) <+> mkB(2 ** 21);
        defBenchmark("Solve (Intersect) (n = ${2 **  4})", -> { solve p, facts04 }) ::
        defBenchmark("Solve (Intersect) (n = ${2 **  8})", -> { solve p, facts08 }) ::
        defBenchmark("Solve (Intersect) (n = ${2 ** 12})", -> { solve p, facts12 }) ::
        defBenchmark("Solve (Intersect) (n = ${2 ** 16})", () -> { solve p, facts16 }) ::
        defBenchmark("Solve (Intersect) (n = ${2 ** 20})", () -> { solve p, facts20 }) ::
        //defBenchmark("Solve (Intersect) (n = ${2 ** 21})", () -> { solve p, facts21 }) ::
        Nil

    pub def benchmarkCompile(): List[Benchmark] =
        let mkRules = n -> List.range(1, n) |> List.foldLeft((acc, _) -> acc <+> #{
                A(x, y, z) :- A(z, y, x).
                A(x: Int32, y: Int32, z: Int32) :- A(x, y, z), B(x, y, z), C(x, y, z).
        }, #{});
        let rules04 = mkRules(2 ** 4);
        let rules08 = mkRules(2 ** 8);
        let rules12 = mkRules(2 ** 12);
        defBenchmark("Compile Rules (n = ${  2 ** 04})", () -> { solve rules04 }) ::
        defBenchmark("Compile Rules (n = ${  2 ** 08})", () -> { solve rules08 }) ::
        defBenchmark("Compile Rules (n = ${  2 ** 12})", () -> { solve rules12 }) ::
        Nil

}
