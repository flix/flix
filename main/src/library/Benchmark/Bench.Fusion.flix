mod Bench.Fusion {

    use Benchmark.Benchmark
    use Benchmark.defBenchmark

    ///
    /// References:
    ///
    /// https://github.com/cs-au-dk/streamliner/blob/master/src/main/java/dk/casa/streamliner/jmh/
    /// https://github.com/biboudis/streamalg/blob/master/src/main/java/benchmarks/
    ///

    pub def runBenchmarks(): Unit \ IO = region r {
        let s1 = biboudisSuite(100);
        discard Benchmark.runWithBudget(s1 |> List.toArray(r), 250_000_000_000i64);
        ()
    }

    ////////////////////////////////////////////////////////////////////////////
    /// Initial Input                                                        ///
    ////////////////////////////////////////////////////////////////////////////

    ///
    /// Returns a list of `n` concatenated lists with elements from `1` to `1_000`.
    ///
    pub def input(r: Region[r], n: Int32): Array[Int32, r] \ Write(r) =
        Array.range(r, 1, n) |>
        Array.flatMap(_ -> Array.range(r, 1, 1_000))

    ////////////////////////////////////////////////////////////////////////////
    /// Biboudis Suite                                                       ///
    ////////////////////////////////////////////////////////////////////////////
    pub def biboudisSuite(n: Int32): List[Benchmark] = region r {
        let l = input(r, n);
        // List
        listLength(l) ::
        listFilterLength(l) ::
        listFilterMapLength(l) ::
        listSum(l) ::
        listFilterSum(l) ::
        listFilterMapSum(l) ::
        listFilter8(l) ::
        listMap8(l) ::
        listFlatMapTakeSum(l) ::
        listCartesian(l) ::

        // DelayList
        delayListLength(l) ::
        delayListFilterLength(l) ::
        delayListFilterMapLength(l) ::
        delayListSum(l) ::
        delayListFilterSum(l) ::
        delayListFilterMapSum(l) ::
        delayListFilter8(l) ::
        delayListMap8(l) ::
        delayListFlatMapTakeSum(l) ::
        delayListCartesian(l) ::
        Nil
    }

    ////////////////////////////////////////////////////////////////////////////
    /// length                                                               ///
    ////////////////////////////////////////////////////////////////////////////
    pub def listLength(l: Array[Int32, r]): Benchmark =
        defBenchmark("List.length", () -> {
            l |>
            Array.toList |>
            List.length
        })

    pub def delayListLength(l: Array[Int32, r]): Benchmark =
        defBenchmark("DelayList.length", () -> {
            l |>
            Array.toDelayList |>
            DelayList.length
        })

    ////////////////////////////////////////////////////////////////////////////
    /// filter length                                                        ///
    ////////////////////////////////////////////////////////////////////////////
    pub def listFilterLength(l: Array[Int32, r]): Benchmark =
        defBenchmark("List.filterLength", () -> {
            l |>
            Array.toList |>
            List.filter(x -> x `Int32.rem` 2 == 0) |>
            List.length
        })

    pub def delayListFilterLength(l: Array[Int32, r]): Benchmark =
        defBenchmark("DelayList.filterLength", () -> {
            l |>
            Array.toDelayList |>
            DelayList.filter(x -> x `Int32.rem` 2 == 0) |>
            DelayList.length
        })

    ////////////////////////////////////////////////////////////////////////////
    /// filter map length                                                    ///
    ////////////////////////////////////////////////////////////////////////////
    pub def listFilterMapLength(l: Array[Int32, r]): Benchmark =
        defBenchmark("List.filterMapLength", () -> {
            l |>
            Array.toList |>
            List.filter(x -> x `Int32.rem` 2 == 0) |>
            List.map(x -> x * x) |>
            List.length
        })

    pub def delayListFilterMapLength(l: Array[Int32, r]): Benchmark =
        defBenchmark("DelayList.filterMapLength", () -> {
            l |>
            Array.toDelayList |>
            DelayList.filter(x -> x `Int32.rem` 2 == 0) |>
            DelayList.map(x -> x * x) |>
            DelayList.length
        })

    ////////////////////////////////////////////////////////////////////////////
    /// sum                                                                  ///
    ////////////////////////////////////////////////////////////////////////////
    pub def listSum(l: Array[Int32, r]): Benchmark =
        defBenchmark("List.sum", () -> {
            l |>
            Array.toList |>
            List.sum
        })

    pub def delayListSum(l: Array[Int32, r]): Benchmark =
        defBenchmark("DelayList.sum", () -> {
            l |>
            Array.toDelayList |>
            DelayList.sum
        })

    ////////////////////////////////////////////////////////////////////////////
    /// filter sum                                                           ///
    ////////////////////////////////////////////////////////////////////////////
    pub def listFilterSum(l: Array[Int32, r]): Benchmark =
        defBenchmark("List.filterSum", () -> {
            l |>
            Array.toList |>
            List.filter(x -> x `Int32.rem` 2 == 0) |>
            List.sum
        })

    pub def delayListFilterSum(l: Array[Int32, r]): Benchmark =
        defBenchmark("DelayList.filterSum", () -> {
            l |>
            Array.toDelayList |>
            DelayList.filter(x -> x `Int32.rem` 2 == 0) |>
            DelayList.sum
        })

    ////////////////////////////////////////////////////////////////////////////
    /// filter map sum                                                       ///
    ////////////////////////////////////////////////////////////////////////////
    pub def listFilterMapSum(l: Array[Int32, r]): Benchmark =
        defBenchmark("List.filterMapSum", () -> {
            l |>
            Array.toList |>
            List.filter(x -> x `Int32.rem` 2 == 0) |>
            List.map(x -> x * x) |>
            List.sum
        })

    pub def delayListFilterMapSum(l: Array[Int32, r]): Benchmark =
        defBenchmark("DelayList.filterMapSum", () -> {
            l |>
            Array.toDelayList |>
            DelayList.filter(x -> x `Int32.rem` 2 == 0) |>
            DelayList.map(x -> x * x) |>
            DelayList.sum
        })

    ////////////////////////////////////////////////////////////////////////////
    /// filter8                                                              ///
    ////////////////////////////////////////////////////////////////////////////
    pub def listFilter8(l: Array[Int32, r]): Benchmark =
        defBenchmark("List.filter8", () -> {
            l |>
            Array.toList |>
            List.filter(x -> x > 1) |>
            List.filter(x -> x > 2) |>
            List.filter(x -> x > 3) |>
            List.filter(x -> x > 4) |>
            List.filter(x -> x > 5) |>
            List.filter(x -> x > 6) |>
            List.filter(x -> x > 7) |>
            List.filter(x -> x > 8) |>
            List.length
        })

    pub def delayListFilter8(l: Array[Int32, r]): Benchmark =
        defBenchmark("DelayList.filter8", () -> {
            l |>
            Array.toDelayList |>
            DelayList.filter(x -> x > 1) |>
            DelayList.filter(x -> x > 2) |>
            DelayList.filter(x -> x > 3) |>
            DelayList.filter(x -> x > 4) |>
            DelayList.filter(x -> x > 5) |>
            DelayList.filter(x -> x > 6) |>
            DelayList.filter(x -> x > 7) |>
            DelayList.filter(x -> x > 8) |>
            DelayList.length
        })

    ////////////////////////////////////////////////////////////////////////////
    /// map8                                                                 ///
    ////////////////////////////////////////////////////////////////////////////
    pub def listMap8(l: Array[Int32, r]): Benchmark =
        defBenchmark("List.map8", () -> {
            l |>
            Array.toList |>
            List.map(x -> x + 1) |>
            List.map(x -> x + 2) |>
            List.map(x -> x + 3) |>
            List.map(x -> x + 4) |>
            List.map(x -> x + 5) |>
            List.map(x -> x + 6) |>
            List.map(x -> x + 7) |>
            List.map(x -> x + 8) |>
            List.length
        })

    pub def delayListMap8(l: Array[Int32, r]): Benchmark =
        defBenchmark("DelayList.map8", () -> {
            l |>
            Array.toDelayList |>
            DelayList.map(x -> x + 1) |>
            DelayList.map(x -> x + 2) |>
            DelayList.map(x -> x + 3) |>
            DelayList.map(x -> x + 4) |>
            DelayList.map(x -> x + 5) |>
            DelayList.map(x -> x + 6) |>
            DelayList.map(x -> x + 7) |>
            DelayList.map(x -> x + 8) |>
            DelayList.length
        })

    ////////////////////////////////////////////////////////////////////////////
    /// flatMap take sum                                                     ///
    ////////////////////////////////////////////////////////////////////////////
    pub def listFlatMapTakeSum(l: Array[Int32, r]): Benchmark =
        let l2 = List.range(1, 10);
        defBenchmark("List.flatMapTakeSum", () -> {
            l |>
            Array.toList |>
            List.flatMap(x -> List.map(y -> x * y, l2)) |>
            List.take(100_000) |>  // TODO
            List.sum
        })

    pub def delayListFlatMapTakeSum(l: Array[Int32, r]): Benchmark =
        let l2 = DelayList.range(1, 10);
        defBenchmark("DelayList.flatMapTakeSum", () -> {
            l |>
            Array.toDelayList |>
            DelayList.flatMap(x -> DelayList.map(y -> x * y, l2)) |>
            DelayList.take(100_000) |>  // TODO
            DelayList.sum
        })

    ////////////////////////////////////////////////////////////////////////////
    /// cartesian                                                            ///
    ////////////////////////////////////////////////////////////////////////////
    pub def listCartesian(l: Array[Int32, r]): Benchmark =
        let l2 = List.range(1, 10);
        defBenchmark("List.cartesian", () -> {
            l |>
            Array.toList |>
            List.flatMap(x -> List.map(y -> x * y, l2)) |>
            List.sum
        })

    pub def delayListCartesian(l: Array[Int32, r]): Benchmark =
        let l2 = DelayList.range(1, 10);
        defBenchmark("DelayList.cartesian", () -> {
            l |>
            Array.toDelayList |>
            DelayList.flatMap(x -> DelayList.map(y -> x * y, l2)) |>
            DelayList.sum
        })

}
