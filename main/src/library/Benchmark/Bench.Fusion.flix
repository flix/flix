use Benchmark.Benchmark;
use Benchmark.defBenchmark;

///
/// References:
///
/// https://github.com/cs-au-dk/streamliner/blob/master/src/main/java/dk/casa/streamliner/jmh/
/// https://github.com/biboudis/streamalg/blob/master/src/main/java/benchmarks/
///

///////////////////////////////////////////////////////////////////////////////
/// filter8                                                                 ///
///////////////////////////////////////////////////////////////////////////////
pub def filter8WithList(l: List[Int32]): Benchmark =
    defBenchmark("filter8 (List)", () -> {
        l |>
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

pub def filter8WithLazyList(l: List[Int32]): Benchmark =
    let l1 = List.toLazy(l);
    defBenchmark("filter8 (LazyList)", () -> {
        l1 |>
        LazyList.filter(x -> x > 1) |>
        LazyList.filter(x -> x > 2) |>
        LazyList.filter(x -> x > 3) |>
        LazyList.filter(x -> x > 4) |>
        LazyList.filter(x -> x > 5) |>
        LazyList.filter(x -> x > 6) |>
        LazyList.filter(x -> x > 7) |>
        LazyList.filter(x -> x > 8) |>
        LazyList.length
    })

///////////////////////////////////////////////////////////////////////////////
/// map8                                                                    ///
///////////////////////////////////////////////////////////////////////////////
pub def map8WithList(l: List[Int32]): Benchmark =
    defBenchmark("map8 (List)", () -> {
        l |>
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

pub def map8WithLazyList(l: List[Int32]): Benchmark =
    let l1 = List.toLazy(l);
    defBenchmark("map8 (LazyList)", () -> {
        l1 |>
        LazyList.map(x -> x + 1) |>
        LazyList.map(x -> x + 2) |>
        LazyList.map(x -> x + 3) |>
        LazyList.map(x -> x + 4) |>
        LazyList.map(x -> x + 5) |>
        LazyList.map(x -> x + 6) |>
        LazyList.map(x -> x + 7) |>
        LazyList.map(x -> x + 8) |>
        LazyList.length
    })


///////////////////////////////////////////////////////////////////////////////
/// cartesian_reduce                                                        ///
///////////////////////////////////////////////////////////////////////////////
pub def cartesianReduceList(l: List[Int32]): Benchmark =
    let l2 = List.range(1, 10);
    defBenchmark("cartesian_reduce (List)", () -> {
        l |>
        List.flatMap(x -> List.map(y -> x * y, l2)) |>
        List.foldLeft((x, y) -> x + y, 0)
    })

pub def cartesianReduceLazyList(l: List[Int32]): Benchmark =
    let l1 = List.toLazy(l);
    let l2 = LazyList.range(1, 10);
    defBenchmark("cartesian_reduce (LazyList)", () -> {
        l1 |>
        LazyList.flatMap(x -> LazyList.map(y -> x * y, l2)) |>
        LazyList.foldLeft((x, y) -> x + y, 0)
    })

///////////////////////////////////////////////////////////////////////////////
/// length                                                                  ///
///////////////////////////////////////////////////////////////////////////////
pub def listLength(l: List[Int32]): Benchmark =
    defBenchmark("List.length", () -> {
        l |>
        List.length
    })

pub def lazyListLength(l: List[Int32]): Benchmark =
    let l1 = List.toLazy(l);
    defBenchmark("LazyList.length", () -> {
        l1 |>
        LazyList.length
    })

///////////////////////////////////////////////////////////////////////////////
/// filter length                                                           ///
///////////////////////////////////////////////////////////////////////////////
pub def listFilterLength(l: List[Int32]): Benchmark =
    defBenchmark("List.filterLength", () -> {
        l |>
        List.filter(x -> x % 2 == 0) |>
        List.length
    })

pub def lazyListFilterLength(l: List[Int32]): Benchmark =
    let l1 = List.toLazy(l);
    defBenchmark("LazyList.filterLength", () -> {
        l1 |>
        LazyList.filter(x -> x % 2 == 0) |>
        LazyList.length
    })

///////////////////////////////////////////////////////////////////////////////
/// filter map length                                                       ///
///////////////////////////////////////////////////////////////////////////////
pub def filterMapLengthList(l: List[Int32]): Benchmark =
    defBenchmark("filter map length (List)", () -> {
        l |>
        List.filter(x -> x % 2 == 0) |>
        List.map(x -> x * x) |>
        List.length
    })

pub def filterMapLengthLazyList(l: List[Int32]): Benchmark =
    let l1 = List.toLazy(l);
    defBenchmark("filter Map length (LazyList)", () -> {
        l1 |>
        LazyList.filter(x -> x % 2 == 0) |>
        LazyList.map(x -> x * x) |>
        LazyList.length
    })


///////////////////////////////////////////////////////////////////////////////
/// flatMap take sum                                                        ///
///////////////////////////////////////////////////////////////////////////////
pub def flatMapTakeSumList(l: List[Int32]): Benchmark =
    let l2 = List.range(1, 10);
    defBenchmark("flatMapTakeSum (List)", () -> {
        l |>
        List.flatMap(x -> List.map(y -> x * y, l2)) |>
        List.take(100_000) |>  // TODO
        List.foldLeft((x, y) -> x + y, 0)
    })

pub def flatMapTakeSumLazyList(l: List[Int32]): Benchmark =
    let l1 = List.toLazy(l);
    let l2 = LazyList.range(1, 10);
    defBenchmark("flatMapTakeSumLazy (List)", () -> {
        l1 |>
        LazyList.flatMap(x -> LazyList.map(y -> x * y, l2)) |>
        LazyList.take(100_000) |>  // TODO
        LazyList.foldLeft((x, y) -> x + y, 0)
    })


///////////////////////////////////////////////////////////////////////////////
/// reduce                                                                  ///
///////////////////////////////////////////////////////////////////////////////
pub def reduceList(l: List[Int32]): Benchmark =
    defBenchmark("reduce (List)", () -> {
        l |>
        List.foldLeft((x, y) -> x + y, 0)
    })

pub def reduceLazyList(l: List[Int32]): Benchmark =
    let l1 = List.toLazy(l);
    defBenchmark("reduce (LazyList)", () -> {
        l1 |>
        LazyList.foldLeft((x, y) -> x + y, 0)
    })

///////////////////////////////////////////////////////////////////////////////
/// filter_reduce                                                           ///
///////////////////////////////////////////////////////////////////////////////
pub def filterReduceList(l: List[Int32]): Benchmark =
    defBenchmark("filterReduce (List)", () -> {
        l |>
        List.filter(x -> x % 2 == 0) |>
        List.foldLeft((x, y) -> x + y, 0)
    })

pub def filterReduceLazyList(l: List[Int32]): Benchmark =
    let l1 = List.toLazy(l);
    defBenchmark("filterReduce (LazyList)", () -> {
        l1 |>
        LazyList.filter(x -> x % 2 == 0) |>
        LazyList.foldLeft((x, y) -> x + y, 0)
    })

///////////////////////////////////////////////////////////////////////////////
/// filter_map_reduce                                                       ///
///////////////////////////////////////////////////////////////////////////////
pub def filterMapReduceList(l: List[Int32]): Benchmark =
    defBenchmark("filterMapReduce (List)", () -> {
        l |>
        List.filter(x -> x % 2 == 0) |>
        List.map(x -> x * x) |>
        List.foldLeft((x, y) -> x + y, 0)
    })

pub def filterMapReduceLazyList(l: List[Int32]): Benchmark =
    let l1 = List.toLazy(l);
    defBenchmark("filterMapReduce (LazyList)", () -> {
        l1 |>
        LazyList.filter(x -> x % 2 == 0) |>
        LazyList.map(x -> x * x) |>
        LazyList.foldLeft((x, y) -> x + y, 0)
    })

///////////////////////////////////////////////////////////////////////////////
/// Initial Input                                                           ///
///////////////////////////////////////////////////////////////////////////////

///
/// Returns a list of `n` concatenated lists with elements from `1` to `1_000`.
///
pub def input(n: Int32): List[Int32] =
    List.range(1, n) |>
    List.flatMap(_ -> List.range(1, 1_000))

///////////////////////////////////////////////////////////////////////////////
/// Biboudis Suite                                                          ///
///////////////////////////////////////////////////////////////////////////////
pub def biboudisSuite(n: Int32): Array[Benchmark] & Impure =
    let l = input(n);
    [
        // List
        listLength(l),
        listFilterLength(l),

        filterMapLengthList(l),
        reduceList(l),
        filterReduceList(l),
        filterMapReduceList(l),
        filter8WithList(l),
        map8WithList(l),
        cartesianReduceList(l),
        flatMapTakeSumList(l),

        // LazyList
        lazyListLength(l),
        lazyListFilterLength(l),

        filter8WithLazyList(l),
        map8WithLazyList(l),
        filterMapLengthLazyList(l),
        flatMapTakeSumLazyList(l),
        reduceLazyList(l),
        filterReduceLazyList(l),
        cartesianReduceLazyList(l),
        filterMapReduceLazyList(l)
    ]

def main(_args: Array[String]) : Int32 & Impure =
    let s1 = biboudisSuite(100);
    Benchmark.runWithBudget(s1, 25_000_000_000i64);
    0
