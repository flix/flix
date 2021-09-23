use Benchmark.Benchmark;
use Benchmark.defBenchmark;

///
/// References:
///
/// https://github.com/cs-au-dk/streamliner/blob/master/src/main/java/dk/casa/streamliner/jmh/
/// https://github.com/biboudis/streamalg/blob/master/src/main/java/benchmarks/
///

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
pub def listFilterMapLength(l: List[Int32]): Benchmark =
    defBenchmark("List.filterMapLength", () -> {
        l |>
        List.filter(x -> x % 2 == 0) |>
        List.map(x -> x * x) |>
        List.length
    })

pub def lazyListFilterMapLength(l: List[Int32]): Benchmark =
    let l1 = List.toLazy(l);
    defBenchmark("LazyList.filterMapLength", () -> {
        l1 |>
        LazyList.filter(x -> x % 2 == 0) |>
        LazyList.map(x -> x * x) |>
        LazyList.length
    })

///////////////////////////////////////////////////////////////////////////////
/// sum                                                                     ///
///////////////////////////////////////////////////////////////////////////////
pub def listSum(l: List[Int32]): Benchmark =
    defBenchmark("List.sum", () -> {
        l |>
        List.foldLeft((x, y) -> x + y, 0)
    })

pub def lazyListSum(l: List[Int32]): Benchmark =
    let l1 = List.toLazy(l);
    defBenchmark("LazyList.sum", () -> {
        l1 |>
        LazyList.foldLeft((x, y) -> x + y, 0)
    })

///////////////////////////////////////////////////////////////////////////////
/// filter sum                                                              ///
///////////////////////////////////////////////////////////////////////////////
pub def listFilterSum(l: List[Int32]): Benchmark =
    defBenchmark("List.filterSum", () -> {
        l |>
        List.filter(x -> x % 2 == 0) |>
        List.foldLeft((x, y) -> x + y, 0)
    })

pub def lazyListFilterSum(l: List[Int32]): Benchmark =
    let l1 = List.toLazy(l);
    defBenchmark("LazyList.filterSum", () -> {
        l1 |>
        LazyList.filter(x -> x % 2 == 0) |>
        LazyList.foldLeft((x, y) -> x + y, 0)
    })

///////////////////////////////////////////////////////////////////////////////
/// filter map sum                                                          ///
///////////////////////////////////////////////////////////////////////////////
pub def listFilterMapSum(l: List[Int32]): Benchmark =
    defBenchmark("List.filterMapSum", () -> {
        l |>
        List.filter(x -> x % 2 == 0) |>
        List.map(x -> x * x) |>
        List.foldLeft((x, y) -> x + y, 0)
    })

pub def lazyListFilterMapSum(l: List[Int32]): Benchmark =
    let l1 = List.toLazy(l);
    defBenchmark("LazyList.filterMapSum", () -> {
        l1 |>
        LazyList.filter(x -> x % 2 == 0) |>
        LazyList.map(x -> x * x) |>
        LazyList.foldLeft((x, y) -> x + y, 0)
    })

///////////////////////////////////////////////////////////////////////////////
/// filter8                                                                 ///
///////////////////////////////////////////////////////////////////////////////
pub def listFilter8(l: List[Int32]): Benchmark =
    defBenchmark("List.filter8", () -> {
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

pub def lazyListFilter8(l: List[Int32]): Benchmark =
    let l1 = List.toLazy(l);
    defBenchmark("LazyList.filter8", () -> {
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
pub def listMap8(l: List[Int32]): Benchmark =
    defBenchmark("List.map8", () -> {
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

pub def lazyListMap8(l: List[Int32]): Benchmark =
    let l1 = List.toLazy(l);
    defBenchmark("LazyList.map8", () -> {
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
/// flatMap take sum                                                        ///
///////////////////////////////////////////////////////////////////////////////
pub def listFlatMapTakeSum(l: List[Int32]): Benchmark =
    let l2 = List.range(1, 10);
    defBenchmark("List.flatMapTakeSum", () -> {
        l |>
        List.flatMap(x -> List.map(y -> x * y, l2)) |>
        List.take(100_000) |>  // TODO
        List.foldLeft((x, y) -> x + y, 0)
    })

pub def lazyListFlatMapTakeSum(l: List[Int32]): Benchmark =
    let l1 = List.toLazy(l);
    let l2 = LazyList.range(1, 10);
    defBenchmark("LazyList.flatMapTakeSum", () -> {
        l1 |>
        LazyList.flatMap(x -> LazyList.map(y -> x * y, l2)) |>
        LazyList.take(100_000) |>  // TODO
        LazyList.foldLeft((x, y) -> x + y, 0)
    })


///////////////////////////////////////////////////////////////////////////////
/// cartesian                                                               ///
///////////////////////////////////////////////////////////////////////////////
pub def listCartesian(l: List[Int32]): Benchmark =
    let l2 = List.range(1, 10);
    defBenchmark("List.cartesian", () -> {
        l |>
        List.flatMap(x -> List.map(y -> x * y, l2)) |>
        List.foldLeft((x, y) -> x + y, 0)
    })

pub def lazyListCartesian(l: List[Int32]): Benchmark =
    let l1 = List.toLazy(l);
    let l2 = LazyList.range(1, 10);
    defBenchmark("LazyList.cartesian", () -> {
        l1 |>
        LazyList.flatMap(x -> LazyList.map(y -> x * y, l2)) |>
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
        listFilterMapLength(l),
        listSum(l),
        listFilterSum(l),
        listFilterMapSum(l),
        listFilter8(l),
        listMap8(l),
        listFlatMapTakeSum(l),
        listCartesian(l),

        // LazyList
        lazyListLength(l),
        lazyListFilterLength(l),
        lazyListFilterMapLength(l),
        lazyListSum(l),
        lazyListFilterSum(l),
        lazyListFilterMapSum(l),
        lazyListFilter8(l),
        lazyListMap8(l),
        lazyListFlatMapTakeSum(l),
        lazyListCartesian(l)
    ]

def main(_args: Array[String]) : Int32 & Impure =
    let s1 = biboudisSuite(100);
    Benchmark.runWithBudget(s1, 250_000_000_000i64);
    0
