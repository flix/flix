use Benchmark.Benchmark;
use Benchmark.defBenchmark;

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
        //filter8WithList(l),
        //filter8WithLazyList(l),
        //map8WithList(l),
        //map8WithLazyList(l),
        cartesianReduceList(l),
        cartesianReduceLazyList(l)
    ]

def main(_args: Array[String]) : Int32 & Impure =
    let s1 = biboudisSuite(100);
    Benchmark.runWithBudget(s1, 25_000_000_000i64);
    0
