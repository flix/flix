use Benchmark.Benchmark;
use Benchmark.defBenchmark;

///////////////////////////////////////////////////////////////////////////////
/// filter8                                                                 ///
///////////////////////////////////////////////////////////////////////////////
pub def filter8WithList(n: Int32): Benchmark =
    let l = input(n);
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

pub def filter8WithLazyList(n: Int32): Benchmark =
    let l = List.toLazy(input(n));
    defBenchmark("filter8 (LazyList)", () -> {
        l |>
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
pub def map8WithList(n: Int32): Benchmark =
    let l = input(n);
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

pub def map8WithLazyList(n: Int32): Benchmark =
    let l = List.toLazy(input(n));
    defBenchmark("map8 (LazyList)", () -> {
        l |>
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
pub def biboudisSuite(n: Int32): Array[Benchmark] & Impure = [
        filter8WithList(n),
        filter8WithLazyList(n),
        map8WithList(n),
        map8WithLazyList(n)
    ]

def main(_args: Array[String]) : Int32 & Impure =
    let s1 = biboudisSuite(100);
    Benchmark.runWithBudget(s1, 50_000_000_000i64);
    0
