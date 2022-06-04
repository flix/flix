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
pub def listLength(l: UnscopedArray[Int32]): Benchmark =
    defBenchmark("List.length", () -> {
        l |>
        Array.toList |>
        List.length
    } as & Pure)

pub def delayListLength(l: UnscopedArray[Int32]): Benchmark =
    defBenchmark("DelayList.length", () -> {
        l |>
        Array.toDelayList |>
        DelayList.length
    } as & Pure)

pub def streamLength(l: UnscopedArray[Int32]): Benchmark =
    defBenchmark("Stream.length", () -> {
        l |>
        Array.toStream |>
        Stream.length
    })

///////////////////////////////////////////////////////////////////////////////
/// filter length                                                           ///
///////////////////////////////////////////////////////////////////////////////
pub def listFilterLength(l: UnscopedArray[Int32]): Benchmark =
    defBenchmark("List.filterLength", () -> {
        l |>
        Array.toList |>
        List.filter(x -> x rem 2 == 0) |>
        List.length
    } as & Pure)

pub def delayListFilterLength(l: UnscopedArray[Int32]): Benchmark =
    defBenchmark("DelayList.filterLength", () -> {
        l |>
        Array.toDelayList |>
        DelayList.filter(x -> x rem 2 == 0) |>
        DelayList.length
    } as & Pure)

pub def streamFilterLength(l: UnscopedArray[Int32]): Benchmark =
    defBenchmark("Stream.filterLength", () -> {
        l |>
        Array.toStream |>
        Stream.filter(x -> x rem 2 == 0) |>
        Stream.length
    } as & Pure)

///////////////////////////////////////////////////////////////////////////////
/// filter map length                                                       ///
///////////////////////////////////////////////////////////////////////////////
pub def listFilterMapLength(l: UnscopedArray[Int32]): Benchmark =
    defBenchmark("List.filterMapLength", () -> {
        l |>
        Array.toList |>
        List.filter(x -> x rem 2 == 0) |>
        List.map(x -> x * x) |>
        List.length
    } as & Pure)

pub def delayListFilterMapLength(l: UnscopedArray[Int32]): Benchmark =
    defBenchmark("DelayList.filterMapLength", () -> {
        l |>
        Array.toDelayList |>
        DelayList.filter(x -> x rem 2 == 0) |>
        DelayList.map(x -> x * x) |>
        DelayList.length
    } as & Pure)

pub def streamFilterMapLength(l: UnscopedArray[Int32]): Benchmark =
    defBenchmark("Stream.filterMapLength", () -> {
        l |>
        Array.toStream |>
        Stream.filter(x -> x rem 2 == 0) |>
        Stream.map(x -> x * x) |>
        Stream.length
    } as & Pure)

///////////////////////////////////////////////////////////////////////////////
/// sum                                                                     ///
///////////////////////////////////////////////////////////////////////////////
pub def listSum(l: UnscopedArray[Int32]): Benchmark =
    defBenchmark("List.sum", () -> {
        l |>
        Array.toList |>
        List.sum
    } as & Pure)

pub def delayListSum(l: UnscopedArray[Int32]): Benchmark =
    defBenchmark("DelayList.sum", () -> {
        l |>
        Array.toDelayList |>
        DelayList.sum
    } as & Pure)

pub def streamSum(l: UnscopedArray[Int32]): Benchmark =
    defBenchmark("Stream.sum", () -> {
        l |>
        Array.toStream |>
        Stream.sum
    } as & Pure)

///////////////////////////////////////////////////////////////////////////////
/// filter sum                                                              ///
///////////////////////////////////////////////////////////////////////////////
pub def listFilterSum(l: UnscopedArray[Int32]): Benchmark =
    defBenchmark("List.filterSum", () -> {
        l |>
        Array.toList |>
        List.filter(x -> x rem 2 == 0) |>
        List.sum
    } as & Pure)

pub def delayListFilterSum(l: UnscopedArray[Int32]): Benchmark =
    defBenchmark("DelayList.filterSum", () -> {
        l |>
        Array.toDelayList |>
        DelayList.filter(x -> x rem 2 == 0) |>
        DelayList.sum
    } as & Pure)

pub def streamFilterSum(l: UnscopedArray[Int32]): Benchmark =
    defBenchmark("Stream.filterSum", () -> {
        l |>
        Array.toStream |>
        Stream.filter(x -> x rem 2 == 0) |>
        Stream.sum
    } as & Pure)

///////////////////////////////////////////////////////////////////////////////
/// filter map sum                                                          ///
///////////////////////////////////////////////////////////////////////////////
pub def listFilterMapSum(l: UnscopedArray[Int32]): Benchmark =
    defBenchmark("List.filterMapSum", () -> {
        l |>
        Array.toList |>
        List.filter(x -> x rem 2 == 0) |>
        List.map(x -> x * x) |>
        List.sum
    } as & Pure)

pub def delayListFilterMapSum(l: UnscopedArray[Int32]): Benchmark =
    defBenchmark("DelayList.filterMapSum", () -> {
        l |>
        Array.toDelayList |>
        DelayList.filter(x -> x rem 2 == 0) |>
        DelayList.map(x -> x * x) |>
        DelayList.sum
    } as & Pure)

pub def streamFilterMapSum(l: UnscopedArray[Int32]): Benchmark =
    defBenchmark("Stream.filterMapSum", () -> {
        l |>
        Array.toStream |>
        Stream.filter(x -> x rem 2 == 0) |>
        Stream.map(x -> x * x) |>
        Stream.sum
    } as & Pure)

///////////////////////////////////////////////////////////////////////////////
/// filter8                                                                 ///
///////////////////////////////////////////////////////////////////////////////
pub def listFilter8(l: UnscopedArray[Int32]): Benchmark =
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
    } as & Pure)

pub def delayListFilter8(l: UnscopedArray[Int32]): Benchmark =
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
    } as & Pure)

pub def streamFilter8(l: UnscopedArray[Int32]): Benchmark =
    defBenchmark("Stream.filter8", () -> {
        l |>
        Array.toStream |>
        Stream.filter(x -> x > 1) |>
        Stream.filter(x -> x > 2) |>
        Stream.filter(x -> x > 3) |>
        Stream.filter(x -> x > 4) |>
        Stream.filter(x -> x > 5) |>
        Stream.filter(x -> x > 6) |>
        Stream.filter(x -> x > 7) |>
        Stream.filter(x -> x > 8) |>
        Stream.length
    } as & Pure)

///////////////////////////////////////////////////////////////////////////////
/// map8                                                                    ///
///////////////////////////////////////////////////////////////////////////////
pub def listMap8(l: UnscopedArray[Int32]): Benchmark =
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
    } as & Pure)

pub def delayListMap8(l: UnscopedArray[Int32]): Benchmark =
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
    } as & Pure)

pub def streamMap8(l: UnscopedArray[Int32]): Benchmark =
    defBenchmark("Stream.map8", () -> {
        l |>
        Array.toStream |>
        Stream.map(x -> x + 1) |>
        Stream.map(x -> x + 2) |>
        Stream.map(x -> x + 3) |>
        Stream.map(x -> x + 4) |>
        Stream.map(x -> x + 5) |>
        Stream.map(x -> x + 6) |>
        Stream.map(x -> x + 7) |>
        Stream.map(x -> x + 8) |>
        Stream.length
    } as & Pure)

///////////////////////////////////////////////////////////////////////////////
/// flatMap take sum                                                        ///
///////////////////////////////////////////////////////////////////////////////
pub def listFlatMapTakeSum(l: UnscopedArray[Int32]): Benchmark =
    let l2 = List.range(1, 10);
    defBenchmark("List.flatMapTakeSum", () -> {
        l |>
        Array.toList |>
        List.flatMap(x -> List.map(y -> x * y, l2)) |>
        List.take(100_000) |>  // TODO
        List.sum
    } as & Pure)

pub def delayListFlatMapTakeSum(l: UnscopedArray[Int32]): Benchmark =
    let l2 = DelayList.range(1, 10);
    defBenchmark("DelayList.flatMapTakeSum", () -> {
        l |>
        Array.toDelayList |>
        DelayList.flatMap(x -> DelayList.map(y -> x * y, l2)) |>
        DelayList.take(100_000) |>  // TODO
        DelayList.sum
    } as & Pure)

pub def streamFlatMapTakeSum(l: UnscopedArray[Int32]): Benchmark =
    let l2 = Stream.range(1, 10);
    defBenchmark("Stream.flatMapTakeSum", () -> {
        l |>
        Array.toStream |>
        Stream.flatMapL(x -> Stream.map(y -> x * y, l2)) |>
        Stream.take(100_000) |>  // TODO
        Stream.sum
    } as & Pure)

///////////////////////////////////////////////////////////////////////////////
/// cartesian                                                               ///
///////////////////////////////////////////////////////////////////////////////
pub def listCartesian(l: UnscopedArray[Int32]): Benchmark =
    let l2 = List.range(1, 10);
    defBenchmark("List.cartesian", () -> {
        l |>
        Array.toList |>
        List.flatMap(x -> List.map(y -> x * y, l2)) |>
        List.sum
    } as & Pure)

pub def delayListCartesian(l: UnscopedArray[Int32]): Benchmark =
    let l2 = DelayList.range(1, 10);
    defBenchmark("DelayList.cartesian", () -> {
        l |>
        Array.toDelayList |>
        DelayList.flatMap(x -> DelayList.map(y -> x * y, l2)) |>
        DelayList.sum
    } as & Pure)

pub def streamCartesian(l: UnscopedArray[Int32]): Benchmark =
    let l2 = Stream.range(1, 10);
    defBenchmark("Stream.cartesian", () -> {
        l |>
        Array.toStream |>
        Stream.flatMapL(x -> Stream.map(y -> x * y, l2)) |>
        Stream.sum
    } as & Pure)

///////////////////////////////////////////////////////////////////////////////
/// Initial Input                                                           ///
///////////////////////////////////////////////////////////////////////////////

///
/// Returns a list of `n` concatenated lists with elements from `1` to `1_000`.
///
pub def input(n: Int32): UnscopedArray[Int32] & Impure =
    Array.range(1, n, Static) |>
    Array.flatMap(_ -> Array.range(1, 1_000, Static))

///////////////////////////////////////////////////////////////////////////////
/// Biboudis Suite                                                          ///
///////////////////////////////////////////////////////////////////////////////
pub def biboudisSuite(n: Int32): UnscopedArray[Benchmark] & Impure =
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

        // DelayList
        delayListLength(l),
        delayListFilterLength(l),
        delayListFilterMapLength(l),
        delayListSum(l),
        delayListFilterSum(l),
        delayListFilterMapSum(l),
        delayListFilter8(l),
        delayListMap8(l),
        delayListFlatMapTakeSum(l),
        delayListCartesian(l),

        // Stream
        streamLength(l),
        streamFilterLength(l),
        streamFilterMapLength(l),
        streamSum(l),
        streamFilterSum(l),
        streamFilterMapSum(l),
        streamFilter8(l),
        streamMap8(l),
        streamFlatMapTakeSum(l),
        streamCartesian(l)

    ]

def main() : Unit & Impure =
    let s1 = biboudisSuite(100);
    Benchmark.runWithBudget(s1, 250_000_000_000i64);
    ()


// TODO: Remove
namespace Stream {

    pub def sum(s: Stream[a, s]): Int32 =
        let Stream(next0, s0) = s;
        sumAcc(next0, s0, 0)

    def sumAcc(next0: s -> Step[a, s], s0: s, acc: Int32): Int32 =
        match next0(s0) {
            case Done           => acc
            case Skip(s1)       => sumAcc(next0, s1, acc)
            case Yield(_, s1)   => sumAcc(next0, s1, acc + 1)
        }

    pub def flatMapL(f: a -> Stream[a, sb], s0: Stream[a, sa]): Stream[a, (sa, Option[Stream[a, sb]])] =
        let Stream(nexta, sa0) = s0;
        let next = s -> match s {
            case (sa, None) => match nexta(sa) {
                case Done           => Done
                case Skip(sa1)      => Skip((sa1, None))
                case Yield(a, sa1)  => Skip((sa1, Some(f(a))))
            }
            case (sa, Some(Stream(nextb, sb))) => match nextb(sb) {
                case Done           => Skip(    (sa, None))
                case Skip(sb1)      => Skip(    (sa, Some(Stream(nextb, sb1))))
                case Yield(b, sb1)  => Yield(b, (sa, Some(Stream(nextb, sb1))))
            }
        };
        Stream(next, (sa0, None))


}
