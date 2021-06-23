use Benchmark.Benchmark;
use Benchmark.defBenchmark;

type alias IsDone = Unit ~> Bool

type alias Pull[a] = Unit ~> a

pub enum ListIterator[a] {
    case ListIterator(IsDone, Pull[a])
}

// TODO: The constant unwrapping of ListIterator is probably expensive.
// TODO: Need opaque types.
// TODO: Can we then introduce hasNext as separate functions?

namespace ListIterator {

    pub def isEmpty(iter: ListIterator[a]): Bool & Impure =
        let ListIterator(isDone, _pull) = iter;
        isDone()

    pub def count(f: a -> Bool, iter: ListIterator[a]): Int32 & Impure =
        if (isEmpty(iter))
            0
        else {
            if (f(next(iter)))
                1 + count(f, iter) // TODO: Tail call
            else
                count(f, iter)
        }



    // TODO: mapE

    // NB: Pure (!!!)
    pub def mapL(f: a -> b, iter: ListIterator[a]): ListIterator[b] =
        let ListIterator(isDone, pull) = iter;
        let pull1 = () -> f(pull());
        ListIterator(isDone, pull1)

    // TODO: A shame this cannot be pure.
    pub def filterL(f: a -> Bool, iter: ListIterator[a]): ListIterator[a] & Impure =
        let ListIterator(_isDone, pull) = iter;
        let cursor = ref None;
        let isDone1 = () -> match fastForward(f, iter) {
            case None   => true
            case Some(x) =>
                cursor := Some(x);
                false
        };
        let pull1 = () -> match deref cursor {
            case None => pull()
            case Some(x) =>
                cursor := None;
                x
        };
        ListIterator(isDone1, pull1)



    pub def toIter(l: List[a]): ListIterator[a] & Impure =
        let curr = ref l;
        let isDone = () -> List.isEmpty(deref curr);
        let pull = () -> match deref curr {
            case Nil     => bug!("Empty iterator.")
            case x :: xs =>
                curr := xs;
                x
        };
        ListIterator(isDone, pull)

    pub def toList(iter: ListIterator[a]): List[a] & Impure =
        let ListIterator(isDone, pull) = iter;
        if (isDone())
            Nil
        else
            pull() :: toList(iter)


    def next(iter: ListIterator[a]): a & Impure =
        let ListIterator(_isDone, pull) = iter;
        pull()

    def fastForward(f: a -> Bool, iter: ListIterator[a]): Option[a] & Impure =
        if (isEmpty(iter))
            None
        else {
            let n = next(iter);
            if (f(n)) Some(n) else fastForward(f, iter)
        }
}


pub def benchmarks(): Array[Benchmark] & Impure = [
        benchmarkList()
    ] |> Array.flatten

pub def benchmarkList(): Array[Benchmark] & Impure =
    let pipeline = l ->
        l |>
        List.map(x -> x + 1) |>
        List.map(x -> x + 1) |>
        List.map(x -> x + 1) |>
        List.filter(x -> x % 2 == 0) |>
        List.filter(x -> x % 2 == 0) |>
        List.filter(x -> x % 2 == 0) |>
        List.map(x -> x + 1) |>
        List.map(x -> x + 1) |>
        List.map(x -> x + 1);
    [
        defBenchmark("List (n = ${2 **  2})", () -> { pipeline(List.range(1, 2 **  2)) }),
        defBenchmark("List (n = ${2 **  4})", () -> { pipeline(List.range(1, 2 **  4)) }),
        defBenchmark("List (n = ${2 **  8})", () -> { pipeline(List.range(1, 2 **  8)) }),
        defBenchmark("List (n = ${2 ** 12})", () -> { pipeline(List.range(1, 2 ** 12)) })
    ]

//def main(_args: Array[String]) : Int32 & Impure =
//    Benchmark.runWithBudget(benchmarks(), 1_000_000_000i64)
