use Benchmark.Benchmark;
use Benchmark.defBenchmark;

def listSnoc1(xs: List[a], x: a): List[a] = listSnoc1Helper(xs, x, ks -> ks)

def listSnoc1Helper(xs: List[a], x: a, k: List[a] -> List[a]): List[a] = match xs {
    case Nil => k(x :: Nil)
    case x1 :: rs => listSnoc1Helper(rs, x, ks -> k(x1 :: ks))
}

def listSnocMany(xs: List[a], ys: List[a]): List[a] = match ys {
    case Nil      => xs
    case y1 :: rs => listSnocMany(listSnoc1(xs, y1), rs)
}

def chainSnocMany(xs: Chain[a], ys: List[a]): Chain[a] = match ys {
    case Nil      => xs
    case y1 :: rs => chainSnocMany(Chain.snoc(xs, y1), rs)
}

def listConsMany(xs: List[a], ys: List[a]): List[a] = match ys {
    case Nil      => xs
    case y1 :: rs => listConsMany(y1 :: xs, rs)
}

def chainConsMany(xs: Chain[a], ys: List[a]): Chain[a] = match ys {
    case Nil      => xs
    case y1 :: rs => chainConsMany(Chain.cons(y1, xs), rs)
}

def listAppendMany(xs: List[a], ys: List[List[a]]): List[a] = match ys {
    case Nil       => xs
    case ys1 :: rs => listAppendMany(List.append(xs, ys1), rs)
}

def chainAppendMany(xs: Chain[a], ys: List[Chain[a]]): Chain[a] = match ys {
    case Nil       => xs
    case ys1 :: rs => chainAppendMany(Chain.append(xs, ys1), rs)
}

///////////////////////////////////////////////////////////////////////////////
/// ofArray                                                               ///
///////////////////////////////////////////////////////////////////////////////
pub def listOfArray(l: Array[Int32, r]): Benchmark =
    defBenchmark("List.ofArray", () -> {
        l |>
        Array.toList
    } as & Pure)

pub def chainOfArray(l: Array[Int32, r]): Benchmark =
    defBenchmark("Chain.ofArray", () -> {
        l |>
        Array.toChain
    } as & Pure)


///////////////////////////////////////////////////////////////////////////////
/// cons                                                                    ///
///////////////////////////////////////////////////////////////////////////////
pub def listCons(l: Array[Int32, r]): Benchmark =
    defBenchmark("List.cons", () -> {
        l |>
        Array.toList |>
        listConsMany(Nil)
    } as & Pure)

pub def chainCons(l: Array[Int32, r]): Benchmark =
    defBenchmark("Chain.cons", () -> {
        l |>
        Array.toList |>
        chainConsMany(Chain.empty())
    } as & Pure)

///////////////////////////////////////////////////////////////////////////////
/// snoc                                                                    ///
///////////////////////////////////////////////////////////////////////////////
pub def listSnoc(l: Array[Int32, r]): Benchmark =
    defBenchmark("List.snoc", () -> {
        l |>
        Array.toList |>
        listSnocMany(Nil)
    } as & Pure)

pub def chainSnoc(l: Array[Int32, r]): Benchmark =
    defBenchmark("Chain.snoc", () -> {
        l |>
        Array.toList |>
        chainSnocMany(Chain.empty())
    } as & Pure)

///////////////////////////////////////////////////////////////////////////////
/// append                                                                    ///
///////////////////////////////////////////////////////////////////////////////
pub def listAppend(l: Array[Int32, r]): Benchmark =
    defBenchmark("List.append", () -> {
        let l1 = Array.toList(l);
        listAppendMany(l1, List.map(List.repeat(3), l1))
    } as & Pure)

pub def chainAppend(l: Array[Int32, r]): Benchmark =
    defBenchmark("Chain.append", () -> {
        let c1 = Array.toChain(l);
        let l1 = Array.toList(l);
        chainAppendMany(c1, List.map(Chain.repeat(3), l1))
    } as & Pure)


///////////////////////////////////////////////////////////////////////////////
/// Chain Suite                                                             ///
///////////////////////////////////////////////////////////////////////////////
pub def chainSuite(r: Region[r], n: Int32): Array[Benchmark, r] \ Write(r) =
    let l = Array.range(r, 1, n);
    [
        // Consing and Array.toList / Array.toChain should be pretty much the same

        // List
        listOfArray(l),
        listCons(l),
        listSnoc(l),
        listAppend(l),

        // Chain
        chainOfArray(l),
        chainCons(l),
        chainSnoc(l),
        chainAppend(l)
    ] @ r

def main() : Unit & Impure = region r {
    let s1 = chainSuite(r, 1000);
    Benchmark.runWithBudget(s1, 250_000_000_000i64);
    ()
}

