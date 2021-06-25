use Benchmark.Benchmark;
use Benchmark.defBenchmark;

///
/// The type of the done function.
///
type alias Done = Unit ~> Bool

///
/// The type of the next function.
///
type alias Next[a] = Unit ~> a

///
/// An iterator is a stream of elements that are produced on demand.
///
pub enum Iterator[a] {
    case Iterator(Done, Next[a])
}

namespace Iterator {

    ///
    /// Returns `true` if and only if the iterator `iter` has no more elements.
    ///
    /// Does *not* consume any elements of the iterator.
    ///
    pub def isEmpty(iter: Iterator[a]): Bool & Impure =
        let Iterator(done, _) = iter;
        done()

	///
    /// Returns the number of elements in `iter` that satisfies the predicate `f`.
	///
	/// Consumes the entire iterator.
    ///
    pub def count(f: a -> Bool, iter: Iterator[a]): Int32 & Impure = countAcc(f, iter, 0)

    def countAcc(f: a -> Bool, iter: Iterator[a], n: Int32): Int32 & Impure =
        if (isEmpty(iter))
            n
        else if (f(next(iter)))
            countAcc(f, iter, n + 1)
        else
            countAcc(f, iter, n)

    ///
    /// Returns an iterator without the first `n` elements of the iterator `iter`.
    ///
	/// Consumes the first `n` elements of the iterator.
    ///
    pub def drop(n: Int32, iter: Iterator[a]): Iterator[a] & Impure =
        if (n <= 0)
            iter
        else if (isEmpty(iter))
            iter
        else {
            next(iter);
            drop(n  - 1, iter)
        }

    ///
    /// Returns an iterator with the first `n` elements of the iterator `iter`.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    pub def take(n: Int32, iter: Iterator[a]): Iterator[a] & Impure =
        if (n <= 0)
            Iterator(() -> true as & Impure, () -> bug!("Empty iterator") as & Impure)
        else {
            let Iterator(done, next) = iter;
            let count = ref 0;
            let done1 = () -> deref count >= n or done();
            let next1 = () -> {
                count := (deref count + 1);
                next()
            };
            Iterator(done1, next1)
        }

	///
	/// Returns an iterator with the function `f` applied to every element of
	/// the iterator `iter`.
	///
	/// Whether `f` is applied eagerly or lazily depends on its purity:
    /// - If `f` is pure then it is applied lazily.
    /// - If `f` is impure then it is applied eagerly (consuming the iterator).
    ///
    pub def map(f: a -> b & ef, iter: Iterator[a]): Iterator[b] & Impure =
        matchEff f {
            case Pure   => mapL(f, iter)
            case Impure => mapE(f, iter)
        }

    ///
	/// Returns an iterator with the pure function `f` applied *lazily* to every
	/// element of the iterator `iter`.
	///
	/// Does *not* consume any elements from the iterator.
	///
    def mapL(f: a -> b & ef, iter: Iterator[a]): Iterator[b] =
		let Iterator(done, next) = iter;
		let next1 = () -> f(next());
		Iterator(done, next1)

	///
	/// Returns an iterator with the impure function `f` applied eagerly to
	/// every element of the iterator `iter`.
	///
	/// Consumes all elements in the iterator.
	///
    def mapE(f: a -> b & ef, iter: Iterator[a]): Iterator[b] & Impure =
		toList(iter) |> List.map(f) |> toIter

    ///
    /// Returns an iterator with every element of the iterator `iter` that
    /// satisfies the predicate `f`.
	///
	/// Whether `f` is applied eagerly or lazily depends on its purity:
	///
    /// - If `f` is pure then it is applied lazily.
    /// - If `f` is impure then it is applied eagerly (consuming the iterator).
    ///
    pub def filter(f: a -> Bool & ef, iter: Iterator[a]): Iterator[a] & Impure =
        matchEff f {
            case Pure   => filterL(f, iter)
            case Impure => filterE(f, iter)
        }

    ///
    /// Returns an iterator with every element of the iterator `iter` that
    /// satisfies the predicate `f`.
	///
	/// Does *not* consume any elements from the iterator.
	///
	pub def filterL(f: a -> Bool & ef, iter: Iterator[a]): Iterator[a] & Impure =
        let Iterator(done, next) = iter;

        // Allocate a reference to hold the next element.
        let cursor = ref None;

        let done1 = () -> match deref cursor {
            case None => match forward(f, done, next) {
                case None    => true
                case Some(x) =>
                    // An element was found. Store it in the reference.
                    cursor := Some(x);
                    false
            }
            case Some(_) => false
        };

        let next1 = () -> match deref cursor {
            case None    => bug!("Empty iterator")
            case Some(x) =>
                // An element is in the reference. Use it.
                cursor := None;
                x
        };

        Iterator(done1, next1)

    ///
    /// Returns an iterator with every element of the iterator `iter` that
    /// satisfies the predicate `f`.
	///
	/// Consume the entire iterator.
	///
	pub def filterE(f: a -> Bool & ef, iter: Iterator[a]): Iterator[a] & Impure =
		(toList(iter) |> List.filter(f as a -> Bool) |> toIter) as & Impure

    ///
    /// Alias for `findLeft`.
    ///
    pub def find(f: a -> Bool & ef, iter: Iterator[a]): Option[a] & Impure = findLeft(f, iter)

    ///
    /// Optionally returns the first element of `iter` that satisfies the
    /// predicate `f` when searching from left to right.
    ///
    pub def findLeft(f: a -> Bool & ef, iter: Iterator[a]): Option[a] & Impure =
        let Iterator(done, next) = iter;
        forward(f, done, next)

	///
	/// Returns an iterator over the elements of the given list `l`.
	///
    pub def toIter(l: List[a]): Iterator[a] & Impure =
        let cursor = ref l;
        let done = () -> List.isEmpty(deref cursor);
        let next = () -> match (deref cursor) {
            case Nil     => bug!("Empty iterator.")
            case x :: xs =>
                cursor := xs;
                x
        };
        Iterator(done, next)

	///
	/// Returns all elements in the given iterator `iter` as a list.
	///
	/// Consumes the entire iterator.
	///
    pub def toList(iter: Iterator[a]): List[a] & Impure = toListAcc(iter, Nil) |> List.reverse

	def toListAcc(iter: Iterator[a], rs: List[a]): List[a] & Impure =
		if (isEmpty(iter)) rs else toListAcc(iter, next(iter) :: rs)


	///
	/// Returns the next element of the given iterator `iter`.
	///
	/// Consumes one element from the iterator.
	///
	/// Note: The iterator _must_ have a next element.
	///
    def next(iter: Iterator[a]): a & Impure = // TODO: Avoid because it is slow to destruct multiple times?
        let Iterator(_, n) = iter;
        n()

	///
	/// Fast forwards the given iterator until the next element that satisfies
	/// the given predicate `f`. Returns it (if it exists).
	///
    def forward(f: a -> Bool & ef, done: Done, next: Next[a]): Option[a] & Impure =
        if (done())
            None
        else {
            let e = next();
            if (f(e)) Some(e) else forward(f, done, next)
        }

}


pub def benchmarks(): Array[Benchmark] & Impure = [
        benchmarkIterator(),
        benchmarkList()
    ] |> Array.flatten

pub def benchmarkList(): Array[Benchmark] & Impure =
    let pipeline = n ->
        List.range(1, n) |>
        List.map(x -> x + 1) |>
        List.map(x -> List.length(List.range(1, 100)) + x) |>
        List.map(x -> x + 1) |>
        List.filter(x -> x % 2 == 0) |>
        List.filter(x -> x % 2 == 0) |>
        List.filter(x -> x % 2 == 0) |>
        List.map(x -> x + 1) |>
        List.map(x -> List.length(List.range(1, 100)) + x) |>
        List.map(x -> x + 1) |>
        List.take(n / 4) |>
        List.count(constant(true));
    [
        defBenchmark("List (n = ${2 **  2})", () -> { pipeline(2 **  2) }),
        defBenchmark("List (n = ${2 **  4})", () -> { pipeline(2 **  4) }),
        defBenchmark("List (n = ${2 **  8})", () -> { pipeline(2 **  8) }),
        defBenchmark("List (n = ${2 ** 12})", () -> { pipeline(2 ** 12) })
    ]

pub def benchmarkIterator(): Array[Benchmark] & Impure =
    let pipeline = n ->
        List.range(1, n) |>
        Iterator.toIter |>
        Iterator.map(x -> x + 1) |>
        Iterator.map(x -> List.length(List.range(1, 100)) + x) |>
        Iterator.map(x -> x + 1) |>
        Iterator.filter(x -> x % 2 == 0) |>
        Iterator.filter(x -> x % 2 == 0) |>
        Iterator.filter(x -> x % 2 == 0) |>
        Iterator.map(x -> x + 1) |>
        Iterator.map(x -> List.length(List.range(1, 100)) + x) |>
        Iterator.map(x -> x + 1) |>
        Iterator.take(n / 4) |>
        Iterator.count(constant(true));
    [
        defBenchmark("Iterator (n = ${2 **  2})", () -> { pipeline(2 **  2) as & Pure }),
        defBenchmark("Iterator (n = ${2 **  4})", () -> { pipeline(2 **  4) as & Pure }),
        defBenchmark("Iterator (n = ${2 **  8})", () -> { pipeline(2 **  8) as & Pure }),
        defBenchmark("Iterator (n = ${2 ** 12})", () -> { pipeline(2 ** 12) as & Pure })
    ]


pub def main(_args: Array[String]) : Int32 & Impure =
    Benchmark.runWithBudget(benchmarks(), 15_000_000_000i64)

pub def main2(_args: Array[String]) : Int32 & Impure =
    List.range(1, 4) |>
    Iterator.toIter |>
    Iterator.map(x -> { println("a"); x + 1 } as & Pure) |>
    Iterator.map(x -> { println("b"); x } as & Pure) |>
    Iterator.toList;
    0

pub def main3(_args: Array[String]) : Int32 & Impure =
    let b1 = () -> {
        List.range(1, 1000) |>
        List.map(x -> List.range(1, x)) |>
        List.find(l -> List.length(l) > 10)
    };
    let b2 = () -> {
        List.range(1, 1000) |>
        Iterator.toIter |>
        Iterator.map(x -> List.range(1, x)) |>
        Iterator.find(l -> List.length(l) > 10)
    } as & Pure;
    Benchmark.runWithBudget([
        defBenchmark("Bench 1", b1),
        defBenchmark("Bench 2", b2)
    ],
    15_000_000_000i64)

