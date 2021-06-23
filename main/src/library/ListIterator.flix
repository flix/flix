use Benchmark.Benchmark;
use Benchmark.defBenchmark;

///
/// The type of the isDone function.
///
type alias IsDone = Unit ~> Bool

///
/// The type of the pull function.
///
type alias Pull[a] = Unit ~> a

///
/// An iterator consists of two impure functions that can produce a (possibly
/// infinite) sequence of values.
///
pub enum ListIterator[a] {
    case ListIterator(IsDone, Pull[a])
	case FilteredIterator(a -> Bool, IsDone, Pull[a])
}

namespace ListIterator {

    ///
    /// Returns `true` if and only if the iterator `iter` has no more elements.
    ///
    pub def isEmpty(iter: ListIterator[a]): Bool & Impure = match iter {
		case ListIterator(done, _) => done()
		case FilteredIterator(f, done, next) =>
			let (done1, _) = transformFilter(f, done, next) ;
			done1()
	}

	///
    /// Returns the number of elements in `iter` that satisfy the predicate `f`.
	///
	/// Consumes the entire iterator.
    ///
    pub def count(f: a -> Bool, iter: ListIterator[a]): Int32 & Impure = countAcc(f, iter, 0)

    def countAcc(f: a -> Bool, iter: ListIterator[a], n: Int32): Int32 & Impure =
        if (isEmpty(iter))
            n
        else if (f(next(iter)))
            countAcc(f, iter, n + 1)
        else
            countAcc(f, iter, n)

    ///
    /// Returns an iterator with the first `n` elements of the given iterator `iter`.
    ///
    /// Returns the empty iterator if `iter` is empty.
	/// Returns the same iterator if `n <= 0`.
	///
	/// Consumes the first `n` elements of the iterator.
    ///
    pub def drop(n: Int32, iter: ListIterator[a]): ListIterator[a] & Impure =
        if (n <= 0)
            iter
        else if (isEmpty(iter))
            iter
        else {
            next(iter);
            drop(n  - 1, iter)
        }

	///
	/// Returns an iterator with the given function `f` applied to every element
	/// of the given iterator `iter`.
	///
	/// Whether `f` is applied eagerly or lazily depends on whether it is
	/// impure.
	///
	/// If `f` is impure it is applied immediately (consuming the entire
	/// iterator). Otherwise it is applied lazily.
	///
    pub def map(f: a -> b, iter: ListIterator[a]): ListIterator[b] & Impure =
		if (true) // TODO: Use the purity of `f` to decide which function to call.
			mapL(f, iter)
		else
			mapE(f, iter)

    ///
	/// Return an iterator with the given pure function `f` *lazily* applied to
	/// every element of the given iterator `iter`.
	///
	/// Does _not_ consume any elements from the iterator.
	///
    def mapL(f: a -> b, iter: ListIterator[a]): ListIterator[b] & Impure = match iter {
		case ListIterator(done, next) =>
		    let next1 = () -> f(next());
        	ListIterator(done, next1)
		case FilteredIterator(g, done, next) =>
			let (done1, next1) = transformFilter(g, done, next);
			let next2 = () -> f(next1());
        	ListIterator(done1, next2)
	}

	///
	/// Returns an iterator with the given impure function `f` eagerly applied
	/// to every element of the given iterator `iter`.
	///
	/// Consumes all elements in the iterator and returns a new iterator.
	///
    def mapE(f: a -> b, iter: ListIterator[a]): ListIterator[b] & Impure =
		// TODO: Rewrite to use a more efficient implementation.
		toList(iter) |> List.map(f) |> toIter

    ///
    /// Returns an iterator with every element of the iterator `iter` that
    /// satisfies the predicate `f`.
	///
	/// Whether `f` is applied eagerly or lazily depends on whether it is
	/// impure.
	///
	/// If `f` is impure it is applied immediately (consuming the entire
	/// iterator). Otherwise it is applied lazily.
    ///
    pub def filter(f: a -> Bool, iter: ListIterator[a]): ListIterator[a] =
		if (true) // TODO: Use the purity of `f` to decide which function to call.
			filterL(f, iter) as & Pure // TODO
		else
			???

    ///
    /// Returns an iterator with every element of the iterator `iter` that
    /// satisfies the predicate `f`.
	///
	/// Does _not_ consume any elements from the iterator.
	///
	pub def filterL(f: a -> Bool, iter: ListIterator[a]): ListIterator[a] & Impure = match iter {
		case ListIterator(done, next) =>
		    let pair = transformFilter(f, done, next);
        	ListIterator(pair)
		case FilteredIterator(g, done, next) =>
		    let pair = transformFilter(x -> g(x) and f(x), done, next);
        	ListIterator(pair)
	}

    ///
    /// Returns an iterator with every element of the iterator `iter` that
    /// satisfies the predicate `f`.
	///
	/// Consume the entire iterator.
	///
	pub def filterE(f: a -> Bool, iter: ListIterator[a]): ListIterator[a] & Impure =
		// TODO: Rewrite to use a more efficient implementation.
		toList(iter) |> List.filter(f) |> toIter

	///
	/// Returns an iterator over the elements of the given list `l`.
	///
    pub def toIter(l: List[a]): ListIterator[a] & Impure =
        let cursor = ref l;
        let done = () -> List.isEmpty(deref cursor);
        let next = () -> match (deref cursor) {
            case Nil     => bug!("Empty iterator.")
            case x :: xs =>
                cursor := xs;
                x
        };
        ListIterator(done, next)

	///
	/// Returns all elements in the given iterator `iter` as a list.
	///
	/// Consumes the entire iterator.
	///
    pub def toList(iter: ListIterator[a]): List[a] & Impure = toListAcc(iter, Nil) |> List.reverse

	def toListAcc(iter: ListIterator[a], rs: List[a]): List[a] & Impure =
		if (isEmpty(iter)) rs else toListAcc(iter, next(iter) :: rs)


	///
	/// Returns the next element of the given iterator `iter`.
	///
	/// Consumes one element from the iterator.
	///
	/// Note: The iterator _must_ have a next element.
	///
    def next(iter: ListIterator[a]): a & Impure = match iter {
		case ListIterator(_, n) => n()
		case FilteredIterator(f, done, next) =>
			let (_, next1) = transformFilter(f, done, next);
			next1()
	}

	///
	/// Fast forwards the given iterator until the next element that satisfies
	/// the given predicate `f`. Returns it (if it exists).
	///
    def forward(f: a -> Bool, done: IsDone, next: Pull[a]): Option[a] & Impure =
        if (done())
            None
        else {
            let n = next();
            if (f(n)) Some(n) else forward(f, done, next)
        }


    // TODO: DOC
	// TODO: Why not reutrn an iterator?
    def transformFilter(f: a -> Bool, isDone: IsDone, pull: Pull[a]): (IsDone, Pull[a]) & Impure = {
        let cursor = ref None;
        let isDone1 = () -> match forward(f, isDone, pull) {
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
        (isDone1, pull1)
    }
}


pub def benchmarks(): Array[Benchmark] & Impure = [
        benchmarkListIterator(),
        benchmarkList()
    ] |> Array.flatten

pub def benchmarkList(): Array[Benchmark] & Impure =
    let pipeline = n ->
        List.range(1, n) |>
        List.map(x -> x + 1) |>
        List.map(x -> List.length(List.range(1, 100)) + x) |>
        List.map(x -> x + 1) |>
        List.drop(n / 4) |>
        List.filter(x -> x % 2 == 0) |>
        List.filter(x -> x % 2 == 0) |>
        List.filter(x -> x % 2 == 0) |>
        List.map(x -> x + 1) |>
        List.map(x -> List.length(List.range(1, 100)) + x) |>
        List.map(x -> x + 1) |>
        List.drop(n / 2) |>
        List.count(constant(true));
    [
        defBenchmark("List (n = ${2 **  2})", () -> { pipeline(2 **  2) }),
        defBenchmark("List (n = ${2 **  4})", () -> { pipeline(2 **  4) }),
        defBenchmark("List (n = ${2 **  8})", () -> { pipeline(2 **  8) }),
        defBenchmark("List (n = ${2 ** 12})", () -> { pipeline(2 ** 12) })
    ]

pub def benchmarkListIterator(): Array[Benchmark] & Impure =
    let pipeline = n ->
        List.range(1, n) |>
        ListIterator.toIter |>
        ListIterator.map(x -> x + 1) |>
        ListIterator.map(x -> List.length(List.range(1, 100)) + x) |>
        ListIterator.map(x -> x + 1) |>
        ListIterator.drop(n / 4) |>
        ListIterator.filter(x -> x % 2 == 0) |>
        ListIterator.filter(x -> x % 2 == 0) |>
        ListIterator.filter(x -> x % 2 == 0) |>
        ListIterator.map(x -> x + 1) |>
        ListIterator.map(x -> List.length(List.range(1, 100)) + x) |>
        ListIterator.map(x -> x + 1) |>
        ListIterator.drop(n / 2) |>
        ListIterator.count(constant(true));
    [
        defBenchmark("ListIterator (n = ${2 **  2})", () -> { pipeline(2 **  2) as & Pure }),
        defBenchmark("ListIterator (n = ${2 **  4})", () -> { pipeline(2 **  4) as & Pure }),
        defBenchmark("ListIterator (n = ${2 **  8})", () -> { pipeline(2 **  8) as & Pure }),
        defBenchmark("ListIterator (n = ${2 ** 12})", () -> { pipeline(2 ** 12) as & Pure })
    ]

def main(_args: Array[String]) : Int32 & Impure =
    Benchmark.runWithBudget(benchmarks(), 15_000_000_000i64)
