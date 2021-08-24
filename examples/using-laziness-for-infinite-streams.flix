/// A predicate for prime numbers
def isPrime(p: Int32): Bool =
    LazyList.from(2) |>
    LazyList.take(p - 2) |>
    LazyList.forall(x -> p % x != 0)
/// An infinite sequence of prime numbers

def primes(): LazyList[Int32] =
    LazyList.from(2) |>
    (LazyList.filter(isPrime) as LazyList[Int32] -> LazyList[Int32] & Pure)

/// Alternative definition using sieve
def primes2(): LazyList[Int32] = sieve(LazyList.from(2))
def sieve(ps: LazyList[Int32]): LazyList[Int32] = match LazyList.head(ps) {
    case Some(p) =>
        LCons(p,
            lazy sieve(
                LazyList.filter(x -> x % p != 0, LazyList.tail(ps)) as LazyList[Int32] & Pure
                )
            )
    case None => LazyList.empty()
}

/// Returns the first 10 prime numbers
def main(_args: Array[String]): Int32 & Impure =
    println("Using 'primes'");
    LazyList.take(10, primes()) |> LazyList.toList |> println;
    println("Using 'primes2'");
    LazyList.take(10, primes2()) |> LazyList.toList |> println;
    0 // exit code
