use LazyList.LazyList;
use LazyList.LazyList.{Empty, LazyCons};
/// A predicate for prime numbers
def isPrime(p: Int32): Bool =
    LazyList.from(2) |>
    LazyList.take(p - 2) |>
    LazyList.forall(x -> p % x != 0)
/// An infinite sequence of prime numbers
def primes(): LazyList[Int32] =
    LazyList.from(2) |>
    LazyList.filter(isPrime)
/// Alternative definition using sieve
def primes2(): LazyList[Int32] = sieve(LazyList.from(2))
def sieve(ps: LazyList[Int32]): LazyList[Int32] = match ps {
    case LazyCons(p, t) =>
        LazyCons(p,
            lazy sieve(LazyList.filter(x -> x % p != 0, force t)))
    case _ => Empty
}
/// Returns the first 10 prime numbers
def main(): LazyList[Int32] = LazyList.take(10, primes())