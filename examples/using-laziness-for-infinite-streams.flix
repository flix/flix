/// A predicate for prime numbers
def isPrime(p: Int32): Bool =
    DelayList.startFrom(2) |>
    DelayList.take(p - 2) |>
    DelayList.forAll(x -> p `Int32.remainder` x != 0)
/// An infinite sequence of prime numbers

def primes(): DelayList[Int32] =
    DelayList.startFrom(2) |>
    (DelayList.filter(isPrime))

/// Alternative definition using sieve
def primes2(): DelayList[Int32] = sieve(DelayList.startFrom(2))
def sieve(ps: DelayList[Int32]): DelayList[Int32] = match DelayList.head(ps) {
    case Some(p) =>
        DelayList.LCons(p,
            lazy sieve(
                DelayList.filter(x -> x `Int32.remainder` p != 0, DelayList.tail(ps))
                )
            )
    case None => DelayList.empty()
}

/// Returns the first 10 prime numbers
def main(): Unit \ IO =
    println("Using 'primes'");
    DelayList.take(10, primes()) |> DelayList.toList |> println;
    println("Using 'primes2'");
    DelayList.take(10, primes2()) |> DelayList.toList |> println
