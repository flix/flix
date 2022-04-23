/// A predicate for prime numbers
def isPrime(p: Int32): Bool =
    DelayList.from(2) |>
    DelayList.take(p - 2) |>
    DelayList.forall(x -> p rem x != 0)
/// An infinite sequence of prime numbers

def primes(): DelayList[Int32] =
    DelayList.from(2) |>
    (DelayList.filter(isPrime))

/// Alternative definition using sieve
def primes2(): DelayList[Int32] = sieve(DelayList.from(2))
def sieve(ps: DelayList[Int32]): DelayList[Int32] = match DelayList.head(ps) {
    case Some(p) =>
        LCons(p,
            lazy sieve(
                DelayList.filter(x -> x rem p != 0, DelayList.tail(ps))
                )
            )
    case None => DelayList.empty()
}

/// Returns the first 10 prime numbers
def main(): Unit & Impure =
    println("Using 'primes'");
    DelayList.take(10, primes()) |> DelayList.toList |> println;
    println("Using 'primes2'");
    DelayList.take(10, primes2()) |> DelayList.toList |> println
