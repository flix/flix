use LazyList.LazyList;
use LazyList.LazyList.{Empty, LazyCons};

/// An infinite sequence of Fibonacci numbers
def fibs(): LazyList[Int32] =
    LazyCons(0,
        lazy LazyCons(1,
            lazy LazyList.zipWith(
                (x, y) -> x + y, fibs(), LazyList.tail(fibs()))))

/// Prints the first 10 Fibonacci numbers
def main(_: Array[String]): Int32 & Impure =
    LazyList.take(10, fibs()) |> LazyList.toList |> println;
    0
