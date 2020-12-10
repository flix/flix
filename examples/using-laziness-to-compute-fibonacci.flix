/// An infinite sequence of Fibonacci numbers
def fibs(): LazyList[Int32] =
    LazyCons(0,
        lazy LazyCons(1,
            lazy LazyList.zipWith(
                (x, y) -> x + y, fibs(), LazyList.tail(fibs()))))
/// Returns the first 10 Fibonacci numbers
def main(): LazyList[Int32] = LazyList.take(10, fibs())