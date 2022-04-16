/// An infinite sequence of Fibonacci numbers
def fibs(): DelayList[Int32] =
    LCons(0,
        lazy LCons(1,
            lazy DelayList.zipWith(
                (x, y) -> x + y, fibs(), DelayList.tail(fibs()))))

/// Prints the first 10 Fibonacci numbers
def main(): Int32 & Impure =
    DelayList.take(10, fibs()) |> DelayList.toList |> println;
    0 // exit code
