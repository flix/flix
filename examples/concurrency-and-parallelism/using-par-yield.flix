def parSum(): (Int32, Int32) =
   par (x <- 1 + 2; y <- 3 + 4)
        yield (x, y)

def main(): Unit \ IO =
    println(parSum())
