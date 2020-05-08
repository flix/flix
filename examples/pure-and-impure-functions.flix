/// We can declare a pure function.
def inc(x: Int): Int & Pure = x + 1

/// The pure annotation is default, so we can just write:
def inc2(x: Int): Int = x + 1

/// We can also declare an impure function.
def printAndInc(x: Int): Int & Impure =
    Console.printLine("Hello");
    x + 1

/// We can declare a function that expects a pure function:
def twice(f: Int -> Int, x: Int): Int = f(f(x))

/// We can pass a pure function to twice.
def main(): Int = twice(inc, 42)

/// But we *cannot* pass an impure function.
// def main2(): Int = twice(printAndInc, 42)
