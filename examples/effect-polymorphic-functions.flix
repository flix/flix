/// Assume we have some pure and impure functions:
def inc1(x: Int): Int & Pure = x + 1
def inc2(x: Int): Int & Impure = Console.printLine("Hello"); x + 1

/// We can write functions that expect pure or impure functions:
def twice1(f: Int -> Int & Pure, x: Int): Int & Pure = f(f(x))
def twice2(f: Int -> Int & Impure, x: Int): Int & Impure = f(f(x))

/// But we can also write *effect polymorphic* functions:
def twice3(f: Int -> Int & e, x: Int): Int & e = f(f(x))

/// We can use `twice3` with both pure and impure functions:
def main(_args: Array[String]): Int32 & Impure =
    (twice3(inc1, 0) + twice3(inc2, 0)) |> println;
    0 // exit code
