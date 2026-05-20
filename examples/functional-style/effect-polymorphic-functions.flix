/// Assume we have some pure and impure functions:
def inc1(x: Int32): Int32 \ {} = x + 1
def inc2(x: Int32): Int32 \ IO = println("Hello"); x + 1

/// We can write functions that expect pure or impure functions:
def twice1(f: Int32 -> Int32 \ {}, x: Int32): Int32 \ {} = f(f(x))
def twice2(f: Int32 -> Int32 \ IO, x: Int32): Int32 \ IO = f(f(x))

/// But we can also write *effect polymorphic* functions:
def twice3(f: Int32 -> Int32 \ ef, x: Int32): Int32 \ ef = f(f(x))

/// We can use `twice3` with both pure and impure functions:
def main(): Unit \ IO =
    (twice3(inc1, 0) + twice3(inc2, 0)) |> println
