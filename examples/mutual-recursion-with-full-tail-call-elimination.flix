/// Flix, despite being a JVM-language,
/// supports full tail call elimination.

/// We can demonstrate this with a naive implementation
/// of a program that computes if a number is odd.

/// Returns true if n is odd.
def isOdd(n: Int32): Bool =
    if (n == 0) false else isEvn(n - 1)

/// Returns true if n is even.
def isEvn(n: Int32): Bool =
    if (n == 0) true else isOdd(n - 1)

/// We can now compute if 12345 is odd.
/// In a language without TCE this would
/// quickly consume all stack space.
def main(): Unit \ IO =
    isOdd(12345) |> println
