/// Returns the sum of `x` and `y`.
def add(x: Int32, y: Int32): Int = x + y

/// Returns `x` plus one.
def inc(x: Int32): Int32 = add(x, 1)

/// Returns a function that applies `f` twice.
def twice(f: Int32 -> Int32): Int32 -> Int32 = x -> f(f(x))

/// Returns `x` plus two.
def two(x: Int32): Int32 = twice(inc)(x)

/// Returns 123 plus 4 = 127.
def main(_args: Array[String]): Int32 & Impure =
    println(twice(two)(123));
    0 // exit code
