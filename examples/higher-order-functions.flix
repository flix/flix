/// Returns the sum of x and y.
def add(x: Int, y: Int): Int = x + y

/// Returns x plus one.
def inc(x: Int): Int = add(x, 1)

/// Returns a function that applies f twice.
def twice(f: Int -> Int): Int -> Int = x -> f(f(x))

/// Returns x plus two.
def two(x: Int): Int = twice(inc)(x)

/// Returns 123 plus 4 = 127.
def main(_: Array[String]): Int32 & Impure =
    twice(two)(123) |> println;
    0 // exit code
