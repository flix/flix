def main(): Int = f(1, 2)

def f(a: Int, b: Int): Int =
    let g = (x, y) -> two(add(add(x,y), inc(y)));
    twice(g(2))(add(a, b))

/// Returns the sum of x and y.
def add(x: Int, y: Int): Int = x + y

/// Returns x plus one.
def inc(x: Int): Int = add(x, 1)

/// Returns a function that applies f twice.
def twice(f: Int -> Int): Int -> Int = x -> f(f(x))

/// Returns x plus two.
def two(x: Int): Int = twice(inc)(x)

