/// Returns x plus one.
def inc(x: Int): Int = x + 1

/// Returns the sum of x and y.
def sum(x: Int, y: Int): Int = x + y

/// We can call these functions in the standard way:
def main(): Int =
    let i = inc(123);
    let s = sum(123, 456);
        i + s

/// Or with uniform function call syntax:
def main2(): Int =
    let i = 123.inc();
    let s = 123.sum(456);
        i + s

/// Or even using an infix notation for sum:
def main3(): Int =
    let s = 123 `sum` 456;
        s
