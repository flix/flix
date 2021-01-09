/// Returns x plus one.
def inc(x: Int): Int = x + 1

/// Returns the sum of x and y.
def sum(x: Int, y: Int): Int = x + y

/// We can call these functions in the standard way:
def main(_args: Array[String]): Int32 & Impure =
    let i = inc(123);
    let s = sum(123, 456);
    println(i + s);
    0 // exit code

/// Or with uniform function call syntax:
def main2(_args: Array[String]): Int32 & Impure =
    let i = 123.inc();
    let s = 123.sum(456);
    println(i + s);
    0 // exit code

/// Or even using an infix notation for sum:
def main3(_args: Array[String]): Int32 & Impure =
    let s = 123 `sum` 456;
    println(s);
    0 // exit code
