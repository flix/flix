/// We can declare a pure function.
def inc(x: Int32): Int32 \ {} = x + 1

/// The pure annotation is default, so we can just write:
def inc2(x: Int32): Int32 = x + 1

/// We can also declare an impure function.
def printAndInc(x: Int32): Int32 \ IO =
    println("Hello");
    x + 1

/// We can declare a function that expects a pure function:
def twice(f: Int32 -> Int32, x: Int32): Int32 = f(f(x))

/// We can pass a pure function to twice.
pub def f(): Int32 = twice(inc, 42)

// But we *cannot* pass an impure function.
// pub def g(): Int32 = twice(printAndInc, 42)
