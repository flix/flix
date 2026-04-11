//
// A small "real program" fixture for the LLVM-native and LLVM-wasm backends.
//
// Exercises:
// - region-scoped arrays
// - `String.iterator` + iterator combinators
// - tuple patterns in lambdas
//
def assertEqChar(actual: Char, expected: Char, msg: String): Unit =
    if (actual == expected) () else bug!("${msg}: expected='${expected}', got='${actual}'")

def main(): Unit \ IO = region rc {
    let s = "Flix is cool!";
    let xs = Array.repeat(rc, 8, 'a');

    Iterator.forEach(match (i, x) -> {
        if (i < Array.length(xs))
            Array.put(x, i, xs)
        else
            ()
        }, String.iterator(rc, s) |> Iterator.zipWithIndex);

    let _ = assertEqChar(Array.get(0i32, xs), 'F', "xs[0]");
    let _ = assertEqChar(Array.get(1i32, xs), 'l', "xs[1]");
    let _ = assertEqChar(Array.get(2i32, xs), 'i', "xs[2]");
    let _ = assertEqChar(Array.get(3i32, xs), 'x', "xs[3]");
    let _ = assertEqChar(Array.get(4i32, xs), ' ', "xs[4]");
    let _ = assertEqChar(Array.get(5i32, xs), 'i', "xs[5]");
    let _ = assertEqChar(Array.get(6i32, xs), 's', "xs[6]");
    let _ = assertEqChar(Array.get(7i32, xs), ' ', "xs[7]");

    println("array-foreach: ok");
    ()
}

