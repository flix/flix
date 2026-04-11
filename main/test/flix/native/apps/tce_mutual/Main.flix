//
// A small "real program" fixture for the LLVM-native and LLVM-wasm backends.
//
// Exercises:
// - mutual recursion
// - full tail call elimination (TCE)
//
def assertTrue(cond: Bool, msg: String): Unit =
    if (cond) () else bug!(msg)

def isOdd(n: Int32): Bool =
    if (n == 0i32) false else isEvn(n - 1i32)

def isEvn(n: Int32): Bool =
    if (n == 0i32) true else isOdd(n - 1i32)

def main(): Unit \ IO = {
    // Must not stack overflow if TCE is implemented correctly.
    let r = isOdd(200_001i32);
    let _ = assertTrue(r, "expected odd");
    println("tce-mutual: ok");
    ()
}

