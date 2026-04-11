//
// A small "real program" fixture for the LLVM-native and LLVM-wasm backends.
//
// Exercises:
// - user-defined effects and handlers
// - resumption passing (`k`) and local mutable state in handler
//
eff Counter {
    def inc(): Int32
    def get(): Int32
}

def assertEqInt32(actual: Int32, expected: Int32, msg: String): Unit =
    if (actual == expected) () else bug!("${msg}: expected=${expected}, got=${actual}")

def main(): Unit \ IO = region rc {
    let state = Ref.fresh(rc, 0i32);

    run {
        let a = Counter.inc();
        let b = Counter.inc();
        let c = Counter.get();

        let _ = assertEqInt32(a, 1i32, "a");
        let _ = assertEqInt32(b, 2i32, "b");
        let _ = assertEqInt32(c, 2i32, "c");

        println("counter-effect: ok");
        ()
    } with handler Counter {
        def inc(k) = {
            let next = Ref.get(state) + 1i32;
            Ref.put(next, state);
            k(next)
        }
        def get(k) = k(Ref.get(state))
    }
}
