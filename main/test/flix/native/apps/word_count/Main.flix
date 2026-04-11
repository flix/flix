//
// A small "real program" fixture for the LLVM-native and LLVM-wasm backends.
//
// Exercises:
// - `Env.getArgs` (argv plumbing)
// - `FileReadWithResult` (filesystem I/O)
// - `String.words` / regex-backed splitting
// - region-scoped mutable collections (`MutHashMap`)
//
def expectOk(res: Result[a, b]): b with ToString[a] = match res {
    case Ok(v) => v
    case Err(e) => bug!("unexpected Err: " + ToString.toString(e))
}

def assertEqInt32(actual: Int32, expected: Int32, msg: String): Unit =
    if (actual == expected) () else bug!("${msg}: expected=${expected}, got=${actual}")

def main(): Unit \ IO =
    run {
        let args = Env.getArgs();
        let filePath = match args {
            case f :: Nil => f
            case _ => bug!("expected args: <file>")
        };

        let contents = FileReadWithResult.read(filePath) |> expectOk;
        let ws = String.words(contents);

        region rc {
            let counts = MutHashMap.empty(rc);
            List.forEach(w -> {
                let n = MutHashMap.getWithDefault(w, 0i32, counts);
                MutHashMap.put(w, n + 1i32, counts);
                ()
            }, ws);

            assertEqInt32(MutHashMap.getWithDefault("a", -1i32, counts), 2i32, "count(a)");
            assertEqInt32(MutHashMap.getWithDefault("b", -1i32, counts), 1i32, "count(b)");
            assertEqInt32(MutHashMap.getWithDefault("c", -1i32, counts), 3i32, "count(c)");
            ()
        };

        println("word-count: ok");
        ()
    } with Env.runWithIO
      with FileReadWithResult.runWithIO
