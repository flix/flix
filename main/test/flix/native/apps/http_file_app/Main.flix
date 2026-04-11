//
// A small "real program" fixture for the LLVM-native backend.
//
// Exercises:
// - multi-file compilation (calls `Test.Native.HttpFileApp.Parse.extractToken`)
// - `Env.getArgs` (argv plumbing)
// - `FileReadWithResult` (filesystem I/O)
// - `HttpWithResult` (network I/O)
//
def expectOk(res: Result[a, b]): b with ToString[a] = match res {
    case Ok(v) => v
    case Err(e) => bug!("unexpected Err: " + ToString.toString(e))
}

def main(): Unit \ IO =
    run {
        let args = Env.getArgs();
        let (url, filePath) = match args {
            case u :: f :: Nil => (u, f)
            case _ => bug!("expected args: <url> <file>")
        };

        let expectedToken =
            FileReadWithResult.read(filePath)
                |> expectOk
                |> String.trim;

        let resp = HttpWithResult.get(url, Map.empty()) |> expectOk;
        let _ =
            if (Http.Response.status(resp) != 200i32)
                bug!("unexpected status: " + Int32.toString(Http.Response.status(resp)))
            else ();

        let body = Http.Response.body(resp);
        let actualToken = NativeHttpFileParse.extractToken(body) |> expectOk;

        let _ =
            if (actualToken != expectedToken)
                bug!("token mismatch: expected=${expectedToken}, actual=${actualToken}")
            else ();

        println("native-http-file: ok");
        ()
    } with Env.runWithIO
      with FileReadWithResult.runWithIO
      with HttpWithResult.runWithIO
