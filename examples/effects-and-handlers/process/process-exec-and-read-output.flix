use Sys.Process

/// Runs a command, retrieves its stderr stream via `Process.stderr`,
/// and reads the output using the `Readable` instance on `StdErr`.
/// Note: `java -version` writes to stderr, not stdout.
def main(): Unit \ { Process, IO } = region rc {
    match Process.exec("java", "-version" :: Nil) {
        case Result.Err(err) => println("exec failed: ${err}")
        case Result.Ok(ph)   =>
            match Process.stderr(ph) {
                case Result.Err(err) => println("stderr failed: ${err}")
                case Result.Ok(err)  =>
                    let buf = Array.repeat(rc, 1024, (0i8: Int8));
                    match Readable.read(buf, err) {
                        case Result.Err(e) => println("read failed: ${e}")
                        case Result.Ok(n)  => println("Read ${n} bytes from stderr.")
                    }
            }
    }
}
