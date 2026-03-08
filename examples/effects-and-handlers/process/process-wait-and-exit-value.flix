use Sys.Process

/// Spawns a process, waits for it to finish with `Process.waitFor`,
/// and prints the exit code.
def main(): Unit \ { Process, IO } =
    match Process.exec("java", "-version" :: Nil) {
        case Result.Err(err) => println("exec failed: ${err}")
        case Result.Ok(ph)   =>
            match Process.waitFor(ph) {
                case Result.Err(err) => println("waitFor failed: ${err}")
                case Result.Ok(code) => println("Process exited with code: ${code}")
            }
    }
