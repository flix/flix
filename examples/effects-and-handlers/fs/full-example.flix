use Fs.FileSystem

/// Demonstrates composing multiple Fs middleware via `with` clauses.
/// Stacks base directory, parent directory creation, backup, atomic writes,
/// conflict checking, and logging. Each `with` wraps the preceding block. The `FileSystem` and
/// `Logger` effects propagate to `main` and are handled automatically via
/// their default handlers.
def main(): Unit \ { FileSystem, Logger, IO } =
    run {
        match FileSystem.write(str = "Hello, Flix!", "data/greeting.txt") {
            case Err(err) => println("Write error: ${err}")
            case Ok(_) =>
                match FileSystem.read("data/greeting.txt") {
                    case Ok(content) => println("Read: ${content}")
                    case Err(err)    => println("Read error: ${err}")
                }
        }
    } with FileSystem.withBaseDir("/tmp/flix-example")
      with FileSystem.withMkParentDirs
      with FileSystem.withConflictCheck
      with FileSystem.withBackup(".bak")
      with FileSystem.withAtomicWrite
      with FileSystem.withLogging
