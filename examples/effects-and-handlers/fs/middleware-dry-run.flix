use Fs.FileSystem

/// Uses `withDryRun` to log write operations without actually performing them.
/// Read operations still work normally.
def main(): Unit \ { FileSystem, Logger, IO } =
    run {
        match FileSystem.write(str = "This won't be written", "phantom.txt") {
            case Err(err) => println("Write error: ${err}")
            case Ok(_) =>
                match FileSystem.exists("phantom.txt") {
                    case Ok(b)    => println("Exists: ${b}")
                    case Err(err) => println("Error: ${err}")
                }
        }
    } with FileSystem.withDryRun
