use Fs.FileSystem

/// Uses `withReadOnly` to reject all write operations with an error.
/// Read operations succeed normally.
def main(): Unit \ { FileSystem, IO } =
    run {
        match FileSystem.write(str = "This will fail", "blocked.txt") {
            case Ok(_)    => println("Unexpected: write succeeded")
            case Err(err) => println("Write blocked: ${err}")
        };
        match FileSystem.exists("blocked.txt") {
            case Ok(b)    => println("Exists: ${b}")
            case Err(err) => println("Error: ${err}")
        }
    } with FileSystem.withReadOnly
