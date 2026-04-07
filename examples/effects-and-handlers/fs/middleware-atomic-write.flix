use Fs.FileSystem

/// Uses `withAtomicWrite` to write via a temp file and atomic rename,
/// preventing partial writes on failure.
def main(): Unit \ { FileSystem, IO } =
    run {
        match FileSystem.write(str = "Atomic content", "output.txt") {
            case Ok(_)    => println("Atomic write succeeded.")
            case Err(err) => println("Write error: ${err}")
        }
    } with FileSystem.withAtomicWrite
