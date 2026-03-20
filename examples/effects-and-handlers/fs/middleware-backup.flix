use Fs.FileSystem

/// Uses `withBackup` to automatically create a backup copy before overwriting a file.
/// Before each write, the existing file is copied to `<file>.bak`.
def main(): Unit \ { FileSystem, IO } =
    match FileSystem.write(str = "Original content", "data.txt") {
        case Err(err) => println("Setup error: ${err}")
        case Ok(_) =>
            run {
                match FileSystem.write(str = "New content", "data.txt") {
                    case Ok(_)    => println("Write succeeded; backup saved to data.txt.bak")
                    case Err(err) => println("Write error: ${err}")
                }
            } with FileSystem.withBackup(".bak")
    }
