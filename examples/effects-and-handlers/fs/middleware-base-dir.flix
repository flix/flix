use Fs.FileSystem

/// Uses `withBaseDir` to resolve relative paths against a base directory.
/// Absolute paths pass through unchanged.
def main(): Unit \ { FileSystem, IO } =
    match FileSystem.mkDirs("/tmp/flix-basedir") {
        case Err(err) => println("Setup error: ${err}")
        case Ok(_) =>
            run {
                match FileSystem.write(str = "Hello", "greeting.txt") {
                    case Err(err) => println("Write error: ${err}")
                    case Ok(_) =>
                        match FileSystem.read("greeting.txt") {
                            case Ok(content) => println("Read: ${content}")
                            case Err(err)    => println("Read error: ${err}")
                        }
                }
            } with FileSystem.withBaseDir("/tmp/flix-basedir")
    }
