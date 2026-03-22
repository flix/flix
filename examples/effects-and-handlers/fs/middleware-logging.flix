use Fs.FileSystem

/// Uses `withLogging` to log each filesystem operation via the `Logger` effect.
def main(): Unit \ { FileSystem, Logger, IO } =
    run {
        match FileSystem.write(str = "Hello, Flix!", "greeting.txt") {
            case Err(err) => println("Write error: ${err}")
            case Ok(_) =>
                match FileSystem.read("greeting.txt") {
                    case Ok(content) => println(content)
                    case Err(err)    => println("Read error: ${err}")
                }
        }
    } with FileSystem.withLogging
