use Fs.FileRead
use Fs.FileWrite

/// Appends text to a file, then reads the result.
/// Shows the difference between `write` (overwrite) and `append`.
def main(): Unit \ { FileRead, FileWrite, IO } =
    match FileWrite.write(str = "Line 1\n", "log.txt") {
        case Err(err) => println("Write error: ${err}")
        case Ok(_) =>
            match FileWrite.append(str = "Line 2\n", "log.txt") {
                case Err(err) => println("Append error: ${err}")
                case Ok(_) =>
                    match FileRead.read("log.txt") {
                        case Ok(content) => println(content)
                        case Err(err)    => println("Read error: ${err}")
                    }
            }
    }
