use Fs.FileStat
use Fs.FileWrite

/// Inspects file metadata: existence, type, size, and modification time.
def main(): Unit \ { FileStat, FileWrite, IO } =
    let file = "example.txt";
    match FileWrite.write(str = "Hello!", file) {
        case Err(err) => println("Write error: ${err}")
        case Ok(_) =>
            match FileStat.exists(file) {
                case Ok(b)    => println("Exists: ${b}")
                case Err(err) => println("Error: ${err}")
            };
            match FileStat.isRegularFile(file) {
                case Ok(b)    => println("Is regular file: ${b}")
                case Err(err) => println("Error: ${err}")
            };
            match FileStat.isDirectory(file) {
                case Ok(b)    => println("Is directory: ${b}")
                case Err(err) => println("Error: ${err}")
            };
            match FileStat.size(file) {
                case Ok(s)    => println("Size: ${s}")
                case Err(err) => println("Error: ${err}")
            };
            match FileStat.modificationTime(file) {
                case Ok(t)    => println("Modification time: ${t}ms")
                case Err(err) => println("Error: ${err}")
            }
    }
