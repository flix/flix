use Fs.FileSystem

/// Lists the contents of a directory.
def main(): Unit \ { FileSystem, IO } =
    match FileSystem.list(".") {
        case Ok(entries) =>
            foreach (entry <- entries) {
                println(entry)
            }
        case Err(err) => println("Error: ${err}")
    }
