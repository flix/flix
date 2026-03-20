use Fs.FileSystem

/// Finds files matching a glob pattern under a base directory.
def main(): Unit \ { FileSystem, IO } =
    match FileSystem.glob(".", "*.flix") {
        case Ok(files) =>
            foreach (file <- files) {
                println(file)
            }
        case Err(err) => println("Error: ${err}")
    }
