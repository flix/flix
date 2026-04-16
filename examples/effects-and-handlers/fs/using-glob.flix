use Fs.Glob

/// Finds files matching a glob pattern under a base directory.
def main(): Unit \ { Glob, IO } =
    match Glob.glob(".", "*.flix") {
        case Ok(files) =>
            foreach (file <- files) {
                println(file)
            }
        case Err(err) => println("Error: ${err}")
    }
