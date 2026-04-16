use Fs.DirList

/// Lists the contents of a directory.
def main(): Unit \ { DirList, IO } =
    match DirList.list(".") {
        case Ok(entries) =>
            foreach (entry <- entries) {
                println(entry)
            }
        case Err(err) => println("Error: ${err}")
    }
