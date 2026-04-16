use Fs.FileRead

/// Reads a file as a string and prints its contents.
/// The `FileRead` effect has a default handler, so no explicit `runWithIO` is needed.
def main(): Unit \ { FileRead, IO } =
    match FileRead.read("example.txt") {
        case Ok(content) => println(content)
        case Err(err)    => println("Error: ${err}")
    }
