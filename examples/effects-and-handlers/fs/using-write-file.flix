use Fs.FileWrite

/// Writes a string to a file and confirms success.
/// The `FileWrite` effect has a default handler, so no explicit `runWithIO` is needed.
def main(): Unit \ { FileWrite, IO } =
    match FileWrite.write(str = "Hello, Flix!", "greeting.txt") {
        case Ok(_)    => println("File written successfully.")
        case Err(err) => println("Error: ${err}")
    }
