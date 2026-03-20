use Fs.FileExists
use Fs.FileRead
use Fs.FileTest
use Fs.ReadFile

/// Demonstrates the Fs effect hierarchy by using leaf effects and
/// running them into parent effects via `runWith` functions.
/// `FileExists` is run into `FileTest`, and `ReadFile` into `FileRead`.
/// Both parent effects have default handlers.
def main(): Unit \ { FileRead, FileTest, IO } =
    run {
        safeRead("example.txt")
    } with FileExists.runWithFileTest
      with ReadFile.runWithFileRead

def safeRead(file: String): Unit \ { FileExists, ReadFile, IO } =
    match FileExists.exists(file) {
        case Err(err)  => println("Error: ${err}")
        case Ok(false) => println("File does not exist")
        case Ok(true)  =>
            match ReadFile.read(file) {
                case Ok(content) => println(content)
                case Err(err)    => println("Read error: ${err}")
            }
    }
