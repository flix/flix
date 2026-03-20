use Fs.FileRead

/// Reads a file as raw bytes and prints the byte count and content.
def main(): Unit \ { FileRead, IO } =
    match FileRead.readBytes("binary.dat") {
        case Ok(bytes) =>
            println("Read ${Vector.length(bytes)} bytes.");
            println("As string: ${String.fromBytes(bytes)}")
        case Err(err) => println("Error: ${err}")
    }
