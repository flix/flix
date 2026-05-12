use Fs.FileWrite

/// Writes raw bytes to a file.
/// Unlike `write`, `writeBytes` takes a `Vector[Int8]` directly.
def main(): Unit \ { FileWrite, IO } =
    let data = Vector#{72i8, 101i8, 108i8, 108i8, 111i8};
    match FileWrite.writeBytes(data, "binary.dat") {
        case Ok(_)    => println("Wrote ${Vector.length(data)} bytes.")
        case Err(err) => println("Error: ${err}")
    }
