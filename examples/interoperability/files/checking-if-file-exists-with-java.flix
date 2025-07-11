import java.io.File

def main(): Unit \ { FsRead, IO } =
    let file = new File("foo.txt");
    if (file.exists() and file.isFile()) {
        println("Path exists and is a regular file.")
    } else {
        println("File does not exist or is not a regular file")
    }
