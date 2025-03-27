import java.io.FileReader
import java.io.IOException

def main(): Unit \ IO =
    try {
        let reader = new FileReader("foo.txt");
        discard reader.read();
        reader.close()
    } catch {
        case ex: IOException => ex.printStackTrace()
    }
