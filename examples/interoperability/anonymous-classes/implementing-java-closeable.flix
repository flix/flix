import java.io.Closeable

def newCloseable(): Closeable \ IO = new Closeable {
    def close(_this: Closeable): Unit \ IO = {
        println("Closing...")
    }
}

def main(): Unit \ IO =
    newCloseable().close()
