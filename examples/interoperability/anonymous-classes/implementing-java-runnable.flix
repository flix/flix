import java.lang.Runnable
import java.lang.Thread

def newRunnable(): Runnable \ IO = new Runnable {
    // Note: In Flix `run` is a keyword so we must escape it with $.
    def $run(_this: Runnable): Unit \ IO = {
        println("Running...")
    }
}

def main(): Unit \ IO =
    new Thread(newRunnable()).start()
