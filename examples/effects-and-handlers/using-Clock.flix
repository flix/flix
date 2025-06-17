def main(): Unit \ IO =
    run {
        let timestamp = Clock.currentTime(TimeUnit.Milliseconds);
        println("${timestamp} ms since the epoc")
    } with Clock.runWithIO
