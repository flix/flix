def main(): Unit \ IO =
    run {
        let timestamp = Clock.currentTime(Time.TimeUnit.Milliseconds);
        println("${timestamp} ms since the epoc")
    } with Clock.runWithIO
