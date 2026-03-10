use Time.Duration.seconds
use Time.Sleep

/// Uses `withNoOp` to skip all sleeps, useful for testing.
def main(): Unit \ IO =
    run {
        println("Going to sleep...");
        Sleep.sleep(seconds(10));
        println("Woke up instantly!")
    } with Sleep.withNoOp
