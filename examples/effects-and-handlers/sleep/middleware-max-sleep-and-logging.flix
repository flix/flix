use Time.Duration.{milliseconds, seconds}
use Time.Sleep

/// Composes `withMaxSleep` and `withLogging` to cap sleep durations
/// to 500 milliseconds and log each sleep via the `Logger` effect.
def main(): Unit \ { Logger, Sleep, IO } =
    run {
        println("Sleeping for 2 seconds (capped to 500ms)...");
        Sleep.sleep(seconds(2));
        println("Done!")
    } with Sleep.withMaxSleep(milliseconds(500))
      with Sleep.withLogging
