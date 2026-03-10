use Time.Duration.{seconds}
use Time.Sleep

/// Composes `withJitter` and `withLogging` to add ±20% random jitter
/// to sleep durations and log each sleep via the `Logger` effect.
def main(): Unit \ { Logger, Random, Sleep, IO } =
    run {
        println("Sleeping 3 times with ±20% jitter...");
        Sleep.sleep(seconds(1));
        Sleep.sleep(seconds(2));
        Sleep.sleep(seconds(3));
        println("Done!")
    } with Sleep.withJitter(0.2)
      with Sleep.withLogging
