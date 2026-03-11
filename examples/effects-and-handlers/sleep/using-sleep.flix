use Time.Duration.seconds
use Time.Sleep

/// Sleeps for one second and prints messages before and after.
/// The Sleep effect has a default handler, so no explicit `runWithIO` is needed.
def main(): Unit \ { Sleep, IO } =
    println("Going to sleep...");
    Sleep.sleep(seconds(1));
    println("Woke up!")
