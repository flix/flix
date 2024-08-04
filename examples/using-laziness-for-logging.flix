import java.lang.Thread;

/// Emulates some slow computation.
def slowFunction(): String = {
    let _ = unsafe Thread.sleep(5000i64);
    Int32.toString(42)
}

/// A lazy log function.
/// The idea is that we add the message to some buffer.
/// Later, we can force the evaluation and store it permanently.
/// For this example we just return the unit value.
def log(_: Lazy[String]): Unit \ IO = checked_ecast(())

/// Writes a message to the log.
/// The slow function will not be evaluated.
def main(): Unit \ IO =
    log(lazy "The computation returned ${slowFunction()}")
