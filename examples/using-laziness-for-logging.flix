/// Emulates some slow computation.
def slowFunction(): String = {
    import java.lang.Thread:sleep(Int64);
    let _ = sleep(5000i64) as & Pure;
    Int32.toString(42)
}

/// A lazy log function.
/// The idea is that we add the message to some buffer.
/// Later, we can force the evaluation and store it permanently.
/// For this example we just return the unit value.
def log(_: Lazy[String]): Unit & Impure = () as & Impure

/// Writes a message to the log.
/// The slow function will not be evaluated.
def main(_args: Array[String]): Int32 & Impure =
    log(lazy "The computation returned ${slowFunction()}");
    0 // exit code
