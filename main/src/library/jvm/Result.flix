///
/// JVM-only extensions for `Result`.
///

pub mod Result {

    import java.lang.Exception

    ///
    /// Returns `Ok(x)` if `f` was invoked without throwing an exception.
    ///
    /// If `f` throws a Java `Exception`, `Err(e)` is returned
    /// where `e` is the error message.
    ///
    pub def tryCatch(f: Unit -> a \ ef): Result[String, a] \ ef =
        try {
            Ok(f())
        } catch {
            case e: Exception => Err(e.getMessage())
        }

}

