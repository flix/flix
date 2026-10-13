use Util.Json.JsonError

///
/// The failure we may encounter during the analysis.
/// A failure should only be raised in cases where we can not provide a sensible analysis result.
///
eff AnalysisFailure {
    ///
    /// This failure means we have trouble sending or receiving a request.
    ///
    def raiseRequestFailure(err: IoError): Void

    ///
    /// This failure means we have trouble getting the configuration.
    ///
    def raiseConfigError(msg: String): Void

    ///
    /// This failure means we have trouble parsing a string to json or accessing a json value.
    ///
    def raiseJsonError(err: JsonError): Void
}

mod AnalysisFailure{
    use Sys.Exit

    ///
    /// Handles analysis failure.
    ///
    pub def handleAnalysisFailure(f: Unit -> a \ ef): a \ ef - AnalysisFailure + IO + Exit = {
        run {
            f()
        } with handler AnalysisFailure {
            def raiseRequestFailure(err, _k) = {
                println(err);
                Exit.exit(1)
            }
            def raiseConfigError(msg, _k) = {
                println(msg);
                Exit.exit(2)
            }
            def raiseJsonError(err, _k) = {
                println("JSON error: ${err}");
                Exit.exit(3)
            }
        }
    }
}
