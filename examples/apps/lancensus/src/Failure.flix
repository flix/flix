use Json.JsonError

///
/// The failure we may encounter during the analysis.
/// A failure should only be raised in cases where we can not provide a sensible analysis result.
/// TODO: try to handle some resumable failure.
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
    /// This failure means we have trouble parsing a string to a json element.
    /// TODO: We can provide a better report.
    ///
    def raiseParseError(): Void
    
    ///
    /// This failure means we have trouble accessing a json element.
    ///
    def raiseJsonError(err: JsonError): Void
}

mod AnalysisFailure{
    use Exit.exit
    
    ///
    /// Handles analysis failure.
    ///
    pub def handleAnalysisFailure(f: Unit -> a \ ef): a \ ef - AnalysisFailure + IO + Exit = {
        run {
            f()
        } with handler AnalysisFailure {
            def raiseRequestFailure(err, _k) = {
                println(err); 
                exit(1)
            }
            def raiseConfigError(msg, _k) = {
                println(msg);
                exit(2)
            }
            def raiseParseError(_k) = {
                println("Parse Error");
                exit(3)
            }
            // TODO: we can try to print the json error.
            def raiseJsonError(_err, _k) = {
                exit(4)
            }
        }
    }
}
