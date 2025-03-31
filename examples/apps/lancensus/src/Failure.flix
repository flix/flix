use Json.JsonError
eff AnalysisFailure {
    def raiseRequestFailure(err: IoError): Void
    def raiseTokenError(msg: String): Void
    def raiseParseError(): Void
    def raiseJsonError(err: JsonError): Void
}

mod AnalysisFailure{
    use Exit.exit
    
    pub def handleAnalysisFailure(f: Unit -> a \ ef): a \ ef - AnalysisFailure + IO + Exit = {
        run {
            f()
        } with handler AnalysisFailure {
            def raiseRequestFailure(err, _k) = {
                println(err); 
                exit(1)
            }
            def raiseTokenError(msg, _k) = {
                println(msg);
                exit(2)
            }
            def raiseParseError(_k) = {
                println("Parse Error");
                exit(3)
            }
            def raiseJsonError(_err, _k) = {
                exit(4)
            }
        }
    }
}
