mod TestLogger {
    use Assert.{assertEq}
    use Logger.{log, info, fatal, warn, trace, debug, runWithConsole}

    /////////////////////////////////////////////////////////////////////////////
    // Console                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    // Tests the log operation with Severity set to Fatal
    @Test
    def runWithConsole01(): Unit \ Assert = 
        let m = RichString.fromString("Testing a fatal string");
        let s = Severity.Fatal;
        let res = run {
            Logger.log(s, m);
            ""
        } with runWithConsole
        with consoleHandlerStub;
        let expected = "${RichString.toString(RichString.bold(RichString.red("[Fatal]")))} Testing a fatal string";
        Assert.assertEq(expected="Logging to Console stderr: " + expected, res)

    // Tests the log operation with Severity set to Warn
    @Test
    def runWithConsole02(): Unit \ Assert = 
        let m = RichString.fromString("Testing a warning string");
        let s = Severity.Warn;
        let res = run {
            Logger.log(s, m);
            ""
        } with runWithConsole
        with consoleHandlerStub;
        let expected = "${RichString.toString(RichString.yellow("[Warn]"))} Testing a warning string";
        Assert.assertEq(expected="Logging to Console stderr: " + expected, res)

    // Tests the log operation with Severity set to Debug
    @Test
    def runWithConsole03(): Unit \ Assert = 
        let m = RichString.fromString("Testing a debug string");
        let s = Severity.Debug;
        let res = run {
            Logger.log(s, m);
            ""
        } with runWithConsole
        with consoleHandlerStub;
        let expected = "${RichString.toString(RichString.cyan("[Debug]"))} Testing a debug string";
        Assert.assertEq(expected="Logging to Console stdout: " + expected, res)

    // Tests the log operation with Severity set to Info
    @Test
    def runWithConsole04(): Unit \ Assert = 
        let m = RichString.fromString("Testing a info string");
        let s = Severity.Info;
        let res = run {
            Logger.log(s, m);
            ""
        } with runWithConsole
        with consoleHandlerStub;
        let expected = "${RichString.toString(RichString.green("[Info]"))} Testing a info string";
        Assert.assertEq(expected="Logging to Console stdout: " + expected, res)

    // Tests the log operation with Severity set to Trace
    @Test
    def runWithConsole05(): Unit \ Assert = 
        let m = RichString.fromString("Testing a trace string");
        let s = Severity.Trace;
        let res = run {
            Logger.log(s, m);
            ""
        } with runWithConsole
        with consoleHandlerStub;
        let expected = "${RichString.toString(RichString.gray("[Trace]"))} Testing a trace string";
        Assert.assertEq(expected="Logging to Console stdout: " + expected, res)

    /////////////////////////////////////////////////////////////////////////////
    // Helpers                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    def consoleHandlerStub(g: Unit -> String \ ef): String \ (ef - Console) = 
        run {
            g()
        } with handler Console {
            def println(s, k) = {
                let res = "Logging to Console stdout: " + s + k();
                res
            }
            def readln(k) = unreachable!()
            def print(s, k) = unreachable!()
            def eprint(s, k) = unreachable!()
            def eprintln(s, k) = {
                let res = "Logging to Console stderr: " + s + k();
                res
            }
        }
}