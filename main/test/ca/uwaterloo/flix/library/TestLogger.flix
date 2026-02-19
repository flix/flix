mod TestLogger {
    use Assert.assertEq
    use Logger.{log, info, fatal, warn, trace, debug, runWithFileWrite, runWithConsole}

    /////////////////////////////////////////////////////////////////////////////
    // Console                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    // Tests the log operation directly
    @Test
    def runWithFileWrite01(): Unit \ Assert = 
        let m = RichString.fromString("Testing a simple string");
        let s = Severity.Fatal;
        let res = run {
            Logger.log(s, m);
            ""
        } with runWithConsole
        with consoleHandlerStub;
        let expected = "${RichString.toString(RichString.bold(RichString.red("[Fatal]")))} Testing a simple string";
        Assert.assertEq(expected="Logging to Console: " + expected, res)

    /////////////////////////////////////////////////////////////////////////////
    // Helpers                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    def consoleHandlerStub(g: Unit -> String \ ef): String \ (ef - Console) = 
        run {
            g()
        } with handler Console {
            def println(s, k) = {
                let res = "Logging to Console: " + s + k();
                res
            }
            def readln(k) = unreachable!()
            def print(s, k) = unreachable!()
            def eprint(s, k) = unreachable!()
            def eprintln(s, k) = unreachable!()
        }
}
