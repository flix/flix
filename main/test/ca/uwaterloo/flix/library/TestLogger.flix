mod TestLogger {
    use Assert.assertEq
    use Logger.{log, info, fatal, warn, trace, debug, runWithConsole}

    /////////////////////////////////////////////////////////////////////////////
    // Console                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    // Tests the log operation directly
    @Test
    def runWithConsole01(): Unit \ Assert = 
        let m = RichString.fromString("Testing a simple string");
        let s = Severity.Fatal;
        let res = run {
            Logger.log(s, m);
            ""
        } with runWithConsole
        with consoleHandlerStub;
        let expected = "${RichString.toString(RichString.bold(RichString.red("[Fatal]")))} Testing a simple string";
        Assert.assertEq(expected="Logging to Console: " + expected, res)

    @Test
    def testRunWithList01(): Unit \ Assert =
        let (_, list) = run {
            Logger.fatal("A fatal error has occured")
        } with runWithList;
        let expected = List#{(Severity.Fatal, RichString.fromString("A fatal error has occured"))};
        assertTrue(richStringSevListEqual(expected, list))

    @Test
    def testRunWithList02(): Unit \ Assert =
        let (_, list) = run {
            Logger.fatal("A fatal error has occured");
            Logger.info("An info line")
        } with runWithList;
        let expected = List#{
            (Severity.Fatal, RichString.fromString("A fatal error has occured")),
            (Severity.Info, RichString.fromString("An info line"))
        };
        assertTrue(richStringSevListEqual(expected, list))

    @Test
    def testRunWithList03(): Unit \ Assert =
        let (stdRes, _list) = run {
            Logger.fatal("A fatal error has occured");
            5
        } with runWithList;
        assertEq(expected = 5, stdRes)

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
            def readln(_) = unreachable!()
            def print(_, _) = unreachable!()
            def eprint(_, _) = unreachable!()
            def eprintln(_, _) = unreachable!()
        }
}
