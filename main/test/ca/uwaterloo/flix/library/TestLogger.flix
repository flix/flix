mod TestLogger {
    use Assert.assertEq
    use Logger.{log, info, fatal, warn, trace, debug, runWithFileWrite, runWithConsole}

    /////////////////////////////////////////////////////////////////////////////
    // FileWrite                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def runWithFileWrite01(): Unit \ Assert = 
        let m = RichString.text("Testing a simple string");
        let s = Severity.Fatal;
        let res = run {
            log(s, m);
            ""
        } with runWithFileWrite("log")
        with fileWriteHandlerStub;
        assertEq(expected = "Writing to log: [Fatal] Testing a simple string\n", res)

    @Test
    def runWithFileWrite02(): Unit \ Assert = 
        let res = run {
            info("Log at info level");
            ""
        } with runWithFileWrite("info.log")
        with fileWriteHandlerStub;
        assertEq(expected = "Writing to info.log: [Info] Log at info level\n", res)

    @Test
    def runWithFileWrite03(): Unit \ Assert = 
        let res = run {
            info("Logging 1");
            info("Logging 2");
            info("Logging 3");
            info("Logging 4");
            fatal("Logging 5");
            ""
        } with runWithFileWrite("info.log")
        with fileWriteHandlerStub;
        let expected = 
            "Writing to info.log: [Info] Logging 1\n" 
            + "Writing to info.log: [Info] Logging 2\n"
            + "Writing to info.log: [Info] Logging 3\n"
            + "Writing to info.log: [Info] Logging 4\n"
            + "Writing to info.log: [Fatal] Logging 5\n";
        assertEq(expected = expected, res)

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

    def fileWriteHandlerStub(g: Unit -> String \ ef): String \ (ef - FileWrite) = 
        run {
            g()
        } with handler FileWrite {
            def append(dat, f, k) = {
                let res = "Writing to " + f + ": " + dat#str + k();
                res
            }
            def write(data, f, k) = unreachable!()
            def writeLines(data, f, k) = unreachable!()
            def writeBytes(data, f, k) = unreachable!()
            def appendLines(data, f, k) = unreachable!()
            def appendBytes(data, f, k) = unreachable!()
            def truncate(f, k) = unreachable!()
            def mkDir(d, k) = unreachable!()
            def mkDirs(d, k) = unreachable!()
            def mkTempDir(prefix, k) = unreachable!()
        }

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
