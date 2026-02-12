mod TestLogger {
    use Assert.assertEq
    use Logger.{log, info, fatal, warn, trace, debug, runWithConsole, runWithFileWrite}

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

    /////////////////////////////////////////////////////////////////////////////
    // FileWrite                                                               //
    /////////////////////////////////////////////////////////////////////////////

    // Tests the log-operation directly
    @Test
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
        } with runWithFileWrite(file = "log")
        with fileWriteHandlerStub;
        assertEq(expected = "Writing to log: [Fatal]: Testing a simple string\n", res)

    // Tests the info-defintion. 
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
        let expected = run {
            "Writing to info.log: [Info] Logging 1\n" 
            + "Writing to info.log: [Info] Logging 2\n"
            + "Writing to info.log: [Info] Logging 3\n"
            + "Writing to info.log: [Info] Logging 4\n"
            + "Writing to info.log: [Fatal] Logging 5\n";
        assertEq(expected = expected, res)
        } with runWithFileWrite(file = "info.log")
        with fileWriteHandlerStub;
        assertEq(expected = "Writing to info.log: [Info]: Log at info level\n", res)

    // Tests doing multiple writes using different defintions.
    @Test
    pub def runWithFileWrite03(): Unit \ Assert = 
        let res = run {
            info("Logging 1");
            info("Logging 2");
            info("Logging 3");
            info("Logging 4");
            fatal("Logging 5");
            ""
        } with runWithFileWrite(file = "info.log")
        with fileWriteHandlerStub;
        let expected = 
            "Writing to info.log: [Info]: Logging 1\n" 
            + "Writing to info.log: [Info]: Logging 2\n"
            + "Writing to info.log: [Info]: Logging 3\n"
            + "Writing to info.log: [Info]: Logging 4\n"
            + "Writing to info.log: [Fatal]: Logging 5\n";
        assertEq(expected = expected, res)

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

    def fileWriteHandlerStub(g: Unit -> String \ ef): String \ (ef - FileWrite) = 
        run {
            g()
        } with handler FileWrite {
            def append(dat, f, k) = {
                let res = "Writing to " + f + ": " + dat#str + k();
                res
            }
            def write(_data, _f, _k) = unreachable!()
            def writeLines(_data, _f, _k) = unreachable!()
            def writeBytes(_data, _f, _k) = unreachable!()
            def appendLines(_data, _f, _k) = unreachable!()
            def appendBytes(_data, _f, _k) = unreachable!()
            def truncate(_f, _k) = unreachable!()
            def mkDir(_d, _k) = unreachable!()
            def mkDirs(_d, _k) = unreachable!()
            def mkTempDir(_prefix, _k) = unreachable!()
        }

}
