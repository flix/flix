mod TestLogger {
    use Assert.assertEq
    use Logger.{log, info, fatal, warn, trace, debug, runWithFileWrite}

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
