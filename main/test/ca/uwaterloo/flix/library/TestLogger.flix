mod TestLogger {
    use Assert.assertEq
    use Logger.{log, info, fatal}

    def fileWriteHandlerStub(g: Unit -> String \ ef): String \ (ef - FileWrite) = 
        run {
            g()
        } with handler FileWrite {
            def append(dat, f, k) = {
                let res = "Writing to " + f + ": " + dat#str + k();
                res
            }
            def write(data, f, k) = ???
            def writeLines(data, f, k) = ???
            def writeBytes(data, f, k) = ???
            def appendLines(data, f, k) = ???
            def appendBytes(data, f, k) = ???
            def truncate(f, k) = ???
            def mkDir(d, k) = ???
            def mkDirs(d, k) = ???
            def mkTempDir(prefix, k) = ???
        }

    // Tests the log-operation directly
    @Test
    def runWithFileWrite01(): Unit \ Assert = 
        let m = RichString.text("Testing a simple string");
        let s = Severity.Fatal;
        let res = run {
            log(s, m);
            ""
        } with runWithFileWrite(file = "log")
        with fileWriteHandlerStub;
        assertEq(expected = "Writing to log: [Fatal]: Testing a simple string\n", res)

    // Tests the info-defintion. 
    @Test
    def runWithFileWrite02(): Unit \ Assert = 
        let res = run {
            info("Log at info level");
            ""
        } with runWithFileWrite(file = "info.log")
        with fileWriteHandlerStub;
        assertEq(expected = "Writing to info.log: [Info]: Log at info level\n", res)

    // Tests doing multiple writes using different defintions.
    @Test
    def runWithFileWrite03(): Unit \ Assert = 
        let res = run {
            info("Logging 1");
            Trace("Logging 2");
            debug("Logging 3");
            warn("Logging 4");
            fatal("Logging 5");
            ""
        } with runWithFileWrite(file = "info.log")
        with fileWriteHandlerStub;
        let expected = 
            "Writing to info.log: [Info]: Logging 1\n" 
            + "Writing to info.log: [Trace]: Logging 2\n"
            + "Writing to info.log: [Debug]: Logging 3\n"
            + "Writing to info.log: [Warn]: Logging 4\n"
            + "Writing to info.log: [Fatal]: Logging 5\n";
        assertEq(expected = expected, res)

}
