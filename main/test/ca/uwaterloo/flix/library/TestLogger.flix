mod TestLogger {
    use Assert.assertEq
    use Logger.{log, info}

    @Test
    def runWithFileWrite01(): Unit \ Assert = 
        let m = RichString.text("Testing a simple string");
        let s = Severity.Fatal;
        let res = run {
            log(s, m);
            ""
        } with runWithFileWrite(file = "log")
        with handler FileWrite {
            def append(dat, f, k) = {
                let res = "Writing to " + f + ": " + dat#str;
                let _ = k();
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
        };
        assertEq(expected = "Writing to log: [Fatal]: Testing a simple string\n", res)

    @Test
    def runWithFileWrite02(): Unit \ Assert = 
        let res = run {
            info("Log at info level");
            ""
        } with runWithFileWrite(file = "info.log")
        with handler FileWrite {
            def append(dat, f, k) = {
                let res = "Writing to " + f + ": " + dat#str;
                let _ = k();
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
        };
        assertEq(expected = "Writing to info.log: [Info]: Log at info level\n", res)
}
