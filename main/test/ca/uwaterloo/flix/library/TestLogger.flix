mod TestLogger {
    use Assert.assertEq
    use Logger.{log, info, fatal, warn, trace, debug, fileWriteHandlerStub}

    /////////////////////////////////////////////////////////////////////////////
    // RunWithWriteLevels                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def runWithWriteLevels01(): Unit \ Assert = 
        let res = run {
            info("Hello, world");
            ""
        } with runWithWriteByLevel(
            {trace = "", debug = "", info = "info.txt", warn = "", fatal = ""}
        )
        with fileWriteHandlerStub;
        assertEq(expected = "Writing to info.txt: [Info] Hello, world\n", res)

    @Test
    def runWithWriteLevels02(): Unit \ Assert = 
        let res = run {
            info("Info");
            trace("Trace");
            fatal("Fatal");
            ""
        } with runWithWriteByLevel(
            {trace = "trace.txt", debug = "", info = "info.txt", warn = "", fatal = "fatal.txt"}
        )
        with fileWriteHandlerStub;
        let expected =  "Writing to info.txt: [Info] Info\nWriting to trace.txt: [Trace] Trace\nWriting to fatal.txt: [Fatal] Fatal\n";
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
}