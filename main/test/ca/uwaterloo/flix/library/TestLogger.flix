mod TestLogger {
    use Assert.assertEq
    use Logger.{log, info, fatal, warn, trace, debug, runWithWriteByLevel}
    use Severity.{Debug, Trace, Fatal, Warn, Info}

    /////////////////////////////////////////////////////////////////////////////
    // RunWithWriteLevels                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def runWithWriteLevels01(): Unit \ Assert = 
        let res = run {
            info("Hello, world");
            ""
        } with runWithWriteByLevel(
            s -> match s {
                case Info => "info.txt"
                case _ => ""
            }
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
            s -> match s {
                case Info => "info.txt"
                case Trace => "trace.txt"
                case Fatal => "fatal.txt"
                case _ => ""
            }
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