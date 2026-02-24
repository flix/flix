mod TestLogger {
    use Severity.{Fatal, Warn, Info, Trace}
    use Assert.{assertEq}
    use Logger.{runWithFilter, onlyLevels}

    /////////////////////////////////////////////////////////////////////////////
    // Filtering                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def testRunWithFilter01(): Unit \ Assert =
        let res: String = run {
            Logger.info("info");
            "ignored"
        } with runWithFilter((_msg, _sev) -> true)
        with handler Logger {
            def log(_s, m, _k) = RichString.toString(m)
        };
        assertEq(expected = "info", res)

    @Test
    def testRunWithFilter02(): Unit \ Assert =
        let res: String = run {
            Logger.info("info");
            "ignored"
        } with runWithFilter((sev, _) -> Set.exists(a -> a == sev, Set#{Fatal}))
        with handler Logger {
            def log(_s, m, _k) = RichString.toString(m)
        };
        assertEq(expected = "ignored", res)

    @Test
    def testRunWithFilter03(): Unit \ Assert =
        let res: String = run {
            Logger.info("info");
            "ignored"
        } with runWithFilter((sev, msg) -> if (Set.exists(a -> a == sev, Set#{Fatal})) true else {
            Console.println(RichString.toString(msg)); false})
        with handler Logger {
            def log(_s, m, _k) = RichString.toString(m)
        } with handler Console {
            def readln(_k) = unreachable!()
            def print(_s, _k) = unreachable!()
            def eprint(_s, _k) = unreachable!()
            def println(s, _k) = {s}
            def eprintln(_s, _k) = unreachable!()
        };
        assertEq(expected = "info", res)


    @Test
    def testRunWithFilter04(): Unit \ Assert =
        let res: String = run {
            Logger.info("info");
            "ignored"
        } with runWithFilter(onlyLevels(Set#{Warn, Fatal}))
        with handler Logger {
            def log(_s, m, _k) = RichString.toString(m)
        };
        assertEq(expected = "ignored", res)
}