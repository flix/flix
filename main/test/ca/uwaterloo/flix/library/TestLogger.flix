mod TestLogger {
    use Severity.{Fatal, Warn, Info, Trace}
    use Assert.{assertEq}
    use Logger.runWithSeverityFilter

    /////////////////////////////////////////////////////////////////////////////
    // Filtering                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def testRunWithSeverityFilter01(): Unit \ Assert =
        let res: String = run {
            Logger.info("info");
            "ignored"
        } with runWithSeverityFilter((_msg, _sev) -> true)
        with handler Logger {
            def log(_s, m, _k) = RichString.toString(m)
        };
        assertEq(expected = "info", res)

    @Test
    def testRunWithSeverityFilter02(): Unit \ Assert =
        let res: String = run {
            Logger.info("info");
            "ignored"
        } with runWithSeverityFilter((_msg, sev) -> Set.exists(a -> a == sev, Set#{Severity.Fatal}))
        with handler Logger {
            def log(_s, m, _k) = RichString.toString(m)
        };
        assertEq(expected = "ignored", res)

    @Test
    def testRunWithSeverityFilter03(): Unit \ Assert =
        let res: String = run {
            Logger.info("info");
            "ignored"
        } with runWithSeverityFilter((msg, sev) -> if (Set.exists(a -> a == sev, Set#{Severity.Fatal})) true else {
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
    def testRunWithSeverityFilter04(): Unit \ Assert =
        let res: String = run {
            Logger.info("info");
            "ignored"
        } with runWithSeverityFilter(Logger.onlyLevels(Set#{Warn, Fatal}))
        with handler Logger {
            def log(_s, m, _k) = RichString.toString(m)
        };
        assertEq(expected = "ignored", res)
}