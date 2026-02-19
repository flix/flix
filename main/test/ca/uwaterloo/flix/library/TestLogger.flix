mod TestLogger {
    use Severity.{Fatal, Warn, Info, Trace}
    use Assert.{assertEq}
    use Logger.runWithSeverityFilter

    /////////////////////////////////////////////////////////////////////////////
    // Console                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    
    @Test
    def testRunWithSeverityFilter01(): Unit \ Assert =
        let res: String = run {
            Logger.info("info");
            "ignored"
        } with runWithSeverityFilter(Set#{Fatal})
        with handler Logger {
            def log(_s, m, _k) = RichString.toString(m)
        };
        assertEq(expected = "ignored", res)

    @Test
    def testRunWithSeverityFilter02(): Unit \ Assert =
        let res: String = run {
            Logger.fatal("fatal");
            "ignored"
        } with runWithSeverityFilter(Set#{Fatal})
        with handler Logger {
            def log(_s, m, _k) = RichString.toString(m)
        };
        assertEq(expected = "fatal", res)


    @Test
    def testRunWithSeverityFilter03(): Unit \ Assert =
        let res: Set[String] = run {
            Logger.fatal("fatal");
            Logger.warn("warn");
            Logger.info("info");
            Set.empty()
        } with runWithSeverityFilter(Set#{Fatal, Warn})
        with handler Logger {
            def log(_s, m, k) = Set.union(k(), Set#{RichString.toString(m)})
        };
        assertEq(expected = Set#{"fatal", "warn"}, res)
}