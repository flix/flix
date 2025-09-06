mod TestEnvironment {

    use Assert.{assertFalse, assertNeq, assertTrue}

    @Test
    def getEnv01(): Unit \ Assert + IO + Sys = Environment.handle(() ->
        assertFalse(Map.isEmpty(Environment.getEnv()))
    )()

    @Test
    def getVar01(): Unit \ Assert + IO + Sys = Environment.handle(() ->
        assertFalse(Option.isEmpty(Environment.getVar("PATH")))
    )()

    @Test
    def getVar02(): Unit \ Assert + IO + Sys = Environment.handle(() ->
        assertTrue(Option.isEmpty(Environment.getVar("UNKNOWN")))
    )()

    @Test
    def getProp01(): Unit \ Assert + IO + Sys = Environment.handle(() ->
        assertTrue(Option.isEmpty(Environment.getProp("UNKNOWN")))
    )()

    @Test
    def getOsName01(): Unit \ Assert + IO + Sys = Environment.handle(() ->
        assertFalse(Option.isEmpty(Environment.getOsName()))
    )()

    @Test
    def getOsArch01(): Unit \ Assert + IO + Sys = Environment.handle(() ->
        assertFalse(Option.isEmpty(Environment.getOsArch()))
    )()

    @Test
    def getOsVersion01(): Unit \ Assert + IO + Sys = Environment.handle(() ->
        assertFalse(Option.isEmpty(Environment.getOsVersion()))
    )()

    @Test
    def getFileSeparator01(): Unit \ Assert + IO + Sys = Environment.handle(() ->
        assertFalse(String.isEmpty(Environment.getFileSeparator()))
    )()

    @Test
    def getPathSeparator01(): Unit \ Assert + IO + Sys = Environment.handle(() ->
        assertFalse(String.isEmpty(Environment.getPathSeparator()))
    )()

    @Test
    def getLineSeparator01(): Unit \ Assert + IO + Sys = Environment.handle(() ->
        assertFalse(String.isEmpty(Environment.getLineSeparator()))
    )()

    @Test
    def getCurrentWorkingDirectory01(): Unit \ Assert + IO + Sys = Environment.handle(() ->
        assertFalse(Option.isEmpty(Environment.getCurrentWorkingDirectory()))
    )()

    @Test
    def getTemporaryDirectory01(): Unit \ Assert + IO + Sys = Environment.handle(() ->
        assertFalse(Option.isEmpty(Environment.getTemporaryDirectory()))
    )()

    @Test
    def getUserName01(): Unit \ Assert + IO + Sys = Environment.handle(() ->
        assertFalse(Option.isEmpty(Environment.getUserName()))
    )()

    @Test
    def getUserHomeDirectory01(): Unit \ Assert + IO + Sys = Environment.handle(() ->
        assertFalse(Option.isEmpty(Environment.getUserHomeDirectory()))
    )()

    @Test
    def getVirtualProcessors01(): Unit \ Assert + IO + Sys = Environment.handle(() ->
        assertTrue(Environment.getVirtualProcessors() > 0)
    )()

    @Test
    def getArgs01(): Unit \ Assert + IO + Sys = Environment.handle(() -> {
        let args = Environment.getArgs();
        assertNeq(unexpected = List.Nil, args)
    })()

}
