mod TestEnv {

    use Assert.{assertFalse, assertNeq, assertTrue}

    @Test
    def getEnv01(): Unit \ Assert + IO = Env.handle(() ->
        assertFalse(Map.isEmpty(Env.getEnv()))
    )()

    @Test
    def getVar01(): Unit \ Assert + IO = Env.handle(() ->
        assertFalse(Option.isEmpty(Env.getVar("PATH")))
    )()

    @Test
    def getVar02(): Unit \ Assert + IO = Env.handle(() ->
        assertTrue(Option.isEmpty(Env.getVar("UNKNOWN")))
    )()

    @Test
    def getProp01(): Unit \ Assert + IO = Env.handle(() ->
        assertTrue(Option.isEmpty(Env.getProp("UNKNOWN")))
    )()

    @Test
    def getOsName01(): Unit \ Assert + IO = Env.handle(() ->
        assertFalse(Option.isEmpty(Env.getOsName()))
    )()

    @Test
    def getOsArch01(): Unit \ Assert + IO = Env.handle(() ->
        assertFalse(Option.isEmpty(Env.getOsArch()))
    )()

    @Test
    def getOsVersion01(): Unit \ Assert + IO = Env.handle(() ->
        assertFalse(Option.isEmpty(Env.getOsVersion()))
    )()

    @Test
    def getFileSeparator01(): Unit \ Assert + IO = Env.handle(() ->
        assertFalse(String.isEmpty(Env.getFileSeparator()))
    )()

    @Test
    def getPathSeparator01(): Unit \ Assert + IO = Env.handle(() ->
        assertFalse(String.isEmpty(Env.getPathSeparator()))
    )()

    @Test
    def getLineSeparator01(): Unit \ Assert + IO = Env.handle(() ->
        assertFalse(String.isEmpty(Env.getLineSeparator()))
    )()

    @Test
    def getCurrentWorkingDirectory01(): Unit \ Assert + IO = Env.handle(() ->
        assertFalse(Option.isEmpty(Env.getCurrentWorkingDirectory()))
    )()

    @Test
    def getTemporaryDirectory01(): Unit \ Assert + IO = Env.handle(() ->
        assertFalse(Option.isEmpty(Env.getTemporaryDirectory()))
    )()

    @Test
    def getUserName01(): Unit \ Assert + IO = Env.handle(() ->
        assertFalse(Option.isEmpty(Env.getUserName()))
    )()

    @Test
    def getUserHomeDirectory01(): Unit \ Assert + IO = Env.handle(() ->
        assertFalse(Option.isEmpty(Env.getUserHomeDirectory()))
    )()

    @Test
    def getVirtualProcessors01(): Unit \ Assert + IO = Env.handle(() ->
        assertTrue(Env.getVirtualProcessors() > 0)
    )()

}
