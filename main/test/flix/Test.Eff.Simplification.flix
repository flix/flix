mod Test.Eff.Simplification {
    @Test
    def testPureAndPure(): Unit \ {Pure, Pure} = pure()

    @Test
    def testPureAndImpure(): Unit \ {Pure, IO} = impure()

    @Test
    def testImpureAndImpure(): Unit \ {IO, IO} = impure()

    @Test
    def testPureOrPure(): Unit \ Pure & Pure = pure()

    @Test
    def testPureOrImpure(): Unit \ Pure & Impure = pure()

    @Test
    def testImpureOrImpure(): Unit \ Impure & Impure = impure()

    /// a pure function
    def pure(): Unit \ {} = ()

    /// an impure function
    def impure(): Unit \ IO = unchecked_cast(() as _ \ IO)
}
