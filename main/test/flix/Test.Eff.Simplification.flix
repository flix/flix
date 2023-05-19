mod Test.Eff.Simplification {
    @test
    def testPureAndPure(): Unit \ {Pure, Pure} = pure()

    @test
    def testPureAndImpure(): Unit \ {Pure, IO} = impure()

    @test
    def testImpureAndImpure(): Unit \ {IO, IO} = impure()

    @test
    def testPureOrPure(): Unit \ Pure & Pure = pure()

    @test
    def testPureOrImpure(): Unit \ Pure & Impure = pure()

    @test
    def testImpureOrImpure(): Unit \ Impure & Impure = impure()

    /// a pure function
    def pure(): Unit \ {} = ()

    /// an impure function
    def impure(): Unit \ IO = unchecked_cast(() as _ \ IO)
}
