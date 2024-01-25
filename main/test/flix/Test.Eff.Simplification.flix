mod Test.Eff.Simplification {
    @test
    def testPureAndPure(): Unit \ {Pure, Pure} = pure()

    @test
    def testPureAndIo(): Unit \ {Pure, IO} = io()

    @test
    def testIoAndIo(): Unit \ {IO, IO} = io()

    @test
    def testPureOrPure(): Unit \ Pure & Pure = pure()

    @test
    def testPureOrIo(): Unit \ Pure & IO = pure()

    @test
    def testIoOrIo(): Unit \ IO & IO = io()

    /// a pure function
    def pure(): Unit \ {} = ()

    /// an io function
    def io(): Unit \ IO = unchecked_cast(() as _ \ IO)
}
