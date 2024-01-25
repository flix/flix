mod Test.Eff.Simplification {
    pub def testPureAndPure(): Unit \ {Pure, Pure} = pure()

    pub def testPureAndIo(): Unit \ {Pure, IO} = io()

    pub def testIoAndIo(): Unit \ {IO, IO} = io()

    pub def testPureOrPure(): Unit \ Pure & Pure = pure()

    pub def testPureOrIo(): Unit \ Pure & IO = pure()

    pub def testIoOrIo(): Unit \ IO & IO = io()

    /// a pure function
    def pure(): Unit \ {} = ()

    /// an io function
    def io(): Unit \ IO = unchecked_cast(() as _ \ IO)
}
