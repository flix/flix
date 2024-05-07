mod Test.Eff.Simplification {
    pub def testPureAndPure(): Unit \ {{}, {}} = pure()

    pub def testPureAndIo(): Unit \ {{}, IO} = io()

    pub def testIoAndIo(): Unit \ {IO, IO} = io()

    pub def testPureOrPure(): Unit \ {} & {} = pure()

    pub def testPureOrIo(): Unit \ {} & IO = pure()

    pub def testIoOrIo(): Unit \ IO & IO = io()

    /// a pure function
    def pure(): Unit \ {} = ()

    /// an io function
    def io(): Unit \ IO = unchecked_cast(() as _ \ IO)
}
