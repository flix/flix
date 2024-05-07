mod Test.Eff.Simplification {
    // TODO: Does this even make sense anymore without Pure?
    // The test fails right now, and we should probably remove it
    // pub def testPureAndPure(): Unit \ {{}, {}} = pure()

    // TODO: Does this even make sense anymore without Pure?
    // The test fails right now, and we should probably remove it
    //pub def testPureAndIo(): Unit \ {{}, IO} = io()

    pub def testIoAndIo(): Unit \ {IO, IO} = io()

    // TODO: Uncomment with new parser
    // Previous parser has a hard-time with effect sets in binary-type expressions.
    // pub def testPureOrPure(): Unit \ {} & {} = pure()
    //
    // pub def testPureOrIo(): Unit \ {} & IO = pure()
    //
    // pub def testIoOrIo(): Unit \ IO & IO = io()

    /// a pure function
    def pure(): Unit \ {} = ()

    /// an io function
    def io(): Unit \ IO = unchecked_cast(() as _ \ IO)
}
