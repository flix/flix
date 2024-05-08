mod Test.Eff.Simplification {
    // a pure function
    // def pure(): Unit \ {} = ()

    // TODO: Does this even make sense anymore without Pure?
    // Effect sets cannot nest. With Pure (which had special privilege) that was not caught but it is now.
    // pub def testPureAndPure(): Unit \ {{}, {}} = pure()

    // TODO: Does this even make sense anymore without Pure?
    // Effect sets cannot nest. With Pure (which had special privilege) that was not caught but it is now.
    // pub def testPureAndIo(): Unit \ {{}, IO} = io()

    pub def testIoAndIo(): Unit \ {IO, IO} = io()

    // TODO: Uncomment with new parser
    // Previous parser has a hard-time with effect sets in binary-type expressions.
    // pub def testPureOrPure(): Unit \ {} & {} = pure()
    //
    // pub def testPureOrIo(): Unit \ {} & IO = pure()
    //
    // pub def testIoOrIo(): Unit \ IO & IO = io()

    /// an io function
    def io(): Unit \ IO = unchecked_cast(() as _ \ IO)
}
