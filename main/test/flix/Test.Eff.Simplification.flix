mod Test.Eff.Simplification {

    pub def testIoAndIo(): Unit \ {IO, IO} = io()

    /// an io function
    def io(): Unit \ IO = unchecked_cast(() as _ \ IO)
}
