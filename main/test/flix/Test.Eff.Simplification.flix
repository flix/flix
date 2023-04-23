mod Test.Eff.Simplification {
    @test
    def testPureAndPure(): Unit & Pure and Pure \ {} = pure()

    @test
    def testPureAndImpure(): Unit & Pure and Impure \ {} = impure()

    @test
    def testImpureAndImpure(): Unit & Impure and Impure \ {} = impure()

    @test
    def testPureOrPure(): Unit & Pure or Pure \ {} = pure()

    @test
    def testPureOrImpure(): Unit & Pure or Impure \ {} = pure()

    @test
    def testImpureOrImpure(): Unit & Impure or Impure \ {} = impure()

    /// a pure function
    def pure(): Unit \ {} = ()

    /// an impure function
    def impure(): Unit & Impure \ {} = unchecked_cast(() as _ \ IO)
}
