namespace Test/Eff/Simplification {
    @test
    def testPureAndPure(): Unit \ {Pure, Pure} = pure()

    @test
    def testPureAndImpure(): Unit \ {Pure, Impure} = impure()

    @test
    def testImpureAndImpure(): Unit \ {IO, Impure} = impure()

    @test
    def testPureOrPure(): Unit \ {Pure or Pure} = pure()

    @test
    def testPureOrImpure(): Unit \ {Pure or Impure} = pure()

    @test
    def testImpureOrImpure(): Unit \ {IO or Impure} = impure()

    /// a pure function
    def pure(): Unit \ {} = ()

    /// an impure function
    def impure(): Unit \ IO = () as \ IO
}
