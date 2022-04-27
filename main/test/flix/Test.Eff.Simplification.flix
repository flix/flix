namespace Test/Eff/Simplification {
    @test
    def testPureAndPure(): Unit & Pure and Pure = pure()

    @test
    def testPureAndImpure(): Unit = Pure and Impure = impure()

    @test
    def testImpureAndImpure(): Unit = Impure and Impure = impure()

    @test
    def testPureOrPure(): Unit & Pure or Pure = pure()

    @test
    def testPureOrImpure(): Unit = Pure and Impure = pure()

    @test
    def testImpureOrImpure(): Unit = Impure and Impure = impure()

    /// a pure function
    def pure(): Unit & Pure = ()

    /// an impure function
    def impure(): Unit & Impure = () as & Impure
}
