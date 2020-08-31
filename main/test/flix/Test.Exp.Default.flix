namespace Test/Exp/Default {

    @test
    def testDefaultUnit(): Bool = (default: Unit) == ()

    @test
    def testDefaultBool(): Bool = (default: Bool) == false

}
