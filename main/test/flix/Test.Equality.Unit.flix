mod Test.Equality.Unit {

    @test
    def testEqUnit01(): Bool = () == ()

    @test
    def testEqUnit02(): Bool = (() != ()) == false

}
