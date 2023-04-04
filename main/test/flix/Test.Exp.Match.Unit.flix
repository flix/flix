mod Test.Exp.Match.Unit {

    @test
    def testMatchUnit01(): Bool = match () {
        case () => true
    }

    @test
    def testMatchUnitVar01(): Bool = match () {
        case x => x == ()
    }

}
