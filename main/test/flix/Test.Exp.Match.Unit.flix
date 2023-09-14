mod Test.Exp.Match.Unit {

    @Test
    def testMatchUnit01(): Bool = match () {
        case () => true
    }

    @Test
    def testMatchUnitVar01(): Bool = match () {
        case x => x == ()
    }

}
