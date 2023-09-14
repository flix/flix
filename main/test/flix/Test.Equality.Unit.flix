mod Test.Equality.Unit {

    @Test
    def testEqUnit01(): Bool = () == ()

    @Test
    def testEqUnit02(): Bool = (() != ()) == false

}
