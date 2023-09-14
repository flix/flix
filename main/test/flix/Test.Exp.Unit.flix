mod Test.Exp.Unit {

    @Test
    def testUnit01(): Unit = ()

    @Test
    def testUnit02(): Unit =
        let f = () -> ();
        f()

}
