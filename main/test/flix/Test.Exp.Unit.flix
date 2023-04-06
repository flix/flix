mod Test.Exp.Unit {

    @test
    def testUnit01(): Unit = ()

    @test
    def testUnit02(): Unit =
        let f = () -> ();
        f()

}
