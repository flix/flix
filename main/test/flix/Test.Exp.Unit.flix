namespace Test/Exp/Unit {

    @test
    def testUnit01(): () = ()

    @test
    def testUnit02(): Unit = ()

    @test
    def testUnit03(): Unit = 
        let f = () -> ();
        f()
        
}
