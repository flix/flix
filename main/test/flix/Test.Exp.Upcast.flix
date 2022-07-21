namespace Test/Exp/Upcast {

    @test
    def upcast01(): Unit & Impure =
        upcast ()

    @test
    def upcast02(): Unit = // Should trigger error
        upcast ()

}
