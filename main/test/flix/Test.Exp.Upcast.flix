namespace Test/Exp/Upcast {

    @test
    def testUpcast01(): Unit =
        let _ =
            if (true)
                upcast x -> x + 1
            else
                x -> { println("x"); x + 1 };
        ()

    @test
    def testUpcast02(): Unit & ef =
        let f =
            if (true)
                upcast x -> x + 1
            else
                ((x -> x) as & ef);
        let _ = f(1);
        ()

}
