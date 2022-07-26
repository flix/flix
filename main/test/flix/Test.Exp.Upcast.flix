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
                (x -> x as & ef);
        let _ = f(1);
        ()
/*
    @test
    def testUpcast03(): Unit & ef = // this should fail
        let f =
            if (true)
                upcast x -> x + 1
            else
                (_ -> "x" as & ef);
        let _ = f(1);
        ()
*/
    @test
    def testUpcast04(): Unit =
        let _ =
            if (true)
                upcast _ -> ()
            else
                _ -> println(1);
        ()

}
