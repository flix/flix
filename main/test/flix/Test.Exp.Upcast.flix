namespace Test/Exp/Upcast {
/*
    @test
    def testUpcast01(): Unit & Impure =
        let _ =
            if (true)
                upcast x -> x + 1
            else {
                println(1);
                x -> x + 1
            };
        ()

    @test
    def testUpcast02(): Unit & ef =
        let f =
            if (true)
                x -> upcast x + 1
            else
                (x -> (x as & ef));
        let _ = f(1);
        ()

    @test
    def testUpcast03(): Unit & Impure =
        let f =
            if (true)
                upcast x -> x + 1
            else
                x -> x + 1 as & Impure;
        let _ = f(1);
        ()

    @test
    def testUpcast04(): Unit =
        let _ =
            if (true)
                _ -> upcast ()
            else
                _ -> println(1);
        ()

    @test
    def testUpcast05(): Unit & Impure =
        let _ =
            if (true)
                ()
            else {
               let _ = [8; 8];
               ()
           };
        ()

    @test
    def testUpcast06(): Unit & Impure =
        import new java.lang.StringBuilder(): ##java.lang.StringBuilder & Impure as newStringBuilder;
        import new java.lang.Object(): ##java.lang.Object & Impure as newObject;
        let _ =
            if (true)
                upcast newStringBuilder()
            else
                newObject();
        ()

    @test
    def testUpcast07(): Unit & Impure =
        import new java.lang.StringBuilder(): ##java.lang.StringBuilder & Impure as newStringBuilder;
        import new java.lang.Object(): ##java.lang.Object & Impure as newObject;
        let _ =
            if (true)
                upcast (newStringBuilder(), newStringBuilder())
            else
                (newObject(), newObject());
        ()
*/

    @test
    def testUpcast100(): Unit & Impure =
        import new java.lang.StringBuilder(): ##java.lang.StringBuilder & Impure as newStringBuilder;
        import new java.lang.Object(): ##java.lang.Object & Impure as newObject;
        let f = (_: ##java.lang.StringBuilder) -> newObject(); // strBuilder -> obj
        let g = (_: ##java.lang.Object) -> newStringBuilder();        // obj        -> strBuilder
        let _ =
            if (true)
                upcast f
            else
                g;
        ()

    @test
    def testUpcast101(): Unit & Impure =
        import new java.lang.StringBuilder(): ##java.lang.StringBuilder & Impure as newStringBuilder;
        import new java.lang.Object(): ##java.lang.Object & Impure as newObject;
        let f = (_: ##java.lang.StringBuilder) -> (_: ##java.lang.Object) -> newObject(); // strBuilder -> obj -> obj
        let g = (_: ##java.lang.Object) -> (_: ##java.lang.Object) -> newStringBuilder(); // obj        -> obj -> strBuilder
        let _ =
            if (true)
                upcast f
            else
                g;
        ()

}
