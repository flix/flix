namespace Test/Exp/Upcast {

    @test
    def testUpcast01(): Unit & ef =
        let f =
            if (true)
                upcast x -> x + 1
            else
                (x -> (x as & ef));
        let _ = f(1);
        ()

    @test
    def testUpcast02(): Unit & Impure =
        let f =
            if (true)
                upcast x -> x + 1
            else
                x -> x + 1 as & Impure;
        let _ = f(1);
        ()

    @test
    def testUpcast03(): Unit =
        let _ =
            if (true)
                upcast (_: Int32) -> ()
            else
                (_: Int32) -> println(1);
        ()

    @test
    def testUpcast05(): Unit & Impure =
        import new java.lang.StringBuilder(): ##java.lang.StringBuilder & Impure as newStringBuilder;
        import new java.lang.Object(): ##java.lang.Object & Impure as newObject;
        let _ =
            if (true)
                upcast newStringBuilder()
            else
                newObject();
        ()

    @test
    def testUpcast06(): Unit & Impure =
        import new java.lang.StringBuilder(): ##java.lang.StringBuilder & Impure as newStringBuilder;
        import new java.lang.Object(): ##java.lang.Object & Impure as newObject;
        let _ =
            if (true)
                upcast (newStringBuilder(), newStringBuilder())
            else
                (newObject(), newObject());
        ()

    @test
    def testUpcast07(): Unit =
        import new java.lang.StringBuilder(): ##java.lang.StringBuilder & Impure as newStringBuilder;
        import new java.lang.Object(): ##java.lang.Object & Impure as newObject;
        let f = (_: ##java.lang.StringBuilder) -> newObject(); // sb  -> obj
        let g = (_: ##java.lang.Object) -> newStringBuilder(); // obj -> sb
        let _ =
            if (true)
                upcast f
            else
                g;
        ()


    @test
    def testUpcast08(): Unit =
        import new java.lang.StringBuilder(): ##java.lang.StringBuilder & Impure as newStringBuilder;
        import new java.lang.Object(): ##java.lang.Object & Impure as newObject;
        let f = (_: ##java.lang.StringBuilder) -> (_: ##java.lang.StringBuilder) -> newObject(); // sb  -> sb  -> obj
        let g = (_: ##java.lang.Object) -> (_: ##java.lang.Object) -> newStringBuilder();        // obj -> obj -> sb
        let _ =
            if (true)
                upcast f
            else
                g;
        ()

    @test
    def testUpcast09(): Unit =
        import new java.lang.StringBuilder(): ##java.lang.StringBuilder & Impure as newStringBuilder;
        import new java.lang.Object(): ##java.lang.Object & Impure as newObject;
        let f = (_: ##java.lang.StringBuilder) -> (_: ##java.lang.Object) -> newObject(); // sb  -> obj -> obj
        let g = (_: ##java.lang.Object) -> (_: ##java.lang.Object) -> newStringBuilder(); // obj -> obj -> sb
        let _ =
            if (true)
                upcast f
            else
                g;
        ()

    @test
    def testUpcast10(): Unit =
        import new java.lang.StringBuilder(): ##java.lang.StringBuilder & Impure as newStringBuilder;
        import new java.lang.Object(): ##java.lang.Object & Impure as newObject;
        let f = (_: ##java.lang.StringBuilder) -> (_: ##java.lang.StringBuilder) -> (_: ##java.lang.Object) -> newObject(); // sb  -> sb  -> obj -> obj
        let g = (_: ##java.lang.Object) -> (_: ##java.lang.Object) -> (_: ##java.lang.Object) -> newStringBuilder();        // obj -> obj -> obj -> sb
        let _ =
            if (true)
                upcast f
            else
                g;
        ()

    @test
    def testUpcast11(): Unit =
        import new java.lang.StringBuilder(): ##java.lang.StringBuilder & Impure as newStringBuilder;
        import new java.lang.Object(): ##java.lang.Object & Impure as newObject;
        let f = (_: ##java.lang.StringBuilder) -> (_: ##java.lang.Object) -> (newObject(), newObject(), newStringBuilder());        // sb  -> obj -> (obj, obj, sb)
        let g = (_: ##java.lang.Object) -> (_: ##java.lang.Object) -> (newStringBuilder(), newStringBuilder(), newStringBuilder()); // obj -> obj -> (sb,  sb,  sb)
        let _ =
            if (true)
                upcast f
            else
                g;
        ()

}
