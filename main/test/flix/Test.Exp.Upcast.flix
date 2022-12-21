namespace Test/Exp/Upcast {

    @test
    def testUpcast01(): Unit & ef =
        let f =
            if (true)
                upcast x -> x + 1
            else
                (x -> unsafe_cast x as _ & ef);
        let _ = f(1);
        ()

    @test
    def testUpcast02(): Unit & Impure =
        let f =
            if (true)
                upcast x -> x + 1
            else
                x -> unsafe_cast x + 1 as _ & Impure;
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
    def testUpcast06(): Unit =
        import new java.lang.StringBuilder(): ##java.lang.StringBuilder & Impure as newStringBuilder;
        import new java.lang.Object(): ##java.lang.Object & Impure as newObject;
        let f = (_: ##java.lang.Object) -> newStringBuilder();
        let g = (_: ##java.lang.StringBuilder) -> newObject();
        let _ =
            if (true)
                upcast f
            else
                g;
        ()


    @test
    def testUpcast07(): Unit =
        import new java.lang.StringBuilder(): ##java.lang.StringBuilder & Impure as newStringBuilder;
        import new java.lang.Object(): ##java.lang.Object & Impure as newObject;
        let f = (_: ##java.lang.Object) -> (_: ##java.lang.Object) -> newStringBuilder();
        let g = (_: ##java.lang.StringBuilder) -> (_: ##java.lang.StringBuilder) -> newObject();
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
        let f = (_: ##java.lang.Object) -> (_: ##java.lang.Object) -> newStringBuilder();
        let g = (_: ##java.lang.StringBuilder) -> (_: ##java.lang.Object) -> newObject();
        let _ =
            if (true)
                upcast f
            else
                g;
        ()

    @test
    def testUpcast09(): Unit =
        import new java.lang.Object(): ##java.lang.Object & Impure as newObject;
        let f = (_: ##java.lang.Object) -> (_: ##java.lang.Object) -> (_: ##java.lang.Object) -> newObject();
        let g = (_: ##java.lang.StringBuilder) -> (_: ##java.lang.Object) -> (_: ##java.lang.StringBuilder) -> newObject();
        let _ =
            if (true)
                upcast f
            else
                g;
        ()

    @test
    def testUpcast10(): Unit =
        import new java.lang.Object(): ##java.lang.Object & Impure as newObject;
        let f = (_: ##java.lang.Object) -> (_: ##java.lang.Object) -> (_: ##java.lang.Object) -> (_: ##java.lang.StringBuilder) -> newObject();
        let g = (_: ##java.lang.StringBuilder) -> (_: ##java.lang.Object) -> (_: ##java.lang.StringBuilder) -> (_: ##java.lang.StringBuilder) -> newObject();
        let _ =
            if (true)
                upcast f
            else
                g;
        ()

    @test
    def testUpcast11(): Unit =
        import new java.lang.Object(): ##java.lang.Object & Impure as newObject;
        let f = (_: ##java.lang.Object) -> (_: ##java.lang.Object) -> (_: ##java.lang.Object) -> (_: ##java.io.Serializable) -> newObject();
        let g = (_: ##java.io.Serializable) -> (_: ##java.lang.Object) -> (_: ##java.io.Serializable) -> (_: ##java.io.Serializable) -> newObject();
        let _ =
            if (true)
                upcast f
            else
                g;
        ()

    @test
    def testUpcast12(): Unit =
        import new java.lang.Boolean(Bool): ##java.lang.Boolean & Impure as newBool;
        let f = (_: ##java.io.Serializable) -> (_: ##java.io.Serializable) -> (_: ##java.io.Serializable) -> (_: ##java.lang.StringBuilder) -> newBool(true);
        let g = (_: ##java.lang.StringBuilder) -> (_: ##java.io.Serializable) -> (_: ##java.lang.StringBuilder) -> (_: ##java.lang.StringBuilder) -> newBool(false);
        let _ =
            if (true)
                upcast f
            else
                g;
        ()

    @test
    def testUpcast13(): Unit =
        let f = () -> 1;
        let g = () -> unsafe_cast 1 as _ \ A;
        let _ =
            if (true)
                upcast f
            else
                g;
        ()

    @test
    def testUpcast14(): Unit =
        import new java.lang.StringBuilder(): ##java.lang.StringBuilder & Impure as newStringBuilder;
        import new java.lang.Object(): ##java.lang.Object & Impure \ A as newObject;
        let f = (_: ##java.lang.Object) -> newStringBuilder();
        let g = (_: ##java.lang.StringBuilder) -> newObject();
        let _ =
            if (true)
                upcast f
            else
                g;
        ()

    @test
    def testUpcast15(): Unit =
        let f = () -> ();
        let g = () -> unsafe_cast () as _ \ { A, B, C };
        let _ =
            if (true)
                upcast f
            else
                g;
        ()

    @test
    def testUpcast16(): Unit =
        let f = () -> unsafe_cast () as _ \ A;
        let g = () -> unsafe_cast () as _ \ { A, B, C };
        let _ =
            if (true)
                upcast f
            else
                g;
        ()

    @test
    def testUpcast17(): Unit =
        let f = (_: ##java.lang.CharSequence) -> ();
        f(upcast "hello")

    @test
    def testUpcast18(): Unit =
        let f = (_: ##java.lang.Number) -> ();
        f(upcast 123ii)

    @test
    def testUpcast19(): Unit =
        let f = (_: ##java.lang.Number) -> ();
        f(upcast 1.23ff)

    @test
    def testUpcast20(): Int32 = region r {
        let _ = r; // Use r in the scope to avoid redundancy error
        let f = (g: Int32 -> Int32 \ r) -> x -> g(x);
        f(upcast x -> x + 1: Int32, 1)
    }

    pub eff A
    pub eff B
    pub eff C

}
