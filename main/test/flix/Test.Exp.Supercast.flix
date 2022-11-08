namespace Test/Exp/Supercast {

    @test
    def testSupercast01(): ##java.lang.Object \ Impure =
        import new java.lang.Object(): ##java.lang.Object \ Impure as newObject;
        import new java.lang.StringBuilder(): ##java.lang.StringBuilder \ Impure as newStringBuilder;
        if (true)
            supercast(newStringBuilder())
        else
            newObject()

    @test
    def testSupercast02(): ##java.lang.Object \ Impure =
        import new java.lang.Object(): ##java.lang.Object \ Impure as newObject;
        import new java.lang.StringBuilder(): ##java.lang.StringBuilder \ Impure as newStringBuilder;
        if (true)
            newObject()
        else
            supercast(newStringBuilder())

    @test
    def testSupercast03(): Unit \ Impure =
        import new java.lang.StringBuilder(): ##java.lang.StringBuilder \ Impure as newStringBuilder;
        let f = (_: ##java.lang.CharSequence) -> ();
        f(supercast(newStringBuilder()))

    @test
    def testSupercast04(): Unit \ Impure =
        import new java.lang.StringBuilder(): ##java.lang.StringBuilder \ Impure as newStringBuilder;
        import java.lang.StringBuilder.append(String): ##java.lang.StringBuilder \ Impure;
        let f = (_: ##java.lang.CharSequence) -> ();
        let sb = newStringBuilder();
        f(supercast(append(sb, "Hello")))

    @test
    def testSupercast05(): Unit \ Impure =
        import new java.lang.StringBuilder(): ##java.lang.StringBuilder \ Impure as newStringBuilder;
        let f = (_: ##java.lang.Comparable) -> ();
        f(supercast(newStringBuilder()))

}
