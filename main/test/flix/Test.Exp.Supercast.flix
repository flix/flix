namespace Test/Exp/Supercast {

    @test
    def testSupercast01(): ##java.lang.Object \ Impure =
        import new java.lang.Object(): ##java.lang.Object \ Impure as newObject;
        import new java.lang.StringBuilder(): ##java.lang.StringBuilder \ Impure as newStringBuilder;
        if (true)
            supercast(newStringBuilder())
        else
            newObject()

}
