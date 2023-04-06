mod Test.Equality.String {

    @test
    def testEqString01(): Bool = "" == ""

    @test
    def testEqString02(): Bool = "a" == "a"

    @test
    def testEqString03(): Bool = "abc" == "abc"

    @test
    def testNeqString01(): Bool = "" != "a"

    @test
    def testNeqString02(): Bool = "a" != "abc"

}
