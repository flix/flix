mod Test.Equality.String {

    @Test
    def testEqString01(): Bool = "" == ""

    @Test
    def testEqString02(): Bool = "a" == "a"

    @Test
    def testEqString03(): Bool = "abc" == "abc"

    @Test
    def testNeqString01(): Bool = "" != "a"

    @Test
    def testNeqString02(): Bool = "a" != "abc"

}
