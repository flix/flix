mod Test.Equality.Int64 {

    @Test
    def testEqInt64_01(): Bool = 0i64 == 0i64

    @Test
    def testEqInt64_02(): Bool = +1i64 == +1i64

    @Test
    def testEqInt64_03(): Bool = -1i64 == -1i64

    @Test
    def testNeqInt64_01(): Bool = -1i64 != +1i64

    @Test
    def testNeqInt64_02(): Bool = +1i64 != -1i64

}
