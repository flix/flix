mod Test.Equality.BigInt {

    @Test
    def testEqBigInt01(): Bool = 0ii == 0ii

    @Test
    def testEqBigInt02(): Bool = +1ii == +1ii

    @Test
    def testEqBigInt03(): Bool = -1ii == -1ii

    @Test
    def testNeqBigInt01(): Bool = -1ii != +1ii

    @Test
    def testNeqBigInt02(): Bool = +1ii != -1ii

}
