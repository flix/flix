mod Test.Equality.BigInt {

    @test
    def testEqBigInt01(): Bool = 0ii == 0ii

    @test
    def testEqBigInt02(): Bool = +1ii == +1ii

    @test
    def testEqBigInt03(): Bool = -1ii == -1ii

    @test
    def testNeqBigInt01(): Bool = -1ii != +1ii

    @test
    def testNeqBigInt02(): Bool = +1ii != -1ii

}
