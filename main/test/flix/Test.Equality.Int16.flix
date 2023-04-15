mod Test.Equality.Int16 {

    @test
    def testEqInt16_01(): Bool = 0i16 == 0i16

    @test
    def testEqInt16_02(): Bool = +1i16 == +1i16

    @test
    def testEqInt16_03(): Bool = -1i16 == -1i16

    @test
    def testNeqInt16_01(): Bool = -1i16 != +1i16

    @test
    def testNeqInt16_02(): Bool = +1i16 != -1i16

}
