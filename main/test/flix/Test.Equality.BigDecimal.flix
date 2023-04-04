mod Test.Equality.BigDecimal {

    @test
    def testEqBigDecimal_01(): Bool = +0.0ff == +0.0ff

    @test
    def testEqBigDecimal_02(): Bool = -0.0ff == -0.0ff

    @test
    def testEqBigDecimal_03(): Bool = +1.0ff == +1.0ff

    @test
    def testEqBigDecimal_04(): Bool = -1.0ff == -1.0ff

    @test
    def testNeqBigDecimal_01(): Bool = -1.0ff != +1.0ff

    @test
    def testNeqBigDecimal_02(): Bool = +1.0ff != -1.0ff

}
