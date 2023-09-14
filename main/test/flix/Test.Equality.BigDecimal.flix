mod Test.Equality.BigDecimal {

    @Test
    def testEqBigDecimal_01(): Bool = +0.0ff == +0.0ff

    @Test
    def testEqBigDecimal_02(): Bool = -0.0ff == -0.0ff

    @Test
    def testEqBigDecimal_03(): Bool = +1.0ff == +1.0ff

    @Test
    def testEqBigDecimal_04(): Bool = -1.0ff == -1.0ff

    @Test
    def testNeqBigDecimal_01(): Bool = -1.0ff != +1.0ff

    @Test
    def testNeqBigDecimal_02(): Bool = +1.0ff != -1.0ff

}
