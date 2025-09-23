mod Test.Exp.BigDecimal {

    @test
    def testBigDecimal_01(): BigDecimal = 0.0ff

    @test
    def testBigDecimal_02(): BigDecimal = -0.0ff

    @test
    def testBigDecimal_03(): BigDecimal = 1.0ff

    @test
    def testBigDecimal_04(): BigDecimal = 2.0ff

    @test
    def testBigDecimal_05(): BigDecimal = 4.2ff

    @test
    def testBigDecimal_06(): BigDecimal = 99999999999999999999999999999999999999999999999999999999999999999999999999999999.0ff

    @test
    def testBigDecimal_07(): BigDecimal = 0.000000000000000000000000000000000000000000000000000000000000000000000000000000001ff

    @test
    def testBigDecimal_08(): BigDecimal = -99999999999999999999999999999999999999999999999999999999999999999999999999999999.0ff

    @test
    def testBigDecimal_09(): BigDecimal = -0.000000000000000000000000000000000000000000000000000000000000000000000000000000001ff

    @test
    def testBigDecimal_10(): BigDecimal = 1_0.0_0ff

    @test
    def testBigDecimal_11(): BigDecimal = 1_000.123_456_789ff

    @test
    def testBigDecimal_12(): BigDecimal = 42_000.0ff

    @test
    def testBigDecimal_13(): BigDecimal = 1_000_000_000_000_000_000_000_000_000.0ff

    @test
    def testBigDecimal_14(): BigDecimal = 0.000_000_000_000_000_000_000_000_000_1ff

    @test
    def testBigDecimal_15(): BigDecimal = 1ff

    @test
    def testBigDecimal_16(): BigDecimal = 1e23ff

    @test
    def testBigDecimal_17(): BigDecimal = 1.23e45ff

}
