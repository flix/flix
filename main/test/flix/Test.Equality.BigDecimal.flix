mod Test.Equality.BigDecimal {

    use Assert.{assertEq, assertNeq}

    @Test
    def testEqBigDecimal_01(): Unit \ Assert =
        assertEq(expected = +0.0ff, +0.0ff)

    @Test
    def testEqBigDecimal_02(): Unit \ Assert =
        assertEq(expected = -0.0ff, -0.0ff)

    @Test
    def testEqBigDecimal_03(): Unit \ Assert =
        assertEq(expected = +1.0ff, +1.0ff)

    @Test
    def testEqBigDecimal_04(): Unit \ Assert =
        assertEq(expected = -1.0ff, -1.0ff)

    @Test
    def testEqBigDecimal_05(): Unit \ Assert =
        assertEq(expected = +0.0ff, -0.0ff)

    @Test
    def testNeqBigDecimal_01(): Unit \ Assert =
        assertNeq(unexpected = -1.0ff, +1.0ff)

    @Test
    def testNeqBigDecimal_02(): Unit \ Assert =
        assertNeq(unexpected = +1.0ff, -1.0ff)

    @Test
    def testNeqBigDecimal_03(): Unit \ Assert =
        assertNeq(unexpected = 1.0ff, 2.0ff)

}
