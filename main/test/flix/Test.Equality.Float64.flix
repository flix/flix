mod Test.Equality.Float64 {

    use Assert.{assertEq, assertNeq}

    @Test
    def testEqFloat64_01(): Unit \ Assert =
        assertEq(expected = 0.0f64, 0.0f64)

    @Test
    def testEqFloat64_02(): Unit \ Assert =
        assertEq(expected = -0.0f64, -0.0f64)

    @Test
    def testEqFloat64_03(): Unit \ Assert =
        assertEq(expected = 1.0f64, 1.0f64)

    @Test
    def testEqFloat64_04(): Unit \ Assert =
        assertEq(expected = -1.0f64, -1.0f64)

    @Test
    def testEqFloat64_05(): Unit \ Assert =
        assertEq(expected = 0.0f64, -0.0f64)

    @Test
    def testNeqFloat64_01(): Unit \ Assert =
        assertNeq(unexpected = -1.0f64, 1.0f64)

    @Test
    def testNeqFloat64_02(): Unit \ Assert =
        assertNeq(unexpected = 1.0f64, -1.0f64)

    @Test
    def testNeqFloat64_03(): Unit \ Assert =
        assertNeq(unexpected = 1.0f64, 2.0f64)

}
