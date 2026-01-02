mod Test.Equality.Int8 {

    use Assert.{assertEq, assertNeq}

    @Test
    def testEqInt8_01(): Unit \ Assert =
        assertEq(expected = 0i8, 0i8)

    @Test
    def testEqInt8_02(): Unit \ Assert =
        assertEq(expected = +1i8, +1i8)

    @Test
    def testEqInt8_03(): Unit \ Assert =
        assertEq(expected = -1i8, -1i8)

    @Test
    def testNeqInt8_01(): Unit \ Assert =
        assertNeq(unexpected = -1i8, +1i8)

    @Test
    def testNeqInt8_02(): Unit \ Assert =
        assertNeq(unexpected = +1i8, -1i8)

    @Test
    def testNeqInt8_03(): Unit \ Assert =
        assertNeq(unexpected = 1i8, 2i8)

}
