mod Test.Equality.Int16 {

    use Assert.{assertEq, assertNeq}

    @Test
    def testEqInt16_01(): Unit \ Assert =
        assertEq(expected = 0i16, 0i16)

    @Test
    def testEqInt16_02(): Unit \ Assert =
        assertEq(expected = +1i16, +1i16)

    @Test
    def testEqInt16_03(): Unit \ Assert =
        assertEq(expected = -1i16, -1i16)

    @Test
    def testNeqInt16_01(): Unit \ Assert =
        assertNeq(unexpected = -1i16, +1i16)

    @Test
    def testNeqInt16_02(): Unit \ Assert =
        assertNeq(unexpected = +1i16, -1i16)

    @Test
    def testNeqInt16_03(): Unit \ Assert =
        assertNeq(unexpected = 1i16, 2i16)

}
