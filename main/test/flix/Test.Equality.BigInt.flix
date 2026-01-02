mod Test.Equality.BigInt {

    use Assert.{assertEq, assertNeq}

    @Test
    def testEqBigInt01(): Unit \ Assert =
        assertEq(expected = 0ii, 0ii)

    @Test
    def testEqBigInt02(): Unit \ Assert =
        assertEq(expected = +1ii, +1ii)

    @Test
    def testEqBigInt03(): Unit \ Assert =
        assertEq(expected = -1ii, -1ii)

    @Test
    def testNeqBigInt01(): Unit \ Assert =
        assertNeq(unexpected = -1ii, +1ii)

    @Test
    def testNeqBigInt02(): Unit \ Assert =
        assertNeq(unexpected = +1ii, -1ii)

    @Test
    def testNeqBigInt03(): Unit \ Assert =
        assertNeq(unexpected = 1ii, 2ii)

}
