mod Test.Equality.Int64 {

    use Assert.{assertEq, assertNeq}

    @Test
    def testEqInt64_01(): Unit \ Assert =
        assertEq(expected = 0i64, 0i64)

    @Test
    def testEqInt64_02(): Unit \ Assert =
        assertEq(expected = +1i64, +1i64)

    @Test
    def testEqInt64_03(): Unit \ Assert =
        assertEq(expected = -1i64, -1i64)

    @Test
    def testNeqInt64_01(): Unit \ Assert =
        assertNeq(unexpected = -1i64, +1i64)

    @Test
    def testNeqInt64_02(): Unit \ Assert =
        assertNeq(unexpected = +1i64, -1i64)

    @Test
    def testNeqInt64_03(): Unit \ Assert =
        assertNeq(unexpected = 1i64, 2i64)

}
