mod Test.Equality.Int32 {

    use Assert.{assertEq, assertNeq}

    @Test
    def testEqInt32_01(): Unit \ Assert =
        assertEq(expected = 0i32, 0i32)

    @Test
    def testEqInt32_02(): Unit \ Assert =
        assertEq(expected = +1i32, +1i32)

    @Test
    def testEqInt32_03(): Unit \ Assert =
        assertEq(expected = -1i32, -1i32)

    @Test
    def testNeqInt32_01(): Unit \ Assert =
        assertNeq(unexpected = -1i32, +1i32)

    @Test
    def testNeqInt32_02(): Unit \ Assert =
        assertNeq(unexpected = +1i32, -1i32)

    @Test
    def testNeqInt32_03(): Unit \ Assert =
        assertNeq(unexpected = 1i32, 2i32)

}
