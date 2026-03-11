mod Test.Equality.Char {

    use Assert.{assertEq, assertNeq}

    @Test
    def testEqChar01(): Unit \ Assert =
        assertEq(expected = 'a', 'a')

    @Test
    def testEqChar02(): Unit \ Assert =
        assertEq(expected = 'z', 'z')

    @Test
    def testEqChar03(): Unit \ Assert =
        assertEq(expected = 'A', 'A')

    @Test
    def testNeqChar01(): Unit \ Assert =
        assertNeq(unexpected = 'a', 'b')

    @Test
    def testNeqChar02(): Unit \ Assert =
        assertNeq(unexpected = 'z', 'w')

    @Test
    def testNeqChar03(): Unit \ Assert =
        assertNeq(unexpected = 'a', 'A')

}
