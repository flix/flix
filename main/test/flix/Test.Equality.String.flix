mod Test.Equality.String {

    use Assert.{assertEq, assertNeq}

    @Test
    def testEqString01(): Unit \ Assert =
        assertEq(expected = "", "")

    @Test
    def testEqString02(): Unit \ Assert =
        assertEq(expected = "a", "a")

    @Test
    def testEqString03(): Unit \ Assert =
        assertEq(expected = "abc", "abc")

    @Test
    def testNeqString01(): Unit \ Assert =
        assertNeq(unexpected = "", "a")

    @Test
    def testNeqString02(): Unit \ Assert =
        assertNeq(unexpected = "a", "abc")

    @Test
    def testNeqString03(): Unit \ Assert =
        assertNeq(unexpected = "a", "A")

    @Test
    def testNeqString04(): Unit \ Assert =
        assertNeq(unexpected = "abc", "abd")

}
