mod Test.Equality.Bool {

    use Assert.{assertEq, assertNeq}

    @Test
    def testEqBool01(): Unit \ Assert =
        assertEq(expected = true, true)

    @Test
    def testEqBool02(): Unit \ Assert =
        assertEq(expected = false, false)

    @Test
    def testNeqBool01(): Unit \ Assert =
        assertNeq(unexpected = true, false)

    @Test
    def testNeqBool02(): Unit \ Assert =
        assertNeq(unexpected = false, true)

}
