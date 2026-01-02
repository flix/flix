mod Test.Exp.Unary.Bool {

    use Assert.{assertTrue, assertFalse}

    @Test
    def testLogicalNot01(): Unit \ Assert = assertFalse(not true)

    @Test
    def testLogicalNot02(): Unit \ Assert = assertTrue(not false)

    @Test
    def testLogicalNot03(): Unit \ Assert = assertFalse(not true)

    @Test
    def testLogicalNot04(): Unit \ Assert = assertTrue(not false)

}
