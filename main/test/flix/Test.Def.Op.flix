mod Test.Def.Op {

    use Assert.assertEq

    @Test
    def testUserOp01(): Unit \ Assert =
        assertEq(expected = "a || b", "a" || "b")

    @Test
    def testUserOp02(): Unit \ Assert =
        assertEq(expected = "a && b", "a" && "b")

    @Test
    def testUserOp03(): Unit \ Assert =
        assertEq(expected = "a |= b", "a" |= "b")

    def ||(x: String, y: String): String = "${x} || ${y}"

    def &&(x: String, y: String): String = "${x} && ${y}"

    def |=(x: String, y: String): String = "${x} |= ${y}"

}
