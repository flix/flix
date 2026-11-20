mod Test.Def.Op {

    use Assert.assertEq

    // Math-name operators: infix use desugars to an application of the same name.
    @Test
    def testUserOpMath01(): Unit \ Assert =
        assertEq(expected = "a ⊆ b", "a" ⊆ "b")

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

    def ⊆(x: String, y: String): String = "${x} ⊆ ${y}"

}
