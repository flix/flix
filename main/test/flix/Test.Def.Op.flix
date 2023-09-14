mod Test.Def.Op {
    def ||(x: String, y: String): String = "${x} || ${y}"

    def &&(x: String, y: String): String = "${x} && ${y}"

    def |=(x: String, y: String): String = "${x} |= ${y}"

    @Test
    def testUserOp01(): Bool = "a" || "b" == "a || b"

    @Test
    def testUserOp02(): Bool = "a" && "b" == "a && b"

    @Test
    def testUserOp03(): Bool = "a" |= "b" == "a |= b"
}
