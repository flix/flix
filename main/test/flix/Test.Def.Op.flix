mod Test.Def.Op {
    def ||(x: String, y: String): String = "${x} || ${y}"

    def &&(x: String, y: String): String = "${x} && ${y}"

    def |=(x: String, y: String): String = "${x} |= ${y}"

    @test
    def testUserOp01(): Bool = "a" || "b" == "a || b"

    @test
    def testUserOp02(): Bool = "a" && "b" == "a && b"

    @test
    def testUserOp03(): Bool = "a" |= "b" == "a |= b"
}
