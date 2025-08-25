mod Test.Exp.Lambda.ExtMatch {

    @test
    def testLambdaExtMatch01(): #| A(Int32) |# -> Int32 = ematch A(x) -> x

    @test
    def testLambdaExtMatch02(): #| A(Int32, Int32) |# -> Int32 = ematch A(x, y) -> ((x + y) : Int32)

    @test
    def testLambdaExtMatch03(): #| A(Int32, Int32, Int32) |# -> Int32 = ematch A(x, y, z) -> ((x + y + z) : Int32)

    @test
    def testLambdaExtMatch04(): #| A(Int32, Int32, Int32, Int32) |# -> Int32 = ematch A(x, y, z, _) -> ((x + y + z) : Int32)

    @test
    def testLambdaExtMatch05(): #| A(Int32) |# -> Int32 = ematch _ -> 42

    @test
    def testLambdaExtMatch06(): #| A(Int32), B(Bool) |# -> Int32 = ematch _ -> 42

    @test
    def testLambdaExtMatch07(): Int32 = (ematch A(x) -> x)(xvar A(1))

    @test
    def testLambdaExtMatch08(): Int32 = (ematch A(x, y) -> ((x + y) : Int32))(xvar A(1, 2))

    @test
    def testLambdaExtMatch09(): Int32 = (ematch A(x, y, z) -> ((x + y + z) : Int32))(xvar A(1, 2, 3))

    @test
    def testLambdaExtMatch10(): Int32 = (ematch A(x, y, z, _) -> ((x + y + z) : Int32))(xvar A(1, 2, 3, 4))

    @test
    def testLambdaExtMatch11(): Int32 = (ematch _ -> 42)(xvar A(0))

    @test
    def testLambdaExtMatch12(): Int32 = (ematch _ -> 42)(xvar B(0))

    @test
    def testLambdaExtMatch13(): Int32 = (ematch _ -> 42)(if (true) xvar A(0) else xvar B(0))

    @test
    def testLambdaExtMatch14(): Int32 = (ematch _ -> 42)(if (true) xvar A(0) else if (true) xvar B(0) else xvar C(0))

}
