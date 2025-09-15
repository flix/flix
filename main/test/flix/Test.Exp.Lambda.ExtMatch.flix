mod Test.Exp.Lambda.ExtMatch {

    @Test
    def testLambdaExtMatch01(): #| A(Int32) |# -> Int32 = ematch A(x) -> x

    @Test
    def testLambdaExtMatch02(): #| A(Int32, Int32) |# -> Int32 = ematch A(x, y) -> ((x + y) : Int32)

    @Test
    def testLambdaExtMatch03(): #| A(Int32, Int32, Int32) |# -> Int32 = ematch A(x, y, z) -> ((x + y + z) : Int32)

    @Test
    def testLambdaExtMatch04(): #| A(Int32, Int32, Int32, Int32) |# -> Int32 = ematch A(x, y, z, _) -> ((x + y + z) : Int32)

    @Test
    def testLambdaExtMatch05(): #| A(Int32) |# -> Int32 = ematch _ -> 42

    @Test
    def testLambdaExtMatch06(): #| A(Int32), B(Bool) |# -> Int32 = ematch _ -> 42

    @Test
    def testLambdaExtMatch07(): Bool = (ematch A(x) -> x)(xvar A(1)) == 1

    @Test
    def testLambdaExtMatch08(): Bool = (ematch A(x, y) -> ((x + y) : Int32))(xvar A(1, 2)) == 3

    @Test
    def testLambdaExtMatch09(): Bool = (ematch A(x, y, z) -> ((x + y + z) : Int32))(xvar A(1, 2, 3)) == 6

    @Test
    def testLambdaExtMatch10(): Bool = (ematch A(x, y, z, _) -> ((x + y + z) : Int32))(xvar A(1, 2, 3, 4)) == 6

    @Test
    def testLambdaExtMatch11(): Bool = (ematch _ -> 42)(xvar A(0)) == 42

    @Test
    def testLambdaExtMatch12(): Bool = (ematch _ -> 42)(xvar B(0)) == 42

    @Test
    def testLambdaExtMatch13(): Bool = (ematch _ -> 42)(if (true) xvar A(0) else xvar B(0)) == 42

    @Test
    def testLambdaExtMatch14(): Bool = (ematch _ -> 42)(if (true) xvar A(0) else if (true) xvar B(0) else xvar C(0)) == 42

}
