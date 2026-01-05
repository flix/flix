mod Test.Exp.Lambda.ExtMatch {

    use Assert.assertEq

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
    def testLambdaExtMatch07(): Unit \ Assert = assertEq(expected = 1, (ematch A(x) -> x)(xvar A(1)))

    @Test
    def testLambdaExtMatch08(): Unit \ Assert = assertEq(expected = 3, (ematch A(x, y) -> ((x + y) : Int32))(xvar A(1, 2)))

    @Test
    def testLambdaExtMatch09(): Unit \ Assert = assertEq(expected = 6, (ematch A(x, y, z) -> ((x + y + z) : Int32))(xvar A(1, 2, 3)))

    @Test
    def testLambdaExtMatch10(): Unit \ Assert = assertEq(expected = 6, (ematch A(x, y, z, _) -> ((x + y + z) : Int32))(xvar A(1, 2, 3, 4)))

    @Test
    def testLambdaExtMatch11(): Unit \ Assert = assertEq(expected = 42, (ematch _ -> 42)(xvar A(0)))

    @Test
    def testLambdaExtMatch12(): Unit \ Assert = assertEq(expected = 42, (ematch _ -> 42)(xvar B(0)))

    @Test
    def testLambdaExtMatch13(): Unit \ Assert = assertEq(expected = 42, (ematch _ -> 42)(if (true) xvar A(0) else xvar B(0)))

    @Test
    def testLambdaExtMatch14(): Unit \ Assert = assertEq(expected = 42, (ematch _ -> 42)(if (true) xvar A(0) else if (true) xvar B(0) else xvar C(0)))

}
