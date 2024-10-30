mod Test.Exp.Lambda.Space {
    @test
    def testLambdaNullary01(): Unit -> Int32 =
        () -> 15

    @test
    def testLambdaNullary02(): Unit -> Int32 =
        () ->15

    @test
    def testLambdaNullary03(): Unit -> Int32 =
        ()-> 15

    @test
    def testLambdaUnary01(): a -> a =
        p -> p

    @test
    def testLambdaUnary02(): a -> a =
        p ->p

    @test
    def testLambdaUnary03(): a -> a =
        p-> p

    @test
    def testLambdaMany01(): a -> b -> _c -> a =
        a -> _b -> _c -> a

    @test
    def testLambdaMany02(): a -> _b -> _c -> a =
        a -> _b -> _c ->a

    @test
    def testLambdaMany03(): a -> _b -> _c -> a =
        a -> _b -> _c-> a

    @test
    def testLambdaMany04(): a -> _b -> _c -> a =
        a -> _b ->_c -> a

    @test
    def testLambdaMany05(): a -> _b -> _c -> a =
        a -> _b-> _c -> a

    @test
    def testLambdaMany06(): a -> _b -> _c -> a =
        a ->_b -> _c -> a

    @test
    def testLambdaMany07(): a -> _b -> _c -> a =
        a-> _b -> _c -> a
}
