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

    def _testLambdaUnary01(): a -> a =
        p -> p

    def _testLambdaUnary02(): a -> a =
        p ->p

    def _testLambdaUnary03(): a -> a =
        p-> p

    def _testLambdaMany01(): a -> b -> _c -> a =
        a -> _b -> _c -> a

    def _testLambdaMany02(): a -> _b -> _c -> a =
        a -> _b -> _c ->a

    def _testLambdaMany03(): a -> _b -> _c -> a =
        a -> _b -> _c-> a

    def _testLambdaMany04(): a -> _b -> _c -> a =
        a -> _b ->_c -> a

    def _testLambdaMany05(): a -> _b -> _c -> a =
        a -> _b-> _c -> a

    def _testLambdaMany06(): a -> _b -> _c -> a =
        a ->_b -> _c -> a

    def _testLambdaMany07(): a -> _b -> _c -> a =
        a-> _b -> _c -> a
}
