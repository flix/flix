mod Test.Exp.Lambda.Space {

    @CompileTest
    def testLambdaNullary01(): Unit -> Int32 =
        () -> 15

    @CompileTest
    def testLambdaNullary02(): Unit -> Int32 =
        () ->15

    @CompileTest
    def testLambdaNullary03(): Unit -> Int32 =
        ()-> 15

    @CompileTest
    def testLambdaUnary01(): a -> a =
        p -> p

    @CompileTest
    def testLambdaUnary02(): a -> a =
        p ->p

    @CompileTest
    def testLambdaUnary03(): a -> a =
        p-> p

    @CompileTest
    def testLambdaMany01(): a -> b -> _c -> a =
        a -> _b -> _c -> a

    @CompileTest
    def testLambdaMany02(): a -> _b -> _c -> a =
        a -> _b -> _c ->a

    @CompileTest
    def testLambdaMany03(): a -> _b -> _c -> a =
        a -> _b -> _c-> a

    @CompileTest
    def testLambdaMany04(): a -> _b -> _c -> a =
        a -> _b ->_c -> a

    @CompileTest
    def testLambdaMany05(): a -> _b -> _c -> a =
        a -> _b-> _c -> a

    @CompileTest
    def testLambdaMany06(): a -> _b -> _c -> a =
        a ->_b -> _c -> a

    @CompileTest
    def testLambdaMany07(): a -> _b -> _c -> a =
        a-> _b -> _c -> a

}
