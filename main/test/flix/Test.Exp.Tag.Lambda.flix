mod Test.Exp.Tag.Lambda {

    @CompileTest
    def testTagLambda01(): Option[Int32] = (Some)(42)

    @CompileTest
    def testTagLambda02(): Result[Int32, Int32] = (Ok)(42)

    @CompileTest
    def testTagLambda03(): Result[Int32, Int32] = (Err)(42)

    @CompileTest
    def testTagLambda04(): Option[Int32] =
        let f = Some;
            f(123)

    @CompileTest
    def testTagLambda05(): Result[Int32, Int32] =
        let f = Ok;
            f(42)

    @CompileTest
    def testTagLambda06(): Result[Int32, Int32] =
        let f = Err;
            f(42)

    @CompileTest
    def testTagLambda07(): List[Option[Int32]] = List.map(Some, 1 :: 2 :: 3 :: Nil)

    @CompileTest
    def testTagLambda08(): List[Result[Int32, Int32]] = List.map(Ok, 1 :: 2 :: 3 :: Nil)

    @CompileTest
    def testTagLambda09(): List[Result[Int32, Int32]] = List.map(Err, 1 :: 2 :: 3 :: Nil)

}
