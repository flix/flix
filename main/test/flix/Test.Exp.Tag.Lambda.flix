namespace Test/Exp/Tag/Lambda {

    @test
    def testTagLambda01(): Option[Int] = (Some)(42)

    @test
    def testTagLambda02(): Result[Int, Int] = (Ok)(42)

    @test
    def testTagLambda03(): Result[Int, Int] = (Err)(42)

    @test
    def testTagLambda04(): Option[Int] =
        let f = Some;
            f(123)

    @test
    def testTagLambda05(): Result[Int, Int] =
        let f = Ok;
            f(42)

    @test
    def testTagLambda06(): Result[Int, Int] =
        let f = Err;
            f(42)

    @test
    def testTagLambda07(): List[Option[Int]] = List.map(Some, 1 :: 2 :: 3 :: Nil)

    @test
    def testTagLambda08(): List[Result[Int, Int]] = List.map(Ok, 1 :: 2 :: 3 :: Nil)

    @test
    def testTagLambda09(): List[Result[Int, Int]] = List.map(Err, 1 :: 2 :: 3 :: Nil)

}
