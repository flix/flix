mod Test.Exp.Tag.Lambda {

    @Test
    def testTagLambda01(): Option[Int32] = (Some)(42)

    @Test
    def testTagLambda02(): Result[Int32, Int32] = (Ok)(42)

    @Test
    def testTagLambda03(): Result[Int32, Int32] = (Err)(42)

    @Test
    def testTagLambda04(): Option[Int32] =
        let f = Some;
            f(123)

    @Test
    def testTagLambda05(): Result[Int32, Int32] =
        let f = Ok;
            f(42)

    @Test
    def testTagLambda06(): Result[Int32, Int32] =
        let f = Err;
            f(42)

    @Test
    def testTagLambda07(): List[Option[Int32]] = List.map(Some, 1 :: 2 :: 3 :: Nil)

    @Test
    def testTagLambda08(): List[Result[Int32, Int32]] = List.map(Ok, 1 :: 2 :: 3 :: Nil)

    @Test
    def testTagLambda09(): List[Result[Int32, Int32]] = List.map(Err, 1 :: 2 :: 3 :: Nil)

}
