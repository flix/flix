@test
def tagLambda01(): Option[Int] = (Some)(42)

@test
def tagLambda02(): Result[Int, Int] = (Ok)(42)

@test
def tagLambda03(): Result[Int, Int] = (Err)(42)

@test
def tagLambda04(): Option[Int] =
    let f = Some;
        f(123)

@test
def tagLambda05(): Result[Int, Int] =
    let f = Ok;
        f(42)

@test
def tagLambda06(): Result[Int, Int] =
    let f = Err;
        f(42)

@test
def tagLambda07(): List[Option[Int]] = List.map(Some, 1 :: 2 :: 3 :: Nil)

@test
def tagLambda08(): List[Result[Int, Int]] = List.map(Ok, 1 :: 2 :: 3 :: Nil)

@test
def tagLambda09(): List[Result[Int, Int]] = List.map(Err, 1 :: 2 :: 3 :: Nil)
