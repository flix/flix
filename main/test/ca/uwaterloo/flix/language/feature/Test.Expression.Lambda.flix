@test
def testLambda01(): Int -> Int = x -> x

@test
def testLambda02(): Int -> Int = (x) -> x

@test
def testLambda03(): (Int, Int) -> Int = (x, y) -> x + y

@test
def testLambda04(): Int -> (Int, Int) = x -> (x, x)

@test
def testLambda05(): (Bool, Int, Int) -> Int = (x, y, z) -> if (x) y else z

@test
def testLambda06(): Bool -> Int -> Int = y -> if (y) (x -> x + 1) else (x -> x + 2)

@test
def testLambda07(): Int -> Int = scoped x -> x

@test
def testLambda08(): Int -> Int -> Int = x -> y -> x + y

@test
def testLambda09(): Bool = testLambda09Helper()(3) == "3"
def testLambda09Helper(): Int -> String = x -> ToString.toString(x)

@test
def testLambda10(): Bool = testLambda10Helper()(5) == 5
def testLambda10Helper(): Int -> Int = x -> x
