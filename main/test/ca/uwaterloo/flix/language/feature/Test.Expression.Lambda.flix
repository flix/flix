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
def testLambda06(): Bool -> Int -> Int = x -> if (x) (x -> x + 1) else (x -> x + 2)
