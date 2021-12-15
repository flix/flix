@test
def testLambda01(): Int32 -> Int32 = x -> x

@test
def testLambda02(): Int32 -> Int32 = (x) -> x

@test
def testLambda03(): (Int32, Int32) -> Int32 = (x, y) -> x + y

@test
def testLambda04(): Int32 -> (Int32, Int32) = x -> (x, x)

@test
def testLambda05(): (Bool, Int32, Int32) -> Int32 = (x, y, z) -> if (x) y else z

@test
def testLambda06(): Bool -> Int32 -> Int32 = y -> if (y) (x -> x + 1) else (x -> x + 2)

@test
def testLambda07(): Int32 -> Int32 = scoped x -> x

@test
def testLambda08(): Int32 -> Int32 -> Int32 = x -> y -> x + y

@test
def testLambda09(): Bool = testLambda09Helper()(3) == "3"
def testLambda09Helper(): Int32 -> String = x -> ToString.toString(x)

@test
def testLambda10(): Bool = testLambda10Helper()(5) == 5
def testLambda10Helper(): Int32 -> Int32 = x -> x

@test
def testLambda11(): Bool = testLambda11Helper1() == 4
def testLambda11Helper1(): Int32 =
    let x = (a: Int) -> testLambda11Helper2(a);
    x(4)
def testLambda11Helper2(x: a): a = x
