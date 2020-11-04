namespace Test/Exp/Lambda/Match {

    opaque type Celsius = Int
    opaque type Fahrenheit = Int

    opaque type EU = Celsius
    opaque type US = Fahrenheit

    @test
    def testLambdaMatch01(): Int -> Int = match x -> x

    @test
    def testLambdaMatch02(): ((Int, Int)) -> Int = match (x, y) -> (x + y) : Int32

    @test
    def testLambdaMatch03(): ((Int, Int, Int)) -> Int = match (x, y, z) -> (x + y + z) : Int32

    @test
    def testLambdaMatch04(): (((Int, Int), (Int, Int))) -> Int = match ((x, y), (z, w)) -> (x + y + z + w) : Int32

    @test
    def testLambdaMatch05(): Celsius -> Int = match Celsius(x) -> x

    @test
    def testLambdaMatch06(): Fahrenheit -> Int = match Fahrenheit(x) -> x

    @test
    def testLambdaMatch07(): EU -> Int = match EU(Celsius(x)) -> x

    @test
    def testLambdaMatch08(): US -> Int = match US(Fahrenheit(x)) -> x

    @test
    def testLambdaMatch09(): Int = (match x -> x)(1)

    @test
    def testLambdaMatch10(): Int = (match (x, y) -> x + y)((1, 2))

    @test
    def testLambdaMatch11(): Int = (match (x, y, z) -> x + y + z)(((1, 2, 3)))

    @test
    def testLambdaMatch12(): Int = (match ((x, y), (z, w)) -> x + y + z + w)(((1, 2), (3, 4)))

    @test
    def testLambdaMatch13(): Int = (match Celsius(x) -> x)(Celsius(42))

    @test
    def testLambdaMatch14(): Int = (match Fahrenheit(x) -> x)(Fahrenheit(42))

    @test
    def testLambdaMatch15(): Int = (match EU(Celsius(x)) -> x)(EU(Celsius(42)))

    @test
    def testLambdaMatch16(): Int = (match US(Fahrenheit(x)) -> x)(US(Fahrenheit(42)))

}
