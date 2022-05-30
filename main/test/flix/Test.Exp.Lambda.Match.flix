namespace Test/Exp/Lambda/Match {

    enum Celsius(Int32)
    enum Fahrenheit(Int32)

    enum EU(Celsius)
    enum US(Fahrenheit)

    @test
    def testLambdaMatch01(): Int32 -> Int32 = match x -> x

    @test
    def testLambdaMatch02(): ((Int32, Int32)) -> Int32 = match (x, y) -> (x + y) : Int32

    @test
    def testLambdaMatch03(): ((Int32, Int32, Int32)) -> Int32 = match (x, y, z) -> (x + y + z) : Int32

    @test
    def testLambdaMatch04(): (((Int32, Int32), (Int32, Int32))) -> Int32 = match ((x, y), (z, w)) -> (x + y + z + w) : Int32

    @test
    def testLambdaMatch05(): Celsius -> Int32 = match Celsius(x) -> x

    @test
    def testLambdaMatch06(): Fahrenheit -> Int32 = match Fahrenheit(x) -> x

    @test
    def testLambdaMatch07(): EU -> Int32 = match EU(Celsius(x)) -> x

    @test
    def testLambdaMatch08(): US -> Int32 = match US(Fahrenheit(x)) -> x

    @test
    def testLambdaMatch09(): Int32 = (match x -> x)(1)

    @test
    def testLambdaMatch10(): Int32 = (match (x, y) -> x + y)((1, 2))

    @test
    def testLambdaMatch11(): Int32 = (match (x, y, z) -> x + y + z)(((1, 2, 3)))

    @test
    def testLambdaMatch12(): Int32 = (match ((x, y), (z, w)) -> x + y + z + w)(((1, 2), (3, 4)))

    @test
    def testLambdaMatch13(): Int32 = (match Celsius(x) -> x)(Celsius(42))

    @test
    def testLambdaMatch14(): Int32 = (match Fahrenheit(x) -> x)(Fahrenheit(42))

    @test
    def testLambdaMatch15(): Int32 = (match EU(Celsius(x)) -> x)(EU(Celsius(42)))

    @test
    def testLambdaMatch16(): Int32 = (match US(Fahrenheit(x)) -> x)(US(Fahrenheit(42)))

}
