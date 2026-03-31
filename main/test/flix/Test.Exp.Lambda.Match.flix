mod Test.Exp.Lambda.Match {

    enum Celsius(Int32)
    enum Fahrenheit(Int32)

    enum EU(Celsius)
    enum US(Fahrenheit)

    @CompileTest
    def testLambdaMatch01(): Int32 -> Int32 = match x -> x

    @CompileTest
    def testLambdaMatch02(): ((Int32, Int32)) -> Int32 = match (x, y) -> ((x + y) : Int32)

    @CompileTest
    def testLambdaMatch03(): ((Int32, Int32, Int32)) -> Int32 = match (x, y, z) -> ((x + y + z) : Int32)

    @CompileTest
    def testLambdaMatch04(): (((Int32, Int32), (Int32, Int32))) -> Int32 = match ((x, y), (z, w)) -> ((x + y + z + w) : Int32)

    @CompileTest
    def testLambdaMatch05(): Celsius -> Int32 = match Celsius.Celsius(x) -> x

    @CompileTest
    def testLambdaMatch06(): Fahrenheit -> Int32 = match Fahrenheit.Fahrenheit(x) -> x

    @CompileTest
    def testLambdaMatch07(): EU -> Int32 = match EU.EU(Celsius.Celsius(x)) -> x

    @CompileTest
    def testLambdaMatch08(): US -> Int32 = match US.US(Fahrenheit.Fahrenheit(x)) -> x

    @CompileTest
    def testLambdaMatch09(): Int32 = (match x -> x)(1)

    @CompileTest
    def testLambdaMatch10(): Int32 = (match (x, y) -> x + y)((1, 2))

    @CompileTest
    def testLambdaMatch11(): Int32 = (match (x, y, z) -> x + y + z)(((1, 2, 3)))

    @CompileTest
    def testLambdaMatch12(): Int32 = (match ((x, y), (z, w)) -> x + y + z + w)(((1, 2), (3, 4)))

    @CompileTest
    def testLambdaMatch13(): Int32 = (match Celsius.Celsius(x) -> x)(Celsius.Celsius(42))

    @CompileTest
    def testLambdaMatch14(): Int32 = (match Fahrenheit.Fahrenheit(x) -> x)(Fahrenheit.Fahrenheit(42))

    @CompileTest
    def testLambdaMatch15(): Int32 = (match EU.EU(Celsius.Celsius(x)) -> x)(EU.EU(Celsius.Celsius(42)))

    @CompileTest
    def testLambdaMatch16(): Int32 = (match US.US(Fahrenheit.Fahrenheit(x)) -> x)(US.US(Fahrenheit.Fahrenheit(42)))

}
