mod Test.Exp.Lambda.Match {

    enum Celsius(Int32)
    enum Fahrenheit(Int32)

    enum EU(Celsius)
    enum US(Fahrenheit)

    @Test
    def testLambdaMatch01(): Int32 -> Int32 = match x -> x

    @Test
    def testLambdaMatch02(): ((Int32, Int32)) -> Int32 = match (x, y) -> (x + y) : Int32

    @Test
    def testLambdaMatch03(): ((Int32, Int32, Int32)) -> Int32 = match (x, y, z) -> (x + y + z) : Int32

    @Test
    def testLambdaMatch04(): (((Int32, Int32), (Int32, Int32))) -> Int32 = match ((x, y), (z, w)) -> (x + y + z + w) : Int32

    @Test
    def testLambdaMatch05(): Celsius -> Int32 = match Celsius.Celsius(x) -> x

    @Test
    def testLambdaMatch06(): Fahrenheit -> Int32 = match Fahrenheit.Fahrenheit(x) -> x

    @Test
    def testLambdaMatch07(): EU -> Int32 = match EU.EU(Celsius.Celsius(x)) -> x

    @Test
    def testLambdaMatch08(): US -> Int32 = match US.US(Fahrenheit.Fahrenheit(x)) -> x

    @Test
    def testLambdaMatch09(): Int32 = (match x -> x)(1)

    @Test
    def testLambdaMatch10(): Int32 = (match (x, y) -> x + y)((1, 2))

    @Test
    def testLambdaMatch11(): Int32 = (match (x, y, z) -> x + y + z)(((1, 2, 3)))

    @Test
    def testLambdaMatch12(): Int32 = (match ((x, y), (z, w)) -> x + y + z + w)(((1, 2), (3, 4)))

    @Test
    def testLambdaMatch13(): Int32 = (match Celsius.Celsius(x) -> x)(Celsius.Celsius(42))

    @Test
    def testLambdaMatch14(): Int32 = (match Fahrenheit.Fahrenheit(x) -> x)(Fahrenheit.Fahrenheit(42))

    @Test
    def testLambdaMatch15(): Int32 = (match EU.EU(Celsius.Celsius(x)) -> x)(EU.EU(Celsius.Celsius(42)))

    @Test
    def testLambdaMatch16(): Int32 = (match US.US(Fahrenheit.Fahrenheit(x)) -> x)(US.US(Fahrenheit.Fahrenheit(42)))

}
