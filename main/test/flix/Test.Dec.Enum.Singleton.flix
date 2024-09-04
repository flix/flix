mod Test.Dec.Enum.Singleton {

    ///
    /// An enum for degrees celsius.
     ///
    enum Celsius(Int32)

    ///
    /// An enum for degrees fahrenheit.
    ///
    enum Fahrenheit(Int32)

    ///
    /// An enum for united states dollars.
    ///
    enum USD(Float64)

    ///
    /// An enum for canadian dollars.
    ///
    enum CAD(Float64)

    ///
    /// An enum for an Option.
    ///
    enum A(Option[Int32])

    ///
    /// An enum for a Result.
    ///
    enum B(Result[String, Int32])

    ///
    /// An enum for a polymorphic pair.
    ///
    enum Pair[a, b](a, b)

    ///
    /// An enum for a polymorphic Result.
    ///
    enum C[a](Result[Int32, a])

    ///
    /// An enum for a polymorphic Result.
    ///
    enum D[a, b, c](Result[Result[c, b], a])

    @test
    def testOpaqueType01(): Celsius = Celsius.Celsius(123)

    @test
    def testOpaqueType02(): Bool = match Celsius.Celsius(123) {
        case Celsius.Celsius(x) => x == 123
    }

    @test
    def testOpaqueType03(): Fahrenheit = Fahrenheit.Fahrenheit(123)

    @test
    def testOpaqueType04(): Bool = match Fahrenheit.Fahrenheit(123) {
        case Fahrenheit.Fahrenheit(x) => x == 123
    }

    @test
    def testOpaqueType05(): USD = USD.USD(123.456)

    @test
    def testOpaqueType06(): Bool = match USD.USD(123.456) {
        case USD.USD(x) => x == 123.456
    }

    @test
    def testOpaqueType07(): CAD = CAD.CAD(123.456)

    @test
    def testOpaqueType08(): Bool = match CAD.CAD(123.456) {
        case CAD.CAD(x) => x == 123.456
    }

    @test
    def testOpaqueType09(): A = A.A(None)

    @test
    def testOpaqueType10(): A = A.A(Some(123))

    @test
    def testOpaqueType11(): Bool = match A.A(None) {
        case A.A(x) => x == None
    }

    @test
    def testOpaqueType12(): Bool = match A.A(Some(123)) {
        case A.A(x) => x == Some(123)
    }

    @test
    def testOpaqueType13(): B = B.B(Ok(123))

    @test
    def testOpaqueType14(): B = B.B(Err("Hello World"))

    @test
    def testOpaqueType15(): Bool = match B.B(Ok(123)) {
        case B.B(x) => x == Ok(123)
    }

    @test
    def testOpaqueType16(): Bool = match B.B(Err("Hello World")) {
        case B.B(x) => x == Err("Hello World")
    }

    @test
    def testOpaqueType17(): Pair[Int32, Int32] = Pair.Pair((21, 42))

    @test
    def testOpaqueType18(): Bool = match Pair.Pair((21, 42)) {
        case Pair.Pair(x) => x == (21, 42)
    }

    @test
    def testOpaqueType19(): Pair[Bool, Int32] = Pair.Pair((true, 42))

    @test
    def testOpaqueType20(): Bool = match Pair.Pair((true, 42)) {
        case Pair.Pair(x) => x == (true, 42)
    }

    @test
    def testOpaqueType21(): C[Bool] = C.C(Ok(true))

    @test
    def testOpaqueType22(): Bool = match C.C(Ok(true)) {
        case C.C(x) => x == Ok(true)
    }

    @test
    def testOpaqueType23(): D[Bool, Int32, String] = D.D(Ok(true))

    @test
    def testOpaqueType24(): D[Bool, Int32, String] = D.D(Err(Ok(123)))

    @test
    def testOpaqueType25(): D[Bool, Int32, String] = D.D(Err(Err("hello")))

    ///
    /// A type alias for Bool.
    ///
    type alias X = Bool

    ///
    /// A type alias for Int.
    ///
    type alias Y = Int32

    ///
    /// A polymorphic type alias for Option.
    ///
    type alias Z[a] = Option[a]

    ///
    /// An enum for with a type alias.
    ///
    enum U(Option[X])

    ///
    /// An enum for with a type alias.
    ///
    enum V(Result[X, Y])

    @test
    def testOpaqueType26(): U = U.U(None)

    @test
    def testOpaqueType27(): Bool = match U.U(None) {
        case U.U(x) => x == None
    }

    @test
    def testOpaqueType28(): U = U.U(Some(true))

    @test
    def testOpaqueType29(): Bool = match U.U(Some(true)) {
        case U.U(x) => x == Some(true)
    }

    @test
    def testOpaqueType30(): V = V.V(Err(true))

    @test
    def testOpaqueType31(): Bool = match V.V(Err(true)) {
        case V.V(x) => x == Err(true)
    }

    @test
    def testOpaqueType32(): V = V.V(Ok(123))

    @test
    def testOpaqueType33(): Bool = match V.V(Ok(123)) {
        case V.V(x) => x == Ok(123)
    }

}
