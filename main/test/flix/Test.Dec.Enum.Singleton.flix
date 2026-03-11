mod Test.Dec.Enum.Singleton {

    use Assert.assertEq

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

    @CompileTest
    def testOpaqueType01(): Celsius = Celsius.Celsius(123)

    @Test
    def testOpaqueType02(): Unit \ Assert = match Celsius.Celsius(123) {
        case Celsius.Celsius(x) => assertEq(expected = 123, x)
    }

    @CompileTest
    def testOpaqueType03(): Fahrenheit = Fahrenheit.Fahrenheit(123)

    @Test
    def testOpaqueType04(): Unit \ Assert = match Fahrenheit.Fahrenheit(123) {
        case Fahrenheit.Fahrenheit(x) => assertEq(expected = 123, x)
    }

    @CompileTest
    def testOpaqueType05(): USD = USD.USD(123.456)

    @Test
    def testOpaqueType06(): Unit \ Assert = match USD.USD(123.456) {
        case USD.USD(x) => assertEq(expected = 123.456, x)
    }

    @CompileTest
    def testOpaqueType07(): CAD = CAD.CAD(123.456)

    @Test
    def testOpaqueType08(): Unit \ Assert = match CAD.CAD(123.456) {
        case CAD.CAD(x) => assertEq(expected = 123.456, x)
    }

    @CompileTest
    def testOpaqueType09(): A = A.A(None)

    @CompileTest
    def testOpaqueType10(): A = A.A(Some(123))

    @Test
    def testOpaqueType11(): Unit \ Assert = match A.A(None) {
        case A.A(x) => assertEq(expected = None, x)
    }

    @Test
    def testOpaqueType12(): Unit \ Assert = match A.A(Some(123)) {
        case A.A(x) => assertEq(expected = Some(123), x)
    }

    @CompileTest
    def testOpaqueType13(): B = B.B(Ok(123))

    @CompileTest
    def testOpaqueType14(): B = B.B(Err("Hello World"))

    @Test
    def testOpaqueType15(): Unit \ Assert = match B.B(Ok(123)) {
        case B.B(x) => assertEq(expected = Ok(123), x)
    }

    @Test
    def testOpaqueType16(): Unit \ Assert = match B.B(Err("Hello World")) {
        case B.B(x) => assertEq(expected = Err("Hello World"), x)
    }

    @CompileTest
    def testOpaqueType17(): Pair[Int32, Int32] = Pair.Pair(21, 42)

    @Test
    def testOpaqueType18(): Unit \ Assert = match Pair.Pair(21, 42) {
        case Pair.Pair(x, y) => assertEq(expected = (21, 42), (x, y))
    }

    @CompileTest
    def testOpaqueType19(): Pair[Bool, Int32] = Pair.Pair(true, 42)

    @Test
    def testOpaqueType20(): Unit \ Assert = match Pair.Pair(true, 42) {
        case Pair.Pair(x, y) => assertEq(expected = (true, 42), (x, y))
    }

    @CompileTest
    def testOpaqueType21(): C[Bool] = C.C(Ok(true))

    @Test
    def testOpaqueType22(): Unit \ Assert = match C.C(Ok(true)) {
        case C.C(x) => assertEq(expected = Ok(true), x)
    }

    @CompileTest
    def testOpaqueType23(): D[Bool, Int32, String] = D.D(Ok(true))

    @CompileTest
    def testOpaqueType24(): D[Bool, Int32, String] = D.D(Err(Ok(123)))

    @CompileTest
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

    @CompileTest
    def testOpaqueType26(): U = U.U(None)

    @Test
    def testOpaqueType27(): Unit \ Assert = match U.U(None) {
        case U.U(x) => assertEq(expected = None, x)
    }

    @CompileTest
    def testOpaqueType28(): U = U.U(Some(true))

    @Test
    def testOpaqueType29(): Unit \ Assert = match U.U(Some(true)) {
        case U.U(x) => assertEq(expected = Some(true), x)
    }

    @CompileTest
    def testOpaqueType30(): V = V.V(Err(true))

    @Test
    def testOpaqueType31(): Unit \ Assert = match V.V(Err(true)) {
        case V.V(x) => assertEq(expected = Err(true), x)
    }

    @CompileTest
    def testOpaqueType32(): V = V.V(Ok(123))

    @Test
    def testOpaqueType33(): Unit \ Assert = match V.V(Ok(123)) {
        case V.V(x) => assertEq(expected = Ok(123), x)
    }

}
