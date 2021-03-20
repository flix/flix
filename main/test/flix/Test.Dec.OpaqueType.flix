namespace Test/Dec/OpaqueType {

    /**
     * An opaque type for degrees celsius.
     */
    opaque type Celsius = Int

    /**
     * An opaque type for degrees fahrenheit.
     */
    opaque type Fahrenheit = Int

    /**
     * An opaque type for united states dollars.
     */
    opaque type USD = Float64

    /**
     * An opaque type for canadian dollars.
     */
    opaque type CAD = Float64

    /**
     * An opaque type for an Option.
     */
    opaque type A = Option[Int]

    /**
     * An opaque type for a Result.
     */
    opaque type B = Result[Int, String]

    /**
     * An opaque type for a polymorphic pair.
     */
    opaque type Pair[a, b] = (a, b)

    /**
     * An opaque type for a polymorphic Result.
     */
    opaque type C[a] = Result[a, Int]

    /**
     * An opaque type for a polymorphic Result.
     */
    opaque type D[a, b, c] = Result[a, Result[b, c]]

    @test
    def testOpaqueType01(): Celsius = Celsius(123)

    @test
    def testOpaqueType02(): Bool = match Celsius(123) {
        case Celsius(x) => x == 123
    }

    @test
    def testOpaqueType03(): Fahrenheit = Fahrenheit(123)

    @test
    def testOpaqueType04(): Bool = match Fahrenheit(123) {
        case Fahrenheit(x) => x == 123
    }

    @test
    def testOpaqueType05(): USD = USD(123.456)

    @test
    def testOpaqueType06(): Bool = match USD(123.456) {
        case USD(x) => x == 123.456
    }

    @test
    def testOpaqueType07(): CAD = CAD(123.456)

    @test
    def testOpaqueType08(): Bool = match CAD(123.456) {
        case CAD(x) => x == 123.456
    }

    @test
    def testOpaqueType09(): A = A(None)

    @test
    def testOpaqueType10(): A = A(Some(123))

    @test
    def testOpaqueType11(): Bool = match A(None) {
        case A(x) => x == None
    }

    @test
    def testOpaqueType12(): Bool = match A(Some(123)) {
        case A(x) => x == Some(123)
    }

    @test
    def testOpaqueType13(): B = B(Ok(123))

    @test
    def testOpaqueType14(): B = B(Err("Hello World"))

    @test
    def testOpaqueType15(): Bool = match B(Ok(123)) {
        case B(x) => x == Ok(123)
    }

    @test
    def testOpaqueType16(): Bool = match B(Err("Hello World")) {
        case B(x) => x == Err("Hello World")
    }

    @test
    def testOpaqueType17(): Pair[Int, Int] = Pair((21, 42))

    @test
    def testOpaqueType18(): Bool = match Pair((21, 42)) {
        case Pair(x) => x == (21, 42)
    }

    @test
    def testOpaqueType19(): Pair[Bool, Int] = Pair((true, 42))

    @test
    def testOpaqueType20(): Bool = match Pair((true, 42)) {
        case Pair(x) => x == (true, 42)
    }

    @test
    def testOpaqueType21(): C[Bool] = C(Ok(true))

    @test
    def testOpaqueType22(): Bool = match C(Ok(true)) {
        case C(x) => x == Ok(true)
    }

    @test
    def testOpaqueType23(): D[Bool, Int, String] = D(Ok(true))

    @test
    def testOpaqueType24(): D[Bool, Int, String] = D(Err(Ok(123)))

    @test
    def testOpaqueType25(): D[Bool, Int, String] = D(Err(Err("hello")))

    /**
     * A type alias for Bool.
     */
    type alias X = Bool

    /**
     * A type alias for Int.
     */
    type alias Y = Int

    /**
     * A polymorphic type alias for Option.
     */
    type alias Z[a] = Option[a]

    /**
     * An opaque type for with a type alias.
     */
    opaque type U = Option[X]

    /**
     * An opaque type for with a type alias.
     */
    opaque type V = Result[X, Y]

    @test
    def testOpaqueType26(): U = U(None)

    @test
    def testOpaqueType27(): Bool = match U(None) {
        case U(x) => x == None
    }

    @test
    def testOpaqueType28(): U = U(Some(true))

    @test
    def testOpaqueType29(): Bool = match U(Some(true)) {
        case U(x) => x == Some(true)
    }

    @test
    def testOpaqueType30(): V = V(Ok(true))

    @test
    def testOpaqueType31(): Bool = match V(Ok(true)) {
        case V(x) => x == Ok(true)
    }

    @test
    def testOpaqueType32(): V = V(Err(123))

    @test
    def testOpaqueType33(): Bool = match V(Err(123)) {
        case V(x) => x == Err(123)
    }

    enum Option[t] {
        case None,
        case Some(t)
    }

    enum Result[t, e] {
        case Ok(t),
        case Err(e)
    }

    instance Eq[Option[a]] with[a : Eq] {
        pub def eq(o1: Option[a], o2: Option[a]): Bool = match (o1, o2) {
            case (None, None) => true
            case (Some(v1), Some(v2)) => v1 == v2
            case _ => false
        }
    }

    instance Eq[Result[t, e]] with[t : Eq, e : Eq] {
        pub def eq(r1: Result[t, e], r2: Result[t, e]): Bool = match (r1, r2) {
            case (Ok(v1), Ok(v2)) => v1 == v2
            case (Err(w1), Err(w2)) => w1 == w2
            case _ => false
        }
    }

}
