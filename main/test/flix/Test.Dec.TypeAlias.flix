mod Test.Dec.TypeAlias {

    /**
     * A type alias for degrees celsius.
     */
    type alias Celsius = Int32

    /**
     * A type alias for degrees fahrenheit.
     */
    type alias Fahrenheit = Int32

    /**
     * A type alias for united states dollars.
     */
    type alias USD = Float64

    /**
     * A type alias for canadian dollars.
     */
    type alias CAD = Float64

    /**
     * A type alias for Option of Int.
     */
    type alias A = Option[Int32]

    /**
     * A type alias for a Result.
     */
    type alias B = Result[Int32, Bool]

    /**
     * A polymorphic type alias for a Option.
     */
    type alias C[a] = Option[a]

    /**
     * A polymorphic type alias for a Result.
     */
    type alias D[a, b] = Result[a, b]

    /**
     * A partially polymorphic type alias for a Result.
     */
    type alias E[a] = Result[String, a]

    @test
    def testTypeAlias01(): Celsius = 123

    @test
    def testTypeAlias02(): Fahrenheit = 123

    @test
    def testTypeAlias03(): Int32 = (123: Celsius) + (123: Fahrenheit)

    @test
    def testTypeAlias04(): USD = 123.456

    @test
    def testTypeAlias05(): CAD = 123.456

    @test
    def testTypeAlias06(): Float64 = (123.456: USD) + (123.456: CAD)

    @test
    def testTypeAlias07(): A = None

    @test
    def testTypeAlias08(): A = Some(123)

    @test
    def testTypeAlias09(): B = Ok(true)

    @test
    def testTypeAlias10(): B = Err(123)

    @test
    def testTypeAlias11(): C[Bool] = None

    @test
    def testTypeAlias12(): C[Bool] = Some(true)

    @test
    def testTypeAlias13(): C[Int32] = None

    @test
    def testTypeAlias14(): C[Int32] = Some(123)

    @test
    def testTypeAlias15(): D[Bool, Int32] = Ok(123)

    @test
    def testTypeAlias16(): D[Bool, Int32] = Err(true)

    @test
    def testTypeAlias17(): E[Bool] = Ok(true)

    @test
    def testTypeAlias18(): E[Int32] = Ok(123)

    @test
    def testTypeAlias19(): E[Bool] = Err("Hello World")

    @test
    def testTypeAlias20(): E[Int32] = Err("Hello World")

    /**
     * An enum for Int.
     */
    enum X(Int32)

    /**
     * An enum for Option.
     */
    enum Y[a](Option[a])

    /**
     * An enum for Result.
     */
    enum Z[a, b](Result[a, b])

    /**
     * A type alias for an enum.
     */
    type alias U = X

    /**
     * A type alias for a polymorphic enum.
     */
    type alias V[a] = Y[a]

    /**
     * A type alias for a polymorphic enum.
     */
    type alias W[a, b] = Z[a, b]

    @test
    def testTypeAlias21(): U = X.X(123)

    @test
    def testTypeAlias22(): V[Bool] = Y.Y(None)

    @test
    def testTypeAlias23(): V[Bool] = Y.Y(Some(true))

    @test
    def testTypeAlias24(): V[Int32] = Y.Y(None)

    @test
    def testTypeAlias25(): V[Int32] = Y.Y(Some(123))

    @test
    def testTypeAlias26(): W[Bool, Int32] = Z.Z(Ok(123))

    @test
    def testTypeAlias27(): W[Bool, Int32] = Z.Z(Err(true))

    ///
    /// Type-level if-then-else
    ///
    type alias If[cond: Bool, t: Bool, f: Bool] = (cond and t) or (not cond and f)

    ///
    /// Type alias with phantom type.
    ///
    type alias P[_: Bool] = String

    @test
    def testTypeAlias28(): P[If[true, true, false]] = "xyz": P[true]
}
