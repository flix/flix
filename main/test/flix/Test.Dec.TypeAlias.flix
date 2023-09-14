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

    @Test
    def testTypeAlias01(): Celsius = 123

    @Test
    def testTypeAlias02(): Fahrenheit = 123

    @Test
    def testTypeAlias03(): Int32 = (123: Celsius) + (123: Fahrenheit)

    @Test
    def testTypeAlias04(): USD = 123.456

    @Test
    def testTypeAlias05(): CAD = 123.456

    @Test
    def testTypeAlias06(): Float64 = (123.456: USD) + (123.456: CAD)

    @Test
    def testTypeAlias07(): A = None

    @Test
    def testTypeAlias08(): A = Some(123)

    @Test
    def testTypeAlias09(): B = Ok(true)

    @Test
    def testTypeAlias10(): B = Err(123)

    @Test
    def testTypeAlias11(): C[Bool] = None

    @Test
    def testTypeAlias12(): C[Bool] = Some(true)

    @Test
    def testTypeAlias13(): C[Int32] = None

    @Test
    def testTypeAlias14(): C[Int32] = Some(123)

    @Test
    def testTypeAlias15(): D[Bool, Int32] = Ok(123)

    @Test
    def testTypeAlias16(): D[Bool, Int32] = Err(true)

    @Test
    def testTypeAlias17(): E[Bool] = Ok(true)

    @Test
    def testTypeAlias18(): E[Int32] = Ok(123)

    @Test
    def testTypeAlias19(): E[Bool] = Err("Hello World")

    @Test
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

    @Test
    def testTypeAlias21(): U = X.X(123)

    @Test
    def testTypeAlias22(): V[Bool] = Y.Y(None)

    @Test
    def testTypeAlias23(): V[Bool] = Y.Y(Some(true))

    @Test
    def testTypeAlias24(): V[Int32] = Y.Y(None)

    @Test
    def testTypeAlias25(): V[Int32] = Y.Y(Some(123))

    @Test
    def testTypeAlias26(): W[Bool, Int32] = Z.Z(Ok(123))

    @Test
    def testTypeAlias27(): W[Bool, Int32] = Z.Z(Err(true))

    ///
    /// Type-level if-then-else
    ///
    type alias If[cond: Bool, t: Bool, f: Bool] = (cond and t) or (not cond and f)

    ///
    /// Type alias with phantom type.
    ///
    type alias P[_: Bool] = String

    @Test
    def testTypeAlias28(): P[If[true, true, false]] = "xyz": P[true]
}
