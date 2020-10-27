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
opaque type D[a, b, c] = Map[a, Result[b, c]]

@test
def opaqueType01(): Celsius = Celsius(123)

@test
def opaqueType02(): Bool = match Celsius(123) {
    case Celsius(x) => x == 123
}

@test
def opaqueType03(): Fahrenheit = Fahrenheit(123)

@test
def opaqueType04(): Bool = match Fahrenheit(123) {
    case Fahrenheit(x) => x == 123
}

@test
def opaqueType05(): USD = USD(123.456)

@test
def opaqueType06(): Bool = match USD(123.456) {
    case USD(x) => x == 123.456
}

@test
def opaqueType07(): CAD = CAD(123.456)

@test
def opaqueType08(): Bool = match CAD(123.456) {
    case CAD(x) => x == 123.456
}

@test
def opaqueType09(): A = A(None)

@test
def opaqueType10(): A = A(Some(123))

@test
def opaqueType11(): Bool = match A(None) {
    case A(x) => x == None
}

@test
def opaqueType12(): Bool = match A(Some(123)) {
    case A(x) => x == Some(123)
}

@test
def opaqueType13(): B = B(Ok(123))

@test
def opaqueType14(): B = B(Err("Hello World"))

@test
def opaqueType15(): Bool = match B(Ok(123)) {
    case B(x) => x == Ok(123)
}

@test
def opaqueType16(): Bool = match B(Err("Hello World")) {
    case B(x) => x == Err("Hello World")
}

@test
def opaqueType17(): Pair[Int, Int] = Pair((21, 42))

@test
def opaqueType18(): Bool = match Pair((21, 42)) {
    case Pair(x) => x == (21, 42)
}

@test
def opaqueType19(): Pair[Bool, Int] = Pair((true, 42))

@test
def opaqueType20(): Bool = match Pair((true, 42)) {
    case Pair(x) => x == (true, 42)
}

@test
def opaqueType21(): C[Bool] = C(Ok(true))

@test
def opaqueType22(): Bool = match C(Ok(true)) {
    case C(x) => x == Ok(true)
}

@test
def opaqueType23(): D[Bool, Int, String] = D(Map#{})

@test
def opaqueType24(): D[Bool, Int, String] = D(Map#{true -> Ok(123)})

@test
def opaqueType25(): D[Bool, Int, String] = D(Map#{true -> Err("Hello World")})

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
def opaqueType26(): U = U(None)

@test
def opaqueType27(): Bool = match U(None) {
    case U(x) => x == None
}

@test
def opaqueType28(): U = U(Some(true))

@test
def opaqueType29(): Bool = match U(Some(true)) {
    case U(x) => x == Some(true)
}

@test
def opaqueType30(): V = V(Ok(true))

@test
def opaqueType31(): Bool = match V(Ok(true)) {
    case V(x) => x == Ok(true)
}

@test
def opaqueType32(): V = V(Err(123))

@test
def opaqueType33(): Bool = match V(Err(123)) {
    case V(x) => x == Err(123)
}
