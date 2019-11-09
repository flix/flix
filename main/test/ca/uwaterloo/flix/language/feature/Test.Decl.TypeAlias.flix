/**
 * A type alias for degrees celsius.
 */
type alias Celsius = Int

/**
 * A type alias for degrees fahrenheit.
 */
type alias Fahrenheit = Int

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
type alias MaybeInt = Option[Int]

/**
 * A type alias for a polymorphic Result.
 */
type alias ResultOrString[a] = Result[a, Str]

@test
def typeAlias01(): Celsius = 123

@test
def typeAlias02(): Fahrenheit = 123

@test
def typeAlias03(): USD = 123.456

@test
def typeAlias04(): CAD = 123.456

@test
def typeAlias05(): MaybeInt = None

@test
def typeAlias06(): MaybeInt = Some(123)

@test
def typeAlias07(): ResultOrString[Bool] = Ok(true)

@test
def typeAlias08(): ResultOrString[Int] = Ok(123)

