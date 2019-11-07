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
 * An opaque type for an Option of Int.
 */
opaque type MaybeInt = Option[Int]

/**
 * An opaque type for a polymorphic Result.
 */
opaque type ResultOrString[a] = Result[a, Str]

@test
def opaqueType01(): Celsius = Celsius(123)

@test
def opaqueType02(): Fahrenheit = Fahrenheit(123)

@test
def opaqueType03(): USD = USD(123.456)

@test
def opaqueType04(): CAD = CAD(123.456)

@test
def opaqueType05(): MaybeInt = MaybeInt(None)

@test
def opaqueType06(): MaybeInt = MaybeInt(Some(123))

@test
def opaqueType07(): ResultOrString[Int] = ResultOrString(Ok(123))

@test
def opaqueType08(): ResultOrString[Int] = ResultOrString(Err("hello world"))

@test
def opaqueType09(): Bool = match Celsius(123) with {
    case Celsius(x) => x == 123
}

@test
def opaqueType10(): Bool = match Fahrenheit(123) with {
    case Fahrenheit(x) => x == 123
}

@test
def opaqueType11(): Bool = match USD(123.456) with {
    case USD(x) => x == 123.456
}

@test
def opaqueType12(): Bool = match CAD(123.456) with {
    case CAD(x) => x == 123.456
}

@test
def opaqueType13(): Bool = match MaybeInt(None) with {
    case MaybeInt(None) => true
    case _              => false
}

@test
def opaqueType14(): Bool = match MaybeInt(Some(123)) with {
    case MaybeInt(Some(123))    => true
    case _                      => false
}

@test
def opaqueType15(): Bool = match ResultOrString(Ok(123)) with {
    case ResultOrString(Ok(123))    => true
    case _                          => false
}

@test
def opaqueType16(): Bool = match ResultOrString(Err("hello world")) with {
    case ResultOrString(Err("hello world")) => true
    case _                                  => false
}
