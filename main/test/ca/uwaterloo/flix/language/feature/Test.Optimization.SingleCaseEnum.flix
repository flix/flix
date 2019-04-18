/**
 * A type safe wrapper around integers that are degrees celsius.
 */
type Celsius = Celsius(Int)

/**
 * A type safe wrapper around integers that are degrees fahrenheit.
 */
type Fahrenheit = Fahrenheit(Int)

/**
 * A type safe wrapper around celsius.
 */
type Temperature = Temperature(Celsius)

/**
 * A polymorphic wrapper.
 */
enum Wrapper[a, b, c] {
    case Wrapper(a, b, c)
}

@test
def testSingleCase01(): Celsius = Celsius(42)

@test
def testSingleCase02(): Fahrenheit = Fahrenheit(42)

@test
def testSingleCase03(): Temperature = Temperature(Celsius(42))

@test
def testSingleCase04(): Wrapper[Bool, Int, Str] = Wrapper(true, 42, "Hello World")

@test
def testSingleCase05(): Option[Celsius] = None

@test
def testSingleCase06(): Option[Celsius] = Some(Celsius(42))

@test
def testSingleCase07(): Option[Temperature] = Some(Temperature(Celsius(42)))

@test
def testSingleCase08(): Bool = match Celsius(42) with {
    case Celsius(x) => x == 42
}

@test
def testSingleCase09(): Bool =
    let Celsius(x) = Celsius(42);
        x == 42

@test
def testSingleCase10(): Bool = match Some(Celsius(42)) with {
    case None => true
    case Some(Celsius(x)) => x == 42
}

@test
def testSingleCase11(): Bool = match Some(Temperature(Celsius(42))) with {
    case None => true
    case Some(Temperature(Celsius(x))) => x == 42
}

@test
def testSingleCase12(): Bool = match Wrapper(true, 42, "Hello World") with {
    case Wrapper(_, y, _) => y == 42
}

@test
def testSingleCase13(): Bool =
    let xs = List.map(Celsius, List.range(0, 10));
    let ys = List.map(Fahrenheit, List.range(0, 10));
    let zs = List.map(Temperature, xs);
    let ws = List.map(Temperature, ys);
        List.length(zs) == 10 && List.length(ws) == 10

