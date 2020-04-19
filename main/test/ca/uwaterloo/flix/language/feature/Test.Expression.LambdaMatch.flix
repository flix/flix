opaque type Celsius = Int
opaque type Fahrenheit = Int

opaque type EU = Celsius
opaque type US = Fahrenheit

@test
def testMatchLambda01(): Int -> Int = match x -> x

@test
def testMatchLambda02(): ((Int, Int)) -> Int = match (x, y) -> (x + y) : Int32

@test
def testMatchLambda03(): ((Int, Int, Int)) -> Int = match (x, y, z) -> (x + y + z) : Int32

@test
def testMatchLambda04(): (((Int, Int), (Int, Int))) -> Int = match ((x, y), (z, w)) -> (x + y + z + w) : Int32

@test
def testMatchLambda05(): Celsius -> Int = match Celsius(x) -> x

@test
def testMatchLambda06(): Fahrenheit -> Int = match Fahrenheit(x) -> x

@test
def testMatchLambda07(): EU -> Int = match EU(Celsius(x)) -> x

@test
def testMatchLambda08(): US -> Int = match US(Fahrenheit(x)) -> x

@test
def testMatchLambda09(): Int = (match x -> x)(1)

@test
def testMatchLambda10(): Int = (match (x, y) -> x + y)((1, 2))

@test
def testMatchLambda11(): Int = (match (x, y, z) -> x + y + z)(((1, 2, 3)))

@test
def testMatchLambda12(): Int = (match ((x, y), (z, w)) -> x + y + z + w)(((1, 2), (3, 4)))

@test
def testMatchLambda13(): Int = (match Celsius(x) -> x)(Celsius(42))

@test
def testMatchLambda14(): Int = (match Fahrenheit(x) -> x)(Fahrenheit(42))

@test
def testMatchLambda15(): Int = (match EU(Celsius(x)) -> x)(EU(Celsius(42)))

@test
def testMatchLambda16(): Int = (match US(Fahrenheit(x)) -> x)(US(Fahrenheit(42)))
