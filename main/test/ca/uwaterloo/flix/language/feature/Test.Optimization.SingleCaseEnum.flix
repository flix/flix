// Introduce two single-case enums.
type Celsius = Celsius(Int)
type Fahrenheit = Fahrenheit(Int)

@test
def testSingleCase01(): Celsius = Celsius(42)

@test
def testSingleCase02(): Fahrenheit = Fahrenheit(42)

