enum SingleCaseEnum {
    case Single
}

@test
def testEnum01(): SingleCaseEnum = Single

@test
def testEnum02(): SingleCaseEnum = SingleCaseEnum.Single

enum TwoCaseEnum {
    case One,
    case Two
}

@test
def testEnum03(): TwoCaseEnum = One

@test
def testEnum04(): TwoCaseEnum = TwoCaseEnum.One

@test
def testEnum05(): TwoCaseEnum = Two

@test
def testEnum06(): TwoCaseEnum = TwoCaseEnum.Two

enum ThreeCaseEnum {
    case Uno(Bool),
    case Duo(Bool, Int),
    case Tre(Bool, Int, Str)
}

@test
def testEnum07(): ThreeCaseEnum = Uno(true)

@test
def testEnum08(): ThreeCaseEnum = Duo(true, 42)

@test
def testEnum09(): ThreeCaseEnum = Tre(true, 42, "Hello World")

enum PolymorphicEnum[a] {
    case Mono,
    case Poly(a)
}

@test
def testEnum10(): PolymorphicEnum[Bool] = Mono

@test
def testEnum11(): PolymorphicEnum[Int] = Mono

@test
def testEnum12(): PolymorphicEnum[Bool] = Poly(true)

@test
def testEnum13(): PolymorphicEnum[Int] = Poly(42)

enum ColorNoSep {
    case Red
    case Blu
}

@test
def testEnum014(): ColorNoSep = Red

@test
def testEnum015(): ColorNoSep = Blu

enum AnimalShort {
    case Cat, Dog
}

@test
def testEnum016(): AnimalShort = Cat

@test
def testEnum017(): AnimalShort = Dog
