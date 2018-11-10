@test
def testNewChannelOnUnit(): Channel[Unit] = newch Unit

@test
def testNewChannelOnBool(): Channel[Bool] = newch Bool

@test
def testNewChannelOnChar(): Channel[Char] = newch Char

@test
def testNewChannelOnFloat(): Channel[Float] = newch Float

@test
def testNewChannelOnInt(): Channel[Int] = newch Int

@test
def testNewChannelOnBigInt(): Channel[BigInt] = newch BigInt

@test
def testNewChannelOnStr(): Channel[Str] = newch Str

@test
def testNewChannelOnChannel(): Channel[Channel[Int]] = newch Channel[Int]

@test
def testNewChannelOnArray(): Channel[Array[Int]] = newch Array[Int]

@test
def testNewChannelOnNestedType(): Channel[Channel[Channel[Channel[Str]]]] = newch Channel[Channel[Channel[Str]]]

