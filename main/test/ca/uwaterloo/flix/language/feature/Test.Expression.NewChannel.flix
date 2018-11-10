@test
def testNewChannelPrimitiveUnit(): Channel[Unit] = newch Unit

@test
def testNewChannelPrimitiveBool(): Channel[Bool] = newch Bool

@test
def testNewChannelPrimitiveChar(): Channel[Char] = newch Char

@test
def testNewChannelPrimitiveFloat(): Channel[Float] = newch Float

@test
def testNewChannelPrimitiveInt(): Channel[Int] = newch Int

@test
def testNewChannelPrimitiveBigInt(): Channel[BigInt] = newch BigInt

@test
def testNewChannelPrimitiveStr(): Channel[Str] = newch Str

@test
def testNewChannelPrimitiveChannel(): Channel[Channel[Int]] = newch Channel[Int]

@test
def testNewChannelPrimitiveArray(): Channel[Array[Int]] = newch Array[Int]

@test
def testNewChannelPrimitiveNested(): Channel[Channel[Channel[Channel[Str]]]] = newch Channel[Channel[Channel[Str]]]

