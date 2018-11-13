//TODO SJ: fix naming

@test
def testNewChannelOnUnit(): Channel[Unit] = chan Unit

@test
def testNewChannelOnBool(): Channel[Bool] = chan Bool

@test
def testNewChannelOnChar(): Channel[Char] = chan Char

@test
def testNewChannelOnFloat(): Channel[Float] = chan Float

@test
def testNewChannelOnInt(): Channel[Int] = chan Int

@test
def testNewChannelOnBigInt(): Channel[BigInt] = chan BigInt

@test
def testNewChannelOnStr(): Channel[Str] = chan Str

@test
def testNewChannelOnChannel(): Channel[Channel[Int]] = chan Channel[Int]

@test
def testNewChannelOnArray(): Channel[Array[Int]] = chan Array[Int]

@test
def testNewChannelOnNestedType(): Channel[Channel[Channel[Channel[Str]]]] = chan Channel[Channel[Channel[Str]]]

