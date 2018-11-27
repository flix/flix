//TODO SJ: fix naming

@test
def testNewChannel01(): Channel[Unit] = chan Unit

@test
def testNewChannel02(): Channel[Bool] = chan Bool

@test
def testNewChannel03(): Channel[Char] = chan Char

@test
def testNewChannel04(): Channel[Float] = chan Float

@test
def testNewChannel05(): Channel[Int] = chan Int

@test
def testNewChannel06(): Channel[BigInt] = chan BigInt

@test
def testNewChannel07(): Channel[Str] = chan Str

@test
def testNewChannel08(): Channel[Channel[Int]] = chan Channel[Int]

@test
def testNewChannel09(): Channel[Array[Int]] = chan Array[Int]

@test
def testNewChannel10(): Channel[Channel[Channel[Channel[Str]]]] = chan Channel[Channel[Channel[Str]]]

@test
def testNewChannel11(): Channel[Option[Int]] = chan Option[Int]