@test
def testNewChannelPrimitive1(): Channel[Int] = newch Int

@test
def testNewChannelPrimitive2(): Channel[Bool] = newch Bool

@test
def testNewChannelPrimitive3(): Channel[Unit] = newch Unit

@test
def testNewChannelPrimitive4(): Channel[Char] = newch Char

@test
def testNewChannelPrimitive5(): Channel[Float32] = newch Float32

@test
def testNewChannelPrimitive6(): Channel[BigInt] = newch BigInt

@test
def testNewChannelPrimitive7(): Channel[Str] = newch Str