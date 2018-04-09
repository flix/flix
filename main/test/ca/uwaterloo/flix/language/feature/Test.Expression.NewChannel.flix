/////////////////////////////////////////////////////////////////////////////
// Tests the new Channel function that creates a new Channel               //
//                                                                         //
/////////////////////////////////////////////////////////////////////////////

@test
def newChannel01(): Channel[Int] = channel Int

@test
def newChannel02(): Channel[Bool] = channel Bool

@test
def newChannel03(): Channel[Char] = channel Char

@test
def newChannel04(): Channel[Int8] = channel Int8

@test
def newChannel05(): Channel[Int16] = channel Int16

@test
def newChannel06(): Channel[Int32] = channel Int32

@test
def newChannel07(): Channel[Int62] = channel Int64

@test
def newChannel08(): Channel[BigInt] = channel BigInt

@test
def newChannel09(): Channel[Float32] = channel Float32

@test
def newChannel010(): Channel[Float64] = channel Float64

@test
def newChannel011(): Channel[Str] = channel Str

@test
def newChannel012(): Channel[Int -> Int] = channel Int -> Int

@test
def newChannel013(): Channel[(Int, Str) -> Int] = channel (Int, Str) -> Int

@test
def newChannel014(): Channel[(Int, Str) -> Int] = channel Int -> Str -> Int

@test
def newChannel15(): Channel[Channel[Int]] = channel Channel[Int]

@test
def newChannel16(): Channel[Int] = channel Int:10

@test
def newChannel20(): Unit = let x = channel Int; ()










