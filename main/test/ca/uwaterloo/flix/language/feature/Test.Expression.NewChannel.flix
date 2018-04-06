/////////////////////////////////////////////////////////////////////////////
// Tests the new Channel function that creates a new Channel               //
//                                                                         //
/////////////////////////////////////////////////////////////////////////////

def f01(): Channel[Int] = channel Int

def f02(): Channel[Bool] = channel Bool

def f03(): Channel[Char] = channel Char

def f04(): Channel[Int8] = channel Int8

def f05(): Channel[Int16] = channel Int16

def f06(): Channel[Int32] = channel Int32

def f07(): Channel[Int62] = channel Int64

def f08(): Channel[BigInt] = channel BigInt

def f09(): Channel[Float32] = channel Float32

def f010(): Channel[Float64] = channel Float64

def f011(): Channel[Str] = channel Str

def f012(): Channel[Int -> Int] = channel Int -> Int

def f013(): Channel[(Int, Str) -> Int] = channel (Int, Str) -> Int

def f014(): Channel[(Int, Str) -> Int] = channel Int -> Str -> Int

def f15(): Channel[Channel[Int]] = channel Channel[Int]

def f16(): Channel[Int] = channel Int:10

def f20(): Unit = let x = channel Int; ()










