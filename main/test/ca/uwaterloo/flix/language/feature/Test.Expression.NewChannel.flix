/////////////////////////////////////////////////////////////////////////////
// Tests the new Channel function that creates a new Channel               //
//                                                                         //
/////////////////////////////////////////////////////////////////////////////

/************ Channels with default buffersize ***********/
enum Color {
    case Red,
    case Green,
    case Blue
}

@test
def testNewChannel01(): Unit =
  let x = channel Int;
  ()

@test
def testNewChannel02(): Channel[Int] = channel Int

@test
def testNewChannel03(): Channel[Bool] = channel Bool

@test
def testNewChannel04(): Channel[Char] = channel Char

@test
def testNewChannel05(): Channel[Int8] = channel Int8

@test
def testNewChannel06(): Channel[Int16] = channel Int16

@test
def testNewChannel07(): Channel[Int32] = channel Int32

@test
def testNewChannel08(): Channel[Int64] = channel Int64

@test
def testNewChannel09(): Channel[BigInt] = channel BigInt

@test
def testNewChannel10(): Channel[Float32] = channel Float32

@test
def testNewChannel11(): Channel[Float64] = channel Float64

@test
def testNewChannel12(): Channel[Str] = channel Str

@test
def testNewChannel13(): Channel[(Int, Int)] = channel (Int, Int)

//@test
//def testNewChannel14(): Channel[Opt[Int]] = channel Opt[Int]

@test
def testNewChannel15(): Channel[List[Int]] = channel List[Int]

//@test
//def testNewChannel16(): Channel[Vec[Int]] = channel Vec[Int]

@test
def testNewChannel17(): Channel[Set[Int]] = channel Set[Int]

@test
def testNewChannel18(): Channel[Color] = channel Color

@test
def testNewChannel19(): Channel[Int -> Int] = channel Int -> Int

@test
def testNewChannel20(): Channel[(Int, Str) -> Int] = channel (Int, Str) -> Int

@test
def testNewChannel21(): Channel[Int -> (Str -> Int)] = channel Int -> Str -> Int

@test
def testNewChannel22(): Channel[Channel[Int]] = channel Channel[Int]

@test
def testNewChannel23(): Channel[Channel[Channel[Int]]] = channel Channel[Channel[Int]]

@test
def testNewChannel24(): Channel[Channel[Channel[Channel[Int]]]] = channel Channel[Channel[Channel[Int]]]

/************ Channels with specified buffersize ***********/

@test
def testNewChannel25(): Unit =
  let x = channel Int 2;
  ()

@test
def testNewChannel26(): Channel[Int] = channel Int 2

@test
def testNewChannel27(): Channel[Bool] = channel Bool 2

@test
def testNewChannel28(): Channel[Char] = channel Char 2

@test
def testNewChannel29(): Channel[Int8] = channel Int8 2

@test
def testNewChannel30(): Channel[Int16] = channel Int16 2

@test
def testNewChannel31(): Channel[Int32] = channel Int32 2

@test
def testNewChannel32(): Channel[Int64] = channel Int64 2

@test
def testNewChannel33(): Channel[BigInt] = channel BigInt 2

@test
def testNewChannel34(): Channel[Float32] = channel Float32 2

@test
def testNewChannel35(): Channel[Float64] = channel Float64 2

@test
def testNewChannel36(): Channel[Str] = channel Str 2

//@test
//def testNewChannel37(): Channel[(Int, Int)] = channel (Int, Int) 2
//
//@test
//def testNewChannel38(): Channel[Opt[Int]] = channel Opt[Int] 2

@test
def testNewChannel39(): Channel[List[Int]] = channel List[Int] 2

//@test
//def testNewChannel40(): Channel[Vec[Int]] = channel Vec[Int] 2

@test
def testNewChannel41(): Channel[Set[Int]] = channel Set[Int] 2

@test
def testNewChannel42(): Channel[Color] = channel Color 2

@test
def testNewChannel43(): Channel[Int -> Int] = channel Int -> Int 2

@test
def testNewChannel44(): Channel[(Int, Str) -> Int] = channel (Int, Str) -> Int 2

@test
def testNewChannel45(): Channel[Int -> Str -> Int] = channel Int -> Str -> Int 2

@test
def testNewChannel46(): Channel[Channel[Int]] = channel Channel[Int] 2

def bufferSize():Int = 2

@test
def testNewChannel52(): Channel[Int] = channel Int bufferSize()
