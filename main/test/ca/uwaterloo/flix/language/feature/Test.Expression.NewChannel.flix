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

/*

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
def testNewChannel23(): Channel[Channel[Int8]] = channel Channel[Int8]

@test
def testNewChannel24(): Channel[Channel[Int16]] = channel Channel[Int16]

@test
def testNewChannel25(): Channel[Channel[Int32]] = channel Channel[Int32]

@test
def testNewChannel26(): Channel[Channel[Int64]] = channel Channel[Int64]

@test
def testNewChannel27(): Channel[Channel[BigInt]] = channel Channel[BigInt]

@test
def testNewChannel28(): Channel[Channel[Bool]] = channel Channel[Bool]

@test
def testNewChannel29(): Channel[Channel[Str]] = channel Channel[Str]

@test
def testNewChannel30(): Channel[Channel[Float32]] = channel Channel[Float32]

@test
def testNewChannel31(): Channel[Channel[Float64]] = channel Channel[Float64]

@test
def testNewChannel32(): Channel[Channel[Char]] = channel Channel[Char]

@test
def testNewChannel33(): Channel[Channel[Color]] = channel Channel[Color]

@test
def testNewChannel34(): Channel[Channel[(Int, Int)]] = channel Channel[(Int, Int)]

@test
def testNewChannel35(): Channel[Channel[(Int8, Int8)]] = channel Channel[(Int8, Int8)]

@test
def testNewChannel36(): Channel[Channel[(Int16, Int16)]] = channel Channel[(Int16, Int16)]

@test
def testNewChannel37(): Channel[Channel[(Int32, Int32)]] = channel Channel[(Int32, Int32)]

@test
def testNewChannel38(): Channel[Channel[(Int64, Int64)]] = channel Channel[(Int64, Int64)]

@test
def testNewChannel39(): Channel[Channel[(BigInt, BigInt)]] = channel Channel[(BigInt, BigInt)]

@test
def testNewChannel40(): Channel[Channel[(Float32, Float32)]] = channel Channel[(Float32, Float32)]

@test
def testNewChannel41(): Channel[Channel[(Float64, Float64)]] = channel Channel[(Float64, Float64)]

@test
def testNewChannel42(): Channel[Channel[(Bool, Bool)]] = channel Channel[(Bool, Bool)]

@test
def testNewChannel43(): Channel[Channel[(Str, Str)]] = channel Channel[(Str, Str)]

@test
def testNewChannel44(): Channel[Channel[(Char, Char)]] = channel Channel[(Char, Char)]

@test
def testNewChannel45(): Channel[Channel[(Color, Color)]] = channel Channel[(Color, Color)]

@test
def testNewChannel46(): Channel[Channel[Channel[Int]]] = channel Channel[Channel[Int]]

@test
def testNewChannel47(): Channel[Channel[Channel[Int8]]] = channel Channel[Channel[Int8]]

@test
def testNewChannel48(): Channel[Channel[Channel[Int16]]] = channel Channel[Channel[Int16]]

@test
def testNewChannel49(): Channel[Channel[Channel[Int32]]] = channel Channel[Channel[Int32]]

@test
def testNewChannel50(): Channel[Channel[Channel[Int64]]] = channel Channel[Channel[Int64]]

@test
def testNewChannel51(): Channel[Channel[Channel[BigInt]]] = channel Channel[Channel[BigInt]]

@test
def testNewChannel52(): Channel[Channel[Channel[Float32]]] = channel Channel[Channel[Float32]]

@test
def testNewChannel53(): Channel[Channel[Channel[Float64]]] = channel Channel[Channel[Float64]]

@test
def testNewChannel54(): Channel[Channel[Channel[Bool]]] = channel Channel[Channel[Bool]]

@test
def testNewChannel55(): Channel[Channel[Channel[Str]]] = channel Channel[Channel[Str]]

@test
def testNewChannel56(): Channel[Channel[Channel[Char]]] = channel Channel[Channel[Char]]

@test
def testNewChannel57(): Channel[Channel[Channel[Color]]] = channel Channel[Channel[Color]]

@test
def testNewChannel58(): Channel[Channel[Channel[(Int, Int)]]] = channel Channel[Channel[(Int, Int)]]

@test
def testNewChannel59(): Channel[Channel[Channel[(Int8, Int8)]]] = channel Channel[Channel[(Int8, Int8)]]

@test
def testNewChannel60(): Channel[Channel[Channel[(Int16, Int16)]]] = channel Channel[Channel[(Int16, Int16)]]

@test
def testNewChannel61(): Channel[Channel[Channel[(Int32, Int32)]]] = channel Channel[Channel[(Int32, Int32)]]

@test
def testNewChannel62(): Channel[Channel[Channel[(Int64, Int64)]]] = channel Channel[Channel[(Int64, Int64)]]

@test
def testNewChannel63(): Channel[Channel[Channel[(BigInt, BigInt)]]] = channel Channel[Channel[(BigInt, BigInt)]]

@test
def testNewChannel64(): Channel[Channel[Channel[(Float32, Float32)]]] = channel Channel[Channel[(Float32, Float32)]]

@test
def testNewChannel65(): Channel[Channel[Channel[(Float64, Float64)]]] = channel Channel[Channel[(Float64, Float64)]]

@test
def testNewChannel66(): Channel[Channel[Channel[(Bool, Bool)]]] = channel Channel[Channel[(Bool, Bool)]]

@test
def testNewChannel67(): Channel[Channel[Channel[(Str, Str)]]] = channel Channel[Channel[(Str, Str)]]

@test
def testNewChannel68(): Channel[Channel[Channel[(Char, Char)]]] = channel Channel[Channel[(Char, Char)]]

@test
def testNewChannel69(): Channel[Channel[Channel[(Color, Color)]]] = channel Channel[Channel[(Color, Color)]]

@test
def testNewChannel70(): Channel[Channel[Channel[Channel[Int]]]] = channel Channel[Channel[Channel[Int]]]

@test
def testNewChannel71(): Channel[Channel[Channel[Channel[Int8]]]] = channel Channel[Channel[Channel[Int8]]]

@test
def testNewChannel72(): Channel[Channel[Channel[Channel[Int16]]]] = channel Channel[Channel[Channel[Int16]]]

@test
def testNewChannel73(): Channel[Channel[Channel[Channel[Int32]]]] = channel Channel[Channel[Channel[Int32]]]

@test
def testNewChannel74(): Channel[Channel[Channel[Channel[Int64]]]] = channel Channel[Channel[Channel[Int64]]]

@test
def testNewChannel75(): Channel[Channel[Channel[Channel[BigInt]]]] = channel Channel[Channel[Channel[BigInt]]]

@test
def testNewChannel76(): Channel[Channel[Channel[Channel[Float32]]]] = channel Channel[Channel[Channel[Float32]]]

@test
def testNewChannel77(): Channel[Channel[Channel[Channel[Float64]]]] = channel Channel[Channel[Channel[Float64]]]

@test
def testNewChannel78(): Channel[Channel[Channel[Channel[Bool]]]] = channel Channel[Channel[Channel[Bool]]]

@test
def testNewChannel79(): Channel[Channel[Channel[Channel[Str]]]] = channel Channel[Channel[Channel[Str]]]

@test
def testNewChannel80(): Channel[Channel[Channel[Channel[Char]]]] = channel Channel[Channel[Channel[Char]]]

@test
def testNewChannel81(): Channel[Channel[Channel[Channel[Color]]]] = channel Channel[Channel[Channel[Color]]]

@test
def testNewChannel82(): Channel[Channel[Channel[Channel[(Int, Int)]]]] = channel Channel[Channel[Channel[(Int, Int)]]]

@test
def testNewChannel83(): Channel[Channel[Channel[Channel[(Int8, Int8)]]]] = channel Channel[Channel[Channel[(Int8, Int8)]]]

@test
def testNewChannel84(): Channel[Channel[Channel[Channel[(Int16, Int16)]]]] = channel Channel[Channel[Channel[(Int16, Int16)]]]

@test
def testNewChannel85(): Channel[Channel[Channel[Channel[(Int32, Int32)]]]] = channel Channel[Channel[Channel[(Int32, Int32)]]]

@test
def testNewChannel86(): Channel[Channel[Channel[Channel[(Int64, Int64)]]]] = channel Channel[Channel[Channel[(Int64, Int64)]]]

@test
def testNewChannel87(): Channel[Channel[Channel[Channel[(BigInt, BigInt)]]]] = channel Channel[Channel[Channel[(BigInt, BigInt)]]]

@test
def testNewChannel88(): Channel[Channel[Channel[Channel[(Float32, Float32)]]]] = channel Channel[Channel[Channel[(Float32, Float32)]]]

@test
def testNewChannel89(): Channel[Channel[Channel[Channel[(Float64, Float64)]]]] = channel Channel[Channel[Channel[(Float64, Float64)]]]

@test
def testNewChannel90(): Channel[Channel[Channel[Channel[(Bool, Bool)]]]] = channel Channel[Channel[Channel[(Bool, Bool)]]]

@test
def testNewChannel91(): Channel[Channel[Channel[Channel[(Str, Str)]]]] = channel Channel[Channel[Channel[(Str, Str)]]]

@test
def testNewChannel92(): Channel[Channel[Channel[Channel[(Char, Char)]]]] = channel Channel[Channel[Channel[(Char, Char)]]]

@test
def testNewChannel93(): Channel[Channel[Channel[Channel[(Color, Color)]]]] = channel Channel[Channel[Channel[(Color, Color)]]]

@test
def testNewChannel94(): Channel[Channel[Channel[Channel[(Char, Char)]]]] = channel Channel[Channel[Channel[(Char, Char)]]]

@test
def testNewChannel95(): Channel[Channel[Unit]] = channel Channel[Unit]

@test
def testNewChannel96(): Channel[Channel[(Unit, Unit)]] = channel Channel[(Unit, Unit)]

@test
def testNewChannel97(): Channel[Channel[Channel[Unit]]] = channel Channel[Channel[Unit]]

@test
def testNewChannel98(): Channel[Channel[Channel[(Unit, Unit)]]] = channel Channel[Channel[(Unit, Unit)]]

@test
def testNewChannel99(): Channel[Channel[Channel[Channel[Unit]]]] = channel Channel[Channel[Channel[Unit]]]

@test
def testNewChannel100(): Channel[Channel[Channel[Channel[(Unit, Unit)]]]] = channel Channel[Channel[Channel[(Unit, Unit)]]]

*/

/************ Channels with specified buffersize ***********/

@test
def testNewChannel101(): Unit =
  let x = channel Int 2;
  ()

@test
def testNewChannel102(): Channel[Int] = channel Int 2

@test
def testNewChannel103(): Channel[Bool] = channel Bool 2

@test
def testNewChannel104(): Channel[Char] = channel Char 2

@test
def testNewChannel105(): Channel[Int8] = channel Int8 2

@test
def testNewChannel106(): Channel[Int16] = channel Int16 2

@test
def testNewChannel107(): Channel[Int32] = channel Int32 2

@test
def testNewChannel108(): Channel[Int64] = channel Int64 2

@test
def testNewChannel109(): Channel[BigInt] = channel BigInt 2

@test
def testNewChannel110(): Channel[Float32] = channel Float32 2

@test
def testNewChannel111(): Channel[Float64] = channel Float64 2

@test
def testNewChannel112(): Channel[Str] = channel Str 2

//@test
//def testNewChannel113(): Channel[(Int, Int)] = channel (Int, Int) 2

//@test
//def testNewChannel114(): Channel[Opt[Int]] = channel Opt[Int] 2

@test
def testNewChannel115(): Channel[List[Int]] = channel List[Int] 2

//@test
//def testNewChannel116(): Channel[Vec[Int]] = channel Vec[Int] 2

@test
def testNewChannel117(): Channel[Set[Int]] = channel Set[Int] 2

@test
def testNewChannel118(): Channel[Color] = channel Color 2

@test
def testNewChannel119(): Channel[Int -> Int] = channel Int -> Int 2

@test
def testNewChannel120(): Channel[(Int, Str) -> Int] = channel (Int, Str) -> Int 2

@test
def testNewChannel121(): Channel[Int -> Str -> Int] = channel Int -> Str -> Int 2

@test
def testNewChannel122(): Channel[Channel[Int]] = channel Channel[Int] 2

@test
def testNewChannel123(): Channel[Channel[Int8]] = channel Channel[Int8] 2

@test
def testNewChannel124(): Channel[Channel[Int16]] = channel Channel[Int16] 2

@test
def testNewChannel125(): Channel[Channel[Int32]] = channel Channel[Int32] 2

@test
def testNewChannel126(): Channel[Channel[Int64]] = channel Channel[Int64] 2

@test
def testNewChannel127(): Channel[Channel[BigInt]] = channel Channel[BigInt] 2

@test
def testNewChannel128(): Channel[Channel[Bool]] = channel Channel[Bool] 2

@test
def testNewChannel129(): Channel[Channel[Str]] = channel Channel[Str] 2

@test
def testNewChannel130(): Channel[Channel[Float32]] = channel Channel[Float32] 2

@test
def testNewChannel131(): Channel[Channel[Float64]] = channel Channel[Float64] 2

@test
def testNewChannel132(): Channel[Channel[Char]] = channel Channel[Char] 2

@test
def testNewChannel133(): Channel[Channel[Color]] = channel Channel[Color] 2

@test
def testNewChannel134(): Channel[Channel[(Int, Int)]] = channel Channel[(Int, Int)] 2

@test
def testNewChannel135(): Channel[Channel[(Int8, Int8)]] = channel Channel[(Int8, Int8)] 2

@test
def testNewChannel136(): Channel[Channel[(Int16, Int16)]] = channel Channel[(Int16, Int16)] 2

@test
def testNewChannel137(): Channel[Channel[(Int32, Int32)]] = channel Channel[(Int32, Int32)] 2

@test
def testNewChannel138(): Channel[Channel[(Int64, Int64)]] = channel Channel[(Int64, Int64)] 2

@test
def testNewChannel139(): Channel[Channel[(BigInt, BigInt)]] = channel Channel[(BigInt, BigInt)] 2

@test
def testNewChannel140(): Channel[Channel[(Float32, Float32)]] = channel Channel[(Float32, Float32)] 2

@test
def testNewChannel141(): Channel[Channel[(Float64, Float64)]] = channel Channel[(Float64, Float64)] 2

@test
def testNewChannel142(): Channel[Channel[(Bool, Bool)]] = channel Channel[(Bool, Bool)] 2

@test
def testNewChannel143(): Channel[Channel[(Str, Str)]] = channel Channel[(Str, Str)] 2

@test
def testNewChannel144(): Channel[Channel[(Char, Char)]] = channel Channel[(Char, Char)] 2

@test
def testNewChannel145(): Channel[Channel[(Color, Color)]] = channel Channel[(Color, Color)] 2

@test
def testNewChannel146(): Channel[Channel[Channel[Int]]] = channel Channel[Channel[Int]] 2

@test
def testNewChannel147(): Channel[Channel[Channel[Int8]]] = channel Channel[Channel[Int8]] 2

@test
def testNewChannel148(): Channel[Channel[Channel[Int16]]] = channel Channel[Channel[Int16]] 2

@test
def testNewChannel149(): Channel[Channel[Channel[Int32]]] = channel Channel[Channel[Int32]] 2

@test
def testNewChannel150(): Channel[Channel[Channel[Int64]]] = channel Channel[Channel[Int64]] 2

@test
def testNewChannel151(): Channel[Channel[Channel[BigInt]]] = channel Channel[Channel[BigInt]] 2

@test
def testNewChannel152(): Channel[Channel[Channel[Float32]]] = channel Channel[Channel[Float32]] 2

@test
def testNewChannel153(): Channel[Channel[Channel[Float64]]] = channel Channel[Channel[Float64]] 2

@test
def testNewChannel154(): Channel[Channel[Channel[Bool]]] = channel Channel[Channel[Bool]] 2

@test
def testNewChannel155(): Channel[Channel[Channel[Str]]] = channel Channel[Channel[Str]] 2

@test
def testNewChannel156(): Channel[Channel[Channel[Char]]] = channel Channel[Channel[Char]] 2

@test
def testNewChannel157(): Channel[Channel[Channel[Color]]] = channel Channel[Channel[Color]] 2

@test
def testNewChannel158(): Channel[Channel[Channel[(Int, Int)]]] = channel Channel[Channel[(Int, Int)]] 2

@test
def testNewChannel159(): Channel[Channel[Channel[(Int8, Int8)]]] = channel Channel[Channel[(Int8, Int8)]] 2

@test
def testNewChannel160(): Channel[Channel[Channel[(Int16, Int16)]]] = channel Channel[Channel[(Int16, Int16)]] 2

@test
def testNewChannel161(): Channel[Channel[Channel[(Int32, Int32)]]] = channel Channel[Channel[(Int32, Int32)]] 2

@test
def testNewChannel162(): Channel[Channel[Channel[(Int64, Int64)]]] = channel Channel[Channel[(Int64, Int64)]] 2

@test
def testNewChannel163(): Channel[Channel[Channel[(BigInt, BigInt)]]] = channel Channel[Channel[(BigInt, BigInt)]] 2

@test
def testNewChannel164(): Channel[Channel[Channel[(Float32, Float32)]]] = channel Channel[Channel[(Float32, Float32)]] 2

@test
def testNewChannel165(): Channel[Channel[Channel[(Float64, Float64)]]] = channel Channel[Channel[(Float64, Float64)]] 2

@test
def testNewChannel166(): Channel[Channel[Channel[(Bool, Bool)]]] = channel Channel[Channel[(Bool, Bool)]] 2

@test
def testNewChannel167(): Channel[Channel[Channel[(Str, Str)]]] = channel Channel[Channel[(Str, Str)]] 2

@test
def testNewChannel168(): Channel[Channel[Channel[(Char, Char)]]] = channel Channel[Channel[(Char, Char)]] 2

@test
def testNewChannel169(): Channel[Channel[Channel[(Color, Color)]]] = channel Channel[Channel[(Color, Color)]] 2

@test
def testNewChannel170(): Channel[Channel[Channel[Channel[Int]]]] = channel Channel[Channel[Channel[Int]]] 2

@test
def testNewChannel171(): Channel[Channel[Channel[Channel[Int8]]]] = channel Channel[Channel[Channel[Int8]]] 2

@test
def testNewChannel172(): Channel[Channel[Channel[Channel[Int16]]]] = channel Channel[Channel[Channel[Int16]]] 2

@test
def testNewChannel173(): Channel[Channel[Channel[Channel[Int32]]]] = channel Channel[Channel[Channel[Int32]]] 2

@test
def testNewChannel174(): Channel[Channel[Channel[Channel[Int64]]]] = channel Channel[Channel[Channel[Int64]]] 2

@test
def testNewChannel175(): Channel[Channel[Channel[Channel[BigInt]]]] = channel Channel[Channel[Channel[BigInt]]] 2

@test
def testNewChannel176(): Channel[Channel[Channel[Channel[Float32]]]] = channel Channel[Channel[Channel[Float32]]] 2

@test
def testNewChannel177(): Channel[Channel[Channel[Channel[Float64]]]] = channel Channel[Channel[Channel[Float64]]] 2

@test
def testNewChannel178(): Channel[Channel[Channel[Channel[Bool]]]] = channel Channel[Channel[Channel[Bool]]] 2

@test
def testNewChannel179(): Channel[Channel[Channel[Channel[Str]]]] = channel Channel[Channel[Channel[Str]]] 2

@test
def testNewChannel180(): Channel[Channel[Channel[Channel[Char]]]] = channel Channel[Channel[Channel[Char]]] 2

@test
def testNewChannel181(): Channel[Channel[Channel[Channel[Color]]]] = channel Channel[Channel[Channel[Color]]] 2

@test
def testNewChannel182(): Channel[Channel[Channel[Channel[(Int, Int)]]]] = channel Channel[Channel[Channel[(Int, Int)]]] 2

@test
def testNewChannel183(): Channel[Channel[Channel[Channel[(Int8, Int8)]]]] = channel Channel[Channel[Channel[(Int8, Int8)]]] 2

@test
def testNewChannel184(): Channel[Channel[Channel[Channel[(Int16, Int16)]]]] = channel Channel[Channel[Channel[(Int16, Int16)]]] 2

@test
def testNewChannel185(): Channel[Channel[Channel[Channel[(Int32, Int32)]]]] = channel Channel[Channel[Channel[(Int32, Int32)]]] 2

@test
def testNewChannel186(): Channel[Channel[Channel[Channel[(Int64, Int64)]]]] = channel Channel[Channel[Channel[(Int64, Int64)]]] 2

@test
def testNewChannel187(): Channel[Channel[Channel[Channel[(BigInt, BigInt)]]]] = channel Channel[Channel[Channel[(BigInt, BigInt)]]] 2

@test
def testNewChannel188(): Channel[Channel[Channel[Channel[(Float32, Float32)]]]] = channel Channel[Channel[Channel[(Float32, Float32)]]] 2

@test
def testNewChannel189(): Channel[Channel[Channel[Channel[(Float64, Float64)]]]] = channel Channel[Channel[Channel[(Float64, Float64)]]] 2

@test
def testNewChannel190(): Channel[Channel[Channel[Channel[(Bool, Bool)]]]] = channel Channel[Channel[Channel[(Bool, Bool)]]] 2

@test
def testNewChannel191(): Channel[Channel[Channel[Channel[(Str, Str)]]]] = channel Channel[Channel[Channel[(Str, Str)]]] 2

@test
def testNewChannel192(): Channel[Channel[Channel[Channel[(Char, Char)]]]] = channel Channel[Channel[Channel[(Char, Char)]]] 2

@test
def testNewChannel193(): Channel[Channel[Channel[Channel[(Color, Color)]]]] = channel Channel[Channel[Channel[(Color, Color)]]] 2

@test
def testNewChannel194(): Channel[Channel[Channel[Channel[(Char, Char)]]]] = channel Channel[Channel[Channel[(Char, Char)]]] 2

@test
def testNewChannel195(): Channel[Channel[Unit]] = channel Channel[Unit] 2

@test
def testNewChannel196(): Channel[Channel[(Unit, Unit)]] = channel Channel[(Unit, Unit)] 2

@test
def testNewChannel197(): Channel[Channel[Channel[Unit]]] = channel Channel[Channel[Unit]] 2

@test
def testNewChannel198(): Channel[Channel[Channel[(Unit, Unit)]]] = channel Channel[Channel[(Unit, Unit)]] 2

@test
def testNewChannel199(): Channel[Channel[Channel[Channel[Unit]]]] = channel Channel[Channel[Channel[Unit]]] 2

@test
def testNewChannel200(): Channel[Channel[Channel[Channel[(Unit, Unit)]]]] = channel Channel[Channel[Channel[(Unit, Unit)]]] 2

def bs01(): Int = 2

@test
def testNewChannel201(): Channel[Int] = channel Int bs01()

def bs02(): Int = 2

@test
def testNewChannel202(): Channel[Int8] = channel Int8 bs02()

def bs03(): Int = 2

@test
def testNewChannel203(): Channel[Int16] = channel Int16 bs03()

def bs04(): Int = 2

@test
def testNewChannel204(): Channel[Int32] = channel Int32 bs04()

def bs05(): Int = 2

@test
def testNewChannel205(): Channel[Int64] = channel Int64 bs05()

def bs06(): Int = 2

@test
def testNewChannel206(): Channel[BigInt] = channel BigInt bs06()

def bs07(): Int = 2

@test
def testNewChannel207(): Channel[Float32] = channel Float32 bs07()

def bs08(): Int = 2

@test
def testNewChannel208(): Channel[Float64] = channel Float64 bs08()

def bs09(): Int = 2

@test
def testNewChannel209(): Channel[Bool] = channel Bool bs09()

def bs10(): Int = 2

@test
def testNewChannel210(): Channel[Str] = channel Str bs10()

def bs11(): Int = 2

@test
def testNewChannel211(): Channel[Char] = channel Char bs11()

def bs12(): Int = 2

@test
def testNewChannel212(): Channel[Color] = channel Color bs12()

def bs13(): Int = 2

@test
def testNewChannel213(): Channel[Unit] = channel Unit bs13()
