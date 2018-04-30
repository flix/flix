/////////////////////////////////////////////////////////////////////////////
// Select expressions (channel select)                                     //
/////////////////////////////////////////////////////////////////////////////

/******************************* Test units *******************************/
def testSelectChannelUnit(): Unit =
  let ch: Channel[Unit] = channel Unit 1;
  ch <- ();;
  select {
      case x <- ch => x
  }

@test
def testSelectChannel01(): Bool = testSelectChannelUnit() `assertEq!` ()

/******************************* Test bools *******************************/
def testSelectChannelBool(var: Bool): Bool =
  let ch: Channel[Bool] = channel Bool 1;
  ch <- var;;
  select {
      case x <- ch => x
  }

@test
def testSelectChannel02(): Bool = testSelectChannelBool(true) `assertEq!` true

@test
def testSelectChannel03(): Bool = testSelectChannelBool(false) `assertEq!` false

/******************************* Test integers *******************************/
def testSelectChannelInt(var: Int): Int =
  let ch: Channel[Int] = channel Int 1;
  ch <- var;;
  select {
      case x <- ch => x
  }

@test
def testSelectChannel04(): Bool = testSelectChannelInt(0) `assertEq!` 0

@test
def testSelectChannel05(): Bool = testSelectChannelInt(-1) `assertEq!` -1

@test
def testSelectChannel06(): Bool = testSelectChannelInt(1) `assertEq!` 1

@test
def testSelectChannel07(): Bool = testSelectChannelInt(-100) `assertEq!` -100

@test
def testSelectChannel08(): Bool = testSelectChannelInt(100) `assertEq!` 100

/******************************* Test floats *******************************/
//def testSelectChannelFloat32(var: Float32): Float32 =
//  let ch: Channel[Float32] = channel Float32 1;
//  ch <- var;;
//  select {
//      case x <- ch => x
//  }

// @test
// def testSelectChannel09(): Bool = testSelectChannelFloat32(0f32) `assertEq!` 0f32
// 
// @test
// def testSelectChannel10(): Bool = testSelectChannelFloat32(-1.1f32) `assertEq!` -1.1f32
// 
// @test
// def testSelectChannel11(): Bool = testSelectChannelFloat32(1.1f32) `assertEq!` 1.1f32

/******************************* Test strings *******************************/
def testSelectChannelStr(var: Str): Str =
  let ch: Channel[Str] = channel Str 1;
  ch <- var;;
  select {
      case x <- ch => x
  }

@test
def testSelectChannel12(): Bool = testSelectChannelStr("") `assertEq!` ""

@test
def testSelectChannel13(): Bool = testSelectChannelStr("str") `assertEq!` "str"

/******************************* Test chars *******************************/
def testSelectChannelChar(var: Char): Char =
  let ch: Channel[Char] = channel Char 1;
  ch <- var;;
  select {
      case x <- ch => x
  }

@test
def testSelectChannel14(): Bool = testSelectChannelChar('a') `assertEq!` 'a'

@test
def testSelectChannel15(): Bool = testSelectChannelChar('b') `assertEq!` 'b'

/******************************* Test tuples *******************************/
def testSelectChannelIntStrTuple(var: (Int, Str)): (Int, Str) =
  let ch: Channel[(Int, Str)] = channel (Int, Str);
  ch <- var;;
  select {
      case x <- ch => x
  }

def testSelectChannelIntIntTuple(var: (Int, Int)): (Int, Int) =
  let ch: Channel[(Int, Int)] = channel (Int, Int);
  ch <- var;;
  select {
      case x <- ch => x
  }

def testSelectChannelIntIntIntIntTuple(var: (Int, Int, Int, Int)): (Int, Int, Int, Int) =
  let ch: Channel[(Int, Int, Int, Int)] = channel (Int, Int, Int, Int);
  ch <- var;;
  select {
      case x <- ch => x
  }

@test
def testSelectChannel16(): Bool = testSelectChannelIntStrTuple((2, "Str")) `assertEq!` (2, "Str")

@test
def testSelectChannel17(): Bool = testSelectChannelIntIntTuple((2, 10)) `assertEq!` (2, 10)

@test
def testSelectChannel18(): Bool = testSelectChannelIntIntIntIntTuple((2, 10, 12, 0)) `assertEq!` (2, 10, 12, 0)

/******************************* Test lists *******************************/
def testSelectChannelIntList(var: List[Int]): List[Int] =
  let ch: Channel[List[Int]] = channel List[Int] 1;
  ch <- var;;
  select {
      case x <- ch => x
  }

def testSelectChannelStrList(var: List[Str]): List[Str] =
  let ch: Channel[List[Str]] = channel List[Str] 1;
  ch <- var;;
  select {
      case x <- ch => x
  }

@test
def testSelectChannel19(): Bool = testSelectChannelIntList(Nil) `assertEq!` Nil

@test
def testSelectChannel20(): Bool = testSelectChannelIntList(1 :: Nil) `assertEq!` 1 :: Nil

@test
def testSelectChannel21(): Bool = testSelectChannelStrList("str" :: Nil) `assertEq!` "str" :: Nil

@test
def testSelectChannel22(): Bool = testSelectChannelStrList("str" :: "trs" :: Nil) `assertEq!` "str" :: "trs" :: Nil

/******************************* Test functions *******************************/
def testSelectChannelIntIntFunction(var: Int -> Int): Int -> Int =
  let ch: Channel[Int -> Int] = channel Int -> Int 1;
  ch <- var;;
  select {
      case x <- ch => x
  }

def testSelectChannelIntStrFunction(var: Int -> Str): Int -> Str =
  let ch: Channel[Int -> Str] = channel Int -> Str 1;
  ch <- var;;
  select {
      case x <- ch => x
  }

def testSelectChannelIntIntIntFunction(var: Int -> Int -> Int): Int -> Int -> Int =
  let ch: Channel[Int -> Int -> Int] = channel Int -> Int -> Int 1;
  ch <- var;;
  select {
      case x <- ch => x
  }

def testSelectChannelTupleIntIntIntFunction(var: (Int, Int) -> Int): (Int, Int) -> Int =
  let ch: Channel[(Int, Int) -> Int] = channel (Int, Int) -> Int 1;
  ch <- var;;
  select {
      case x <- ch => x
  }

@test
def testSelectChannel23(): Bool = testSelectChannelIntIntFunction(x -> x)(2) `assertEq!` 2

@test
def testSelectChannel24(): Bool = testSelectChannelIntStrFunction(x -> "2")(2) `assertEq!` "2"

//@test
//def testSelectChannel25(): Bool = testSelectChannelIntIntIntFunction((x -> y) -> x + y)(2, 3) `assertEq!` 5

//@test
//def testSelectChannel26(): Bool = testSelectChannelTupleIntIntIntFunction((x -> y) -> x + y)((2, 3)) `assertEq!` 5

/******************************* Default *******************************/
//def testSelectChannelDefault(): Int =
//  let ch: Channel[Unit] = channel Unit;
//  ch <- ();;
//  select {
//    case _ <- ch => 2
//  }

//@test
//def testSelectChannel29(): Bool = testSelectChannelDefault() `assertEq!` 2

/******************************* Multiple channel types *******************************/
def testSelectChannelMultipleTypes(): Int =
  let ch1: Channel[Int] = channel Int;
  let ch2: Channel[Str] = channel Str;
  ch2 <- "";;
  select {
    case x <- ch1 => 1
    case x <- ch2 => 2
  }

@test
def testSelectChannel30(): Bool = testSelectChannelMultipleTypes() `assertEq!` 2

@test
def select_int_channel_first(): Bool =
  let ch1 = channel Int;
  let ch2 = channel Int;
  let r = select {
    case x <- ch1 => x + 1
    case x <- ch2 => x + 2
  };
  ch1 <- 1;;
  r `assertEq!` 2

@test
def select_int_channel_last(): Bool =
  let ch1 = channel Int;
  let ch2 = channel Int;
  let r = select {
    case x <- ch1 => x + 1
    case x <- ch2 => x + 2
  };
  ch2 <- 1;;
  r `assertEq!` 3

@test
def select_string_channel_first(): Bool =
  let ch1 = channel Str;
  let ch2 = channel Str;
  let r = select {
    case x <- ch1 => x + "1"
    case x <- ch2 => x + "2"
  };
  ch1 <- "str";;
  r `assertEq!` "str1"

@test
def select_string_channel_last(): Bool =
  let ch1 = channel Str;
  let ch2 = channel Str;
  let r = select {
    case x <- ch1 => x + "1"
    case x <- ch2 => x + "2"
  };
  ch2 <- "str";;
  r `assertEq!` "str2"
