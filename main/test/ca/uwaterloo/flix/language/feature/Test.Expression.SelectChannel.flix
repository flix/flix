/////////////////////////////////////////////////////////////////////////////
// Select expressions (channel select)                                     //
/////////////////////////////////////////////////////////////////////////////

/******************************* Test units *******************************/
def testSelectChannelUnit(): Unit =
  let ch: Channel[Unit] = channel Unit:1;
  //ch <- var;
  select {
      case x <- ch => ()
  }

@test
def testSelectChannel01(): Bool = assertEq!(testSelectChannelUnit(), ())

/******************************* Test bools *******************************/
def testSelectChannelBool(var: Bool): Bool =
  let ch: Channel[Bool] = channel Bool:1;
  //ch <- var;
  select {
      case x <- ch => x
  }

@test
def testSelectChannel02(): Bool = assertEq!(testSelectChannelBool(true), true)

@test
def testSelectChannel03(): Bool = assertEq!(testSelectChannelBool(false), false)

/******************************* Test integers *******************************/
def testSelectChannelInt(var: Int): Int =
  let ch: Channel[Int] = channel Int:1;
  //ch <- var;
  select {
      case x <- ch => x
  }

@test
def testSelectChannel04(): Bool = assertEq!(testSelectChannelInt(0), 0)

@test
def testSelectChannel05(): Bool = assertEq!(testSelectChannelInt(-1), -1)

@test
def testSelectChannel06(): Bool = assertEq!(testSelectChannelInt(1), 1)

@test
def testSelectChannel07(): Bool = assertEq!(testSelectChannelInt(-100), -100)

@test
def testSelectChannel08(): Bool = assertEq!(testSelectChannelInt(100), 100)

/******************************* Test floats *******************************/
//def testSelectChannelFloat32(var: Float32): Float32 =
//  let ch: Channel[Float32] = channel Float32:1;
//  //ch <- var;
//  select {
//      case x <- ch => x
//  }

// @test
// def testSelectChannel09(): Bool = assertEq!(testSelectChannelFloat32(0f32), 0f32)
// 
// @test
// def testSelectChannel10(): Bool = assertEq!(testSelectChannelFloat32(-1.1f32), -1.1f32)
// 
// @test
// def testSelectChannel11(): Bool = assertEq!(testSelectChannelFloat32(1.1f32), 1.1f32)

/******************************* Test strings *******************************/
def testSelectChannelStr(var: Str): Str =
  let ch: Channel[Str] = channel Str:1;
  //ch <- var;
  select {
      case x <- ch => x
  }

@test
def testSelectChannel12(): Bool = assertEq!(testSelectChannelStr(""), "")

@test
def testSelectChannel13(): Bool = assertEq!(testSelectChannelStr("str"), "str")

/******************************* Test chars *******************************/
def testSelectChannelChar(var: Char): Char =
  let ch: Channel[Char] = channel Char:1;
  //ch <- var;
  select {
      case x <- ch => x
  }

@test
def testSelectChannel14(): Bool = assertEq!(testSelectChannelChar('a'), 'a')

@test
def testSelectChannel15(): Bool = assertEq!(testSelectChannelChar('b'), 'b')

/******************************* Test tuples *******************************/
def testSelectChannelIntStrTuple(var: (Int, Str)): (Int, Str) =
  let ch: Channel[(Int, Str)] = channel (Int, Str):1;
  //ch <- var;
  select {
      case x <- ch => x
  }

def testSelectChannelIntIntTuple(var: (Int, Int)): (Int, Int) =
  let ch: Channel[(Int, Int)] = channel (Int, Int):1;
  //ch <- var;
  select {
      case x <- ch => x
  }

def testSelectChannelIntIntIntIntTuple(var: (Int, Int, Int, Int)): (Int, Int, Int, Int) =
  let ch: Channel[(Int, Int, Int, Int)] = channel (Int, Int, Int, Int):1;
  //ch <- var;
  select {
      case x <- ch => x
  }

@test
def testSelectChannel16(): Bool = assertEq!(testSelectChannelIntStrTuple((2, "Str")), (2, "Str"))

@test
def testSelectChannel17(): Bool = assertEq!(testSelectChannelIntIntTuple((2, 10)), (2, 10))

@test
def testSelectChannel18(): Bool = assertEq!(testSelectChannelIntIntIntIntTuple((2, 10, 12, 0)), (2, 10, 12, 0))

/******************************* Test lists *******************************/
def testSelectChannelIntList(var: List[Int]): List[Int] =
  let ch: Channel[List[Int]] = channel List[Int]:1;
  //ch <- var;
  select {
      case x <- ch => x
  }

def testSelectChannelStrList(var: List[Str]): List[Str] =
  let ch: Channel[List[Str]] = channel List[Str]:1;
  //ch <- var;
  select {
      case x <- ch => x
  }

@test
def testSelectChannel19(): Bool = assertEq!(testSelectChannelIntList(Nil), Nil)

@test
def testSelectChannel20(): Bool = assertEq!(testSelectChannelIntList(1 :: Nil), 1 :: Nil)

@test
def testSelectChannel21(): Bool = assertEq!(testSelectChannelStrList("str" :: Nil), "str" :: Nil)

@test
def testSelectChannel22(): Bool = assertEq!(testSelectChannelStrList("str" :: "trs" :: Nil), "str" :: "trs" :: Nil)

/******************************* Test functions *******************************/
def testSelectChannelIntIntFunction(var: Int -> Int): Int -> Int =
  let ch: Channel[Int -> Int] = channel Int -> Int:1;
  //ch <- var;
  select {
      case x <- ch => x
  }

def testSelectChannelIntStrFunction(var: Int -> Str): Int -> Str =
  let ch: Channel[Int -> Str] = channel Int -> Str:1;
  //ch <- var;
  select {
      case x <- ch => x
  }

def testSelectChannelIntIntIntFunction(var: Int -> Int -> Int): Int -> Int -> Int =
  let ch: Channel[Int -> Int -> Int] = channel Int -> Int -> Int:1;
  //ch <- var;
  select {
      case x <- ch => x
  }

@test
def testSelectChannel23(): Bool = assertEq!(testSelectChannelIntIntFunction(x -> x)(2), 2)

@test
def testSelectChannel24(): Bool = assertEq!(testSelectChannelIntStrFunction(x -> "2")(2), "2")

@test
def testSelectChannel25(): Bool = assertEq!(testSelectChannelIntIntIntFunction((x, y) -> x + y)(2, 3), 5)

@test
def testSelectChannel26(): Bool = assertEq!(testSelectChannelIntIntIntFunction(x -> y -> x + y)(2, 3), 5)
