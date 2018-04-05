/////////////////////////////////////////////////////////////////////////////
// Select expressions (channel select)                                     //
/////////////////////////////////////////////////////////////////////////////

def testSelectChannel[a](var: a): a =
    let ch = 2; //: Channel[a] = channel a:1;
    //ch <- var;
    select {
        case x <- ch => x
    }

/******************************* Test units *******************************/
@test
def testSelectChannel01(): Bool = assertEq!(testSelectChannel(()), ())

/******************************* Test bools *******************************/
@test
def testSelectChannel02(): Bool = assertEq!(testSelectChannel(true), true)

@test
def testSelectChannel03(): Bool = assertEq!(testSelectChannel(false), false)

/******************************* Test integers *******************************/
@test
def testSelectChannel04(): Bool = assertEq!(testSelectChannel(0), 0)

@test
def testSelectChannel05(): Bool = assertEq!(testSelectChannel(-1), -1)

@test
def testSelectChannel06(): Bool = assertEq!(testSelectChannel(1), 1)

@test
def testSelectChannel07(): Bool = assertEq!(testSelectChannel(-100), -100)

@test
def testSelectChannel08(): Bool = assertEq!(testSelectChannel(100), 100)

/******************************* Test floats *******************************/
// @test
// def testSelectChannel09(): Bool = assertEq!(testSelectChannel[Float32](0f32), 0f32)
// 
// @test
// def testSelectChannel10(): Bool = assertEq!(testSelectChannel[Float32](-1.1f32), -1.1f32)
// 
// @test
// def testSelectChannel11(): Bool = assertEq!(testSelectChannel[Float32](1.1f32), 1.1f32)

/******************************* Test strings *******************************/
@test
def testSelectChannel12(): Bool = assertEq!(testSelectChannel(""), "")

@test
def testSelectChannel13(): Bool = assertEq!(testSelectChannel("str"), "str")

/******************************* Test chars *******************************/
@test
def testSelectChannel14(): Bool = assertEq!(testSelectChannel('a'), 'a')

@test
def testSelectChannel15(): Bool = assertEq!(testSelectChannel('b'), 'b')

/******************************* Test tuples *******************************/
@test
def testSelectChannel16(): Bool = assertEq!(testSelectChannel((2, "Str")), (2, "Str"))

@test
def testSelectChannel17(): Bool = assertEq!(testSelectChannel((2, 10)), (2, 10))

@test
def testSelectChannel18(): Bool = assertEq!(testSelectChannel((2, 10, 12, 0)), (2, 10, 12, 0))

/******************************* Test lists *******************************/
@test
def testSelectChannel19(): Bool = assertEq!(testSelectChannel(Nil), Nil)

@test
def testSelectChannel20(): Bool = assertEq!(testSelectChannel(1 :: Nil), 1 :: Nil)

@test
def testSelectChannel21(): Bool = assertEq!(testSelectChannel("str" :: Nil), "str" :: Nil)

@test
def testSelectChannel22(): Bool = assertEq!(testSelectChannel("str" :: "trs" :: Nil), "str" :: "trs" :: Nil)

/******************************* Test functions *******************************/
@test
def testSelectChannel23(): Bool = assertEq!(testSelectChannel(x -> x)(2), 2)

@test
def testSelectChannel24(): Bool = assertEq!(testSelectChannel(x -> "2")(2), "2")

@test
def testSelectChannel25(): Bool = assertEq!(testSelectChannel((x, y) -> x + y)(2, 3), 5)

@test
def testSelectChannel26(): Bool = assertEq!(testSelectChannel(x -> y -> x + y)(2, 3), 5)
