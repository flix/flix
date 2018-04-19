/////////////////////////////////////////////////////////////////////////////
// Put channel expressions                                                 //
/////////////////////////////////////////////////////////////////////////////

def putChannelInt(ch1: Channel[Int], x: Int): Channel[Int] =
    let _ = ch1 <- x;
    ch1

def testPutChannelInt(): Bool =
    let ch = channel Int;
    let x = putChannelInt(ch, 2);
    assertEq!(<- x, 2)
