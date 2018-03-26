/////////////////////////////////////////////////////////////////////////////
// Select expressions (channel select)                                     //
/////////////////////////////////////////////////////////////////////////////



// Test integers
def testSelectChannel01(): Int = 
    //let ch: Channel[Int] = channel Int;
    //ch <: 3;
    select {
        case x <- ch => 0
    }

@test
def testSelectWildcard01(): Bool = assertEq!(testSelectChannel01(), 0)

def testSelectChannel02(): Int = select {
    case x <- ch => -1
}

@test
def testSelectWildcard02(): Bool = assertEq!(testSelectChannel02(), -1)

def testSelectChannel03(): Int = select {
    case x <- ch => 1
}

@test
def testSelectWildcard03(): Bool = assertEq!(testSelectChannel03(), 1)

def testSelectChannel04(): Int = select {
    case x <- ch => -100
}

@test
def testSelectWildcard04(): Bool = assertEq!(testSelectChannel04(), -100)

def testSelectChannel05(): Int = select {
    case x <- ch => 100
}

@test
def testSelectWildcard05(): Bool = assertEq!(testSelectChannel05(), 100)

// Test floats
// def testSelectChannel06(): Float32 = select {
//     case x <- ch => 0.0f32
// }
// 
// @test
// def testSelectWildcard06(): Bool = assertEq!(testSelectChannel06(), 0f32)
// 
// def testSelectChannel07(): Float32 = select {
//     case x <- ch => -1.1f32
// }
// 
// @test
// def testSelectWildcard07(): Bool = assertEq!(testSelectChannel07(), -1.1f32)
// 
// def testSelectChannel08(): Float32 = select {
//     case x <- ch => 1.1f32
// }
// 
// @test
// def testSelectWildcard08(): Bool = assertEq!(testSelectChannel08(), 1.1f32)

// Test strings
def testSelectChannel09(): Str = select {
    case x <- ch => ""
}

@test
def testSelectWildcard09(): Bool = assertEq!(testSelectChannel09(), "")

def testSelectChannel10(): Str = select {
    case x <- ch => "str"
}

@test
def testSelectWildcard10(): Bool = assertEq!(testSelectChannel10(), "str")

def testSelectChannel11(): Str = select {
    case x <- ch => "strstrstrstrstrstrstrstrstr"
}

@test
def testSelectWildcard11(): Bool = assertEq!(testSelectChannel11(), "strstrstrstrstrstrstrstrstr")
