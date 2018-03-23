/////////////////////////////////////////////////////////////////////////////
// Select expressions (channel select)                                     //
/////////////////////////////////////////////////////////////////////////////

def main(): Int = 42

// Test integers
def fi01(): Int = select {
    case x = <- ch => 0
}

@test
def testSelectWildcardi01(): Bool = assertEq!(fi01(), 0)

def fi02(): Int = select {
    case x = <- ch => -1
}

@test
def testSelectWildcardi02(): Bool = assertEq!(fi02(), -1)

def fi03(): Int = select {
    case x = <- ch => 1
}

@test
def testSelectWildcardi03(): Bool = assertEq!(fi03(), 1)

def fi04(): Int = select {
    case x = <- ch => -100
}

@test
def testSelectWildcardi04(): Bool = assertEq!(fi04(), -100)

def fi05(): Int = select {
    case x = <- ch => 100
}

@test
def testSelectWildcardi05(): Bool = assertEq!(fi05(), 100)

// Test floats
// def ff01(): Float32 = select {
//     case x = <- ch => 0.0f32
// }
// 
// @test
// def testSelectWildcardf01(): Bool = assertEq!(ff01(), 0f32)
// 
// def ff02(): Float32 = select {
//     case x = <- ch => -1.1f32
// }
// 
// @test
// def testSelectWildcardf02(): Bool = assertEq!(ff02(), -1.1f32)
// 
// def ff03(): Float32 = select {
//     case x = <- ch => 1.1f32
// }
// 
// @test
// def testSelectWildcardf03(): Bool = assertEq!(ff03(), 1.1f32)

// Test strings
def fs01(): Str = select {
    case x = <- ch => ""
}

@test
def testSelectWildcards01(): Bool = assertEq!(fs01(), "")

def fs02(): Str = select {
    case x = <- ch => "str"
}

@test
def testSelectWildcards02(): Bool = assertEq!(fs02(), "str")

def fs03(): Str = select {
    case x = <- ch => "strstrstrstrstrstrstrstrstr"
}

@test
def testSelectWildcards03(): Bool = assertEq!(fs03(), "strstrstrstrstrstrstrstrstr")
