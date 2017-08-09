/////////////////////////////////////////////////////////////////////////////
// Lambdas - Expression.{MkClosureRef,ApplyRef,ApplyClosure}               //
// Note that closure conversion and lambda lifting means we don't actually //
// have lambdas in the AST. A lot of functionality is tested indirectly    //
// by pattern matching.                                                    //
/////////////////////////////////////////////////////////////////////////////

// TODO: More tests when the typer handles lambda expressions.
// Test actual lambda expressions (not just top-level definitions): passing them around, free variables, etc.
// Note: this also applies to comparison (?) of lambdas and using lambdas in let-expressions.

namespace A01/B01 {
    def f(): Bool = false
}
namespace A01 {
    def g(): Bool = A01/B01.f()
}

@test
def testLambda01(): Bool = assertEq!(A01.g(), false)

namespace A02 { def f(x: Int): Int = 24 }

@test
def testLambda02(): Bool = assertEq!(A02.f(3), 24)

namespace A03 { def f(x: Int): Int = x }
namespace A03 { def g(): Int = f(3) }

@test
def testLambda03(): Bool = assertEq!(A03.g(), 3)

def f04(x: Int64, y: Int64): Int64 = x * y - 6i64

@test
def testLambda04(): Bool = assertEq!(f04(3i64, 42i64), 120i64)

namespace A05 { def f(x: Int32): Int32 = let y = B05.g(x + 1i32); y * y }
namespace B05 { def g(x: Int32): Int32 = x - 4i32 }
namespace C05 { def h(): Int32 = A05.f(5i32) + B05.g(0i32) }

@test
def testLambda05(): Bool = assertEq!(C05.h(), 0)

def f06(x: Int16): Int16 = g06(x + 1i16)
def g06(x: Int16): Int16 = h06(x + 10i16)
def h06(x: Int16): Int16 = x * x

@test
def testLambda06(): Bool = assertEq!(f06(3i16), 196i16)

def f07(x: Int8, y: Int8): Int8 = (x: Int8) - y
def g07(x: Int8): Int8 = x * 3i8
def h07(x: Int8): Int8 = g07(x - 1i8)
def x07(): Int8 = let x = 7i8; f07(g07(3i8), h07(h07(x)))

@test
def testLambda07(): Bool = assertEq!(x07(), -42i8)

def f08(x: Bool, y: Bool): Bool = if (x) true else y

@test
def testLambda08(): Bool = assertEq!(f08(true, true), true)

@test
def testLambda09(): Bool = assertEq!(f08(true, false), true)

@test
def testLambda10(): Bool = assertEq!(f08(false, false), false)

@test
def testLambda11(): Bool = assertEq!(f08(false, true), true)

def f09(x: Bool, y: Bool): Bool = if (x) y else false

@test
def testLambda12(): Bool = assertEq!(f09(true, true), true)

@test
def testLambda13(): Bool = assertEq!(f09(true, false), false)

@test
def testLambda14(): Bool = assertEq!(f09(false, false), false)

@test
def testLambda15(): Bool = assertEq!(f09(false, true), false)

def f10(x: Int, y: Int, z: Int): Int = x + y + z

@test
def testLambda16(): Bool = assertEq!(f10(2, 42, 5), 49)

def f11(x: (Int) -> Int, y: Int): Int = x(y)
def g11(x: Int): Int = x + 1

@test
def testLambda17(): Bool = assertEq!(f11(g11, 5), 6)

def f12(x: (Int) -> Int): (Int) -> Int = x
def g12(x: Int): Int = x + 5

@test
def testLambda18(): Bool = assertEq!((f12(g12))(40), 45)

enum Val13 { case Val13(Int) }
def f13(x: Int): Val13 = Val13.Val13(x)

@test
def testLambda19(): Bool = assertEq!(f13(111), Val13(111))

def f14(a: Int, b: Int, c: Str, d: Int, e: Bool, f: ()): (Int, Int, Str, Int, Bool, ()) = (a, b, c, d, e, f)

@test
def testLambda20(): Bool = assertEq!(f14(24, 53, "qwertyuiop", 9978, false, ()), (24, 53, "qwertyuiop", 9978, false, ()))

def f15(a: Int, b: Int, c: Int): Set[Int] = #{a, b, c}

@test
def testLambda21(): Bool = assertEq!(f15(24, 53, 24), #{24, 53, 24})

def f16(a: Char, b: Char): Bool = a == b

@test
def testLambda22(): Bool = assertEq!(f16('a', 'b'), false)

def f17(a: Float32, b: Float32): Float32 = a + b

@test
def testLambda23(): Bool = assertEq!(f17(1.2f32, 2.1f32), 3.3f32)

def f18(a: Float64, b: Float64): Float64 = a + b

@test
def testLambda24(): Bool = assertEq!(f18(1.2f64, 2.1f64), 3.3f64)

def f19(a: BigInt, b: BigInt): BigInt = a + b

@test
def testLambda25(): Bool = assertEq!(f19(1ii, 9223372036854775808ii), 9223372036854775809ii)
