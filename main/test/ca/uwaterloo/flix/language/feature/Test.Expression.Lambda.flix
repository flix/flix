/////////////////////////////////////////////////////////////////////////////
// Lambdas - Expression.{MkClosureRef,ApplyRef,ApplyClosure}               //
// Note that closure conversion and lambda lifting means we don't actually //
// have lambdas in the AST. A lot of functionality is tested indirectly    //
// by pattern matching.                                                    //
/////////////////////////////////////////////////////////////////////////////

// TODO: More tests when the typer handles lambda expressions.
// Test actual lambda expressions (not just top-level definitions): passing them around, free variables, etc.
// Note: this also applies to comparison (?) of lambdas and using lambdas in let-expressions.

namespace A_01/B_01 {
    def f(): Bool = false
}
namespace A_01 {
    def g(): Bool = A_01/B_01.f()
}

@test
def lambdaTest_01(): Bool = assertEq!(A_01.g(), false)

namespace A_02 { def f(x: Int): Int = 24 }

@test
def lambdaTest_02(): Bool = assertEq!(A_02.f(3), 24)

namespace A_03 { def f(x: Int): Int = x }
namespace A_03 { def g(): Int = f(3) }

@test
def lambdaTest_03(): Bool = assertEq!(A_03.g(), 3)

def f_04(x: Int64, y: Int64): Int64 = x * y - 6i64

@test
def lambdaTest_04(): Bool = assertEq!(f_04(3i64, 42i64), 120i64)

namespace A_05 { def f(x: Int32): Int32 = let y = B_05.g(x + 1i32); y * y }
namespace B_05 { def g(x: Int32): Int32 = x - 4i32 }
namespace C_05 { def h(): Int32 = A_05.f(5i32) + B_05.g(0i32) }

@test
def lambdaTest_05(): Bool = assertEq!(C_05.h(), 0)

def f_06(x: Int16): Int16 = g_06(x + 1i16)
def g_06(x: Int16): Int16 = h_06(x + 10i16)
def h_06(x: Int16): Int16 = x * x

@test
def lambdaTest_06(): Bool = assertEq!(f_06(3i16), 196i16)

def f_07(x: Int8, y: Int8): Int8 = (x: Int8) - y
def g_07(x: Int8): Int8 = x * 3i8
def h_07(x: Int8): Int8 = g_07(x - 1i8)
def x_07(): Int8 = let x = 7i8; f_07(g_07(3i8), h_07(h_07(x)))

@test
def lambdaTest_07(): Bool = assertEq!(x_07(), -42i8)

def f_08(x: Bool, y: Bool): Bool = if (x) true else y

@test
def lambdaTest_08(): Bool = assertEq!(f_08(true, true), true)

@test
def lambdaTest_09(): Bool = assertEq!(f_08(true, false), true)

@test
def lambdaTest_10(): Bool = assertEq!(f_08(false, false), false)

@test
def lambdaTest_11(): Bool = assertEq!(f_08(false, true), true)

def f_09(x: Bool, y: Bool): Bool = if (x) y else false

@test
def lambdaTest_12(): Bool = assertEq!(f_09(true, true), true)

@test
def lambdaTest_13(): Bool = assertEq!(f_09(true, false), false)

@test
def lambdaTest_14(): Bool = assertEq!(f_09(false, false), false)

@test
def lambdaTest_15(): Bool = assertEq!(f_09(false, true), false)

def f_10(x: Int, y: Int, z: Int): Int = x + y + z

@test
def lambdaTest_16(): Bool = assertEq!(f_10(2, 42, 5), 49)

def f_11(x: (Int) -> Int, y: Int): Int = x(y)
def g_11(x: Int): Int = x + 1

@test
def lambdaTest_17(): Bool = assertEq!(f_11(g_11, 5), 6)

def f_12(x: (Int) -> Int): (Int) -> Int = x
def g_12(x: Int): Int = x + 5

@test
def lambdaTest_18(): Bool = assertEq!((f_12(g_12))(40), 45)

enum Val_13 { case Val_13(Int) }
def f_13(x: Int): Val_13 = Val_13.Val_13(x)

@test
def lambdaTest_19(): Bool = assertEq!(f_13(111), Val_13(111))

def f_14(a: Int, b: Int, c: Str, d: Int, e: Bool, f: ()): (Int, Int, Str, Int, Bool, ()) = (a, b, c, d, e, f)

@test
def lambdaTest_20(): Bool = assertEq!(f_14(24, 53, "qwertyuiop", 9978, false, ()), (24, 53, "qwertyuiop", 9978, false, ()))

def f_15(a: Int, b: Int, c: Int): Set[Int] = #{a, b, c}

@test
def lambdaTest_21(): Bool = assertEq!(f_15(24, 53, 24), #{24, 53, 24})

def f_16(a: Char, b: Char): Bool = a == b

@test
def lambdaTest_22(): Bool = assertEq!(f_16('a', 'b'), false)

def f_17(a: Float32, b: Float32): Float32 = a + b

@test
def lambdaTest_23(): Bool = assertEq!(f_17(1.2f32, 2.1f32), 3.3f32)

def f_18(a: Float64, b: Float64): Float64 = a + b

@test
def lambdaTest_24(): Bool = assertEq!(f_18(1.2f64, 2.1f64), 3.3f64)

def f_19(a: BigInt, b: BigInt): BigInt = a + b

@test
def lambdaTest_25(): Bool = assertEq!(f_19(1ii, 9223372036854775808ii), 9223372036854775809ii)
