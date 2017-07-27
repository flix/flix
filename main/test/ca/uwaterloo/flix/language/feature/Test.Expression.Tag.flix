/////////////////////////////////////////////////////////////////////////////
// Expression.{CheckTag,GetTagValue}                                       //
// Tested indirectly by pattern matching.                                  //
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// Expression.Tag                                                          //
/////////////////////////////////////////////////////////////////////////////

enum ConstProp01 { case Top01, case Val01(Int), case Bot01 }

def f01(): ConstProp01 = ConstProp01.Top01

@test
def tagTest01(): Bool = assertEq!(f01(), ConstProp1.Top01)

def f02(): ConstProp01 = ConstProp01.Val01(42)

@test
def tagTest02(): Bool = assertEq!(f02(), ConstProp01.Val01(42))

def f03(): ConstProp01 = ConstProp01.Bot01

@test
def tagTest03(): Bool = assertEq!(f03(), ConstProp01.Bot01)

enum Val02 { case Val02(Bool) }

def f04(): Val02 = Val02.Val02(true)

@test
def tagTest04(): Bool = assertEq!(f04(), Val02(true))

def f05(x: Bool): Val02 = Val02.Val02(x)

@test
def tagTest05(): Bool = assertEq!(f05(true), Val02(true))

@test
def tagTest06(): Bool = assertEq!(f05(false), Val02(false))

enum Val03 { case Val03(Str) }

@test
def tagTest07(): Val03 = Val03.Val03("hi")

enum Val04 { case Val04(Int, Str) }

@test
def tagTest08(): Val04 = Val04.Val04(1, "one")

@test
def tagTest09(): Bool = assertEq!(Val03.Val03(if (!(4 != 4)) "foo" else "bar"), Val03("foo"))

@test
def tagTest10(): Bool = assertEq!(Val04.Val04(20 + 22, "ABC"), Val04(42, "ABC"))

enum Val05 { case Val05((Int, Str)) }

@test
def tagTest11(): Bool = assertEq!(Val05.Val05((20 + 22, "ABC")), Val05((42, "ABC")))

enum Val06 { case Val06(Int8) }

@test
def tagTest12(): Val06 = Val06.Val06(32i8)

enum Val07 { case Val07(Int16) }

@test
def tagTest13(): Val07 = Val07.Val07(3200i16)

enum Val08 { case Val08(Int32) }

@test
def tagTest14(): Val08 = Val08.Val08(32000000i32)

enum Val09 { case Val09(Int64) }

@test
def tagTest15(): Val09 = Val09.Val09(320000000000i64)

enum Val10 { case Val10(Char) }

@test
def tagTest16(): Val10 = Val10.Val10('a')

enum Val11 { case Val11(Float32) }

@test
def tagTest17(): Val11 = Val11.Val11(4.2f32)

enum Val12 { case Val12(Float64) }

@test
def tagTest18(): Val12 = Val12.Val12(4.2f64)

enum A13 { case AA13(Int) }
enum B13 { case BB13(A13) }

@test
def tagTest19(): B13 = B13.BB13(A13.AA13(42))

enum Val14 { case Val14(Set[Int]) }

@test
def tagTest20(): Val14 = Val14.Val14(#{1, 2, 3})

enum Val15 { case Val15(BigInt) }

@test
def tagTest21(): Val15 = Val15.Val15(12345678901234567890ii)











