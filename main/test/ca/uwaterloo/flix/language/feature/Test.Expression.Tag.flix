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
def testTag01(): Bool = assertEq!(f01(), ConstProp1.Top01)

def f02(): ConstProp01 = ConstProp01.Val01(42)

@test
def testTag02(): Bool = assertEq!(f02(), ConstProp01.Val01(42))

def f03(): ConstProp01 = ConstProp01.Bot01

@test
def testTag03(): Bool = assertEq!(f03(), ConstProp01.Bot01)

enum Val02 { case Val02(Bool) }

def f04(): Val02 = Val02.Val02(true)

@test
def testTag04(): Bool = assertEq!(f04(), Val02(true))

def f05(x: Bool): Val02 = Val02.Val02(x)

@test
def testTag05(): Bool = assertEq!(f05(true), Val02(true))

@test
def testTag06(): Bool = assertEq!(f05(false), Val02(false))

enum Val03 { case Val03(Str) }

@test
def testTag07(): Val03 = Val03.Val03("hi")

enum Val04 { case Val04(Int, Str) }

@test
def testTag08(): Val04 = Val04.Val04(1, "one")

@test
def testTag09(): Bool = assertEq!(Val03.Val03(if (!(4 != 4)) "foo" else "bar"), Val03("foo"))

@test
def testTag10(): Bool = assertEq!(Val04.Val04(20 + 22, "ABC"), Val04(42, "ABC"))

enum Val05 { case Val05((Int, Str)) }

@test
def testTag11(): Bool = assertEq!(Val05.Val05((20 + 22, "ABC")), Val05((42, "ABC")))

enum Val06 { case Val06(Int8) }

@test
def testTag12(): Val06 = Val06.Val06(32i8)

enum Val07 { case Val07(Int16) }

@test
def testTag13(): Val07 = Val07.Val07(3200i16)

enum Val08 { case Val08(Int32) }

@test
def testTag14(): Val08 = Val08.Val08(32000000i32)

enum Val09 { case Val09(Int64) }

@test
def testTag15(): Val09 = Val09.Val09(320000000000i64)

enum Val10 { case Val10(Char) }

@test
def testTag16(): Val10 = Val10.Val10('a')

enum Val11 { case Val11(Float32) }

@test
def testTag17(): Val11 = Val11.Val11(4.2f32)

enum Val12 { case Val12(Float64) }

@test
def testTag18(): Val12 = Val12.Val12(4.2f64)

enum A13 { case AA13(Int) }
enum B13 { case BB13(A13) }

@test
def testTag19(): B13 = B13.BB13(A13.AA13(42))

enum Val14 { case Val14(Set[Int]) }

@test
def testTag20(): Val14 = Val14.Val14(Set#{1, 2, 3})

enum Val15 { case Val15(BigInt) }

@test
def testTag21(): Val15 = Val15.Val15(12345678901234567890ii)











