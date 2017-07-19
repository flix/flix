/////////////////////////////////////////////////////////////////////////////
// Expression.Binary (Comparison)                                          //
// BinaryOperator.{Less,LessEqual,Greater,GreaterEqual}                    //
// BinaryOperator.{Equal,NotEqual}                                         //
/////////////////////////////////////////////////////////////////////////////

@test
def binaryLessTest01(): Bool = assertEq!(120000 < 30000, false)

@test
def binaryLessTest02(): Bool = 30000 < 120000

@test
def binaryLessTest03(): Bool = assertEq!(30000 < 30000, false)

@test
def binaryLessTest04(): Bool = -120000 < -30000

@test
def binaryLessTest05(): Bool = assertEq!(-30000 < -120000, false)

@test
def binaryLessTest06(): Bool = assertEq!(-30000 < -30000, false)

@test
def binaryLessTest07(): Bool = assertEq!(12i8 < 3i8, false)

@test
def binaryLessTest08(): Bool = 3i8 < 12i8

@test
def binaryLessTest09(): Bool = assertEq!(3i8 < 3i8, false)

@test
def binaryLessTest10(): Bool = -12i8 < -3i8

@test
def binaryLessTest11(): Bool = assertEq!(-3i8 < -12i8, false)

@test
def binaryLessTest12(): Bool = assertEq!(-3i8 < -3i8, false)

@test
def binaryLessTest13(): Bool = assertEq!(1200i16 < 300i16, false)

@test
def binaryLessTest14(): Bool = 300i16 < 1200i16

@test
def binaryLessTest15(): Bool = assertEq!(300i16 < 300i16, false)

@test
def binaryLessTest16(): Bool = -1200i16 < -300i16

@test
def binaryLessTest17(): Bool = assertEq!(-300i16 < -1200i16, false)

@test
def binaryLessTest18(): Bool = assertEq!(-300i16 < -300i16, false)

@test
def binaryLessTest19(): Bool = assertEq!(120000i32 < 30000i32, false)

@test
def binaryLessTest20(): Bool = 30000i32 < 120000i32

@test
def binaryLessTest21(): Bool = assertEq!(30000i32 < 30000i32, false)

@test
def binaryLessTest22(): Bool = -120000i32 < -30000i32

@test
def binaryLessTest23(): Bool = assertEq!(-30000i32 < -120000i32, false)

@test
def binaryLessTest24(): Bool = assertEq!(-30000i32 < -30000i32, false)

@test
def binaryLessTest25(): Bool = assertEq!(12000000000i64 < 3000000000i64, false)

@test
def binaryLessTest26(): Bool = 3000000000i64 < 12000000000i64

@test
def binaryLessTest27(): Bool = assertEq!(3000000000i64 < 3000000000i64, false)

@test
def binaryLessTest28(): Bool = -12000000000i64 < -3000000000i64

@test
def binaryLessTest29(): Bool = assertEq!(-3000000000i64 < -12000000000i64, false)

@test
def binaryLessTest30(): Bool = assertEq!(-3000000000i64 < -3000000000i64, false)

@test
def binaryLessTest31(): Bool = assertEq!(120000000000000000000000000000000000000000.0 < 30000000000000000000000000000000000000000.0, false)

@test
def binaryLessTest32(): Bool = 30000000000000000000000000000000000000000.0 < 120000000000000000000000000000000000000000.0

@test
def binaryLessTest33(): Bool = assertEq!(30000000000000000000000000000000000000000.0 < 30000000000000000000000000000000000000000.0, false)

@test
def binaryLessTest34(): Bool = -120000000000000000000000000000000000000000.0 < -30000000000000000000000000000000000000000.0

@test
def binaryLessTest35(): Bool = assertEq!(-30000000000000000000000000000000000000000.0 < -120000000000000000000000000000000000000000.0, false)

@test
def binaryLessTest36(): Bool = assertEq!(-30000000000000000000000000000000000000000.0 < -30000000000000000000000000000000000000000.0, false)

@test
def binaryLessTest37(): Bool = assertEq!(1200000000000000000000.0f32 < 300000000000000000000.0f32, false)

@test
def binaryLessTest38(): Bool = 300000000000000000000.0f32 < 1200000000000000000000.0f32

@test
def binaryLessTest39(): Bool = assertEq!(300000000000000000000.0f32 < 300000000000000000000.0f32, false)

@test
def binaryLessTest40(): Bool = -1200000000000000000000.0f32 < -300000000000000000000.0f32

@test
def binaryLessTest41(): Bool = assertEq!(-300000000000000000000.0f32 < -1200000000000000000000.0f32, false)

@test
def binaryLessTest42(): Bool = assertEq!(-300000000000000000000.0f32 < -300000000000000000000.0f32, false)

@test
def binaryLessTest43(): Bool = assertEq!(120000000000000000000000000000000000000000.0f64 < 30000000000000000000000000000000000000000.0f64, false)

@test
def binaryLessTest44(): Bool = 30000000000000000000000000000000000000000.0f64 < 120000000000000000000000000000000000000000.0f64

@test
def binaryLessTest45(): Bool = assertEq!(30000000000000000000000000000000000000000.0f64 < 30000000000000000000000000000000000000000.0f64, false)

@test
def binaryLessTest46(): Bool = -120000000000000000000000000000000000000000.0f64 < -30000000000000000000000000000000000000000.0f64

@test
def binaryLessTest47(): Bool = assertEq!(-30000000000000000000000000000000000000000.0f64 < -120000000000000000000000000000000000000000.0f64, false)

@test
def binaryLessTest48(): Bool = assertEq!(-30000000000000000000000000000000000000000.0f64 < -30000000000000000000000000000000000000000.0f64, false)

@test
def binaryLessTest49(): Bool = assertEq!(12000000000000000000ii < 3000000000000000000ii, false)

@test
def binaryLessTest50(): Bool = 3000000000000000000ii < 12000000000000000000ii

@test
def binaryLessTest51(): Bool = assertEq!(3000000000000000000ii < 3000000000000000000ii, false)

@test
def binaryLessTest52(): Bool = -12000000000000000000ii < -3000000000000000000ii

@test
def binaryLessTest53(): Bool = assertEq!(-3000000000000000000ii < -12000000000000000000ii, false)

@test
def binaryLessTest54(): Bool = assertEq!(-3000000000000000000ii < -3000000000000000000ii, false)

@test
def binaryLessTest55(): Bool = assertEq!('十' < '\u0000', false)

@test
def binaryLessTest56(): Bool = '\u0000' < '十'

@test
def binaryLessTest57(): Bool = assertEq!('\u0000' < '\u0000', false)

@test
def binaryLessEqualTest01(): Bool = assertEq!(120000 <= 30000, false)

@test
def binaryLessEqualTest02(): Bool = 30000 <= 120000

@test
def binaryLessEqualTest03(): Bool = 30000 <= 30000

@test
def binaryLessEqualTest04(): Bool = -120000 <= -30000

@test
def binaryLessEqualTest05(): Bool = assertEq!(-30000 <= -120000, false)

@test
def binaryLessEqualTest06(): Bool = -30000 <= -30000

@test
def binaryLessEqualTest07(): Bool = assertEq!(12i8 <= 3i8, false)

@test
def binaryLessEqualTest08(): Bool = 3i8 <= 12i8

@test
def binaryLessEqualTest09(): Bool = 3i8 <= 3i8

@test
def binaryLessEqualTest10(): Bool = -12i8 <= -3i8

@test
def binaryLessEqualTest11(): Bool = assertEq!(-3i8 <= -12i8, false)

@test
def binaryLessEqualTest12(): Bool = -3i8 <= -3i8

@test
def binaryLessEqualTest13(): Bool = assertEq!(1200i16 <= 300i16, false)

@test
def binaryLessEqualTest14(): Bool = 300i16 <= 1200i16

@test
def binaryLessEqualTest15(): Bool = 300i16 <= 300i16

@test
def binaryLessEqualTest16(): Bool = -1200i16 <= -300i16

@test
def binaryLessEqualTest17(): Bool = assertEq!(-300i16 <= -1200i16, false)

@test
def binaryLessEqualTest18(): Bool = -300i16 <= -300i16

@test
def binaryLessEqualTest19(): Bool = assertEq!(120000i32 <= 30000i32, false)

@test
def binaryLessEqualTest20(): Bool = 30000i32 <= 120000i32

@test
def binaryLessEqualTest21(): Bool = 30000i32 <= 30000i32

@test
def binaryLessEqualTest22(): Bool = -120000i32 <= -30000i32

@test
def binaryLessEqualTest23(): Bool = assertEq!(-30000i32 <= -120000i32, false)

@test
def binaryLessEqualTest24(): Bool = -30000i32 <= -30000i32

@test
def binaryLessEqualTest25(): Bool = assertEq!(12000000000i64 <= 3000000000i64, false)

@test
def binaryLessEqualTest26(): Bool = 3000000000i64 <= 12000000000i64

@test
def binaryLessEqualTest27(): Bool = 3000000000i64 <= 3000000000i64

@test
def binaryLessEqualTest28(): Bool = -12000000000i64 <= -3000000000i64

@test
def binaryLessEqualTest29(): Bool = assertEq!(-3000000000i64 <= -12000000000i64, false)

@test
def binaryLessEqualTest30(): Bool = -3000000000i64 <= -3000000000i64

@test
def binaryLessEqualTest31(): Bool = assertEq!(120000000000000000000000000000000000000000.0 <= 30000000000000000000000000000000000000000.0, false)

@test
def binaryLessEqualTest32(): Bool = 30000000000000000000000000000000000000000.0 <= 120000000000000000000000000000000000000000.0

@test
def binaryLessEqualTest33(): Bool = 30000000000000000000000000000000000000000.0 <= 30000000000000000000000000000000000000000.0

@test
def binaryLessEqualTest34(): Bool = -120000000000000000000000000000000000000000.0 <= -30000000000000000000000000000000000000000.0

@test
def binaryLessEqualTest35(): Bool = assertEq!(-30000000000000000000000000000000000000000.0 <= -120000000000000000000000000000000000000000.0, false)

@test
def binaryLessEqualTest36(): Bool = -30000000000000000000000000000000000000000.0 <= -30000000000000000000000000000000000000000.0

@test
def binaryLessEqualTest37(): Bool = assertEq!(1200000000000000000000.0f32 <= 300000000000000000000.0f32, false)

@test
def binaryLessEqualTest38(): Bool = 300000000000000000000.0f32 <= 1200000000000000000000.0f32

@test
def binaryLessEqualTest39(): Bool = 300000000000000000000.0f32 <= 300000000000000000000.0f32

@test
def binaryLessEqualTest40(): Bool = -1200000000000000000000.0f32 <= -300000000000000000000.0f32

@test
def binaryLessEqualTest41(): Bool = assertEq!(-300000000000000000000.0f32 <= -1200000000000000000000.0f32, false)

@test
def binaryLessEqualTest42(): Bool = -300000000000000000000.0f32 <= -300000000000000000000.0f32

@test
def binaryLessEqualTest43(): Bool = assertEq!(120000000000000000000000000000000000000000.0f64 <= 30000000000000000000000000000000000000000.0f64, false)

@test
def binaryLessEqualTest44(): Bool = 30000000000000000000000000000000000000000.0f64 <= 120000000000000000000000000000000000000000.0f64

@test
def binaryLessEqualTest45(): Bool = 30000000000000000000000000000000000000000.0f64 <= 30000000000000000000000000000000000000000.0f64

@test
def binaryLessEqualTest46(): Bool = -120000000000000000000000000000000000000000.0f64 <= -30000000000000000000000000000000000000000.0f64

@test
def binaryLessEqualTest47(): Bool = assertEq!(-30000000000000000000000000000000000000000.0f64 <= -120000000000000000000000000000000000000000.0f64, false)

@test
def binaryLessEqualTest48(): Bool = -30000000000000000000000000000000000000000.0f64 <= -30000000000000000000000000000000000000000.0f64

@test
def binaryLessEqualTest49(): Bool = assertEq!(12000000000000000000ii <= 3000000000000000000ii, false)

@test
def binaryLessEqualTest50(): Bool = 3000000000000000000ii <= 12000000000000000000ii

@test
def binaryLessEqualTest51(): Bool = 3000000000000000000ii <= 3000000000000000000ii

@test
def binaryLessEqualTest52(): Bool = -12000000000000000000ii <= -3000000000000000000ii

@test
def binaryLessEqualTest53(): Bool = assertEq!(-3000000000000000000ii <= -12000000000000000000ii, false)

@test
def binaryLessEqualTest54(): Bool = -3000000000000000000ii <= -3000000000000000000ii

@test
def binaryLessEqualTest55(): Bool = assertEq!('十' <= '\u0000', false)

@test
def binaryLessEqualTest56(): Bool = '\u0000' <= '十'

@test
def binaryLessEqualTest57(): Bool = '\u0000' <= '\u0000'

@test
def binaryGreaterTest01(): Bool = 120000 > 30000

@test
def binaryGreaterTest02(): Bool = assertEq!(30000 > 120000, false)

@test
def binaryGreaterTest03(): Bool = assertEq!(30000 > 30000, false)

@test
def binaryGreaterTest04(): Bool = assertEq!(-120000 > -30000, false)

@test
def binaryGreaterTest05(): Bool = -30000 > -120000

@test
def binaryGreaterTest06(): Bool = assertEq!(-30000 > -30000, false)

@test
def binaryGreaterTest07(): Bool = 12i8 > 3i8

@test
def binaryGreaterTest08(): Bool = assertEq!(3i8 > 12i8, false)

@test
def binaryGreaterTest09(): Bool = assertEq!(3i8 > 3i8, false)

@test
def binaryGreaterTest10(): Bool = assertEq!(-12i8 > -3i8, false)

@test
def binaryGreaterTest11(): Bool = -3i8 > -12i8

@test
def binaryGreaterTest12(): Bool = assertEq!(-3i8 > -3i8, false)

@test
def binaryGreaterTest13(): Bool = 1200i16 > 300i16

@test
def binaryGreaterTest14(): Bool = assertEq!(300i16 > 1200i16, false)

@test
def binaryGreaterTest15(): Bool = assertEq!(300i16 > 300i16, false)

@test
def binaryGreaterTest16(): Bool = assertEq!(-1200i16 > -300i16, false)

@test
def binaryGreaterTest17(): Bool = -300i16 > -1200i16

@test
def binaryGreaterTest18(): Bool = assertEq!(-300i16 > -300i16, false)

@test
def binaryGreaterTest19(): Bool = 120000i32 > 30000i32

@test
def binaryGreaterTest20(): Bool = assertEq!(30000i32 > 120000i32, false)

@test
def binaryGreaterTest21(): Bool = assertEq!(30000i32 > 30000i32, false)

@test
def binaryGreaterTest22(): Bool = assertEq!(-120000i32 > -30000i32, false)

@test
def binaryGreaterTest23(): Bool = -30000i32 > -120000i32

@test
def binaryGreaterTest24(): Bool = assertEq!(-30000i32 > -30000i32, false)

@test
def binaryGreaterTest25(): Bool = 12000000000i64 > 3000000000i64

@test
def binaryGreaterTest26(): Bool = assertEq!(3000000000i64 > 12000000000i64, false)

@test
def binaryGreaterTest27(): Bool = assertEq!(3000000000i64 > 3000000000i64, false)

@test
def binaryGreaterTest28(): Bool = assertEq!(-12000000000i64 > -3000000000i64, false)

@test
def binaryGreaterTest29(): Bool = -3000000000i64 > -12000000000i64

@test
def binaryGreaterTest30(): Bool = assertEq!(-3000000000i64 > -3000000000i64, false)

@test
def binaryGreaterTest31(): Bool = 120000000000000000000000000000000000000000.0 > 30000000000000000000000000000000000000000.0

@test
def binaryGreaterTest32(): Bool = assertEq!(30000000000000000000000000000000000000000.0 > 120000000000000000000000000000000000000000.0, false)

@test
def binaryGreaterTest33(): Bool = assertEq!(30000000000000000000000000000000000000000.0 > 30000000000000000000000000000000000000000.0, false)

@test
def binaryGreaterTest34(): Bool = assertEq!(-120000000000000000000000000000000000000000.0 > -30000000000000000000000000000000000000000.0, false)

@test
def binaryGreaterTest35(): Bool = -30000000000000000000000000000000000000000.0 > -120000000000000000000000000000000000000000.0

@test
def binaryGreaterTest36(): Bool = assertEq!(-30000000000000000000000000000000000000000.0 > -30000000000000000000000000000000000000000.0, false)

@test
def binaryGreaterTest37(): Bool = 1200000000000000000000.0f32 > 300000000000000000000.0f32

@test
def binaryGreaterTest38(): Bool = assertEq!(300000000000000000000.0f32 > 1200000000000000000000.0f32, false)

@test
def binaryGreaterTest39(): Bool = assertEq!(300000000000000000000.0f32 > 300000000000000000000.0f32, false)

@test
def binaryGreaterTest40(): Bool = assertEq!(-1200000000000000000000.0f32 > -300000000000000000000.0f32, false)

@test
def binaryGreaterTest41(): Bool = -300000000000000000000.0f32 > -1200000000000000000000.0f32

@test
def binaryGreaterTest42(): Bool = assertEq!(-300000000000000000000.0f32 > -300000000000000000000.0f32, false)

@test
def binaryGreaterTest43(): Bool = 120000000000000000000000000000000000000000.0f64 > 30000000000000000000000000000000000000000.0f64

@test
def binaryGreaterTest44(): Bool = assertEq!(30000000000000000000000000000000000000000.0f64 > 120000000000000000000000000000000000000000.0f64, false)

@test
def binaryGreaterTest45(): Bool = assertEq!(30000000000000000000000000000000000000000.0f64 > 30000000000000000000000000000000000000000.0f64, false)

@test
def binaryGreaterTest46(): Bool = assertEq!(-120000000000000000000000000000000000000000.0f64 > -30000000000000000000000000000000000000000.0f64, false)

@test
def binaryGreaterTest47(): Bool =-30000000000000000000000000000000000000000.0f64 > -120000000000000000000000000000000000000000.0f64

@test
def binaryGreaterTest48(): Bool =  assertEq!(-30000000000000000000000000000000000000000.0f64 > -30000000000000000000000000000000000000000.0f64, false)

@test
def binaryGreaterTest49(): Bool = 12000000000000000000ii > 3000000000000000000ii

@test
def binaryGreaterTest50(): Bool = assertEq!(3000000000000000000ii > 12000000000000000000ii, false)

@test
def binaryGreaterTest51(): Bool = assertEq!(3000000000000000000ii > 3000000000000000000ii, false)

@test
def binaryGreaterTest52(): Bool = assertEq!(-12000000000000000000ii > -3000000000000000000ii, false)

@test
def binaryGreaterTest53(): Bool = -3000000000000000000ii > -12000000000000000000ii

@test
def binaryGreaterTest54(): Bool = assertEq!(-3000000000000000000ii > -3000000000000000000ii, false)

@test
def binaryGreaterTest55(): Bool = '十' > '\u0000'

@test
def binaryGreaterTest56(): Bool = assertEq!('\u0000' > '十', false)

@test
def binaryGreaterTest57(): Bool = assertEq!('\u0000' > '\u0000', false)

@test
def binaryGreaterEqualTest01(): Bool = 120000 >= 30000

@test
def binaryGreaterEqualTest02(): Bool = assertEq!(30000 >= 120000, false)

@test
def binaryGreaterEqualTest03(): Bool = 30000 >= 30000

@test
def binaryGreaterEqualTest04(): Bool = assertEq!(-120000 >= -30000, false)

@test
def binaryGreaterEqualTest05(): Bool = -30000 >= -120000

@test
def binaryGreaterEqualTest06(): Bool = -30000 >= -30000

@test
def binaryGreaterEqualTest07(): Bool = 12i8 >= 3i8

@test
def binaryGreaterEqualTest08(): Bool = assertEq!(3i8 >= 12i8, false)

@test
def binaryGreaterEqualTest09(): Bool = 3i8 >= 3i8

@test
def binaryGreaterEqualTest10(): Bool = assertEq!(-12i8 >= -3i8, false)

@test
def binaryGreaterEqualTest11(): Bool = -3i8 >= -12i8

@test
def binaryGreaterEqualTest12(): Bool = -3i8 >= -3i8

@test
def binaryGreaterEqualTest13(): Bool = 1200i16 >= 300i16

@test
def binaryGreaterEqualTest14(): Bool = assertEq!(300i16 >= 1200i16, false)

@test
def binaryGreaterEqualTest15(): Bool = 300i16 >= 300i16

@test
def binaryGreaterEqualTest16(): Bool = assertEq!(-1200i16 >= -300i16, false)

@test
def binaryGreaterEqualTest17(): Bool = -300i16 >= -1200i16

@test
def binaryGreaterEqualTest18(): Bool = -300i16 >= -300i16

@test
def binaryGreaterEqualTest19(): Bool = 120000i32 >= 30000i32

@test
def binaryGreaterEqualTest20(): Bool = assertEq!(30000i32 >= 120000i32, false)

@test
def binaryGreaterEqualTest21(): Bool = 30000i32 >= 30000i32

@test
def binaryGreaterEqualTest22(): Bool = assertEq!(-120000i32 >= -30000i32, false)

@test
def binaryGreaterEqualTest23(): Bool = -30000i32 >= -120000i32

@test
def binaryGreaterEqualTest24(): Bool = -30000i32 >= -30000i32

@test
def binaryGreaterEqualTest25(): Bool = 12000000000i64 >= 3000000000i64

@test
def binaryGreaterEqualTest26(): Bool = assertEq!(3000000000i64 >= 12000000000i64, false)

@test
def binaryGreaterEqualTest27(): Bool = 3000000000i64 >= 3000000000i64

@test
def binaryGreaterEqualTest28(): Bool = assertEq!(-12000000000i64 >= -3000000000i64, false)

@test
def binaryGreaterEqualTest29(): Bool = -3000000000i64 >= -12000000000i64

@test
def binaryGreaterEqualTest30(): Bool = -3000000000i64 >= -3000000000i64

@test
def binaryGreaterEqualTest31(): Bool = 120000000000000000000000000000000000000000.0 >= 30000000000000000000000000000000000000000.0

@test
def binaryGreaterEqualTest32(): Bool = assertEq!(30000000000000000000000000000000000000000.0 >= 120000000000000000000000000000000000000000.0, false)

@test
def binaryGreaterEqualTest33(): Bool = 30000000000000000000000000000000000000000.0 >= 30000000000000000000000000000000000000000.0

@test
def binaryGreaterEqualTest34(): Bool = assertEq!(-120000000000000000000000000000000000000000.0 >= -30000000000000000000000000000000000000000.0, false)

@test
def binaryGreaterEqualTest35(): Bool = -30000000000000000000000000000000000000000.0 >= -120000000000000000000000000000000000000000.0

@test
def binaryGreaterEqualTest36(): Bool = -30000000000000000000000000000000000000000.0 >= -30000000000000000000000000000000000000000.0

@test
def binaryGreaterEqualTest37(): Bool = 1200000000000000000000.0f32 >= 300000000000000000000.0f32

@test
def binaryGreaterEqualTest38(): Bool = assertEq!(300000000000000000000.0f32 >= 1200000000000000000000.0f32, false)

@test
def binaryGreaterEqualTest39(): Bool = 300000000000000000000.0f32 >= 300000000000000000000.0f32

@test
def binaryGreaterEqualTest40(): Bool = assertEq!(-1200000000000000000000.0f32 >= -300000000000000000000.0f32, false)

@test
def binaryGreaterEqualTest41(): Bool = -300000000000000000000.0f32 >= -1200000000000000000000.0f32

@test
def binaryGreaterEqualTest42(): Bool = -300000000000000000000.0f32 >= -300000000000000000000.0f32

@test
def binaryGreaterEqualTest43(): Bool = 120000000000000000000000000000000000000000.0f64 >= 30000000000000000000000000000000000000000.0f64

@test
def binaryGreaterEqualTest44(): Bool = assertEq!(30000000000000000000000000000000000000000.0f64 >= 120000000000000000000000000000000000000000.0f64, false)

@test
def binaryGreaterEqualTest45(): Bool = 30000000000000000000000000000000000000000.0f64 >= 30000000000000000000000000000000000000000.0f64

@test
def binaryGreaterEqualTest46(): Bool = assertEq!(-120000000000000000000000000000000000000000.0f64 >= -30000000000000000000000000000000000000000.0f64, false)

@test
def binaryGreaterEqualTest47(): Bool =-30000000000000000000000000000000000000000.0f64 >= -120000000000000000000000000000000000000000.0f64

@test
def binaryGreaterEqualTest48(): Bool =  -30000000000000000000000000000000000000000.0f64 >= -30000000000000000000000000000000000000000.0f64

@test
def binaryGreaterEqualTest49(): Bool = 12000000000000000000ii >= 3000000000000000000ii

@test
def binaryGreaterEqualTest50(): Bool = assertEq!(3000000000000000000ii >= 12000000000000000000ii, false)

@test
def binaryGreaterEqualTest51(): Bool = 3000000000000000000ii >= 3000000000000000000ii

@test
def binaryGreaterEqualTest52(): Bool = assertEq!(-12000000000000000000ii >= -3000000000000000000ii, false)

@test
def binaryGreaterEqualTest53(): Bool = -3000000000000000000ii >= -12000000000000000000ii

@test
def binaryGreaterEqualTest54(): Bool = -3000000000000000000ii >= -3000000000000000000ii

@test
def binaryGreaterEqualTest55(): Bool = '十' >= '\u0000'

@test
def binaryGreaterEqualTest56(): Bool = assertEq!('\u0000' >= '十', false)

@test
def binaryGreaterEqualTest57(): Bool = '\u0000' >= '\u0000'

@test
def binaryEqualTest01(): Bool = assertEq!(120000 == 30000, false)

@test
def binaryEqualTest02(): Bool = assertEq!(30000 == 120000, false)

@test
def binaryEqualTest03(): Bool = 30000 == 30000

@test
def binaryEqualTest04(): Bool = assertEq!(-120000 == -30000, false)

@test
def binaryEqualTest05(): Bool = assertEq!(-30000 == -120000, false)

@test
def binaryEqualTest06(): Bool = -30000 == -30000

@test
def binaryEqualTest07(): Bool = assertEq!(12i8 == 3i8, false)

@test
def binaryEqualTest08(): Bool = assertEq!(3i8 == 12i8, false)

@test
def binaryEqualTest09(): Bool = 3i8 == 3i8

@test
def binaryEqualTest10(): Bool = assertEq!(-12i8 == -3i8, false)

@test
def binaryEqualTest11(): Bool = assertEq!(-3i8 == -12i8, false)

@test
def binaryEqualTest12(): Bool = -3i8 == -3i8

@test
def binaryEqualTest13(): Bool = assertEq!(1200i16 == 300i16, false)

@test
def binaryEqualTest14(): Bool = assertEq!(300i16 == 1200i16, false)

@test
def binaryEqualTest15(): Bool = 300i16 == 300i16

@test
def binaryEqualTest16(): Bool = assertEq!(-1200i16 == -300i16, false)

@test
def binaryEqualTest17(): Bool = assertEq!(-300i16 == -1200i16, false)

@test
def binaryEqualTest18(): Bool = -300i16 == -300i16

@test
def binaryEqualTest19(): Bool = assertEq!(120000i32 == 30000i32, false)

@test
def binaryEqualTest20(): Bool = assertEq!(30000i32 == 120000i32, false)

@test
def binaryEqualTest21(): Bool = 30000i32 == 30000i32

@test
def binaryEqualTest22(): Bool = assertEq!(-120000i32 == -30000i32, false)

@test
def binaryEqualTest23(): Bool = assertEq!(-30000i32 == -120000i32, false)

@test
def binaryEqualTest24(): Bool = -30000i32 == -30000i32

@test
def binaryEqualTest25(): Bool = assertEq!(12000000000i64 == 3000000000i64, false)

@test
def binaryEqualTest26(): Bool = assertEq!(3000000000i64 == 12000000000i64, false)

@test
def binaryEqualTest27(): Bool = 3000000000i64 == 3000000000i64

@test
def binaryEqualTest28(): Bool = assertEq!(-12000000000i64 == -3000000000i64, false)

@test
def binaryEqualTest29(): Bool = assertEq!(-3000000000i64 == -12000000000i64, false)

@test
def binaryEqualTest30(): Bool = -3000000000i64 == -3000000000i64

@test
def binaryEqualTest31(): Bool = assertEq!(120000000000000000000000000000000000000000.0 == 30000000000000000000000000000000000000000.0, false)

@test
def binaryEqualTest32(): Bool = assertEq!(30000000000000000000000000000000000000000.0 == 120000000000000000000000000000000000000000.0, false)

@test
def binaryEqualTest33(): Bool = 30000000000000000000000000000000000000000.0 == 30000000000000000000000000000000000000000.0

@test
def binaryEqualTest34(): Bool = assertEq!(-120000000000000000000000000000000000000000.0 == -30000000000000000000000000000000000000000.0, false)

@test
def binaryEqualTest35(): Bool = assertEq!(-30000000000000000000000000000000000000000.0 == -120000000000000000000000000000000000000000.0, false)

@test
def binaryEqualTest36(): Bool = -30000000000000000000000000000000000000000.0 == -30000000000000000000000000000000000000000.0

@test
def binaryEqualTest37(): Bool = assertEq!(1200000000000000000000.0f32 == 300000000000000000000.0f32, false)

@test
def binaryEqualTest38(): Bool = assertEq!(300000000000000000000.0f32 == 1200000000000000000000.0f32, false)

@test
def binaryEqualTest39(): Bool = 300000000000000000000.0f32 == 300000000000000000000.0f32

@test
def binaryEqualTest40(): Bool = assertEq!(-1200000000000000000000.0f32 == -300000000000000000000.0f32, false)

@test
def binaryEqualTest41(): Bool = assertEq!(-300000000000000000000.0f32 == -1200000000000000000000.0f32, false)

@test
def binaryEqualTest42(): Bool = -300000000000000000000.0f32 == -300000000000000000000.0f32

@test
def binaryEqualTest43(): Bool = assertEq!(120000000000000000000000000000000000000000.0f64 == 30000000000000000000000000000000000000000.0f64, false)

@test
def binaryEqualTest44(): Bool = assertEq!(30000000000000000000000000000000000000000.0f64 == 120000000000000000000000000000000000000000.0f64, false)

@test
def binaryEqualTest45(): Bool = 30000000000000000000000000000000000000000.0f64 == 30000000000000000000000000000000000000000.0f64

@test
def binaryEqualTest46(): Bool = assertEq!(-120000000000000000000000000000000000000000.0f64 == -30000000000000000000000000000000000000000.0f64, false)

@test
def binaryEqualTest47(): Bool = assertEq!(-30000000000000000000000000000000000000000.0f64 == -120000000000000000000000000000000000000000.0f64, false)

@test
def binaryEqualTest48(): Bool =  -30000000000000000000000000000000000000000.0f64 == -30000000000000000000000000000000000000000.0f64

@test
def binaryEqualTest49(): Bool = assertEq!(12000000000000000000ii == 3000000000000000000ii, false)

@test
def binaryEqualTest50(): Bool = assertEq!(3000000000000000000ii == 12000000000000000000ii, false)

@test
def binaryEqualTest51(): Bool = 3000000000000000000ii == 3000000000000000000ii

@test
def binaryEqualTest52(): Bool = assertEq!(-12000000000000000000ii == -3000000000000000000ii, false)

@test
def binaryEqualTest53(): Bool = assertEq!(-3000000000000000000ii == -12000000000000000000ii, false)

@test
def binaryEqualTest54(): Bool = -3000000000000000000ii == -3000000000000000000ii

@test
def binaryEqualTest55(): Bool = assertEq!('十' == '\u0000', false)

@test
def binaryEqualTest56(): Bool = assertEq!('\u0000' == '十', false)

@test
def binaryEqualTest57(): Bool = '\u0000' == '\u0000'

@test
def binaryNotEqualTest01(): Bool = 120000 != 30000

@test
def binaryNotEqualTest02(): Bool = 30000 != 120000

@test
def binaryNotEqualTest03(): Bool = assertEq!(30000 != 30000, false)

@test
def binaryNotEqualTest04(): Bool = -120000 != -30000

@test
def binaryNotEqualTest05(): Bool = -30000 != -120000

@test
def binaryNotEqualTest06(): Bool = assertEq!(-30000 != -30000, false)

@test
def binaryNotEqualTest07(): Bool = 12i8 != 3i8

@test
def binaryNotEqualTest08(): Bool = 3i8 != 12i8

@test
def binaryNotEqualTest09(): Bool = assertEq!(3i8 != 3i8, false)

@test
def binaryNotEqualTest10(): Bool = -12i8 != -3i8

@test
def binaryNotEqualTest11(): Bool = -3i8 != -12i8

@test
def binaryNotEqualTest12(): Bool = assertEq!(-3i8 != -3i8, false)

@test
def binaryNotEqualTest13(): Bool = 1200i16 != 300i16

@test
def binaryNotEqualTest14(): Bool = 300i16 != 1200i16

@test
def binaryNotEqualTest15(): Bool = assertEq!(300i16 != 300i16, false)

@test
def binaryNotEqualTest16(): Bool = -1200i16 != -300i16

@test
def binaryNotEqualTest17(): Bool = -300i16 != -1200i16

@test
def binaryNotEqualTest18(): Bool = assertEq!(-300i16 != -300i16, false)

@test
def binaryNotEqualTest19(): Bool = 120000i32 != 30000i32

@test
def binaryNotEqualTest20(): Bool = 30000i32 != 120000i32

@test
def binaryNotEqualTest21(): Bool = assertEq!(30000i32 != 30000i32, false)

@test
def binaryNotEqualTest22(): Bool = -120000i32 != -30000i32

@test
def binaryNotEqualTest23(): Bool = -30000i32 != -120000i32

@test
def binaryNotEqualTest24(): Bool = assertEq!(-30000i32 != -30000i32, false)

@test
def binaryNotEqualTest25(): Bool = 12000000000i64 != 3000000000i64

@test
def binaryNotEqualTest26(): Bool = 3000000000i64 != 12000000000i64

@test
def binaryNotEqualTest27(): Bool = assertEq!(3000000000i64 != 3000000000i64, false)

@test
def binaryNotEqualTest28(): Bool = -12000000000i64 != -3000000000i64

@test
def binaryNotEqualTest29(): Bool = -3000000000i64 != -12000000000i64

@test
def binaryNotEqualTest30(): Bool = assertEq!(-3000000000i64 != -3000000000i64, false)

@test
def binaryNotEqualTest31(): Bool = 120000000000000000000000000000000000000000.0 != 30000000000000000000000000000000000000000.0

@test
def binaryNotEqualTest32(): Bool = 30000000000000000000000000000000000000000.0 != 120000000000000000000000000000000000000000.0

@test
def binaryNotEqualTest33(): Bool = assertEq!(30000000000000000000000000000000000000000.0 != 30000000000000000000000000000000000000000.0, false)

@test
def binaryNotEqualTest34(): Bool = -120000000000000000000000000000000000000000.0 != -30000000000000000000000000000000000000000.0

@test
def binaryNotEqualTest35(): Bool = -30000000000000000000000000000000000000000.0 != -120000000000000000000000000000000000000000.0

@test
def binaryNotEqualTest36(): Bool = assertEq!(-30000000000000000000000000000000000000000.0 != -30000000000000000000000000000000000000000.0, false)

@test
def binaryNotEqualTest37(): Bool = 1200000000000000000000.0f32 != 300000000000000000000.0f32

@test
def binaryNotEqualTest38(): Bool = 300000000000000000000.0f32 != 1200000000000000000000.0f32

@test
def binaryNotEqualTest39(): Bool = assertEq!(300000000000000000000.0f32 != 300000000000000000000.0f32, false)

@test
def binaryNotEqualTest40(): Bool = -1200000000000000000000.0f32 != -300000000000000000000.0f32

@test
def binaryNotEqualTest41(): Bool = -300000000000000000000.0f32 != -1200000000000000000000.0f32

@test
def binaryNotEqualTest42(): Bool = assertEq!(-300000000000000000000.0f32 != -300000000000000000000.0f32, false)

@test
def binaryNotEqualTest43(): Bool = 120000000000000000000000000000000000000000.0f64 != 30000000000000000000000000000000000000000.0f64

@test
def binaryNotEqualTest44(): Bool = 30000000000000000000000000000000000000000.0f64 != 120000000000000000000000000000000000000000.0f64

@test
def binaryNotEqualTest45(): Bool = assertEq!(30000000000000000000000000000000000000000.0f64 != 30000000000000000000000000000000000000000.0f64, false)

@test
def binaryNotEqualTest46(): Bool = -120000000000000000000000000000000000000000.0f64 != -30000000000000000000000000000000000000000.0f64

@test
def binaryNotEqualTest47(): Bool = -30000000000000000000000000000000000000000.0f64 != -120000000000000000000000000000000000000000.0f64

@test
def binaryNotEqualTest48(): Bool =  assertEq!(-30000000000000000000000000000000000000000.0f64 != -30000000000000000000000000000000000000000.0f64, false)

@test
def binaryNotEqualTest49(): Bool = 12000000000000000000ii != 3000000000000000000ii

@test
def binaryNotEqualTest50(): Bool = 3000000000000000000ii != 12000000000000000000ii

@test
def binaryNotEqualTest51(): Bool = assertEq!(3000000000000000000ii != 3000000000000000000ii, false)

@test
def binaryNotEqualTest52(): Bool = -12000000000000000000ii != -3000000000000000000ii

@test
def binaryNotEqualTest53(): Bool = -3000000000000000000ii != -12000000000000000000ii

@test
def binaryNotEqualTest54(): Bool = assertEq!(-3000000000000000000ii != -3000000000000000000ii, false)

@test
def binaryNotEqualTest55(): Bool = '十' != '\u0000'

@test
def binaryNotEqualTest56(): Bool = '\u0000' != '十'

@test
def binaryNotEqualTest57(): Bool = assertEq!('\u0000' != '\u0000', false)