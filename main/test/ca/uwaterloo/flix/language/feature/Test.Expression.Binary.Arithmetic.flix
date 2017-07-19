/////////////////////////////////////////////////////////////////////////////
// Expression.Binary (Arithmetic)                                          //
// BinaryOperator.{Plus,Minus,Times,Divide,Modulo,Exponentiate}            //
/////////////////////////////////////////////////////////////////////////////

@test
def binaryPlusTest01(): Bool = assertEq!(2147483647 + 1, -2147483648)

@test
def binaryPlusTest02(): Bool = assertEq!(100000 + 400000, 500000)

@test
def binaryPlusTest03(): Bool = assertEq!(-400000 + 100000, -300000)

@test
def binaryPlusTest04(): Bool = assertEq!(-100000 + 400000, 300000)

@test
def binaryPlusTest05(): Bool = assertEq!(-2147483648 + -1, 2147483647)

@test
def binaryPlusTest06(): Bool = assertEq!(127i8 + 1i8, -128i8)

@test
def binaryPlusTest07(): Bool = assertEq!(10i8 + 40i8, 50i8)

@test
def binaryPlusTest08(): Bool = assertEq!(-40i8 + 10i8, -30i8)

@test
def binaryPlusTest09(): Bool = assertEq!(-10i8 + 40i8, 30i8)

@test
def binaryPlusTest10(): Bool = assertEq!(-128i8 + -1i8, 127i8)

@test
def binaryPlusTest11(): Bool = assertEq!(32767i16 + 1i16, -32768i16)

@test
def binaryPlusTest12(): Bool = assertEq!(1000i16 + 4000i16, 5000i16)

@test
def binaryPlusTest13(): Bool = assertEq!(-4000i16 + 1000i16, -3000i16)

@test
def binaryPlusTest14(): Bool = assertEq!(-1000i16 + 4000i16, 3000i16)

@test
def binaryPlusTest15(): Bool = assertEq!(-32768i16 + -1i16, 32767i16)

@test
def binaryPlusTest16(): Bool = assertEq!(2147483647i32 + 1i32, -2147483648i32)

@test
def binaryPlusTest17(): Bool = assertEq!(100000i32 + 400000i32, 500000i32)

@test
def binaryPlusTest18(): Bool = assertEq!(-400000i32 + 100000i32, -300000i32)

@test
def binaryPlusTest19(): Bool = assertEq!(-100000i32 + 400000i32, 300000i32)

@test
def binaryPlusTest20(): Bool = assertEq!(-2147483648i32 + -1i32, 2147483647i32)

@test
def binaryPlusTest21(): Bool = assertEq!(9223372036854775807i64 + 1i64, -9223372036854775808i64)

@test
def binaryPlusTest22(): Bool = assertEq!(10000000000i64 + 40000000000i64, 50000000000i64)

@test
def binaryPlusTest23(): Bool = assertEq!(-40000000000i64 + 10000000000i64, -30000000000i64)

@test
def binaryPlusTest24(): Bool = assertEq!(-10000000000i64 + 40000000000i64, 30000000000i64)

@test
def binaryPlusTest25(): Bool = assertEq!(-9223372036854775808i64 + -1i64, 9223372036854775807i64)


@test
def binaryPlusTest26(): Bool = assertEq!(12.34 + 56.78, 69.12)

@test
def binaryPlusTest27(): Bool = assertEq!(1234567890000000000000000000000000000000000000000.987654321 + 222.222,
                                         1234567890000000000000000000000000000000000000223.209654321)

@test
def binaryPlusTest28(): Bool = assertEq!(-1234567890000000000000000000000000000000000000000.987654321 + 0.0,
                                         -1234567890000000000000000000000000000000000000000.987654321)

@test
def binaryPlusTest29(): Bool = assertEq!(0.0000000000000000000000000000000000000000987654321 + 0.222, 0.222)

@test
def binaryPlusTest30(): Bool = assertEq!(-0.0000000000000000000000000000000000000000987654321 + 0.222, 0.222)

@test
def binaryPlusTest31(): Bool = assertEq!(12.34f32 + 56.78f32, 69.119995f32)

@test
def binaryPlusTest32(): Bool = assertEq!(1234567890000000000000000000000000000000000000000.987654321f32 + 222.222f32,
                                         1234567890000000000000000000000000000000000000223.209654321f32)

@test
def binaryPlusTest33(): Bool = assertEq!(-1234567890000000000000000000000000000000000000000.987654321f32 + 0.0f32,
                                         -1234567890000000000000000000000000000000000000000.987654321f32)

@test
def binaryPlusTest34(): Bool = assertEq!(0.0000000000000000000000000000000000000000987654321f32 + 0.222f32, 0.222f32)

@test
def binaryPlusTest35(): Bool = assertEq!(-0.0000000000000000000000000000000000000000987654321f32 + 0.222f32, 0.222f32)

@test
def binaryPlusTest36(): Bool = assertEq!(12.34f32 + 56.78f32, 69.119995f32)

@test
def binaryPlusTest37(): Bool = assertEq!(1234567890000000000000000000000000000000000000000.987654321f64 + 222.222f64,
                                         1234567890000000000000000000000000000000000000223.209654321f64)

@test
def binaryPlusTest38(): Bool = assertEq!(-1234567890000000000000000000000000000000000000000.987654321f64 + 0.0f64,
                                         -1234567890000000000000000000000000000000000000000.987654321f64)

@test
def binaryPlusTest39(): Bool = assertEq!(0.0000000000000000000000000000000000000000987654321f64 + 0.222f64, 0.222f64)

@test
def binaryPlusTest40(): Bool = assertEq!(-0.0000000000000000000000000000000000000000987654321f64 + 0.222f64, 0.222f64)

@test
def binaryPlusTest41(): Bool = assertEq!(9223372036854775808ii + 1ii, 9223372036854775809ii)

@test
def binaryPlusTest42(): Bool = assertEq!(10000000000000000000ii + 40000000000000000000ii, 50000000000000000000ii)

@test
def binaryPlusTest43(): Bool = assertEq!(-40000000000000000000ii + 10000000000000000000ii, -30000000000000000000ii)

@test
def binaryPlusTest44(): Bool = assertEq!(-10000000000000000000ii + 40000000000000000000ii, 30000000000000000000ii)

@test
def binaryPlusTest45(): Bool = assertEq!(-9223372036854775809ii + -1ii, -9223372036854775810ii)

@test
def binaryMinusTest01(): Bool = assertEq!(-2147483648 - 1, 2147483647)

@test
def binaryMinusTest02(): Bool = assertEq!(400000 - 100000, 300000)

@test
def binaryMinusTest03(): Bool = assertEq!(-400000 - 100000, -500000)

@test
def binaryMinusTest04(): Bool = assertEq!(-100000 - 400000, -500000)

@test
def binaryMinusTest05(): Bool = assertEq!(2147483647 - -1, -2147483648)

@test
def binaryMinusTest06(): Bool = assertEq!(-128i8 - 1i8, 127i8)

@test
def binaryMinusTest07(): Bool = assertEq!(40i8 - 10i8, 30i8)

@test
def binaryMinusTest08(): Bool = assertEq!(-40i8 - 10i8, -50i8)

@test
def binaryMinusTest09(): Bool = assertEq!(-10i8 - 40i8, -50i8)

@test
def binaryMinusTest10(): Bool = assertEq!(-128i8 - 1i8, 127i8)

@test
def binaryMinusTest11(): Bool = assertEq!(-32768i16 - 1i16, 32767i16)

@test
def binaryMinusTest12(): Bool = assertEq!(4000i16 - 1000i16, 3000i16)

@test
def binaryMinusTest13(): Bool = assertEq!(-4000i16 - 1000i16, -5000i16)

@test
def binaryMinusTest14(): Bool = assertEq!(-1000i16 - 4000i16, -5000i16)

@test
def binaryMinusTest15(): Bool = assertEq!(32767i16 - -1i16, -32768i16)

@test
def binaryMinusTest16(): Bool = assertEq!(-2147483648i32 - 1i32, 2147483647i32)

@test
def binaryMinusTest17(): Bool = assertEq!(400000i32 - 100000i32, 300000i32)

@test
def binaryMinusTest18(): Bool = assertEq!(-400000i32 - 100000i32, -500000i32)

@test
def binaryMinusTest19(): Bool = assertEq!(-100000i32 - 400000i32, -500000i32)

@test
def binaryMinusTest20(): Bool = assertEq!(2147483647i32 - -1i32, -2147483648i32)

@test
def binaryMinusTest21(): Bool = assertEq!(-9223372036854775808i64 - 1i64, 9223372036854775807i64)

@test
def binaryMinusTest22(): Bool = assertEq!(40000000000i64 - 10000000000i64, 30000000000i64)

@test
def binaryMinusTest23(): Bool = assertEq!(-40000000000i64 - 10000000000i64, -50000000000i64)

@test
def binaryMinusTest24(): Bool = assertEq!(-10000000000i64 - 40000000000i64, -50000000000i64)

@test
def binaryMinusTest25(): Bool = assertEq!(9223372036854775807i64 - -1i64, -9223372036854775808i64)

@test
def binaryMinusTest26(): Bool = assertEq!(12.34 - 56.78, -44.44)

@test
def binaryMinusTest27(): Bool = assertEq!(1234567890000000000000000000000000000000000000000.987654321 - 222.222,
                                         1234567890000000000000000000000000000000000000000.0)

@test
def binaryMinusTest28(): Bool = assertEq!(-1234567890000000000000000000000000000000000000000.987654321 - 0.0,
                                         -1234567890000000000000000000000000000000000000000.0)

@test
def binaryMinusTest29(): Bool = assertEq!(0.0000000000000000000000000000000000000000987654321 - 0.222, -0.222)

@test
def binaryMinusTest30(): Bool = assertEq!(-0.0000000000000000000000000000000000000000987654321 - 0.222, -0.222)

@test
def binaryMinusTest31(): Bool = assertEq!(12.34f32 - 56.78f32, -44.44f32)

@test
def binaryMinusTest32(): Bool = assertEq!(123456789000000000000000000000000000000.987654321f32 - 222.222f32,
                                         123456789000000000000000000000000000000.0f32)

@test
def binaryMinusTest33(): Bool = assertEq!(-123456789000000000000000000000000000000.987654321f32 - 0.0f32,
                                         -123456789000000000000000000000000000000.0f32)

@test
def binaryMinusTest34(): Bool = assertEq!(0.000000000000000000000000000000987654321f32 - 0.222f32, -0.222f32)

@test
def binaryMinusTest35(): Bool = assertEq!(-0.000000000000000000000000000000987654321f32 - 0.222f32, -0.222f32)

@test
def binaryMinusTest36(): Bool = assertEq!(12.34f64 - 56.78f64, -44.44f64)

@test
def binaryMinusTest37(): Bool = assertEq!(123456789000000000000000000000000000000.987654321f64 - 222.222f64,
                                         123456789000000000000000000000000000000.0f64)

@test
def binaryMinusTest38(): Bool = assertEq!(-123456789000000000000000000000000000000.987654321f64 - 0.0f64,
                                         -123456789000000000000000000000000000000.0f64)

@test
def binaryMinusTest39(): Bool = assertEq!(0.000000000000000000000000000000987654321f64 - 0.222f64, -0.222f64)

@test
def binaryMinusTest40(): Bool = assertEq!(-0.000000000000000000000000000000987654321f64 - 0.222f64, -0.222f64)

@test
def binaryMinusTest41(): Bool = assertEq!(-9223372036854775809ii - 1ii, -9223372036854775810ii)

@test
def binaryMinusTest42(): Bool = assertEq!(40000000000000000000ii - 10000000000000000000ii, 30000000000000000000ii)

@test
def binaryMinusTest43(): Bool = assertEq!(-40000000000000000000ii - 10000000000000000000ii, -50000000000000000000ii)

@test
def binaryMinusTest44(): Bool = assertEq!(-10000000000000000000ii - 40000000000000000000ii, -50000000000000000000ii)

@test
def binaryMinusTest45(): Bool = assertEq!(9223372036854775808ii - -1ii, 9223372036854775809ii)

@test
def binaryTimesTest01(): Bool = assertEq!(2147483647 * 2, -2)

@test
def binaryTimesTest02(): Bool = assertEq!(300 * 200, 60000)

@test
def binaryTimesTest03(): Bool = assertEq!(-200 * 300, -60000)

@test
def binaryTimesTest04(): Bool = assertEq!(-200 * -300, 60000)

@test
def binaryTimesTest05(): Bool = assertEq!(-2147483648 * -1, -2147483648)

@test
def binaryTimesTest06(): Bool = assertEq!(127i8 * 2i8, -2i8)

@test
def binaryTimesTest07(): Bool = assertEq!(3i8 * 2i8, 6i8)

@test
def binaryTimesTest08(): Bool = assertEq!(-2i8 * 3i8, -6i8)

@test
def binaryTimesTest09(): Bool = assertEq!(-2i8 * -3i8, 6i8)

@test
def binaryTimesTest10(): Bool = assertEq!(-128i8 * -1i8, -128i8)

@test
def binaryTimesTest11(): Bool = assertEq!(32767i16 * 2i16, -2i16)

@test
def binaryTimesTest12(): Bool = assertEq!(30i16 * 20i16, 600i16)

@test
def binaryTimesTest13(): Bool = assertEq!(-20i16 * 30i16, -600i16)

@test
def binaryTimesTest14(): Bool = assertEq!(-20i16 * -30i16, 600i16)

@test
def binaryTimesTest15(): Bool = assertEq!(-32768i16 * -1i16, -32768i16)

@test
def binaryTimesTest16(): Bool = assertEq!(2147483647i32 * 2i32, -2i32)

@test
def binaryTimesTest17(): Bool = assertEq!(300i32 * 200i32, 60000i32)

@test
def binaryTimesTest18(): Bool = assertEq!(-200i32 * 300i32, -60000i32)

@test
def binaryTimesTest19(): Bool = assertEq!(-200i32 * -300i32, 60000i32)

@test
def binaryTimesTest20(): Bool = assertEq!(-2147483648i32 * -1i32, -2147483648i32)

@test
def binaryTimesTest21(): Bool = assertEq!(9223372036854775807i64 * 2i64, -2i64)

@test
def binaryTimesTest22(): Bool = assertEq!(300000i64 * 200000i64, 60000000000i64)

@test
def binaryTimesTest23(): Bool = assertEq!(-200000i64 * 300000i64, -60000000000i64)

@test
def binaryTimesTest24(): Bool = assertEq!(-200000i64 * -300000i64, 60000000000i64)

@test
def binaryTimesTest25(): Bool = assertEq!(-9223372036854775808i64 * -1i64, -9223372036854775808i64)

@test
def binaryTimesTest26(): Bool = assertEq!(12.34 * 56.78, 700.6652)

@test
def binaryTimesTest27(): Bool = assertEq!(1234567890000000000000000000000000000000000000000.987654321 * 222.222,
                                          274348145651580030000000000000000000000000000000000.0)

@test
def binaryTimesTest28(): Bool = assertEq!(-1234567890000000000000000000000000000000000000000.987654321 * 222.222,
                                          -274348145651580030000000000000000000000000000000000.0)

@test
def binaryTimesTest29(): Bool = assertEq!(0.0000000000000000000000000000000000000000987654321 * 0.222,
                                          0.0000000000000000000000000000000000000000219259259262)

@test
def binaryTimesTest30(): Bool = assertEq!(-0.0000000000000000000000000000000000000000987654321 * 0.222,
                                          -0.0000000000000000000000000000000000000000219259259262)

@test
def binaryTimesTest31(): Bool = assertEq!(12.34f32 * 56.78f32, 700.6652f32)

@test
def binaryTimesTest32(): Bool = assertEq!(1234567890000000000000000000000000000000000000000.987654321f32 * 222.222f32,
                                          274348145651580030000000000000000000000000000000000.0f32)

@test
def binaryTimesTest33(): Bool = assertEq!(-1234567890000000000000000000000000000000000000000.987654321f32 * 222.222f32,
                                          -274348145651580030000000000000000000000000000000000.0f32)

@test
def binaryTimesTest34(): Bool = assertEq!(0.0000000000000000000000000000000000000000987654321f32 * 0.222f32,
                                          0.0000000000000000000000000000000000000000219259259262f32)

@test
def binaryTimesTest35(): Bool = assertEq!(-0.0000000000000000000000000000000000000000987654321f32 * 0.222f32,
                                          -0.0000000000000000000000000000000000000000219259259262f32)

@test
def binaryTimesTest36(): Bool = assertEq!(12.34f64 * 56.78f64, 700.6652f64)

@test
def binaryTimesTest37(): Bool = assertEq!(1234567890000000000000000000000000000000000000000.987654321f64 * 222.222f64,
                                          274348145651580030000000000000000000000000000000000.0f64)

@test
def binaryTimesTest38(): Bool = assertEq!(-1234567890000000000000000000000000000000000000000.987654321f64 * 222.222f64,
                                          -274348145651580030000000000000000000000000000000000.0f64)

@test
def binaryTimesTest39(): Bool = assertEq!(0.0000000000000000000000000000000000000000987654321f64 * 0.222f64,
                                          0.0000000000000000000000000000000000000000219259259262f64)

@test
def binaryTimesTest40(): Bool = assertEq!(-0.0000000000000000000000000000000000000000987654321f64 * 0.222f64,
                                          -0.0000000000000000000000000000000000000000219259259262f64)

@test
def binaryTimesTest41(): Bool = assertEq!(9223372036854775808ii * 2ii, 18446744073709551616ii)

@test
def binaryTimesTest42(): Bool = assertEq!(30000000000ii * 20000000000ii, 600000000000000000000ii)

@test
def binaryTimesTest43(): Bool = assertEq!(-20000000000ii * 30000000000ii, -600000000000000000000ii)

@test
def binaryTimesTest44(): Bool = assertEq!(-20000000000ii * -30000000000ii, 600000000000000000000ii)

@test
def binaryTimesTest45(): Bool = assertEq!(-9223372036854775809ii * -1ii, 9223372036854775809ii)

@test
def binaryDivideTest01(): Bool = assertEq!(2147483647 / 1, 2147483647)

@test
def binaryDivideTest02(): Bool = assertEq!(1200000 / 3, 400000)

@test
def binaryDivideTest03(): Bool = assertEq!(-1200000 / 3, -400000)

@test
def binaryDivideTest04(): Bool = assertEq!(-3 / 1200000, 0)

@test
def binaryDivideTest05(): Bool = assertEq!(-2147483648 / -1, -2147483648)

@test
def binaryDivideTest06(): Bool = assertEq!(127i8 / 1i8, 127i8)

@test
def binaryDivideTest07(): Bool = assertEq!(12i8 / 3i8, 4i8)

@test
def binaryDivideTest08(): Bool = assertEq!(-12i8 / 3i8, -4i8)

@test
def binaryDivideTest09(): Bool = assertEq!(-3i8 / 12i8, 0i8)

@test
def binaryDivideTest10(): Bool = assertEq!(-128i8 / -1i8, -128i8)

@test
def binaryDivideTest11(): Bool = assertEq!(32767i16 / 1i16, 32767i16)

@test
def binaryDivideTest12(): Bool = assertEq!(12000i16 / 3i16, 4000i16)

@test
def binaryDivideTest13(): Bool = assertEq!(-12000i16 / 3i16, -4000i16)

@test
def binaryDivideTest14(): Bool = assertEq!(-3i16 / 12000i16, 0i16)

@test
def binaryDivideTest15(): Bool = assertEq!(-32768i16 / -1i16, -32768i16)

@test
def binaryDivideTest16(): Bool = assertEq!(2147483647i32 / 1i32, 2147483647i32)

@test
def binaryDivideTest17(): Bool = assertEq!(1200000i32 / 3i32, 400000i32)

@test
def binaryDivideTest18(): Bool = assertEq!(-1200000i32 / 3i32, -400000i32)

@test
def binaryDivideTest19(): Bool = assertEq!(-3i32 / 1200000i32, 0i32)

@test
def binaryDivideTest20(): Bool = assertEq!(-2147483648i32 / -1i32, -2147483648i32)

@test
def binaryDivideTest21(): Bool = assertEq!(9223372036854775807i64 / 1i64, 9223372036854775807i64)

@test
def binaryDivideTest22(): Bool = assertEq!(120000000000i64 / 3i64, 40000000000i64)

@test
def binaryDivideTest23(): Bool = assertEq!(-120000000000i64 / 3i64, -40000000000i64)

@test
def binaryDivideTest24(): Bool = assertEq!(-3i64 / 120000000000i64, 0i64)

@test
def binaryDivideTest25(): Bool = assertEq!(-9223372036854775808i64 / -1i64, -9223372036854775808i64)

@test
def binaryDivideTest26(): Bool = assertEq!(12.34 / 56.78, 0.2173300457907714)

@test
def binaryDivideTest27(): Bool = assertEq!(1234567890000000000000000000000000000000000000000.987654321 / 222.222,
                                           5555561060561060400000000000000000000000000000.0)

@test
def binaryDivideTest28(): Bool = assertEq!(-1234567890000000000000000000000000000000000000000.987654321 / 222.222,
                                           -5555561060561060400000000000000000000000000000.0)

@test
def binaryDivideTest29(): Bool = assertEq!(0.0000000000000000000000000000000000000000987654321 / 0.222,
                                           0.00000000000000000000000000000000000000044488933378378374)

@test
def binaryDivideTest30(): Bool = assertEq!(-0.0000000000000000000000000000000000000000987654321 / 0.222,
                                           -0.00000000000000000000000000000000000000044488933378378374)

@test
def binaryDivideTest31(): Bool = assertEq!(12.34f32 / 56.78f32, 0.21733005f32)

@test
def binaryDivideTest32(): Bool = assertEq!(123456789000000000000000000000000000000.987654321f32 / 222.222f32,
                                           555556080000000000000000000000000000.0f32)
@test
def binaryDivideTest33(): Bool = assertEq!(-123456789000000000000000000000000000000.987654321f32 / 222.222f32,
                                           -555556080000000000000000000000000000.0f32)

@test
def binaryDivideTest34(): Bool = assertEq!(0.000000000000000000000000000000987654321f32 / 0.222f32,
                                           0.0000000000000000000000000000044488933f32)
@test
def binaryDivideTest35(): Bool = assertEq!(-0.000000000000000000000000000000987654321f32 / 0.222f32,
                                           -0.0000000000000000000000000000044488933f32)

@test
def binaryDivideTest36(): Bool = assertEq!(12.34f64 / 56.78f64, 0.2173300457907714f64)

@test
def binaryDivideTest37(): Bool = assertEq!(1234567890000000000000000000000000000000000000000.987654321f64 / 222.222f64,
                                           5555561060561060400000000000000000000000000000.0f64)

@test
def binaryDivideTest38(): Bool = assertEq!(-1234567890000000000000000000000000000000000000000.987654321f64 / 222.222f64,
                                           -5555561060561060400000000000000000000000000000.0f64)

@test
def binaryDivideTest39(): Bool = assertEq!(0.0000000000000000000000000000000000000000987654321f64 / 0.222f64,
                                           0.00000000000000000000000000000000000000044488933378378374f64)

@test
def binaryDivideTest40(): Bool = assertEq!(-0.0000000000000000000000000000000000000000987654321f64 / 0.222f64,
                                           -0.00000000000000000000000000000000000000044488933378378374f64)

@test
def binaryDivideTest41(): Bool = assertEq!(9223372036854775808ii / 1ii, 9223372036854775808ii)

@test
def binaryDivideTest42(): Bool = assertEq!(1200000000000000000000ii / 3ii, 400000000000000000000ii)

@test
def binaryDivideTest43(): Bool = assertEq!(-1200000000000000000000ii / 3ii, -400000000000000000000ii)

@test
def binaryDivideTest44(): Bool = assertEq!(-3ii / 1200000000000000000000ii, 0ii)

@test
def binaryDivideTest45(): Bool = assertEq!(-9223372036854775809ii / -1ii, 9223372036854775809ii)

@test
def binaryModuleTest01(): Bool = assertEq!(1200000 % 200000, 0)

@test
def binaryModuleTest02(): Bool = assertEq!(1200000 % 500000, 200000)

@test
def binaryModuleTest03(): Bool = assertEq!(-1200000 % 500000, -200000)

@test
def binaryModuleTest04(): Bool = assertEq!(1200000 % -500000, 200000)

@test
def binaryModuleTest05(): Bool = assertEq!(-1200000 % -500000, -200000)

@test
def binaryModuleTest06(): Bool = assertEq!(12i8 % 2i8, 0i8)

@test
def binaryModuleTest07(): Bool = assertEq!(12i8 % 5i8, 2i8)

@test
def binaryModuleTest08(): Bool = assertEq!(-12i8 % 5i8, -2i8)

@test
def binaryModuleTest09(): Bool = assertEq!(12i8 % -5i8, 2i8)

@test
def binaryModuleTest10(): Bool = assertEq!(-12i8 % -5i8, -2i8)

@test
def binaryModuleTest11(): Bool = assertEq!(12000i16 % 2000i16, 0i16)

@test
def binaryModuleTest12(): Bool = assertEq!(12000i16 % 5000i16, 2000i16)

@test
def binaryModuleTest13(): Bool = assertEq!(-12000i16 % 5000i16, -2000i16)

@test
def binaryModuleTest14(): Bool = assertEq!(12000i16 % -5000i16, 2000i16)

@test
def binaryModuleTest15(): Bool = assertEq!(-12000i16 % -5000i16, -2000i16)

@test
def binaryModuleTest16(): Bool = assertEq!(1200000i32 % 200000i32, 0i32)

@test
def binaryModuleTest17(): Bool = assertEq!(1200000i32 % 500000i32, 200000i32)

@test
def binaryModuleTest18(): Bool = assertEq!(-1200000i32 % 500000i32, -200000i32)

@test
def binaryModuleTest19(): Bool = assertEq!(1200000i32 % -500000i32, 200000i32)

@test
def binaryModuleTest20(): Bool = assertEq!(-1200000i32 % -500000i32, -200000i32)

@test
def binaryModuleTest21(): Bool = assertEq!(120000000000i64 % 20000000000i64, 0i64)

@test
def binaryModuleTest22(): Bool = assertEq!(120000000000i64 % 50000000000i64, 20000000000i64)

@test
def binaryModuleTest23(): Bool = assertEq!(-120000000000i64 % 50000000000i64, -20000000000i64)

@test
def binaryModuleTest24(): Bool = assertEq!(120000000000i64 % -50000000000i64, 20000000000i64)

@test
def binaryModuleTest25(): Bool = assertEq!(-120000000000i64 % -50000000000i64, -20000000000i64)

@test
def binaryModuleTest26(): Bool = assertEq!(12.34 % 56.78, 12.34)

@test
def binaryModuleTest27(): Bool = assertEq!(1234567890000000000000000000000000000000000000000.987654321 % 222.222,
                                           88.53722751835619)

@test
def binaryModuleTest28(): Bool = assertEq!(-1234567890000000000000000000000000000000000000000.987654321 % 222.222,
                                           -88.53722751835619)

@test
def binaryModuleTest29(): Bool = assertEq!(0.0000000000000000000000000000000000000000987654321 % 0.222,
                                           0.0000000000000000000000000000000000000000987654321)

@test
def binaryModuleTest30(): Bool = assertEq!(-0.0000000000000000000000000000000000000000987654321 % 0.222,
                                           -0.0000000000000000000000000000000000000000987654321)

@test
def binaryModuleTest31(): Bool = assertEq!(12.34f32 % 56.78f32, 12.34f32)

@test
def binaryModuleTest32(): Bool = assertEq!(123456789000000000000000000000000000000.987654321f32 % 222.222f32,
                                           29.297333f32)

@test
def binaryModuleTest33(): Bool = assertEq!(-123456789000000000000000000000000000000.987654321f32 % 222.222f32,
                                           -29.297333f32)

@test
def binaryModuleTest34(): Bool = assertEq!(0.000000000000000000000000000000987654321f32 % 0.222f32,
                                           0.000000000000000000000000000000987654321f32)

@test
def binaryModuleTest35(): Bool = assertEq!(-0.00000000000000000000000000000987654321f32 % 0.222f32,
                                           -0.00000000000000000000000000000987654321f32)

@test
def binaryModuleTest36(): Bool = assertEq!(12.34f64 % 56.78f64, 12.34f64)

@test
def binaryModuleTest37(): Bool = assertEq!(1234567890000000000000000000000000000000000000000.987654321f64 % 222.222f64,
                                           88.53722751835619f64)

@test
def binaryModuleTest38(): Bool = assertEq!(-1234567890000000000000000000000000000000000000000.987654321f64 % 222.222f64,
                                           -88.53722751835619f64)

@test
def binaryModuleTest39(): Bool = assertEq!(0.0000000000000000000000000000000000000000987654321f64 % 0.222f64,
                                           0.0000000000000000000000000000000000000000987654321f64)

@test
def binaryModuleTest40(): Bool = assertEq!(-0.0000000000000000000000000000000000000000987654321f64 % 0.222f64,
                                           -0.0000000000000000000000000000000000000000987654321f64)

@test
def binaryModuleTest41(): Bool = assertEq!(1200000000000000000000ii % 200000000000000000000ii, 0ii)

@test
def binaryModuleTest42(): Bool = assertEq!(1200000000000000000000ii % 500000000000000000000ii, 200000000000000000000ii)

@test
def binaryModuleTest43(): Bool = assertEq!(-1200000000000000000000ii % 500000000000000000000ii, -200000000000000000000ii)

@test
def binaryModuleTest44(): Bool = assertEq!(1200000000000000000000ii % -500000000000000000000ii, 200000000000000000000ii)

@test
def binaryModuleTest45(): Bool = assertEq!(-1200000000000000000000ii % -500000000000000000000ii, -200000000000000000000ii)

@test
def binaryExponentiateTest01(): Bool = assertEq!(2 ** 0, 1)

@test
def binaryExponentiateTest02(): Bool = assertEq!(-2 ** 1, -2)

@test
def binaryExponentiateTest03(): Bool = assertEq!(2 ** 2, 4)

@test
def binaryExponentiateTest04(): Bool = assertEq!(-2 ** 31, -2147483648)

@test
def binaryExponentiateTest05(): Bool = assertEq!(2i8 ** 0i8, 1i8)

@test
def binaryExponentiateTest06(): Bool = assertEq!(-2i8 ** 1i8, -2i8)

@test
def binaryExponentiateTest07(): Bool = assertEq!(2i8 ** 2i8, 4i8)

@test
def binaryExponentiateTest08(): Bool = assertEq!(-2i8 ** 7i8, -128i8)

@test
def binaryExponentiateTest09(): Bool = assertEq!(2i16 ** 0i16, 1i16)

@test
def binaryExponentiateTest10(): Bool = assertEq!(-2i16 ** 1i16, -2i16)

@test
def binaryExponentiateTest11(): Bool = assertEq!(2i16 ** 2i16,4i16)

@test
def binaryExponentiateTest12(): Bool = assertEq!(-2i16 ** 15i16, -32768i16)

@test
def binaryExponentiateTest13(): Bool = assertEq!(2i32 ** 0i32, 1i32)

@test
def binaryExponentiateTest14(): Bool = assertEq!(-2i32 ** 1i32, -2i32)

@test
def binaryExponentiateTest15(): Bool = assertEq!(2i32 ** 2i32, 4i32)

@test
def binaryExponentiateTest16(): Bool = assertEq!(-2i32 ** 31i32, -2147483648)

@test
def binaryExponentiateTest17(): Bool = assertEq!(2i64 ** 0i64, 1i64)

@test
def binaryExponentiateTest18(): Bool = assertEq!(-2i64 ** 1i64, -2i64)

@test
def binaryExponentiateTest19(): Bool = assertEq!(2i64 ** 2i64, 4i64)

@test
def binaryExponentiateTest20(): Bool = assertEq!(-2i64 ** 63i64, -9223372036854775808i64)

@test
def binaryExponentiateTest21(): Bool = assertEq!(2.0 ** 0.0, 1.0)

@test
def binaryExponentiateTest22(): Bool = assertEq!(-2.0 ** -1.0, -0.5)

@test
def binaryExponentiateTest23(): Bool = assertEq!(0.01 ** 0.5, 0.1)

@test
def binaryExponentiateTest24(): Bool = assertEq!(-2.0 ** 100.0, 1267650600228229400000000000000.0)

@test
def binaryExponentiateTest25(): Bool = assertEq!(2.0f32 ** 0.0f32, 1.0f32)

@test
def binaryExponentiateTest26(): Bool = assertEq!(-2.0f32 ** -1.0f32, -0.5f32)

@test
def binaryExponentiateTest27(): Bool = assertEq!(0.01f32 ** 0.5f32, 0.1f32)

@test
def binaryExponentiateTest28(): Bool = assertEq!(-2.0f32 ** 100.0f32, 1267650600000000000000000000000.0f32)

@test
def binaryExponentiateTest29(): Bool = assertEq!(2.0f64 ** 0.0f64, 1.0f64)

@test
def binaryExponentiateTest30(): Bool = assertEq!(-2.0f64 ** -1.0f64, -0.5f64)

@test
def binaryExponentiateTest31(): Bool = assertEq!(0.01f64 ** 0.5f64, 0.1f64)

@test
def binaryExponentiateTest32(): Bool = assertEq!(-2.0f64 ** 100.0f64, 1267650600228229400000000000000.0f64)



