/////////////////////////////////////////////////////////////////////////////
// Expression.Binary (Arithmetic)                                          //
// BinaryOperator.{Plus,Minus,Times,Divide,Modulo,Exponentiate}            //
/////////////////////////////////////////////////////////////////////////////

@test
def testBinaryPlus01(): Bool = assertEq!(2147483647 + 1, -2147483648)

@test
def testBinaryPlus02(): Bool = assertEq!(100000 + 400000, 500000)

@test
def testBinaryPlus03(): Bool = assertEq!(-400000 + 100000, -300000)

@test
def testBinaryPlus04(): Bool = assertEq!(-100000 + 400000, 300000)

@test
def testBinaryPlus05(): Bool = assertEq!(-2147483648 + -1, 2147483647)

@test
def testBinaryPlus06(): Bool = assertEq!(127i8 + 1i8, -128i8)

@test
def testBinaryPlus07(): Bool = assertEq!(10i8 + 40i8, 50i8)

@test
def testBinaryPlus08(): Bool = assertEq!(-40i8 + 10i8, -30i8)

@test
def testBinaryPlus09(): Bool = assertEq!(-10i8 + 40i8, 30i8)

@test
def testBinaryPlus10(): Bool = assertEq!(-128i8 + -1i8, 127i8)

@test
def testBinaryPlus11(): Bool = assertEq!(32767i16 + 1i16, -32768i16)

@test
def testBinaryPlus12(): Bool = assertEq!(1000i16 + 4000i16, 5000i16)

@test
def testBinaryPlus13(): Bool = assertEq!(-4000i16 + 1000i16, -3000i16)

@test
def testBinaryPlus14(): Bool = assertEq!(-1000i16 + 4000i16, 3000i16)

@test
def testBinaryPlus15(): Bool = assertEq!(-32768i16 + -1i16, 32767i16)

@test
def testBinaryPlus16(): Bool = assertEq!(2147483647i32 + 1i32, -2147483648i32)

@test
def testBinaryPlus17(): Bool = assertEq!(100000i32 + 400000i32, 500000i32)

@test
def testBinaryPlus18(): Bool = assertEq!(-400000i32 + 100000i32, -300000i32)

@test
def testBinaryPlus19(): Bool = assertEq!(-100000i32 + 400000i32, 300000i32)

@test
def testBinaryPlus20(): Bool = assertEq!(-2147483648i32 + -1i32, 2147483647i32)

@test
def testBinaryPlus21(): Bool = assertEq!(9223372036854775807i64 + 1i64, -9223372036854775808i64)

@test
def testBinaryPlus22(): Bool = assertEq!(10000000000i64 + 40000000000i64, 50000000000i64)

@test
def testBinaryPlus23(): Bool = assertEq!(-40000000000i64 + 10000000000i64, -30000000000i64)

@test
def testBinaryPlus24(): Bool = assertEq!(-10000000000i64 + 40000000000i64, 30000000000i64)

@test
def testBinaryPlus25(): Bool = assertEq!(-9223372036854775808i64 + -1i64, 9223372036854775807i64)


@test
def testBinaryPlus26(): Bool = assertEq!(12.34 + 56.78, 69.12)

@test
def testBinaryPlus27(): Bool = assertEq!(1234567890000000000000000000000000000000000000000.987654321 + 222.222,
                                         1234567890000000000000000000000000000000000000223.209654321)

@test
def testBinaryPlus28(): Bool = assertEq!(-1234567890000000000000000000000000000000000000000.987654321 + 0.0,
                                         -1234567890000000000000000000000000000000000000000.987654321)

@test
def testBinaryPlus29(): Bool = assertEq!(0.0000000000000000000000000000000000000000987654321 + 0.222, 0.222)

@test
def testBinaryPlus30(): Bool = assertEq!(-0.0000000000000000000000000000000000000000987654321 + 0.222, 0.222)

@test
def testBinaryPlus31(): Bool = assertEq!(12.34f32 + 56.78f32, 69.119995f32)

@test
def testBinaryPlus32(): Bool = assertEq!(1234567890000000000000000000000000000000000000000.987654321f32 + 222.222f32,
                                         1234567890000000000000000000000000000000000000223.209654321f32)

@test
def testBinaryPlus33(): Bool = assertEq!(-1234567890000000000000000000000000000000000000000.987654321f32 + 0.0f32,
                                         -1234567890000000000000000000000000000000000000000.987654321f32)

@test
def testBinaryPlus34(): Bool = assertEq!(0.0000000000000000000000000000000000000000987654321f32 + 0.222f32, 0.222f32)

@test
def testBinaryPlus35(): Bool = assertEq!(-0.0000000000000000000000000000000000000000987654321f32 + 0.222f32, 0.222f32)

@test
def testBinaryPlus36(): Bool = assertEq!(12.34f32 + 56.78f32, 69.119995f32)

@test
def testBinaryPlus37(): Bool = assertEq!(1234567890000000000000000000000000000000000000000.987654321f64 + 222.222f64,
                                         1234567890000000000000000000000000000000000000223.209654321f64)

@test
def testBinaryPlus38(): Bool = assertEq!(-1234567890000000000000000000000000000000000000000.987654321f64 + 0.0f64,
                                         -1234567890000000000000000000000000000000000000000.987654321f64)

@test
def testBinaryPlus39(): Bool = assertEq!(0.0000000000000000000000000000000000000000987654321f64 + 0.222f64, 0.222f64)

@test
def testBinaryPlus40(): Bool = assertEq!(-0.0000000000000000000000000000000000000000987654321f64 + 0.222f64, 0.222f64)

@test
def testBinaryPlus41(): Bool = assertEq!(9223372036854775808ii + 1ii, 9223372036854775809ii)

@test
def testBinaryPlus42(): Bool = assertEq!(10000000000000000000ii + 40000000000000000000ii, 50000000000000000000ii)

@test
def testBinaryPlus43(): Bool = assertEq!(-40000000000000000000ii + 10000000000000000000ii, -30000000000000000000ii)

@test
def testBinaryPlus44(): Bool = assertEq!(-10000000000000000000ii + 40000000000000000000ii, 30000000000000000000ii)

@test
def testBinaryPlus45(): Bool = assertEq!(-9223372036854775809ii + -1ii, -9223372036854775810ii)

@test
def testBinaryMinus01(): Bool = assertEq!(-2147483648 - 1, 2147483647)

@test
def testBinaryMinus02(): Bool = assertEq!(400000 - 100000, 300000)

@test
def testBinaryMinus03(): Bool = assertEq!(-400000 - 100000, -500000)

@test
def testBinaryMinus04(): Bool = assertEq!(-100000 - 400000, -500000)

@test
def testBinaryMinus05(): Bool = assertEq!(2147483647 - -1, -2147483648)

@test
def testBinaryMinus06(): Bool = assertEq!(-128i8 - 1i8, 127i8)

@test
def testBinaryMinus07(): Bool = assertEq!(40i8 - 10i8, 30i8)

@test
def testBinaryMinus08(): Bool = assertEq!(-40i8 - 10i8, -50i8)

@test
def testBinaryMinus09(): Bool = assertEq!(-10i8 - 40i8, -50i8)

@test
def testBinaryMinus10(): Bool = assertEq!(-128i8 - 1i8, 127i8)

@test
def testBinaryMinus11(): Bool = assertEq!(-32768i16 - 1i16, 32767i16)

@test
def testBinaryMinus12(): Bool = assertEq!(4000i16 - 1000i16, 3000i16)

@test
def testBinaryMinus13(): Bool = assertEq!(-4000i16 - 1000i16, -5000i16)

@test
def testBinaryMinus14(): Bool = assertEq!(-1000i16 - 4000i16, -5000i16)

@test
def testBinaryMinus15(): Bool = assertEq!(32767i16 - -1i16, -32768i16)

@test
def testBinaryMinus16(): Bool = assertEq!(-2147483648i32 - 1i32, 2147483647i32)

@test
def testBinaryMinus17(): Bool = assertEq!(400000i32 - 100000i32, 300000i32)

@test
def testBinaryMinus18(): Bool = assertEq!(-400000i32 - 100000i32, -500000i32)

@test
def testBinaryMinus19(): Bool = assertEq!(-100000i32 - 400000i32, -500000i32)

@test
def testBinaryMinus20(): Bool = assertEq!(2147483647i32 - -1i32, -2147483648i32)

@test
def testBinaryMinus21(): Bool = assertEq!(-9223372036854775808i64 - 1i64, 9223372036854775807i64)

@test
def testBinaryMinus22(): Bool = assertEq!(40000000000i64 - 10000000000i64, 30000000000i64)

@test
def testBinaryMinus23(): Bool = assertEq!(-40000000000i64 - 10000000000i64, -50000000000i64)

@test
def testBinaryMinus24(): Bool = assertEq!(-10000000000i64 - 40000000000i64, -50000000000i64)

@test
def testBinaryMinus25(): Bool = assertEq!(9223372036854775807i64 - -1i64, -9223372036854775808i64)

@test
def testBinaryMinus26(): Bool = assertEq!(12.34 - 56.78, -44.44)

@test
def testBinaryMinus27(): Bool = assertEq!(1234567890000000000000000000000000000000000000000.987654321 - 222.222,
                                         1234567890000000000000000000000000000000000000000.0)

@test
def testBinaryMinus28(): Bool = assertEq!(-1234567890000000000000000000000000000000000000000.987654321 - 0.0,
                                         -1234567890000000000000000000000000000000000000000.0)

@test
def testBinaryMinus29(): Bool = assertEq!(0.0000000000000000000000000000000000000000987654321 - 0.222, -0.222)

@test
def testBinaryMinus30(): Bool = assertEq!(-0.0000000000000000000000000000000000000000987654321 - 0.222, -0.222)

@test
def testBinaryMinus31(): Bool = assertEq!(12.34f32 - 56.78f32, -44.44f32)

@test
def testBinaryMinus32(): Bool = assertEq!(123456789000000000000000000000000000000.987654321f32 - 222.222f32,
                                         123456789000000000000000000000000000000.0f32)

@test
def testBinaryMinus33(): Bool = assertEq!(-123456789000000000000000000000000000000.987654321f32 - 0.0f32,
                                         -123456789000000000000000000000000000000.0f32)

@test
def testBinaryMinus34(): Bool = assertEq!(0.000000000000000000000000000000987654321f32 - 0.222f32, -0.222f32)

@test
def testBinaryMinus35(): Bool = assertEq!(-0.000000000000000000000000000000987654321f32 - 0.222f32, -0.222f32)

@test
def testBinaryMinus36(): Bool = assertEq!(12.34f64 - 56.78f64, -44.44f64)

@test
def testBinaryMinus37(): Bool = assertEq!(123456789000000000000000000000000000000.987654321f64 - 222.222f64,
                                         123456789000000000000000000000000000000.0f64)

@test
def testBinaryMinus38(): Bool = assertEq!(-123456789000000000000000000000000000000.987654321f64 - 0.0f64,
                                         -123456789000000000000000000000000000000.0f64)

@test
def testBinaryMinus39(): Bool = assertEq!(0.000000000000000000000000000000987654321f64 - 0.222f64, -0.222f64)

@test
def testBinaryMinus40(): Bool = assertEq!(-0.000000000000000000000000000000987654321f64 - 0.222f64, -0.222f64)

@test
def testBinaryMinus41(): Bool = assertEq!(-9223372036854775809ii - 1ii, -9223372036854775810ii)

@test
def testBinaryMinus42(): Bool = assertEq!(40000000000000000000ii - 10000000000000000000ii, 30000000000000000000ii)

@test
def testBinaryMinus43(): Bool = assertEq!(-40000000000000000000ii - 10000000000000000000ii, -50000000000000000000ii)

@test
def testBinaryMinus44(): Bool = assertEq!(-10000000000000000000ii - 40000000000000000000ii, -50000000000000000000ii)

@test
def testBinaryMinus45(): Bool = assertEq!(9223372036854775808ii - -1ii, 9223372036854775809ii)

@test
def testBinaryTimes01(): Bool = assertEq!(2147483647 * 2, -2)

@test
def testBinaryTimes02(): Bool = assertEq!(300 * 200, 60000)

@test
def testBinaryTimes03(): Bool = assertEq!(-200 * 300, -60000)

@test
def testBinaryTimes04(): Bool = assertEq!(-200 * -300, 60000)

@test
def testBinaryTimes05(): Bool = assertEq!(-2147483648 * -1, -2147483648)

@test
def testBinaryTimes06(): Bool = assertEq!(127i8 * 2i8, -2i8)

@test
def testBinaryTimes07(): Bool = assertEq!(3i8 * 2i8, 6i8)

@test
def testBinaryTimes08(): Bool = assertEq!(-2i8 * 3i8, -6i8)

@test
def testBinaryTimes09(): Bool = assertEq!(-2i8 * -3i8, 6i8)

@test
def testBinaryTimes10(): Bool = assertEq!(-128i8 * -1i8, -128i8)

@test
def testBinaryTimes11(): Bool = assertEq!(32767i16 * 2i16, -2i16)

@test
def testBinaryTimes12(): Bool = assertEq!(30i16 * 20i16, 600i16)

@test
def testBinaryTimes13(): Bool = assertEq!(-20i16 * 30i16, -600i16)

@test
def testBinaryTimes14(): Bool = assertEq!(-20i16 * -30i16, 600i16)

@test
def testBinaryTimes15(): Bool = assertEq!(-32768i16 * -1i16, -32768i16)

@test
def testBinaryTimes16(): Bool = assertEq!(2147483647i32 * 2i32, -2i32)

@test
def testBinaryTimes17(): Bool = assertEq!(300i32 * 200i32, 60000i32)

@test
def testBinaryTimes18(): Bool = assertEq!(-200i32 * 300i32, -60000i32)

@test
def testBinaryTimes19(): Bool = assertEq!(-200i32 * -300i32, 60000i32)

@test
def testBinaryTimes20(): Bool = assertEq!(-2147483648i32 * -1i32, -2147483648i32)

@test
def testBinaryTimes21(): Bool = assertEq!(9223372036854775807i64 * 2i64, -2i64)

@test
def testBinaryTimes22(): Bool = assertEq!(300000i64 * 200000i64, 60000000000i64)

@test
def testBinaryTimes23(): Bool = assertEq!(-200000i64 * 300000i64, -60000000000i64)

@test
def testBinaryTimes24(): Bool = assertEq!(-200000i64 * -300000i64, 60000000000i64)

@test
def testBinaryTimes25(): Bool = assertEq!(-9223372036854775808i64 * -1i64, -9223372036854775808i64)

@test
def testBinaryTimes26(): Bool = assertEq!(12.34 * 56.78, 700.6652)

@test
def testBinaryTimes27(): Bool = assertEq!(1234567890000000000000000000000000000000000000000.987654321 * 222.222,
                                          274348145651580030000000000000000000000000000000000.0)

@test
def testBinaryTimes28(): Bool = assertEq!(-1234567890000000000000000000000000000000000000000.987654321 * 222.222,
                                          -274348145651580030000000000000000000000000000000000.0)

@test
def testBinaryTimes29(): Bool = assertEq!(0.0000000000000000000000000000000000000000987654321 * 0.222,
                                          0.0000000000000000000000000000000000000000219259259262)

@test
def testBinaryTimes30(): Bool = assertEq!(-0.0000000000000000000000000000000000000000987654321 * 0.222,
                                          -0.0000000000000000000000000000000000000000219259259262)

@test
def testBinaryTimes31(): Bool = assertEq!(12.34f32 * 56.78f32, 700.6652f32)

@test
def testBinaryTimes32(): Bool = assertEq!(1234567890000000000000000000000000000000000000000.987654321f32 * 222.222f32,
                                          274348145651580030000000000000000000000000000000000.0f32)

@test
def testBinaryTimes33(): Bool = assertEq!(-1234567890000000000000000000000000000000000000000.987654321f32 * 222.222f32,
                                          -274348145651580030000000000000000000000000000000000.0f32)

@test
def testBinaryTimes34(): Bool = assertEq!(0.0000000000000000000000000000000000000000987654321f32 * 0.222f32,
                                          0.0000000000000000000000000000000000000000219259259262f32)

@test
def testBinaryTimes35(): Bool = assertEq!(-0.0000000000000000000000000000000000000000987654321f32 * 0.222f32,
                                          -0.0000000000000000000000000000000000000000219259259262f32)

@test
def testBinaryTimes36(): Bool = assertEq!(12.34f64 * 56.78f64, 700.6652f64)

@test
def testBinaryTimes37(): Bool = assertEq!(1234567890000000000000000000000000000000000000000.987654321f64 * 222.222f64,
                                          274348145651580030000000000000000000000000000000000.0f64)

@test
def testBinaryTimes38(): Bool = assertEq!(-1234567890000000000000000000000000000000000000000.987654321f64 * 222.222f64,
                                          -274348145651580030000000000000000000000000000000000.0f64)

@test
def testBinaryTimes39(): Bool = assertEq!(0.0000000000000000000000000000000000000000987654321f64 * 0.222f64,
                                          0.0000000000000000000000000000000000000000219259259262f64)

@test
def testBinaryTimes40(): Bool = assertEq!(-0.0000000000000000000000000000000000000000987654321f64 * 0.222f64,
                                          -0.0000000000000000000000000000000000000000219259259262f64)

@test
def testBinaryTimes41(): Bool = assertEq!(9223372036854775808ii * 2ii, 18446744073709551616ii)

@test
def testBinaryTimes42(): Bool = assertEq!(30000000000ii * 20000000000ii, 600000000000000000000ii)

@test
def testBinaryTimes43(): Bool = assertEq!(-20000000000ii * 30000000000ii, -600000000000000000000ii)

@test
def testBinaryTimes44(): Bool = assertEq!(-20000000000ii * -30000000000ii, 600000000000000000000ii)

@test
def testBinaryTimes45(): Bool = assertEq!(-9223372036854775809ii * -1ii, 9223372036854775809ii)

@test
def testBinaryDivide01(): Bool = assertEq!(2147483647 / 1, 2147483647)

@test
def testBinaryDivide02(): Bool = assertEq!(1200000 / 3, 400000)

@test
def testBinaryDivide03(): Bool = assertEq!(-1200000 / 3, -400000)

@test
def testBinaryDivide04(): Bool = assertEq!(-3 / 1200000, 0)

@test
def testBinaryDivide05(): Bool = assertEq!(-2147483648 / -1, -2147483648)

@test
def testBinaryDivide06(): Bool = assertEq!(127i8 / 1i8, 127i8)

@test
def testBinaryDivide07(): Bool = assertEq!(12i8 / 3i8, 4i8)

@test
def testBinaryDivide08(): Bool = assertEq!(-12i8 / 3i8, -4i8)

@test
def testBinaryDivide09(): Bool = assertEq!(-3i8 / 12i8, 0i8)

@test
def testBinaryDivide10(): Bool = assertEq!(-128i8 / -1i8, -128i8)

@test
def testBinaryDivide11(): Bool = assertEq!(32767i16 / 1i16, 32767i16)

@test
def testBinaryDivide12(): Bool = assertEq!(12000i16 / 3i16, 4000i16)

@test
def testBinaryDivide13(): Bool = assertEq!(-12000i16 / 3i16, -4000i16)

@test
def testBinaryDivide14(): Bool = assertEq!(-3i16 / 12000i16, 0i16)

@test
def testBinaryDivide15(): Bool = assertEq!(-32768i16 / -1i16, -32768i16)

@test
def testBinaryDivide16(): Bool = assertEq!(2147483647i32 / 1i32, 2147483647i32)

@test
def testBinaryDivide17(): Bool = assertEq!(1200000i32 / 3i32, 400000i32)

@test
def testBinaryDivide18(): Bool = assertEq!(-1200000i32 / 3i32, -400000i32)

@test
def testBinaryDivide19(): Bool = assertEq!(-3i32 / 1200000i32, 0i32)

@test
def testBinaryDivide20(): Bool = assertEq!(-2147483648i32 / -1i32, -2147483648i32)

@test
def testBinaryDivide21(): Bool = assertEq!(9223372036854775807i64 / 1i64, 9223372036854775807i64)

@test
def testBinaryDivide22(): Bool = assertEq!(120000000000i64 / 3i64, 40000000000i64)

@test
def testBinaryDivide23(): Bool = assertEq!(-120000000000i64 / 3i64, -40000000000i64)

@test
def testBinaryDivide24(): Bool = assertEq!(-3i64 / 120000000000i64, 0i64)

@test
def testBinaryDivide25(): Bool = assertEq!(-9223372036854775808i64 / -1i64, -9223372036854775808i64)

@test
def testBinaryDivide26(): Bool = assertEq!(12.34 / 56.78, 0.2173300457907714)

@test
def testBinaryDivide27(): Bool = assertEq!(1234567890000000000000000000000000000000000000000.987654321 / 222.222,
                                           5555561060561060400000000000000000000000000000.0)

@test
def testBinaryDivide28(): Bool = assertEq!(-1234567890000000000000000000000000000000000000000.987654321 / 222.222,
                                           -5555561060561060400000000000000000000000000000.0)

@test
def testBinaryDivide29(): Bool = assertEq!(0.0000000000000000000000000000000000000000987654321 / 0.222,
                                           0.00000000000000000000000000000000000000044488933378378374)

@test
def testBinaryDivide30(): Bool = assertEq!(-0.0000000000000000000000000000000000000000987654321 / 0.222,
                                           -0.00000000000000000000000000000000000000044488933378378374)

@test
def testBinaryDivide31(): Bool = assertEq!(12.34f32 / 56.78f32, 0.21733005f32)

@test
def testBinaryDivide32(): Bool = assertEq!(123456789000000000000000000000000000000.987654321f32 / 222.222f32,
                                           555556080000000000000000000000000000.0f32)
@test
def testBinaryDivide33(): Bool = assertEq!(-123456789000000000000000000000000000000.987654321f32 / 222.222f32,
                                           -555556080000000000000000000000000000.0f32)

@test
def testBinaryDivide34(): Bool = assertEq!(0.000000000000000000000000000000987654321f32 / 0.222f32,
                                           0.0000000000000000000000000000044488933f32)
@test
def testBinaryDivide35(): Bool = assertEq!(-0.000000000000000000000000000000987654321f32 / 0.222f32,
                                           -0.0000000000000000000000000000044488933f32)

@test
def testBinaryDivide36(): Bool = assertEq!(12.34f64 / 56.78f64, 0.2173300457907714f64)

@test
def testBinaryDivide37(): Bool = assertEq!(1234567890000000000000000000000000000000000000000.987654321f64 / 222.222f64,
                                           5555561060561060400000000000000000000000000000.0f64)

@test
def testBinaryDivide38(): Bool = assertEq!(-1234567890000000000000000000000000000000000000000.987654321f64 / 222.222f64,
                                           -5555561060561060400000000000000000000000000000.0f64)

@test
def testBinaryDivide39(): Bool = assertEq!(0.0000000000000000000000000000000000000000987654321f64 / 0.222f64,
                                           0.00000000000000000000000000000000000000044488933378378374f64)

@test
def testBinaryDivide40(): Bool = assertEq!(-0.0000000000000000000000000000000000000000987654321f64 / 0.222f64,
                                           -0.00000000000000000000000000000000000000044488933378378374f64)

@test
def testBinaryDivide41(): Bool = assertEq!(9223372036854775808ii / 1ii, 9223372036854775808ii)

@test
def testBinaryDivide42(): Bool = assertEq!(1200000000000000000000ii / 3ii, 400000000000000000000ii)

@test
def testBinaryDivide43(): Bool = assertEq!(-1200000000000000000000ii / 3ii, -400000000000000000000ii)

@test
def testBinaryDivide44(): Bool = assertEq!(-3ii / 1200000000000000000000ii, 0ii)

@test
def testBinaryDivide45(): Bool = assertEq!(-9223372036854775809ii / -1ii, 9223372036854775809ii)

@test
def testBinaryModulo01(): Bool = assertEq!(1200000 % 200000, 0)

@test
def testBinaryModulo02(): Bool = assertEq!(1200000 % 500000, 200000)

@test
def testBinaryModulo03(): Bool = assertEq!(-1200000 % 500000, -200000)

@test
def testBinaryModulo04(): Bool = assertEq!(1200000 % -500000, 200000)

@test
def testBinaryModulo05(): Bool = assertEq!(-1200000 % -500000, -200000)

@test
def testBinaryModulo06(): Bool = assertEq!(12i8 % 2i8, 0i8)

@test
def testBinaryModulo07(): Bool = assertEq!(12i8 % 5i8, 2i8)

@test
def testBinaryModulo08(): Bool = assertEq!(-12i8 % 5i8, -2i8)

@test
def testBinaryModulo09(): Bool = assertEq!(12i8 % -5i8, 2i8)

@test
def testBinaryModulo10(): Bool = assertEq!(-12i8 % -5i8, -2i8)

@test
def testBinaryModulo11(): Bool = assertEq!(12000i16 % 2000i16, 0i16)

@test
def testBinaryModulo12(): Bool = assertEq!(12000i16 % 5000i16, 2000i16)

@test
def testBinaryModulo13(): Bool = assertEq!(-12000i16 % 5000i16, -2000i16)

@test
def testBinaryModulo14(): Bool = assertEq!(12000i16 % -5000i16, 2000i16)

@test
def testBinaryModulo15(): Bool = assertEq!(-12000i16 % -5000i16, -2000i16)

@test
def testBinaryModulo16(): Bool = assertEq!(1200000i32 % 200000i32, 0i32)

@test
def testBinaryModulo17(): Bool = assertEq!(1200000i32 % 500000i32, 200000i32)

@test
def testBinaryModulo18(): Bool = assertEq!(-1200000i32 % 500000i32, -200000i32)

@test
def testBinaryModulo19(): Bool = assertEq!(1200000i32 % -500000i32, 200000i32)

@test
def testBinaryModulo20(): Bool = assertEq!(-1200000i32 % -500000i32, -200000i32)

@test
def testBinaryModulo21(): Bool = assertEq!(120000000000i64 % 20000000000i64, 0i64)

@test
def testBinaryModulo22(): Bool = assertEq!(120000000000i64 % 50000000000i64, 20000000000i64)

@test
def testBinaryModulo23(): Bool = assertEq!(-120000000000i64 % 50000000000i64, -20000000000i64)

@test
def testBinaryModulo24(): Bool = assertEq!(120000000000i64 % -50000000000i64, 20000000000i64)

@test
def testBinaryModulo25(): Bool = assertEq!(-120000000000i64 % -50000000000i64, -20000000000i64)

@test
def testBinaryModulo26(): Bool = assertEq!(12.34 % 56.78, 12.34)

@test
def testBinaryModulo27(): Bool = assertEq!(1234567890000000000000000000000000000000000000000.987654321 % 222.222,
                                           88.53722751835619)

@test
def testBinaryModulo28(): Bool = assertEq!(-1234567890000000000000000000000000000000000000000.987654321 % 222.222,
                                           -88.53722751835619)

@test
def testBinaryModulo29(): Bool = assertEq!(0.0000000000000000000000000000000000000000987654321 % 0.222,
                                           0.0000000000000000000000000000000000000000987654321)

@test
def testBinaryModulo30(): Bool = assertEq!(-0.0000000000000000000000000000000000000000987654321 % 0.222,
                                           -0.0000000000000000000000000000000000000000987654321)

@test
def testBinaryModulo31(): Bool = assertEq!(12.34f32 % 56.78f32, 12.34f32)

@test
def testBinaryModulo32(): Bool = assertEq!(123456789000000000000000000000000000000.987654321f32 % 222.222f32,
                                           29.297333f32)

@test
def testBinaryModulo33(): Bool = assertEq!(-123456789000000000000000000000000000000.987654321f32 % 222.222f32,
                                           -29.297333f32)

@test
def testBinaryModulo34(): Bool = assertEq!(0.000000000000000000000000000000987654321f32 % 0.222f32,
                                           0.000000000000000000000000000000987654321f32)

@test
def testBinaryModulo35(): Bool = assertEq!(-0.00000000000000000000000000000987654321f32 % 0.222f32,
                                           -0.00000000000000000000000000000987654321f32)

@test
def testBinaryModulo36(): Bool = assertEq!(12.34f64 % 56.78f64, 12.34f64)

@test
def testBinaryModulo37(): Bool = assertEq!(1234567890000000000000000000000000000000000000000.987654321f64 % 222.222f64,
                                           88.53722751835619f64)

@test
def testBinaryModulo38(): Bool = assertEq!(-1234567890000000000000000000000000000000000000000.987654321f64 % 222.222f64,
                                           -88.53722751835619f64)

@test
def testBinaryModulo39(): Bool = assertEq!(0.0000000000000000000000000000000000000000987654321f64 % 0.222f64,
                                           0.0000000000000000000000000000000000000000987654321f64)

@test
def testBinaryModulo40(): Bool = assertEq!(-0.0000000000000000000000000000000000000000987654321f64 % 0.222f64,
                                           -0.0000000000000000000000000000000000000000987654321f64)

@test
def testBinaryModulo41(): Bool = assertEq!(1200000000000000000000ii % 200000000000000000000ii, 0ii)

@test
def testBinaryModulo42(): Bool = assertEq!(1200000000000000000000ii % 500000000000000000000ii, 200000000000000000000ii)

@test
def testBinaryModulo43(): Bool = assertEq!(-1200000000000000000000ii % 500000000000000000000ii, -200000000000000000000ii)

@test
def testBinaryModulo44(): Bool = assertEq!(1200000000000000000000ii % -500000000000000000000ii, 200000000000000000000ii)

@test
def testBinaryModulo45(): Bool = assertEq!(-1200000000000000000000ii % -500000000000000000000ii, -200000000000000000000ii)

@test
def testBinaryExponentiate01(): Bool = assertEq!(2 ** 0, 1)

@test
def testBinaryExponentiate02(): Bool = assertEq!(-2 ** 1, -2)

@test
def testBinaryExponentiate03(): Bool = assertEq!(2 ** 2, 4)

@test
def testBinaryExponentiate04(): Bool = assertEq!(-2 ** 31, -2147483648)

@test
def testBinaryExponentiate05(): Bool = assertEq!(2i8 ** 0i8, 1i8)

@test
def testBinaryExponentiate06(): Bool = assertEq!(-2i8 ** 1i8, -2i8)

@test
def testBinaryExponentiate07(): Bool = assertEq!(2i8 ** 2i8, 4i8)

@test
def testBinaryExponentiate08(): Bool = assertEq!(-2i8 ** 7i8, -128i8)

@test
def testBinaryExponentiate09(): Bool = assertEq!(2i16 ** 0i16, 1i16)

@test
def testBinaryExponentiate10(): Bool = assertEq!(-2i16 ** 1i16, -2i16)

@test
def testBinaryExponentiate11(): Bool = assertEq!(2i16 ** 2i16,4i16)

@test
def testBinaryExponentiate12(): Bool = assertEq!(-2i16 ** 15i16, -32768i16)

@test
def testBinaryExponentiate13(): Bool = assertEq!(2i32 ** 0i32, 1i32)

@test
def testBinaryExponentiate14(): Bool = assertEq!(-2i32 ** 1i32, -2i32)

@test
def testBinaryExponentiate15(): Bool = assertEq!(2i32 ** 2i32, 4i32)

@test
def testBinaryExponentiate16(): Bool = assertEq!(-2i32 ** 31i32, -2147483648)

@test
def testBinaryExponentiate17(): Bool = assertEq!(2i64 ** 0i64, 1i64)

@test
def testBinaryExponentiate18(): Bool = assertEq!(-2i64 ** 1i64, -2i64)

@test
def testBinaryExponentiate19(): Bool = assertEq!(2i64 ** 2i64, 4i64)

@test
def testBinaryExponentiate20(): Bool = assertEq!(-2i64 ** 63i64, -9223372036854775808i64)

@test
def testBinaryExponentiate21(): Bool = assertEq!(2.0 ** 0.0, 1.0)

@test
def testBinaryExponentiate22(): Bool = assertEq!(-2.0 ** -1.0, -0.5)

@test
def testBinaryExponentiate23(): Bool = assertEq!(0.01 ** 0.5, 0.1)

@test
def testBinaryExponentiate24(): Bool = assertEq!(-2.0 ** 100.0, 1267650600228229400000000000000.0)

@test
def testBinaryExponentiate25(): Bool = assertEq!(2.0f32 ** 0.0f32, 1.0f32)

@test
def testBinaryExponentiate26(): Bool = assertEq!(-2.0f32 ** -1.0f32, -0.5f32)

@test
def testBinaryExponentiate27(): Bool = assertEq!(0.01f32 ** 0.5f32, 0.1f32)

@test
def testBinaryExponentiate28(): Bool = assertEq!(-2.0f32 ** 100.0f32, 1267650600000000000000000000000.0f32)

@test
def testBinaryExponentiate29(): Bool = assertEq!(2.0f64 ** 0.0f64, 1.0f64)

@test
def testBinaryExponentiate30(): Bool = assertEq!(-2.0f64 ** -1.0f64, -0.5f64)

@test
def testBinaryExponentiate31(): Bool = assertEq!(0.01f64 ** 0.5f64, 0.1f64)

@test
def testBinaryExponentiate32(): Bool = assertEq!(-2.0f64 ** 100.0f64, 1267650600228229400000000000000.0f64)



