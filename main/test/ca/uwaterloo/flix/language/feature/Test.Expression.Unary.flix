/////////////////////////////////////////////////////////////////////////////
// Expression.Unary                                                        //
// UnaryOperator.{LogicalNot,Plus,Minus,BitwiseNegate}                     //
/////////////////////////////////////////////////////////////////////////////

@test
def logicalNotTest01(): Bool = assertEq!(false, !true)

@test
def logicalNotTest02(): Bool = assertEq!(true, !false)

@test
def unaryPlusTest01(): Bool = assertEq!(0, +0)

@test
def unaryPlusTest02(): Bool = assertEq!(+36000, 36000)

@test
def unaryPlusTest03(): Bool = assertEq!(+(-36000), -36000)

@test
def unaryPlusTest04(): Bool = assertEq!(+2147483647, 2147483647)

@test
def unaryPlusTest05(): Bool = assertEq!(-2147483648, +-2147483648)

@test
def unaryPlusTest06(): Bool = assertEq!(+0i8, 0i8)

@test
def unaryPlusTest07(): Bool = assertEq!(+36i8, 36i8)

@test
def unaryPlusTest08(): Bool = assertEq!(+(-36i8), -36i8)

@test
def unaryPlusTest09(): Bool = assertEq!(+127i8, 127i8)

@test
def unaryPlusTest10(): Bool = assertEq!(+-128i8, -128i8)

@test
def unaryPlusTest11(): Bool = assertEq!(+0i16, 0i16)

@test
def unaryPlusTest12(): Bool = assertEq!(+3600i16, 3600i16)

@test
def unaryPlusTest13(): Bool = assertEq!(+(-3600i16), -3600i16)

@test
def unaryPlusTest14(): Bool = assertEq!(+32767i16, 32767i16)

@test
def unaryPlusTest15(): Bool = assertEq!(+-32768i16, -32768i16)

@test
def unaryPlusTest16(): Bool = assertEq!(+0i32, 0i32)

@test
def unaryPlusTest17(): Bool = assertEq!(+36000i32, 36000i32)

@test
def unaryPlusTest18(): Bool = assertEq!(+(-36000i32), -36000i32)

@test
def unaryPlusTest19(): Bool = assertEq!(+2147483647i32, 2147483647i32)

@test
def unaryPlusTest20(): Bool = assertEq!(+-2147483648i32, -2147483648i32)

@test
def unaryPlusTest21(): Bool = assertEq!(+0i64, 0i64)

@test
def unaryPlusTest22(): Bool = assertEq!(+3600000000i64, 3600000000i64)

@test
def unaryPlusTest23(): Bool = assertEq!(+(-3600000000i64), -3600000000i64)

@test
def unaryPlusTest24(): Bool = assertEq!(+9223372036854775807i64, 9223372036854775807i64)

@test
def unaryPlusTest25(): Bool = assertEq!(+-9223372036854775808i64, -9223372036854775808i64)

@test
def unaryPlusTest26(): Bool = assertEq!(+0.0, 0.0)

@test
def unaryPlusTest27(): Bool = assertEq!(+(-0.0), 0.0)

@test
def unaryPlusTest28(): Bool = assertEq!(+(4.2), 4.2)

@test
def unaryPlusTest29(): Bool = assertEq!(+99999999999999999999999999999999999999999999999999999999999999999999999999999999.0,
                                         99999999999999999999999999999999999999999999999999999999999999999999999999999999.0)

@test
def unaryPlusTest30(): Bool = assertEq!(+0.000000000000000000000000000000000000000000000000000000000000000000000000000000001,
                                          0.000000000000000000000000000000000000000000000000000000000000000000000000000000001)

@test
def unaryPlusTest31(): Bool = assertEq!(+(-99999999999999999999999999999999999999999999999999999999999999999999999999999999.0),
                                           -99999999999999999999999999999999999999999999999999999999999999999999999999999999.0)

@test
def unaryPlusTest32(): Bool = assertEq!(+(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000001),
                                           -0.000000000000000000000000000000000000000000000000000000000000000000000000000000001)

@test
def unaryPlusTest33(): Bool = assertEq!(+0.0f32, 0.0f32)

@test
def unaryPlusTest34(): Bool = assertEq!(+(-0.0f32), 0.0f32)

@test
def unaryPlusTest35(): Bool = assertEq!(+(4.2f32), 4.2f32)

@test
def unaryPlusTest36(): Bool = assertEq!(+999999999999999999999999999999.0f32, 999999999999999999999999999999.0f32)

@test
def unaryPlusTest37(): Bool = assertEq!(+0.0000000000000000000000000000001f32, 0.0000000000000000000000000000001f32)

@test
def unaryPlusTest38(): Bool = assertEq!(+(-999999999999999999999999999999.0f32), -999999999999999999999999999999.0f32)

@test
def unaryPlusTest39(): Bool = assertEq!(+(-0.0000000000000000000000000000001f32), -0.0000000000000000000000000000001f32)

@test
def unaryPlusTest40(): Bool = assertEq!(+0.0f64, 0.0f64)

@test
def unaryPlusTest41(): Bool = assertEq!(+(-0.0f64), 0.0f64)

@test
def unaryPlusTest42(): Bool = assertEq!(+(4.2f64), 4.2f64)

@test
def unaryPlusTest43(): Bool = assertEq!(+99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64,
                                         99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64)

@test
def unaryPlusTest44(): Bool = assertEq!(+0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64,
                                         0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64)

@test
def unaryPlusTest45(): Bool = assertEq!(+(-99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64),
                                         -99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64)

@test
def unaryPlusTest46(): Bool = assertEq!(+(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64),
                                         -0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64)

@test
def unaryPlusTest47(): Bool = assertEq!(+0ii, 0ii)

@test
def unaryPlusTest48(): Bool = assertEq!(+1ii, 1ii)

@test
def unaryPlusTest49(): Bool = assertEq!(+10ii, 10ii)

@test
def unaryPlusTest50(): Bool = assertEq!(+9223372036854775808ii, 9223372036854775808ii)

@test
def unaryPlusTest51(): Bool = assertEq!(+(-9223372036854775809ii), -9223372036854775809ii)

@test
def unaryMinusTest01(): Bool = assertEq!(-0, 0)

@test
def unaryMinusTest02(): Int = -36000

@test
def unaryMinusTest03(): Bool = assertEq!(-(-36000), 36000)

@test
def unaryMinusTest04(): Bool = assertEq!(--2147483648, -2147483648)

@test
def unaryMinusTest05(): Int = -2147483648

@test
def unaryMinusTest06(): Int8 = -0i8

@test
def unaryMinusTest07(): Int8 = -36i8

@test
def unaryMinusTest08(): Bool = assertEq!(-(-36i8), 36i8)

@test
def unaryMinusTest09(): Bool = assertEq!(--128i8, -128i8)

@test
def unaryMinusTest10(): Int8 = -127i8

@test
def unaryMinusTest11(): Bool = assertEq!(-0i16, 0i16)

@test
def unaryMinusTest12(): Int16 = -3600i16

@test
def unaryMinusTest13(): Bool = assertEq!(-(-3600i16), 3600i16)

@test
def unaryMinusTest14(): Int16 = -32767i16

@test
def unaryMinusTest15(): Bool = assertEq!(--32768i16, -32768i16)

@test
def unaryMinusTest16(): Bool = assertEq!(-0i32, 0i32)

@test
def unaryMinusTest17(): Int32 = -36000i32

@test
def unaryMinusTest18(): Bool = assertEq!(-(-36000i32), 36000i32)

@test
def unaryMinusTest19(): Int32 = -2147483647i32

@test
def unaryMinusTest20(): Bool = assertEq!(--2147483648i32, -2147483648i32)

@test
def unaryMinusTest21(): Bool = assertEq!(-0i64, 0i64)

@test
def unaryMinusTest22(): Int64 = -3600000000i64

@test
def unaryMinusTest23(): Bool = assertEq!(-(-3600000000i64), 3600000000i64)

@test
def unaryMinusTest24(): Int64 = -9223372036854775807i64

@test
def unaryMinusTest25(): Bool = assertEq!(--9223372036854775808i64, -9223372036854775808i64)

@test
def unaryMinusTest26(): Bool = assertEq!(-0.0, 0.0)

@test
def unaryMinusTest27(): Bool = assertEq!(-(-0.0), 0.0)

@test
def unaryMinusTest28(): Bool = assertEq!(-(4.2), -4.2)

@test
def unaryMinusTest29(): Float = -99999999999999999999999999999999999999999999999999999999999999999999999999999999.0

@test
def unaryMinusTest30(): Float = -0.000000000000000000000000000000000000000000000000000000000000000000000000000000001

@test
def unaryMinusTest31(): Bool = assertEq!(-(-99999999999999999999999999999999999999999999999999999999999999999999999999999999.0),
                                         99999999999999999999999999999999999999999999999999999999999999999999999999999999.0)

@test
def unaryMinusTest32(): Bool = assertEq!(-(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000001),
                                         0.000000000000000000000000000000000000000000000000000000000000000000000000000000001)

@test
def unaryMinusTest33(): Bool = assertEq!(-0.0f32, 0.0f32)

@test
def unaryMinusTest34(): Bool = assertEq!(-(-0.0f32), 0.0f32)

@test
def unaryMinusTest35(): Bool = assertEq!(-(4.2f32), -4.2f32)

@test
def unaryMinusTest36(): Float32 = -999999999999999999999999999999.0f32

@test
def unaryMinusTest37(): Float32 = -0.0000000000000000000000000000001f32

@test
def unaryMinusTest38(): Bool = assertEq!(-(-999999999999999999999999999999.0f32), 999999999999999999999999999999.0f32)

@test
def unaryMinusTest39(): Bool = assertEq!(-(-0.0000000000000000000000000000001f32), 0.0000000000000000000000000000001f32)

@test
def unaryMinusTest40(): Float64 = -0.0f64

@test
def unaryMinusTest41(): Bool = assertEq!(-(-0.0f64), 0.0f64)

@test
def unaryMinusTest42(): Bool = assertEq!(-(4.2f64), -4.2f64)

@test
def unaryMinusTest43(): Float64 = -99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64

@test
def unaryMinusTest44(): Float64 = -0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64

@test
def unaryMinusTest45(): Bool = assertEq!(-(-99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64),
                                         99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64)

@test
def unaryMinusTest46(): Bool = assertEq!(-(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64),
                                         0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64)

@test
def unaryMinusTest47(): Bool = assertEq!(-0ii, 0ii)

@test
def unaryMinusTest48(): BigInt = -1ii

@test
def unaryMinusTest49(): BigInt = -10ii

@test
def unaryMinusTest50(): BigInt = -9223372036854775808ii

@test
def unaryMinusTest51(): Bool = assertEq!(-(-9223372036854775809ii), 9223372036854775809ii)

@test
def unaryBitwiseNegate01(): Bool = assertEq!(~~~0, -1)

@test
def unaryBitwiseNegate02(): Bool = assertEq!(~~~1, -2)

@test
def unaryBitwiseNegate03(): Bool = assertEq!(~~~(-1), 0)

@test
def unaryBitwiseNegate04(): Bool = assertEq!(~~~36000, -36001)

@test
def unaryBitwiseNegate05(): Bool = assertEq!(~~~(-36000), 35999)

@test
def unaryBitwiseNegate06(): Bool = assertEq!(~~~2147483647, -2147483648)

@test
def unaryBitwiseNegate07(): Bool = assertEq!(~~~-2147483648, 2147483647)

@test
def unaryBitwiseNegate08(): Bool = assertEq!(~~~0i8, -1i8)

@test
def unaryBitwiseNegate09(): Bool = assertEq!(~~~1i8, -2i8)

@test
def unaryBitwiseNegate10(): Bool = assertEq!(~~~(-1i8), 0i8)

@test
def unaryBitwiseNegate11(): Bool = assertEq!(~~~42i8, -43i8)

@test
def unaryBitwiseNegate12(): Bool = assertEq!(~~~(-42i8), 41i8)

@test
def unaryBitwiseNegate13(): Bool = assertEq!(~~~127i8, -128i8)

@test
def unaryBitwiseNegate14(): Bool = assertEq!(~~~-128i8, 127i8)

@test
def unaryBitwiseNegate15(): Bool = assertEq!(~~~0i16, -1i16)

@test
def unaryBitwiseNegate16(): Bool = assertEq!(~~~1i16, -2i16)

@test
def unaryBitwiseNegate17(): Bool = assertEq!(~~~(-1i16), 0i16)

@test
def unaryBitwiseNegate18(): Bool = assertEq!(~~~420i16, -421i16)

@test
def unaryBitwiseNegate19(): Bool = assertEq!(~~~(-420i16), 419i16)

@test
def unaryBitwiseNegate20(): Bool = assertEq!(~~~32767i16, -32768i16)

@test
def unaryBitwiseNegate21(): Bool = assertEq!(~~~-32768i16, 32767i16)

@test
def unaryBitwiseNegate22(): Bool = assertEq!(~~~0i32, -1i32)

@test
def unaryBitwiseNegate23(): Bool = assertEq!(~~~1i32, -2i32)

@test
def unaryBitwiseNegate24(): Bool = assertEq!(~~~(-1i32), 0i32)

@test
def unaryBitwiseNegate25(): Bool = assertEq!(~~~36000i32, -36001i32)

@test
def unaryBitwiseNegate26(): Bool = assertEq!(~~~(-36000i32), 35999i32)

@test
def unaryBitwiseNegate27(): Bool = assertEq!(~~~2147483647i32, -2147483648i32)

@test
def unaryBitwiseNegate28(): Bool = assertEq!(~~~-2147483648i32, 2147483647i32)

@test
def unaryBitwiseNegate29(): Bool = assertEq!(~~~0i64, -1i64)

@test
def unaryBitwiseNegate30(): Bool = assertEq!(~~~1i64, -2i64)

@test
def unaryBitwiseNegate31(): Bool = assertEq!(~~~(-1i64), 0i64)

@test
def unaryBitwiseNegate32(): Bool = assertEq!(~~~10000000000i64, -10000000001i64)

@test
def unaryBitwiseNegate33(): Bool = assertEq!(~~~(-10000000000i64), 9999999999i64)

@test
def unaryBitwiseNegate34(): Bool = assertEq!(~~~9223372036854775807i64, -9223372036854775808i64)

@test
def unaryBitwiseNegate35(): Bool = assertEq!(~~~-9223372036854775808i64, 9223372036854775807i64)

@test
def unaryBitwiseNegate36(): Bool = assertEq!(~~~0ii, -1ii)

@test
def unaryBitwiseNegate37(): Bool = assertEq!(~~~1ii, -2ii)

@test
def unaryBitwiseNegate38(): Bool = assertEq!(~~~(-1ii), 0ii)

@test
def unaryBitwiseNegate39(): Bool = assertEq!(~~~9223372036854775808ii, -9223372036854775809ii)

@test
def unaryBitwiseNegate40(): Bool = assertEq!(~~~(-9223372036854775809ii), 9223372036854775808ii)


