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

