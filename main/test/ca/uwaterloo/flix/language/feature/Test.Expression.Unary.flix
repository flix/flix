/////////////////////////////////////////////////////////////////////////////
// Expression.Unary                                                        //
// UnaryOperator.{LogicalNot,Plus,Minus,BitwiseNegate}                     //
/////////////////////////////////////////////////////////////////////////////

@test
def logicalNotTest_01(): Bool = assertEq!(false, !true)

@test
def logicalNotTest_02(): Bool = assertEq!(true, !false)

@test
def unaryPlusTest_01(): Bool = assertEq!(0, +0)

@test
def unaryPlusTest_02(): Bool = assertEq!(+36000, 36000)

@test
def unaryPlusTest_03(): Bool = assertEq!(+(-36000), -36000)

@test
def unaryPlusTest_04(): Bool = assertEq!(+2147483647, 2147483647)

@test
def unaryPlusTest_05(): Bool = assertEq!(-2147483648, +-2147483648)

@test
def unaryPlusTest_06(): Bool = assertEq!(+0i8, 0i8)

@test
def unaryPlusTest_07(): Bool = assertEq!(+36i8, 36i8)

@test
def unaryPlusTest_08(): Bool = assertEq!(+(-36i8), -36i8)

@test
def unaryPlusTest_09(): Bool = assertEq!(+127i8, 127i8)

@test
def unaryPlusTest_10(): Bool = assertEq!(+-128i8, -128i8)

@test
def unaryPlusTest_11(): Bool = assertEq!(+0i16, 0i16)

@test
def unaryPlusTest_12(): Bool = assertEq!(+3600i16, 3600i16)

@test
def unaryPlusTest_13(): Bool = assertEq!(+(-3600i16), -3600i16)

@test
def unaryPlusTest_14(): Bool = assertEq!(+32767i16, 32767i16)

@test
def unaryPlusTest_15(): Bool = assertEq!(+-32768i16, -32768i16)

@test
def unaryPlusTest_16(): Bool = assertEq!(+0i32, 0i32)

@test
def unaryPlusTest_17(): Bool = assertEq!(+36000i32, 36000i32)

@test
def unaryPlusTest_18(): Bool = assertEq!(+(-36000i32), -36000i32)

@test
def unaryPlusTest_19(): Bool = assertEq!(+2147483647i32, 2147483647i32)

@test
def unaryPlusTest_20(): Bool = assertEq!(+-2147483648i32, -2147483648i32)

@test
def unaryPlusTest_21(): Bool = assertEq!(+0i64, 0i64)

@test
def unaryPlusTest_22(): Bool = assertEq!(+3600000000i64, 3600000000i64)

@test
def unaryPlusTest_23(): Bool = assertEq!(+(-3600000000i64), -3600000000i64)

@test
def unaryPlusTest_24(): Bool = assertEq!(+9223372036854775807i64, 9223372036854775807i64)

@test
def unaryPlusTest_25(): Bool = assertEq!(+-9223372036854775808i64, -9223372036854775808i64)

@test
def unaryPlusTest_26(): Bool = assertEq!(+0.0, 0.0)

@test
def unaryPlusTest_27(): Bool = assertEq!(+(-0.0), 0.0)

@test
def unaryPlusTest_28(): Bool = assertEq!(+(4.2), 4.2)

@test
def unaryPlusTest_29(): Bool = assertEq!(+99999999999999999999999999999999999999999999999999999999999999999999999999999999.0,
                                         99999999999999999999999999999999999999999999999999999999999999999999999999999999.0)

@test
def unaryPlusTest_30(): Bool = assertEq!(+0.000000000000000000000000000000000000000000000000000000000000000000000000000000001,
                                          0.000000000000000000000000000000000000000000000000000000000000000000000000000000001)

@test
def unaryPlusTest_31(): Bool = assertEq!(+(-99999999999999999999999999999999999999999999999999999999999999999999999999999999.0),
                                           -99999999999999999999999999999999999999999999999999999999999999999999999999999999.0)

@test
def unaryPlusTest_32(): Bool = assertEq!(+(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000001),
                                           -0.000000000000000000000000000000000000000000000000000000000000000000000000000000001)

@test
def unaryPlusTest_33(): Bool = assertEq!(+0.0f32, 0.0f32)

@test
def unaryPlusTest_34(): Bool = assertEq!(+(-0.0f32), 0.0f32)

@test
def unaryPlusTest_35(): Bool = assertEq!(+(4.2f32), 4.2f32)

@test
def unaryPlusTest_36(): Bool = assertEq!(+999999999999999999999999999999.0f32, 999999999999999999999999999999.0f32)

@test
def unaryPlusTest_37(): Bool = assertEq!(+0.0000000000000000000000000000001f32, 0.0000000000000000000000000000001f32)

@test
def unaryPlusTest_38(): Bool = assertEq!(+(-999999999999999999999999999999.0f32), -999999999999999999999999999999.0f32)

@test
def unaryPlusTest_39(): Bool = assertEq!(+(-0.0000000000000000000000000000001f32), -0.0000000000000000000000000000001f32)

@test
def unaryPlusTest_40(): Bool = assertEq!(+0.0f64, 0.0f64)

@test
def unaryPlusTest_41(): Bool = assertEq!(+(-0.0f64), 0.0f64)

@test
def unaryPlusTest_42(): Bool = assertEq!(+(4.2f64), 4.2f64)

@test
def unaryPlusTest_43(): Bool = assertEq!(+99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64,
                                         99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64)

@test
def unaryPlusTest_44(): Bool = assertEq!(+0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64,
                                         0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64)

@test
def unaryPlusTest_45(): Bool = assertEq!(+(-99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64),
                                         -99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64)

@test
def unaryPlusTest_46(): Bool = assertEq!(+(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64),
                                         -0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64)

@test
def unaryPlusTest_47(): Bool = assertEq!(+0ii, 0ii)

@test
def unaryPlusTest_48(): Bool = assertEq!(+1ii, 1ii)

@test
def unaryPlusTest_49(): Bool = assertEq!(+10ii, 10ii)

@test
def unaryPlusTest_50(): Bool = assertEq!(+9223372036854775808ii, 9223372036854775808ii)

@test
def unaryPlusTest_51(): Bool = assertEq!(+(-9223372036854775809ii), -9223372036854775809ii)

