/////////////////////////////////////////////////////////////////////////////
// Expression.Unary                                                        //
// UnaryOperator.{LogicalNot,Plus,Minus,BitwiseNegate}                     //
/////////////////////////////////////////////////////////////////////////////

@test
def testLogicalNot01(): Bool = false == !true

@test
def testLogicalNot02(): Bool = true == !false

@test
def testUnaryPlus01(): Bool = 0 == +0

@test
def testUnaryPlus02(): Bool = +36000 == 36000

@test
def testUnaryPlus03(): Bool = +(-36000) == -36000

@test
def testUnaryPlus04(): Bool = +2147483647 == 2147483647

@test
def testUnaryPlus05(): Bool = -2147483648 == +-2147483648

@test
def testUnaryPlus06(): Bool = +0i8 == 0i8

@test
def testUnaryPlus07(): Bool = +36i8 == 36i8

@test
def testUnaryPlus08(): Bool = +(-36i8) == -36i8

@test
def testUnaryPlus09(): Bool = +127i8 == 127i8

@test
def testUnaryPlus10(): Bool = +-128i8 == -128i8

@test
def testUnaryPlus11(): Bool = +0i16 == 0i16

@test
def testUnaryPlus12(): Bool = +3600i16 == 3600i16

@test
def testUnaryPlus13(): Bool = +(-3600i16) == -3600i16

@test
def testUnaryPlus14(): Bool = +32767i16 == 32767i16

@test
def testUnaryPlus15(): Bool = +-32768i16 == -32768i16

@test
def testUnaryPlus16(): Bool = +0i32 == 0i32

@test
def testUnaryPlus17(): Bool = +36000i32 == 36000i32

@test
def testUnaryPlus18(): Bool = +(-36000i32) == -36000i32

@test
def testUnaryPlus19(): Bool = +2147483647i32 == 2147483647i32

@test
def testUnaryPlus20(): Bool = +-2147483648i32 == -2147483648i32

@test
def testUnaryPlus21(): Bool = +0i64 == 0i64

@test
def testUnaryPlus22(): Bool = +3600000000i64 == 3600000000i64

@test
def testUnaryPlus23(): Bool = +(-3600000000i64) == -3600000000i64

@test
def testUnaryPlus24(): Bool = +9223372036854775807i64 == 9223372036854775807i64

@test
def testUnaryPlus25(): Bool = +-9223372036854775808i64 == -9223372036854775808i64

@test
def testUnaryPlus26(): Bool = +0.0 == 0.0

@test
def testUnaryPlus27(): Bool = +(-0.0) == 0.0

@test
def testUnaryPlus28(): Bool = +(4.2) == 4.2

@test
def testUnaryPlus29(): Bool = +99999999999999999999999999999999999999999999999999999999999999999999999999999999.0 ==
                                         99999999999999999999999999999999999999999999999999999999999999999999999999999999.0

@test
def testUnaryPlus30(): Bool = +0.000000000000000000000000000000000000000000000000000000000000000000000000000000001 ==
                                          0.000000000000000000000000000000000000000000000000000000000000000000000000000000001

@test
def testUnaryPlus31(): Bool = +(-99999999999999999999999999999999999999999999999999999999999999999999999999999999.0) ==
                                           -99999999999999999999999999999999999999999999999999999999999999999999999999999999.0

@test
def testUnaryPlus32(): Bool = +(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000001) ==
                                           -0.000000000000000000000000000000000000000000000000000000000000000000000000000000001

@test
def testUnaryPlus33(): Bool = +0.0f32 == 0.0f32

@test
def testUnaryPlus34(): Bool = +(-0.0f32) == 0.0f32

@test
def testUnaryPlus35(): Bool = +(4.2f32) == 4.2f32

@test
def testUnaryPlus36(): Bool = +999999999999999999999999999999.0f32 == 999999999999999999999999999999.0f32

@test
def testUnaryPlus37(): Bool = +0.0000000000000000000000000000001f32 == 0.0000000000000000000000000000001f32

@test
def testUnaryPlus38(): Bool = +(-999999999999999999999999999999.0f32) == -999999999999999999999999999999.0f32

@test
def testUnaryPlus39(): Bool = +(-0.0000000000000000000000000000001f32) == -0.0000000000000000000000000000001f32

@test
def testUnaryPlus40(): Bool = +0.0f64 == 0.0f64

@test
def testUnaryPlus41(): Bool = +(-0.0f64) == 0.0f64

@test
def testUnaryPlus42(): Bool = +(4.2f64) == 4.2f64

@test
def testUnaryPlus43(): Bool = +99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64 ==
                                         99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64

@test
def testUnaryPlus44(): Bool = +0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64 ==
                                         0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64

@test
def testUnaryPlus45(): Bool = +(-99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64) ==
                                         -99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64

@test
def testUnaryPlus46(): Bool = +(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64) ==
                                         -0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64

@test
def testUnaryPlus47(): Bool = +0ii == 0ii

@test
def testUnaryPlus48(): Bool = +1ii == 1ii

@test
def testUnaryPlus49(): Bool = +10ii == 10ii

@test
def testUnaryPlus50(): Bool = +9223372036854775808ii == 9223372036854775808ii

@test
def testUnaryPlus51(): Bool = +(-9223372036854775809ii) == -9223372036854775809ii

@test
def testUnaryMinus01(): Bool = -0 == 0

@test
def testUnaryMinus02(): Int = -36000

@test
def testUnaryMinus03(): Bool = -(-36000) == 36000

@test
def testUnaryMinus04(): Bool = --2147483648 == -2147483648

@test
def testUnaryMinus05(): Int = -2147483648

@test
def testUnaryMinus06(): Int8 = -0i8

@test
def testUnaryMinus07(): Int8 = -36i8

@test
def testUnaryMinus08(): Bool = -(-36i8) == 36i8

@test
def testUnaryMinus09(): Bool = --128i8 == -128i8

@test
def testUnaryMinus10(): Int8 = -127i8

@test
def testUnaryMinus11(): Bool = -0i16 == 0i16

@test
def testUnaryMinus12(): Int16 = -3600i16

@test
def testUnaryMinus13(): Bool = -(-3600i16) == 3600i16

@test
def testUnaryMinus14(): Int16 = -32767i16

@test
def testUnaryMinus15(): Bool = --32768i16 == -32768i16

@test
def testUnaryMinus16(): Bool = -0i32 == 0i32

@test
def testUnaryMinus17(): Int32 = -36000i32

@test
def testUnaryMinus18(): Bool = -(-36000i32) == 36000i32

@test
def testUnaryMinus19(): Int32 = -2147483647i32

@test
def testUnaryMinus20(): Bool = --2147483648i32 == -2147483648i32

@test
def testUnaryMinus21(): Bool = -0i64 == 0i64

@test
def testUnaryMinus22(): Int64 = -3600000000i64

@test
def testUnaryMinus23(): Bool = -(-3600000000i64) == 3600000000i64

@test
def testUnaryMinus24(): Int64 = -9223372036854775807i64

@test
def testUnaryMinus25(): Bool = --9223372036854775808i64 == -9223372036854775808i64

@test
def testUnaryMinus26(): Bool = -0.0 == 0.0

@test
def testUnaryMinus27(): Bool = -(-0.0) == 0.0

@test
def testUnaryMinus28(): Bool = -(4.2) == -4.2

@test
def testUnaryMinus29(): Float = -99999999999999999999999999999999999999999999999999999999999999999999999999999999.0

@test
def testUnaryMinus30(): Float = -0.000000000000000000000000000000000000000000000000000000000000000000000000000000001

@test
def testUnaryMinus31(): Bool = -(-99999999999999999999999999999999999999999999999999999999999999999999999999999999.0) ==
                                         99999999999999999999999999999999999999999999999999999999999999999999999999999999.0

@test
def testUnaryMinus32(): Bool = -(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000001) ==
                                         0.000000000000000000000000000000000000000000000000000000000000000000000000000000001

@test
def testUnaryMinus33(): Bool = -0.0f32 == 0.0f32

@test
def testUnaryMinus34(): Bool = -(-0.0f32) == 0.0f32

@test
def testUnaryMinus35(): Bool = -(4.2f32) == -4.2f32

@test
def testUnaryMinus36(): Float32 = -999999999999999999999999999999.0f32

@test
def testUnaryMinus37(): Float32 = -0.0000000000000000000000000000001f32

@test
def testUnaryMinus38(): Bool = -(-999999999999999999999999999999.0f32) == 999999999999999999999999999999.0f32

@test
def testUnaryMinus39(): Bool = -(-0.0000000000000000000000000000001f32) == 0.0000000000000000000000000000001f32

@test
def testUnaryMinus40(): Float64 = -0.0f64

@test
def testUnaryMinus41(): Bool = -(-0.0f64) == 0.0f64

@test
def testUnaryMinus42(): Bool = -(4.2f64) == -4.2f64

@test
def testUnaryMinus43(): Float64 = -99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64

@test
def testUnaryMinus44(): Float64 = -0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64

@test
def testUnaryMinus45(): Bool = -(-99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64) ==
                                         99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64

@test
def testUnaryMinus46(): Bool = -(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64) ==
                                         0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64

@test
def testUnaryMinus47(): Bool = -0ii == 0ii

@test
def testUnaryMinus48(): BigInt = -1ii

@test
def testUnaryMinus49(): BigInt = -10ii

@test
def testUnaryMinus50(): BigInt = -9223372036854775808ii

@test
def testUnaryMinus51(): Bool = -(-9223372036854775809ii) == 9223372036854775809ii

@test
def testUnaryBitwiseNegate01(): Bool = ~~~0 == -1

@test
def testUnaryBitwiseNegate02(): Bool = ~~~1 == -2

@test
def testUnaryBitwiseNegate03(): Bool = ~~~(-1) == 0

@test
def testUnaryBitwiseNegate04(): Bool = ~~~36000 == -36001

@test
def testUnaryBitwiseNegate05(): Bool = ~~~(-36000) == 35999

@test
def testUnaryBitwiseNegate06(): Bool = ~~~2147483647 == -2147483648

@test
def testUnaryBitwiseNegate07(): Bool = ~~~-2147483648 == 2147483647

@test
def testUnaryBitwiseNegate08(): Bool = ~~~0i8 == -1i8

@test
def testUnaryBitwiseNegate09(): Bool = ~~~1i8 == -2i8

@test
def testUnaryBitwiseNegate10(): Bool = ~~~(-1i8) == 0i8

@test
def testUnaryBitwiseNegate11(): Bool = ~~~42i8 == -43i8

@test
def testUnaryBitwiseNegate12(): Bool = ~~~(-42i8) == 41i8

@test
def testUnaryBitwiseNegate13(): Bool = ~~~127i8 == -128i8

@test
def testUnaryBitwiseNegate14(): Bool = ~~~-128i8 == 127i8

@test
def testUnaryBitwiseNegate15(): Bool = ~~~0i16 == -1i16

@test
def testUnaryBitwiseNegate16(): Bool = ~~~1i16 == -2i16

@test
def testUnaryBitwiseNegate17(): Bool = ~~~(-1i16) == 0i16

@test
def testUnaryBitwiseNegate18(): Bool = ~~~420i16 == -421i16

@test
def testUnaryBitwiseNegate19(): Bool = ~~~(-420i16) == 419i16

@test
def testUnaryBitwiseNegate20(): Bool = ~~~32767i16 == -32768i16

@test
def testUnaryBitwiseNegate21(): Bool = ~~~-32768i16 == 32767i16

@test
def testUnaryBitwiseNegate22(): Bool = ~~~0i32 == -1i32

@test
def testUnaryBitwiseNegate23(): Bool = ~~~1i32 == -2i32

@test
def testUnaryBitwiseNegate24(): Bool = ~~~(-1i32) == 0i32

@test
def testUnaryBitwiseNegate25(): Bool = ~~~36000i32 == -36001i32

@test
def testUnaryBitwiseNegate26(): Bool = ~~~(-36000i32) == 35999i32

@test
def testUnaryBitwiseNegate27(): Bool = ~~~2147483647i32 == -2147483648i32

@test
def testUnaryBitwiseNegate28(): Bool = ~~~-2147483648i32 == 2147483647i32

@test
def testUnaryBitwiseNegate29(): Bool = ~~~0i64 == -1i64

@test
def testUnaryBitwiseNegate30(): Bool = ~~~1i64 == -2i64

@test
def testUnaryBitwiseNegate31(): Bool = ~~~(-1i64) == 0i64

@test
def testUnaryBitwiseNegate32(): Bool = ~~~10000000000i64 == -10000000001i64

@test
def testUnaryBitwiseNegate33(): Bool = ~~~(-10000000000i64) == 9999999999i64

@test
def testUnaryBitwiseNegate34(): Bool = ~~~9223372036854775807i64 == -9223372036854775808i64

@test
def testUnaryBitwiseNegate35(): Bool = ~~~-9223372036854775808i64 == 9223372036854775807i64

@test
def testUnaryBitwiseNegate36(): Bool = ~~~0ii == -1ii

@test
def testUnaryBitwiseNegate37(): Bool = ~~~1ii == -2ii

@test
def testUnaryBitwiseNegate38(): Bool = ~~~(-1ii) == 0ii

@test
def testUnaryBitwiseNegate39(): Bool = ~~~9223372036854775808ii == -9223372036854775809ii

@test
def testUnaryBitwiseNegate40(): Bool = ~~~(-9223372036854775809ii) == 9223372036854775808ii


