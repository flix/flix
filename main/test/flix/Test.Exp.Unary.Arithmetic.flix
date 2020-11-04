namespace Test/Exp/Unary/Arithmetic {

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

}
