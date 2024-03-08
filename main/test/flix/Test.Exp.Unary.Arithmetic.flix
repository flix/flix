mod Test.Exp.Unary.Arithmetic {

    @test
    def testUnaryMinus01(): Bool = -0 == 0

    @test
    def testUnaryMinus02(): Int32 = -36000

    @test
    def testUnaryMinus03(): Bool = -(-36000) == 36000

    @test
    def testUnaryMinus04(): Bool = --2147483648 == -2147483648

    @test
    def testUnaryMinus05(): Int32 = -2147483648

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
    def testUnaryMinus29(): Float64 = -99999999999999999999999999999999999999999999999999999999999999999999999999999999.0

    @test
    def testUnaryMinus30(): Float64 = -0.000000000000000000000000000000000000000000000000000000000000000000000000000000001

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
    def testUnaryMinus52(): BigDecimal = -0.0ff

    @test
    def testUnaryMinus53(): Bool = -(-0.0ff) == 0.0ff

    @test
    def testUnaryMinus54(): Bool = -(4.2ff) == -4.2ff

    @test
    def testUnaryMinus55(): BigDecimal = -99999999999999999999999999999999999999999999999999999999999999999999999999999999.0ff

    @test
    def testUnaryMinus56(): BigDecimal = -0.000000000000000000000000000000000000000000000000000000000000000000000000000000001ff

    @test
    def testUnaryMinus57(): Bool = -(-99999999999999999999999999999999999999999999999999999999999999999999999999999999.0ff) ==
                                         99999999999999999999999999999999999999999999999999999999999999999999999999999999.0ff

    @test
    def testUnaryMinus58(): Bool = -(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000001ff) ==
                                         0.000000000000000000000000000000000000000000000000000000000000000000000000000000001ff

}
