mod Test.Exp.Unary.Arithmetic {

    @Test
    def testUnaryMinus01(): Bool = -0 == 0

    @Test
    def testUnaryMinus02(): Int32 = -36000

    @Test
    def testUnaryMinus03(): Bool = -(-36000) == 36000

    @Test
    def testUnaryMinus04(): Bool = --2147483648 == -2147483648

    @Test
    def testUnaryMinus05(): Int32 = -2147483648

    @Test
    def testUnaryMinus06(): Int8 = -0i8

    @Test
    def testUnaryMinus07(): Int8 = -36i8

    @Test
    def testUnaryMinus08(): Bool = -(-36i8) == 36i8

    @Test
    def testUnaryMinus09(): Bool = --128i8 == -128i8

    @Test
    def testUnaryMinus10(): Int8 = -127i8

    @Test
    def testUnaryMinus11(): Bool = -0i16 == 0i16

    @Test
    def testUnaryMinus12(): Int16 = -3600i16

    @Test
    def testUnaryMinus13(): Bool = -(-3600i16) == 3600i16

    @Test
    def testUnaryMinus14(): Int16 = -32767i16

    @Test
    def testUnaryMinus15(): Bool = --32768i16 == -32768i16

    @Test
    def testUnaryMinus16(): Bool = -0i32 == 0i32

    @Test
    def testUnaryMinus17(): Int32 = -36000i32

    @Test
    def testUnaryMinus18(): Bool = -(-36000i32) == 36000i32

    @Test
    def testUnaryMinus19(): Int32 = -2147483647i32

    @Test
    def testUnaryMinus20(): Bool = --2147483648i32 == -2147483648i32

    @Test
    def testUnaryMinus21(): Bool = -0i64 == 0i64

    @Test
    def testUnaryMinus22(): Int64 = -3600000000i64

    @Test
    def testUnaryMinus23(): Bool = -(-3600000000i64) == 3600000000i64

    @Test
    def testUnaryMinus24(): Int64 = -9223372036854775807i64

    @Test
    def testUnaryMinus25(): Bool = --9223372036854775808i64 == -9223372036854775808i64

    @Test
    def testUnaryMinus26(): Bool = -0.0 == 0.0

    @Test
    def testUnaryMinus27(): Bool = -(-0.0) == 0.0

    @Test
    def testUnaryMinus28(): Bool = -(4.2) == -4.2

    @Test
    def testUnaryMinus29(): Float64 = -99999999999999999999999999999999999999999999999999999999999999999999999999999999.0

    @Test
    def testUnaryMinus30(): Float64 = -0.000000000000000000000000000000000000000000000000000000000000000000000000000000001

    @Test
    def testUnaryMinus31(): Bool = -(-99999999999999999999999999999999999999999999999999999999999999999999999999999999.0) ==
                                         99999999999999999999999999999999999999999999999999999999999999999999999999999999.0

    @Test
    def testUnaryMinus32(): Bool = -(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000001) ==
                                         0.000000000000000000000000000000000000000000000000000000000000000000000000000000001

    @Test
    def testUnaryMinus33(): Bool = -0.0f32 == 0.0f32

    @Test
    def testUnaryMinus34(): Bool = -(-0.0f32) == 0.0f32

    @Test
    def testUnaryMinus35(): Bool = -(4.2f32) == -4.2f32

    @Test
    def testUnaryMinus36(): Float32 = -999999999999999999999999999999.0f32

    @Test
    def testUnaryMinus37(): Float32 = -0.0000000000000000000000000000001f32

    @Test
    def testUnaryMinus38(): Bool = -(-999999999999999999999999999999.0f32) == 999999999999999999999999999999.0f32

    @Test
    def testUnaryMinus39(): Bool = -(-0.0000000000000000000000000000001f32) == 0.0000000000000000000000000000001f32

    @Test
    def testUnaryMinus40(): Float64 = -0.0f64

    @Test
    def testUnaryMinus41(): Bool = -(-0.0f64) == 0.0f64

    @Test
    def testUnaryMinus42(): Bool = -(4.2f64) == -4.2f64

    @Test
    def testUnaryMinus43(): Float64 = -99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64

    @Test
    def testUnaryMinus44(): Float64 = -0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64

    @Test
    def testUnaryMinus45(): Bool = -(-99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64) ==
                                         99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64

    @Test
    def testUnaryMinus46(): Bool = -(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64) ==
                                         0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64

    @Test
    def testUnaryMinus47(): Bool = -0ii == 0ii

    @Test
    def testUnaryMinus48(): BigInt = -1ii

    @Test
    def testUnaryMinus49(): BigInt = -10ii

    @Test
    def testUnaryMinus50(): BigInt = -9223372036854775808ii

    @Test
    def testUnaryMinus51(): Bool = -(-9223372036854775809ii) == 9223372036854775809ii

    @Test
    def testUnaryMinus52(): BigDecimal = -0.0ff

    @Test
    def testUnaryMinus53(): Bool = -(-0.0ff) == 0.0ff

    @Test
    def testUnaryMinus54(): Bool = -(4.2ff) == -4.2ff

    @Test
    def testUnaryMinus55(): BigDecimal = -99999999999999999999999999999999999999999999999999999999999999999999999999999999.0ff

    @Test
    def testUnaryMinus56(): BigDecimal = -0.000000000000000000000000000000000000000000000000000000000000000000000000000000001ff

    @Test
    def testUnaryMinus57(): Bool = -(-99999999999999999999999999999999999999999999999999999999999999999999999999999999.0ff) ==
                                         99999999999999999999999999999999999999999999999999999999999999999999999999999999.0ff

    @Test
    def testUnaryMinus58(): Bool = -(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000001ff) ==
                                         0.000000000000000000000000000000000000000000000000000000000000000000000000000000001ff

}
