mod Test.Exp.Unary.Arithmetic {

    use Assert.assertEq

    @Test
    def testUnaryMinus01(): Unit \ Assert =
        assertEq(expected = 0, -0)

    @Test
    def testUnaryMinus02(): Unit \ Assert =
        assertEq(expected = -36000, -36000)

    @Test
    def testUnaryMinus03(): Unit \ Assert =
        assertEq(expected = 36000, -(-36000))

    @Test
    def testUnaryMinus04(): Unit \ Assert =
        assertEq(expected = -2147483648, -(-2147483648))

    @Test
    def testUnaryMinus05(): Unit \ Assert =
        assertEq(expected = -2147483648, -2147483648)

    @Test
    def testUnaryMinus06(): Unit \ Assert =
        assertEq(expected = 0i8, -0i8)

    @Test
    def testUnaryMinus07(): Unit \ Assert =
        assertEq(expected = -36i8, -36i8)

    @Test
    def testUnaryMinus08(): Unit \ Assert =
        assertEq(expected = 36i8, -(-36i8))

    @Test
    def testUnaryMinus09(): Unit \ Assert =
        assertEq(expected = -128i8, -(-128i8))

    @Test
    def testUnaryMinus10(): Unit \ Assert =
        assertEq(expected = -127i8, -127i8)

    @Test
    def testUnaryMinus11(): Unit \ Assert =
        assertEq(expected = 0i16, -0i16)

    @Test
    def testUnaryMinus12(): Unit \ Assert =
        assertEq(expected = -3600i16, -3600i16)

    @Test
    def testUnaryMinus13(): Unit \ Assert =
        assertEq(expected = 3600i16, -(-3600i16))

    @Test
    def testUnaryMinus14(): Unit \ Assert =
        assertEq(expected = -32767i16, -32767i16)

    @Test
    def testUnaryMinus15(): Unit \ Assert =
        assertEq(expected = -32768i16, -(-32768i16))

    @Test
    def testUnaryMinus16(): Unit \ Assert =
        assertEq(expected = 0i32, -0i32)

    @Test
    def testUnaryMinus17(): Unit \ Assert =
        assertEq(expected = -36000i32, -36000i32)

    @Test
    def testUnaryMinus18(): Unit \ Assert =
        assertEq(expected = 36000i32, -(-36000i32))

    @Test
    def testUnaryMinus19(): Unit \ Assert =
        assertEq(expected = -2147483647i32, -2147483647i32)

    @Test
    def testUnaryMinus20(): Unit \ Assert =
        assertEq(expected = -2147483648i32, -(-2147483648i32))

    @Test
    def testUnaryMinus21(): Unit \ Assert =
        assertEq(expected = 0i64, -0i64)

    @Test
    def testUnaryMinus22(): Unit \ Assert =
        assertEq(expected = -3600000000i64, -3600000000i64)

    @Test
    def testUnaryMinus23(): Unit \ Assert =
        assertEq(expected = 3600000000i64, -(-3600000000i64))

    @Test
    def testUnaryMinus24(): Unit \ Assert =
        assertEq(expected = -9223372036854775807i64, -9223372036854775807i64)

    @Test
    def testUnaryMinus25(): Unit \ Assert =
        assertEq(expected = -9223372036854775808i64, -(-9223372036854775808i64))

    @Test
    def testUnaryMinus26(): Unit \ Assert =
        assertEq(expected = 0.0, -0.0)

    @Test
    def testUnaryMinus27(): Unit \ Assert =
        assertEq(expected = 0.0, -(-0.0))

    @Test
    def testUnaryMinus28(): Unit \ Assert =
        assertEq(expected = -4.2, -(4.2))

    @Test
    def testUnaryMinus29(): Unit \ Assert =
        assertEq(expected = -99999999999999999999999999999999999999999999999999999999999999999999999999999999.0,
                            -99999999999999999999999999999999999999999999999999999999999999999999999999999999.0)

    @Test
    def testUnaryMinus30(): Unit \ Assert =
        assertEq(expected = -0.000000000000000000000000000000000000000000000000000000000000000000000000000000001,
                            -0.000000000000000000000000000000000000000000000000000000000000000000000000000000001)

    @Test
    def testUnaryMinus31(): Unit \ Assert =
        assertEq(expected = 99999999999999999999999999999999999999999999999999999999999999999999999999999999.0,
                         -(-99999999999999999999999999999999999999999999999999999999999999999999999999999999.0))

    @Test
    def testUnaryMinus32(): Unit \ Assert =
        assertEq(expected = 0.000000000000000000000000000000000000000000000000000000000000000000000000000000001,
                         -(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000001))

    @Test
    def testUnaryMinus33(): Unit \ Assert =
        assertEq(expected = 0.0f32, -0.0f32)

    @Test
    def testUnaryMinus34(): Unit \ Assert =
        assertEq(expected = 0.0f32, -(-0.0f32))

    @Test
    def testUnaryMinus35(): Unit \ Assert =
        assertEq(expected = -4.2f32, -(4.2f32))

    @Test
    def testUnaryMinus36(): Unit \ Assert =
        assertEq(expected = -999999999999999999999999999999.0f32,
                            -999999999999999999999999999999.0f32)

    @Test
    def testUnaryMinus37(): Unit \ Assert =
        assertEq(expected = -0.0000000000000000000000000000001f32,
                            -0.0000000000000000000000000000001f32)

    @Test
    def testUnaryMinus38(): Unit \ Assert =
        assertEq(expected = 999999999999999999999999999999.0f32,
                         -(-999999999999999999999999999999.0f32))

    @Test
    def testUnaryMinus39(): Unit \ Assert =
        assertEq(expected = 0.0000000000000000000000000000001f32,
                         -(-0.0000000000000000000000000000001f32))

    @Test
    def testUnaryMinus40(): Unit \ Assert =
        assertEq(expected = -0.0f64, -0.0f64)

    @Test
    def testUnaryMinus41(): Unit \ Assert =
        assertEq(expected = 0.0f64, -(-0.0f64))

    @Test
    def testUnaryMinus42(): Unit \ Assert =
        assertEq(expected = -4.2f64, -(4.2f64))

    @Test
    def testUnaryMinus43(): Unit \ Assert =
        assertEq(expected = -99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64,
                            -99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64)

    @Test
    def testUnaryMinus44(): Unit \ Assert =
        assertEq(expected = -0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64,
                            -0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64)

    @Test
    def testUnaryMinus45(): Unit \ Assert =
        assertEq(expected = 99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64,
                         -(-99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64))

    @Test
    def testUnaryMinus46(): Unit \ Assert =
        assertEq(expected = 0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64,
                         -(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64))

    @Test
    def testUnaryMinus47(): Unit \ Assert =
        assertEq(expected = 0ii, -0ii)

    @Test
    def testUnaryMinus48(): Unit \ Assert =
        assertEq(expected = -1ii, -1ii)

    @Test
    def testUnaryMinus49(): Unit \ Assert =
        assertEq(expected = -10ii, -10ii)

    @Test
    def testUnaryMinus50(): Unit \ Assert =
        assertEq(expected = -9223372036854775808ii, -9223372036854775808ii)

    @Test
    def testUnaryMinus51(): Unit \ Assert =
        assertEq(expected = 9223372036854775809ii, -(-9223372036854775809ii))

    @Test
    def testUnaryMinus52(): Unit \ Assert =
        assertEq(expected = -0.0ff, -0.0ff)

    @Test
    def testUnaryMinus53(): Unit \ Assert =
        assertEq(expected = 0.0ff, -(-0.0ff))

    @Test
    def testUnaryMinus54(): Unit \ Assert =
        assertEq(expected = -4.2ff, -(4.2ff))

    @Test
    def testUnaryMinus55(): Unit \ Assert =
        assertEq(expected = -99999999999999999999999999999999999999999999999999999999999999999999999999999999.0ff,
                            -99999999999999999999999999999999999999999999999999999999999999999999999999999999.0ff)

    @Test
    def testUnaryMinus56(): Unit \ Assert =
        assertEq(expected = -0.000000000000000000000000000000000000000000000000000000000000000000000000000000001ff,
                            -0.000000000000000000000000000000000000000000000000000000000000000000000000000000001ff)

    @Test
    def testUnaryMinus57(): Unit \ Assert =
        assertEq(expected = 99999999999999999999999999999999999999999999999999999999999999999999999999999999.0ff,
                         -(-99999999999999999999999999999999999999999999999999999999999999999999999999999999.0ff))

    @Test
    def testUnaryMinus58(): Unit \ Assert =
        assertEq(expected = 0.000000000000000000000000000000000000000000000000000000000000000000000000000000001ff,
                         -(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000001ff))

}
