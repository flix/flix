mod TestBigDecimal {

    use Assert.{assertEq, assertTrue, assertNone};

    /////////////////////////////////////////////////////////////////////////////
    // min                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def min01(): Unit \ Assert = assertEq(expected = 1.0ff, BigDecimal.min(1.0ff, 2.0ff))

    @Test
    def min02(): Unit \ Assert = assertEq(expected = -1.0ff, BigDecimal.min(2.0ff, -1.0ff))

    @Test
    def min03(): Unit \ Assert = assertEq(expected = -66.0ff, BigDecimal.min(-33.0ff, -66.0ff))

    /////////////////////////////////////////////////////////////////////////////
    // max                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def max01(): Unit \ Assert = assertEq(expected = 49.0ff, BigDecimal.max(48.0ff, 49.0ff))

    @Test
    def max02(): Unit \ Assert = assertEq(expected = 4.0ff, BigDecimal.max(4.0ff, -16.0ff))

    @Test
    def max03(): Unit \ Assert = assertEq(expected = -16.0ff, BigDecimal.max(-34.0ff, -16.0ff))

    /////////////////////////////////////////////////////////////////////////////
    // abs                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def abs01(): Unit \ Assert = assertEq(expected = 0.0ff, BigDecimal.abs(0.0ff))

    @Test
    def abs02(): Unit \ Assert = assertEq(expected = 10.1ff, BigDecimal.abs(10.1ff))

    @Test
    def abs03(): Unit \ Assert = assertEq(expected = 10.1ff, BigDecimal.abs(-10.1ff))

    /////////////////////////////////////////////////////////////////////////////
    // dist                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dist01(): Unit \ Assert = assertEq(expected = 0.0ff, BigDecimal.dist(0.0ff, 0.0ff))

    @Test
    def dist02(): Unit \ Assert = assertEq(expected = 0.1ff, BigDecimal.dist(10.1ff, 10.0ff))

    @Test
    def dist03(): Unit \ Assert = assertEq(expected = 0.1ff, BigDecimal.dist(-10.1ff, -10.0ff))

    @Test
    def dist04(): Unit \ Assert = assertEq(expected = 0.01ff, BigDecimal.dist(10.01ff, 10.00ff))

    @Test
    def dist05(): Unit \ Assert = assertEq(expected = 0.01ff, BigDecimal.dist(-10.01ff, -10.00ff))

    /////////////////////////////////////////////////////////////////////////////
    // compare                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def compare01(): Unit \ Assert = assertEq(expected = 1, BigDecimal.compare(1.0ff, -1.0ff))

    @Test
    def compare02(): Unit \ Assert = assertEq(expected = 1, BigDecimal.compare(-9.0ff, -11.0ff))

    @Test
    def compare03(): Unit \ Assert = assertEq(expected = 0, BigDecimal.compare(1.0ff, 1.0ff))

    @Test
    def compare04(): Unit \ Assert = assertEq(expected = 0, BigDecimal.compare(-1.0ff, -1.0ff))

    @Test
    def compare05(): Unit \ Assert = assertEq(expected = -1, BigDecimal.compare(-1.0ff, 1.0ff))

    @Test
    def compare06(): Unit \ Assert = assertEq(expected = -1, BigDecimal.compare(11.0ff, 21.0ff))

    /////////////////////////////////////////////////////////////////////////////
    // signum                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def signum01(): Unit \ Assert = assertEq(expected = 1, BigDecimal.signum(2.0ff))

    @Test
    def signum02(): Unit \ Assert = assertEq(expected = 1, BigDecimal.signum(3.0ff))

    @Test
    def signum03(): Unit \ Assert = assertEq(expected = 0, BigDecimal.signum(0.0ff))

    @Test
    def signum04(): Unit \ Assert = assertEq(expected = -1, BigDecimal.signum(-2.0ff))

    @Test
    def signum05(): Unit \ Assert = assertEq(expected = -1, BigDecimal.signum(-10.0ff))

    /////////////////////////////////////////////////////////////////////////////
    // pow                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def pow01(): Unit \ Assert = assertEq(expected = 1.0ff, BigDecimal.pow(base = 0.0ff, 0))

    @Test
    def pow02(): Unit \ Assert = assertEq(expected = 0.0ff, BigDecimal.pow(base = 0.0ff, 1))

    @Test
    def pow03(): Unit \ Assert = assertEq(expected = 1.0ff, BigDecimal.pow(base = 1.0ff, 0))

    @Test
    def pow04(): Unit \ Assert = assertEq(expected = 1.0ff, BigDecimal.pow(base = 1.0ff, 1))

    @Test
    def pow05(): Unit \ Assert = assertEq(expected = 25.0ff, BigDecimal.pow(base = 5.0ff, 2))

    /////////////////////////////////////////////////////////////////////////////
    // fromString                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def fromString01(): Unit \ Assert = assertNone(BigDecimal.fromString("Bad"))

    @Test
    def fromString02(): Unit \ Assert = assertEq(expected = Some(1.0ff), BigDecimal.fromString(" 1.0"))

    @Test
    def fromString03(): Unit \ Assert = assertEq(expected = Some(1.0ff), BigDecimal.fromString("1.0 "))

    @Test
    def fromString04(): Unit \ Assert = assertEq(expected = Some(0.0ff), BigDecimal.fromString("0.0"))

    @Test
    def fromString05(): Unit \ Assert = assertEq(expected = Some(1.0ff), BigDecimal.fromString("1.0"))

    @Test
    def fromString06(): Unit \ Assert = assertEq(expected = Some(-1.0ff), BigDecimal.fromString("-1.0"))

    @Test
    def fromString07(): Unit \ Assert = assertEq(expected = Some(3.14ff), BigDecimal.fromString("3.14"))

    @Test
    def fromString08(): Unit \ Assert = assertEq(expected = Some(-3.14ff), BigDecimal.fromString("-3.14"))

    /////////////////////////////////////////////////////////////////////////////
    // toPlainString                                                           //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toPlainString01(): Unit \ Assert = assertEq(expected = "1.1", BigDecimal.toPlainString(1.1ff))

    @Test
    def toPlainString02(): Unit \ Assert = assertEq(expected = "1.01", BigDecimal.toPlainString(1.01ff))

    @Test
    def toPlainString03(): Unit \ Assert = assertEq(expected = "1.001", BigDecimal.toPlainString(1.001ff))

    @Test
    def toPlainString04(): Unit \ Assert = assertEq(expected = "1.000001", BigDecimal.toPlainString(1.000001ff))

    /////////////////////////////////////////////////////////////////////////////
    // numericEquals                                                           //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def numericEquals01(): Unit \ Assert = assertTrue(BigDecimal.numericEquals(1.1ff, 1.100000ff))

    @Test
    def numericEquals02(): Unit \ Assert = assertTrue(BigDecimal.numericEquals(1.01ff, 1.0100000ff))

    @Test
    def numericEquals03(): Unit \ Assert = assertTrue(BigDecimal.numericEquals(1.001ff, 1.00100000ff))

    /////////////////////////////////////////////////////////////////////////////
    // scale                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def scale01(): Unit \ Assert = assertEq(expected = 1, BigDecimal.scale(1.1ff))

    @Test
    def scale02(): Unit \ Assert = assertEq(expected = 2, BigDecimal.scale(1.01ff))

    /////////////////////////////////////////////////////////////////////////////
    // precision                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def precision01(): Unit \ Assert = assertEq(expected = 1, BigDecimal.precision(0.0ff))

    /////////////////////////////////////////////////////////////////////////////
    // ceil                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def ceil01(): Unit \ Assert =
        assertTrue(BigDecimal.numericEquals(BigDecimal.ceil(0.5ff), 1.0ff))

    @Test
    def ceil02(): Unit \ Assert =
        assertTrue(BigDecimal.numericEquals(BigDecimal.ceil(0.1ff), 1.0ff))

    @Test
    def ceil03(): Unit \ Assert =
        assertTrue(BigDecimal.numericEquals(BigDecimal.ceil(1.0ff), 1.0ff))

    @Test
    def ceil04(): Unit \ Assert =
        assertTrue(BigDecimal.numericEquals(BigDecimal.ceil(0.0ff), 0.0ff))

    @Test
    def ceil05(): Unit \ Assert =
        assertTrue(BigDecimal.numericEquals(BigDecimal.ceil(-0.1ff), 0.0ff))

    @Test
    def ceil06(): Unit \ Assert =
        assertTrue(BigDecimal.numericEquals(BigDecimal.ceil(-0.9ff), 0.0ff))

    /////////////////////////////////////////////////////////////////////////////
    // floor                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def floor01(): Unit \ Assert =
        assertTrue(BigDecimal.numericEquals(BigDecimal.floor(0.5ff), 0.0ff))

    @Test
    def floor02(): Unit \ Assert =
        assertTrue(BigDecimal.numericEquals(BigDecimal.floor(0.1ff), 0.0ff))

    @Test
    def floor03(): Unit \ Assert =
        assertTrue(BigDecimal.numericEquals(BigDecimal.floor(1.0ff), 1.0ff))

    @Test
    def floor04(): Unit \ Assert =
        assertTrue(BigDecimal.numericEquals(BigDecimal.floor(0.0ff), 0.0ff))

    @Test
    def floor05(): Unit \ Assert =
        assertTrue(BigDecimal.numericEquals(BigDecimal.floor(-0.1ff), -1.0ff))

    @Test
    def floor06(): Unit \ Assert =
        assertTrue(BigDecimal.numericEquals(BigDecimal.floor(-0.9ff), -1.0ff))

    /////////////////////////////////////////////////////////////////////////////
    // round                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def round01(): Unit \ Assert =
        assertTrue(BigDecimal.numericEquals(BigDecimal.round(0.5ff), 0.0ff))

    @Test
    def round02(): Unit \ Assert =
        assertTrue(BigDecimal.numericEquals(BigDecimal.round(0.1ff), 0.0ff))

    @Test
    def round03(): Unit \ Assert =
        assertTrue(BigDecimal.numericEquals(BigDecimal.round(1.0ff), 1.0ff))

    @Test
    def round04(): Unit \ Assert =
        assertTrue(BigDecimal.numericEquals(BigDecimal.round(0.0ff), 0.0ff))

    @Test
    def round05(): Unit \ Assert =
        assertTrue(BigDecimal.numericEquals(BigDecimal.round(-0.1ff), 0.0ff))

    @Test
    def round06(): Unit \ Assert =
        assertTrue(BigDecimal.numericEquals(BigDecimal.round(-0.9ff), -1.0ff))

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt8                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToInt801(): Unit \ Assert = assertEq(expected = Some(0i8), BigDecimal.tryToInt8(0.0ff))

    @Test
    def tryToInt802(): Unit \ Assert = assertEq(expected = Some(1i8), BigDecimal.tryToInt8(1.0ff))

    @Test
    def tryToInt803(): Unit \ Assert = assertEq(expected = Some(-1i8), BigDecimal.tryToInt8(-1.0ff))

    @Test
    def tryToInt804(): Unit \ Assert = assertEq(expected = Some(127i8), BigDecimal.tryToInt8(127.0ff))

    @Test
    def tryToInt805(): Unit \ Assert = assertEq(expected = None, BigDecimal.tryToInt8(128.0ff))

    @Test
    def tryToInt806(): Unit \ Assert = assertEq(expected = Some(-128i8), BigDecimal.tryToInt8(-128.0ff))

    @Test
    def tryToInt807(): Unit \ Assert = assertEq(expected = None, BigDecimal.tryToInt8(-129.0ff))

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt16                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToInt1601(): Unit \ Assert = assertEq(expected = Some(0i16), BigDecimal.tryToInt16(0.0ff))

    @Test
    def tryToInt1602(): Unit \ Assert = assertEq(expected = Some(1i16), BigDecimal.tryToInt16(1.0ff))

    @Test
    def tryToInt1603(): Unit \ Assert = assertEq(expected = Some(-1i16), BigDecimal.tryToInt16(-1.0ff))

    @Test
    def tryToInt1604(): Unit \ Assert = assertEq(expected = Some(32767i16), BigDecimal.tryToInt16(32767.0ff))

    @Test
    def tryToInt1605(): Unit \ Assert = assertEq(expected = None, BigDecimal.tryToInt16(32768.0ff))

    @Test
    def tryToInt1606(): Unit \ Assert = assertEq(expected = Some(-32768i16), BigDecimal.tryToInt16(-32768.0ff))

    @Test
    def tryToInt1607(): Unit \ Assert = assertEq(expected = None, BigDecimal.tryToInt16(-32769.0ff))

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt32                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToInt3201(): Unit \ Assert = assertEq(expected = Some(0), BigDecimal.tryToInt32(0.0ff))

    @Test
    def tryToInt3202(): Unit \ Assert = assertEq(expected = Some(1), BigDecimal.tryToInt32(1.0ff))

    @Test
    def tryToInt3203(): Unit \ Assert = assertEq(expected = Some(-1), BigDecimal.tryToInt32(-1.0ff))

    @Test
    def tryToInt3204(): Unit \ Assert = assertEq(expected = Some(2147483647), BigDecimal.tryToInt32(2147483647.0ff))

    @Test
    def tryToInt3205(): Unit \ Assert = assertEq(expected = None, BigDecimal.tryToInt32(2147483648.0ff))

    @Test
    def tryToInt3206(): Unit \ Assert = assertEq(expected = Some(-2147483648), BigDecimal.tryToInt32(-2147483648.0ff))

    @Test
    def tryToInt3207(): Unit \ Assert = assertEq(expected = None, BigDecimal.tryToInt32(-2147483649.0ff))


    /////////////////////////////////////////////////////////////////////////////
    // tryToInt64                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToInt6401(): Unit \ Assert = assertEq(expected = Some(0i64), BigDecimal.tryToInt64(0.0ff))

    @Test
    def tryToInt6402(): Unit \ Assert = assertEq(expected = Some(1i64), BigDecimal.tryToInt64(1.0ff))

    @Test
    def tryToInt6403(): Unit \ Assert = assertEq(expected = Some(-1i64), BigDecimal.tryToInt64(-1.0ff))

    @Test
    def tryToInt6404(): Unit \ Assert = assertEq(expected = Some(9223372036854775807i64), BigDecimal.tryToInt64(9223372036854775807.0ff))

    @Test
    def tryToInt6405(): Unit \ Assert = assertEq(expected = None, BigDecimal.tryToInt64(9223372036854780000.0ff))        // Out of range

    @Test
    def tryToInt6406(): Unit \ Assert = assertEq(expected = Some(-9223372036854775808i64), BigDecimal.tryToInt64(-9223372036854775808.0ff))

    @Test
    def tryToInt6407(): Unit \ Assert = assertEq(expected = None, BigDecimal.tryToInt64(-9223372036854780000.0ff))       // Out of range

    /////////////////////////////////////////////////////////////////////////////
    // toBigInt                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toBigInt01(): Unit \ Assert = assertEq(expected = 0ii, BigDecimal.toBigInt(0.0ff))

    @Test
    def toBigInt02(): Unit \ Assert = assertEq(expected = 1ii, BigDecimal.toBigInt(1.0ff))

    @Test
    def toBigInt03(): Unit \ Assert = assertEq(expected = -1ii, BigDecimal.toBigInt(-1.0ff))

    @Test
    def toBigInt04(): Unit \ Assert = assertEq(expected = 9223372036854775807ii, BigDecimal.toBigInt(9223372036854775807.0ff))

    @Test
    def toBigInt05(): Unit \ Assert = assertEq(expected = 9223372036854780000ii, BigDecimal.toBigInt(9223372036854780000.0ff))

    @Test
    def toBigInt06(): Unit \ Assert = assertEq(expected = -9223372036854775808ii, BigDecimal.toBigInt(-9223372036854775808.0ff))

    @Test
    def toBigInt07(): Unit \ Assert = assertEq(expected = -9223372036854780000ii, BigDecimal.toBigInt(-9223372036854780000.0ff))

    /////////////////////////////////////////////////////////////////////////////
    // tryToFloat32                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToFloat3201(): Unit \ Assert = assertEq(expected = Some(0.0f32), BigDecimal.tryToFloat32(0.0ff))

    @Test
    def tryToFloat3202(): Unit \ Assert = assertEq(expected = Some(1.0f32), BigDecimal.tryToFloat32(1.0ff))

    @Test
    def tryToFloat3203(): Unit \ Assert = assertEq(expected = Some(-1.0f32), BigDecimal.tryToFloat32(-1.0ff))

    @Test
    def tryToFloat3204(): Unit \ Assert = assertTrue(match BigDecimal.tryToFloat32(123456789.10ff) {
        case None => false
        case Some(d) => d >= 123456700.0f32 and d <= 123456800.0f32       // Allow imprecision
    })

    @Test
    def tryToFloat3205(): Unit \ Assert = assertTrue(match BigDecimal.tryToFloat32(-123456789.10ff) {
        case None => false
        case Some(d) => d <= -123456700.0f32 and d >= -123456800.0f32     // Allow imprecision
    })

    /////////////////////////////////////////////////////////////////////////////
    // tryToFloat64                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToFloat6401(): Unit \ Assert = assertEq(expected = Some(0.0f64), BigDecimal.tryToFloat64(0.0ff))

    @Test
    def tryToFloat6402(): Unit \ Assert = assertEq(expected = Some(1.0f64), BigDecimal.tryToFloat64(1.0ff))

    @Test
    def tryToFloat6403(): Unit \ Assert = assertEq(expected = Some(-1.0f64), BigDecimal.tryToFloat64(-1.0ff))

    @Test
    def tryToFloat6404(): Unit \ Assert = assertTrue(match BigDecimal.tryToFloat64(123456789.10ff) {
        case None => false
        case Some(d) => d >= 123456700.0f64 and d <= 123456800.0f64       // Allow imprecision
    })

    @Test
    def tryToFloat6405(): Unit \ Assert = assertTrue(match BigDecimal.tryToFloat64(-123456789.10ff) {
        case None => false
        case Some(d) => d <= -123456700.0f64 and d >= -123456800.0f64     // Allow imprecision
    })

}
