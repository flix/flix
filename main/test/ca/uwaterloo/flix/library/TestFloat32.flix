mod TestFloat32 {

    use Assert.{assertEq, assertNeq, assertTrue, assertFalse, fail, assertSome, assertNone};

    /////////////////////////////////////////////////////////////////////////////
    // mul                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mul01(): Unit \ Assert = assertEq(expected = 1.0f32, Mul.mul(1.0f32, 1.0f32))

    @Test
    def mul02(): Unit \ Assert = assertEq(expected = 0.0f32, Mul.mul(17.0f32, 0.0f32))

    @Test
    def mul03(): Unit \ Assert = assertEq(expected = 0.0f32, Mul.mul(0.0f32, 32.0f32))

    @Test
    def mul04(): Unit \ Assert = assertEq(expected = 246.246f32, Mul.mul(123.123f32, 2.0f32))

    @Test
    def mul05(): Unit \ Assert = assertEq(expected = 1115.0f32, Mul.mul(5.0f32, 223.0f32))

    @Test
    def mul06(): Unit \ Assert = assertEq(expected = 0.0f32, Mul.mul(0.0f32, 0.0f32))

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def size01(): Unit \ Assert = assertEq(expected = 32, Float32.size())

    /////////////////////////////////////////////////////////////////////////////
    // minExponent                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def minExponent01(): Unit \ Assert = assertEq(expected = -126, Float32.minExponent())

    /////////////////////////////////////////////////////////////////////////////
    // maxExponent                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def maxExponent01(): Unit \ Assert = assertEq(expected = 127, Float32.maxExponent())

    /////////////////////////////////////////////////////////////////////////////
    // maxValue                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def maxValue01(): Unit \ Assert = assertEq(expected = ((2.0f32 - Float32.pow(base = 2.0f32, -23.0f32)) * Float32.pow(base = 2.0f32, 127.0f32)), Float32.maxValue())

    /////////////////////////////////////////////////////////////////////////////
    // minValue                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def minValue01(): Unit \ Assert = assertEq(expected = (-(2.0f32 - Float32.pow(base = 2.0f32, -23.0f32)) * Float32.pow(base = 2.0f32, 127.0f32)), Float32.minValue())

    /////////////////////////////////////////////////////////////////////////////
    // minPositiveValue                                                        //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def minPositiveValue01(): Unit \ Assert = assertEq(expected = Float32.pow(base = 2.0f32, -149.0f32), Float32.minPositiveValue())

    /////////////////////////////////////////////////////////////////////////////
    // nan                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def nan01(): Unit \ Assert = assertTrue(Float32.isNan(Float32.nan()))

    /////////////////////////////////////////////////////////////////////////////
    // positiveInfinity                                                        //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def positiveInfinity01(): Unit \ Assert = assertEq(expected = (1.0f32 / 0.0f32), Float32.positiveInfinity())

    /////////////////////////////////////////////////////////////////////////////
    // negativeInfinity                                                        //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def negativeInfinity01(): Unit \ Assert = assertEq(expected = (-1.0f32 / 0.0f32), Float32.negativeInfinity())

    /////////////////////////////////////////////////////////////////////////////
    // isFinite                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def isFinite01(): Unit \ Assert = assertTrue(Float32.isFinite(1.0f32))

    @Test
    def isFinite02(): Unit \ Assert = assertTrue(Float32.isFinite(-1.0f32))

    @Test
    def isFinite03(): Unit \ Assert = assertTrue(Float32.isFinite(Float32.minValue()))

    @Test
    def isFinite04(): Unit \ Assert = assertTrue(Float32.isFinite(Float32.maxValue()))

    @Test
    def isFinite05(): Unit \ Assert = assertFalse(Float32.isFinite(Float32.negativeInfinity()))

    @Test
    def isFinite06(): Unit \ Assert = assertFalse(Float32.isFinite(Float32.positiveInfinity()))

    @Test
    def isFinite07(): Unit \ Assert = assertFalse(Float32.isFinite(Float32.nan()))

    /////////////////////////////////////////////////////////////////////////////
    // isInfinite                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def isInfinite01(): Unit \ Assert = assertFalse(Float32.isInfinite(1.0f32))

    @Test
    def isInfinite02(): Unit \ Assert = assertFalse(Float32.isInfinite(-1.0f32))

    @Test
    def isInfinite03(): Unit \ Assert = assertFalse(Float32.isInfinite(Float32.minValue()))

    @Test
    def isInfinite04(): Unit \ Assert = assertFalse(Float32.isInfinite(Float32.maxValue()))

    @Test
    def isInfinite05(): Unit \ Assert = assertTrue(Float32.isInfinite(Float32.negativeInfinity()))

    @Test
    def isInfinite06(): Unit \ Assert = assertTrue(Float32.isInfinite(Float32.positiveInfinity()))

    @Test
    def isInfinite07(): Unit \ Assert = assertFalse(Float32.isInfinite(Float32.nan()))

    /////////////////////////////////////////////////////////////////////////////
    // isNan                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def isNan01(): Unit \ Assert = assertTrue(Float32.isNan(Float32.nan()))

    @Test
    def isNan02(): Unit \ Assert = assertTrue(Float32.isNan(0.0f32 / 0.0f32))

    @Test
    def isNan03(): Unit \ Assert = assertFalse(Float32.isNan(Float32.positiveInfinity()))

    @Test
    def isNan04(): Unit \ Assert = assertFalse(Float32.isNan(Float32.negativeInfinity()))

    @Test
    def isNan05(): Unit \ Assert = assertFalse(Float32.isNan(1.0f32))

    @Test
    def isNan06(): Unit \ Assert = assertFalse(Float32.isNan(-1.0f32))

    @Test
    def isNan07(): Unit \ Assert = assertFalse(Float32.isNan(Float32.minValue()))

    @Test
    def isNan08(): Unit \ Assert = assertFalse(Float32.isNan(Float32.maxValue()))

    /////////////////////////////////////////////////////////////////////////////
    // min                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def min01(): Unit \ Assert = assertEq(expected = 1.0f32, Float32.min(1.0f32, 2.0f32))

    @Test
    def min02(): Unit \ Assert = assertEq(expected = -1.0f32, Float32.min(2.0f32, -1.0f32))

    @Test
    def min03(): Unit \ Assert = assertEq(expected = -66.0f32, Float32.min(-33.0f32, -66.0f32))

    /////////////////////////////////////////////////////////////////////////////
    // max                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def max01(): Unit \ Assert = assertEq(expected = 49.0f32, Float32.max(48.0f32, 49.0f32))

    @Test
    def max02(): Unit \ Assert = assertEq(expected = 4.0f32, Float32.max(4.0f32, -16.0f32))

    @Test
    def max03(): Unit \ Assert = assertEq(expected = -16.0f32, Float32.max(-34.0f32, -16.0f32))

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toString01(): Unit \ Assert = assertEq(expected = "0.0", Float32.toString(0.0f32))

    @Test
    def toString02(): Unit \ Assert = assertEq(expected = "1.0", Float32.toString(1.0f32))

    @Test
    def toString03(): Unit \ Assert = assertEq(expected = "-1.0", Float32.toString(-1.0f32))

    @Test
    def toString04(): Unit \ Assert = assertEq(expected = "3.14", Float32.toString(3.14f32))

    @Test
    def toString05(): Unit \ Assert = assertEq(expected = "-3.14", Float32.toString(-3.14f32))

    @Test
    def toString06(): Unit \ Assert = assertEq(expected = "NaN", Float32.toString(Float32.nan()))

    @Test
    def toString07(): Unit \ Assert = assertEq(expected = "Infinity", Float32.toString(Float32.positiveInfinity()))

    @Test
    def toString08(): Unit \ Assert = assertEq(expected = "-Infinity", Float32.toString(Float32.negativeInfinity()))

    /////////////////////////////////////////////////////////////////////////////
    // fromString                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def fromString01(): Unit \ Assert = assertTrue(Float32.fromString("Bad") |> Option.isEmpty)

    @Test
    def fromString02(): Unit \ Assert = assertEq(expected = Some(1.0f32), Float32.fromString(" 1.0"))

    @Test
    def fromString03(): Unit \ Assert = assertEq(expected = Some(1.0f32), Float32.fromString("1.0 "))

    @Test
    def fromString04(): Unit \ Assert = assertEq(expected = Some(0.0f32), Float32.fromString("0.0"))

    @Test
    def fromString05(): Unit \ Assert = assertEq(expected = Some(1.0f32), Float32.fromString("1.0"))

    @Test
    def fromString06(): Unit \ Assert = assertEq(expected = Some(-1.0f32), Float32.fromString("-1.0"))

    @Test
    def fromString07(): Unit \ Assert = assertEq(expected = Some(3.14f32), Float32.fromString("3.14"))

    @Test
    def fromString08(): Unit \ Assert = assertEq(expected = Some(-3.14f32), Float32.fromString("-3.14"))

    @Test
    def fromString09(): Unit \ Assert = match Float32.fromString("NaN") {
        case Some(x)  => assertTrue(Float32.isNan(x))
        case _ => fail("Expected Some(NaN)")
    }

    @Test
    def fromString10(): Unit \ Assert = assertEq(expected = Some(Float32.positiveInfinity()), Float32.fromString("Infinity"))

    @Test
    def fromString11(): Unit \ Assert = assertEq(expected = Some(Float32.negativeInfinity()), Float32.fromString("-Infinity"))

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt8                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToInt801(): Unit \ Assert = assertEq(expected = Some(0i8), Float32.tryToInt8(0.0f32))

    @Test
    def tryToInt802(): Unit \ Assert = assertEq(expected = Some(1i8), Float32.tryToInt8(1.0f32))

    @Test
    def tryToInt803(): Unit \ Assert = assertEq(expected = Some(-1i8), Float32.tryToInt8(-1.0f32))

    @Test
    def tryToInt804(): Unit \ Assert = assertEq(expected = Some(127i8), Float32.tryToInt8(127.0f32))

    @Test
    def tryToInt805(): Unit \ Assert = assertEq(expected = None, Float32.tryToInt8(128.0f32))

    @Test
    def tryToInt806(): Unit \ Assert = assertEq(expected = Some(-128i8), Float32.tryToInt8(-128.0f32))

    @Test
    def tryToInt807(): Unit \ Assert = assertEq(expected = None, Float32.tryToInt8(-129.0f32))

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt16                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToInt1601(): Unit \ Assert = assertEq(expected = Some(0i16), Float32.tryToInt16(0.0f32))

    @Test
    def tryToInt1602(): Unit \ Assert = assertEq(expected = Some(1i16), Float32.tryToInt16(1.0f32))

    @Test
    def tryToInt1603(): Unit \ Assert = assertEq(expected = Some(-1i16), Float32.tryToInt16(-1.0f32))

    @Test
    def tryToInt1604(): Unit \ Assert = assertEq(expected = Some(32767i16), Float32.tryToInt16(32767.0f32))

    @Test
    def tryToInt1605(): Unit \ Assert = assertEq(expected = None, Float32.tryToInt16(32768.0f32))

    @Test
    def tryToInt1606(): Unit \ Assert = assertEq(expected = Some(-32768i16), Float32.tryToInt16(-32768.0f32))

    @Test
    def tryToInt1607(): Unit \ Assert = assertEq(expected = None, Float32.tryToInt16(-32769.0f32))

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt32                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToInt3201(): Unit \ Assert = assertEq(expected = Some(0), Float32.tryToInt32(0.0f32))

    @Test
    def tryToInt3202(): Unit \ Assert = assertEq(expected = Some(1), Float32.tryToInt32(1.0f32))

    @Test
    def tryToInt3203(): Unit \ Assert = assertEq(expected = Some(-1), Float32.tryToInt32(-1.0f32))

    @Test
    def tryToInt3204(): Unit \ Assert = assertSome(Float32.tryToInt32(2147483647.0f32))

    @Test
    def tryToInt3205(): Unit \ Assert = assertNone(Float32.tryToInt32(2147484000.0f32))        // Allow imprecision

    @Test
    def tryToInt3206(): Unit \ Assert = assertSome(Float32.tryToInt32(-2147483648.0f32))

    @Test
    def tryToInt3207(): Unit \ Assert = assertNone(Float32.tryToInt32(-2147484000.0f32))       // Allow imprecision

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt64                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToInt6401(): Unit \ Assert = assertEq(expected = Some(0i64), Float32.tryToInt64(0.0f32))

    @Test
    def tryToInt6402(): Unit \ Assert = assertEq(expected = Some(1i64), Float32.tryToInt64(1.0f32))

    @Test
    def tryToInt6403(): Unit \ Assert = assertEq(expected = Some(-1i64), Float32.tryToInt64(-1.0f32))

    @Test
    def tryToInt6404(): Unit \ Assert = assertSome(Float32.tryToInt64(9223372036854775807.0f32))

    @Test
    def tryToInt6405(): Unit \ Assert = assertNone(Float32.tryToInt64(9223373000000000000.0f32))        // Allow large imprecision

    @Test
    def tryToInt6406(): Unit \ Assert = assertSome(Float32.tryToInt64(-9223372036854775808.0f32))

    @Test
    def tryToInt6407(): Unit \ Assert = assertNone(Float32.tryToInt64(-9223373000000000000.0f32))       // Allow large imprecision

    /////////////////////////////////////////////////////////////////////////////
    // tryToBigInt                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToBigInt01(): Unit \ Assert = assertEq(expected = Some(0ii), Float32.tryToBigInt(0.0f32))

    @Test
    def tryToBigInt02(): Unit \ Assert = assertEq(expected = Some(1ii), Float32.tryToBigInt(1.0f32))

    @Test
    def tryToBigInt03(): Unit \ Assert = assertEq(expected = Some(-1ii), Float32.tryToBigInt(-1.0f32))

    @Test
    def tryToBigInt04(): Unit \ Assert =
        let step = () -> Float32.maxValue() |> Float32.tryToBigInt;
        assertSome(step())

    @Test
    def tryToBigInt05(): Unit \ Assert =
        let step = () -> Float32.minValue() |> Float32.tryToBigInt;
        assertSome(step())

    /////////////////////////////////////////////////////////////////////////////
    // toFloat64                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toFloat6401(): Unit \ Assert = assertEq(expected = 0.0f64, Float32.toFloat64(0.0f32))

    @Test
    def toFloat6402(): Unit \ Assert = assertEq(expected = 1.0f64, Float32.toFloat64(1.0f32))

    @Test
    def toFloat6403(): Unit \ Assert = assertEq(expected = -1.0f64, Float32.toFloat64(-1.0f32))

    @Test
    def toFloat6404(): Unit \ Assert =
        let d = Float32.toFloat64(123456789.10f32);
        assertTrue(d >= 123456700.0f64 and d <= 123456800.0f64)       // Allow imprecision

    @Test
    def toFloat6405(): Unit \ Assert =
        let d = Float32.toFloat64(-123456789.10f32);
        assertTrue( d <= -123456700.0f64 and d >= -123456800.0f64)     // Allow imprecision

    /////////////////////////////////////////////////////////////////////////////
    // tryToBigDecimal                                                         //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def tryToBigDecimal01(): Unit \ Assert = assertEq(expected = Some(0.0ff), Float32.tryToBigDecimal(0.0f32))

    @Test
    def tryToBigDecimal02(): Unit \ Assert = assertEq(expected = Some(1.0ff), Float32.tryToBigDecimal(1.0f32))

    @Test
    def tryToBigDecimal03(): Unit \ Assert = assertEq(expected = Some(-1.0ff), Float32.tryToBigDecimal(-1.0f32))

    @Test
    def tryToBigDecimal04(): Unit \ Assert =
        match Float32.tryToBigDecimal(123456789.10f32) {
            case Some(d) => assertTrue(d >= 123456700.0ff and d <= 123456800.0ff)       // Allow imprecision
            case None    => fail("Expected Some(BigDecimal)")
        }

    @Test
    def tryToBigDecimal05(): Unit \ Assert =
        match Float32.tryToBigDecimal(-123456789.10f32) {
            case Some(d) => assertTrue( d <= -123456700.0ff and d >= -123456800.0ff)     // Allow imprecision
            case None    => fail("Expected Some(BigDecimal)")
        }

    @Test
    def tryToBigDecimal06(): Unit \ Assert =
        match Float32.tryToBigDecimal(Float32.nan()) {
            case Some(_) => fail("Expected None for NaN")
            case None    => ()
        }

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt8                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clampToInt801(): Unit \ Assert = assertEq(expected = 0i8, Float32.clampToInt8(min = -100i8, max = 100i8, nanValue = -127i8, 0.0f32))

    @Test
    def clampToInt802(): Unit \ Assert = assertEq(expected = 100i8, Float32.clampToInt8(min = -100i8, max = 100i8, nanValue = -127i8, 100.0f32))

    @Test
    def clampToInt803(): Unit \ Assert = assertEq(expected = 100i8, Float32.clampToInt8(min = -100i8, max = 100i8, nanValue = -127i8, 101.0f32))

    @Test
    def clampToInt804(): Unit \ Assert = assertEq(expected = -100i8, Float32.clampToInt8(min = -100i8, max = 100i8, nanValue = -127i8, -100.0f32))

    @Test
    def clampToInt805(): Unit \ Assert = assertEq(expected = -100i8, Float32.clampToInt8(min = -100i8, max = 100i8, nanValue = -127i8, -101.0f32))

    @Test
    def clampToInt806(): Unit \ Assert = assertEq(expected = 100i8, Float32.clampToInt8(min = -100i8, max = 100i8, nanValue = -127i8, Float32.maxValue()))

    @Test
    def clampToInt807(): Unit \ Assert = assertEq(expected = -100i8, Float32.clampToInt8(min = -100i8, max = 100i8, nanValue = -127i8, Float32.minValue()))

    @Test
    def clampToInt808(): Unit \ Assert = assertEq(expected = -127i8, Float32.clampToInt8(min = -100i8, max = 100i8, nanValue = -127i8, Float32.nan()))

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt16                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clampToInt1601(): Unit \ Assert = assertEq(expected = 0i16, Float32.clampToInt16(min = -100i16, max = 100i16, nanValue = -127i16, 0.0f32))

    @Test
    def clampToInt1602(): Unit \ Assert = assertEq(expected = 100i16, Float32.clampToInt16(min = -100i16, max = 100i16, nanValue = -127i16, 100.0f32))

    @Test
    def clampToInt1603(): Unit \ Assert = assertEq(expected = 100i16, Float32.clampToInt16(min = -100i16, max = 100i16, nanValue = -127i16, 101.0f32))

    @Test
    def clampToInt1604(): Unit \ Assert = assertEq(expected = -100i16, Float32.clampToInt16(min = -100i16, max = 100i16, nanValue = -127i16, -100.0f32))

    @Test
    def clampToInt1605(): Unit \ Assert = assertEq(expected = -100i16, Float32.clampToInt16(min = -100i16, max = 100i16, nanValue = -127i16, -101.0f32))

    @Test
    def clampToInt1606(): Unit \ Assert = assertEq(expected = 100i16, Float32.clampToInt16(min = -100i16, max = 100i16, nanValue = -127i16, Float32.maxValue()))

    @Test
    def clampToInt1607(): Unit \ Assert = assertEq(expected = -100i16, Float32.clampToInt16(min = -100i16, max = 100i16, nanValue = -127i16, Float32.minValue()))

    @Test
    def clampToInt1608(): Unit \ Assert = assertEq(expected = -127i16, Float32.clampToInt16(min = -100i16, max = 100i16, nanValue = -127i16, Float32.nan()))

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt32                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clampToInt3201(): Unit \ Assert = assertEq(expected = 0i32, Float32.clampToInt32(min = -100i32, max = 100i32, nanValue = -127i32, 0.0f32))

    @Test
    def clampToInt3202(): Unit \ Assert = assertEq(expected = 100i32, Float32.clampToInt32(min = -100i32, max = 100i32, nanValue = -127i32, 100.0f32))

    @Test
    def clampToInt3203(): Unit \ Assert = assertEq(expected = 100i32, Float32.clampToInt32(min = -100i32, max = 100i32, nanValue = -127i32, 101.0f32))

    @Test
    def clampToInt3204(): Unit \ Assert = assertEq(expected = -100i32, Float32.clampToInt32(min = -100i32, max = 100i32, nanValue = -127i32, -100.0f32))

    @Test
    def clampToInt3205(): Unit \ Assert = assertEq(expected = -100i32, Float32.clampToInt32(min = -100i32, max = 100i32, nanValue = -127i32, -101.0f32))

    @Test
    def clampToInt3206(): Unit \ Assert = assertEq(expected = 100i32, Float32.clampToInt32(min = -100i32, max = 100i32, nanValue = -127i32, Float32.maxValue()))

    @Test
    def clampToInt3207(): Unit \ Assert = assertEq(expected = -100i32, Float32.clampToInt32(min = -100i32, max = 100i32, nanValue = -127i32, Float32.minValue()))

    @Test
    def clampToInt3208(): Unit \ Assert = assertEq(expected = -127i32, Float32.clampToInt32(min = -100i32, max = 100i32, nanValue = -127i32, Float32.nan()))

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt64                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def clampToInt6401(): Unit \ Assert = assertEq(expected = 0i64, Float32.clampToInt64(min = -100i64, max = 100i64, nanValue = -127i64, 0.0f32))

    @Test
    def clampToInt6402(): Unit \ Assert = assertEq(expected = 100i64, Float32.clampToInt64(min = -100i64, max = 100i64, nanValue = -127i64, 100.0f32))

    @Test
    def clampToInt6403(): Unit \ Assert = assertEq(expected = 100i64, Float32.clampToInt64(min = -100i64, max = 100i64, nanValue = -127i64, 101.0f32))

    @Test
    def clampToInt6404(): Unit \ Assert = assertEq(expected = -100i64, Float32.clampToInt64(min = -100i64, max = 100i64, nanValue = -127i64, -100.0f32))

    @Test
    def clampToInt6405(): Unit \ Assert = assertEq(expected = -100i64, Float32.clampToInt64(min = -100i64, max = 100i64, nanValue = -127i64, -101.0f32))

    @Test
    def clampToInt6406(): Unit \ Assert = assertEq(expected = 100i64, Float32.clampToInt64(min = -100i64, max = 100i64, nanValue = -127i64, Float32.maxValue()))

    @Test
    def clampToInt6407(): Unit \ Assert = assertEq(expected = -100i64, Float32.clampToInt64(min = -100i64, max = 100i64, nanValue = -127i64, Float32.minValue()))

    @Test
    def clampToInt6408(): Unit \ Assert = assertEq(expected = -127i64, Float32.clampToInt64(min = -100i64, max = 100i64, nanValue = -127i64, Float32.nan()))

    /////////////////////////////////////////////////////////////////////////////
    // abs                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    def equalsEps(x: Float32, y: Float32, eps: Float32): Bool =
        let diff = Float32.abs(x - y);
        let x1 = Float32.abs(x);
        let y1 = Float32.abs(y);
        let largest = if (y1 > x1) y1 else x1;
        if (diff <= largest * eps) true else false

    @Test
    def abs01(): Unit \ Assert =
        assertTrue(equalsEps(Float32.abs(0.0f32), 0.0f32, 0.001f32))

    @Test
    def abs02(): Unit \ Assert =
        assertTrue(equalsEps(Float32.abs(10.1f32), 10.1f32, 0.001f32))

    @Test
    def abs03(): Unit \ Assert =
        assertTrue(equalsEps(Float32.abs(-10.1f32), 10.1f32, 0.001f32))

    @Test
    def abs04(): Unit \ Assert =
        assertTrue(Float32.abs(Float32.nan()) |> Float32.isNan)

    @Test
    def abs05(): Unit \ Assert =
        assertEq(expected = Float32.positiveInfinity(), Float32.abs(Float32.positiveInfinity()))

    @Test
    def abs06(): Unit \ Assert =
        assertEq(expected = Float32.positiveInfinity(), Float32.abs(Float32.negativeInfinity()))

    /////////////////////////////////////////////////////////////////////////////
    // ceil                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def ceil01(): Unit \ Assert =
        assertTrue(equalsEps(Float32.ceil(0.5f32), 1.0f32, 0.001f32))

    @Test
    def ceil02(): Unit \ Assert =
        assertTrue(equalsEps(Float32.ceil(0.1f32), 1.0f32, 0.001f32))

    @Test
    def ceil03(): Unit \ Assert =
        assertTrue(equalsEps(Float32.ceil(1.0f32), 1.0f32, 0.001f32))

    @Test
    def ceil04(): Unit \ Assert =
        assertTrue(equalsEps(Float32.ceil(0.0f32), 0.0f32, 0.001f32))

    @Test
    def ceil05(): Unit \ Assert =
        assertTrue(equalsEps(Float32.ceil(-0.1f32), 0.0f32, 0.001f32))

    @Test
    def ceil06(): Unit \ Assert =
        assertTrue(equalsEps(Float32.ceil(-0.9f32), 0.0f32, 0.001f32))

    @Test
    def ceil07(): Unit \ Assert =
        assertTrue(Float32.ceil(Float32.nan()) |> Float32.isNan)

    @Test
    def ceil08(): Unit \ Assert =
        assertEq(expected = Float32.positiveInfinity(), Float32.ceil(Float32.positiveInfinity()))

    @Test
    def ceil09(): Unit \ Assert =
        assertEq(expected = Float32.negativeInfinity(), Float32.ceil(Float32.negativeInfinity()))

    /////////////////////////////////////////////////////////////////////////////
    // floor                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def floor01(): Unit \ Assert =
        assertTrue(equalsEps(Float32.floor(0.5f32), 0.0f32, 0.001f32))

    @Test
    def floor02(): Unit \ Assert =
        assertTrue(equalsEps(Float32.floor(0.1f32), 0.0f32, 0.001f32))

    @Test
    def floor03(): Unit \ Assert =
        assertTrue(equalsEps(Float32.floor(1.0f32), 1.0f32, 0.001f32))

    @Test
    def floor04(): Unit \ Assert =
        assertTrue(equalsEps(Float32.floor(0.0f32), 0.0f32, 0.001f32))

    @Test
    def floor05(): Unit \ Assert =
        assertTrue(equalsEps(Float32.floor(-0.1f32), -1.0f32, 0.001f32))

    @Test
    def floor06(): Unit \ Assert =
        assertTrue(equalsEps(Float32.floor(-0.9f32), -1.0f32, 0.001f32))

    @Test
    def floor07(): Unit \ Assert =
        assertTrue(Float32.floor(Float32.nan()) |> Float32.isNan)

    @Test
    def floor08(): Unit \ Assert =
        assertEq(expected = Float32.positiveInfinity(), Float32.floor(Float32.positiveInfinity()))

    @Test
    def floor09(): Unit \ Assert =
        assertEq(expected = Float32.negativeInfinity(), Float32.floor(Float32.negativeInfinity()))

    /////////////////////////////////////////////////////////////////////////////
    // round                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def round01(): Unit \ Assert =
        assertTrue(equalsEps(Float32.round(0.5f32), 0.0f32, 0.001f32))

    @Test
    def round02(): Unit \ Assert =
        assertTrue(equalsEps(Float32.round(0.1f32), 0.0f32, 0.001f32))

    @Test
    def round03(): Unit \ Assert =
        assertTrue(equalsEps(Float32.round(1.0f32), 1.0f32, 0.001f32))

    @Test
    def round04(): Unit \ Assert =
        assertTrue(equalsEps(Float32.round(0.0f32), 0.0f32, 0.001f32))

    @Test
    def round05(): Unit \ Assert =
        assertTrue(equalsEps(Float32.round(-0.1f32), 0.0f32, 0.001f32))

    @Test
    def round06(): Unit \ Assert =
        assertTrue(equalsEps(Float32.round(-0.9f32), -1.0f32, 0.001f32))

    @Test
    def round07(): Unit \ Assert =
        assertTrue(Float32.round(Float32.nan()) |> Float32.isNan)

    @Test
    def round08(): Unit \ Assert =
        assertEq(expected = Float32.positiveInfinity(), Float32.round(Float32.positiveInfinity()))

    @Test
    def round09(): Unit \ Assert =
        assertEq(expected = Float32.negativeInfinity(), Float32.round(Float32.negativeInfinity()))

    @Test
    def round10(): Unit \ Assert =
        assertTrue(equalsEps(Float32.round(1.5f32), 2.0f32, 0.001f32))

    @Test
    def round11(): Unit \ Assert =
        assertTrue(equalsEps(Float32.round(2.5f32), 2.0f32, 0.001f32))

    /////////////////////////////////////////////////////////////////////////////
    // pow                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def pow01(): Unit \ Assert = assertTrue(equalsEps(Float32.pow(base = 0.0f32, 0.0f32), 1.0f32, 0.001f32))

    @Test
    def pow02(): Unit \ Assert = assertTrue(equalsEps(Float32.pow(base = 0.0f32, 1.0f32), 0.0f32, 0.001f32))

    @Test
    def pow03(): Unit \ Assert = assertTrue(equalsEps(Float32.pow(base = 1.0f32, 0.0f32), 1.0f32, 0.001f32))

    @Test
    def pow04(): Unit \ Assert = assertTrue(equalsEps(Float32.pow(base = 1.0f32, 1.0f32), 1.0f32, 0.001f32))

    @Test
    def pow05(): Unit \ Assert = assertTrue(equalsEps(Float32.pow(base = 5.0f32, 2.0f32), 25.0f32, 0.001f32))

    /////////////////////////////////////////////////////////////////////////////
    // floatValue                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def floatValue01(): Unit \ Assert =
        let d = Float32.valueOf(0.0f32);
        assertEq(expected = 0.0f32, Float32.floatValue(d))

    @Test
    def floatValue02(): Unit \ Assert =
        let d = Float32.valueOf(1.0f32);
        assertEq(expected = 1.0f32, Float32.floatValue(d))

    @Test
    def floatValue03(): Unit \ Assert =
        let d = Float32.valueOf(-1.0f32);
        assertEq(expected = -1.0f32, Float32.floatValue(d))

    /////////////////////////////////////////////////////////////////////////////
    // valueOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def valueOf01(): Unit \ Assert =
        let d = Float32.valueOf(0.0f32);
        assertTrue(d.equals(Float32.valueOf(0.0f32)))

    @Test
    def valueOf02(): Unit \ Assert =
        let d = Float32.valueOf(1.0f32);
        assertTrue(d.equals(Float32.valueOf(1.0f32)))

    @Test
    def valueOf03(): Unit \ Assert =
        let d = Float32.valueOf(-1.0f32);
        assertTrue(d.equals(Float32.valueOf(-1.0f32)))

}
