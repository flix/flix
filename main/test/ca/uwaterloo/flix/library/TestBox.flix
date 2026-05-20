mod TestBox {

    use Assert.{assertEq, assertTrue};
    use Box.{box, unbox}
    import java.lang.Boolean
    import java.lang.Character
    import java.lang.Float
    import java.lang.Double
    import java.lang.Byte
    import java.lang.Short
    import java.lang.Integer
    import java.lang.Long

    /////////////////////////////////////////////////////////////////////////////
    // Bool                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def boolUnbox01(): Unit \ Assert = assertEq(expected = true, unbox(Boolean.valueOf(true)))

    @Test
    def boolUnbox02(): Unit \ Assert = assertEq(expected = false, unbox(Boolean.valueOf(false)))

    @Test
    def boolBox01(): Unit \ Assert = assertTrue(box(true).equals(Boolean.valueOf(true)))

    @Test
    def boolBox02(): Unit \ Assert = assertTrue(box(false).equals(Boolean.valueOf(false)))

    @Test
    def boolRoundTrip01(): Unit \ Assert = assertEq(expected = true, unbox(box(true)))

    @Test
    def boolRoundTrip02(): Unit \ Assert = assertEq(expected = false, unbox(box(false)))

    /////////////////////////////////////////////////////////////////////////////
    // Char                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def charUnbox01(): Unit \ Assert = assertEq(expected = 'a', unbox(Character.valueOf('a')))

    @Test
    def charUnbox02(): Unit \ Assert = assertEq(expected = 'Z', unbox(Character.valueOf('Z')))

    @Test
    def charBox01(): Unit \ Assert = assertTrue(box('a').equals(Character.valueOf('a')))

    @Test
    def charBox02(): Unit \ Assert = assertTrue(box('Z').equals(Character.valueOf('Z')))

    @Test
    def charRoundTrip01(): Unit \ Assert = assertEq(expected = 'a', unbox(box('a')))

    @Test
    def charRoundTrip02(): Unit \ Assert = assertEq(expected = 'Z', unbox(box('Z')))

    /////////////////////////////////////////////////////////////////////////////
    // Float32                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def float32Unbox01(): Unit \ Assert = assertEq(expected = 1.23f32, unbox(Float.valueOf(1.23f32)))

    @Test
    def float32Unbox02(): Unit \ Assert = assertEq(expected = -4.56f32, unbox(Float.valueOf(-4.56f32)))

    @Test
    def float32Box01(): Unit \ Assert = assertTrue(box(1.23f32).equals(Float.valueOf(1.23f32)))

    @Test
    def float32Box02(): Unit \ Assert = assertTrue(box(-4.56f32).equals(Float.valueOf(-4.56f32)))

    @Test
    def float32RoundTrip01(): Unit \ Assert = assertEq(expected = 1.23f32, unbox(box(1.23f32)))

    @Test
    def float32RoundTrip02(): Unit \ Assert = assertEq(expected = -4.56f32, unbox(box(-4.56f32)))

    @Test
    def float32MaxValue(): Unit \ Assert = assertEq(expected = Float.MAX_VALUE, unbox(box(Float.MAX_VALUE)))

    @Test
    def float32MinValue(): Unit \ Assert = assertEq(expected = Float.MIN_VALUE, unbox(box(Float.MIN_VALUE)))

    /////////////////////////////////////////////////////////////////////////////
    // Float64                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def float64Unbox01(): Unit \ Assert = assertEq(expected = 1.23, unbox(Double.valueOf(1.23)))

    @Test
    def float64Unbox02(): Unit \ Assert = assertEq(expected = -4.56, unbox(Double.valueOf(-4.56)))

    @Test
    def float64Box01(): Unit \ Assert = assertTrue(box(1.23).equals(Double.valueOf(1.23)))

    @Test
    def float64Box02(): Unit \ Assert = assertTrue(box(-4.56).equals(Double.valueOf(-4.56)))

    @Test
    def float64RoundTrip01(): Unit \ Assert = assertEq(expected = 1.23, unbox(box(1.23)))

    @Test
    def float64RoundTrip02(): Unit \ Assert = assertEq(expected = -4.56, unbox(box(-4.56)))

    @Test
    def float64MaxValue(): Unit \ Assert = assertEq(expected = Double.MAX_VALUE, unbox(box(Double.MAX_VALUE)))

    @Test
    def float64MinValue(): Unit \ Assert = assertEq(expected = Double.MIN_VALUE, unbox(box(Double.MIN_VALUE)))

    /////////////////////////////////////////////////////////////////////////////
    // Int8                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def int8Unbox01(): Unit \ Assert = assertEq(expected = 42i8, unbox(Byte.valueOf(42i8)))

    @Test
    def int8Unbox02(): Unit \ Assert = assertEq(expected = -128i8, unbox(Byte.valueOf(-128i8)))

    @Test
    def int8Box01(): Unit \ Assert = assertTrue(box(42i8).equals(Byte.valueOf(42i8)))

    @Test
    def int8Box02(): Unit \ Assert = assertTrue(box(-128i8).equals(Byte.valueOf(-128i8)))

    @Test
    def int8RoundTrip01(): Unit \ Assert = assertEq(expected = 42i8, unbox(box(42i8)))

    @Test
    def int8RoundTrip02(): Unit \ Assert = assertEq(expected = -128i8, unbox(box(-128i8)))

    @Test
    def int8MaxValue(): Unit \ Assert = assertEq(expected = Byte.MAX_VALUE, unbox(box(Byte.MAX_VALUE)))

    @Test
    def int8MinValue(): Unit \ Assert = assertEq(expected = Byte.MIN_VALUE, unbox(box(Byte.MIN_VALUE)))

    /////////////////////////////////////////////////////////////////////////////
    // Int16                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def int16Unbox01(): Unit \ Assert = assertEq(expected = 12345i16, unbox(Short.valueOf(12345i16)))

    @Test
    def int16Unbox02(): Unit \ Assert = assertEq(expected = -32768i16, unbox(Short.valueOf(-32768i16)))

    @Test
    def int16Box01(): Unit \ Assert = assertTrue(box(12345i16).equals(Short.valueOf(12345i16)))

    @Test
    def int16Box02(): Unit \ Assert = assertTrue(box(-32768i16).equals(Short.valueOf(-32768i16)))

    @Test
    def int16RoundTrip01(): Unit \ Assert = assertEq(expected = 12345i16, unbox(box(12345i16)))

    @Test
    def int16RoundTrip02(): Unit \ Assert = assertEq(expected = -32768i16, unbox(box(-32768i16)))

    @Test
    def int16MaxValue(): Unit \ Assert = assertEq(expected = Short.MAX_VALUE, unbox(box(Short.MAX_VALUE)))

    @Test
    def int16MinValue(): Unit \ Assert = assertEq(expected = Short.MIN_VALUE, unbox(box(Short.MIN_VALUE)))

    /////////////////////////////////////////////////////////////////////////////
    // Int32                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def int32Unbox01(): Unit \ Assert = assertEq(expected = 1234567890i32, unbox(Integer.valueOf(1234567890i32)))

    @Test
    def int32Unbox02(): Unit \ Assert = assertEq(expected = -2147483648i32, unbox(Integer.valueOf(-2147483648i32)))

    @Test
    def int32Box01(): Unit \ Assert = assertTrue(box(1234567890i32).equals(Integer.valueOf(1234567890i32)))

    @Test
    def int32Box02(): Unit \ Assert = assertTrue(box(-2147483648i32).equals(Integer.valueOf(-2147483648i32)))

    @Test
    def int32RoundTrip01(): Unit \ Assert = assertEq(expected = 1234567890i32, unbox(box(1234567890i32)))

    @Test
    def int32RoundTrip02(): Unit \ Assert = assertEq(expected = -2147483648i32, unbox(box(-2147483648i32)))

    @Test
    def int32MaxValue(): Unit \ Assert = assertEq(expected = Integer.MAX_VALUE, unbox(box(Integer.MAX_VALUE)))

    @Test
    def int32MinValue(): Unit \ Assert = assertEq(expected = Integer.MIN_VALUE, unbox(box(Integer.MIN_VALUE)))

    /////////////////////////////////////////////////////////////////////////////
    // Int64                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def int64Unbox01(): Unit \ Assert = assertEq(expected = 1234567890123456789i64, unbox(Long.valueOf(1234567890123456789i64)))

    @Test
    def int64Unbox02(): Unit \ Assert = assertEq(expected = -9223372036854775808i64, unbox(Long.valueOf(-9223372036854775808i64)))

    @Test
    def int64Box01(): Unit \ Assert = assertTrue(box(1234567890123456789i64).equals(Long.valueOf(1234567890123456789i64)))

    @Test
    def int64Box02(): Unit \ Assert = assertTrue(box(-9223372036854775808i64).equals(Long.valueOf(-9223372036854775808i64)))

    @Test
    def int64RoundTrip01(): Unit \ Assert = assertEq(expected = 1234567890123456789i64, unbox(box(1234567890123456789i64)))

    @Test
    def int64RoundTrip02(): Unit \ Assert = assertEq(expected = -9223372036854775808i64, unbox(box(-9223372036854775808i64)))

    @Test
    def int64MaxValue(): Unit \ Assert = assertEq(expected = Long.MAX_VALUE, unbox(box(Long.MAX_VALUE)))

    @Test
    def int64MinValue(): Unit \ Assert = assertEq(expected = Long.MIN_VALUE, unbox(box(Long.MIN_VALUE)))

}
