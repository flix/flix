mod TestBoxable {

    use Assert.{assertEq, assertTrue};
    use Fixpoint.Boxable.{box, unbox}

    /////////////////////////////////////////////////////////////////////////////
    // Unit                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unitUnbox01(): Unit \ Assert = assertEq(expected = (), unbox(box(())))

    @Test
    def unitCompare01(): Unit \ Assert = assertEq(expected = box(()), box(()))

    @Test
    def unitToString01(): Unit \ Assert = assertEq(expected = "()", "${box(())}")

    /////////////////////////////////////////////////////////////////////////////
    // Bool                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def boolUnbox01(): Unit \ Assert = assertEq(expected = false, unbox(box(false)))

    @Test
    def boolUnbox02(): Unit \ Assert = assertEq(expected = true, unbox(box(true)))

    @Test
    def boolCompare01(): Unit \ Assert = assertEq(expected = box(true), box(true))

    @Test
    def boolCompare02(): Unit \ Assert = assertTrue(box(false) < box(true))

    @Test
    def boolToString01(): Unit \ Assert = assertEq(expected = "true", "${box(true)}")

    /////////////////////////////////////////////////////////////////////////////
    // Char                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def charUnbox01(): Unit \ Assert = assertEq(expected = 'a', unbox(box('a')))

    @Test
    def charUnbox02(): Unit \ Assert = assertEq(expected = 'A', unbox(box('A')))

    @Test
    def charUnbox03(): Unit \ Assert = assertEq(expected = ' ', unbox(box(' ')))

    @Test
    def charUnbox04(): Unit \ Assert = assertEq(expected = '+', unbox(box('+')))

    @Test
    def charCompare01(): Unit \ Assert = assertEq(expected = box('a'), box('a'))

    @Test
    def charCompare02(): Unit \ Assert = assertTrue(box('a') < box('b'))

    @Test
    def charToString01(): Unit \ Assert = assertEq(expected = "V", "${box('V')}")

    /////////////////////////////////////////////////////////////////////////////
    // Float32                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def float32Unbox01(): Unit \ Assert = assertEq(expected = 0.0f32, unbox(box(0.0f32)))

    @Test
    def float32Unbox02(): Unit \ Assert = assertEq(expected = 1.0f32, unbox(box(1.0f32)))

    @Test
    def float32Unbox03(): Unit \ Assert = assertEq(expected = -1.0f32, unbox(box(-1.0f32)))

    @Test
    def float32Unbox04(): Unit \ Assert = assertEq(expected = 3.14f32, unbox(box(3.14f32)))

    @Test
    def float32Compare01(): Unit \ Assert = assertEq(expected = box(0.0f32), box(0.0f32))

    @Test
    def float32Compare02(): Unit \ Assert = assertTrue(box(0.0f32) < box(1.0f32))

    @Test
    def float32ToString01(): Unit \ Assert = assertEq(expected = "123.0", "${box(123.0f32)}")

    /////////////////////////////////////////////////////////////////////////////
    // Float64                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def float64Unbox01(): Unit \ Assert = assertEq(expected = 0.0f64, unbox(box(0.0f64)))

    @Test
    def float64Unbox02(): Unit \ Assert = assertEq(expected = 1.0f64, unbox(box(1.0f64)))

    @Test
    def float64Unbox03(): Unit \ Assert = assertEq(expected = -1.0f64, unbox(box(-1.0f64)))

    @Test
    def float64Unbox04(): Unit \ Assert = assertEq(expected = 3.14f64, unbox(box(3.14f64)))

    @Test
    def float64Compare01(): Unit \ Assert = assertEq(expected = box(0.0f64), box(0.0f64))

    @Test
    def float64Compare02(): Unit \ Assert = assertTrue(box(0.0f64) < box(1.0f64))

    @Test
    def float64ToString01(): Unit \ Assert = assertEq(expected = "123.0", "${box(123.0f64)}")

    /////////////////////////////////////////////////////////////////////////////
    // BigDecimal                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bigDecimalUnbox01(): Unit \ Assert = assertEq(expected = 0.0ff, unbox(box(0.0ff)))

    @Test
    def bigDecimalUnbox02(): Unit \ Assert = assertEq(expected = 1.0ff, unbox(box(1.0ff)))

    @Test
    def bigDecimalUnbox03(): Unit \ Assert = assertEq(expected = -1.0ff, unbox(box(-1.0ff)))

    @Test
    def bigDecimalUnbox04(): Unit \ Assert = assertEq(expected = 3.14ff, unbox(box(3.14ff)))

    @Test
    def bigDecimalCompare01(): Unit \ Assert = assertEq(expected = box(0.0ff), box(0.0ff))

    @Test
    def bigDecimalCompare02(): Unit \ Assert = assertTrue(box(0.0ff) < box(1.0ff))

    @Test
    def bigDecimalToString01(): Unit \ Assert = assertEq(expected = "123.0", "${box(123.0ff)}")

    /////////////////////////////////////////////////////////////////////////////
    // Int8                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def int8Unbox01(): Unit \ Assert = assertEq(expected = 0i8, unbox(box(0i8)))

    @Test
    def int8Unbox02(): Unit \ Assert = assertEq(expected = 1i8, unbox(box(1i8)))

    @Test
    def int8Unbox03(): Unit \ Assert = assertEq(expected = -128i8, unbox(box(-128i8)))

    @Test
    def int8Unbox04(): Unit \ Assert = assertEq(expected = 127i8, unbox(box(127i8)))

    @Test
    def int8Compare01(): Unit \ Assert = assertEq(expected = box(0i8), box(0i8))

    @Test
    def int8Compare02(): Unit \ Assert = assertTrue(box(0i8) < box(1i8))

    @Test
    def int8ToString01(): Unit \ Assert = assertEq(expected = "123", "${box(123i8)}")

    /////////////////////////////////////////////////////////////////////////////
    // Int16                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def int16Unbox01(): Unit \ Assert = assertEq(expected = 0i16, unbox(box(0i16)))

    @Test
    def int16Unbox02(): Unit \ Assert = assertEq(expected = 1i16, unbox(box(1i16)))

    @Test
    def int16Unbox03(): Unit \ Assert = assertEq(expected = -32768i16, unbox(box(-32768i16)))

    @Test
    def int16Unbox04(): Unit \ Assert = assertEq(expected = 32767i16, unbox(box(32767i16)))

    @Test
    def int16Compare01(): Unit \ Assert = assertEq(expected = box(0i16), box(0i16))

    @Test
    def int16Compare02(): Unit \ Assert = assertTrue(box(0i16) < box(1i16))

    @Test
    def int16ToString01(): Unit \ Assert = assertEq(expected = "123", "${box(123i16)}")

    /////////////////////////////////////////////////////////////////////////////
    // Int32                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def int32Unbox01(): Unit \ Assert = assertEq(expected = 0i32, unbox(box(0)))

    @Test
    def int32Unbox02(): Unit \ Assert = assertEq(expected = 1i32, unbox(box(1)))

    @Test
    def int32Unbox03(): Unit \ Assert = assertEq(expected = -2147483648i32, unbox(box(-2147483648)))

    @Test
    def int32Unbox04(): Unit \ Assert = assertEq(expected = 2147483647i32, unbox(box(2147483647)))

    @Test
    def int32Compare01(): Unit \ Assert = assertEq(expected = box(0i32), box(0i32))

    @Test
    def int32Compare02(): Unit \ Assert = assertTrue(box(0i32) < box(1i32))

    @Test
    def int32ToString01(): Unit \ Assert = assertEq(expected = "123", "${box(123i32)}")

    /////////////////////////////////////////////////////////////////////////////
    // Int64                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def int64Unbox01(): Unit \ Assert = assertEq(expected = 0i64, unbox(box(0i64)))

    @Test
    def int64Unbox02(): Unit \ Assert = assertEq(expected = 1i64, unbox(box(1i64)))

    @Test
    def int64Unbox03(): Unit \ Assert = assertEq(expected = -9223372036854775808i64, unbox(box(-9223372036854775808i64)))

    @Test
    def int64Unbox04(): Unit \ Assert = assertEq(expected = 9223372036854775807i64, unbox(box(9223372036854775807i64)))

    @Test
    def int64Compare01(): Unit \ Assert = assertEq(expected = box(0i64), box(0i64))

    @Test
    def int64Compare02(): Unit \ Assert = assertTrue(box(0i64) < box(1i64))

    @Test
    def int64ToString01(): Unit \ Assert = assertEq(expected = "123", "${box(123i64)}")

    /////////////////////////////////////////////////////////////////////////////
    // String                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def stringUnbox01(): Unit \ Assert = assertEq(expected = "", unbox(box("")))

    @Test
    def stringUnbox02(): Unit \ Assert = assertEq(expected = "a", unbox(box("a")))

    @Test
    def stringUnbox03(): Unit \ Assert = assertEq(expected = ".", unbox(box(".")))

    @Test
    def stringUnbox04(): Unit \ Assert = assertEq(expected = ".#)!933", unbox(box(".#)!933")))

    @Test
    def stringCompare01(): Unit \ Assert = assertEq(expected = box(""), box(""))

    @Test
    def stringCompare02(): Unit \ Assert = assertTrue(box("a") < box("b"))

    @Test
    def stringToString01(): Unit \ Assert = assertEq(expected = "abc", "${box("abc")}")

    /////////////////////////////////////////////////////////////////////////////
    // BigInt                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bigIntUnbox01(): Unit \ Assert = assertEq(expected = 0ii, unbox(box(0ii)))

    @Test
    def bigIntUnbox02(): Unit \ Assert = assertEq(expected = 1ii, unbox(box(1ii)))

    @Test
    def bigIntUnbox03(): Unit \ Assert = assertEq(expected = -3498457932459234592873452983453245ii, unbox(box(-3498457932459234592873452983453245ii)))

    @Test
    def bigIntUnbox04(): Unit \ Assert = assertEq(expected = 3498457932459234592873452983453245ii, unbox(box(3498457932459234592873452983453245ii)))

    @Test
    def bigIntCompare01(): Unit \ Assert = assertEq(expected = box(0ii), box(0ii))

    @Test
    def bigIntCompare02(): Unit \ Assert = assertTrue(box(0ii) < box(1ii))

    @Test
    def bigIntToString01(): Unit \ Assert = assertEq(expected = "123", "${box(123ii)}")

    /////////////////////////////////////////////////////////////////////////////
    // Tuple2                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple2Unbox01(): Unit \ Assert = assertEq(expected = (1, 1), unbox(box((1, 1))))

    @Test
    def tuple2Unbox02(): Unit \ Assert = assertEq(expected = (1, Some(2)), unbox(box((1, Some(2)))))

    @Test
    def tuple2Unbox03(): Unit \ Assert = assertEq(expected = (true, 1), unbox(box((true, 1))))

    @Test
    def tuple2Unbox04(): Unit \ Assert = assertEq(expected = ((1, 2), 91), unbox(box(((1, 2), 91))))

    @Test
    def tuple2Compare01(): Unit \ Assert = assertEq(expected = box((0, 0)), box((0, 0)))

    @Test
    def tuple2Compare02(): Unit \ Assert = assertTrue(box((0, 0)) < box((0, 1)))

    @Test
    def tuple2ToString01(): Unit \ Assert = assertEq(expected = "(1, 2)", "${box((1, 2))}")

    /////////////////////////////////////////////////////////////////////////////
    // Tuple3                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple3Unbox01(): Unit \ Assert = assertEq(expected = (1, 1, 82), unbox(box((1, 1, 82))))

    @Test
    def tuple3Unbox02(): Unit \ Assert = assertEq(expected = (1, Some(2), 3), unbox(box((1, Some(2), 3))))

    @Test
    def tuple3Unbox03(): Unit \ Assert = assertEq(expected = (true, 1, false), unbox(box((true, 1, false))))

    @Test
    def tuple3Unbox04(): Unit \ Assert = assertEq(expected = ((1, 2), 91, true), unbox(box(((1, 2), 91, true))))

    @Test
    def tuple3Compare01(): Unit \ Assert = assertEq(expected = box((0, 0, 0)), box((0, 0, 0)))

    @Test
    def tuple3Compare02(): Unit \ Assert = assertTrue(box((0, 0, 0)) < box((0, 0, 1)))

    @Test
    def tuple3ToString01(): Unit \ Assert = assertEq(expected = "(1, 2, 3)", "${box((1, 2, 3))}")
}
