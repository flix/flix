mod TestToString {

    use ToString.toString;
    use Assert.assertEq;

    /////////////////////////////////////////////////////////////////////////////
    // Unit                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unitToString01(): Unit \ Assert =
        assertEq(expected = "()", toString(()))

    /////////////////////////////////////////////////////////////////////////////
    // Bool                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def boolToString01(): Unit \ Assert =
        assertEq(expected = "false", toString(false))

    @Test
    def boolToString02(): Unit \ Assert =
        assertEq(expected = "true", toString(true))

    /////////////////////////////////////////////////////////////////////////////
    // Char                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def charToString01(): Unit \ Assert =
        assertEq(expected = "a", toString('a'))

    @Test
    def charToString02(): Unit \ Assert =
        assertEq(expected = "A", toString('A'))

    @Test
    def charToString03(): Unit \ Assert =
        assertEq(expected = " ", toString(' '))

    @Test
    def charToString04(): Unit \ Assert =
        assertEq(expected = "+", toString('+'))

    /////////////////////////////////////////////////////////////////////////////
    // Float32                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def float32ToString01(): Unit \ Assert =
        assertEq(expected = "0.0", toString(0.0f32))

    @Test
    def float32ToString02(): Unit \ Assert =
        assertEq(expected = "1.0", toString(1.0f32))

    @Test
    def float32ToString03(): Unit \ Assert =
        assertEq(expected = "-1.0", toString(-1.0f32))

    @Test
    def float32ToString04(): Unit \ Assert =
        assertEq(expected = "3.14", toString(3.14f32))

    /////////////////////////////////////////////////////////////////////////////
    // Float64                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def float64ToString01(): Unit \ Assert =
        assertEq(expected = "0.0", toString(0.0f64))

    @Test
    def float64ToString02(): Unit \ Assert =
        assertEq(expected = "1.0", toString(1.0f64))

    @Test
    def float64ToString03(): Unit \ Assert =
        assertEq(expected = "-1.0", toString(-1.0f64))

    @Test
    def float64ToString04(): Unit \ Assert =
        assertEq(expected = "3.14", toString(3.14f64))

    /////////////////////////////////////////////////////////////////////////////
    // BigDecimal                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bigDecimalToString01(): Unit \ Assert =
        assertEq(expected = "0.0", toString(0.0ff))

    @Test
    def bigDecimalToString02(): Unit \ Assert =
        assertEq(expected = "1.0", toString(1.0ff))

    @Test
    def bigDecimalToString03(): Unit \ Assert =
        assertEq(expected = "-1.0", toString(-1.0ff))

    @Test
    def bigDecimalToString04(): Unit \ Assert =
        assertEq(expected = "3.14", toString(3.14ff))

    /////////////////////////////////////////////////////////////////////////////
    // Int8                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def int8ToString01(): Unit \ Assert =
        assertEq(expected = "0", toString(0i8))

    @Test
    def int8ToString02(): Unit \ Assert =
        assertEq(expected = "1", toString(1i8))

    @Test
    def int8ToString03(): Unit \ Assert =
        assertEq(expected = "-128", toString(-128i8))

    @Test
    def int8ToString04(): Unit \ Assert =
        assertEq(expected = "127", toString(127i8))

    /////////////////////////////////////////////////////////////////////////////
    // Int16                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def int16ToString01(): Unit \ Assert =
        assertEq(expected = "0", toString(0i16))

    @Test
    def int16ToString02(): Unit \ Assert =
        assertEq(expected = "1", toString(1i16))

    @Test
    def int16ToString03(): Unit \ Assert =
        assertEq(expected = "-32768", toString(-32768i16))

    @Test
    def int16ToString04(): Unit \ Assert =
        assertEq(expected = "32767", toString(32767i16))

    /////////////////////////////////////////////////////////////////////////////
    // Int32                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def int32ToString01(): Unit \ Assert =
        assertEq(expected = "0", toString(0))

    @Test
    def int32ToString02(): Unit \ Assert =
        assertEq(expected = "1", toString(1))

    @Test
    def int32ToString03(): Unit \ Assert =
        assertEq(expected = "-2147483648", toString(-2147483648))

    @Test
    def int32ToString04(): Unit \ Assert =
        assertEq(expected = "2147483647", toString(2147483647))

    /////////////////////////////////////////////////////////////////////////////
    // Int64                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def int64ToString01(): Unit \ Assert =
        assertEq(expected = "0", toString(0i64))

    @Test
    def int64ToString02(): Unit \ Assert =
        assertEq(expected = "1", toString(1i64))

    @Test
    def int64ToString03(): Unit \ Assert =
        assertEq(expected = "-9223372036854775808", toString(-9223372036854775808i64))

    @Test
    def int64ToString04(): Unit \ Assert =
        assertEq(expected = "9223372036854775807", toString(9223372036854775807i64))

    /////////////////////////////////////////////////////////////////////////////
    // String                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def stringToString01(): Unit \ Assert =
        assertEq(expected = "", toString(""))

    @Test
    def stringToString02(): Unit \ Assert =
        assertEq(expected = "a", toString("a"))

    @Test
    def stringToString03(): Unit \ Assert =
        assertEq(expected = ".", toString("."))

    @Test
    def stringToString04(): Unit \ Assert =
        assertEq(expected = ".#)!933", toString(".#)!933"))

    /////////////////////////////////////////////////////////////////////////////
    // BigInt                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bigIntToString01(): Unit \ Assert =
        assertEq(expected = "0", toString(0ii))

    @Test
    def bigIntToString02(): Unit \ Assert =
        assertEq(expected = "1", toString(1ii))

    @Test
    def bigIntToString03(): Unit \ Assert =
        assertEq(expected = "-3498457932459234592873452983453245", toString(-3498457932459234592873452983453245ii))

    @Test
    def bigIntToString04(): Unit \ Assert =
        assertEq(expected = "3498457932459234592873452983453245", toString(3498457932459234592873452983453245ii))

    /////////////////////////////////////////////////////////////////////////////
    // Tuple2                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple2ToString01(): Unit \ Assert =
        assertEq(expected = "(1, 1)", toString((1, 1)))

    @Test
    def tuple2ToString02(): Unit \ Assert =
        assertEq(expected = "(1, Some(2))", toString((1, Some(2))))

    @Test
    def tuple2ToString03(): Unit \ Assert =
        assertEq(expected = "(true, 1)", toString((true, 1)))

    @Test
    def tuple2ToString04(): Unit \ Assert =
        assertEq(expected = "((1, 2), 91)", toString(((1, 2), 91)))

    /////////////////////////////////////////////////////////////////////////////
    // Tuple3                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple3ToString01(): Unit \ Assert =
        assertEq(expected = "(1, 1, 82)", toString((1, 1, 82)))

    @Test
    def tuple3ToString02(): Unit \ Assert =
        assertEq(expected = "(1, Some(2), 3)", toString((1, Some(2), 3)))

    @Test
    def tuple3ToString03(): Unit \ Assert =
        assertEq(expected = "(true, 1, false)", toString((true, 1, false)))

    @Test
    def tuple3ToString04(): Unit \ Assert =
        assertEq(expected = "((1, 2), 91, true)", toString(((1, 2), 91, true)))

    /////////////////////////////////////////////////////////////////////////////
    // Tuple4                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple4ToString01(): Unit \ Assert =
        assertEq(expected = "(1, 1, 2, 3)", toString((1, 1, 2, 3)))

    @Test
    def tuple4ToString02(): Unit \ Assert =
        assertEq(expected = "(1, Some(2), true, false)", toString((1, Some(2), true, false)))

    @Test
    def tuple4ToString03(): Unit \ Assert =
        assertEq(expected = "(true, 1, 97, 2)", toString((true, 1, 97, 2)))

    @Test
    def tuple4ToString04(): Unit \ Assert =
        assertEq(expected = "((1, 2, 4, 7), (1, 2), (1, 2, 4), 4)", toString(((1, 2, 4, 7), (1, 2), (1, 2, 4), 4)))

    /////////////////////////////////////////////////////////////////////////////
    // Tuple5                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple5ToString01(): Unit \ Assert =
        assertEq(expected = "(1, 1, 2, 3, 9)", toString((1, 1, 2, 3, 9)))

    @Test
    def tuple5ToString02(): Unit \ Assert =
        assertEq(expected = "(1, Some(2), true, false, true)", toString((1, Some(2), true, false, true)))

    @Test
    def tuple5ToString03(): Unit \ Assert =
        assertEq(expected = "(true, 1, 97, 2, 9)", toString((true, 1, 97, 2, 9)))

    @Test
    def tuple5ToString04(): Unit \ Assert =
        assertEq(expected = "((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5))", toString(((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5))))

    /////////////////////////////////////////////////////////////////////////////
    // Tuple6                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple6ToString01(): Unit \ Assert =
        assertEq(expected = "(1, 1, 2, 3, 9, 42)", toString((1, 1, 2, 3, 9, 42)))

    @Test
    def tuple6ToString02(): Unit \ Assert =
        assertEq(expected = "(1, Some(2), true, false, true, 0)", toString((1, Some(2), true, false, true, 0)))

    @Test
    def tuple6ToString03(): Unit \ Assert =
        assertEq(expected = "(true, 1, 97, 2, 9, false)", toString((true, 1, 97, 2, 9, false)))

    @Test
    def tuple6ToString04(): Unit \ Assert =
        assertEq(expected = "((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9))", toString(((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9))))

    /////////////////////////////////////////////////////////////////////////////
    // Tuple7                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple7ToString01(): Unit \ Assert =
        assertEq(expected = "(1, 1, 2, 3, 9, 42, 2)", toString((1, 1, 2, 3, 9, 42, 2)))

    @Test
    def tuple7ToString02(): Unit \ Assert =
        assertEq(expected = "(1, Some(2), true, false, true, 0, false)", toString((1, Some(2), true, false, true, 0, false)))

    @Test
    def tuple7ToString03(): Unit \ Assert =
        assertEq(expected = "(true, 1, 97, 2, 9, false, Some(42))", toString((true, 1, 97, 2, 9, false, Some(42))))

    @Test
    def tuple7ToString04(): Unit \ Assert =
        assertEq(expected = "((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9, 1))", toString(((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9, 1))))

    /////////////////////////////////////////////////////////////////////////////
    // Tuple8                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple8ToString01(): Unit \ Assert =
        assertEq(expected = "(1, 1, 2, 3, 9, 42, 2, 0)", toString((1, 1, 2, 3, 9, 42, 2, 0)))

    @Test
    def tuple8ToString02(): Unit \ Assert =
        assertEq(expected = "(1, Some(2), true, false, true, 0, false, Some(true))", toString((1, Some(2), true, false, true, 0, false, Some(true))))

    @Test
    def tuple8ToString03(): Unit \ Assert =
        assertEq(expected = "(true, 1, 97, 2, 9, false, Some(42), 1)", toString((true, 1, 97, 2, 9, false, Some(42), 1i64)))

    @Test
    def tuple8ToString04(): Unit \ Assert =
        assertEq(expected = "((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9, 1, 42))", toString(((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9, 1, 42))))

    /////////////////////////////////////////////////////////////////////////////
    // Tuple9                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple9ToString01(): Unit \ Assert =
        assertEq(expected = "(1, 1, 2, 3, 9, 42, 2, 0, 9)", toString((1, 1, 2, 3, 9, 42, 2, 0, 9)))

    @Test
    def tuple9ToString02(): Unit \ Assert =
        assertEq(expected = "(1, Some(2), true, false, true, 0, false, Some(true), 1)", toString((1, Some(2), true, false, true, 0, false, Some(true), 1)))

    @Test
    def tuple9ToString03(): Unit \ Assert =
        assertEq(expected = "(true, 1, 97, 2, 9, false, Some(42), 1, 432)", toString((true, 1, 97, 2, 9, false, Some(42), 1i64, 432i32)))

    @Test
    def tuple9ToString04(): Unit \ Assert =
        assertEq(expected = "((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9, 1, 42, false))", toString(((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9, 1, 42, false))))

    /////////////////////////////////////////////////////////////////////////////
    // Tuple10                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple10ToString01(): Unit \ Assert =
        assertEq(expected = "(1, 1, 2, 3, 9, 42, 2, 0, 9, 342)", toString((1, 1, 2, 3, 9, 42, 2, 0, 9, 342)))

    @Test
    def tuple10ToString02(): Unit \ Assert =
        assertEq(expected = "(1, Some(2), true, false, true, 0, false, Some(true), 1, 1337)", toString((1, Some(2), true, false, true, 0, false, Some(true), 1, 1337)))

    @Test
    def tuple10ToString03(): Unit \ Assert =
        assertEq(expected = "(true, 1, 97, 2, 9, false, Some(42), 1, 432, Some(false))", toString((true, 1, 97, 2, 9, false, Some(42), 1i64, 432i32, Some(false))))

    @Test
    def tuple10ToString04(): Unit \ Assert =
        assertEq(expected = "((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9, 1, 42, false, true))", toString(((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9, 1, 42, false, true))))

}
