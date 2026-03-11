mod Test.Exp.Jvm.InvokeStaticMethod {

    use Assert.assertEq

    import java.lang.{Boolean => JBoolean}
    import java.lang.{Byte => JByte}
    import java.lang.{Double => JDouble}
    import java.lang.{Float => JFloat}
    import java.lang.{Integer => JInteger}
    import java.lang.{Long => JLong}
    import java.lang.{Math => JMath}
    import java.lang.{Short => JShort}
    import java.lang.{String => JString}

    @Test
    def invokeStringValueOfBoolean01(): Unit \ {Assert, IO} =
        assertEq(expected = "true", JString.valueOf(true))

    @Test
    def invokeStringValueOfBoolean02(): Unit \ {Assert, IO} =
        assertEq(expected = "false", JString.valueOf(false))

    @Test
    def invokeStringValueOfChar01(): Unit \ {Assert, IO} =
        assertEq(expected = "A", JString.valueOf('A'))

    @Test
    def invokeStringValueOfChar02(): Unit \ {Assert, IO} =
        assertEq(expected = "z", JString.valueOf('z'))

    @Test
    def invokeStringValueOfChar03(): Unit \ {Assert, IO} =
        assertEq(expected = "5", JString.valueOf('5'))

    @Test
    def invokeStringValueOfInt01(): Unit \ {Assert, IO} =
        assertEq(expected = "42", JString.valueOf(42i32))

    @Test
    def invokeStringValueOfInt02(): Unit \ {Assert, IO} =
        assertEq(expected = "-123", JString.valueOf(-123i32))

    @Test
    def invokeStringValueOfInt03(): Unit \ {Assert, IO} =
        assertEq(expected = "0", JString.valueOf(0i32))

    @Test
    def invokeStringValueOfLong01(): Unit \ {Assert, IO} =
        assertEq(expected = "42", JString.valueOf(42i64))

    @Test
    def invokeStringValueOfLong02(): Unit \ {Assert, IO} =
        assertEq(expected = "-9876543210", JString.valueOf(-9876543210i64))

    @Test
    def invokeStringValueOfLong03(): Unit \ {Assert, IO} =
        assertEq(expected = "0", JString.valueOf(0i64))

    @Test
    def invokeStringValueOfFloat01(): Unit \ {Assert, IO} =
        assertEq(expected = "21.42", JString.valueOf(21.42f32))

    @Test
    def invokeStringValueOfFloat02(): Unit \ {Assert, IO} =
        assertEq(expected = "-3.14", JString.valueOf(-3.14f32))

    @Test
    def invokeStringValueOfFloat03(): Unit \ {Assert, IO} =
        assertEq(expected = "0.0", JString.valueOf(0.0f32))

    @Test
    def invokeStringValueOfDouble01(): Unit \ {Assert, IO} =
        assertEq(expected = "21.42", JString.valueOf(21.42f64))

    @Test
    def invokeStringValueOfDouble02(): Unit \ {Assert, IO} =
        assertEq(expected = "-3.141592653589793", JString.valueOf(-3.141592653589793f64))

    @Test
    def invokeStringValueOfDouble03(): Unit \ {Assert, IO} =
        assertEq(expected = "0.0", JString.valueOf(0.0f64))

    @Test
    def invokeMathAbs01(): Unit \ {Assert} =
        assertEq(expected = 42, JMath.abs(-42))

    @Test
    def invokeMathAbs02(): Unit \ {Assert} =
        assertEq(expected = 3.14f64, JMath.abs(-3.14f64))

    @Test
    def invokeMathMax01(): Unit \ {Assert, IO} =
        assertEq(expected = 10, JMath.max(5, 10))

    @Test
    def invokeMathMin01(): Unit \ {Assert, IO} =
        assertEq(expected = 5, JMath.min(5, 10))

    @Test
    def invokeMathSqrt01(): Unit \ {Assert, IO} =
        assertEq(expected = 4.0f64, JMath.sqrt(16.0f64))

    @Test
    def invokeBooleanParseBoolean01(): Unit \ {Assert} =
        assertEq(expected = true, JBoolean.parseBoolean("true"))

    @Test
    def invokeBooleanParseBoolean02(): Unit \ {Assert} =
        assertEq(expected = false, JBoolean.parseBoolean("false"))

    @Test
    def invokeBooleanParseBoolean03(): Unit \ {Assert} =
        assertEq(expected = false, JBoolean.parseBoolean("invalid"))

    @Test
    def invokeByteParseB01(): Unit \ {Assert} =
        assertEq(expected = 123i8, JByte.parseByte("123"))

    @Test
    def invokeByteParseB02(): Unit \ {Assert} =
        assertEq(expected = -45i8, JByte.parseByte("-45"))

    @Test
    def invokeByteParseB03(): Unit \ {Assert} =
        assertEq(expected = 0i8, JByte.parseByte("0"))

    @Test
    def invokeShortParseShort01(): Unit \ {Assert} =
        assertEq(expected = 12345i16, JShort.parseShort("12345"))

    @Test
    def invokeShortParseShort02(): Unit \ {Assert} =
        assertEq(expected = -6789i16, JShort.parseShort("-6789"))

    @Test
    def invokeShortParseShort03(): Unit \ {Assert} =
        assertEq(expected = 0i16, JShort.parseShort("0"))

    @Test
    def invokeIntegerParseInt01(): Unit \ {Assert} =
        assertEq(expected = 123, JInteger.parseInt("123"))

    @Test
    def invokeIntegerParseInt02(): Unit \ {Assert} =
        assertEq(expected = -456, JInteger.parseInt("-456"))

    @Test
    def invokeIntegerParseInt03(): Unit \ {Assert} =
        assertEq(expected = 0, JInteger.parseInt("0"))

    @Test
    def invokeLongParseLong01(): Unit \ {Assert} =
        assertEq(expected = 9876543210i64, JLong.parseLong("9876543210"))

    @Test
    def invokeLongParseLong02(): Unit \ {Assert} =
        assertEq(expected = -123456789i64, JLong.parseLong("-123456789"))

    @Test
    def invokeLongParseLong03(): Unit \ {Assert} =
        assertEq(expected = 0i64, JLong.parseLong("0"))

    @Test
    def invokeFloatParseFloat01(): Unit \ {Assert} =
        assertEq(expected = 3.14f32, JFloat.parseFloat("3.14"))

    @Test
    def invokeFloatParseFloat02(): Unit \ {Assert} =
        assertEq(expected = -2.718f32, JFloat.parseFloat("-2.718"))

    @Test
    def invokeFloatParseFloat03(): Unit \ {Assert} =
        assertEq(expected = 0.0f32, JFloat.parseFloat("0.0"))

    @Test
    def invokeDoubleParseDouble01(): Unit \ {Assert} =
        assertEq(expected = 3.141592653589793f64, JDouble.parseDouble("3.141592653589793"))

    @Test
    def invokeDoubleParseDouble02(): Unit \ {Assert} =
        assertEq(expected = -2.718281828459045f64, JDouble.parseDouble("-2.718281828459045"))

    @Test
    def invokeDoubleParseDouble03(): Unit \ {Assert} =
        assertEq(expected = 0.0f64, JDouble.parseDouble("0.0"))

}
