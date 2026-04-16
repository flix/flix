mod TestDebug {
    use Debug.stringify;
    use Assert.{assertEq, assertTrue, fail};

    /////////////////////////////////////////////////////////////////////////////
    // stringify                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testBoolStringify01(): Unit \ Assert = assertEq(expected = "true", stringify(true))

    @Test
    def testBoolStringify02(): Unit \ Assert = assertEq(expected = "false", stringify(false))


    @Test
    def testCharStringify01(): Unit \ Assert = assertEq(expected = "\'a\'", stringify('a'))

    @Test
    def testCharStringify02(): Unit \ Assert = assertEq(expected = "\'\u2194\'", stringify('\u2194'))

    @Test
    def testCharStringify03(): Unit \ Assert = assertEq(expected = "\'\\t\'", stringify('\t'))


    @Test
    def testFloat32Stringify01(): Unit \ Assert = assertEq(expected = "21.42f32", stringify(21.42f32))


    @Test
    def testInt8Stringify01(): Unit \ Assert = assertEq(expected = "42i8", stringify(42i8))


    @Test
    def testInt16Stringify01(): Unit \ Assert = assertEq(expected = "42i16", stringify(42i16))


    @Test
    def testInt32Stringify01(): Unit \ Assert = assertEq(expected = "42", stringify(42i32))


    @Test
    def testInt64Stringify01(): Unit \ Assert = assertEq(expected = "42i64", stringify(42i64))


    @Test
    def testBigIntStringify01(): Unit \ Assert = assertEq(expected = "42ii", stringify(42ii))


    @Test
    def testStringStringify01(): Unit \ Assert = assertEq(expected = "\"Hello World!\"", stringify("Hello World!"))

    @Test
    def testStringStringify02(): Unit \ Assert = assertEq(expected = "\"one\\ttwo\"", stringify("one\ttwo"))

    @Test
    def testStringStringify03(): Unit \ Assert = assertEq(expected = "\"\u0000\"", stringify("\u0000"))

    @Test
    def testStringStringify05(): Unit \ Assert = assertEq(
        expected = "\"" + "\\n" + "\\r" + "\\\\" + "\\\"" + "\\\'" + "\\t" + "\${" + "\%{" + "\u1234" + "\u0000" + "\"",
        stringify("\n\r\\\"\'\t\${\%{\u1234\u0000")
    )

    @Test
    def testArrayOfBoolStringify01(): Unit \ {Assert, IO} = assertEq(expected = "[true, false]", stringify(Array#{true, false} @ Static))


    @Test
    def testArrayOfCharStringify01(): Unit \ {Assert, IO} = assertEq(expected = "[a, b, c]", stringify(Array#{'a', 'b', 'c'} @ Static))


    @Test
    def testArrayOfFloat32Stringify01(): Unit \ {Assert, IO} = assertEq(expected = "[21.42, 98.6]", stringify(Array#{21.42f32, 98.6f32} @ Static))


    @Test
    def testArrayOfInt8Stringify01(): Unit \ {Assert, IO} = assertEq(expected = "[42, 43]", stringify(Array#{42i8, 43i8} @ Static))


    @Test
    def testArrayOfInt16Stringify01(): Unit \ {Assert, IO} = assertEq(expected = "[42, 43]", stringify(Array#{42i16, 43i16} @ Static))


    @Test
    def testArrayOfInt32Stringify01(): Unit \ {Assert, IO} = assertEq(expected = "[42, 43]", stringify(Array#{42i32, 43i32} @ Static))


    @Test
    def testArrayOfInt64Stringify01(): Unit \ {Assert, IO} = assertEq(expected = "[42, 43]", stringify(Array#{42i64, 43i64} @ Static))


    @Test
    def testArrayOfBigIntStringify01(): Unit \ {Assert, IO} = assertEq(expected = "[42, 43]", stringify(Array#{42ii, 43ii} @ Static))


    @Test
    def testArrayOfStringStringify01(): Unit \ {Assert, IO} = assertEq(expected = "[Hello, World!]", stringify(Array#{"Hello", "World!"} @ Static))


    @Test
    def testArrayOfArrayOfIntStringify01(): Unit \ {Assert, IO} = assertEq(expected = "[[1, 2], [3, 4]]", stringify(Array#{Array#{1, 2} @ Static, Array#{3, 4} @ Static} @ Static))


    @Test
    def testArrayOfArrayOfArrayOfIntStringify01(): Unit \ {Assert, IO} = assertEq(expected = "[[[1, 2], [3, 4]], [[5, 6], [7, 8]]]", stringify(Array#{Array#{Array#{1, 2} @ Static, Array#{3, 4} @ Static} @ Static, Array#{Array#{5, 6} @ Static, Array#{7, 8} @ Static} @ Static} @ Static))



     @Test
     def testNullStringify01(): Unit \ Assert = {
         let s = stringify(null);
         assertEq(expected = "null", s)
     }

     @Test
     def testNullStringify02(): Unit \ Assert = {
         let s = stringify((checked_cast(null): String));
         assertEq(expected = "null", s)
     }

     @Test
     def testNullStringify03(): Unit \ Assert = {
         let s = stringify((checked_cast(null): BigInt));
         assertEq(expected = "null", s)
     }

    @Test
    def testNullStringify06(): Unit \ Assert = region rc {
        let s = stringify(Array#{(checked_cast(null): String), "42"} @ rc);
        assertEq(expected = "[null, 42]", s)
    }

}
