mod Test.Exp.Jvm.InvokeMethod {

    use Assert.{assertEq, assertTrue}

    import java.lang.Integer
    import java.lang.Math

    // BigInteger Methods

    @Test
    def testBigIntegerAdd01(): Unit \ Assert =
        assertEq(expected = 468ii, (-32ii).add(500ii))

    @Test
    def testBigIntegerCompareTo01(): Unit \ Assert =
        let val = 500ii;
        assertEq(expected = 1, val.compareTo(-80ii))

    @Test
    def testBigIntegerAnd01(): Unit \ Assert =
        let val = 150ii;
        assertEq(expected = 148ii, val.$and(-100ii))

    @Test
    def testBigIntegerMod01(): Unit \ Assert + IO =
        let val = 123488ii;
        assertEq(expected = 1ii, val.$mod(23ii))

    @Test
    def testBigIntegerModPow01(): Unit \ Assert + IO =
        let val = 123488ii;
        assertEq(expected = 8247ii, val.modPow(10ii, 32023ii))

    @Test
    def testBigIntegerSubtract01(): Unit \ Assert =
        let val = 500ii;
        assertEq(expected = 200ii, val.subtract(300ii))

    @Test
    def testBigIntegerDivide01(): Unit \ Assert =
        let val = 100ii;
        assertEq(expected = 20ii, val.divide(5ii))

    @Test
    def testBigIntegerGcd01(): Unit \ Assert + IO =
        let val = 12ii;
        assertEq(expected = 4ii, val.gcd(8ii))

    @Test
    def testBigIntegerAbs01(): Unit \ Assert + IO =
        assertEq(expected = 50ii, (-50ii).abs())

    @Test
    def testBigIntegerSignum01(): Unit \ Assert + IO =
        assertEq(expected = -1, (-100ii).signum())

    // BigDecimal Methods

    @Test
    def testBigDecimalMax01(): Unit \ Assert + IO =
        let val = 9999.999999999ff;
        assertEq(expected = 9999.999999999ff, val.max(9999.99999999ff))

    @Test
    def testBigDecimalMultiply01(): Unit \ Assert =
        assertEq(expected = -4148918.4945ff, (-98432.23ff).multiply(42.15ff))

    @Test
    def testBigDecimalPow01(): Unit \ Assert =
        let val = 9834.003001ff;
        assertEq(expected = 9352362803567718.561955395843171390012001ff, val.pow(4))

    @Test
    def testBigDecimalRemainder01(): Unit \ Assert + IO =
        let val = 9012111.9834ff;
        assertEq(expected = 327.9234ff, val.remainder(429.01ff))

    @Test
    def testBigDecimalSubtract01(): Unit \ Assert =
        let val = 100.50ff;
        assertEq(expected = 50.25ff, val.subtract(50.25ff))

    @Test
    def testBigDecimalAbs01(): Unit \ Assert =
        assertEq(expected = 99.5ff, (-99.5ff).abs())

    @Test
    def testBigDecimalSignum01(): Unit \ Assert =
        assertEq(expected = -1, (-50.0ff).signum())

    @Test
    def testBigDecimalScale01(): Unit \ Assert =
        let val = 123.456ff;
        assertEq(expected = 3, val.scale())

    // Other Methods

    @Test
    def testMathMax01(): Unit \ Assert + IO =
        assertEq(expected = 10, Math.max(5, 10))

    @Test
    def testMathMin01(): Unit \ Assert + IO =
        assertEq(expected = 5, Math.min(5, 10))

    @Test
    def testMathAbs01(): Unit \ Assert =
        assertEq(expected = 42, Math.abs(-42))

    @Test
    def testIntegerParseInt01(): Unit \ Assert =
        assertEq(expected = 123, Integer.parseInt("123"))

    // String Methods

    @Test
    def testIndexOf01(): Unit \ Assert =
        assertEq(expected = 6, "hello world".indexOf("world"))

    @Test
    def testIndexOf02(): Unit \ Assert =
        assertEq(expected = 1, "HELLO WORLD".indexOf("E"))

    @Test
    def testReplaceAll01(): Unit \ Assert =
        assertEq(expected = "goodbye world", "hello world".replaceAll("hello", "goodbye"))

    @Test
    def testConcat01(): Unit \ Assert =
        assertEq(expected = "Hello world", "Hello".concat(" world"))

    @Test
    def testCompareTo01(): Unit \ Assert =
        assertEq(expected = 1, "Hello world".compareTo("Goodbye world"))

    @Test
    def testCharAt01(): Unit \ Assert =
        assertEq(expected = 'L', "HELLO WORLD".charAt(3))

    @Test
    def testStartsWith01(): Unit \ Assert =
        assertTrue("hello world".startsWith("hello"))

    @Test
    def testEndsWith01(): Unit \ Assert =
        assertTrue("hello world".endsWith("world"))

    @Test
    def testIsEmpty01(): Unit \ Assert =
        assertTrue("".isEmpty())

    @Test
    def testSubstring01(): Unit \ Assert =
        assertEq(expected = "llo", "hello".substring(2))

    @Test
    def testEqualsIgnoreCase01(): Unit \ Assert + IO =
        assertTrue("HELLO".equalsIgnoreCase("hello"))

    @Test
    def testMatches01(): Unit \ Assert =
        assertTrue("hello123".matches(".*\\d+"))

}
