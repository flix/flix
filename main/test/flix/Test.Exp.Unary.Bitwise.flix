mod Test.Exp.Unary.Bitwise {

    use Assert.assertEq

    @Test
    def testUnaryBitwiseNegate01(): Unit \ Assert =
        assertEq(expected = -1, Int32.bitwiseNot(0))

    @Test
    def testUnaryBitwiseNegate02(): Unit \ Assert =
        assertEq(expected = -2, Int32.bitwiseNot(1))

    @Test
    def testUnaryBitwiseNegate03(): Unit \ Assert =
        assertEq(expected = 0, Int32.bitwiseNot(-1))

    @Test
    def testUnaryBitwiseNegate04(): Unit \ Assert =
        assertEq(expected = -36001, Int32.bitwiseNot(36000))

    @Test
    def testUnaryBitwiseNegate05(): Unit \ Assert =
        assertEq(expected = 35999, Int32.bitwiseNot(-36000))

    @Test
    def testUnaryBitwiseNegate06(): Unit \ Assert =
        assertEq(expected = -2147483648, Int32.bitwiseNot(2147483647))

    @Test
    def testUnaryBitwiseNegate07(): Unit \ Assert =
        assertEq(expected = 2147483647, Int32.bitwiseNot(-2147483648))

    @Test
    def testUnaryBitwiseNegate08(): Unit \ Assert =
        assertEq(expected = -1i8, Int8.bitwiseNot(0i8))

    @Test
    def testUnaryBitwiseNegate09(): Unit \ Assert =
        assertEq(expected = -2i8, Int8.bitwiseNot(1i8))

    @Test
    def testUnaryBitwiseNegate10(): Unit \ Assert =
        assertEq(expected = 0i8, Int8.bitwiseNot(-1i8))

    @Test
    def testUnaryBitwiseNegate11(): Unit \ Assert =
        assertEq(expected = -43i8, Int8.bitwiseNot(42i8))

    @Test
    def testUnaryBitwiseNegate12(): Unit \ Assert =
        assertEq(expected = 41i8, Int8.bitwiseNot(-42i8))

    @Test
    def testUnaryBitwiseNegate13(): Unit \ Assert =
        assertEq(expected = -128i8, Int8.bitwiseNot(127i8))

    @Test
    def testUnaryBitwiseNegate14(): Unit \ Assert =
        assertEq(expected = 127i8, Int8.bitwiseNot(-128i8))

    @Test
    def testUnaryBitwiseNegate15(): Unit \ Assert =
        assertEq(expected = -1i16, Int16.bitwiseNot(0i16))

    @Test
    def testUnaryBitwiseNegate16(): Unit \ Assert =
        assertEq(expected = -2i16, Int16.bitwiseNot(1i16))

    @Test
    def testUnaryBitwiseNegate17(): Unit \ Assert =
        assertEq(expected = 0i16, Int16.bitwiseNot(-1i16))

    @Test
    def testUnaryBitwiseNegate18(): Unit \ Assert =
        assertEq(expected = -421i16, Int16.bitwiseNot(420i16))

    @Test
    def testUnaryBitwiseNegate19(): Unit \ Assert =
        assertEq(expected = 419i16, Int16.bitwiseNot(-420i16))

    @Test
    def testUnaryBitwiseNegate20(): Unit \ Assert =
        assertEq(expected = -32768i16, Int16.bitwiseNot(32767i16))

    @Test
    def testUnaryBitwiseNegate21(): Unit \ Assert =
        assertEq(expected = 32767i16, Int16.bitwiseNot(-32768i16))

    @Test
    def testUnaryBitwiseNegate22(): Unit \ Assert =
        assertEq(expected = -1i32, Int32.bitwiseNot(0i32))

    @Test
    def testUnaryBitwiseNegate23(): Unit \ Assert =
        assertEq(expected = -2i32, Int32.bitwiseNot(1i32))

    @Test
    def testUnaryBitwiseNegate24(): Unit \ Assert =
        assertEq(expected = 0i32, Int32.bitwiseNot(-1i32))

    @Test
    def testUnaryBitwiseNegate25(): Unit \ Assert =
        assertEq(expected = -36001i32, Int32.bitwiseNot(36000i32))

    @Test
    def testUnaryBitwiseNegate26(): Unit \ Assert =
        assertEq(expected = 35999i32, Int32.bitwiseNot(-36000i32))

    @Test
    def testUnaryBitwiseNegate27(): Unit \ Assert =
        assertEq(expected = -2147483648i32, Int32.bitwiseNot(2147483647i32))

    @Test
    def testUnaryBitwiseNegate28(): Unit \ Assert =
        assertEq(expected = 2147483647i32, Int32.bitwiseNot(-2147483648i32))

    @Test
    def testUnaryBitwiseNegate29(): Unit \ Assert =
        assertEq(expected = -1i64, Int64.bitwiseNot(0i64))

    @Test
    def testUnaryBitwiseNegate30(): Unit \ Assert =
        assertEq(expected = -2i64, Int64.bitwiseNot(1i64))

    @Test
    def testUnaryBitwiseNegate31(): Unit \ Assert =
        assertEq(expected = 0i64, Int64.bitwiseNot(-1i64))

    @Test
    def testUnaryBitwiseNegate32(): Unit \ Assert =
        assertEq(expected = -10000000001i64, Int64.bitwiseNot(10000000000i64))

    @Test
    def testUnaryBitwiseNegate33(): Unit \ Assert =
        assertEq(expected = 9999999999i64, Int64.bitwiseNot(-10000000000i64))

    @Test
    def testUnaryBitwiseNegate34(): Unit \ Assert =
        assertEq(expected = -9223372036854775808i64, Int64.bitwiseNot(9223372036854775807i64))

    @Test
    def testUnaryBitwiseNegate35(): Unit \ Assert =
        assertEq(expected = 9223372036854775807i64, Int64.bitwiseNot(-9223372036854775808i64))

    @Test
    def testUnaryBitwiseNegate36(): Unit \ Assert =
        assertEq(expected = -1ii, BigInt.bitwiseNot(0ii))

    @Test
    def testUnaryBitwiseNegate37(): Unit \ Assert =
        assertEq(expected = -2ii, BigInt.bitwiseNot(1ii))

    @Test
    def testUnaryBitwiseNegate38(): Unit \ Assert =
        assertEq(expected = 0ii, BigInt.bitwiseNot(-1ii))

    @Test
    def testUnaryBitwiseNegate39(): Unit \ Assert =
        assertEq(expected = -9223372036854775809ii, BigInt.bitwiseNot(9223372036854775808ii))

    @Test
    def testUnaryBitwiseNegate40(): Unit \ Assert =
        assertEq(expected = 9223372036854775808ii, BigInt.bitwiseNot(-9223372036854775809ii))

}
