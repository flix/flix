mod Test.Exp.Unary.Bitwise {

    @Test
    def testUnaryBitwiseNegate01(): Bool = Int32.bitwiseNot(0) == -1

    @Test
    def testUnaryBitwiseNegate02(): Bool = Int32.bitwiseNot(1) == -2

    @Test
    def testUnaryBitwiseNegate03(): Bool = Int32.bitwiseNot(-1) == 0

    @Test
    def testUnaryBitwiseNegate04(): Bool = Int32.bitwiseNot(36000) == -36001

    @Test
    def testUnaryBitwiseNegate05(): Bool = Int32.bitwiseNot(-36000) == 35999

    @Test
    def testUnaryBitwiseNegate06(): Bool = Int32.bitwiseNot(2147483647) == -2147483648

    @Test
    def testUnaryBitwiseNegate07(): Bool = Int32.bitwiseNot(-2147483648) == 2147483647

    @Test
    def testUnaryBitwiseNegate08(): Bool = Int8.bitwiseNot(0i8) == -1i8

    @Test
    def testUnaryBitwiseNegate09(): Bool = Int8.bitwiseNot(1i8) == -2i8

    @Test
    def testUnaryBitwiseNegate10(): Bool = Int8.bitwiseNot(-1i8) == 0i8

    @Test
    def testUnaryBitwiseNegate11(): Bool = Int8.bitwiseNot(42i8) == -43i8

    @Test
    def testUnaryBitwiseNegate12(): Bool = Int8.bitwiseNot(-42i8) == 41i8

    @Test
    def testUnaryBitwiseNegate13(): Bool = Int8.bitwiseNot(127i8) == -128i8

    @Test
    def testUnaryBitwiseNegate14(): Bool = Int8.bitwiseNot(-128i8) == 127i8

    @Test
    def testUnaryBitwiseNegate15(): Bool = Int16.bitwiseNot(0i16) == -1i16

    @Test
    def testUnaryBitwiseNegate16(): Bool = Int16.bitwiseNot(1i16) == -2i16

    @Test
    def testUnaryBitwiseNegate17(): Bool = Int16.bitwiseNot(-1i16) == 0i16

    @Test
    def testUnaryBitwiseNegate18(): Bool = Int16.bitwiseNot(420i16) == -421i16

    @Test
    def testUnaryBitwiseNegate19(): Bool = Int16.bitwiseNot(-420i16) == 419i16

    @Test
    def testUnaryBitwiseNegate20(): Bool = Int16.bitwiseNot(32767i16) == -32768i16

    @Test
    def testUnaryBitwiseNegate21(): Bool = Int16.bitwiseNot(-32768i16) == 32767i16

    @Test
    def testUnaryBitwiseNegate22(): Bool = Int32.bitwiseNot(0i32) == -1i32

    @Test
    def testUnaryBitwiseNegate23(): Bool = Int32.bitwiseNot(1i32) == -2i32

    @Test
    def testUnaryBitwiseNegate24(): Bool = Int32.bitwiseNot(-1i32) == 0i32

    @Test
    def testUnaryBitwiseNegate25(): Bool = Int32.bitwiseNot(36000i32) == -36001i32

    @Test
    def testUnaryBitwiseNegate26(): Bool = Int32.bitwiseNot(-36000i32) == 35999i32

    @Test
    def testUnaryBitwiseNegate27(): Bool = Int32.bitwiseNot(2147483647i32) == -2147483648i32

    @Test
    def testUnaryBitwiseNegate28(): Bool = Int32.bitwiseNot(-2147483648i32) == 2147483647i32

    @Test
    def testUnaryBitwiseNegate29(): Bool = Int64.bitwiseNot(0i64) == -1i64

    @Test
    def testUnaryBitwiseNegate30(): Bool = Int64.bitwiseNot(1i64) == -2i64

    @Test
    def testUnaryBitwiseNegate31(): Bool = Int64.bitwiseNot(-1i64) == 0i64

    @Test
    def testUnaryBitwiseNegate32(): Bool = Int64.bitwiseNot(10000000000i64) == -10000000001i64

    @Test
    def testUnaryBitwiseNegate33(): Bool = Int64.bitwiseNot(-10000000000i64) == 9999999999i64

    @Test
    def testUnaryBitwiseNegate34(): Bool = Int64.bitwiseNot(9223372036854775807i64) == -9223372036854775808i64

    @Test
    def testUnaryBitwiseNegate35(): Bool = Int64.bitwiseNot(-9223372036854775808i64) == 9223372036854775807i64

    @Test
    def testUnaryBitwiseNegate36(): Bool = BigInt.bitwiseNot(0ii) == -1ii

    @Test
    def testUnaryBitwiseNegate37(): Bool = BigInt.bitwiseNot(1ii) == -2ii

    @Test
    def testUnaryBitwiseNegate38(): Bool = BigInt.bitwiseNot(-1ii) == 0ii

    @Test
    def testUnaryBitwiseNegate39(): Bool = BigInt.bitwiseNot(9223372036854775808ii) == -9223372036854775809ii

    @Test
    def testUnaryBitwiseNegate40(): Bool = BigInt.bitwiseNot(-9223372036854775809ii) == 9223372036854775808ii

}
