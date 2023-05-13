mod Test.Exp.Unary.Bitwise {

    @test
    def testUnaryBitwiseNegate01(): Bool = Int32.bitwiseNot(0) == -1

    @test
    def testUnaryBitwiseNegate02(): Bool = Int32.bitwiseNot(1) == -2

    @test
    def testUnaryBitwiseNegate03(): Bool = Int32.bitwiseNot(-1) == 0

    @test
    def testUnaryBitwiseNegate04(): Bool = Int32.bitwiseNot(36000) == -36001

    @test
    def testUnaryBitwiseNegate05(): Bool = Int32.bitwiseNot(-36000) == 35999

    @test
    def testUnaryBitwiseNegate06(): Bool = Int32.bitwiseNot(2147483647) == -2147483648

    @test
    def testUnaryBitwiseNegate07(): Bool = Int32.bitwiseNot(-2147483648) == 2147483647

    @test
    def testUnaryBitwiseNegate08(): Bool = Int8.bitwiseNot(0i8) == -1i8

    @test
    def testUnaryBitwiseNegate09(): Bool = Int8.bitwiseNot(1i8) == -2i8

    @test
    def testUnaryBitwiseNegate10(): Bool = Int8.bitwiseNot(-1i8) == 0i8

    @test
    def testUnaryBitwiseNegate11(): Bool = Int8.bitwiseNot(42i8) == -43i8

    @test
    def testUnaryBitwiseNegate12(): Bool = Int8.bitwiseNot(-42i8) == 41i8

    @test
    def testUnaryBitwiseNegate13(): Bool = Int8.bitwiseNot(127i8) == -128i8

    @test
    def testUnaryBitwiseNegate14(): Bool = Int8.bitwiseNot(-128i8) == 127i8

    @test
    def testUnaryBitwiseNegate15(): Bool = Int16.bitwiseNot(0i16) == -1i16

    @test
    def testUnaryBitwiseNegate16(): Bool = Int16.bitwiseNot(1i16) == -2i16

    @test
    def testUnaryBitwiseNegate17(): Bool = Int16.bitwiseNot(-1i16) == 0i16

    @test
    def testUnaryBitwiseNegate18(): Bool = Int16.bitwiseNot(420i16) == -421i16

    @test
    def testUnaryBitwiseNegate19(): Bool = Int16.bitwiseNot(-420i16) == 419i16

    @test
    def testUnaryBitwiseNegate20(): Bool = Int16.bitwiseNot(32767i16) == -32768i16

    @test
    def testUnaryBitwiseNegate21(): Bool = Int16.bitwiseNot(-32768i16) == 32767i16

    @test
    def testUnaryBitwiseNegate22(): Bool = Int32.bitwiseNot(0i32) == -1i32

    @test
    def testUnaryBitwiseNegate23(): Bool = Int32.bitwiseNot(1i32) == -2i32

    @test
    def testUnaryBitwiseNegate24(): Bool = Int32.bitwiseNot(-1i32) == 0i32

    @test
    def testUnaryBitwiseNegate25(): Bool = Int32.bitwiseNot(36000i32) == -36001i32

    @test
    def testUnaryBitwiseNegate26(): Bool = Int32.bitwiseNot(-36000i32) == 35999i32

    @test
    def testUnaryBitwiseNegate27(): Bool = Int32.bitwiseNot(2147483647i32) == -2147483648i32

    @test
    def testUnaryBitwiseNegate28(): Bool = Int32.bitwiseNot(-2147483648i32) == 2147483647i32

    @test
    def testUnaryBitwiseNegate29(): Bool = Int64.bitwiseNot(0i64) == -1i64

    @test
    def testUnaryBitwiseNegate30(): Bool = Int64.bitwiseNot(1i64) == -2i64

    @test
    def testUnaryBitwiseNegate31(): Bool = Int64.bitwiseNot(-1i64) == 0i64

    @test
    def testUnaryBitwiseNegate32(): Bool = Int64.bitwiseNot(10000000000i64) == -10000000001i64

    @test
    def testUnaryBitwiseNegate33(): Bool = Int64.bitwiseNot(-10000000000i64) == 9999999999i64

    @test
    def testUnaryBitwiseNegate34(): Bool = Int64.bitwiseNot(9223372036854775807i64) == -9223372036854775808i64

    @test
    def testUnaryBitwiseNegate35(): Bool = Int64.bitwiseNot(-9223372036854775808i64) == 9223372036854775807i64

    @test
    def testUnaryBitwiseNegate36(): Bool = BigInt.bitwiseNot(0ii) == -1ii

    @test
    def testUnaryBitwiseNegate37(): Bool = BigInt.bitwiseNot(1ii) == -2ii

    @test
    def testUnaryBitwiseNegate38(): Bool = BigInt.bitwiseNot(-1ii) == 0ii

    @test
    def testUnaryBitwiseNegate39(): Bool = BigInt.bitwiseNot(9223372036854775808ii) == -9223372036854775809ii

    @test
    def testUnaryBitwiseNegate40(): Bool = BigInt.bitwiseNot(-9223372036854775809ii) == 9223372036854775808ii

}
