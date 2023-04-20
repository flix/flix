mod Test.Exp.Unary.Bitwise {

    @test
    def testUnaryBitwiseNegate01(): Bool = ~~~0 == -1

    @test
    def testUnaryBitwiseNegate02(): Bool = ~~~1 == -2

    @test
    def testUnaryBitwiseNegate03(): Bool = ~~~(-1) == 0

    @test
    def testUnaryBitwiseNegate04(): Bool = ~~~36000 == -36001

    @test
    def testUnaryBitwiseNegate05(): Bool = ~~~(-36000) == 35999

    @test
    def testUnaryBitwiseNegate06(): Bool = ~~~2147483647 == -2147483648

    @test
    def testUnaryBitwiseNegate07(): Bool = ~~~-2147483648 == 2147483647

    @test
    def testUnaryBitwiseNegate08(): Bool = ~~~0i8 == -1i8

    @test
    def testUnaryBitwiseNegate09(): Bool = ~~~1i8 == -2i8

    @test
    def testUnaryBitwiseNegate10(): Bool = ~~~(-1i8) == 0i8

    @test
    def testUnaryBitwiseNegate11(): Bool = ~~~42i8 == -43i8

    @test
    def testUnaryBitwiseNegate12(): Bool = ~~~(-42i8) == 41i8

    @test
    def testUnaryBitwiseNegate13(): Bool = ~~~127i8 == -128i8

    @test
    def testUnaryBitwiseNegate14(): Bool = ~~~-128i8 == 127i8

    @test
    def testUnaryBitwiseNegate15(): Bool = ~~~0i16 == -1i16

    @test
    def testUnaryBitwiseNegate16(): Bool = ~~~1i16 == -2i16

    @test
    def testUnaryBitwiseNegate17(): Bool = ~~~(-1i16) == 0i16

    @test
    def testUnaryBitwiseNegate18(): Bool = ~~~420i16 == -421i16

    @test
    def testUnaryBitwiseNegate19(): Bool = ~~~(-420i16) == 419i16

    @test
    def testUnaryBitwiseNegate20(): Bool = ~~~32767i16 == -32768i16

    @test
    def testUnaryBitwiseNegate21(): Bool = ~~~-32768i16 == 32767i16

    @test
    def testUnaryBitwiseNegate22(): Bool = ~~~0i32 == -1i32

    @test
    def testUnaryBitwiseNegate23(): Bool = ~~~1i32 == -2i32

    @test
    def testUnaryBitwiseNegate24(): Bool = ~~~(-1i32) == 0i32

    @test
    def testUnaryBitwiseNegate25(): Bool = ~~~36000i32 == -36001i32

    @test
    def testUnaryBitwiseNegate26(): Bool = ~~~(-36000i32) == 35999i32

    @test
    def testUnaryBitwiseNegate27(): Bool = ~~~2147483647i32 == -2147483648i32

    @test
    def testUnaryBitwiseNegate28(): Bool = ~~~-2147483648i32 == 2147483647i32

    @test
    def testUnaryBitwiseNegate29(): Bool = ~~~0i64 == -1i64

    @test
    def testUnaryBitwiseNegate30(): Bool = ~~~1i64 == -2i64

    @test
    def testUnaryBitwiseNegate31(): Bool = ~~~(-1i64) == 0i64

    @test
    def testUnaryBitwiseNegate32(): Bool = ~~~10000000000i64 == -10000000001i64

    @test
    def testUnaryBitwiseNegate33(): Bool = ~~~(-10000000000i64) == 9999999999i64

    @test
    def testUnaryBitwiseNegate34(): Bool = ~~~9223372036854775807i64 == -9223372036854775808i64

    @test
    def testUnaryBitwiseNegate35(): Bool = ~~~-9223372036854775808i64 == 9223372036854775807i64

    @test
    def testUnaryBitwiseNegate36(): Bool = ~~~0ii == -1ii

    @test
    def testUnaryBitwiseNegate37(): Bool = ~~~1ii == -2ii

    @test
    def testUnaryBitwiseNegate38(): Bool = ~~~(-1ii) == 0ii

    @test
    def testUnaryBitwiseNegate39(): Bool = ~~~9223372036854775808ii == -9223372036854775809ii

    @test
    def testUnaryBitwiseNegate40(): Bool = ~~~(-9223372036854775809ii) == 9223372036854775808ii

}
