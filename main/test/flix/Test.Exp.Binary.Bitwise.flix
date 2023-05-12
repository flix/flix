mod Test.Exp.Binary.Bitwise {

    @test
    def testBinaryBitwiseAnd01(): Bool = Int32.bitwiseAnd(40000, -1) == 40000

    @test
    def testBinaryBitwiseAnd02(): Bool = Int32.bitwiseAnd(40000, 40000) == 40000

    @test
    def testBinaryBitwiseAnd03(): Bool = Int32.bitwiseAnd(40000, 0) == 0

    @test
    def testBinaryBitwiseAnd04(): Bool = Int32.bitwiseAnd(-1, -1) == -1

    @test
    def testBinaryBitwiseAnd05(): Bool = Int8.bitwiseAnd(40i8, -1i8) == 40i8

    @test
    def testBinaryBitwiseAnd06(): Bool = Int8.bitwiseAnd(40i8, 40i8) == 40i8

    @test
    def testBinaryBitwiseAnd07(): Bool = Int8.bitwiseAnd(40i8, 0i8) == 0i8

    @test
    def testBinaryBitwiseAnd08(): Bool = Int8.bitwiseAnd(-1i8, -1i8) == -1i8

    @test
    def testBinaryBitwiseAnd09(): Bool = Int16.bitwiseAnd(400i16, -1i16) == 400i16

    @test
    def testBinaryBitwiseAnd10(): Bool = Int16.bitwiseAnd(400i16, 400i16) == 400i16

    @test
    def testBinaryBitwiseAnd11(): Bool = Int16.bitwiseAnd(400i16, 0i16) == 0i16

    @test
    def testBinaryBitwiseAnd12(): Bool = Int16.bitwiseAnd(-1i16, -1i16) == -1i16

    @test
    def testBinaryBitwiseAnd13(): Bool = Int32.bitwiseAnd(40000i32, -1i32) == 40000i32

    @test
    def testBinaryBitwiseAnd14(): Bool = Int32.bitwiseAnd(40000i32, 40000i32) == 40000i32

    @test
    def testBinaryBitwiseAnd15(): Bool = Int32.bitwiseAnd(40000i32, 0i32) == 0i32

    @test
    def testBinaryBitwiseAnd16(): Bool = Int32.bitwiseAnd(-1i32, -1i32) == -1i32

    @test
    def testBinaryBitwiseAnd17(): Bool = Int64.bitwiseAnd(40000000000i64, -1i64) == 40000000000i64

    @test
    def testBinaryBitwiseAnd18(): Bool = Int64.bitwiseAnd(40000000000i64, 40000000000i64) == 40000000000i64

    @test
    def testBinaryBitwiseAnd19(): Bool = Int64.bitwiseAnd(40000000000i64, 0i64) == 0i64

    @test
    def testBinaryBitwiseAnd20(): Bool = Int64.bitwiseAnd(-1i64, -1i64) == -1i64

    @test
    def testBinaryBitwiseAnd21(): Bool = BigInt.bitwiseAnd(40000000000000000000ii, -1ii) == 40000000000000000000ii

    @test
    def testBinaryBitwiseAnd22(): Bool = BigInt.bitwiseAnd(40000000000000000000ii, 40000000000000000000ii) == 40000000000000000000ii

    @test
    def testBinaryBitwiseAnd23(): Bool = BigInt.bitwiseAnd(40000000000000000000ii, 0ii) == 0ii

    @test
    def testBinaryBitwiseAnd24(): Bool = BigInt.bitwiseAnd(-1ii, -1ii) == -1ii

    @test
    def testBinaryBitwiseOr01(): Bool = Int32.bitwiseOr(40000, -1) == -1

    @test
    def testBinaryBitwiseOr02(): Bool = Int32.bitwiseOr(40000, 40000) == 40000

    @test
    def testBinaryBitwiseOr03(): Bool = Int32.bitwiseOr(40000, 0) == 40000

    @test
    def testBinaryBitwiseOr04(): Bool = Int32.bitwiseOr(-1, -1) == -1

    @test
    def testBinaryBitwiseOr05(): Bool = Int8.bitwiseOr(40i8, -1i8) == -1i8

    @test
    def testBinaryBitwiseOr06(): Bool = Int8.bitwiseOr(40i8, 40i8) == 40i8

    @test
    def testBinaryBitwiseOr07(): Bool = Int8.bitwiseOr(40i8, 0i8) == 40i8

    @test
    def testBinaryBitwiseOr08(): Bool = Int8.bitwiseOr(-1i8, -1i8) == -1i8

    @test
    def testBinaryBitwiseOr09(): Bool = Int16.bitwiseOr(400i16, -1i16) == -1i16

    @test
    def testBinaryBitwiseOr10(): Bool = Int16.bitwiseOr(400i16, 400i16) == 400i16

    @test
    def testBinaryBitwiseOr11(): Bool = Int16.bitwiseOr(400i16, 0i16) == 400i16

    @test
    def testBinaryBitwiseOr12(): Bool = Int16.bitwiseOr(-1i16, -1i16) == -1i16

    @test
    def testBinaryBitwiseOr13(): Bool = Int32.bitwiseOr(40000i32, -1i32) == -1i32

    @test
    def testBinaryBitwiseOr14(): Bool = Int32.bitwiseOr(40000i32, 40000i32) == 40000i32

    @test
    def testBinaryBitwiseOr15(): Bool = Int32.bitwiseOr(40000i32, 0i32) == 40000i32

    @test
    def testBinaryBitwiseOr16(): Bool = Int32.bitwiseOr(-1i32, -1i32) == -1i32

    @test
    def testBinaryBitwiseOr17(): Bool = Int64.bitwiseOr(40000000000i64, -1i64) == -1i64

    @test
    def testBinaryBitwiseOr18(): Bool = Int64.bitwiseOr(40000000000i64, 40000000000i64) == 40000000000i64

    @test
    def testBinaryBitwiseOr19(): Bool = Int64.bitwiseOr(40000000000i64, 0i64) == 40000000000i64

    @test
    def testBinaryBitwiseOr20(): Bool = Int64.bitwiseOr(-1i64, -1i64) == -1i64

    @test
    def testBinaryBitwiseOr21(): Bool = BigInt.bitwiseOr(40000000000000000000ii, -1ii) == -1ii

    @test
    def testBinaryBitwiseOr22(): Bool = BigInt.bitwiseOr(40000000000000000000ii, 40000000000000000000ii) == 40000000000000000000ii

    @test
    def testBinaryBitwiseOr23(): Bool = BigInt.bitwiseOr(40000000000000000000ii, 0ii) == 40000000000000000000ii

    @test
    def testBinaryBitwiseOr24(): Bool = BigInt.bitwiseOr(-1ii, -1ii) == -1ii

    @test
    def testBinaryBitwiseXor01(): Bool = Int32.bitwiseXor(40000, -1) == -40001

    @test
    def testBinaryBitwiseXor02(): Bool = Int32.bitwiseXor(40000, 40000) == 0

    @test
    def testBinaryBitwiseXor03(): Bool = Int32.bitwiseXor(40000, 0) == 40000

    @test
    def testBinaryBitwiseXor04(): Bool = Int32.bitwiseXor(-1, -1) == 0

    @test
    def testBinaryBitwiseXor05(): Bool = Int8.bitwiseXor(40i8, -1i8) == -41i8

    @test
    def testBinaryBitwiseXor06(): Bool = Int8.bitwiseXor(40i8, 40i8) == 0i8

    @test
    def testBinaryBitwiseXor07(): Bool = Int8.bitwiseXor(40i8, 0i8) == 40i8

    @test
    def testBinaryBitwiseXor08(): Bool = Int8.bitwiseXor(-1i8, -1i8) == 0i8

    @test
    def testBinaryBitwiseXor09(): Bool = Int16.bitwiseXor(400i16, -1i16) == -401i16

    @test
    def testBinaryBitwiseXor10(): Bool = Int16.bitwiseXor(400i16, 400i16) == 0i16

    @test
    def testBinaryBitwiseXor11(): Bool = Int16.bitwiseXor(400i16, 0i16) == 400i16

    @test
    def testBinaryBitwiseXor12(): Bool = Int16.bitwiseXor(-1i16, -1i16) == 0i16

    @test
    def testBinaryBitwiseXor13(): Bool = Int32.bitwiseXor(40000i32, -1i32) == -40001i32

    @test
    def testBinaryBitwiseXor14(): Bool = Int32.bitwiseXor(40000i32, 40000i32) == 0i32

    @test
    def testBinaryBitwiseXor15(): Bool = Int32.bitwiseXor(40000i32, 0i32) == 40000i32

    @test
    def testBinaryBitwiseXor16(): Bool = Int32.bitwiseXor(-1i32, -1i32) == 0i32

    @test
    def testBinaryBitwiseXor17(): Bool = Int64.bitwiseXor(40000000000i64, -1i64) == -40000000001i64

    @test
    def testBinaryBitwiseXor18(): Bool = Int64.bitwiseXor(40000000000i64, 40000000000i64) == 0i64

    @test
    def testBinaryBitwiseXor19(): Bool = Int64.bitwiseXor(40000000000i64, 0i64) == 40000000000i64

    @test
    def testBinaryBitwiseXor20(): Bool = Int64.bitwiseXor(-1i64, -1i64) == 0i64

    @test
    def testBinaryBitwiseXor21(): Bool = BigInt.bitwiseXor(40000000000000000000ii, -1ii) == -40000000000000000001ii

    @test
    def testBinaryBitwiseXor22(): Bool = BigInt.bitwiseXor(40000000000000000000ii, 40000000000000000000ii) == 0ii

    @test
    def testBinaryBitwiseXor23(): Bool = BigInt.bitwiseXor(40000000000000000000ii, 0ii) == 40000000000000000000ii

    @test
    def testBinaryBitwiseXor24(): Bool = BigInt.bitwiseXor(-1ii, -1ii) == 0ii

}
