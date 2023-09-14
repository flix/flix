mod Test.Exp.Binary.Bitwise {

    @Test
    def testBinaryBitwiseAnd01(): Bool = Int32.bitwiseAnd(40000, -1) == 40000

    @Test
    def testBinaryBitwiseAnd02(): Bool = Int32.bitwiseAnd(40000, 40000) == 40000

    @Test
    def testBinaryBitwiseAnd03(): Bool = Int32.bitwiseAnd(40000, 0) == 0

    @Test
    def testBinaryBitwiseAnd04(): Bool = Int32.bitwiseAnd(-1, -1) == -1

    @Test
    def testBinaryBitwiseAnd05(): Bool = Int8.bitwiseAnd(40i8, -1i8) == 40i8

    @Test
    def testBinaryBitwiseAnd06(): Bool = Int8.bitwiseAnd(40i8, 40i8) == 40i8

    @Test
    def testBinaryBitwiseAnd07(): Bool = Int8.bitwiseAnd(40i8, 0i8) == 0i8

    @Test
    def testBinaryBitwiseAnd08(): Bool = Int8.bitwiseAnd(-1i8, -1i8) == -1i8

    @Test
    def testBinaryBitwiseAnd09(): Bool = Int16.bitwiseAnd(400i16, -1i16) == 400i16

    @Test
    def testBinaryBitwiseAnd10(): Bool = Int16.bitwiseAnd(400i16, 400i16) == 400i16

    @Test
    def testBinaryBitwiseAnd11(): Bool = Int16.bitwiseAnd(400i16, 0i16) == 0i16

    @Test
    def testBinaryBitwiseAnd12(): Bool = Int16.bitwiseAnd(-1i16, -1i16) == -1i16

    @Test
    def testBinaryBitwiseAnd13(): Bool = Int32.bitwiseAnd(40000i32, -1i32) == 40000i32

    @Test
    def testBinaryBitwiseAnd14(): Bool = Int32.bitwiseAnd(40000i32, 40000i32) == 40000i32

    @Test
    def testBinaryBitwiseAnd15(): Bool = Int32.bitwiseAnd(40000i32, 0i32) == 0i32

    @Test
    def testBinaryBitwiseAnd16(): Bool = Int32.bitwiseAnd(-1i32, -1i32) == -1i32

    @Test
    def testBinaryBitwiseAnd17(): Bool = Int64.bitwiseAnd(40000000000i64, -1i64) == 40000000000i64

    @Test
    def testBinaryBitwiseAnd18(): Bool = Int64.bitwiseAnd(40000000000i64, 40000000000i64) == 40000000000i64

    @Test
    def testBinaryBitwiseAnd19(): Bool = Int64.bitwiseAnd(40000000000i64, 0i64) == 0i64

    @Test
    def testBinaryBitwiseAnd20(): Bool = Int64.bitwiseAnd(-1i64, -1i64) == -1i64

    @Test
    def testBinaryBitwiseAnd21(): Bool = BigInt.bitwiseAnd(40000000000000000000ii, -1ii) == 40000000000000000000ii

    @Test
    def testBinaryBitwiseAnd22(): Bool = BigInt.bitwiseAnd(40000000000000000000ii, 40000000000000000000ii) == 40000000000000000000ii

    @Test
    def testBinaryBitwiseAnd23(): Bool = BigInt.bitwiseAnd(40000000000000000000ii, 0ii) == 0ii

    @Test
    def testBinaryBitwiseAnd24(): Bool = BigInt.bitwiseAnd(-1ii, -1ii) == -1ii

    @Test
    def testBinaryBitwiseOr01(): Bool = Int32.bitwiseOr(40000, -1) == -1

    @Test
    def testBinaryBitwiseOr02(): Bool = Int32.bitwiseOr(40000, 40000) == 40000

    @Test
    def testBinaryBitwiseOr03(): Bool = Int32.bitwiseOr(40000, 0) == 40000

    @Test
    def testBinaryBitwiseOr04(): Bool = Int32.bitwiseOr(-1, -1) == -1

    @Test
    def testBinaryBitwiseOr05(): Bool = Int8.bitwiseOr(40i8, -1i8) == -1i8

    @Test
    def testBinaryBitwiseOr06(): Bool = Int8.bitwiseOr(40i8, 40i8) == 40i8

    @Test
    def testBinaryBitwiseOr07(): Bool = Int8.bitwiseOr(40i8, 0i8) == 40i8

    @Test
    def testBinaryBitwiseOr08(): Bool = Int8.bitwiseOr(-1i8, -1i8) == -1i8

    @Test
    def testBinaryBitwiseOr09(): Bool = Int16.bitwiseOr(400i16, -1i16) == -1i16

    @Test
    def testBinaryBitwiseOr10(): Bool = Int16.bitwiseOr(400i16, 400i16) == 400i16

    @Test
    def testBinaryBitwiseOr11(): Bool = Int16.bitwiseOr(400i16, 0i16) == 400i16

    @Test
    def testBinaryBitwiseOr12(): Bool = Int16.bitwiseOr(-1i16, -1i16) == -1i16

    @Test
    def testBinaryBitwiseOr13(): Bool = Int32.bitwiseOr(40000i32, -1i32) == -1i32

    @Test
    def testBinaryBitwiseOr14(): Bool = Int32.bitwiseOr(40000i32, 40000i32) == 40000i32

    @Test
    def testBinaryBitwiseOr15(): Bool = Int32.bitwiseOr(40000i32, 0i32) == 40000i32

    @Test
    def testBinaryBitwiseOr16(): Bool = Int32.bitwiseOr(-1i32, -1i32) == -1i32

    @Test
    def testBinaryBitwiseOr17(): Bool = Int64.bitwiseOr(40000000000i64, -1i64) == -1i64

    @Test
    def testBinaryBitwiseOr18(): Bool = Int64.bitwiseOr(40000000000i64, 40000000000i64) == 40000000000i64

    @Test
    def testBinaryBitwiseOr19(): Bool = Int64.bitwiseOr(40000000000i64, 0i64) == 40000000000i64

    @Test
    def testBinaryBitwiseOr20(): Bool = Int64.bitwiseOr(-1i64, -1i64) == -1i64

    @Test
    def testBinaryBitwiseOr21(): Bool = BigInt.bitwiseOr(40000000000000000000ii, -1ii) == -1ii

    @Test
    def testBinaryBitwiseOr22(): Bool = BigInt.bitwiseOr(40000000000000000000ii, 40000000000000000000ii) == 40000000000000000000ii

    @Test
    def testBinaryBitwiseOr23(): Bool = BigInt.bitwiseOr(40000000000000000000ii, 0ii) == 40000000000000000000ii

    @Test
    def testBinaryBitwiseOr24(): Bool = BigInt.bitwiseOr(-1ii, -1ii) == -1ii

    @Test
    def testBinaryBitwiseXor01(): Bool = Int32.bitwiseXor(40000, -1) == -40001

    @Test
    def testBinaryBitwiseXor02(): Bool = Int32.bitwiseXor(40000, 40000) == 0

    @Test
    def testBinaryBitwiseXor03(): Bool = Int32.bitwiseXor(40000, 0) == 40000

    @Test
    def testBinaryBitwiseXor04(): Bool = Int32.bitwiseXor(-1, -1) == 0

    @Test
    def testBinaryBitwiseXor05(): Bool = Int8.bitwiseXor(40i8, -1i8) == -41i8

    @Test
    def testBinaryBitwiseXor06(): Bool = Int8.bitwiseXor(40i8, 40i8) == 0i8

    @Test
    def testBinaryBitwiseXor07(): Bool = Int8.bitwiseXor(40i8, 0i8) == 40i8

    @Test
    def testBinaryBitwiseXor08(): Bool = Int8.bitwiseXor(-1i8, -1i8) == 0i8

    @Test
    def testBinaryBitwiseXor09(): Bool = Int16.bitwiseXor(400i16, -1i16) == -401i16

    @Test
    def testBinaryBitwiseXor10(): Bool = Int16.bitwiseXor(400i16, 400i16) == 0i16

    @Test
    def testBinaryBitwiseXor11(): Bool = Int16.bitwiseXor(400i16, 0i16) == 400i16

    @Test
    def testBinaryBitwiseXor12(): Bool = Int16.bitwiseXor(-1i16, -1i16) == 0i16

    @Test
    def testBinaryBitwiseXor13(): Bool = Int32.bitwiseXor(40000i32, -1i32) == -40001i32

    @Test
    def testBinaryBitwiseXor14(): Bool = Int32.bitwiseXor(40000i32, 40000i32) == 0i32

    @Test
    def testBinaryBitwiseXor15(): Bool = Int32.bitwiseXor(40000i32, 0i32) == 40000i32

    @Test
    def testBinaryBitwiseXor16(): Bool = Int32.bitwiseXor(-1i32, -1i32) == 0i32

    @Test
    def testBinaryBitwiseXor17(): Bool = Int64.bitwiseXor(40000000000i64, -1i64) == -40000000001i64

    @Test
    def testBinaryBitwiseXor18(): Bool = Int64.bitwiseXor(40000000000i64, 40000000000i64) == 0i64

    @Test
    def testBinaryBitwiseXor19(): Bool = Int64.bitwiseXor(40000000000i64, 0i64) == 40000000000i64

    @Test
    def testBinaryBitwiseXor20(): Bool = Int64.bitwiseXor(-1i64, -1i64) == 0i64

    @Test
    def testBinaryBitwiseXor21(): Bool = BigInt.bitwiseXor(40000000000000000000ii, -1ii) == -40000000000000000001ii

    @Test
    def testBinaryBitwiseXor22(): Bool = BigInt.bitwiseXor(40000000000000000000ii, 40000000000000000000ii) == 0ii

    @Test
    def testBinaryBitwiseXor23(): Bool = BigInt.bitwiseXor(40000000000000000000ii, 0ii) == 40000000000000000000ii

    @Test
    def testBinaryBitwiseXor24(): Bool = BigInt.bitwiseXor(-1ii, -1ii) == 0ii

}
