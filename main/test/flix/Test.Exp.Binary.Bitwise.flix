mod Test.Exp.Binary.Bitwise {

    use Assert.assertEq

    @Test
    def testBinaryBitwiseAnd01(): Unit \ Assert =
        assertEq(expected = 40000, Int32.bitwiseAnd(40000, -1))

    @Test
    def testBinaryBitwiseAnd02(): Unit \ Assert =
        assertEq(expected = 40000, Int32.bitwiseAnd(40000, 40000))

    @Test
    def testBinaryBitwiseAnd03(): Unit \ Assert =
        assertEq(expected = 0, Int32.bitwiseAnd(40000, 0))

    @Test
    def testBinaryBitwiseAnd04(): Unit \ Assert =
        assertEq(expected = -1, Int32.bitwiseAnd(-1, -1))

    @Test
    def testBinaryBitwiseAnd05(): Unit \ Assert =
        assertEq(expected = 40i8, Int8.bitwiseAnd(40i8, -1i8))

    @Test
    def testBinaryBitwiseAnd06(): Unit \ Assert =
        assertEq(expected = 40i8, Int8.bitwiseAnd(40i8, 40i8))

    @Test
    def testBinaryBitwiseAnd07(): Unit \ Assert =
        assertEq(expected = 0i8, Int8.bitwiseAnd(40i8, 0i8))

    @Test
    def testBinaryBitwiseAnd08(): Unit \ Assert =
        assertEq(expected = -1i8, Int8.bitwiseAnd(-1i8, -1i8))

    @Test
    def testBinaryBitwiseAnd09(): Unit \ Assert =
        assertEq(expected = 400i16, Int16.bitwiseAnd(400i16, -1i16))

    @Test
    def testBinaryBitwiseAnd10(): Unit \ Assert =
        assertEq(expected = 400i16, Int16.bitwiseAnd(400i16, 400i16))

    @Test
    def testBinaryBitwiseAnd11(): Unit \ Assert =
        assertEq(expected = 0i16, Int16.bitwiseAnd(400i16, 0i16))

    @Test
    def testBinaryBitwiseAnd12(): Unit \ Assert =
        assertEq(expected = -1i16, Int16.bitwiseAnd(-1i16, -1i16))

    @Test
    def testBinaryBitwiseAnd13(): Unit \ Assert =
        assertEq(expected = 40000i32, Int32.bitwiseAnd(40000i32, -1i32))

    @Test
    def testBinaryBitwiseAnd14(): Unit \ Assert =
        assertEq(expected = 40000i32, Int32.bitwiseAnd(40000i32, 40000i32))

    @Test
    def testBinaryBitwiseAnd15(): Unit \ Assert =
        assertEq(expected = 0i32, Int32.bitwiseAnd(40000i32, 0i32))

    @Test
    def testBinaryBitwiseAnd16(): Unit \ Assert =
        assertEq(expected = -1i32, Int32.bitwiseAnd(-1i32, -1i32))

    @Test
    def testBinaryBitwiseAnd17(): Unit \ Assert =
        assertEq(expected = 40000000000i64, Int64.bitwiseAnd(40000000000i64, -1i64))

    @Test
    def testBinaryBitwiseAnd18(): Unit \ Assert =
        assertEq(expected = 40000000000i64, Int64.bitwiseAnd(40000000000i64, 40000000000i64))

    @Test
    def testBinaryBitwiseAnd19(): Unit \ Assert =
        assertEq(expected = 0i64, Int64.bitwiseAnd(40000000000i64, 0i64))

    @Test
    def testBinaryBitwiseAnd20(): Unit \ Assert =
        assertEq(expected = -1i64, Int64.bitwiseAnd(-1i64, -1i64))

    @Test
    def testBinaryBitwiseAnd21(): Unit \ Assert =
        assertEq(expected = 40000000000000000000ii, BigInt.bitwiseAnd(40000000000000000000ii, -1ii))

    @Test
    def testBinaryBitwiseAnd22(): Unit \ Assert =
        assertEq(expected = 40000000000000000000ii, BigInt.bitwiseAnd(40000000000000000000ii, 40000000000000000000ii))

    @Test
    def testBinaryBitwiseAnd23(): Unit \ Assert =
        assertEq(expected = 0ii, BigInt.bitwiseAnd(40000000000000000000ii, 0ii))

    @Test
    def testBinaryBitwiseAnd24(): Unit \ Assert =
        assertEq(expected = -1ii, BigInt.bitwiseAnd(-1ii, -1ii))

    @Test
    def testBinaryBitwiseOr01(): Unit \ Assert =
        assertEq(expected = -1, Int32.bitwiseOr(40000, -1))

    @Test
    def testBinaryBitwiseOr02(): Unit \ Assert =
        assertEq(expected = 40000, Int32.bitwiseOr(40000, 40000))

    @Test
    def testBinaryBitwiseOr03(): Unit \ Assert =
        assertEq(expected = 40000, Int32.bitwiseOr(40000, 0))

    @Test
    def testBinaryBitwiseOr04(): Unit \ Assert =
        assertEq(expected = -1, Int32.bitwiseOr(-1, -1))

    @Test
    def testBinaryBitwiseOr05(): Unit \ Assert =
        assertEq(expected = -1i8, Int8.bitwiseOr(40i8, -1i8))

    @Test
    def testBinaryBitwiseOr06(): Unit \ Assert =
        assertEq(expected = 40i8, Int8.bitwiseOr(40i8, 40i8))

    @Test
    def testBinaryBitwiseOr07(): Unit \ Assert =
        assertEq(expected = 40i8, Int8.bitwiseOr(40i8, 0i8))

    @Test
    def testBinaryBitwiseOr08(): Unit \ Assert =
        assertEq(expected = -1i8, Int8.bitwiseOr(-1i8, -1i8))

    @Test
    def testBinaryBitwiseOr09(): Unit \ Assert =
        assertEq(expected = -1i16, Int16.bitwiseOr(400i16, -1i16))

    @Test
    def testBinaryBitwiseOr10(): Unit \ Assert =
        assertEq(expected = 400i16, Int16.bitwiseOr(400i16, 400i16))

    @Test
    def testBinaryBitwiseOr11(): Unit \ Assert =
        assertEq(expected = 400i16, Int16.bitwiseOr(400i16, 0i16))

    @Test
    def testBinaryBitwiseOr12(): Unit \ Assert =
        assertEq(expected = -1i16, Int16.bitwiseOr(-1i16, -1i16))

    @Test
    def testBinaryBitwiseOr13(): Unit \ Assert =
        assertEq(expected = -1i32, Int32.bitwiseOr(40000i32, -1i32))

    @Test
    def testBinaryBitwiseOr14(): Unit \ Assert =
        assertEq(expected = 40000i32, Int32.bitwiseOr(40000i32, 40000i32))

    @Test
    def testBinaryBitwiseOr15(): Unit \ Assert =
        assertEq(expected = 40000i32, Int32.bitwiseOr(40000i32, 0i32))

    @Test
    def testBinaryBitwiseOr16(): Unit \ Assert =
        assertEq(expected = -1i32, Int32.bitwiseOr(-1i32, -1i32))

    @Test
    def testBinaryBitwiseOr17(): Unit \ Assert =
        assertEq(expected = -1i64, Int64.bitwiseOr(40000000000i64, -1i64))

    @Test
    def testBinaryBitwiseOr18(): Unit \ Assert =
        assertEq(expected = 40000000000i64, Int64.bitwiseOr(40000000000i64, 40000000000i64))

    @Test
    def testBinaryBitwiseOr19(): Unit \ Assert =
        assertEq(expected = 40000000000i64, Int64.bitwiseOr(40000000000i64, 0i64))

    @Test
    def testBinaryBitwiseOr20(): Unit \ Assert =
        assertEq(expected = -1i64, Int64.bitwiseOr(-1i64, -1i64))

    @Test
    def testBinaryBitwiseOr21(): Unit \ Assert =
        assertEq(expected = -1ii, BigInt.bitwiseOr(40000000000000000000ii, -1ii))

    @Test
    def testBinaryBitwiseOr22(): Unit \ Assert =
        assertEq(expected = 40000000000000000000ii, BigInt.bitwiseOr(40000000000000000000ii, 40000000000000000000ii))

    @Test
    def testBinaryBitwiseOr23(): Unit \ Assert =
        assertEq(expected = 40000000000000000000ii, BigInt.bitwiseOr(40000000000000000000ii, 0ii))

    @Test
    def testBinaryBitwiseOr24(): Unit \ Assert =
        assertEq(expected = -1ii, BigInt.bitwiseOr(-1ii, -1ii))

    @Test
    def testBinaryBitwiseXor01(): Unit \ Assert =
        assertEq(expected = -40001, Int32.bitwiseXor(40000, -1))

    @Test
    def testBinaryBitwiseXor02(): Unit \ Assert =
        assertEq(expected = 0, Int32.bitwiseXor(40000, 40000))

    @Test
    def testBinaryBitwiseXor03(): Unit \ Assert =
        assertEq(expected = 40000, Int32.bitwiseXor(40000, 0))

    @Test
    def testBinaryBitwiseXor04(): Unit \ Assert =
        assertEq(expected = 0, Int32.bitwiseXor(-1, -1))

    @Test
    def testBinaryBitwiseXor05(): Unit \ Assert =
        assertEq(expected = -41i8, Int8.bitwiseXor(40i8, -1i8))

    @Test
    def testBinaryBitwiseXor06(): Unit \ Assert =
        assertEq(expected = 0i8, Int8.bitwiseXor(40i8, 40i8))

    @Test
    def testBinaryBitwiseXor07(): Unit \ Assert =
        assertEq(expected = 40i8, Int8.bitwiseXor(40i8, 0i8))

    @Test
    def testBinaryBitwiseXor08(): Unit \ Assert =
        assertEq(expected = 0i8, Int8.bitwiseXor(-1i8, -1i8))

    @Test
    def testBinaryBitwiseXor09(): Unit \ Assert =
        assertEq(expected = -401i16, Int16.bitwiseXor(400i16, -1i16))

    @Test
    def testBinaryBitwiseXor10(): Unit \ Assert =
        assertEq(expected = 0i16, Int16.bitwiseXor(400i16, 400i16))

    @Test
    def testBinaryBitwiseXor11(): Unit \ Assert =
        assertEq(expected = 400i16, Int16.bitwiseXor(400i16, 0i16))

    @Test
    def testBinaryBitwiseXor12(): Unit \ Assert =
        assertEq(expected = 0i16, Int16.bitwiseXor(-1i16, -1i16))

    @Test
    def testBinaryBitwiseXor13(): Unit \ Assert =
        assertEq(expected = -40001i32, Int32.bitwiseXor(40000i32, -1i32))

    @Test
    def testBinaryBitwiseXor14(): Unit \ Assert =
        assertEq(expected = 0i32, Int32.bitwiseXor(40000i32, 40000i32))

    @Test
    def testBinaryBitwiseXor15(): Unit \ Assert =
        assertEq(expected = 40000i32, Int32.bitwiseXor(40000i32, 0i32))

    @Test
    def testBinaryBitwiseXor16(): Unit \ Assert =
        assertEq(expected = 0i32, Int32.bitwiseXor(-1i32, -1i32))

    @Test
    def testBinaryBitwiseXor17(): Unit \ Assert =
        assertEq(expected = -40000000001i64, Int64.bitwiseXor(40000000000i64, -1i64))

    @Test
    def testBinaryBitwiseXor18(): Unit \ Assert =
        assertEq(expected = 0i64, Int64.bitwiseXor(40000000000i64, 40000000000i64))

    @Test
    def testBinaryBitwiseXor19(): Unit \ Assert =
        assertEq(expected = 40000000000i64, Int64.bitwiseXor(40000000000i64, 0i64))

    @Test
    def testBinaryBitwiseXor20(): Unit \ Assert =
        assertEq(expected = 0i64, Int64.bitwiseXor(-1i64, -1i64))

    @Test
    def testBinaryBitwiseXor21(): Unit \ Assert =
        assertEq(expected = -40000000000000000001ii, BigInt.bitwiseXor(40000000000000000000ii, -1ii))

    @Test
    def testBinaryBitwiseXor22(): Unit \ Assert =
        assertEq(expected = 0ii, BigInt.bitwiseXor(40000000000000000000ii, 40000000000000000000ii))

    @Test
    def testBinaryBitwiseXor23(): Unit \ Assert =
        assertEq(expected = 40000000000000000000ii, BigInt.bitwiseXor(40000000000000000000ii, 0ii))

    @Test
    def testBinaryBitwiseXor24(): Unit \ Assert =
        assertEq(expected = 0ii, BigInt.bitwiseXor(-1ii, -1ii))

}
