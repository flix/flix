mod Test.Exp.Jvm.InvokeMethod.ZeroArg {

    use Assert.assertEq

    @Test
    def testInvokeMethod_01(): Unit \ Assert =
        let obj = "HELLO WORLD";
        assertEq(expected = "hello world", obj.toLowerCase())

    @Test
    def testInvokeMethod_02(): Unit \ Assert =
        let obj = "hello world";
        assertEq(expected = "HELLO WORLD", obj.toUpperCase())

    @Test
    def testInvokeMethod_03(): Unit \ Assert =
        let obj = "hello world";
        assertEq(expected = 11, obj.length())

    @Test
    def testInvokeMethod_04(): Unit \ Assert =
        let obj = "   hello world    ";
        assertEq(expected = "hello world", obj.trim())

    @Test
    def testInvokeMethod_05(): Unit \ Assert + IO =
        let obj = "hello world";
        assertEq(expected = obj, obj.intern())

    @Test
    def testInvokeMethod_06(): Unit \ Assert =
        let val = 12093ii;
        assertEq(expected = 12093, val.intValueExact())

    @Test
    def testInvokeMethod_07(): Unit \ Assert + IO =
        let val = -20938ii;
        assertEq(expected = -20938f32, val.floatValue())

    @Test
    def testInvokeMethod_08(): Unit \ Assert + IO =
        let val = 34882ii;
        assertEq(expected = 1, val.getLowestSetBit())

    @Test
    def testInvokeMethod_09(): Unit \ Assert + IO =
        let val = 2384721ii;
        assertEq(expected = 9, val.bitCount())

    @Test
    def testInvokeMethod_10(): Unit \ Assert + IO =
        let val = -32ii;
        assertEq(expected = 32ii, val.abs())

    @Test
    def testInvokeMethod_11(): Unit \ Assert =
        let val = -123.45ff;
        assertEq(expected = 123.45ff, val.abs())

    @Test
    def testInvokeMethod_12(): Unit \ Assert =
        let val = 12443.1489ff;
        assertEq(expected = 9, val.precision())

    @Test
    def testInvokeMethod_13(): Unit \ Assert + IO =
        let val = -50.000001ff;
        assertEq(expected = -50.000001ff, val.plus())

    @Test
    def testInvokeMethod_14(): Unit \ Assert =
        let val = 23984.1119ff;
        assertEq(expected = 23984.1119f64, val.doubleValue())

    @Test
    def testInvokeMethod_15(): Unit \ Assert =
        let val = 57842.0109ff;
        assertEq(expected = 57842.0109f32, val.floatValue())
}
