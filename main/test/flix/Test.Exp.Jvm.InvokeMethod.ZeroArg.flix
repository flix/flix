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
        let value = 12093ii;
        assertEq(expected = 12093, value.intValueExact())

    @Test
    def testInvokeMethod_07(): Unit \ Assert + IO =
        let value = -20938ii;
        assertEq(expected = -20938f32, value.floatValue())

    @Test
    def testInvokeMethod_08(): Unit \ Assert + IO =
        let value = 34882ii;
        assertEq(expected = 1, value.getLowestSetBit())

    @Test
    def testInvokeMethod_09(): Unit \ Assert + IO =
        let value = 2384721ii;
        assertEq(expected = 9, value.bitCount())

    @Test
    def testInvokeMethod_10(): Unit \ Assert + IO =
        let value = -32ii;
        assertEq(expected = 32ii, value.abs())

    @Test
    def testInvokeMethod_11(): Unit \ Assert =
        let value = -123.45ff;
        assertEq(expected = 123.45ff, value.abs())

    @Test
    def testInvokeMethod_12(): Unit \ Assert =
        let value = 12443.1489ff;
        assertEq(expected = 9, value.precision())

    @Test
    def testInvokeMethod_13(): Unit \ Assert + IO =
        let value = -50.000001ff;
        assertEq(expected = -50.000001ff, value.plus())

    @Test
    def testInvokeMethod_14(): Unit \ Assert =
        let value = 23984.1119ff;
        assertEq(expected = 23984.1119f64, value.doubleValue())

    @Test
    def testInvokeMethod_15(): Unit \ Assert =
        let value = 57842.0109ff;
        assertEq(expected = 57842.0109f32, value.floatValue())
}
