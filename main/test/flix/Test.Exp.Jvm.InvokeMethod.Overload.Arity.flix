mod Test.Exp.Jvm.InvokeMethod.Overload.Arity {

    use Assert.assertEq

    @Test
    def testInvokeMethod_01(): Unit \ Assert =
        let obj = "HELLO WORLD";
        assertEq(expected = 4, obj.indexOf("O"))

    @Test
    def testInvokeMethod_02(): Unit \ Assert =
        let obj = "HELLO WORLD";
        assertEq(expected = -1, obj.indexOf("Z"))

    @Test
    def testInvokeMethod_03(): Unit \ Assert =
        let obj = "HELLO WORLD";
        assertEq(expected = 6, obj.indexOf("W", 5))

    @Test
    def testInvokeMethod_04(): Unit \ Assert =
        let obj = "HELLO WORLD";
        assertEq(expected = -1, obj.indexOf("W", 7))

    @Test
    def testInvokeMethod_05(): Unit \ Assert =
        let obj = "HELLO WORLD";
        assertEq(expected = 7, obj.lastIndexOf("O"))

    @Test
    def testInvokeMethod_06(): Unit \ Assert =
        let obj = "HELLO WORLD";
        assertEq(expected = -1, obj.lastIndexOf("Z"))

    @Test
    def testInvokeMethod_07(): Unit \ Assert =
        let obj = "HELLO WORLD";
        assertEq(expected = 9, obj.lastIndexOf("L", 10))

    @Test
    def testInvokeMethod_08(): Unit \ Assert =
        let obj = "HELLO WORLD";
        assertEq(expected = -1, obj.lastIndexOf("W", 3))

}
