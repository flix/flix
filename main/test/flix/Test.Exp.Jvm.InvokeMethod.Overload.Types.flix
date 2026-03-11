mod Test.Exp.Jvm.InvokeMethod.Overload.Types {

    use Assert.assertEq

    @Test
    def testInvokeMethod_01(): Unit \ Assert =
        let obj = "hello world";
        assertEq(expected = "hello world", obj.replace('O', 'i'))

    @Test
    def testInvokeMethod_02(): Unit \ Assert =
        let obj = "HELLO WORLD";
        assertEq(expected = "HEllO WORlD", obj.replace('L', 'l'))

    @Test
    def testInvokeMethod_03(): Unit \ Assert =
        let obj = "HELLO WORLD";
        assertEq(expected = "HEYO WORLD", obj.replace("LLO", "YO"))

    @Test
    def testInvokeMethod_04(): Unit \ Assert =
        let obj = "HELLO WORLD";
        assertEq(expected = "HELLO WORLD", obj.replace("AZERTY", "QWERTY"))

}
