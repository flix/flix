mod Test.Exp.Jvm.InvokeMethod.Chaining {

    use Assert.assertEq

    @Test
    def testInvokeMethod_01(): Unit \ Assert =
        let obj = "HeLlO WoRlD";
        assertEq(expected = "hello world", obj.toUpperCase().toLowerCase())

    @Test
    def testInvokeMethod_02(): Unit \ Assert =
        let obj = "hEllO WoRld";
        assertEq(expected = "HELLO WORLD", obj.toLowerCase().toUpperCase())

    @Test
    def testInvokeMethod_03(): Unit \ Assert =
        let obj = "hello world";
        assertEq(expected = 10, obj.substring(1, 11).length())

    @Test
    def testInvokeMethod_04(): Unit \ Assert =
        let obj = "   hello world    ";
        let obj2 = "hello world";
        assertEq(expected = obj2.length(), obj.trim().length())

    @Test
    def testInvokeMethod_05(): Unit \ Assert + IO =
        let val = 123488ii;
        assertEq(expected = 4493ii, val.modPow(10ii, 32023ii).modInverse(4831ii))

    @Test
    def testInvokeMethod_06(): Unit \ Assert =
        let val = 94832ii;
        assertEq(expected = -1043152ii, val.multiply(11ii).negate())

    @Test
    def testInvokeMethod_07(): Unit \ Assert + IO =
        let val = 324521ii;
        assertEq(expected = 4ii, val.shiftLeft(2).max(1284ii).modPow(1ii, 10ii))

    @Test
    def testInvokeMethod_08(): Unit \ Assert + IO =
        let val = 9999.999999999ff;
        assertEq(expected = 12.4938ff, val.max(9999.99999999ff).min(12.4938ff))

    @Test
    def testInvokeMethod_09(): Unit \ Assert =
        let val = -932983.1113ff;
        assertEq(expected = 978987.5755350917ff, val.multiply(1.049309ff).negate())

    @Test
    def testInvokeMethod_10(): Unit \ Assert =
        let val = 833.1ff;
        assertEq(expected = 232047597200769719005766.94527841ff, val.pow(4).pow(2).pow(1))

    @Test
    def testInvokeMethod_11(): Unit \ Assert + IO =
        let val = 9012111.9834ff;
        assertEq(expected = 327.9234ff, val.remainder(429.01ff).max(32ff))

}
