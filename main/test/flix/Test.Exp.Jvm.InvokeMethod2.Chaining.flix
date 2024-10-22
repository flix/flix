mod Test.Exp.Jvm.InvokeMethod2.Chaining {

    @test
    def testInvokeMethod2_01(): Bool \ IO =
        let obj = "HeLlO WoRlD";
        obj.toUpperCase().toLowerCase() == "hello world"

    @test
    def testInvokeMethod2_02(): Bool \ IO =
        let obj = "hEllO WoRld";
        obj.toLowerCase().toUpperCase() == "HELLO WORLD"

    @test
    def testInvokeMethod2_03(): Bool \ IO =
        let obj = "hello world";
        obj.substring(1, 11).length() == 10

    @test
    def testInvokeMethod2_04(): Bool \ IO =
        let obj = "   hello world    ";
        let obj2 = "hello world";
        obj.trim().length() == obj2.length()

    @test
    def testInvokeMethod2_05(): Bool \ IO =
        let val = 123488ii;
        val.modPow(10ii, 32023ii).modInverse(4831ii) == 4493ii

    @test
    def testInvokeMethod2_06(): Bool \ IO =
        let val = 94832ii;
        val.multiply(11ii).negate() == -1043152ii

    @test
    def testInvokeMethod2_07(): Bool \ IO =
        let val = 324521ii;
        val.shiftLeft(2).max(1284ii).modPow(1ii, 10ii) == 4ii

    @test
    def testInvokeMethod2_08(): Bool \ IO =
        let val = 9999.999999999ff;
        val.max(9999.99999999ff).min(12.4938ff) == 12.4938ff

    @test
    def testInvokeMethod2_09(): Bool \ IO =
        let val = -932983.1113ff;
        val.multiply(1.049309ff).negate() == 978987.5755350917ff

    @test
    def testInvokeMethod2_10(): Bool \ IO =
        let val = 833.1ff;
        val.pow(4).pow(2).pow(1) == 232047597200769719005766.94527841ff

    @test
    def testInvokeMethod2_11(): Bool \ IO =
        let val = 9012111.9834ff;
        val.remainder(429.01ff).max(32ff) == 327.9234ff
}
