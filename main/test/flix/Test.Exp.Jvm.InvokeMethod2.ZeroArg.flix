mod Test.Exp.Jvm.InvokeMethod2.ZeroArg {

    @test
    def testInvokeMethod2_01(): Bool \ IO =
        let obj = "HELLO WORLD";
        obj#toLowerCase() == "hello world"

    @test
    def testInvokeMethod2_02(): Bool \ IO =
        let obj = "hello world";
        obj#toUpperCase() == "HELLO WORLD"

    @test
    def testInvokeMethod2_03(): Bool \ IO =
        let obj = "hello world";
        obj#length() == 11

    @test
    def testInvokeMethod2_04(): Bool \ IO =
        let obj = "   hello world    ";
        obj#trim() == "hello world"

    @test
    def testInvokeMethod2_05(): Bool \ IO =
        let obj = "hello world";
        obj#intern() == obj
}