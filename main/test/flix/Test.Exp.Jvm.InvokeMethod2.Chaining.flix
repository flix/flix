mod Test.Exp.Jvm.InvokeMethod2.Chaining {

    @test
    def testInvokeMethod2_01(): Bool \ IO =
        let obj = "HeLlO WoRlD";
        obj#toUpperCase()#toLowerCase() == "hello world"

    @test
    def testInvokeMethod2_02(): Bool \ IO =
        let obj = "hEllO WoRld";
        obj#toLowerCase()#toUpperCase() == "HELLO WORLD"

    @test
    def testInvokeMethod2_03(): Bool \ IO =
        let obj = "hello world";
        obj#substring(1, 11)#length() == 10

    @test
    def testInvokeMethod2_04(): Bool \ IO =
        let obj = "   hello world    ";
        let obj2 = "hello world";
        obj#trim()#length() == obj2#length()
}
