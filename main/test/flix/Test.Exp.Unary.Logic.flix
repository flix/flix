namespace Test/Exp/Unary/Logic {

    @test
    def testLogicalNot01(): Bool = false == !true

    @test
    def testLogicalNot02(): Bool = true == !false

    @test
    def testLogicalNot03(): Bool = false == not true

    @test
    def testLogicalNot04(): Bool = true == not false

}