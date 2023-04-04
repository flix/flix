mod Test.Equality.Bool {

    @test
    def testEqBool01(): Bool = true == true

    @test
    def testEqBool02(): Bool = false == false

    @test
    def testNeqBool01(): Bool = true != false

    @test
    def testNeqBool02(): Bool = false != true

}
