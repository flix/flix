mod Test.Equality.Int32 {

    @test
    def testEqInt32_01(): Bool = 0i32 == 0i32

    @test
    def testEqInt32_02(): Bool = +1i32 == +1i32

    @test
    def testEqInt32_03(): Bool = -1i32 == -1i32

    @test
    def testNeqInt32_01(): Bool = -1i32 != +1i32

    @test
    def testNeqInt32_02(): Bool = +1i32 != -1i32

}
