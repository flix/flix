namespace Test/Exp/Par {

    def f(x: Int32): Bool = x == 1

    @test
    def testPar01(): (Int32, Int32, Int32) =
        par (1, 2, 3)

    @test
    def testPar02(): Bool =
        (par (1, 2, 3)) == (1, 2, 3)

    @test
    def testPar03(): Bool =
        (par (f(1), f(2), f(3))) == (true, false, false)

}
