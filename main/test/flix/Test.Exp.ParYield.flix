namespace Test/Exp/ParYield {

    @test
    def testParYield01(): Int32 =
        par (a <- 1, b <- 2, c <- 3) yield a + b + c

    @test
    def testParYield02(): Bool =
        let x = par (a <- 1, b <- 2, c <- 3) yield a + b + c;
        x == 6

}
