namespace Test/Exp/ParYield {

    @test
    def testParYield01(): Int32 =
        par (a <- 1, b <- 2, c <- 3) yield a + b + c

    @test
    def testParYield02(): Int32 =
        par ((a, b) <- (1, 2), (c, d, e) <- (3, 4, 5), f <- 6) yield a + b + c + d + e + f

    @test
    def testParYield03(): Bool =
        let x = par (a <- 1, b <- 2, c <- 3) yield a + b + c;
        x == 6

}
