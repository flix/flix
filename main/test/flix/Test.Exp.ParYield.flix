namespace Test/Exp/ParYield {

    @test
    def testParYield01(): Int32 =
        par (a <- 1, b <- 2, c <- 3) yield a + b + c

    @test
    def testParYield02(): Int32 =
        par ((a, b) <- (1, 2), (c, d, e) <- (3, 4, 5), f <- 6)
            yield a + b + c + d + e + f

    @test
    def testParYield03(): Bool =
        let x = par (a <- 1, b <- 2, c <- 3) yield a + b + c;
        x == 6

    @test
    def testParYield04(): Int32 =
        par (a <- let a = 1; a) yield a

    @test
    def testParYield05(): Int32 =
        par (a <- 5, b <- let a = 1; a) yield a + b

    @test
    def testParYield06(): Bool =
        (par (a <- 5, b <- let a = 1; a) yield a + b) == 6

    @test
    def testParYield07(): Int32 =
        par (a <- par (a <- 5, b <- 6, c <- 7) yield a + b + c, b <- 10) yield a + b

    @test
    def testParYield08(): Bool =
        (par (a <- par (a <- 5, b <- 6, c <- 7) yield a + b + c, b <- 10) yield a + b) == 28

    @test
    def testParYield09(): Int32 =
        par (a <- 1, b <- par (c <- 2, d <- par (e <- 10) yield e) yield d + c) yield b + a

    @test
    def testParYield10(): Bool =
        (par (a <- 1, b <- par (c <- 2, d <- par (e <- 3) yield e) yield d + c) yield b + a) == 6

    @test
    def testParYield11(): Int32 =
        par (a <- par (a <- par (a <- 1) yield a) yield a) yield a

    @test
    def testParYield12(): Bool =
        (par (a <- par (a <- par (a <- 1) yield a) yield a) yield a) == 1

    @test
    def testParYield13(): Int32 = region r {
        let a = ref 10 @ r;
        par (x <- 1) yield x + deref a
    }

    @test
    def testParYield14(): Bool = region r {
        let a = ref 10 @ r;
        (par (x <- 1) yield x + deref a) == 11
    }
}
