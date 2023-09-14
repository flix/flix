mod Test.Exp.ParYield {

    @Test
    def testParYield01(): Int32 =
        par (a <- 1; b <- 2; c <- 3) yield a + b + c

    @Test
    def testParYield02(): Int32 =
        par ((a, b) <- (1, 2); (c, d, e) <- (3, 4, 5); f <- 6)
            yield a + b + c + d + e + f

    @Test
    def testParYield03(): Bool =
        let x = par (a <- 1; b <- 2; c <- 3) yield a + b + c;
        x == 6

    @Test
    def testParYield04(): Int32 =
        par (a <- let a = 1; a) yield a

    @Test
    def testParYield05(): Int32 =
        par (a <- 5; b <- let a = 1; a) yield a + b

    @Test
    def testParYield06(): Bool =
        (par (a <- 5; b <- let a = 1; a) yield a + b) == 6

    @Test
    def testParYield07(): Int32 =
        par (a <- par (a <- 5; b <- 6; c <- 7) yield a + b + c; b <- 10) yield a + b

    @Test
    def testParYield08(): Bool =
        (par (a <- par (a <- 5; b <- 6; c <- 7) yield a + b + c; b <- 10) yield a + b) == 28

    @Test
    def testParYield09(): Int32 =
        par (a <- 1; b <- par (c <- 2; d <- par (e <- 10) yield e) yield d + c) yield b + a

    @Test
    def testParYield10(): Bool =
        (par (a <- 1; b <- par (c <- 2; d <- par (e <- 3) yield e) yield d + c) yield b + a) == 6

    @Test
    def testParYield11(): Int32 =
        par (a <- par (a <- par (a <- 1) yield a) yield a) yield a

    @Test
    def testParYield12(): Bool =
        (par (a <- par (a <- par (a <- 1) yield a) yield a) yield a) == 1

    @Test
    def testParYield13(): Int32 = region rc {
        let a = ref 10 @ rc;
        par (x <- 1) yield x + deref a
    }

    @Test
    def testParYield14(): Bool = region rc {
        let a = ref 10 @ rc;
        (par (x <- 1) yield x + deref a) == 11
    }

    @Test
    def testParYield15(): Bool =
        (par (a <- 5; b <- (let a = 1; a); c <- 6) yield a + b + c) == 12

    @Test
    def testParYield16(): Int32 =
        let c = 2;
        par (
            a <- 1;              // Constant
            b <- c;              // Variable lookup
            d <- (let a = 3; a); // Spawn into thread
            e <- (let b = 4; b)  // Last complex expression
        ) yield a + b + d + e

    @Test
    def testParYield17(): Int32 =
        let c = 2;
        par (
            a <- 1;              // Constant
            b <- c;              // Variable lookup
            d <- (let a = 3; a); // Spawn into thread
            e <- (let b = 4; b); // Last complex expression
            f <- 5;              // Constant
            g <- 6               // Constant
        ) yield a + b + d + e + f + g

    @Test
    def testParYield18(): Int32 =
        let c = 2;
        par (
            a <- 1;              // Constant
            b <- c;              // Variable lookup
            d <- (let a = 3; a); // Spawn into thread
            e <- (let b = 4; b); // Spawn into thread
            f <- 5;              // Constant
            g <- 6;              // Constant
            h <- (let b = 7; b)  // Last complex expression
        ) yield a + b + d + e + f + g + h

    @Test
    def testParYield19(): Int32 =
        let c = 2;
        par (
            a <- 1;              // Constant
            b <- c;              // Variable lookup
            d <- (let a = 3; a); // Spawn into thread
            e <- (let b = 4; b); // Spawn into thread
            f <- 5;              // Constant
            g <- 6;              // Constant
            h <- (let b = 7; b); // Last complex expression
            i <- 8               // Constant
        ) yield a + b + d + e + f + g + h + i

    @Test
    def testParYield20(): Int32 =
        let c = 2;
        par (
            a <- 1;              // Constant
            b <- c;              // Variable lookup
            d <- (let a = 3; a); // Spawn into thread
            e <- 4;              // Constant
            f <- (let b = 5; b); // Spawn into thread
            g <- 6;              // Constant
            h <- (let b = 7; b); // Last complex expression
            i <- 8               // Constant
        ) yield a + b + d + e + f + g + h + i

}
