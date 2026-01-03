mod Test.Exp.ParYield {

    use Assert.assertEq

    @Test
    def testParYield01(): Unit \ Assert =
        let r = par (a <- 1; b <- 2; c <- 3) yield a + b + c;
        assertEq(expected = 6, r)

    @Test
    def testParYield02(): Unit \ Assert =
        let r = par ((a, b) <- (1, 2); (c, d, e) <- (3, 4, 5); f <- 6)
            yield a + b + c + d + e + f;
        assertEq(expected = 21, r)

    @Test
    def testParYield03(): Unit \ Assert =
        let r = par (a <- 1; b <- 2; c <- 3) yield a + b + c;
        assertEq(expected = 6, r)

    @Test
    def testParYield04(): Unit \ Assert =
        let r = par (a <- let a = 1; a) yield a;
        assertEq(expected = 1, r)

    @Test
    def testParYield05(): Unit \ Assert =
        let r = par (a <- 5; b <- let a = 1; a) yield a + b;
        assertEq(expected = 6, r)

    @Test
    def testParYield06(): Unit \ Assert =
        let r = par (a <- 5; b <- let a = 1; a) yield a + b;
        assertEq(expected = 6, r)

    @Test
    def testParYield07(): Unit \ Assert =
        let r = par (a <- par (a <- 5; b <- 6; c <- 7) yield a + b + c; b <- 10) yield a + b;
        assertEq(expected = 28, r)

    @Test
    def testParYield08(): Unit \ Assert =
        let r = par (a <- par (a <- 5; b <- 6; c <- 7) yield a + b + c; b <- 10) yield a + b;
        assertEq(expected = 28, r)

    @Test
    def testParYield09(): Unit \ Assert =
        let r = par (a <- 1; b <- par (c <- 2; d <- par (e <- 10) yield e) yield d + c) yield b + a;
        assertEq(expected = 13, r)

    @Test
    def testParYield10(): Unit \ Assert =
        let r = par (a <- 1; b <- par (c <- 2; d <- par (e <- 3) yield e) yield d + c) yield b + a;
        assertEq(expected = 6, r)

    @Test
    def testParYield11(): Unit \ Assert =
        let r = par (a <- par (a <- par (a <- 1) yield a) yield a) yield a;
        assertEq(expected = 1, r)

    @Test
    def testParYield12(): Unit \ Assert =
        let r = par (a <- par (a <- par (a <- 1) yield a) yield a) yield a;
        assertEq(expected = 1, r)

    @Test
    def testParYield13(): Unit \ Assert = region rc {
        let a = Ref.fresh(rc, 10);
        let r = par (x <- 1) yield x + Ref.get(a);
        assertEq(expected = 11, r)
    }

    @Test
    def testParYield14(): Unit \ Assert = region rc {
        let a = Ref.fresh(rc, 10);
        let r = par (x <- 1) yield x + Ref.get(a);
        assertEq(expected = 11, r)
    }

    @Test
    def testParYield15(): Unit \ Assert =
        let r = par (a <- 5; b <- (let a = 1; a); c <- 6) yield a + b + c;
        assertEq(expected = 12, r)

    @Test
    def testParYield16(): Unit \ Assert =
        let c = 2;
        let r = par (
            a <- 1;              // Constant
            b <- c;              // Variable lookup
            d <- (let a = 3; a); // Spawn into thread
            e <- (let b = 4; b)  // Last complex expression
        ) yield a + b + d + e;
        assertEq(expected = 10, r)

    @Test
    def testParYield17(): Unit \ Assert =
        let c = 2;
        let r = par (
            a <- 1;              // Constant
            b <- c;              // Variable lookup
            d <- (let a = 3; a); // Spawn into thread
            e <- (let b = 4; b); // Last complex expression
            f <- 5;              // Constant
            g <- 6               // Constant
        ) yield a + b + d + e + f + g;
        assertEq(expected = 21, r)

    @Test
    def testParYield18(): Unit \ Assert =
        let c = 2;
        let r = par (
            a <- 1;              // Constant
            b <- c;              // Variable lookup
            d <- (let a = 3; a); // Spawn into thread
            e <- (let b = 4; b); // Spawn into thread
            f <- 5;              // Constant
            g <- 6;              // Constant
            h <- (let b = 7; b)  // Last complex expression
        ) yield a + b + d + e + f + g + h;
        assertEq(expected = 28, r)

    @Test
    def testParYield19(): Unit \ Assert =
        let c = 2;
        let r = par (
            a <- 1;              // Constant
            b <- c;              // Variable lookup
            d <- (let a = 3; a); // Spawn into thread
            e <- (let b = 4; b); // Spawn into thread
            f <- 5;              // Constant
            g <- 6;              // Constant
            h <- (let b = 7; b); // Last complex expression
            i <- 8               // Constant
        ) yield a + b + d + e + f + g + h + i;
        assertEq(expected = 36, r)

    @Test
    def testParYield20(): Unit \ Assert =
        let c = 2;
        let r = par (
            a <- 1;              // Constant
            b <- c;              // Variable lookup
            d <- (let a = 3; a); // Spawn into thread
            e <- 4;              // Constant
            f <- (let b = 5; b); // Spawn into thread
            g <- 6;              // Constant
            h <- (let b = 7; b); // Last complex expression
            i <- 8               // Constant
        ) yield a + b + d + e + f + g + h + i;
        assertEq(expected = 36, r)

}
