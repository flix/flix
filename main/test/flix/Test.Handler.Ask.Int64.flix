mod Test.Handler.Ask.Int64 {

    eff Ask {
        pub def ask(x: Int64): Unit
    }

    def generator(x: Int64): Unit \ Ask =
        Ask.ask(x); generator(x + 1i64)

    def sample(limit: Int64): List[Int64] =
        try {
            generator(0i64); Nil
        } with Ask {
            def ask(x, k) = if (x == limit) Nil else x :: k()
        }

    def range(a: Int64, b: Int64): List[Int64] =
        List.unfold(i -> if (i >= b) None else Some((i, i + 1i64)), a)

    @Test
    def testSample01(): Bool =
        Assert.eq(Nil, sample(0i64))

    @Test
    def testSample02(): Bool =
        Assert.eq(0i64 :: Nil, sample(1i64))

    @Test
    def testSample03(): Bool =
        Assert.eq(0i64 :: 1i64 :: Nil, sample(2i64))

    @Test
    def testSample04(): Bool =
        Assert.eq(0i64 :: 1i64 :: 2i64 :: Nil, sample(3i64))

    @Test
    def testSample05(): Bool =
        Assert.eq(0i64 :: 1i64 :: 2i64 :: 3i64 :: Nil, sample(4i64))

    @Test
    def testSample06(): Bool =
        Assert.eq(0i64 :: 1i64 :: 2i64 :: 3i64 :: 4i64 :: Nil, sample(5i64))

    @Test
    def testSample07(): Bool =
        Assert.eq(0i64 :: 1i64 :: 2i64 :: 3i64 :: 4i64 :: 5i64 :: Nil, sample(6i64))

    @Test
    def testSample08(): Bool =
        Assert.eq(range(0i64, 10i64), sample(10i64))

    @Test
    def testSample09(): Bool =
        Assert.eq(range(0i64, 100i64), sample(100i64))

}
