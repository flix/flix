mod Test.Handler.Ask.Int32 {

    eff Ask {
        def ask(x: Int32): Unit
    }

    def generator(x: Int32): Unit \ Ask =
        Ask.ask(x); generator(x + 1)

    def sample(limit: Int32): List[Int32] =
        try {
            generator(0); Nil
        } with Ask {
            def ask(x, k) = if (x == limit) Nil else x :: k()
        }

    @Test
    def testSample01(): Bool =
        Assert.eq(Nil, sample(0))

    @Test
    def testSample02(): Bool =
        Assert.eq(0 :: Nil, sample(1))

    @Test
    def testSample03(): Bool =
        Assert.eq(0 :: 1 :: Nil, sample(2))

    @Test
    def testSample04(): Bool =
        Assert.eq(0 :: 1 :: 2 :: Nil, sample(3))

    @Test
    def testSample05(): Bool =
        Assert.eq(0 :: 1 :: 2 :: 3 :: Nil, sample(4))

    @Test
    def testSample06(): Bool =
        Assert.eq(0 :: 1 :: 2 :: 3 :: 4 :: Nil, sample(5))

    @Test
    def testSample07(): Bool =
        Assert.eq(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil, sample(6))

    @Test
    def testSample08(): Bool =
        Assert.eq(List.range(0, 10), sample(10))

    @Test
    def testSample09(): Bool =
        Assert.eq(List.range(0, 100), sample(100))

}
