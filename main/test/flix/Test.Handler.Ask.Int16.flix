mod Test.Handler.Ask.Int16 {

    eff Ask {
        def ask(x: Int16): Unit
    }

    def generator(x: Int16): Unit \ Ask =
        Ask.ask(x); generator(x + 1i16)

    def sample(limit: Int16): List[Int16] =
        try {
            generator(0i16); Nil
        } with Ask {
            def ask(x, k) = if (x == limit) Nil else x :: k()
        }

    def range(a: Int16, b: Int16): List[Int16] =
        List.unfold(i -> if (i >= b) None else Some((i, i + 1i16)), a)

    @Test
    def testSample01(): Bool =
        Assert.eq(Nil, sample(0i16))

    @Test
    def testSample02(): Bool =
        Assert.eq(0i16 :: Nil, sample(1i16))

    @Test
    def testSample03(): Bool =
        Assert.eq(0i16 :: 1i16 :: Nil, sample(2i16))

    @Test
    def testSample04(): Bool =
        Assert.eq(0i16 :: 1i16 :: 2i16 :: Nil, sample(3i16))

    @Test
    def testSample05(): Bool =
        Assert.eq(0i16 :: 1i16 :: 2i16 :: 3i16 :: Nil, sample(4i16))

    @Test
    def testSample06(): Bool =
        Assert.eq(0i16 :: 1i16 :: 2i16 :: 3i16 :: 4i16 :: Nil, sample(5i16))

    @Test
    def testSample07(): Bool =
        Assert.eq(0i16 :: 1i16 :: 2i16 :: 3i16 :: 4i16 :: 5i16 :: Nil, sample(6i16))

    @Test
    def testSample08(): Bool =
        Assert.eq(range(0i16, 10i16), sample(10i16))

    @Test
    def testSample09(): Bool =
        Assert.eq(range(0i16, 100i16), sample(100i16))

}
