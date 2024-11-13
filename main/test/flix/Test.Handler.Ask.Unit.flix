mod Test.Handler.Ask.Unit {

    eff Ask {
        def ask(x: Int32): Unit
    }

    def generator(x: Int32): Unit \ Ask =
        Ask.ask(x); generator(x + 1)

    def sample(limit: Int32): List[Unit] =
        try {
            generator(0); Nil
        } with Ask {
            def ask(x, k) = if (x == limit) Nil else () :: k()
        }

    def range(a: Int32, b: Int32): List[Unit] =
        List.unfold(i -> if (i >= b) None else Some(((), i + 1)), a)

    @Test
    def testSample01(): Bool =
        Assert.eq(Nil, sample(0))

    @Test
    def testSample02(): Bool =
        Assert.eq(() :: Nil, sample(1))

    @Test
    def testSample03(): Bool =
        Assert.eq(() :: () :: Nil, sample(2))

    @Test
    def testSample04(): Bool =
        Assert.eq(() :: () :: () :: Nil, sample(3))

    @Test
    def testSample05(): Bool =
        Assert.eq(() :: () :: () :: () :: Nil, sample(4))

    @Test
    def testSample06(): Bool =
        Assert.eq(() :: () :: () :: () :: () :: Nil, sample(5))

    @Test
    def testSample07(): Bool =
        Assert.eq(() :: () :: () :: () :: () :: () :: Nil, sample(6))

    @Test
    def testSample08(): Bool =
        Assert.eq(range(0, 10), sample(10))

    @Test
    def testSample09(): Bool =
        Assert.eq(range(0, 100), sample(100))

}
