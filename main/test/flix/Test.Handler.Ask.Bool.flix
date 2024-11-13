mod Test.Handler.Ask.Bool {

    eff Ask {
        def ask(x: Int32): Unit
    }

    def generator(x: Int32): Unit \ Ask =
        Ask.ask(x); generator(x + 1)

    def sample(limit: Int32): List[Bool] =
        try {
            generator(0); Nil
        } with Ask {
            def ask(x, k) = if (x == limit) Nil else next(x) :: k()
        }

    def next(i: Int32): Bool = if (Int32.modulo(i, 2) == 0) true else false

    def range(a: Int32, b: Int32): List[Bool] =
        List.unfold(i -> if (i >= b) None else Some((next(i), i + 1)), a)

    @Test
    def testSample01(): Bool =
        Assert.eq(Nil, sample(0))

    @Test
    def testSample02(): Bool =
        Assert.eq(true :: Nil, sample(1))

    @Test
    def testSample03(): Bool =
        Assert.eq(true :: false :: Nil, sample(2))

    @Test
    def testSample04(): Bool =
        Assert.eq(true :: false :: true :: Nil, sample(3))

    @Test
    def testSample05(): Bool =
        Assert.eq(true :: false :: true :: false :: Nil, sample(4))

    @Test
    def testSample06(): Bool =
        Assert.eq(true :: false :: true :: false :: true :: Nil, sample(5))

    @Test
    def testSample07(): Bool =
        Assert.eq(true :: false :: true :: false :: true :: false :: Nil, sample(6))

    @Test
    def testSample08(): Bool =
        Assert.eq(range(0, 10), sample(10))

    @Test
    def testSample09(): Bool =
        Assert.eq(range(0, 100), sample(100))

}
