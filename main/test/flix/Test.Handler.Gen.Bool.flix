mod Test.Handler.Gen.Bool {

    eff Gen {
        pub def gen(): Bool
    }

    def generator(): Unit \ Gen =
        Gen.gen(); generator()

    def sample(limit: Int32): List[Bool] =
        region rc {
            let counter = Ref.fresh(rc, (0, true));
            try {
                generator(); Nil
            } with Gen {
                def gen(k) =
                    let (i, b) = getAndInc(counter);
                    if (i == limit) Nil else b :: k(b)
            }
        }

    def getAndInc(r: Ref[(Int32, Bool), r]): (Int32, Bool) \ r =
        let (i, b) = Ref.get(r);
        Ref.put((i + 1, not b), r);
        (i, b)

    def range(a: Int32, b: Int32): List[Bool] =
        List.unfold(match (i, v) -> if (i >= b) None else Some((v, (i + 1, not v))), (a, true))

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
