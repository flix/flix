mod Test.Handler.Gen.Int32 {

    eff Gen {
        pub def gen(): Int32
    }

    def generator(): Unit \ Gen =
        do Gen.gen(); generator()

    def sample(limit: Int32): List[Int32] =
        region rc {
            let counter = Ref.fresh(rc, 0);
            try {
                generator(); Nil
            } with Gen {
                def gen(k) =
                    let i = getAndInc(counter);
                    if (i == limit) Nil else i :: k(i)
            }
        }

    def getAndInc(r: Ref[Int32, r]): Int32 \ r =
        let i = deref r;
        Ref.put(i + 1, r);
        i

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
