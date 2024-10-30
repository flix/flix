mod Test.Handler.Gen.Int8 {

    eff Gen {
        pub def gen(): Int8
    }

    def generator(): Unit \ Gen =
        Gen.gen(); generator()

    def sample(limit: Int8): List[Int8] =
        region rc {
            let counter = Ref.fresh(rc, 0i8);
            try {
                generator(); Nil
            } with Gen {
                def gen(k) =
                    let i = getAndInc(counter);
                    if (i == limit) Nil else i :: k(i)
            }
        }

    def getAndInc(r: Ref[Int8, r]): Int8 \ r =
        let i = Ref.get(r);
        Ref.put(i + 1i8, r);
        i

    def range(a: Int8, b: Int8): List[Int8] =
        List.unfold(i -> if (i >= b) None else Some((i, i + 1i8)), a)

    @Test
    def testSample01(): Bool =
        Assert.eq(Nil, sample(0i8))

    @Test
    def testSample02(): Bool =
        Assert.eq(0i8 :: Nil, sample(1i8))

    @Test
    def testSample03(): Bool =
        Assert.eq(0i8 :: 1i8 :: Nil, sample(2i8))

    @Test
    def testSample04(): Bool =
        Assert.eq(0i8 :: 1i8 :: 2i8 :: Nil, sample(3i8))

    @Test
    def testSample05(): Bool =
        Assert.eq(0i8 :: 1i8 :: 2i8 :: 3i8 :: Nil, sample(4i8))

    @Test
    def testSample06(): Bool =
        Assert.eq(0i8 :: 1i8 :: 2i8 :: 3i8 :: 4i8 :: Nil, sample(5i8))

    @Test
    def testSample07(): Bool =
        Assert.eq(0i8 :: 1i8 :: 2i8 :: 3i8 :: 4i8 :: 5i8 :: Nil, sample(6i8))

    @Test
    def testSample08(): Bool =
        Assert.eq(range(0i8, 10i8), sample(10i8))

    @Test
    def testSample09(): Bool =
        Assert.eq(range(0i8, 100i8), sample(100i8))

}
