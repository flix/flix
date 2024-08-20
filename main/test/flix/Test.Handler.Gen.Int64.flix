mod Test.Handler.Gen.Int64 {

    eff Gen {
        pub def gen(): Int64
    }

    def generator(): Unit \ Gen =
        do Gen.gen(); generator()

    def sample(limit: Int64): List[Int64] =
        region rc {
            let counter = Ref.fresh(rc, 0i64);
            try {
                generator(); Nil
            } with Gen {
                def gen(k) =
                    let i = getAndInc(counter);
                    if (i == limit) Nil else i :: k(i)
            }
        }

    def getAndInc(r: Ref[Int64, r]): Int64 \ r =
        let i = deref r;
        Ref.put(i + 1i64, r);
        i

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
