mod Test.Handler.Gen.Float64 {

    eff Gen {
        pub def gen(): Float64
    }

    def generator(): Unit \ Gen =
        do Gen.gen(); generator()

    def sample(limit: Float64): List[Float64] =
        region rc {
            let counter = Ref.fresh(rc, 0.0f64);
            try {
                generator(); Nil
            } with Gen {
                def gen(k) =
                    let i = getAndInc(counter);
                    if (i == limit) Nil else i :: k(i)
            }
        }

    def getAndInc(r: Ref[Float64, r]): Float64 \ r =
        let i = deref r;
        Ref.put(i + 1.0f64, r);
        i

    def range(a: Float64, b: Float64): List[Float64] =
        List.unfold(i -> if (i >= b) None else Some((i, i + 1.0f64)), a)

    @Test
    def testSample01(): Bool =
        Assert.eq(Nil, sample(0.0f64))

    @Test
    def testSample02(): Bool =
        Assert.eq(0.0f64 :: Nil, sample(1.0f64))

    @Test
    def testSample03(): Bool =
        Assert.eq(0.0f64 :: 1.0f64 :: Nil, sample(2.0f64))

    @Test
    def testSample04(): Bool =
        Assert.eq(0.0f64 :: 1.0f64 :: 2.0f64 :: Nil, sample(3.0f64))

    @Test
    def testSample05(): Bool =
        Assert.eq(0.0f64 :: 1.0f64 :: 2.0f64 :: 3.0f64 :: Nil, sample(4.0f64))

    @Test
    def testSample06(): Bool =
        Assert.eq(0.0f64 :: 1.0f64 :: 2.0f64 :: 3.0f64 :: 4.0f64 :: Nil, sample(5.0f64))

    @Test
    def testSample07(): Bool =
        Assert.eq(0.0f64 :: 1.0f64 :: 2.0f64 :: 3.0f64 :: 4.0f64 :: 5.0f64 :: Nil, sample(6.0f64))

    @Test
    def testSample08(): Bool =
        Assert.eq(range(0.0f64, 10.0f64), sample(10.0f64))

    @Test
    def testSample09(): Bool =
        Assert.eq(range(0.0f64, 100.0f64), sample(100.0f64))

}
