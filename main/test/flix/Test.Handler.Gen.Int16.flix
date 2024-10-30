mod Test.Handler.Gen.Int16 {

    eff Gen {
        pub def gen(): Int16
    }

    def generator(): Unit \ Gen =
        Gen.gen(); generator()

    def sample(limit: Int16): List[Int16] =
        region rc {
            let counter = Ref.fresh(rc, 0i16);
            try {
                generator(); Nil
            } with Gen {
                def gen(k) =
                    let i = getAndInc(counter);
                    if (i == limit) Nil else i :: k(i)
            }
        }

    def getAndInc(r: Ref[Int16, r]): Int16 \ r =
        let i = Ref.get(r);
        Ref.put(i + 1i16, r);
        i

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
